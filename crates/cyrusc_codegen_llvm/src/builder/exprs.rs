use crate::{
    builder::{
        builder::IRBuilderCtx,
        irreg::LocalIRValue,
        values::{InternalValue, InternalValueKind},
    },
    llvm::constness::is_basic_value_constant,
};
use cyrusc_ast::operators::{InfixOperator, PrefixOperator, UnaryOperator};
use cyrusc_cir::{types::*, *};
use cyrusc_tast::types::PlainType;
use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    module::Linkage,
    types::{AnyTypeEnum, ArrayType, BasicTypeEnum},
    values::{
        AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue, StructValue,
    },
};

pub enum DerefMode {
    Load,  // for RValue
    Store, // for LHS assignment
}

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_expr(&mut self, expr: &CIRExpr) -> InternalValue<'ll> {
        match &expr.kind {
            CIRExprKind::Load(value_ref) => self.emit_load(value_ref),
            CIRExprKind::Literal(literal) => self.emit_literal(literal),
            CIRExprKind::Prefix(prefix_expr) => self.emit_prefix_expr(prefix_expr),
            CIRExprKind::Infix(infix_expr) => self.emit_infix_expr(infix_expr),
            CIRExprKind::Unary(unary_expr) => self.build_unary_expr(unary_expr),
            CIRExprKind::SizeOf(sizeof_expr) => self.emit_sizeof(sizeof_expr),
            CIRExprKind::Assign(assign_expr) => self.emit_assign(assign_expr),
            CIRExprKind::Cast(cast_expr) => {
                let lvalue = self.emit_expr(&cast_expr.operand);
                let rvalue = self.load_rvalue(lvalue);
                let target_type = self.emit_ty(*cast_expr.ty.clone());
                let casted = self.emit_cast(target_type, rvalue);
                InternalValue::new(
                    *cast_expr.ty.clone(),
                    InternalValueKind::RValue(casted.try_into().unwrap()),
                )
            }
            CIRExprKind::AddrOf(addr_of_expr) => self.emit_addr_of(addr_of_expr),
            CIRExprKind::Deref(deref_expr) => self.emit_deref(deref_expr, DerefMode::Load),
            CIRExprKind::Array(array_expr) => self.emit_array(array_expr),
            CIRExprKind::ArrayIndex(array_index_expr) => self.emit_array_index(array_index_expr),
            CIRExprKind::Tuple(tuple_expr) => self.emit_tuple(tuple_expr),
            CIRExprKind::TupleAccess(tuple_access) => self.emit_tuple_access(tuple_access),
            CIRExprKind::StructInit(struct_init_expr) => self.emit_struct_init(struct_init_expr),
            CIRExprKind::UnionInit(union_init_expr) => self.emit_union_init(union_init_expr),
            CIRExprKind::EnumInit(enum_init_expr) => self.emit_enum_init(enum_init_expr),
            CIRExprKind::StructFieldAccess(struct_field_access_expr) => {
                self.emit_struct_field_access(struct_field_access_expr)
            }
            CIRExprKind::UnionFieldAccess(union_field_access_expr) => {
                self.emit_union_field_access(union_field_access_expr)
            }
            CIRExprKind::FuncCall(func_call) => self.emit_func_call(func_call),
            CIRExprKind::MonomorphFuncInstanceCall(monomorph_func_instance_call) => {
                self.emit_monomorph_func_instance_call(monomorph_func_instance_call)
            }
            CIRExprKind::Lambda(lambda) => self.emit_lambda(lambda),
        }
    }

    pub(crate) fn emit_array_index_on_pointer(
        &mut self,
        lvalue: PointerValue<'ll>,
        index: InternalValue<'ll>,
        cir_elm_ty: CIRTy,
    ) -> InternalValue<'ll> {
        let elm_ty: BasicTypeEnum<'ll> = self.emit_ty(cir_elm_ty.clone()).try_into().unwrap();
        let idx_int = index.as_basic_value().into_int_value();

        let element_ptr: PointerValue<'ll> = unsafe {
            self.llvmbuilder
                .build_in_bounds_gep(elm_ty, lvalue, &[idx_int], "elem_ptr")
                .unwrap()
        };

        InternalValue::new(cir_elm_ty, InternalValueKind::LValue(element_ptr))
    }

    pub(crate) fn emit_array_index(&mut self, array_index: &CIRArrayIndexExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&array_index.operand);

        let index_lvalue = self.emit_expr(&array_index.index);
        let index_rvalue = self.load_rvalue(index_lvalue);

        if let Some(arr_ty) = lvalue.ty.as_arr_ty() {
            self.emit_inbounds_checked_array_index(
                lvalue.as_basic_value().into_pointer_value(),
                *arr_ty.ty.clone(),
                index_rvalue,
                arr_ty.len.try_into().unwrap(),
            )
        } else if let Some(pointee_ty) = lvalue.ty.get_pointer_inner() {
            self.emit_array_index_on_pointer(
                lvalue.as_basic_value().into_pointer_value(),
                index_rvalue,
                pointee_ty.clone(),
            )
        } else {
            unreachable!("Expected array or pointer type for array indexing expression");
        }
    }

    pub(crate) fn emit_assign(&mut self, assign: &CIRAssignExpr) -> InternalValue<'ll> {
        let lhs_lvalue = match &assign.lhs.kind {
            CIRExprKind::Deref(deref_expr) => self.emit_deref(deref_expr, DerefMode::Store),
            _ => self.emit_expr(&assign.lhs),
        };

        let rhs_lvalue = self.emit_expr(&assign.rhs);
        let rhs_value = self.load_rvalue(rhs_lvalue);

        self.llvmbuilder
            .build_store(
                lhs_lvalue.as_basic_value().into_pointer_value(),
                rhs_value.as_basic_value(),
            )
            .unwrap();

        rhs_value
    }

    pub(crate) fn emit_cast(&self, target_type: AnyTypeEnum<'ll>, value: InternalValue<'ll>) -> AnyValueEnum<'ll> {
        if let Some(fn_value) = value.as_func() {
            return self.emit_fn_as_ptr(*fn_value).into();
        }

        let basic_value = value.as_basic_value();

        match target_type {
            AnyTypeEnum::IntType(int_type) => {
                let val = basic_value;
                if val.is_int_value() {
                    // int -> int
                    AnyValueEnum::IntValue(
                        self.llvmbuilder
                            .build_int_cast(val.into_int_value(), int_type, "cast")
                            .unwrap(),
                    )
                } else if val.is_pointer_value() {
                    // ptr -> int
                    AnyValueEnum::IntValue(
                        self.llvmbuilder
                            .build_ptr_to_int(val.into_pointer_value(), int_type, "ptr_to_int")
                            .unwrap(),
                    )
                } else {
                    panic!("Invalid cast to int");
                }
            }

            AnyTypeEnum::FloatType(float_type) => AnyValueEnum::FloatValue(
                self.llvmbuilder
                    .build_float_cast(basic_value.into_float_value(), float_type, "cast")
                    .unwrap(),
            ),

            AnyTypeEnum::PointerType(ptr_type) => {
                let val = basic_value;
                if val.is_pointer_value() {
                    // ptr -> ptr
                    AnyValueEnum::PointerValue(
                        self.llvmbuilder
                            .build_pointer_cast(val.into_pointer_value(), ptr_type, "cast")
                            .unwrap(),
                    )
                } else if val.is_int_value() {
                    // int -> ptr
                    AnyValueEnum::PointerValue(
                        self.llvmbuilder
                            .build_int_to_ptr(val.into_int_value(), ptr_type, "int_to_ptr")
                            .unwrap(),
                    )
                } else {
                    val.into()
                }
            }

            _ => value.as_basic_value().into(),
        }
    }

    pub(crate) fn emit_implicit_cast(&self, target_type: &CIRTy, rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        let ty = self.emit_ty(target_type.clone());
        let casted = self.emit_cast(ty, rvalue);
        InternalValue::new(
            target_type.clone(),
            InternalValueKind::RValue(casted.try_into().unwrap()),
        )
    }

    pub(crate) fn emit_array(&mut self, array: &CIRArrayExpr) -> InternalValue<'ll> {
        let cir_arr_ty = array.ty.as_arr_ty().expect("Expected array type");
        let element_ty = cir_arr_ty.ty.clone();

        let arr_ty: ArrayType<'ll> = self.emit_arr_ty(cir_arr_ty).try_into().expect("Expected ArrayType");

        let required_len = array.elms.len();
        let mut elements = Vec::with_capacity(required_len);
        let mut all_const = true;

        for expr in &array.elms {
            let lvalue = self.emit_expr(expr);
            let rvalue = self.load_rvalue(lvalue);

            let casted = self.emit_implicit_cast(&element_ty, rvalue).as_basic_value();

            if !is_basic_value_constant(casted) {
                all_const = false;
            }

            elements.push(casted);
        }

        // zero-fill if array type is fixed-length and not fully initialized
        let element_basic_ty: BasicTypeEnum<'ll> = self
            .emit_ty(*element_ty.clone())
            .try_into()
            .expect("Expected BasicTypeEnum for element type");

        while elements.len() < arr_ty.len() as usize {
            elements.push(element_basic_ty.const_zero());
            all_const = false;
        }

        let array_value = if all_const {
            let mut val = arr_ty.get_undef();
            for (i, elem) in elements.iter().enumerate() {
                val = self
                    .llvmbuilder
                    .build_insert_value(val, *elem, i as u32, "insert")
                    .unwrap()
                    .into_array_value();
            }
            val
        } else {
            // build runtime array by inserting each element
            let mut value = arr_ty.get_undef();
            for (i, elem) in elements.iter().enumerate() {
                value = self
                    .llvmbuilder
                    .build_insert_value(value, *elem, i as u32, "insert")
                    .unwrap()
                    .into_array_value();
            }
            value
        };

        InternalValue::new(array.ty.clone(), InternalValueKind::RValue(array_value.into()))
    }

    pub(crate) fn emit_addr_of(&mut self, addr_of: &CIRAddrOfExpr) -> InternalValue<'ll> {
        let operand = self.emit_expr(&addr_of.operand);
        InternalValue::new(
            CIRTy::Pointer(Box::new(operand.ty.clone())),
            InternalValueKind::RValue(operand.as_basic_value()),
        )
    }

    pub(crate) fn emit_deref(&mut self, deref: &CIRDerefExpr, mode: DerefMode) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&deref.operand);
        let rvalue = self.load_rvalue(lvalue);
        let ptr = rvalue.as_basic_value().into_pointer_value();

        match mode {
            DerefMode::Load => {
                let inner_ty = rvalue.ty.get_pointer_inner().expect("Cannot deref non-pointer");
                let llvm_ty: BasicTypeEnum<'ll> = self.emit_ty(inner_ty.clone()).try_into().unwrap();
                let loaded_value = self.llvmbuilder.build_load(llvm_ty, ptr, "deref").unwrap();
                InternalValue::new(inner_ty.clone(), InternalValueKind::RValue(loaded_value.into()))
            }
            DerefMode::Store => {
                let inner_ty = rvalue.ty.get_pointer_inner().expect("Cannot deref non-pointer");
                InternalValue::new(inner_ty.clone(), InternalValueKind::LValue(ptr))
            }
        }
    }

    pub(crate) fn emit_sizeof(&mut self, sizeof_expr: &CIRSizeOfExpr) -> InternalValue<'ll> {
        let ty = self.emit_ty(sizeof_expr.ty.clone());
        let size_value = ty.size_of().unwrap();
        InternalValue::new(
            CIRTy::PlainType(PlainType::SizeT),
            InternalValueKind::RValue(size_value.into()),
        )
    }

    pub(crate) fn build_unary_expr(&mut self, unary_expr: &CIRUnaryExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&unary_expr.operand);
        let lvalue_pointer = lvalue.as_basic_value().into_pointer_value();
        let rvalue = self.load_rvalue(lvalue);

        let signed = rvalue.ty.as_plain().unwrap().is_signed();

        let unit_type = self.emit_ty(rvalue.ty.clone()).into_int_type();
        let unit_value = InternalValue::new(
            rvalue.ty.clone(),
            InternalValueKind::RValue(BasicValueEnum::IntValue(unit_type.const_int(1, signed))),
        );

        match unary_expr.op {
            UnaryOperator::PreIncrement => {
                let new_rhs_rvalue = self.build_add(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                new_rhs_rvalue
            }
            UnaryOperator::PreDecrement => {
                let new_rhs_rvalue = self.build_sub(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                new_rhs_rvalue
            }
            UnaryOperator::PostIncrement => {
                let rhs_rvalue_clone = rvalue.clone();
                let new_rhs_rvalue = self.build_add(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                rhs_rvalue_clone
            }
            UnaryOperator::PostDecrement => {
                let rhs_rvalue_clone = rvalue.clone();
                let new_rhs_rvalue = self.build_sub(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                rhs_rvalue_clone
            }
        }
    }

    pub(crate) fn emit_infix_expr(&mut self, infix_expr: &CIRInfixExpr) -> InternalValue<'ll> {
        let lhs_lvalue = self.emit_expr(&infix_expr.lhs);
        let rhs_lvalue = self.emit_expr(&infix_expr.rhs);
        let lhs_rvalue = self.load_rvalue(lhs_lvalue.clone());
        let rhs_rvalue = self.load_rvalue(rhs_lvalue.clone());

        let get_signed = || rhs_rvalue.ty.as_plain().unwrap().is_signed();

        match infix_expr.op {
            InfixOperator::Add => self.build_add(lhs_rvalue, rhs_rvalue),
            InfixOperator::Sub => self.build_sub(lhs_rvalue, rhs_rvalue),
            InfixOperator::Mul => self.build_mul(lhs_rvalue, rhs_rvalue),
            InfixOperator::Div => self.build_div(lhs_rvalue, rhs_rvalue),
            InfixOperator::Rem => self.build_rem(lhs_rvalue, rhs_rvalue),
            InfixOperator::LessThan => {
                if get_signed() {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SLT, FloatPredicate::OLT)
                } else {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::ULT, FloatPredicate::OLT)
                }
            }
            InfixOperator::LessEqual => {
                if get_signed() {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SLE, FloatPredicate::OLE)
                } else {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::ULE, FloatPredicate::OLE)
                }
            }
            InfixOperator::GreaterThan => {
                if get_signed() {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SGT, FloatPredicate::OGT)
                } else {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::UGT, FloatPredicate::OGT)
                }
            }
            InfixOperator::GreaterEqual => {
                if get_signed() {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SGE, FloatPredicate::OGE)
                } else {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::UGE, FloatPredicate::OGE)
                }
            }
            InfixOperator::Equal => self.build_cmp_eq(lhs_rvalue, rhs_rvalue),
            InfixOperator::NotEqual => self.build_cmp_neq(lhs_rvalue, rhs_rvalue),
            InfixOperator::Or => self.build_logical_or(lhs_rvalue, rhs_rvalue),
            InfixOperator::And => self.build_logical_and(lhs_rvalue, rhs_rvalue),
            InfixOperator::BitwiseAnd => self.build_bitwise_and(lhs_rvalue, rhs_rvalue),
            InfixOperator::BitwiseOr => self.build_bitwise_or(lhs_rvalue, rhs_rvalue),
            InfixOperator::BitwiseXor => self.build_xor(lhs_rvalue, rhs_rvalue),
            InfixOperator::BitwiseAndNot => self.build_bitwise_and_not(lhs_rvalue, rhs_rvalue),
            InfixOperator::ShiftLeft => self.build_shift_left(lhs_rvalue, rhs_rvalue),
            InfixOperator::ShiftRight => self.build_shift_right(lhs_rvalue, rhs_rvalue),
        }
    }

    pub(crate) fn build_logical_or(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let or_value = self.llvmbuilder.build_or(lhs, rhs, "lor").unwrap();
                InternalValue::new(
                    CIRTy::PlainType(PlainType::Bool),
                    InternalValueKind::RValue(or_value.into()),
                )
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                self.build_null_coalescing_pointers(lhs, rhs)
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_null_coalescing_pointers(
        &self,
        lhs: PointerValue<'ll>,
        rhs: PointerValue<'ll>,
    ) -> InternalValue<'ll> {
        // cond: lhs == null
        let is_null = self
            .llvmbuilder
            .build_is_null(lhs, "lhs_is_null")
            .expect("icmp eq null");

        let selected = self
            .llvmbuilder
            .build_select(is_null, rhs, lhs, "null_coalesce")
            .expect("select")
            .into_pointer_value();

        InternalValue::new(
            CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))),
            InternalValueKind::RValue(selected.into()),
        )
    }

    pub(crate) fn build_logical_and(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvmbuilder.build_and(lhs, rhs, "land").unwrap();
                InternalValue::new(
                    CIRTy::PlainType(PlainType::Bool),
                    InternalValueKind::RValue(and_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_xor(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvmbuilder.build_xor(lhs, rhs, "xor").unwrap();
                InternalValue::new(
                    CIRTy::PlainType(PlainType::Bool),
                    InternalValueKind::RValue(and_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_bitwise_and(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvmbuilder.build_and(lhs, rhs, "xor").unwrap();
                InternalValue::new(
                    CIRTy::PlainType(PlainType::Bool),
                    InternalValueKind::RValue(and_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_bitwise_or(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvmbuilder.build_or(lhs, rhs, "or").unwrap();
                InternalValue::new(
                    CIRTy::PlainType(PlainType::Bool),
                    InternalValueKind::RValue(and_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_bitwise_and_not(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                // ~rhs = rhs XOR all ones
                let all_ones = rhs.get_type().const_all_ones();
                let not_rhs = self.llvmbuilder.build_xor(rhs, all_ones, "not_rhs").unwrap();

                // lhs AND (~rhs)
                let and_not_value = self.llvmbuilder.build_and(lhs, not_rhs, "and_not").unwrap();

                InternalValue::new(
                    CIRTy::PlainType(PlainType::Int), // result is integer, not Bool
                    InternalValueKind::RValue(and_not_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_shift_left(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let shift_value = self.llvmbuilder.build_left_shift(lhs, rhs, "lshift").unwrap();
                InternalValue::new(
                    CIRTy::PlainType(PlainType::Bool),
                    InternalValueKind::RValue(shift_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_shift_right(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let signed = rhs_rvalue.ty.as_plain().unwrap().is_signed();

                let shift_value = self.llvmbuilder.build_right_shift(lhs, rhs, signed, "lshift").unwrap();
                InternalValue::new(
                    CIRTy::PlainType(PlainType::Bool),
                    InternalValueKind::RValue(shift_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_add(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_int_add(lhs, rhs, "add").unwrap());
                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_add(lhs, rhs, "add").unwrap());
                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_sub(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_int_sub(lhs, rhs, "sub").unwrap());
                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_sub(lhs, rhs, "sub").unwrap());
                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_mul(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_int_mul(lhs, rhs, "mul").unwrap());
                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_mul(lhs, rhs, "mul").unwrap());
                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_div(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::IntValue(self.llvmbuilder.build_int_signed_div(lhs, rhs, "div").unwrap());
                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_div(lhs, rhs, "div").unwrap());
                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_rem(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::IntValue(self.llvmbuilder.build_int_signed_rem(lhs, rhs, "rem").unwrap());
                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_rem(lhs, rhs, "rem").unwrap());
                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_cmp(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
        int_pred: IntPredicate,
        float_pred: FloatPredicate,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let cmp = self.llvmbuilder.build_int_compare(int_pred, lhs, rhs, "cmp").unwrap();
                InternalValue::new(CIRTy::PlainType(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_float_compare(float_pred, lhs, rhs, "cmp")
                    .unwrap();
                InternalValue::new(CIRTy::PlainType(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_cmp_eq(
        &mut self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
                    .unwrap();
                InternalValue::new(CIRTy::PlainType(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "eq")
                    .unwrap();
                InternalValue::new(CIRTy::PlainType(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
                    .unwrap();
                InternalValue::new(CIRTy::PlainType(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            _ => {
                if lhs_rvalue.ty.is_enum() && rhs_rvalue.ty.is_enum() {
                    return self.emit_compare_enum_variants(lhs_rvalue.clone(), rhs_rvalue.clone(), true);
                }

                unreachable!()
            }
        }
    }

    pub(crate) fn build_cmp_neq(
        &mut self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "neq")
                    .unwrap();
                InternalValue::new(CIRTy::PlainType(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_float_compare(FloatPredicate::ONE, lhs, rhs, "neq")
                    .unwrap();
                InternalValue::new(CIRTy::PlainType(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "neq")
                    .unwrap();
                InternalValue::new(CIRTy::PlainType(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            _ => {
                if lhs_rvalue.ty.is_enum() && rhs_rvalue.ty.is_enum() {
                    return self.emit_compare_enum_variants(lhs_rvalue.clone(), rhs_rvalue.clone(), false);
                }

                unreachable!()
            }
        }
    }

    pub(crate) fn emit_prefix_expr(&mut self, prefix_expr: &CIRPrefixExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&prefix_expr.operand);
        let rvalue = self.load_rvalue(lvalue);

        match prefix_expr.op {
            PrefixOperator::Bang => self.build_logical_not(rvalue),
            PrefixOperator::Minus => self.build_negate(rvalue),
            PrefixOperator::BitwiseNot => self.build_bitwise_not(rvalue),
        }
    }

    pub(crate) fn build_bitwise_not(&self, rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match rvalue.as_basic_value() {
            BasicValueEnum::IntValue(int_value) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_not(int_value, "neg").unwrap());
                InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_negate(&self, rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match rvalue.as_basic_value() {
            BasicValueEnum::IntValue(int_value) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_int_neg(int_value, "neg").unwrap());
                InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            BasicValueEnum::FloatValue(float_value) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_neg(float_value, "neg").unwrap());
                InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_logical_not(&self, rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match rvalue.as_basic_value() {
            BasicValueEnum::IntValue(int_value) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_not(int_value, "neg").unwrap());
                InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn emit_union_field_access(&mut self, field_access: &CIRUnionFieldAccessExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&field_access.operand);
        let ptr = lvalue.as_basic_value().into_pointer_value();
        InternalValue::new(field_access.field_ty.clone(), InternalValueKind::LValue(ptr.into()))
    }

    pub(crate) fn emit_struct_field_access(&mut self, field_access: &CIRStructFieldAccessExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&field_access.operand);

        let ptr = lvalue.as_basic_value().into_pointer_value();
        let pointee_ty: BasicTypeEnum<'ll> = self.emit_ty(field_access.operand.ty.clone()).try_into().unwrap();
        let field_value: BasicValueEnum<'ll> = self
            .llvmbuilder
            .build_struct_gep(
                pointee_ty,
                ptr,
                field_access.field_idx.try_into().unwrap(),
                "struct_gep",
            )
            .unwrap()
            .into();

        InternalValue::new(
            field_access.field_ty.clone(),
            InternalValueKind::LValue(field_value.into_pointer_value()),
        )
    }

    pub(crate) fn emit_tuple_access(&mut self, tuple_access: &CIRTupleAccessExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&tuple_access.operand);
        let rvalue = self.load_rvalue(lvalue);
        let struct_value = rvalue.as_basic_value().into_struct_value();

        let extracted_value = self
            .llvmbuilder
            .build_extract_value(struct_value, tuple_access.index.try_into().unwrap(), "extractvalue")
            .unwrap();

        InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(extracted_value))
    }

    pub(crate) fn emit_tuple(&mut self, tuple: &CIRTupleExpr) -> InternalValue<'ll> {
        let tys: Vec<CIRTy> = tuple.elms.iter().map(|elm| elm.ty.clone()).collect();

        let struct_value = self
            .emit_struct_init(&CIRStructInitExpr {
                ty: CIRStructTy {
                    fields: tys.clone(),
                    is_packed: false,
                },
                fields: tuple.elms.clone(),
            })
            .as_basic_value()
            .into_struct_value();

        InternalValue::new(
            CIRTy::Tuple(CIRTupleTy { items: tys }),
            InternalValueKind::RValue(struct_value.into()),
        )
    }

    pub(crate) fn emit_enum_init(&mut self, enum_init_expr: &CIREnumInitExpr) -> InternalValue<'ll> {
        let enum_ty = &enum_init_expr.enum_ty;

        let enum_struct_ty = self.emit_enum_ty(enum_ty.clone());
        let (payload_ty, _) = self.enum_payload_ty(enum_ty);

        let mut enum_value = enum_struct_ty.get_undef();

        let tag_val = self.llvmctx.i32_type().const_int(enum_init_expr.tag as u64, false);

        enum_value = self
            .llvmbuilder
            .build_insert_value(enum_value, tag_val, 0, "enum.set_tag")
            .expect("insert tag")
            .into_struct_value();

        match &enum_init_expr.variant {
            CIREnumInitVariant::Identifier => {
                let zero_payload = payload_ty.const_zero();
                enum_value = self
                    .llvmbuilder
                    .build_insert_value(enum_value, zero_payload, 1, "enum.zero_payload")
                    .expect("insert zero payload")
                    .into_struct_value();
            }

            CIREnumInitVariant::Valued(expr) => {
                let lvalue = self.emit_expr(expr);
                let rvalue = self.load_rvalue(lvalue);

                let copied_payload = self.intrinsic_copy_payload_to_buffer(rvalue.as_basic_value(), payload_ty);

                enum_value = self
                    .llvmbuilder
                    .build_insert_value(enum_value, copied_payload, 1, "enum.set_payload")
                    .expect("insert payload")
                    .into_struct_value();
            }

            CIREnumInitVariant::Fielded(field_exprs) => {
                let field_basic_tys: Vec<BasicTypeEnum<'ll>> = field_exprs
                    .iter()
                    .map(|fld| {
                        self.emit_ty(fld.ty.clone())
                            .try_into()
                            .expect("field must be basic type")
                    })
                    .collect();

                let payload_struct_ty = self.llvmctx.struct_type(&field_basic_tys, false);

                let mut payload_value = payload_struct_ty.get_undef();
                for (idx, field_expr) in field_exprs.iter().enumerate() {
                    let lvalue = self.emit_expr(&field_expr);
                    let rvalue = self.load_rvalue(lvalue);

                    payload_value = self
                        .llvmbuilder
                        .build_insert_value(payload_value, rvalue.as_basic_value(), idx as u32, "payload.insert")
                        .expect("insert field into payload")
                        .into_struct_value();
                }

                let copied_payload =
                    self.intrinsic_copy_payload_to_buffer(payload_value.as_basic_value_enum(), payload_ty);

                enum_value = self
                    .llvmbuilder
                    .build_insert_value(enum_value, copied_payload, 1, "enum.set_payload")
                    .expect("insert payload")
                    .into_struct_value();
            }
        }

        InternalValue::new(
            CIRTy::Enum(enum_init_expr.enum_ty.clone()),
            InternalValueKind::RValue(enum_value.as_basic_value_enum()),
        )
    }

    pub(crate) fn emit_union_init(&mut self, union_init_expr: &CIRUnionInitExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&union_init_expr.expr);
        self.load_rvalue(lvalue)
    }

    pub(crate) fn emit_struct_init(&mut self, struct_init: &CIRStructInitExpr) -> InternalValue<'ll> {
        let field_types: Vec<BasicTypeEnum<'ll>> = struct_init
            .ty
            .fields
            .iter()
            .map(|ty| self.emit_ty(ty.clone()).try_into().unwrap())
            .collect();

        let struct_type = self.llvmctx.struct_type(&field_types, struct_init.ty.is_packed);

        let mut all_const = true;

        let values: Vec<BasicValueEnum<'ll>> = struct_init
            .fields
            .iter()
            .enumerate()
            .map(|(idx, expr)| {
                let lvalue = self.emit_expr(expr);
                let rvalue = self.load_rvalue(lvalue);

                let target_type = struct_init.ty.fields.get(idx).unwrap();
                let casted = self.emit_implicit_cast(target_type, rvalue).as_basic_value();

                if !is_basic_value_constant(casted) {
                    all_const = false;
                }
                casted
            })
            .collect();

        let struct_value: StructValue<'ll>;

        if all_const {
            struct_value = struct_type.const_named_struct(&values);
        } else {
            struct_value = struct_type.get_undef();

            values.iter().enumerate().for_each(|(index, rvalue)| {
                self.llvmbuilder
                    .build_insert_value(struct_value, *rvalue, index.try_into().unwrap(), "insert")
                    .unwrap()
                    .into_struct_value();
            });
        }

        InternalValue::new(
            CIRTy::Struct(struct_init.ty.clone()),
            InternalValueKind::RValue(struct_value.into()),
        )
    }

    pub(crate) fn emit_monomorph_func_instance_call(
        &mut self,
        monomorph_func_instance_call: &CIRMonomorphFuncInstanceCall,
    ) -> InternalValue<'ll> {
        let (fn_value, cir_func_ty) = self.emit_monomorph_func_instance(&monomorph_func_instance_call.monomorph_key);

        self.emit_direct_call(
            &cir_func_ty,
            &monomorph_func_instance_call.args,
            &monomorph_func_instance_call.ret_ty,
            &fn_value,
        )
    }

    pub(crate) fn emit_func_call(&mut self, func_call: &CIRFuncCall) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&func_call.operand);
        let rvalue = self.load_rvalue(lvalue);

        // check if it's a direct or indirect call
        if let Some(fn_value) = rvalue.as_func() {
            self.emit_direct_call(
                &rvalue.ty.as_fn().unwrap(),
                &func_call.args,
                &func_call.ret_ty,
                fn_value,
            )
        } else if rvalue.as_basic_value().is_pointer_value() {
            self.emit_indirect_call(func_call)
        } else {
            panic!("Expected a function or pointer to function in call expression.")
        }
    }

    pub(crate) fn emit_direct_call(
        &mut self,
        fn_ty: &CIRFuncTy,
        args: &Vec<CIRExpr>,
        ret_ty: &CIRTy,
        fn_value: &FunctionValue<'ll>,
    ) -> InternalValue<'ll> {
        let args: Vec<BasicMetadataValueEnum<'ll>> = args
            .iter()
            .enumerate()
            .map(|(idx, expr)| {
                let lvalue = self.emit_expr(expr);
                let rvalue = self.load_rvalue(lvalue);

                if let Some(cir_ty) = fn_ty.params.get(idx) {
                    self.emit_implicit_cast(cir_ty, rvalue).as_basic_value().into()
                } else {
                    rvalue.as_basic_value().into()
                }
            })
            .collect();

        let call_site = self.llvmbuilder.build_call(*fn_value, &args, "call").unwrap();

        if let Some(basic_value) = call_site.try_as_basic_value().basic() {
            InternalValue::new(ret_ty.clone(), InternalValueKind::RValue(basic_value))
        } else {
            self.emit_null(ret_ty.clone())
        }
    }

    pub(crate) fn emit_indirect_call(&mut self, func_call: &CIRFuncCall) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&func_call.operand);
        let rvalue = self.load_rvalue(lvalue);

        let fn_ty = self.emit_func_ty(rvalue.ty.as_fn().unwrap());
        let fn_ptr = rvalue.as_basic_value().into_pointer_value();

        let args: Vec<BasicMetadataValueEnum<'ll>> = func_call
            .args
            .iter()
            .map(|expr| {
                let lvalue = self.emit_expr(expr);
                self.load_rvalue(lvalue).as_basic_value().into()
            })
            .collect();

        let call_site = self
            .llvmbuilder
            .build_indirect_call(fn_ty, fn_ptr, &args, "indirect_call")
            .unwrap();

        if let Some(basic_value) = call_site.try_as_basic_value().basic() {
            InternalValue::new(func_call.ret_ty.clone(), InternalValueKind::RValue(basic_value))
        } else {
            self.emit_null(func_call.ret_ty.clone())
        }
    }

    pub(crate) fn emit_load(&mut self, value_ref: &CIRValue) -> InternalValue<'ll> {
        {
            let irreg = self.irreg.borrow();
            if let Some(value_ref) = irreg.get(value_ref.irv_id) {
                let internal_value = match value_ref {
                    LocalIRValue::Func(fn_value, ty) => InternalValue::new(ty, InternalValueKind::FuncValue(fn_value)),
                    LocalIRValue::Global(global_value, ty) => {
                        InternalValue::new(ty, InternalValueKind::LValue(global_value.as_pointer_value()))
                    }
                    LocalIRValue::LValue(pointer_value, ty) => {
                        InternalValue::new(ty, InternalValueKind::LValue(pointer_value))
                    }
                };

                return internal_value;
            }
        }

        // fresh declaration

        match &value_ref.kind {
            CIRValueKind::Func(func_decl) => {
                let fn_value = self.emit_func_decl(func_decl);
                let cir_fn_ty = cir_func_decl_as_func_ty(&func_decl);
                InternalValue::new(CIRTy::FuncType(cir_fn_ty), InternalValueKind::FuncValue(fn_value))
            }
            CIRValueKind::GlobalVar(global_var) => {
                let global_value = self.emit_global_var(global_var);
                InternalValue::new(
                    global_var.ty.clone(),
                    InternalValueKind::LValue(global_value.as_pointer_value()),
                )
            }
            CIRValueKind::LocalVariable => unreachable!("Couldn't fetch local variable from local ir value registry."),
        }
    }

    pub(crate) fn emit_literal(&self, lit: &CIRLiteral) -> InternalValue<'ll> {
        let ty: BasicTypeEnum<'ll> = self.emit_ty(lit.ty.clone()).try_into().unwrap();

        let basic_value = match &lit.kind {
            CIRLiteralKind::Bool(value) => {
                BasicValueEnum::IntValue(self.llvmctx.bool_type().const_int(*value as u64, false))
            }
            CIRLiteralKind::Integer(value, is_signed) => {
                BasicValueEnum::IntValue(ty.into_int_type().const_int((*value).try_into().unwrap(), *is_signed))
            }
            CIRLiteralKind::Float(value) => BasicValueEnum::FloatValue(ty.into_float_type().const_float(*value)),
            CIRLiteralKind::Char(value) => {
                BasicValueEnum::IntValue(self.llvmctx.i8_type().const_int(*value as u64, false))
            }
            CIRLiteralKind::Null => {
                BasicValueEnum::PointerValue(self.llvmctx.ptr_type(AddressSpace::default()).const_null())
            }
            CIRLiteralKind::CString(value) => self.emit_cstring(value.clone()),
            CIRLiteralKind::ByteString(value) => self.emit_bytestring(value.clone()),
        };

        InternalValue::new(lit.ty.clone(), InternalValueKind::RValue(basic_value))
    }

    pub(crate) fn emit_null(&self, ty: CIRTy) -> InternalValue<'ll> {
        let basic_value = BasicValueEnum::PointerValue(self.llvmctx.ptr_type(AddressSpace::default()).const_null());
        InternalValue::new(ty, InternalValueKind::RValue(basic_value))
    }

    pub(crate) fn emit_cstring(&self, value: String) -> BasicValueEnum<'ll> {
        let const_str = self.llvmctx.const_string(value.as_bytes(), true);

        let llvmmodule = self.llvmmodule.borrow();
        let global_str = llvmmodule.add_global(const_str.get_type(), None, ".cstring");
        global_str.set_initializer(&const_str);
        global_str.set_constant(true);
        global_str.set_unnamed_addr(true);
        global_str.set_linkage(Linkage::Private);
        global_str.set_alignment(1);
        drop(llvmmodule);

        global_str.as_pointer_value().into()
    }

    pub(crate) fn emit_bytestring(&self, value: String) -> BasicValueEnum<'ll> {
        self.llvmctx.const_string(value.as_bytes(), true).into()
    }
}
