// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    builder::{
        builder::CodeGenIRBuilder,
        funcs::FuncCallKind,
        irreg::LocalIRValue,
        values::{InternalValue, InternalValueKind},
    },
    llvm::{constness::is_basic_value_constant, debug_info::set_debug_location},
};
use cyrusc_ast::operators::{InfixOperator, PrefixOperator};
use cyrusc_internal::{
    abi::{
        args::ABIFunctionInfo,
        layout::{ABIFieldOffsetInfo, ABITypeLayout},
    },
    cir::{
        cir::*,
        types::{CIRArrayType, CIREnumType, CIRFuncType, CIRType, CIRUnionType},
    },
    compiler_options::CompilerOption_Profile,
};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::types::PlainType;
use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    module::Linkage,
    types::{AnyTypeEnum, ArrayType, BasicType, BasicTypeEnum, StructType},
    values::{
        AggregateValueEnum, AnyValueEnum, ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum,
        FunctionValue, IntValue, PointerValue, StructValue,
    },
};

#[allow(unused)]
pub enum DerefMode {
    Load,  // for RValue
    Store, // for LHS assignment
}

impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_expr(&mut self, expr: &CIRExpr, target_cir_type: &Option<CIRType>) -> InternalValue<'ll> {
        let value = match &expr.kind {
            CIRExprKind::Load(value_ref) => self.emit_load(value_ref),
            CIRExprKind::Literal(literal) => self.emit_literal(literal),
            CIRExprKind::Prefix(prefix_expr) => self.emit_prefix_expr(prefix_expr),
            CIRExprKind::Infix(infix_expr) => self.emit_infix_expr(infix_expr, expr.loc),
            CIRExprKind::Assign(assign_expr) => self.emit_assign(assign_expr),
            CIRExprKind::AddrOf(addr_of_expr) => self.emit_addr_of(addr_of_expr),
            CIRExprKind::Deref(deref_expr) => self.emit_deref(deref_expr, DerefMode::Load),
            CIRExprKind::Array(array_expr) => self.emit_array(array_expr),
            CIRExprKind::ArrayIndex(array_index_expr) => self.emit_array_index(array_index_expr),
            CIRExprKind::Tuple(tuple_expr) => self.emit_tuple(tuple_expr),
            CIRExprKind::TupleAccess(tuple_access) => self.emit_tuple_access(tuple_access),
            CIRExprKind::StructInit(struct_init_expr) => self.emit_struct_init(struct_init_expr),
            CIRExprKind::UnionInit(union_init_expr) => self.emit_union_init_value(union_init_expr),
            CIRExprKind::EnumInit(enum_init_expr) => self.emit_enum_init(enum_init_expr),
            CIRExprKind::FieldAccess(field_access) => self.emit_field_access(field_access),
            CIRExprKind::Call(call) => self.emit_call(call),
            CIRExprKind::Lambda(lambda) => self.emit_lambda(lambda),
            CIRExprKind::Dynamic(dynamic) => self.emit_dynamic_expr(dynamic),
            CIRExprKind::InlineAsm(inline_asm) => self.emit_inline_asm(inline_asm),

            CIRExprKind::Type(_) => unreachable!(),
        };

        if let Some(dctx) = &self.dctx {
            unsafe {
                set_debug_location(
                    &dctx,
                    self.llvm_ctx,
                    self.llvm_builder,
                    expr.loc.line.try_into().unwrap(),
                    expr.loc.column.try_into().unwrap(),
                )
            };
        }

        if let Some(ty) = target_cir_type {
            if ty.is_pointer() && value.ty.is_array() {
                return self.emit_decay_array_to_pointer(value);
            }
        }

        value
    }

    fn emit_load(&mut self, value_ref: &CIRValue) -> InternalValue<'ll> {
        if let Some(value_ref) = self.lookup_local_ir_value(value_ref.irv_id) {
            let internal_value = match value_ref {
                LocalIRValue::Func(llvm_func_value, ty) => {
                    InternalValue::new(ty, InternalValueKind::FuncValue(llvm_func_value))
                }
                LocalIRValue::Global(global_value, ty) => {
                    InternalValue::new(ty, InternalValueKind::LValue(global_value.as_pointer_value()))
                }
                LocalIRValue::LValue(pointer_value, ty) => {
                    InternalValue::new(ty, InternalValueKind::LValue(pointer_value))
                }
                LocalIRValue::RValue(val, ty) => InternalValue::new(ty, InternalValueKind::RValue(val)),
            };

            return internal_value;
        }

        // declare
        match &value_ref.kind {
            CIRValueKind::Func => self.get_or_declare_function(value_ref.irv_id),
            CIRValueKind::GlobalVar => self.get_or_declare_global(value_ref.irv_id),
            CIRValueKind::LocalVariable => unreachable!(),
        }
    }

    fn emit_dynamic_expr(&mut self, dynamic: &CIRDynamicExpr) -> InternalValue<'ll> {
        let data_value = self.emit_expr(&dynamic.data_expr, &None);
        let data_basic_value = data_value.as_basic_value();

        let data_ptr = if data_basic_value.is_pointer_value() {
            data_basic_value.into_pointer_value()
        } else {
            // value is not addressable to allocate temp
            let temp = self
                .llvm_builder
                .build_alloca(data_basic_value.get_type(), "dyn.tmp")
                .unwrap();

            self.llvm_builder.build_store(temp, data_basic_value).unwrap();

            temp
        };

        let vtable_ir_value = self.lookup_local_ir_value(dynamic.vtable_irv_id).unwrap();

        let vtable_global = vtable_ir_value.as_global().unwrap();
        let vtable_ptr = vtable_global.as_pointer_value();

        // construct pointers struct { data_ptr, vtable_ptr }
        let dynamic_struct_type = self.emit_dynamic_type(); // { ptr, ptr }

        let mut dynamic_value = dynamic_struct_type.const_zero();

        dynamic_value = self
            .llvm_builder
            .build_insert_value(dynamic_value, data_ptr.as_basic_value_enum(), 0, "dyn.insert.data")
            .unwrap()
            .into_struct_value();

        dynamic_value = self
            .llvm_builder
            .build_insert_value(dynamic_value, vtable_ptr.as_basic_value_enum(), 1, "dyn.insert.vtable")
            .unwrap()
            .into_struct_value();

        let fat_ptr_type = self.tctx.fat_ptr_type();

        InternalValue::new(
            fat_ptr_type,
            InternalValueKind::RValue(dynamic_value.as_basic_value_enum()),
        )
    }

    pub(crate) fn emit_array_index_on_pointer(
        &mut self,
        lvalue: PointerValue<'ll>,
        index: InternalValue<'ll>,
        cir_elm_ty: CIRType,
    ) -> InternalValue<'ll> {
        let element_type: BasicTypeEnum<'ll> = self.emit_type(cir_elm_ty.clone()).try_into().unwrap();
        let index_int = index.as_basic_value().into_int_value();

        let element_ptr: PointerValue<'ll> = unsafe {
            self.llvm_builder
                .build_in_bounds_gep(element_type, lvalue, &[index_int], "index")
                .unwrap()
        };

        InternalValue::new(cir_elm_ty, InternalValueKind::LValue(element_ptr))
    }

    fn emit_array_index(&mut self, array_index: &CIRArrayIndexExpr) -> InternalValue<'ll> {
        let base_addr = self.emit_lvalue_address(&array_index.operand);

        let index_lvalue = self.emit_expr(&array_index.index, &None);
        let index_rvalue = self.load_rvalue(index_lvalue);

        if base_addr.ty.as_array().is_some() {
            let array_type = base_addr.ty.as_array().unwrap();
            let basic_value = base_addr.as_basic_value();

            if basic_value.is_pointer_value() {
                self.emit_inbounds_checked_array_index(
                    base_addr.as_basic_value().into_pointer_value(),
                    *array_type.element_type.clone(),
                    index_rvalue,
                    array_type.len.try_into().unwrap(),
                )
            } else if basic_value.is_array_value() {
                let ptr = self.emit_temp_array_value_alloca(&basic_value.into_array_value());

                self.emit_inbounds_checked_array_index(
                    ptr, // use temp alloca instead
                    *array_type.element_type.clone(),
                    index_rvalue,
                    array_type.len.try_into().unwrap(),
                )
            } else {
                unreachable!("expected array or pointer type for array indexing expression");
            }
        } else if let Some(pointee_ty) = base_addr.ty.pointer_inner().cloned() {
            let array_ptr = self.load_rvalue(base_addr).as_basic_value().into_pointer_value();

            self.emit_array_index_on_pointer(array_ptr, index_rvalue, pointee_ty.clone())
        } else {
            unreachable!("expected array or pointer type for array indexing expression");
        }
    }

    fn emit_temp_array_value_alloca(&self, array_value: &ArrayValue<'ll>) -> PointerValue<'ll> {
        let ptr = self.llvm_builder.build_alloca(array_value.get_type(), "temp").unwrap();
        self.llvm_builder
            .build_store(ptr, array_value.as_basic_value_enum())
            .unwrap();
        ptr
    }

    fn emit_assign(&mut self, assign: &CIRAssignExpr) -> InternalValue<'ll> {
        let lhs_lvalue = self.emit_lvalue_address(&assign.lhs);

        let rhs_lvalue = self.emit_expr(&assign.rhs, &Some(assign.lhs.ty.clone()));
        let rhs_value = self.load_rvalue(rhs_lvalue);

        assert!(lhs_lvalue.as_basic_value().is_pointer_value());

        let lhs_ptr = lhs_lvalue.as_basic_value().into_pointer_value();

        self.llvm_builder
            .build_store(lhs_ptr, rhs_value.as_basic_value())
            .unwrap();

        if let CIRExprKind::Load(value_ref) = &assign.lhs.kind {
            if let Some(LocalIRValue::RValue(_, _)) = self.lookup_local_ir_value(value_ref.irv_id) {
                self.insert_local_ir_value(
                    value_ref.irv_id,
                    LocalIRValue::RValue(rhs_value.as_basic_value(), assign.lhs.ty.clone()),
                );
            }
        }

        rhs_value
    }

    pub(crate) fn emit_cast_func_arg(
        &self,
        value: BasicValueEnum<'ll>,
        from_cir_type: &CIRType,
        target_type: CIRType,
    ) -> BasicValueEnum<'ll> {
        let from_type = value.get_type();
        let target_basic_type: BasicTypeEnum<'ll> = self.emit_type(target_type.clone()).try_into().unwrap();

        if from_type == target_basic_type {
            return value;
        }

        if let CIRType::Enum(type_id) = from_cir_type {
            let enum_type = self.tctx.get_enum(*type_id);

            if !enum_type.includes_payload() {
                if let BasicValueEnum::StructValue(struct_value) = value {
                    let tag = self.extract_enum_tag(struct_value);
                    return self.emit_cast_func_arg(tag.into(), &CIRType::Plain(PlainType::Int32), target_type);
                }
            }
        }

        match (from_type, target_basic_type) {
            (BasicTypeEnum::IntType(from_int), BasicTypeEnum::IntType(to_int)) => {
                let from_width = from_int.get_bit_width();
                let to_width = to_int.get_bit_width();

                if from_width < to_width {
                    if from_cir_type.is_signed_integer() {
                        self.llvm_builder
                            .build_int_s_extend(value.into_int_value(), to_int, "sext")
                            .unwrap()
                            .into()
                    } else {
                        self.llvm_builder
                            .build_int_z_extend(value.into_int_value(), to_int, "zext")
                            .unwrap()
                            .into()
                    }
                } else if from_width > to_width {
                    self.llvm_builder
                        .build_int_truncate(value.into_int_value(), to_int, "trunc")
                        .unwrap()
                        .into()
                } else {
                    self.llvm_builder
                        .build_bit_cast(value, target_basic_type, "bitcast")
                        .unwrap()
                }
            }

            (BasicTypeEnum::PointerType(_), BasicTypeEnum::IntType(to_int)) => self
                .llvm_builder
                .build_ptr_to_int(value.into_pointer_value(), to_int, "ptr_to_int")
                .unwrap()
                .into(),

            (BasicTypeEnum::IntType(_), BasicTypeEnum::PointerType(to_ptr)) => self
                .llvm_builder
                .build_int_to_ptr(value.into_int_value(), to_ptr, "int_to_ptr")
                .unwrap()
                .into(),

            (BasicTypeEnum::FloatType(_), BasicTypeEnum::FloatType(to_float)) => self
                .llvm_builder
                .build_float_cast(value.into_float_value(), to_float, "fpext")
                .unwrap()
                .into(),

            (BasicTypeEnum::IntType(_), BasicTypeEnum::FloatType(to_float)) => {
                if from_cir_type.is_signed_integer() {
                    self.llvm_builder
                        .build_signed_int_to_float(value.into_int_value(), to_float, "sitofp")
                        .unwrap()
                        .into()
                } else {
                    self.llvm_builder
                        .build_unsigned_int_to_float(value.into_int_value(), to_float, "uitofp")
                        .unwrap()
                        .into()
                }
            }

            (BasicTypeEnum::PointerType(_), BasicTypeEnum::PointerType(_)) => value,

            (BasicTypeEnum::VectorType(_), BasicTypeEnum::VectorType(_)) => self
                .llvm_builder
                .build_bit_cast(value, target_basic_type, "bitcast")
                .unwrap(),
            _ => {
                let from_size = from_type.size_of();
                let to_size = target_basic_type.size_of();

                if from_size == to_size {
                    self.llvm_builder
                        .build_bit_cast(value, target_basic_type, "bitcast")
                        .unwrap()
                } else {
                    // fallback
                    self.intrinsic_coerce_through_alloca(value, target_basic_type, "cast_func_arg")
                }
            }
        }
    }

    pub(crate) fn emit_cast(&self, target_type: AnyTypeEnum<'ll>, value: InternalValue<'ll>) -> AnyValueEnum<'ll> {
        let mut basic_value = value.as_basic_value();

        match target_type {
            AnyTypeEnum::IntType(int_type) => {
                if basic_value.is_int_value() {
                    let bit_width = basic_value.into_int_value().get_type().get_bit_width();

                    if bit_width == 1 {
                        AnyValueEnum::IntValue(
                            self.llvm_builder
                                .build_int_z_extend(basic_value.into_int_value(), int_type, "bool_zext")
                                .unwrap(),
                        )
                    } else {
                        // int -> int
                        AnyValueEnum::IntValue(
                            self.llvm_builder
                                .build_int_cast(basic_value.into_int_value(), int_type, "cast")
                                .unwrap(),
                        )
                    }
                } else if basic_value.is_pointer_value() {
                    // ptr -> int
                    let ptr_width = self.llvm_target_machine.get_target_data().get_pointer_byte_size(None) * 8;

                    if int_type.get_bit_width() < ptr_width {
                        let ptr_int = self.llvm_ctx.custom_width_int_type(ptr_width);
                        let tmp = self
                            .llvm_builder
                            .build_ptr_to_int(basic_value.into_pointer_value(), ptr_int, "ptr_to_int")
                            .unwrap();
                        AnyValueEnum::IntValue(
                            self.llvm_builder
                                .build_int_truncate(tmp, int_type, "ptr_trunc")
                                .unwrap(),
                        )
                    } else {
                        AnyValueEnum::IntValue(
                            self.llvm_builder
                                .build_ptr_to_int(basic_value.into_pointer_value(), int_type, "ptr_to_int")
                                .unwrap(),
                        )
                    }
                } else {
                    basic_value.into()
                }
            }
            AnyTypeEnum::FloatType(float_type) => {
                if basic_value.is_int_value() {
                    let is_signed = value.ty.is_signed_integer();

                    if is_signed {
                        AnyValueEnum::FloatValue(
                            self.llvm_builder
                                .build_signed_int_to_float(basic_value.into_int_value(), float_type, "cast")
                                .unwrap(),
                        )
                    } else {
                        AnyValueEnum::FloatValue(
                            self.llvm_builder
                                .build_unsigned_int_to_float(basic_value.into_int_value(), float_type, "cast")
                                .unwrap(),
                        )
                    }
                } else if basic_value.is_float_value() {
                    AnyValueEnum::FloatValue(
                        self.llvm_builder
                            .build_float_cast(basic_value.into_float_value(), float_type, "cast")
                            .unwrap(),
                    )
                } else {
                    basic_value.into()
                }
            }
            AnyTypeEnum::PointerType(ptr_type) => {
                if basic_value.is_pointer_value() {
                    // ptr -> ptr
                    AnyValueEnum::PointerValue(basic_value.into_pointer_value())
                } else if basic_value.is_int_value() {
                    let is_signed = value.ty.is_signed_integer();
                    basic_value = self.widen_int_arg(value, is_signed).as_basic_value();

                    // int -> ptr
                    AnyValueEnum::PointerValue(
                        self.llvm_builder
                            .build_int_to_ptr(basic_value.into_int_value(), ptr_type, "int_to_ptr")
                            .unwrap(),
                    )
                } else {
                    basic_value.into()
                }
            }
            _ => basic_value.into(),
        }
    }

    pub(crate) fn emit_implicit_cast(&self, target_type: &CIRType, rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        let ty = self.emit_type(target_type.clone());
        let casted = self.emit_cast(ty, rvalue);
        InternalValue::new(
            target_type.clone(),
            InternalValueKind::RValue(casted.try_into().unwrap()),
        )
    }

    fn emit_array(&mut self, array: &CIRArrayExpr) -> InternalValue<'ll> {
        let cir_array_type = array.ty.as_array().unwrap();
        let cir_element_type = cir_array_type.element_type.clone();

        let array_type: ArrayType<'ll> = self.emit_array_type(cir_array_type.clone()).try_into().unwrap();

        let required_len = array.elements.len();
        let mut elements = Vec::with_capacity(required_len);
        let mut all_const = true;

        for expr in &array.elements {
            let lvalue = self.emit_expr(expr, &Some(*cir_array_type.element_type.clone()));
            let mut rvalue = self.load_rvalue(lvalue);

            if !self.llvm_builder.get_insert_block().is_none() {
                rvalue = self.emit_implicit_cast(&cir_element_type, rvalue);
            }

            if !is_basic_value_constant(rvalue.as_basic_value()) {
                all_const = false;
            }

            elements.push(rvalue.as_basic_value());
        }

        // zero-fill if array type is fixed-length and not fully initialized
        let element_type: BasicTypeEnum<'ll> = self.emit_type(*cir_element_type.clone()).try_into().unwrap();

        while elements.len() < array_type.len() as usize {
            elements.push(element_type.const_zero());
            all_const = false;
        }

        let array_value = {
            if all_const {
                unsafe { ArrayValue::new_const_array(&element_type, &elements) }
            } else {
                // build runtime array by inserting each element
                let mut value = array_type.const_zero();
                for (i, elem) in elements.iter().enumerate() {
                    value = self
                        .llvm_builder
                        .build_insert_value(value, *elem, i as u32, "array.insert")
                        .unwrap()
                        .into_array_value();
                }
                value
            }
        };

        InternalValue::new(array.ty.clone(), InternalValueKind::RValue(array_value.into()))
    }

    fn emit_addr_of(&mut self, addr_of: &CIRAddrOfExpr) -> InternalValue<'ll> {
        let operand = self.emit_expr(&addr_of.operand, &None);

        let ptr = match operand.kind {
            InternalValueKind::LValue(ptr) => ptr,
            InternalValueKind::RValue(val) => {
                let alloca = self.llvm_builder.build_alloca(val.get_type(), "addr.cast").unwrap();
                self.llvm_builder.build_store(alloca, val).unwrap();
                alloca
            }
            _ => unreachable!("cannot take the address of a function or undefined value"),
        };

        InternalValue::new(
            CIRType::Pointer(Box::new(operand.ty)),
            InternalValueKind::RValue(ptr.as_basic_value_enum()),
        )
    }

    pub(crate) fn emit_decay_array_to_pointer(&self, array_lvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        let array_ptr = match array_lvalue.kind {
            InternalValueKind::LValue(ptr) => ptr,
            InternalValueKind::RValue(val) => {
                let alloca = self.llvm_builder.build_alloca(val.get_type(), "array.decay").unwrap();

                self.llvm_builder.build_store(alloca, val).unwrap();

                alloca
            }
            _ => unreachable!("Cannot decay a non-LValue/RValue array"),
        };

        let element_type = array_lvalue.ty.as_array().unwrap().element_type;

        let zero = self.llvm_ctx.i32_type().const_int(0, false);
        let first_element = unsafe {
            self.llvm_builder
                .build_in_bounds_gep(
                    self.emit_type(array_lvalue.ty.clone()).into_array_type(),
                    array_ptr,
                    &[zero, zero],
                    "array_decay",
                )
                .unwrap()
        };

        let cir_type = CIRType::Pointer(element_type);

        InternalValue::new(cir_type, InternalValueKind::RValue(first_element.as_basic_value_enum()))
    }

    pub(crate) fn emit_lvalue_address(&mut self, expr: &CIRExpr) -> InternalValue<'ll> {
        match &expr.kind {
            CIRExprKind::Deref(deref_expr) => {
                let lvalue = self.emit_expr(&deref_expr.operand, &None);
                let rvalue = self.load_rvalue(lvalue.clone());
                let ptr = rvalue.as_basic_value().into_pointer_value();
                let inner_ty = rvalue.ty.pointer_inner().unwrap();
                InternalValue::new(inner_ty.clone(), InternalValueKind::LValue(ptr))
            }
            CIRExprKind::FieldAccess(field_access) => match &field_access.kind {
                CIRFieldAccessKind::Struct { field_type, index } => {
                    let lvalue = self.emit_lvalue_address(&field_access.operand);

                    let struct_ptr_value = lvalue.as_basic_value().into_pointer_value();
                    let struct_type = lvalue.ty.clone();
                    let layout = self.tctx.layout_of(&struct_type);

                    let llvm_struct_type = self.emit_type(struct_type).into_struct_type();

                    let index = layout.lookup_field_index(*index).unwrap();

                    let field_ptr = self
                        .llvm_builder
                        .build_struct_gep(llvm_struct_type, struct_ptr_value, index, "field_ptr")
                        .unwrap();

                    InternalValue::new(field_type.clone(), InternalValueKind::LValue(field_ptr))
                }
                CIRFieldAccessKind::Union { field_type } => {
                    let mut value = self.emit_lvalue_address(&field_access.operand);
                    value.ty = field_type.clone();
                    value
                }
            },

            _ => self.emit_expr(expr, &None),
        }
    }

    fn emit_deref(&mut self, deref: &CIRDerefExpr, mode: DerefMode) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&deref.operand, &None);
        let rvalue = self.load_rvalue(lvalue.clone());
        let ptr = rvalue.as_basic_value().into_pointer_value();

        match mode {
            DerefMode::Load => {
                let inner_type = rvalue.ty.pointer_inner().unwrap();

                let llvm_type: BasicTypeEnum<'ll> = self.emit_type(inner_type.clone()).try_into().unwrap();
                let loaded_value = self.llvm_builder.build_load(llvm_type, ptr, "deref").unwrap();
                InternalValue::new(inner_type.clone(), InternalValueKind::RValue(loaded_value.into()))
            }
            DerefMode::Store => self.emit_lvalue_address(&CIRExpr {
                kind: CIRExprKind::Deref(deref.clone()),
                ty: deref.operand.ty.pointer_inner().cloned().unwrap(),
                loc: deref.operand.loc,
            }),
        }
    }

    fn emit_infix_expr(&mut self, infix_expr: &CIRInfixExpr, loc: Loc) -> InternalValue<'ll> {
        match infix_expr.op {
            InfixOperator::And => {
                return self.emit_short_circuit_and(&infix_expr.lhs, &infix_expr.rhs);
            }
            InfixOperator::Or => {
                return self.emit_short_circuit_or(&infix_expr.lhs, &infix_expr.rhs);
            }
            _ => {
                let lhs_lvalue = self.emit_expr(&infix_expr.lhs, &None);
                let rhs_lvalue = self.emit_expr(&infix_expr.rhs, &None);

                let mut lhs_rvalue = self.load_rvalue(lhs_lvalue.clone());
                let mut rhs_rvalue = self.load_rvalue(rhs_lvalue.clone());

                if lhs_rvalue.ty.is_integer() && rhs_rvalue.ty.is_integer() {
                    (lhs_rvalue, rhs_rvalue) = self.widen_int_pair(lhs_rvalue, rhs_rvalue);
                }

                let get_signed = || rhs_rvalue.ty.as_plain().unwrap().is_signed();

                match infix_expr.op {
                    InfixOperator::And | InfixOperator::Or => unreachable!(),

                    InfixOperator::Add => self.emit_add(lhs_rvalue, rhs_rvalue, loc),
                    InfixOperator::Sub => self.emit_sub(lhs_rvalue, rhs_rvalue, loc),
                    InfixOperator::Mul => self.emit_mul(lhs_rvalue, rhs_rvalue, loc),
                    InfixOperator::Div => self.emit_div(lhs_rvalue, rhs_rvalue),
                    InfixOperator::Rem => self.emit_rem(lhs_rvalue, rhs_rvalue),
                    InfixOperator::LessThan => {
                        if get_signed() {
                            self.emit_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SLT, FloatPredicate::OLT)
                        } else {
                            self.emit_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::ULT, FloatPredicate::OLT)
                        }
                    }
                    InfixOperator::LessEqual => {
                        if get_signed() {
                            self.emit_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SLE, FloatPredicate::OLE)
                        } else {
                            self.emit_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::ULE, FloatPredicate::OLE)
                        }
                    }
                    InfixOperator::GreaterThan => {
                        if get_signed() {
                            self.emit_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SGT, FloatPredicate::OGT)
                        } else {
                            self.emit_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::UGT, FloatPredicate::OGT)
                        }
                    }
                    InfixOperator::GreaterEqual => {
                        if get_signed() {
                            self.emit_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SGE, FloatPredicate::OGE)
                        } else {
                            self.emit_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::UGE, FloatPredicate::OGE)
                        }
                    }
                    InfixOperator::Equal => self.emit_cmp_eq(lhs_rvalue, rhs_rvalue),
                    InfixOperator::NotEqual => self.emit_cmp_neq(lhs_rvalue, rhs_rvalue),
                    InfixOperator::BitwiseAnd => self.emit_bitwise_and(lhs_rvalue, rhs_rvalue),
                    InfixOperator::BitwiseOr => self.emit_bitwise_or(lhs_rvalue, rhs_rvalue),
                    InfixOperator::BitwiseXor => self.emit_xor(lhs_rvalue, rhs_rvalue),
                    InfixOperator::BitwiseAndNot => self.emit_bitwise_and_not(lhs_rvalue, rhs_rvalue),
                    InfixOperator::ShiftLeft => self.emit_shift_left(lhs_rvalue, rhs_rvalue),
                    InfixOperator::ShiftRight => self.emit_shift_right(lhs_rvalue, rhs_rvalue),
                    InfixOperator::NullCoalesce => self.emit_null_coalesce_operator(lhs_rvalue, rhs_rvalue),
                }
            }
        }
    }

    fn emit_null_coalesce_operator(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                self.emit_null_coalescing_pointers(lhs, rhs, lhs_rvalue.ty.clone())
            }
            _ => unreachable!(),
        }
    }

    fn emit_null_coalescing_pointers(
        &self,
        lhs: PointerValue<'ll>,
        rhs: PointerValue<'ll>,
        ty: CIRType,
    ) -> InternalValue<'ll> {
        // cond: lhs == null
        let is_null = self.llvm_builder.build_is_null(lhs, "lhs_is_null").unwrap();

        let selected = self
            .llvm_builder
            .build_select(is_null, rhs, lhs, "null_coalesce")
            .unwrap()
            .into_pointer_value();

        InternalValue::new(
            CIRType::Pointer(Box::new(ty)),
            InternalValueKind::RValue(selected.into()),
        )
    }

    fn emit_short_circuit_and(&mut self, lhs_expr: &CIRExpr, rhs_expr: &CIRExpr) -> InternalValue<'ll> {
        let cur_fn = self.cur_func.unwrap();

        let cont_block = self.llvm_ctx.append_basic_block(cur_fn, "and_cont");
        let rhs_block = self.llvm_ctx.append_basic_block(cur_fn, "and_rhs");
        let false_block = self.llvm_ctx.append_basic_block(cur_fn, "and_false");

        let result_alloca = self
            .llvm_builder
            .build_alloca(self.llvm_ctx.bool_type(), "and_result_storage")
            .unwrap();

        let lhs_lvalue = self.emit_expr(lhs_expr, &None);
        let lhs_rvalue = self.load_rvalue(lhs_lvalue);
        let lhs_bool = self.int_value_as_bool_i1(lhs_rvalue.as_basic_value().into_int_value());

        if let Some(cur_block) = &self.block_reg.cur_block {
            if cur_block.get_terminator().is_none() {
                self.block_reg.cur_block = None;
                self.llvm_builder
                    .build_conditional_branch(lhs_bool, rhs_block, false_block)
                    .unwrap();
            }
        }

        self.emit_block(false_block);
        let false_value = self.llvm_ctx.bool_type().const_int(0, false);
        self.llvm_builder.build_store(result_alloca, false_value).unwrap();
        if let Some(cur_block) = &self.block_reg.cur_block {
            if cur_block.get_terminator().is_none() {
                self.block_reg.cur_block = None;
                self.llvm_builder.build_unconditional_branch(cont_block).unwrap();
            }
        }

        self.emit_block(rhs_block);
        let rhs_lvalue = self.emit_expr(rhs_expr, &None);
        let rhs_rvalue = self.load_rvalue(rhs_lvalue);
        let rhs_bool = self.int_value_as_bool_i1(rhs_rvalue.as_basic_value().into_int_value());

        self.llvm_builder.build_store(result_alloca, rhs_bool).unwrap();

        if let Some(cur_block) = &self.block_reg.cur_block {
            if cur_block.get_terminator().is_none() {
                self.block_reg.cur_block = None;
                self.llvm_builder.build_unconditional_branch(cont_block).unwrap();
            }
        }

        self.emit_block(cont_block);
        let result = self
            .llvm_builder
            .build_load(self.llvm_ctx.bool_type(), result_alloca, "and_result")
            .unwrap();

        InternalValue::new(
            CIRType::Plain(PlainType::Bool),
            InternalValueKind::RValue(result.into()),
        )
    }

    fn emit_short_circuit_or(&mut self, lhs_expr: &CIRExpr, rhs_expr: &CIRExpr) -> InternalValue<'ll> {
        let cur_fn = self.cur_func.unwrap();

        let cont_block = self.llvm_ctx.append_basic_block(cur_fn, "or_cont");
        let rhs_block = self.llvm_ctx.append_basic_block(cur_fn, "or_rhs");
        let true_block = self.llvm_ctx.append_basic_block(cur_fn, "or_true");

        let result_alloca = self
            .llvm_builder
            .build_alloca(self.llvm_ctx.bool_type(), "or_result_storage")
            .unwrap();

        let lhs_lvalue = self.emit_expr(lhs_expr, &None);
        let lhs_rvalue = self.load_rvalue(lhs_lvalue);
        let lhs_bool = self.int_value_as_bool_i1(lhs_rvalue.as_basic_value().into_int_value());

        if let Some(cur_block) = &self.block_reg.cur_block {
            if cur_block.get_terminator().is_none() {
                self.block_reg.cur_block = None;
                self.llvm_builder
                    .build_conditional_branch(lhs_bool, true_block, rhs_block)
                    .unwrap();
            }
        }

        self.emit_block(true_block);
        let true_value = self.llvm_ctx.bool_type().const_int(1, false);
        self.llvm_builder.build_store(result_alloca, true_value).unwrap();
        if let Some(cur_block) = &self.block_reg.cur_block {
            if cur_block.get_terminator().is_none() {
                self.block_reg.cur_block = None;
                self.llvm_builder.build_unconditional_branch(cont_block).unwrap();
            }
        }

        self.emit_block(rhs_block);
        let rhs_lvalue = self.emit_expr(rhs_expr, &None);
        let rhs_rvalue = self.load_rvalue(rhs_lvalue);
        let rhs_bool = self.int_value_as_bool_i1(rhs_rvalue.as_basic_value().into_int_value());

        self.llvm_builder.build_store(result_alloca, rhs_bool).unwrap();

        if let Some(cur_block) = &self.block_reg.cur_block {
            if cur_block.get_terminator().is_none() {
                self.block_reg.cur_block = None;
                self.llvm_builder.build_unconditional_branch(cont_block).unwrap();
            }
        }

        self.emit_block(cont_block);
        let result = self
            .llvm_builder
            .build_load(self.llvm_ctx.bool_type(), result_alloca, "or_result")
            .unwrap();

        InternalValue::new(
            CIRType::Plain(PlainType::Bool),
            InternalValueKind::RValue(result.into()),
        )
    }

    fn emit_xor(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvm_builder.build_xor(lhs, rhs, "xor").unwrap();

                InternalValue::new(
                    CIRType::Plain(PlainType::Bool),
                    InternalValueKind::RValue(and_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn emit_bitwise_and(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvm_builder.build_and(lhs, rhs, "xor").unwrap();

                InternalValue::new(
                    CIRType::Plain(PlainType::Bool),
                    InternalValueKind::RValue(and_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn emit_bitwise_or(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvm_builder.build_or(lhs, rhs, "or").unwrap();
                InternalValue::new(
                    CIRType::Plain(PlainType::Bool),
                    InternalValueKind::RValue(and_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn emit_bitwise_and_not(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                // ~rhs = rhs XOR all ones
                let all_ones = rhs.get_type().const_all_ones();
                let not_rhs = self.llvm_builder.build_xor(rhs, all_ones, "not_rhs").unwrap();

                // lhs AND (~rhs)
                let and_not_value = self.llvm_builder.build_and(lhs, not_rhs, "and_not").unwrap();

                InternalValue::new(
                    // REVIEW: this does not look good here
                    CIRType::Plain(PlainType::Int32), // result is integer, not bool
                    InternalValueKind::RValue(and_not_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn emit_shift_left(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let shift_value = self.llvm_builder.build_left_shift(lhs, rhs, "lshift").unwrap();

                InternalValue::new(
                    CIRType::Plain(PlainType::Bool),
                    InternalValueKind::RValue(shift_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn emit_shift_right(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let signed = rhs_rvalue.ty.as_plain().unwrap().is_signed();

                let shift_value = self.llvm_builder.build_right_shift(lhs, rhs, signed, "rshift").unwrap();

                InternalValue::new(
                    CIRType::Plain(PlainType::Bool),
                    InternalValueKind::RValue(shift_value.into()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn emit_checked_int_op(
        &mut self,
        op: &str,
        lhs: IntValue<'ll>,
        rhs: IntValue<'ll>,
        is_signed: bool,
        result_type: CIRType,
        loc: Loc,
    ) -> InternalValue<'ll> {
        let int_type = lhs.get_type();
        let width = int_type.get_bit_width();
        let prefix = if is_signed { "s" } else { "u" };
        let intrinsic_name = format!("llvm.{}{}.with.overflow.i{}", prefix, op, width);
        let struct_type = self
            .llvm_ctx
            .struct_type(&[int_type.into(), self.llvm_ctx.bool_type().into()], false);

        let fn_type = struct_type.fn_type(&[int_type.into(), int_type.into()], false);

        let intrinsic_fn = {
            let module = self.llvm_module.borrow();
            match module.get_function(&intrinsic_name) {
                Some(f) => f,
                None => module.add_function(&intrinsic_name, fn_type, None),
            }
        };

        let call_site = self
            .llvm_builder
            .build_call(intrinsic_fn, &[lhs.into(), rhs.into()], "overflow_result")
            .unwrap();

        let result_struct = call_site.try_as_basic_value().basic().unwrap().into_struct_value();

        let math_result = self
            .llvm_builder
            .build_extract_value(result_struct, 0, "result")
            .unwrap()
            .into_int_value();

        let overflow_flag = self
            .llvm_builder
            .build_extract_value(result_struct, 1, "overflow_flag")
            .unwrap()
            .into_int_value();

        let cur_fn = self.cur_func.unwrap();
        let panic_block = self.llvm_ctx.append_basic_block(cur_fn, "panic");
        let cont_block = self.llvm_ctx.append_basic_block(cur_fn, "cont");

        self.llvm_builder
            .build_conditional_branch(overflow_flag, panic_block, cont_block)
            .unwrap();

        self.llvm_builder.position_at_end(panic_block);

        let msg_expr = cir_string_literal(&format!("integer overflow in instruction '{}'", op), loc);

        self.emit_intrinsic_panic(&[msg_expr], loc);

        self.emit_block(cont_block);

        InternalValue::new(
            result_type,
            InternalValueKind::RValue(math_result.as_basic_value_enum()),
        )
    }

    fn emit_add(
        &mut self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
        loc: Loc,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                if self.profile == CompilerOption_Profile::Debug {
                    let is_signed = lhs_rvalue.ty.is_signed_integer();

                    self.emit_checked_int_op("add", lhs, rhs, is_signed, lhs_rvalue.ty.clone(), loc)
                } else {
                    let basic_value =
                        BasicValueEnum::IntValue(self.llvm_builder.build_int_add(lhs, rhs, "add").unwrap());

                    InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
                }
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvm_builder.build_float_add(lhs, rhs, "add").unwrap());

                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::PointerValue(ptr), BasicValueEnum::IntValue(index)) => {
                self.emit_pointer_add(ptr, index, lhs_rvalue.ty.clone())
            }
            (BasicValueEnum::IntValue(index), BasicValueEnum::PointerValue(ptr)) => {
                self.emit_pointer_add(ptr, index, rhs_rvalue.ty.clone())
            }
            _ => unreachable!(),
        }
    }

    fn emit_sub(
        &mut self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
        loc: Loc,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                if self.profile == CompilerOption_Profile::Debug {
                    let is_signed = lhs_rvalue.ty.is_signed_integer();

                    self.emit_checked_int_op("sub", lhs, rhs, is_signed, lhs_rvalue.ty.clone(), loc)
                } else {
                    let basic_value =
                        BasicValueEnum::IntValue(self.llvm_builder.build_int_sub(lhs, rhs, "sub").unwrap());

                    InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
                }
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvm_builder.build_float_sub(lhs, rhs, "sub").unwrap());

                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::PointerValue(ptr), BasicValueEnum::IntValue(index)) => {
                self.emit_pointer_sub(ptr, index, lhs_rvalue.ty.clone())
            }
            (BasicValueEnum::PointerValue(lhs_ptr), BasicValueEnum::PointerValue(rhs_ptr)) => {
                let cir_pointee_type = lhs_rvalue.ty.pointer_inner().unwrap().clone();

                let pointee_type: BasicTypeEnum<'ll> = if cir_pointee_type.is_void() {
                    self.llvm_ctx.i8_type().into()
                } else {
                    self.emit_type(cir_pointee_type).try_into().unwrap()
                };

                self.emit_pointer_diff(pointee_type, lhs_ptr, rhs_ptr)
            }
            _ => unreachable!(),
        }
    }

    fn emit_pointer_add(
        &self,
        ptr: PointerValue<'ll>,
        index: IntValue<'ll>,
        result_type: CIRType,
    ) -> InternalValue<'ll> {
        let cir_pointee_type = result_type.pointer_inner().unwrap().clone();

        let pointee_type: BasicTypeEnum<'ll> = if cir_pointee_type.is_void() {
            self.llvm_ctx.i8_type().into()
        } else {
            self.emit_type(cir_pointee_type).try_into().unwrap()
        };

        let i64_type = self.llvm_ctx.i64_type();
        let gep_index = if index.get_type() == i64_type {
            index
        } else {
            self.llvm_builder.build_int_cast(index, i64_type, "index.cast").unwrap()
        };

        // Create GEP instruction
        // LLVM automatically multiplies by sizeof(pointee)
        let gep_ptr = unsafe {
            self.llvm_builder
                .build_gep(pointee_type, ptr, &[gep_index], "ptr.add")
                .unwrap()
        };

        let basic_value = BasicValueEnum::PointerValue(gep_ptr);
        InternalValue::new(result_type, InternalValueKind::RValue(basic_value))
    }

    fn emit_pointer_sub(
        &self,
        ptr: PointerValue<'ll>,
        index: IntValue<'ll>,
        result_type: CIRType,
    ) -> InternalValue<'ll> {
        let cir_pointee_type = result_type.pointer_inner().unwrap().clone();

        let pointee_type: BasicTypeEnum<'ll> = if cir_pointee_type.is_void() {
            self.llvm_ctx.i8_type().into()
        } else {
            self.emit_type(cir_pointee_type).try_into().unwrap()
        };

        // negate the index for subtraction
        let i64_type = self.llvm_ctx.i64_type();
        let index_i64 = if index.get_type() == i64_type {
            index
        } else {
            self.llvm_builder.build_int_cast(index, i64_type, "i.cast").unwrap()
        };

        let neg_index = self.llvm_builder.build_int_neg(index_i64, "i.neg").unwrap();

        // Create GEP with negative index
        let gep_ptr = unsafe {
            self.llvm_builder
                .build_gep(pointee_type, ptr, &[neg_index], "ptr.sub")
                .unwrap()
        };

        let basic_value = BasicValueEnum::PointerValue(gep_ptr);
        InternalValue::new(result_type, InternalValueKind::RValue(basic_value))
    }

    fn emit_pointer_diff(
        &self,
        pointee_type: BasicTypeEnum<'ll>,
        lhs_ptr: PointerValue<'ll>,
        rhs_ptr: PointerValue<'ll>,
    ) -> InternalValue<'ll> {
        let diff_int_value = self
            .llvm_builder
            .build_ptr_diff(pointee_type, lhs_ptr, rhs_ptr, "ptr.diff")
            .unwrap();

        let result_type = CIRType::Plain(PlainType::ISize);
        let llvm_result_type: BasicTypeEnum<'ll> = self.emit_type(result_type.clone()).try_into().unwrap();
        let diff_casted = self
            .llvm_builder
            .build_int_cast(diff_int_value, llvm_result_type.into_int_type(), "ptr.diff.cast")
            .unwrap();

        InternalValue::new(result_type, InternalValueKind::RValue(diff_casted.into()))
    }

    fn emit_mul(
        &mut self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
        loc: Loc,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                if self.profile == CompilerOption_Profile::Debug {
                    let is_signed = lhs_rvalue.ty.is_signed_integer();

                    self.emit_checked_int_op("mul", lhs, rhs, is_signed, lhs_rvalue.ty.clone(), loc)
                } else {
                    let basic_value =
                        BasicValueEnum::IntValue(self.llvm_builder.build_int_mul(lhs, rhs, "mul").unwrap());

                    InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
                }
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvm_builder.build_float_mul(lhs, rhs, "mul").unwrap());

                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn emit_div(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let is_signed = lhs_rvalue.ty.is_signed_integer();

                let basic_value = {
                    if is_signed {
                        BasicValueEnum::IntValue(self.llvm_builder.build_int_signed_div(lhs, rhs, "div").unwrap())
                    } else {
                        BasicValueEnum::IntValue(self.llvm_builder.build_int_unsigned_div(lhs, rhs, "div").unwrap())
                    }
                };

                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvm_builder.build_float_div(lhs, rhs, "div").unwrap());

                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn emit_rem(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let is_signed = rhs_rvalue.ty.is_signed_integer();

                let basic_value = {
                    if is_signed {
                        BasicValueEnum::IntValue(self.llvm_builder.build_int_signed_rem(lhs, rhs, "rem").unwrap())
                    } else {
                        BasicValueEnum::IntValue(self.llvm_builder.build_int_unsigned_rem(lhs, rhs, "rem").unwrap())
                    }
                };

                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvm_builder.build_float_rem(lhs, rhs, "rem").unwrap());

                InternalValue::new(lhs_rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn emit_cmp(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
        int_pred: IntPredicate,
        float_pred: FloatPredicate,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(mut lhs), BasicValueEnum::IntValue(mut rhs)) => {
                if lhs_rvalue.ty.is_bool() || rhs_rvalue.ty.is_bool() {
                    lhs = self.int_value_as_bool_i1(lhs);
                    rhs = self.int_value_as_bool_i1(rhs);
                }

                let cmp = self.llvm_builder.build_int_compare(int_pred, lhs, rhs, "cmp").unwrap();

                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvm_builder
                    .build_float_compare(float_pred, lhs, rhs, "cmp")
                    .unwrap();

                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            _ => unreachable!(),
        }
    }

    fn emit_cmp_eq_const_strings(
        &self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        let strcmp_result = self.intrinsic_strcmp(
            lhs_rvalue.as_basic_value().into_pointer_value(),
            rhs_rvalue.as_basic_value().into_pointer_value(),
        );

        let zero = strcmp_result.get_type().const_zero();

        let cmp = self
            .llvm_builder
            .build_int_compare(IntPredicate::EQ, strcmp_result, zero, "streq")
            .unwrap();

        InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
    }

    pub(crate) fn emit_cmp_eq(
        &mut self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(mut lhs), BasicValueEnum::IntValue(mut rhs)) => {
                if lhs_rvalue.ty.is_bool() || rhs_rvalue.ty.is_bool() {
                    lhs = self.int_value_as_bool_i1(lhs);
                    rhs = self.int_value_as_bool_i1(rhs);
                }

                let cmp = self
                    .llvm_builder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
                    .unwrap();

                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvm_builder
                    .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "eq")
                    .unwrap();

                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                // streq
                if let (Some(lhs_ptr_inner), Some(rhs_ptr_inner)) =
                    (lhs_rvalue.ty.pointer_inner(), rhs_rvalue.ty.pointer_inner())
                {
                    if lhs_ptr_inner.is_char() && rhs_ptr_inner.is_char() {
                        return self.emit_cmp_eq_const_strings(lhs_rvalue, rhs_rvalue);
                    }
                }

                let cmp = self
                    .llvm_builder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
                    .unwrap();

                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            _ => {
                if lhs_rvalue.ty.is_enum() && rhs_rvalue.ty.is_enum() {
                    let enum_type = lhs_rvalue.ty.as_enum(&self.tctx).unwrap();
                    let tag_type = enum_type.tag_type_or_infer_or_default();

                    return self.emit_compare_enum_variants(lhs_rvalue.clone(), rhs_rvalue.clone(), &tag_type, true);
                }

                unreachable!()
            }
        }
    }

    pub(crate) fn emit_cmp_neq(
        &mut self,
        lhs_rvalue: InternalValue<'ll>,
        rhs_rvalue: InternalValue<'ll>,
    ) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let cmp = self
                    .llvm_builder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "neq")
                    .unwrap();
                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvm_builder
                    .build_float_compare(FloatPredicate::ONE, lhs, rhs, "neq")
                    .unwrap();
                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                let cmp = self
                    .llvm_builder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "neq")
                    .unwrap();
                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            _ => {
                if lhs_rvalue.ty.is_enum() && rhs_rvalue.ty.is_enum() {
                    let enum_type = lhs_rvalue.ty.as_enum(&self.tctx).unwrap();
                    let tag_type = enum_type.tag_type_or_infer_or_default();

                    return self.emit_compare_enum_variants(lhs_rvalue.clone(), rhs_rvalue.clone(), &tag_type, false);
                }

                unreachable!()
            }
        }
    }

    fn emit_prefix_expr(&mut self, prefix_expr: &CIRPrefixExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&prefix_expr.operand, &None);
        let rvalue = self.load_rvalue(lvalue);

        match prefix_expr.op {
            PrefixOperator::Bang => self.emit_logical_not(rvalue),
            PrefixOperator::Minus => self.emit_negate(rvalue),
            PrefixOperator::BitwiseNot => self.emit_bitwise_not(rvalue),
        }
    }

    fn emit_bitwise_not(&self, rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match rvalue.as_basic_value() {
            BasicValueEnum::IntValue(int_value) => {
                let basic_value = BasicValueEnum::IntValue(self.llvm_builder.build_not(int_value, "neg").unwrap());
                InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn emit_negate(&self, rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match rvalue.as_basic_value() {
            BasicValueEnum::IntValue(int_value) => {
                let basic_value = BasicValueEnum::IntValue(self.llvm_builder.build_int_neg(int_value, "neg").unwrap());

                InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            BasicValueEnum::FloatValue(float_value) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvm_builder.build_float_neg(float_value, "neg").unwrap());

                InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn emit_logical_not(&self, rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match rvalue.as_basic_value() {
            BasicValueEnum::IntValue(mut int_value) => {
                int_value = self.int_value_as_bool_i1(int_value);

                let basic_value = BasicValueEnum::IntValue(self.llvm_builder.build_not(int_value, "neg").unwrap());

                InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn emit_field_access(&mut self, field_access: &CIRFieldAccessExpr) -> InternalValue<'ll> {
        match &field_access.kind {
            CIRFieldAccessKind::Struct { .. } => self.emit_struct_field_access(field_access),
            CIRFieldAccessKind::Union { .. } => self.emit_union_field_access(field_access),
        }
    }

    fn emit_union_field_access(&mut self, field_access: &CIRFieldAccessExpr) -> InternalValue<'ll> {
        let operand = self.emit_lvalue_address(&field_access.operand);

        let union_type: BasicTypeEnum<'ll> = self.emit_type(field_access.operand.ty.clone()).try_into().unwrap();

        let union_ptr = match operand.kind {
            InternalValueKind::LValue(ptr) => ptr,
            InternalValueKind::RValue(basic_val) => {
                let alloca = self.llvm_builder.build_alloca(union_type, "union.temp").unwrap();
                self.llvm_builder.build_store(alloca, basic_val).unwrap();
                alloca
            }
            _ => unreachable!(),
        };

        // union field access just narrows the type
        let field_type = match &field_access.kind {
            CIRFieldAccessKind::Union { field_type } => field_type.clone(),
            _ => unreachable!(),
        };

        InternalValue::new(field_type, InternalValueKind::LValue(union_ptr))
    }

    fn emit_struct_field_access(&mut self, field_access: &CIRFieldAccessExpr) -> InternalValue<'ll> {
        let (field_type, field_index) = match &field_access.kind {
            CIRFieldAccessKind::Struct { field_type, index } => (field_type.clone(), *index),
            _ => unreachable!(),
        };

        let operand = self.emit_lvalue_address(&field_access.operand);

        // determine concrete struct type of operand
        let ty = {
            if let Some(inner) = field_access.operand.ty.pointer_inner() {
                inner.clone()
            } else {
                field_access.operand.ty.clone()
            }
        };

        let type_id = ty.as_type_id().unwrap();
        let layout = self.tctx.layout_of(&ty);

        let llvm_field_index = layout
            .lookup_field_index(field_index)
            .expect("layout must contain field");

        let llvm_struct_type = self.emit_struct_type(type_id);

        match operand.kind {
            InternalValueKind::LValue(ptr_value) => {
                let field_ptr = self
                    .llvm_builder
                    .build_struct_gep(llvm_struct_type, ptr_value, llvm_field_index, "field_gep")
                    .unwrap();

                InternalValue::new(field_type, InternalValueKind::LValue(field_ptr))
            }
            InternalValueKind::RValue(struct_val) => {
                if struct_val.is_int_value() || struct_val.is_float_value() {
                    let alloca = self
                        .llvm_builder
                        .build_alloca(llvm_struct_type, "struct_field.cast")
                        .unwrap();

                    self.llvm_builder.build_store(alloca, struct_val).unwrap();

                    let field_ptr = self
                        .llvm_builder
                        .build_struct_gep(llvm_struct_type, alloca, llvm_field_index, "field_gep")
                        .unwrap();

                    InternalValue::new(field_type, InternalValueKind::LValue(field_ptr))
                } else {
                    let struct_value = struct_val.into_struct_value();

                    let field_value = self
                        .llvm_builder
                        .build_extract_value(struct_value, llvm_field_index, "field_extract")
                        .unwrap();

                    InternalValue::new(field_type, InternalValueKind::RValue(field_value))
                }
            }
            _ => unreachable!(),
        }
    }

    fn emit_tuple_access(&mut self, tuple_access: &CIRTupleAccessExpr) -> InternalValue<'ll> {
        let operand_value = self.emit_expr(&tuple_access.operand, &None);

        let ty = &operand_value.ty;
        let type_id = ty.as_type_id().unwrap();

        let cir_struct_type = ty.as_struct(&self.tctx).unwrap();
        let layout = self.tctx.layout_of(ty);

        let field_index = layout.lookup_field_index(tuple_access.index).unwrap();

        let cir_field_type = &cir_struct_type.fields[tuple_access.index];

        let struct_type = self.emit_struct_type(type_id);

        match operand_value.kind {
            InternalValueKind::LValue(addr) => {
                let field_addr = self
                    .llvm_builder
                    .build_struct_gep(struct_type, addr, field_index, "tuple_gep")
                    .unwrap();

                InternalValue::new(cir_field_type.clone(), InternalValueKind::LValue(field_addr))
            }
            InternalValueKind::RValue(val) => {
                let struct_value = val.into_struct_value();

                let field_val = self
                    .llvm_builder
                    .build_extract_value(struct_value, field_index, "tuple_extract")
                    .unwrap();

                InternalValue::new(cir_field_type.clone(), InternalValueKind::RValue(field_val))
            }
            _ => unreachable!(),
        }
    }

    fn emit_tuple(&mut self, tuple: &CIRTupleExpr) -> InternalValue<'ll> {
        let struct_value = self
            .emit_struct_init(&CIRStructInitExpr {
                ty: tuple.ty.clone(),
                fields: tuple.elements.clone(),
            })
            .as_basic_value()
            .into_struct_value();

        InternalValue::new(tuple.ty.clone(), InternalValueKind::RValue(struct_value.into()))
    }

    fn emit_repr_c_enum_init(
        &mut self,
        enum_init_expr: &CIREnumInitExpr,
        enum_type: &CIREnumType,
    ) -> InternalValue<'ll> {
        let ty = enum_init_expr.ty.clone();

        let cir_tag_type = enum_type.tag_type_or_infer_or_default();
        let tag_type = self.emit_type(*cir_tag_type.clone()).into_int_type();
        let tag_value = tag_type.const_int(enum_init_expr.tag.try_into().unwrap(), cir_tag_type.is_signed_integer());

        InternalValue::new(ty, InternalValueKind::RValue(tag_value.as_basic_value_enum()))
    }

    fn emit_enum_init(&mut self, enum_init_expr: &CIREnumInitExpr) -> InternalValue<'ll> {
        let ty = &enum_init_expr.ty;
        let type_id = enum_init_expr.ty.as_type_id().unwrap();
        let enum_type = enum_init_expr.ty.as_enum(&self.tctx).unwrap();

        if enum_type.is_scalar_optimizable() {
            return self.emit_repr_c_enum_init(enum_init_expr, &enum_type);
        }

        let llvm_enum_type = self.emit_enum_type(type_id).into_struct_type();
        let (buffer_type, _) = self.emit_enum_buffer_payload_type(&enum_type);

        let cir_tag_type = enum_type.tag_type_or_infer_or_default();
        let tag_type = self.emit_type(*cir_tag_type.clone()).into_int_type();
        let tag_value = tag_type.const_int(enum_init_expr.tag as u64, false);

        let is_global = self.llvm_builder.get_insert_block().is_none();

        if !is_global {
            let value =
                self.emit_enum_init_with_alloca(enum_init_expr, &enum_type, llvm_enum_type, buffer_type, tag_value);

            InternalValue::new(ty.clone(), InternalValueKind::RValue(value))
        } else {
            panic!("if global var includes enum init in it's initializer expression, it must be initialized lazily");
        }
    }

    fn emit_enum_init_with_alloca(
        &mut self,
        enum_init_expr: &CIREnumInitExpr,
        enum_type: &CIREnumType,
        llvm_enum_type: StructType<'ll>,
        buffer_type: ArrayType<'ll>,
        tag_value: IntValue<'ll>,
    ) -> BasicValueEnum<'ll> {
        let enum_alloca = self.llvm_builder.build_alloca(llvm_enum_type, "enum.alloca").unwrap();

        let tag_ptr = self
            .llvm_builder
            .build_struct_gep(llvm_enum_type, enum_alloca, 0, "enum.tag.ptr")
            .unwrap();

        self.llvm_builder.build_store(tag_ptr, tag_value).unwrap();

        match &enum_init_expr.variant {
            CIREnumInitVariant::Unit => {
                let zero_payload = buffer_type.const_zero();

                let payload_ptr = self
                    .llvm_builder
                    .build_struct_gep(llvm_enum_type, enum_alloca, 1, "enum.payload.ptr")
                    .unwrap();

                self.llvm_builder.build_store(payload_ptr, zero_payload).unwrap();
            }
            CIREnumInitVariant::Valued(expr) => {
                let lvalue = self.emit_expr(expr, &None);
                let rvalue = self.load_rvalue(lvalue);

                let payload_ptr = self
                    .llvm_builder
                    .build_struct_gep(llvm_enum_type, enum_alloca, 1, "enum.payload.ptr")
                    .unwrap();

                self.llvm_builder
                    .build_store(payload_ptr, rvalue.as_basic_value())
                    .unwrap();
            }
            CIREnumInitVariant::Payload(field_exprs) => {
                // IMPORTANT: Zero out the entire payload buffer first
                // to ensure padded slots value is consistent.
                // If we don't do this, it may cause UB when
                // comparing two equal enums.
                let zero_payload = buffer_type.const_zero();

                let payload_ptr = self
                    .llvm_builder
                    .build_struct_gep(llvm_enum_type, enum_alloca, 1, "enum.payload.ptr")
                    .unwrap();
                self.llvm_builder.build_store(payload_ptr, zero_payload).unwrap();

                let field_types: Vec<BasicTypeEnum<'ll>> = field_exprs
                    .iter()
                    .map(|fld| self.emit_type(fld.ty.clone()).try_into().unwrap())
                    .collect();

                let payload_struct_type = self.llvm_ctx.struct_type(&field_types, false);

                for (i, field_expr) in field_exprs.iter().enumerate() {
                    let lvalue = self.emit_expr(&field_expr, &None);
                    let mut rvalue = self.load_rvalue(lvalue);

                    let variant_enum_type = match enum_type.lookup_variant(&enum_init_expr.ident).unwrap() {
                        CIREnumVariant::Payload(_, struct_type, _) => struct_type,
                        _ => unreachable!(),
                    };

                    let field_type = variant_enum_type.fields.get(i).unwrap();

                    if !self.llvm_builder.get_insert_block().is_none() {
                        rvalue = self.emit_implicit_cast(field_type, rvalue);
                    }

                    let field_ptr = self
                        .llvm_builder
                        .build_struct_gep(payload_struct_type, payload_ptr, i as u32, "payload.field.ptr")
                        .unwrap();

                    self.llvm_builder
                        .build_store(field_ptr, rvalue.as_basic_value())
                        .unwrap();
                }
            }
        }

        self.llvm_builder
            .build_load(llvm_enum_type, enum_alloca, "enum.load")
            .unwrap()
    }

    pub(crate) fn emit_union_init(
        &self,
        union_type: &CIRUnionType,
        ptr: PointerValue<'ll>,
        rvalue: InternalValue<'ll>,
    ) {
        let type_id = self.tctx.insert_union(union_type.clone());

        let llvm_union_type = self.emit_union_type(type_id);

        // union-to-union copy
        if rvalue.ty.is_union() {
            self.intrinsic_optimized_memcpy(ptr, rvalue.as_basic_value());
            return;
        }

        let value = rvalue.as_basic_value();
        let ptr_type = self.llvm_ctx.ptr_type(AddressSpace::default());

        if llvm_union_type.is_struct_type() {
            // get pointer to storage field (largest field)
            let union_ptr = self
                .llvm_builder
                .build_struct_gep(llvm_union_type.into_struct_type(), ptr, 0, "union.storage")
                .unwrap();

            let field_ptr = self
                .llvm_builder
                .build_bit_cast(union_ptr, ptr_type, "union.field.ptr")
                .unwrap()
                .into_pointer_value();

            self.llvm_builder.build_store(field_ptr, value).unwrap();
        } else {
            // union represented as largest field type (directly, without struct type as wrapper)
            let field_ptr = self
                .llvm_builder
                .build_bit_cast(ptr, ptr_type, "union.field.ptr")
                .unwrap()
                .into_pointer_value();

            self.llvm_builder.build_store(field_ptr, value).unwrap();
        }
    }

    #[inline]
    fn emit_union_init_value(&mut self, union_init_expr: &CIRUnionInitExpr) -> InternalValue<'ll> {
        self.emit_expr(&union_init_expr.expr, &None)
    }

    #[inline]
    pub(crate) fn extract_enum_tag(&self, struct_value: StructValue<'ll>) -> IntValue<'ll> {
        self.llvm_builder
            .build_extract_value(struct_value, 0, "extract")
            .unwrap()
            .into_int_value()
    }

    #[inline]
    pub(crate) fn extract_enum_payload(&self, struct_value: StructValue<'ll>) -> ArrayValue<'ll> {
        self.llvm_builder
            .build_extract_value(struct_value, 1, "extract")
            .unwrap()
            .into_array_value()
    }

    fn emit_compare_enum_variants(
        &mut self,
        lhs: InternalValue<'ll>,
        rhs: InternalValue<'ll>,
        tag_type: &CIRType,
        cmp_eq: bool,
    ) -> InternalValue<'ll> {
        let struct_value1 = lhs.as_basic_value().into_struct_value();
        let struct_value2 = rhs.as_basic_value().into_struct_value();

        let llvm_tag_type: BasicTypeEnum<'ll> = self.emit_type(tag_type.clone()).try_into().unwrap();

        let tag1 = self.extract_enum_tag(struct_value1);
        let tag2 = self.extract_enum_tag(struct_value2);

        let lhs_tag = InternalValue::new(tag_type.clone(), InternalValueKind::RValue(tag1.into()));
        let rhs_tag = InternalValue::new(tag_type.clone(), InternalValueKind::RValue(tag2.into()));

        let tag_result = if cmp_eq {
            self.emit_cmp_eq(lhs_tag, rhs_tag)
        } else {
            self.emit_cmp_neq(lhs_tag, rhs_tag)
        };

        let tag_result_int_value = tag_result.as_basic_value().into_int_value();

        let current_func = self.cur_func.unwrap();
        let payload_block = self.llvm_ctx.append_basic_block(current_func, "compare.enum.payload");
        let exit_block = self.llvm_ctx.append_basic_block(current_func, "compare.enum.exit");

        let (branch_true, branch_false) = if cmp_eq {
            (payload_block, exit_block)
        } else {
            (exit_block, payload_block)
        };

        let entry_block = self.block_reg.cur_block.unwrap();

        self.llvm_builder
            .build_conditional_branch(tag_result.as_basic_value().into_int_value(), branch_true, branch_false)
            .unwrap();

        self.emit_block(payload_block);

        let payload1 = self.extract_enum_payload(struct_value1);
        let payload2 = self.extract_enum_payload(struct_value2);

        let memcmp_result = self.intrinsic_array_memcmp(payload1, payload2);

        let zero_int = llvm_tag_type.const_zero().into_int_value();

        let predicate = if cmp_eq { IntPredicate::EQ } else { IntPredicate::NE };

        // comparison result
        let payload_result = self
            .llvm_builder
            .build_int_compare(predicate, memcmp_result, zero_int, "compare.payload.is_zero")
            .unwrap();

        let payload_result_bool = self.int_value_as_bool_i1(payload_result);

        self.llvm_builder.build_unconditional_branch(exit_block).unwrap();

        self.emit_block(exit_block);

        let phi = self
            .llvm_builder
            .build_phi(self.llvm_ctx.bool_type(), "compare.enum")
            .unwrap();

        phi.add_incoming(&[(&tag_result_int_value, entry_block)]);
        phi.add_incoming(&[(&payload_result_bool, payload_block)]);

        InternalValue::new(
            CIRType::Plain(PlainType::Bool),
            InternalValueKind::RValue(phi.as_basic_value()),
        )
    }

    fn emit_struct_init(&mut self, struct_init: &CIRStructInitExpr) -> InternalValue<'ll> {
        let ty = &struct_init.ty;

        let type_id = ty.as_type_id().unwrap();
        let layout = self.tctx.layout_of(ty);

        let cir_struct_type = self.tctx.get_struct(type_id);
        let struct_type = self.emit_struct_type(type_id);

        let mut all_const = true;
        let mut values: Vec<(Option<usize>, InternalValue<'ll>)> = Vec::new();

        for field_offset in &layout.field_offsets {
            match field_offset {
                ABIFieldOffsetInfo::Normal { original_index, .. } => {
                    let cir_field_type = &cir_struct_type.fields[*original_index];

                    let expr = &struct_init.fields[*original_index];
                    let lvalue = self.emit_expr(expr, &Some(cir_field_type.clone()));
                    let mut rvalue = self.load_rvalue(lvalue);

                    let field_original_index = field_offset.original_index().unwrap();
                    let target_type = cir_struct_type.fields.get(field_original_index).unwrap();

                    if !self.llvm_builder.get_insert_block().is_none() {
                        rvalue = self.emit_implicit_cast(target_type, rvalue);
                    }

                    if !is_basic_value_constant(rvalue.as_basic_value()) {
                        all_const = false;
                    }

                    values.push((Some(*original_index), rvalue));
                }
                ABIFieldOffsetInfo::Padding { size, .. } => {
                    let cir_array_type = CIRType::Array(CIRArrayType {
                        element_type: Box::new(CIRType::Plain(PlainType::Int8)),
                        len: *size as usize,
                    });

                    let padding_value = self.llvm_ctx.i8_type().array_type(*size).const_zero();

                    values.push((
                        None,
                        InternalValue::new(
                            cir_array_type,
                            InternalValueKind::RValue(padding_value.as_basic_value_enum()),
                        ),
                    ));
                }
            }
        }

        let mut struct_value: StructValue<'ll>;

        if must_init_via_memcpy(&cir_struct_type.fields) {
            let field_values = values
                .iter()
                .filter_map(|(original_index, value)| {
                    original_index.map(|i| {
                        let field_type = cir_struct_type.fields[i].clone();

                        ((Some(i), value.clone()), field_type)
                    })
                })
                .collect::<Vec<_>>();

            struct_value = self.emit_struct_init_via_memcpy(&layout, struct_type, &field_values);
        } else {
            if all_const {
                let field_values = values
                    .iter()
                    .map(|(_, value)| value.as_basic_value())
                    .collect::<Vec<_>>();

                struct_value = struct_type.const_named_struct(&field_values);
            } else {
                struct_value = struct_type.const_zero();

                values.iter().enumerate().for_each(|(index, (_, rvalue))| {
                    struct_value = self
                        .llvm_builder
                        .build_insert_value(
                            struct_value,
                            rvalue.as_basic_value(),
                            index.try_into().unwrap(),
                            "struct.insert",
                        )
                        .unwrap()
                        .into_struct_value();
                });
            }
        }

        InternalValue::new(ty.clone(), InternalValueKind::RValue(struct_value.into()))
    }

    fn emit_struct_init_via_memcpy(
        &self,
        layout: &ABITypeLayout,
        struct_type: StructType<'ll>,
        values: &Vec<((Option<usize>, InternalValue<'ll>), CIRType)>,
    ) -> StructValue<'ll> {
        let struct_ptr = self.llvm_builder.build_alloca(struct_type, "struct.init").unwrap();

        for ((original_index, field_value), field_cir_ty) in values {
            if original_index.is_none() {
                // skip if padding
                continue;
            }

            let llvm_index = layout.lookup_field_index(original_index.unwrap()).unwrap();

            let field_ptr = self
                .llvm_builder
                .build_struct_gep(struct_type, struct_ptr, llvm_index as u32, "struct.field")
                .unwrap();

            self.emit_store(field_ptr, field_value.clone(), field_cir_ty.clone());
        }

        self.llvm_builder
            .build_load(struct_type, struct_ptr, "struct.rvalue")
            .unwrap()
            .into_struct_value()
    }

    fn emit_call(&mut self, call: &CIRCall) -> InternalValue<'ll> {
        match &call.dispatch {
            CIRCallDispatch::Normal { irv_id, func_type, .. } => {
                let llvm_func_value = self.get_or_declare_function(*irv_id).as_func().cloned().unwrap();

                self.emit_direct_call(&func_type, &call.args, &call.ret_type, &llvm_func_value)
            }

            CIRCallDispatch::FunctionPointer { operand } => {
                let lvalue = self.emit_expr(&operand, &None);
                let rvalue = self.load_rvalue(lvalue);

                self.emit_indirect_call(call, rvalue)
            }

            CIRCallDispatch::Interface {
                operand,
                index,
                func_type,
            } => self.emit_interface_method_call(call, operand, *index, func_type),

            CIRCallDispatch::Method {
                irv_id,
                func_type,
                self_meta,
                ..
            } => self.emit_method_call(call, *irv_id, func_type, self_meta),

            CIRCallDispatch::Builtin { builtin_spec } => self.emit_builtin_call(call, builtin_spec),
        }
    }

    fn emit_interface_method_call(
        &mut self,
        call: &CIRCall,
        operand: &CIRExpr,
        method_idx: usize,
        func_type: &CIRFuncType,
    ) -> InternalValue<'ll> {
        let dyn_value = {
            let lvalue = self.emit_expr(operand, &None);
            let rvalue = self.load_rvalue(lvalue);
            rvalue.as_basic_value().into_struct_value()
        };

        let data_ptr = self
            .llvm_builder
            .build_extract_value(dyn_value, 0, "dyn.data")
            .unwrap()
            .into_pointer_value();

        let vtable_ptr = self
            .llvm_builder
            .build_extract_value(dyn_value, 1, "dyn.vtable")
            .unwrap()
            .into_pointer_value();

        let ptr_type = self.llvm_ctx.ptr_type(AddressSpace::default());

        let method_gep = unsafe {
            let idx = self.llvm_ctx.i64_type().const_int(method_idx as u64, false);
            self.llvm_builder
                .build_gep(ptr_type, vtable_ptr, &[idx], "vtable.method.gep")
                .unwrap()
        };

        let fn_ptr = self
            .llvm_builder
            .build_load(ptr_type, method_gep, "vtable.method.load")
            .unwrap()
            .into_pointer_value();

        let abi_func_info = self.target.target_abi.classify_func(func_type).unwrap();

        let mut llvm_args: Vec<BasicMetadataValueEnum<'ll>> = Vec::new();

        let self_value = InternalValue::new(
            func_type.params[0].clone(),
            InternalValueKind::RValue(data_ptr.as_basic_value_enum()),
        );

        let self_param_types = &abi_func_info.params_types[0..1];
        let self_abi_info = &abi_func_info.params_infos[0];

        self.emit_abi_arg(
            self_param_types,
            self_abi_info,
            &self_value, // lvalue for ABI (not used, but okay)
            &self_value, // rvalue (actual)
            &mut llvm_args,
        );

        let remaining_param_infos = &abi_func_info.params_infos[1..];
        let remaining_param_types = &abi_func_info.params_types[1..];

        let mut normal_args = self.emit_func_args(
            &call.args,
            remaining_param_infos,
            remaining_param_types,
            &func_type.params,
        );

        llvm_args.append(&mut normal_args);

        let llvm_func_type = self.emit_func_type(func_type.clone());

        let call_site = self
            .llvm_builder
            .build_indirect_call(llvm_func_type, fn_ptr, &llvm_args, "ifc.call")
            .unwrap();

        // Attach ABI call attributes
        self.emit_func_call_attributes(&abi_func_info, FuncCallKind::Indirect(call_site));

        if let Some(mut basic_value) = call_site.try_as_basic_value().basic() {
            let actual_return_type: BasicTypeEnum<'ll> =
                self.emit_type(*func_type.ret_type.clone()).try_into().unwrap();

            basic_value = self.intrinsic_coerce_through_alloca(basic_value, actual_return_type, "ifc.ret.coerce");

            InternalValue::new(call.ret_type.clone(), InternalValueKind::RValue(basic_value))
        } else {
            // void return
            self.emit_null(call.ret_type.clone())
        }
    }

    fn emit_method_call(
        &mut self,
        call: &CIRCall,
        irv_id: IRValueID,
        cir_func_type: &CIRFuncType,
        self_meta_opt: &Option<CIRCallMethodSelfMetadata>,
    ) -> InternalValue<'ll> {
        let llvm_func_value = self.get_or_declare_function(irv_id).as_func().cloned().unwrap();

        let abi_func_info = self.target.target_abi.classify_func(cir_func_type).unwrap();

        let mut llvm_args: Vec<BasicMetadataValueEnum<'ll>> = Vec::new();

        let mut abi_param_index = 0;

        // emit self argument (if exists)
        if let Some(self_meta) = self_meta_opt {
            let (lvalue, rvalue) = self.emit_self_argument(self_meta.clone());

            // use param index 0 for self
            let self_param_types = &abi_func_info.params_types[0..1];
            let self_abi_info = &abi_func_info.params_infos[0];

            self.emit_abi_arg(self_param_types, self_abi_info, &lvalue, &rvalue, &mut llvm_args);

            abi_param_index = 1; // advance index
        }

        // emit normal arguments
        let remaining_param_infos = &abi_func_info.params_infos[abi_param_index..];

        let remaining_param_types = &abi_func_info.params_types[abi_param_index..];

        let mut normal_args = self.emit_func_args(
            &call.args,
            remaining_param_infos,
            remaining_param_types,
            &cir_func_type.params,
        );

        llvm_args.append(&mut normal_args);

        self.emit_call_with_args(
            &abi_func_info,
            cir_func_type,
            &call.ret_type,
            llvm_func_value,
            llvm_args,
        )
    }

    fn emit_self_argument(&mut self, self_meta: CIRCallMethodSelfMetadata) -> (InternalValue<'ll>, InternalValue<'ll>) {
        if self_meta.use_fat_ptr_data {
            let fat_ptr_value = {
                let value = self.emit_expr(&self_meta.operand, &None);
                self.load_rvalue(value)
            };

            let fat_ptr_struct_value = fat_ptr_value.as_basic_value().into_struct_value();

            let struct_type = fat_ptr_value.ty.as_struct(&self.tctx).unwrap();
            let data_type = struct_type.fields.first().cloned().unwrap();

            debug_assert!(data_type.is_pointer()); // always `void*`

            // let llvm_struct_type = self.emit_struct_type(struct_type);
            let data_ptr = self
                .llvm_builder
                .build_extract_value(AggregateValueEnum::StructValue(fat_ptr_struct_value), 0, "fat_ptr.data")
                .unwrap();

            let data_value = InternalValue::new(
                data_type.clone(),
                InternalValueKind::RValue(data_ptr.as_basic_value_enum()),
            );

            (data_value.clone(), data_value)
        } else {
            if self_meta.is_referenced {
                let value = self.emit_lvalue_address(&self_meta.operand);

                if value.ty.is_pointer() {
                    // already a pointer-to-object
                    let rvalue = self.load_rvalue(value);

                    (rvalue.clone(), rvalue)
                } else {
                    if value.is_rvalue() && !value.ty.is_pointer() {
                        let llvm_type: BasicTypeEnum<'ll> = self.emit_type(value.ty.clone()).try_into().unwrap();

                        let temp = self.llvm_builder.build_alloca(llvm_type, "temp.self").unwrap();
                        self.llvm_builder.build_store(temp, value.as_basic_value()).unwrap();

                        let lvalue = InternalValue::new(value.ty, InternalValueKind::LValue(temp));

                        (lvalue.clone(), lvalue)
                    } else {
                        debug_assert!(value.is_lvalue());

                        (value.clone(), value)
                    }
                }
            } else {
                let lvalue = self.emit_expr(&self_meta.operand, &None);
                let rvalue = self.load_rvalue(lvalue.clone());

                (lvalue, rvalue)
            }
        }
    }

    pub(crate) fn emit_direct_call(
        &mut self,
        cir_func_type: &CIRFuncType,
        args: &Vec<CIRExpr>,
        ret_type: &CIRType,
        llvm_func_value: &FunctionValue<'ll>,
    ) -> InternalValue<'ll> {
        let abi_func_info = cir_func_type.abi_func_info.as_ref().unwrap();

        let llvm_args = self.emit_func_args(
            args,
            &abi_func_info.params_infos,
            &abi_func_info.params_types,
            &cir_func_type.params,
        );

        let abi_func_info = self.target.target_abi.classify_func(cir_func_type).unwrap();

        self.emit_call_with_args(&abi_func_info, cir_func_type, ret_type, *llvm_func_value, llvm_args)
    }

    fn emit_call_with_args(
        &mut self,
        abi_func_info: &ABIFunctionInfo,
        cir_func_type: &CIRFuncType,
        ret_type: &CIRType,
        llvm_func_value: FunctionValue<'ll>,
        mut llvm_args: Vec<BasicMetadataValueEnum<'ll>>,
    ) -> InternalValue<'ll> {
        let mut sret_alloca: Option<PointerValue<'ll>> = None;

        if abi_func_info.ret_info.kind.is_indirect_sret() {
            let sret_type: BasicTypeEnum<'ll> = self.emit_type(*cir_func_type.ret_type.clone()).try_into().unwrap();

            let sret_ptr = {
                if self.is_return && self.cur_sret.is_some() {
                    // We are in a return statement and this function has an SRet param
                    // pass the current function's SRet pointer directly
                    self.cur_sret.unwrap()
                } else {
                    // Normal case, allocate a new temporary
                    self.llvm_builder.build_alloca(sret_type, "sret").unwrap()
                }
            };

            llvm_args.insert(0, sret_ptr.into());
            sret_alloca = Some(sret_ptr);
        }

        let call_site = self
            .llvm_builder
            .build_call(llvm_func_value, &llvm_args, "call")
            .unwrap();

        self.emit_call_site_attributes(&cir_func_type, abi_func_info, &call_site);
        self.emit_func_call_attributes(&abi_func_info, FuncCallKind::Direct(llvm_func_value));

        if let Some(ptr) = sret_alloca {
            InternalValue::new(ret_type.clone(), InternalValueKind::LValue(ptr.into()))
        } else if let Some(mut basic_value) = call_site.try_as_basic_value().basic() {
            let actual_return_type: BasicTypeEnum<'ll> =
                self.emit_type(*cir_func_type.ret_type.clone()).try_into().unwrap();

            // optimization, do not coerce if it matches actual return type
            if actual_return_type == basic_value.get_type() {
                InternalValue::new(ret_type.clone(), InternalValueKind::RValue(basic_value))
            } else {
                basic_value = self.intrinsic_coerce_through_alloca(basic_value, actual_return_type, "coerce_ret");

                InternalValue::new(ret_type.clone(), InternalValueKind::RValue(basic_value))
            }
        } else {
            self.emit_null(ret_type.clone())
        }
    }

    fn emit_indirect_call(&mut self, func_call: &CIRCall, operand: InternalValue<'ll>) -> InternalValue<'ll> {
        let cir_func_type = operand.ty.as_func().unwrap();
        let llvm_func_type = self.emit_func_type(cir_func_type.clone());

        let abi_func_info = self.target.target_abi.classify_func(&cir_func_type).unwrap();

        let llvm_args = self.emit_func_args(
            &func_call.args,
            &abi_func_info.params_infos,
            &abi_func_info.params_types,
            &cir_func_type.params,
        );

        let fn_ptr = operand.as_basic_value().into_pointer_value();

        let call_site = self
            .llvm_builder
            .build_indirect_call(llvm_func_type, fn_ptr, &llvm_args, "indirect_call")
            .unwrap();

        self.emit_call_site_attributes(&cir_func_type, &abi_func_info, &call_site);
        self.emit_func_call_attributes(&abi_func_info, FuncCallKind::Indirect(call_site));

        if let Some(mut basic_value) = call_site.try_as_basic_value().basic() {
            // REVIEW: Optimization Required
            // coerce back from abi return type to actual return type
            let actual_return_type: BasicTypeEnum<'ll> =
                self.emit_type(*cir_func_type.ret_type.clone()).try_into().unwrap();

            basic_value = self.intrinsic_coerce_through_alloca(basic_value, actual_return_type, "coerce_ret");

            InternalValue::new(func_call.ret_type.clone(), InternalValueKind::RValue(basic_value))
        } else {
            self.emit_null(func_call.ret_type.clone())
        }
    }

    fn emit_literal(&mut self, lit: &CIRLiteral) -> InternalValue<'ll> {
        let ty: BasicTypeEnum<'ll> = self.emit_type(lit.ty.clone()).try_into().unwrap();

        let basic_value = match &lit.kind {
            CIRLiteralKind::Char(value) => {
                BasicValueEnum::IntValue(self.llvm_ctx.i8_type().const_int(*value as u64, false))
            }
            CIRLiteralKind::Bool(value) => {
                BasicValueEnum::IntValue(self.llvm_ctx.bool_type().const_int(*value as u64, false))
            }
            CIRLiteralKind::Null => {
                BasicValueEnum::PointerValue(self.llvm_ctx.ptr_type(AddressSpace::default()).const_null())
            }
            CIRLiteralKind::Integer(value, _) => {
                let int_type = ty.into_int_type();
                let bit_width = int_type.get_bit_width() as usize;

                let num_words = (bit_width + 63) / 64;
                let full_value = value.as_int::<u128>();

                let mut words = Vec::with_capacity(num_words);
                for i in 0..num_words {
                    words.push((full_value >> (i * 64)) as u64);
                }

                BasicValueEnum::IntValue(int_type.const_int_arbitrary_precision(&words))
            }
            CIRLiteralKind::CString(value) => self.emit_cstring(value.clone()),
            CIRLiteralKind::ByteString(value) => self.emit_bytestring(value.clone()),
            CIRLiteralKind::Float(value) => BasicValueEnum::FloatValue(ty.into_float_type().const_float(*value)),
        };

        InternalValue::new(lit.ty.clone(), InternalValueKind::RValue(basic_value))
    }

    pub(crate) fn emit_null(&self, ty: CIRType) -> InternalValue<'ll> {
        let basic_value = BasicValueEnum::PointerValue(self.llvm_ctx.ptr_type(AddressSpace::default()).const_null());

        InternalValue::new(ty, InternalValueKind::RValue(basic_value))
    }

    pub(crate) fn emit_cstring(&mut self, value: String) -> BasicValueEnum<'ll> {
        if let Some(global_value) = self.string_cache.get(&value) {
            return global_value.as_pointer_value().into();
        }

        let const_str = self.llvm_ctx.const_string(value.as_bytes(), true);

        let llvm_module = self.llvm_module.borrow();
        let global_value = llvm_module.add_global(const_str.get_type(), None, ".cstring");
        global_value.set_initializer(&const_str);
        global_value.set_constant(true);
        global_value.set_unnamed_addr(true);
        global_value.set_linkage(Linkage::Private);
        global_value.set_alignment(1);
        drop(llvm_module);

        self.string_cache.insert(value, global_value);

        global_value.as_pointer_value().into()
    }

    fn emit_bytestring(&self, value: String) -> BasicValueEnum<'ll> {
        self.llvm_ctx.const_string(value.as_bytes(), true).into()
    }
}

#[inline]
fn must_init_via_memcpy(fields: &Vec<CIRType>) -> bool {
    fields.iter().any(|ty| ty.is_union())
}
