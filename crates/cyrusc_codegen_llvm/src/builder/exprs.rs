/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

use crate::{
    builder::{
        builder::IRBuilderCtx,
        funcs::FuncCallKind,
        irreg::LocalIRValue,
        values::{InternalValue, InternalValueKind},
    },
    llvm::{abi::abi_type::abi_type_to_llvm_type, constness::is_basic_value_constant},
};
use cyrusc_abi::{
    ast_defs::Linkage,
    modifiers::{FuncModifiers, GlobalVarModifiers},
};
use cyrusc_ast::operators::{InfixOperator, PrefixOperator, UnaryOperator};
use cyrusc_internal::{
    abi::types::ABIType,
    cir::{
        cir::{
            CIRAddrOfExpr, CIRArrayExpr, CIRArrayIndexExpr, CIRAssignExpr, CIRDerefExpr, CIRDynamicExpr,
            CIREnumInitExpr, CIREnumInitVariant, CIRExpr, CIRExprKind, CIRFuncCall, CIRInfixExpr,
            CIRInterfaceMethodCall, CIRLiteral, CIRLiteralKind, CIRMonomorphFuncInstanceCall, CIRPrefixExpr,
            CIRSizeOfExpr, CIRStructFieldAccessExpr, CIRStructInitExpr, CIRTupleAccessExpr, CIRTupleExpr, CIRUnaryExpr,
            CIRUnionFieldAccessExpr, CIRUnionInitExpr, CIRValue, CIRValueKind, cir_func_decl_as_func_ty,
        },
        types::{CIRFuncTy, CIRStructTy, CIRTupleTy, CIRTy},
    },
};
use cyrusc_tast::types::PlainType;
use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    types::{AnyTypeEnum, ArrayType, BasicTypeEnum, StructType},
    values::{
        AnyValueEnum, ArrayValue, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue,
    },
};

#[allow(unused)]
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
            CIRExprKind::Unary(unary_expr) => self.emit_unary_expr(unary_expr),
            CIRExprKind::SizeOf(sizeof_expr) => self.emit_sizeof(sizeof_expr),
            CIRExprKind::Assign(assign_expr) => self.emit_assign(assign_expr),
            CIRExprKind::Cast(cast_expr) => {
                let lvalue = self.emit_expr(&cast_expr.operand);
                let rvalue = self.load_rvalue(lvalue);
                let target_type = self.emit_ty(*cast_expr.ty.clone());
                let casted_value: BasicValueEnum<'ll> = self.emit_cast(target_type, rvalue).try_into().unwrap();
                InternalValue::new(*cast_expr.ty.clone(), InternalValueKind::RValue(casted_value))
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
            CIRExprKind::Dynamic(dynamic_expr) => self.emit_dynamic_expr(dynamic_expr),
            CIRExprKind::InterfaceMethodCall(interface_method_call) => {
                self.emit_interface_method_call(interface_method_call)
            }
        }
    }

    fn emit_dynamic_expr(&mut self, dynamic_expr: &CIRDynamicExpr) -> InternalValue<'ll> {
        {
            let data_value = self.emit_lvalue_address(&dynamic_expr.data_expr).as_basic_value();

            let data_ptr = {
                if data_value.is_pointer_value() {
                    data_value.into_pointer_value()
                } else {
                    let temp_ptr = self
                        .llvmbuilder
                        .build_alloca(data_value.get_type(), "dynamic.temp")
                        .unwrap();
                    self.llvmbuilder.build_store(temp_ptr, data_value).unwrap();
                    temp_ptr
                }
            };

            let vtable_global_var_ptr = {
                if let Some(local_ir_value) = {
                    let irreg = self.irreg.borrow();
                    irreg.get(dynamic_expr.global_var_id)
                } {
                    local_ir_value.as_global().unwrap().as_pointer_value()
                } else {
                    let methods: Vec<BasicValueEnum<'ll>> = dynamic_expr
                        .method_decls
                        .iter()
                        .map(|func_decl| {
                            let fn_value = self.emit_func_decl(func_decl);
                            fn_value.as_global_value().as_pointer_value().as_basic_value_enum()
                        })
                        .collect();

                    let vtable_type = self.emit_vtable_ty(methods.len());

                    let llvmmodule = self.llvmmodule.borrow_mut();

                    let global_value = llvmmodule.add_global(vtable_type, None, &dynamic_expr.vtable_abi_name);

                    global_value.set_initializer(&vtable_type.const_named_struct(&methods));
                    global_value.as_pointer_value()
                }
            };

            let dynamic_struct_type = self.emit_dynamic_ty();

            let mut dynamic_struct_value = dynamic_struct_type.get_undef();
            dynamic_struct_value = self
                .llvmbuilder
                .build_insert_value(dynamic_struct_value, data_ptr.as_basic_value_enum(), 0, "insert")
                .unwrap()
                .into_struct_value();
            dynamic_struct_value = self
                .llvmbuilder
                .build_insert_value(
                    dynamic_struct_value,
                    vtable_global_var_ptr.as_basic_value_enum(),
                    1,
                    "insert",
                )
                .unwrap()
                .into_struct_value();

            InternalValue::new(
                self.cir_dynamic_ty(dynamic_expr.data_expr.ty.clone()),
                InternalValueKind::RValue(dynamic_struct_value.as_basic_value_enum()),
            )
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

    fn emit_array_index(&mut self, array_index: &CIRArrayIndexExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_lvalue_address(&array_index.operand);

        let index_lvalue = self.emit_expr(&array_index.index);
        let index_rvalue = self.load_rvalue(index_lvalue);

        if lvalue.ty.as_array().is_some() {
            let arr_ty = lvalue.ty.as_array().unwrap();
            let basic_value = lvalue.as_basic_value();

            if basic_value.is_pointer_value() {
                self.emit_inbounds_checked_array_index(
                    lvalue.as_basic_value().into_pointer_value(),
                    *arr_ty.ty.clone(),
                    index_rvalue,
                    arr_ty.len.try_into().unwrap(),
                )
            } else if basic_value.is_array_value() {
                let ptr = self.emit_temp_array_value_alloca(&basic_value.into_array_value());

                self.emit_inbounds_checked_array_index(
                    ptr, // use temp alloca instead
                    *arr_ty.ty.clone(),
                    index_rvalue,
                    arr_ty.len.try_into().unwrap(),
                )
            } else {
                unreachable!("Expected array or pointer type for array indexing expression");
            }
        } else if let Some(pointee_ty) = lvalue.ty.pointer_inner() {
            self.emit_array_index_on_pointer(
                lvalue.as_basic_value().into_pointer_value(),
                index_rvalue,
                pointee_ty.clone(),
            )
        } else {
            unreachable!("Expected array or pointer type for array indexing expression");
        }
    }

    fn emit_temp_array_value_alloca(&self, array_value: &ArrayValue<'ll>) -> PointerValue<'ll> {
        let ptr = self.llvmbuilder.build_alloca(array_value.get_type(), "temp").unwrap();
        self.llvmbuilder
            .build_store(ptr, array_value.as_basic_value_enum())
            .unwrap();
        ptr
    }

    fn emit_assign(&mut self, assign: &CIRAssignExpr) -> InternalValue<'ll> {
        let lhs_lvalue = self.emit_lvalue_address(&assign.lhs);

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

    pub(crate) fn emit_cast_basic_value_to_target_abi_type(
        &self,
        value: BasicValueEnum<'ll>,
        from_cir_type: &CIRTy,
        target_type: ABIType,
    ) -> BasicValueEnum<'ll> {
        let from_type = value.get_type();
        let target_basic_type: BasicTypeEnum<'ll> =
            abi_type_to_llvm_type(self.llvmctx, &self.target.info, &target_type)
                .try_into()
                .unwrap();

        if from_type == target_basic_type {
            return value;
        }

        match (from_type, target_basic_type) {
            (BasicTypeEnum::IntType(from_int), BasicTypeEnum::IntType(to_int)) => {
                let from_width = from_int.get_bit_width();
                let to_width = to_int.get_bit_width();

                if from_width < to_width {
                    if from_cir_type.is_signed_integer() {
                        self.llvmbuilder
                            .build_int_s_extend(value.into_int_value(), to_int, "sext")
                            .unwrap()
                            .into()
                    } else {
                        self.llvmbuilder
                            .build_int_z_extend(value.into_int_value(), to_int, "zext")
                            .unwrap()
                            .into()
                    }
                } else if from_width > to_width {
                    self.llvmbuilder
                        .build_int_truncate(value.into_int_value(), to_int, "trunc")
                        .unwrap()
                        .into()
                } else {
                    self.llvmbuilder
                        .build_bit_cast(value, target_basic_type, "bitcast")
                        .unwrap()
                }
            }
            (BasicTypeEnum::PointerType(_), BasicTypeEnum::IntType(to_int)) => self
                .llvmbuilder
                .build_ptr_to_int(value.into_pointer_value(), to_int, "ptrtoint")
                .unwrap()
                .into(),
            (BasicTypeEnum::IntType(_), BasicTypeEnum::PointerType(to_ptr)) => self
                .llvmbuilder
                .build_int_to_ptr(value.into_int_value(), to_ptr, "inttoptr")
                .unwrap()
                .into(),
            (BasicTypeEnum::FloatType(_), BasicTypeEnum::FloatType(to_float)) => self
                .llvmbuilder
                .build_float_cast(value.into_float_value(), to_float, "fpext")
                .unwrap()
                .into(),
            (BasicTypeEnum::IntType(_), BasicTypeEnum::FloatType(to_float)) => {
                if from_cir_type.is_signed_integer() {
                    self.llvmbuilder
                        .build_signed_int_to_float(value.into_int_value(), to_float, "sitofp")
                        .unwrap()
                        .into()
                } else {
                    self.llvmbuilder
                        .build_unsigned_int_to_float(value.into_int_value(), to_float, "uitofp")
                        .unwrap()
                        .into()
                }
            }
            (BasicTypeEnum::PointerType(_), BasicTypeEnum::PointerType(to_ptr)) => self
                .llvmbuilder
                .build_pointer_cast(value.into_pointer_value(), to_ptr, "ptrcast")
                .unwrap()
                .into(),
            (BasicTypeEnum::VectorType(_), BasicTypeEnum::VectorType(_)) => self
                .llvmbuilder
                .build_bit_cast(value, target_basic_type, "bitcast")
                .unwrap(),
            _ => self
                .llvmbuilder
                .build_bit_cast(value, target_basic_type, "bitcast")
                .unwrap(),
        }
    }

    pub(crate) fn emit_cast(&self, target_type: AnyTypeEnum<'ll>, value: InternalValue<'ll>) -> AnyValueEnum<'ll> {
        let basic_value = value.as_basic_value();

        match target_type {
            AnyTypeEnum::IntType(int_type) => {
                let val = basic_value;
                if val.is_int_value() {
                    let bit_width = val.into_int_value().get_type().get_bit_width();

                    if bit_width == 1 {
                        AnyValueEnum::IntValue(
                            self.llvmbuilder
                                .build_int_z_extend(val.into_int_value(), int_type, "bool_zext")
                                .unwrap(),
                        )
                    } else {
                        // int -> int
                        AnyValueEnum::IntValue(
                            self.llvmbuilder
                                .build_int_cast(val.into_int_value(), int_type, "cast")
                                .unwrap(),
                        )
                    }
                } else if val.is_pointer_value() {
                    // ptr -> int
                    AnyValueEnum::IntValue(
                        self.llvmbuilder
                            .build_ptr_to_int(val.into_pointer_value(), int_type, "ptr_to_int")
                            .unwrap(),
                    )
                } else {
                    val.into()
                }
            }
            AnyTypeEnum::FloatType(float_type) => {
                if basic_value.is_int_value() {
                    let is_signed = value
                        .ty
                        .as_plain()
                        .and_then(|plain_type| Some(plain_type.is_signed()))
                        .unwrap_or(false);

                    if is_signed {
                        AnyValueEnum::FloatValue(
                            self.llvmbuilder
                                .build_signed_int_to_float(basic_value.into_int_value(), float_type, "cast")
                                .unwrap(),
                        )
                    } else {
                        AnyValueEnum::FloatValue(
                            self.llvmbuilder
                                .build_unsigned_int_to_float(basic_value.into_int_value(), float_type, "cast")
                                .unwrap(),
                        )
                    }
                } else if basic_value.is_float_value() {
                    AnyValueEnum::FloatValue(
                        self.llvmbuilder
                            .build_float_cast(basic_value.into_float_value(), float_type, "cast")
                            .unwrap(),
                    )
                } else {
                    AnyValueEnum::FloatValue(
                        self.llvmbuilder
                            .build_float_cast(basic_value.into_float_value(), float_type, "cast")
                            .unwrap(),
                    )
                }
            }
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

    fn emit_array(&mut self, array: &CIRArrayExpr) -> InternalValue<'ll> {
        let cir_arr_ty = array.ty.as_array().expect("Expected array type");
        let element_ty = cir_arr_ty.ty.clone();

        let arr_ty: ArrayType<'ll> = self.emit_arr_ty(cir_arr_ty).try_into().expect("Expected ArrayType");

        let required_len = array.elms.len();
        let mut elements = Vec::with_capacity(required_len);
        let mut all_const = true;

        for expr in &array.elms {
            let lvalue = self.emit_expr(expr);
            let mut rvalue = self.load_rvalue(lvalue);

            if !self.llvmbuilder.get_insert_block().is_none() {
                rvalue = self.emit_implicit_cast(&element_ty, rvalue);
            }

            if !is_basic_value_constant(rvalue.as_basic_value()) {
                all_const = false;
            }

            elements.push(rvalue.as_basic_value());
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

    fn emit_addr_of(&mut self, addr_of: &CIRAddrOfExpr) -> InternalValue<'ll> {
        let operand = self.emit_expr(&addr_of.operand);
        InternalValue::new(
            CIRTy::Pointer(Box::new(operand.ty.clone())),
            InternalValueKind::RValue(operand.as_basic_value()),
        )
    }

    pub(crate) fn emit_lvalue_address(&mut self, expr: &CIRExpr) -> InternalValue<'ll> {
        match &expr.kind {
            CIRExprKind::Deref(deref_expr) => {
                let lvalue = self.emit_expr(&deref_expr.operand);
                let rvalue = self.load_rvalue(lvalue.clone());
                let ptr = rvalue.as_basic_value().into_pointer_value();
                let inner_ty = rvalue.ty.pointer_inner().unwrap();
                InternalValue::new(inner_ty.clone(), InternalValueKind::LValue(ptr))
            }
            CIRExprKind::StructFieldAccess(struct_field_access) => {
                let lvalue = self.emit_lvalue_address(&struct_field_access.operand);

                let struct_ptr_value = lvalue.as_basic_value().into_pointer_value();
                let struct_ty = lvalue.ty.clone();

                let llvm_struct_ty = self.emit_ty(struct_ty).into_struct_type();

                let field_ptr = self
                    .llvmbuilder
                    .build_struct_gep(
                        llvm_struct_ty,
                        struct_ptr_value,
                        struct_field_access.field_idx as u32,
                        "field_ptr",
                    )
                    .unwrap();

                let field_ty = struct_field_access.field_ty.clone();
                InternalValue::new(field_ty, InternalValueKind::LValue(field_ptr))
            }
            _ => self.emit_expr(expr),
        }
    }

    fn emit_deref(&mut self, deref: &CIRDerefExpr, mode: DerefMode) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&deref.operand);
        let rvalue = self.load_rvalue(lvalue.clone());
        let ptr = rvalue.as_basic_value().into_pointer_value();

        match mode {
            DerefMode::Load => {
                let inner_ty = rvalue.ty.pointer_inner().unwrap();

                let llvm_ty: BasicTypeEnum<'ll> = self.emit_ty(inner_ty.clone()).try_into().unwrap();
                let loaded_value = self.llvmbuilder.build_load(llvm_ty, ptr, "deref").unwrap();
                InternalValue::new(inner_ty.clone(), InternalValueKind::RValue(loaded_value.into()))
            }
            DerefMode::Store => self.emit_lvalue_address(&CIRExpr {
                kind: CIRExprKind::Deref(deref.clone()),
                ty: deref.operand.ty.pointer_inner().cloned().unwrap(),
            }),
        }
    }

    fn emit_sizeof(&mut self, sizeof_expr: &CIRSizeOfExpr) -> InternalValue<'ll> {
        let ty = self.emit_ty(sizeof_expr.ty.clone());
        let size_value = ty.size_of().unwrap();
        InternalValue::new(sizeof_expr.ty.clone(), InternalValueKind::RValue(size_value.into()))
    }

    fn emit_unary_expr(&mut self, unary_expr: &CIRUnaryExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_lvalue_address(&unary_expr.operand);
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
                let new_rhs_rvalue = self.emit_add(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                new_rhs_rvalue
            }
            UnaryOperator::PreDecrement => {
                let new_rhs_rvalue = self.emit_sub(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                new_rhs_rvalue
            }
            UnaryOperator::PostIncrement => {
                let rhs_rvalue_clone = rvalue.clone();
                let new_rhs_rvalue = self.emit_add(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                rhs_rvalue_clone
            }
            UnaryOperator::PostDecrement => {
                let rhs_rvalue_clone = rvalue.clone();
                let new_rhs_rvalue = self.emit_sub(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                rhs_rvalue_clone
            }
        }
    }

    fn emit_infix_expr(&mut self, infix_expr: &CIRInfixExpr) -> InternalValue<'ll> {
        let lhs_lvalue = self.emit_expr(&infix_expr.lhs);
        let rhs_lvalue = self.emit_expr(&infix_expr.rhs);

        let mut lhs_rvalue = self.load_rvalue(lhs_lvalue.clone());
        let mut rhs_rvalue = self.load_rvalue(rhs_lvalue.clone());

        if lhs_rvalue.ty.is_integer() && rhs_rvalue.ty.is_integer() {
            (lhs_rvalue, rhs_rvalue) = self.widen_int_pair(lhs_rvalue, rhs_rvalue);
        }

        let get_signed = || rhs_rvalue.ty.as_plain().unwrap().is_signed();

        match infix_expr.op {
            InfixOperator::Add => self.emit_add(lhs_rvalue, rhs_rvalue),
            InfixOperator::Sub => self.emit_sub(lhs_rvalue, rhs_rvalue),
            InfixOperator::Mul => self.emit_mul(lhs_rvalue, rhs_rvalue),
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
            InfixOperator::Or => self.emit_logical_or(lhs_rvalue, rhs_rvalue),
            InfixOperator::And => self.emit_logical_and(lhs_rvalue, rhs_rvalue),
            InfixOperator::BitwiseAnd => self.emit_bitwise_and(lhs_rvalue, rhs_rvalue),
            InfixOperator::BitwiseOr => self.emit_bitwise_or(lhs_rvalue, rhs_rvalue),
            InfixOperator::BitwiseXor => self.emit_xor(lhs_rvalue, rhs_rvalue),
            InfixOperator::BitwiseAndNot => self.emit_bitwise_and_not(lhs_rvalue, rhs_rvalue),
            InfixOperator::ShiftLeft => self.emit_shift_left(lhs_rvalue, rhs_rvalue),
            InfixOperator::ShiftRight => self.emit_shift_right(lhs_rvalue, rhs_rvalue),
        }
    }

    fn emit_logical_or(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let or_value = self.llvmbuilder.build_or(lhs, rhs, "lor").unwrap();
                InternalValue::new(
                    CIRTy::PlainType(PlainType::Bool),
                    InternalValueKind::RValue(or_value.into()),
                )
            }
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
        ty: CIRTy,
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

        InternalValue::new(CIRTy::Pointer(Box::new(ty)), InternalValueKind::RValue(selected.into()))
    }

    fn emit_logical_and(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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

    fn emit_xor(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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

    fn emit_bitwise_and(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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

    fn emit_bitwise_or(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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

    fn emit_bitwise_and_not(
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

    fn emit_shift_left(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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

    fn emit_shift_right(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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

    fn emit_add(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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
            (BasicValueEnum::PointerValue(ptr), BasicValueEnum::IntValue(index)) => {
                self.emit_pointer_add(ptr, index, lhs_rvalue.ty.clone())
            }
            (BasicValueEnum::IntValue(index), BasicValueEnum::PointerValue(ptr)) => {
                self.emit_pointer_add(ptr, index, rhs_rvalue.ty.clone())
            }
            _ => unreachable!(),
        }
    }

    fn emit_sub(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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
            (BasicValueEnum::PointerValue(ptr), BasicValueEnum::IntValue(index)) => {
                self.emit_pointer_sub(ptr, index, lhs_rvalue.ty.clone())
            }
            (BasicValueEnum::PointerValue(lhs_ptr), BasicValueEnum::PointerValue(rhs_ptr)) => {
                let pointee_type: BasicTypeEnum<'ll> = self
                    .emit_ty(lhs_rvalue.ty.pointer_inner().unwrap().clone())
                    .try_into()
                    .unwrap();
                self.emit_pointer_diff(pointee_type, lhs_ptr, rhs_ptr)
            }
            _ => unreachable!(),
        }
    }

    fn emit_pointer_add(&self, ptr: PointerValue<'ll>, index: IntValue<'ll>, result_type: CIRTy) -> InternalValue<'ll> {
        let pointee_type: BasicTypeEnum<'ll> = self
            .emit_ty(result_type.pointer_inner().unwrap().clone())
            .try_into()
            .unwrap();

        let i64_type = self.llvmctx.i64_type();
        let gep_index = if index.get_type() == i64_type {
            index
        } else {
            self.llvmbuilder.build_int_cast(index, i64_type, "idx.cast").unwrap()
        };

        // Create GEP instruction
        // LLVM automatically multiplies by sizeof(pointee)
        let gep_ptr = unsafe {
            self.llvmbuilder
                .build_gep(pointee_type, ptr, &[gep_index], "ptr.add")
                .unwrap()
        };

        let basic_value = BasicValueEnum::PointerValue(gep_ptr);
        InternalValue::new(result_type, InternalValueKind::RValue(basic_value))
    }

    fn emit_pointer_sub(&self, ptr: PointerValue<'ll>, index: IntValue<'ll>, result_type: CIRTy) -> InternalValue<'ll> {
        let pointee_type: BasicTypeEnum<'ll> = self
            .emit_ty(result_type.pointer_inner().unwrap().clone())
            .try_into()
            .unwrap();

        // negate the index for subtraction
        let i64_type = self.llvmctx.i64_type();
        let index_i64 = if index.get_type() == i64_type {
            index
        } else {
            self.llvmbuilder.build_int_cast(index, i64_type, "idx.cast").unwrap()
        };

        let neg_index = self.llvmbuilder.build_int_neg(index_i64, "idx.neg").unwrap();

        // Create GEP with negative index
        let gep_ptr = unsafe {
            self.llvmbuilder
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
            .llvmbuilder
            .build_ptr_diff(pointee_type, lhs_ptr, rhs_ptr, "ptr.diff")
            .unwrap();

        let result_type = CIRTy::PlainType(PlainType::ISize);
        let llvm_result_type: BasicTypeEnum<'ll> = self.emit_ty(result_type.clone()).try_into().unwrap();
        let diff_casted = self
            .llvmbuilder
            .build_int_cast(diff_int_value, llvm_result_type.into_int_type(), "ptr.diff.cast")
            .unwrap();

        InternalValue::new(result_type, InternalValueKind::RValue(diff_casted.into()))
    }

    fn emit_mul(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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

    fn emit_div(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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

    fn emit_rem(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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

    fn emit_cmp(
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
            .llvmbuilder
            .build_int_compare(IntPredicate::EQ, strcmp_result, zero, "streq")
            .unwrap();

        InternalValue::new(CIRTy::PlainType(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
    }

    pub(crate) fn emit_cmp_eq(
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
                // streq
                if let (Some(lhs_ptr_inner), Some(rhs_ptr_inner)) =
                    (lhs_rvalue.ty.pointer_inner(), rhs_rvalue.ty.pointer_inner())
                {
                    if lhs_ptr_inner.is_char() && rhs_ptr_inner.is_char() {
                        return self.emit_cmp_eq_const_strings(lhs_rvalue, rhs_rvalue);
                    }
                }

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

    pub(crate) fn emit_cmp_neq(
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

    fn emit_prefix_expr(&mut self, prefix_expr: &CIRPrefixExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&prefix_expr.operand);
        let mut rvalue = self.load_rvalue(lvalue);

        match prefix_expr.op {
            PrefixOperator::Bang => {
                rvalue = self.internal_value_as_bool_i1(rvalue);
                self.emit_logical_not(rvalue)
            }
            PrefixOperator::Minus => self.emit_negate(rvalue),
            PrefixOperator::BitwiseNot => self.emit_bitwise_not(rvalue),
        }
    }

    fn emit_bitwise_not(&self, rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match rvalue.as_basic_value() {
            BasicValueEnum::IntValue(int_value) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_not(int_value, "neg").unwrap());
                InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn emit_negate(&self, rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
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

    fn emit_logical_not(&self, rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match rvalue.as_basic_value() {
            BasicValueEnum::IntValue(int_value) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_not(int_value, "neg").unwrap());
                InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn emit_union_field_access(&mut self, field_access: &CIRUnionFieldAccessExpr) -> InternalValue<'ll> {
        let value = self.emit_lvalue_address(&field_access.operand);

        let union_ty: BasicTypeEnum<'ll> = self.emit_ty(field_access.operand.ty.clone()).try_into().unwrap();
        let union_ptr = match value.kind {
            InternalValueKind::LValue(ptr) => ptr,
            InternalValueKind::RValue(basic_value) => {
                let temp = self.llvmbuilder.build_alloca(union_ty, "union.temp").unwrap();
                self.llvmbuilder.build_store(temp, basic_value).unwrap();
                temp
            }
            _ => unreachable!(),
        };

        let field_ptr = self
            .llvmbuilder
            .build_struct_gep(union_ty.clone(), union_ptr, 0, "union.field")
            .unwrap();

        InternalValue::new(field_access.field_ty.clone(), InternalValueKind::LValue(field_ptr))
    }

    fn emit_struct_field_access(&mut self, field_access: &CIRStructFieldAccessExpr) -> InternalValue<'ll> {
        let operand = self.emit_lvalue_address(&field_access.operand);

        let struct_ty: BasicTypeEnum<'ll> = if let Some(inner) = field_access.operand.ty.pointer_inner() {
            self.emit_ty(inner.clone()).try_into().unwrap()
        } else {
            self.emit_ty(field_access.operand.ty.clone()).try_into().unwrap()
        };

        match operand.kind {
            InternalValueKind::LValue(addr) => {
                let field_addr = self
                    .llvmbuilder
                    .build_struct_gep(struct_ty, addr, field_access.field_idx as u32, "field_gep")
                    .unwrap();

                InternalValue::new(field_access.field_ty.clone(), InternalValueKind::LValue(field_addr))
            }
            InternalValueKind::RValue(val) => {
                let struct_val = val.into_struct_value();

                let field_val = self
                    .llvmbuilder
                    .build_extract_value(struct_val, field_access.field_idx as u32, "field_extract")
                    .unwrap();

                InternalValue::new(field_access.field_ty.clone(), InternalValueKind::RValue(field_val))
            }
            _ => unreachable!(),
        }
    }

    fn emit_tuple_access(&mut self, tuple_access: &CIRTupleAccessExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&tuple_access.operand);
        let rvalue = self.load_rvalue(lvalue);
        let struct_value = rvalue.as_basic_value().into_struct_value();

        let extracted_value = self
            .llvmbuilder
            .build_extract_value(struct_value, tuple_access.index.try_into().unwrap(), "extractvalue")
            .unwrap();

        InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(extracted_value))
    }

    fn emit_tuple(&mut self, tuple: &CIRTupleExpr) -> InternalValue<'ll> {
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
            CIRTy::Tuple(CIRTupleTy { elements: tys }),
            InternalValueKind::RValue(struct_value.into()),
        )
    }

    fn emit_enum_init(&mut self, enum_init_expr: &CIREnumInitExpr) -> InternalValue<'ll> {
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
            CIREnumInitVariant::Ident => {
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

    fn emit_union_init(&mut self, union_init_expr: &CIRUnionInitExpr) -> InternalValue<'ll> {
        self.emit_expr(&union_init_expr.expr)
    }

    fn emit_struct_init_via_memcpy(
        &self,
        struct_type: StructType<'ll>,
        values: &Vec<(InternalValue<'ll>, CIRTy)>,
    ) -> StructValue<'ll> {
        let struct_ptr = self.llvmbuilder.build_alloca(struct_type, "struct.init").unwrap();

        for (index, (field_value, field_cir_ty)) in values.iter().enumerate() {
            let field_ptr = self
                .llvmbuilder
                .build_struct_gep(struct_type, struct_ptr, index as u32, "struct.field")
                .unwrap();

            self.emit_store(field_ptr, field_value.clone(), field_cir_ty.clone());
        }

        self.llvmbuilder
            .build_load(struct_type, struct_ptr, "struct.rvalue")
            .unwrap()
            .into_struct_value()
    }

    fn must_init_via_memcpy(&self, fields: &Vec<CIRTy>) -> bool {
        fields.iter().any(|f| f.is_union())
    }

    pub(crate) fn extract_enum_idx(&self, struct_value: StructValue<'ll>) -> IntValue<'ll> {
        self.llvmbuilder
            .build_extract_value(struct_value, 0, "extract")
            .unwrap()
            .into_int_value()
    }

    pub(crate) fn extract_enum_payload(&self, struct_value: StructValue<'ll>) -> ArrayValue<'ll> {
        self.llvmbuilder
            .build_extract_value(struct_value, 1, "extract")
            .unwrap()
            .into_array_value()
    }

    fn emit_compare_enum_variants(
        &mut self,
        lhs: InternalValue<'ll>,
        rhs: InternalValue<'ll>,
        cmp_eq: bool,
    ) -> InternalValue<'ll> {
        let struct_value1 = lhs.as_basic_value().into_struct_value();
        let struct_value2 = rhs.as_basic_value().into_struct_value();

        let tag1 = self.extract_enum_idx(struct_value1);
        let tag2 = self.extract_enum_idx(struct_value2);

        let tag_concrete_type = CIRTy::PlainType(PlainType::UInt32);
        let tag_cmp_result = if cmp_eq {
            self.emit_cmp_eq(
                InternalValue::new(
                    tag_concrete_type.clone(),
                    InternalValueKind::RValue(tag1.as_basic_value_enum()),
                ),
                InternalValue::new(tag_concrete_type, InternalValueKind::RValue(tag2.as_basic_value_enum())),
            )
        } else {
            self.emit_cmp_neq(
                InternalValue::new(
                    tag_concrete_type.clone(),
                    InternalValueKind::RValue(tag1.as_basic_value_enum()),
                ),
                InternalValue::new(tag_concrete_type, InternalValueKind::RValue(tag2.as_basic_value_enum())),
            )
        };

        let current_func = self.cur_fn.unwrap();
        let payload_block = self.llvmctx.append_basic_block(current_func, "enum_cmp.payload");
        let exit_block = self.llvmctx.append_basic_block(current_func, "enum_cmp.exit");

        let entry_block = self.blockreg.cur_block.unwrap();

        self.llvmbuilder
            .build_conditional_branch(
                tag_cmp_result.as_basic_value().into_int_value(),
                payload_block,
                exit_block,
            )
            .unwrap();

        self.emit_block(payload_block);

        let payload1 = self.extract_enum_payload(struct_value1);
        let payload2 = self.extract_enum_payload(struct_value2);

        let memcmp_result = self.intrinsic_array_memcmp(payload1, payload2);

        let i32_zero = self.llvmctx.i32_type().const_zero();
        let payload_eq = self
            .llvmbuilder
            .build_int_compare(inkwell::IntPredicate::EQ, memcmp_result, i32_zero, "payload_eq")
            .unwrap();

        self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();

        let payload_cmp_block = self.blockreg.cur_block.unwrap();

        self.emit_block(exit_block);

        let phi = self
            .llvmbuilder
            .build_phi(self.llvmctx.bool_type(), "enum_cmp_phi")
            .unwrap();

        phi.add_incoming(&[(&self.llvmctx.bool_type().const_zero(), entry_block)]);
        phi.add_incoming(&[(&payload_eq, payload_cmp_block)]);

        InternalValue::new(
            CIRTy::PlainType(PlainType::Bool),
            InternalValueKind::RValue(phi.as_basic_value()),
        )
    }

    fn emit_struct_init(&mut self, struct_init: &CIRStructInitExpr) -> InternalValue<'ll> {
        let field_types: Vec<BasicTypeEnum<'ll>> = struct_init
            .ty
            .fields
            .iter()
            .map(|ty| self.emit_ty(ty.clone()).try_into().unwrap())
            .collect();

        let struct_type = self.llvmctx.struct_type(&field_types, struct_init.ty.is_packed);

        let mut all_const = true;

        let values: Vec<InternalValue<'ll>> = struct_init
            .fields
            .iter()
            .enumerate()
            .map(|(idx, expr)| {
                let lvalue = self.emit_expr(expr);
                let mut rvalue = self.load_rvalue(lvalue);

                let target_type = struct_init.ty.fields.get(idx).unwrap();

                if !self.llvmbuilder.get_insert_block().is_none() {
                    rvalue = self.emit_implicit_cast(target_type, rvalue);
                }

                if !is_basic_value_constant(rvalue.as_basic_value()) {
                    all_const = false;
                }
                rvalue
            })
            .collect();

        let mut struct_value: StructValue<'ll>;

        if self.must_init_via_memcpy(&struct_init.ty.fields) {
            let field_values = values
                .iter()
                .cloned()
                .zip(struct_init.ty.fields.clone())
                .collect::<Vec<_>>();
            struct_value = self.emit_struct_init_via_memcpy(struct_type, &field_values);
        } else {
            if all_const {
                let field_values = values.iter().map(|v| v.as_basic_value()).collect::<Vec<_>>();
                struct_value = struct_type.const_named_struct(&field_values);
            } else {
                struct_value = struct_type.get_undef();

                values.iter().enumerate().for_each(|(index, rvalue)| {
                    struct_value = self
                        .llvmbuilder
                        .build_insert_value(
                            struct_value,
                            rvalue.as_basic_value(),
                            index.try_into().unwrap(),
                            "insert",
                        )
                        .unwrap()
                        .into_struct_value();
                });
            }
        }

        InternalValue::new(
            CIRTy::Struct(struct_init.ty.clone()),
            InternalValueKind::RValue(struct_value.into()),
        )
    }

    fn emit_monomorph_func_instance_call(
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

    fn emit_interface_method_call(&mut self, interface_method_call: &CIRInterfaceMethodCall) -> InternalValue<'ll> {
        let dynamic_value = {
            let lvalue = self.emit_expr(&interface_method_call.operand);
            let rvalue = self.load_rvalue(lvalue);
            rvalue.as_basic_value().into_struct_value()
        };

        let data_ptr = self
            .llvmbuilder
            .build_extract_value(dynamic_value, 0, "extract_data")
            .unwrap()
            .into_pointer_value();

        let vtable_ptr = self
            .llvmbuilder
            .build_extract_value(dynamic_value, 1, "extract_vtable")
            .unwrap()
            .into_pointer_value();

        let ptr_type = self.llvmctx.ptr_type(AddressSpace::default());

        let method_ptr = unsafe {
            let offset = self
                .llvmctx
                .i64_type()
                .const_int(interface_method_call.method_idx as u64, false);

            let method_ptr = self
                .llvmbuilder
                .build_gep(ptr_type, vtable_ptr, &[offset], "method_ptr")
                .unwrap();

            method_ptr
        };

        let vtable_function_pointer = self
            .llvmbuilder
            .build_load(ptr_type, method_ptr, "load")
            .unwrap()
            .into_pointer_value();

        let vtable_function_type = self.emit_func_ty(interface_method_call.func_type.clone());

        let mut args = self.emit_func_args(&interface_method_call.args, &interface_method_call.func_type.clone());

        args.insert(0, data_ptr.as_basic_value_enum().into());

        let call_site = self
            .llvmbuilder
            .build_indirect_call(vtable_function_type, vtable_function_pointer, &args, "indirect_call")
            .unwrap();

        if let Some(basic_value) = call_site.try_as_basic_value().basic() {
            InternalValue::new(
                interface_method_call.ret_ty.clone(),
                InternalValueKind::RValue(basic_value),
            )
        } else {
            self.emit_null(interface_method_call.ret_ty.clone())
        }
    }

    fn emit_func_call(&mut self, func_call: &CIRFuncCall) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&func_call.operand);
        let rvalue = self.load_rvalue(lvalue);

        // check if it's a direct or indirect call
        if let Some(fn_value) = rvalue.as_func() {
            self.emit_direct_call(
                &rvalue.ty.as_func().unwrap(),
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

    fn emit_direct_call(
        &mut self,
        fn_ty: &CIRFuncTy,
        args: &Vec<CIRExpr>,
        ret_ty: &CIRTy,
        fn_value: &FunctionValue<'ll>,
    ) -> InternalValue<'ll> {
        let abi_func_info = self.target.target_abi.classify_func(fn_ty).unwrap();
        let args = self.emit_func_args(args, fn_ty);

        self.emit_func_call_attributes(&abi_func_info, FuncCallKind::Direct(*fn_value));

        let call_site = self.llvmbuilder.build_call(*fn_value, &args, "call").unwrap();

        if let Some(basic_value) = call_site.try_as_basic_value().basic() {
            InternalValue::new(ret_ty.clone(), InternalValueKind::RValue(basic_value))
        } else {
            self.emit_null(ret_ty.clone())
        }
    }

    fn emit_indirect_call(&mut self, func_call: &CIRFuncCall) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&func_call.operand);
        let rvalue = self.load_rvalue(lvalue);

        let cir_fn_ty = rvalue.ty.as_func().unwrap();
        let fn_ty = self.emit_func_ty(cir_fn_ty.clone());

        let abi_func_info = self.target.target_abi.classify_func(&cir_fn_ty).unwrap();
        let args = self.emit_func_args(&func_call.args, &cir_fn_ty);

        let fn_ptr = rvalue.as_basic_value().into_pointer_value();

        let call_site = self
            .llvmbuilder
            .build_indirect_call(fn_ty, fn_ptr, &args, "indirect_call")
            .unwrap();

        self.emit_func_call_attributes(&abi_func_info, FuncCallKind::Indirect(call_site));

        if let Some(basic_value) = call_site.try_as_basic_value().basic() {
            InternalValue::new(func_call.ret_ty.clone(), InternalValueKind::RValue(basic_value))
        } else {
            self.emit_null(func_call.ret_ty.clone())
        }
    }

    fn emit_load(&mut self, value_ref: &CIRValue) -> InternalValue<'ll> {
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
                let mut func_decl = *func_decl.clone();

                func_decl.modifiers = FuncModifiers {
                    linkage: Some(Linkage::Extern(None)),
                    ..func_decl.modifiers
                };

                let fn_value = self.emit_func_decl(&func_decl);
                let cir_fn_ty = cir_func_decl_as_func_ty(&func_decl);
                InternalValue::new(CIRTy::FuncType(cir_fn_ty), InternalValueKind::FuncValue(fn_value))
            }
            CIRValueKind::GlobalVar(global_var) => {
                let mut global_var = *global_var.clone();

                global_var.modifiers = GlobalVarModifiers {
                    linkage: Some(Linkage::Extern(None)),
                    ..global_var.modifiers
                };

                let global_value = self.emit_global_var(&global_var);

                InternalValue::new(
                    global_var.ty.clone(),
                    InternalValueKind::LValue(global_value.as_pointer_value()),
                )
            }
            CIRValueKind::LocalVariable => unreachable!(),
        }
    }

    fn emit_literal(&self, lit: &CIRLiteral) -> InternalValue<'ll> {
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

    fn emit_null(&self, ty: CIRTy) -> InternalValue<'ll> {
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
        global_str.set_linkage(inkwell::module::Linkage::Private);
        global_str.set_alignment(1);
        drop(llvmmodule);

        global_str.as_pointer_value().into()
    }

    fn emit_bytestring(&self, value: String) -> BasicValueEnum<'ll> {
        self.llvmctx.const_string(value.as_bytes(), true).into()
    }
}
