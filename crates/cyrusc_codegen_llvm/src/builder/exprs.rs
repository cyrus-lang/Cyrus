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
        builder::CodeGenIRBuilder,
        funcs::FuncCallKind,
        irreg::LocalIRValue,
        values::{InternalValue, InternalValueKind},
    },
    llvm::{abi::abi_type::abi_type_to_llvm_type, constness::is_basic_value_constant, debug_info::set_debug_location},
};
use cyrusc_ast::operators::{InfixOperator, PrefixOperator, UnaryOperator};
use cyrusc_internal::{
    abi::{
        args::ABIFunctionInfo,
        layout::{ABIFieldOffsetInfo, ABITypeLayout, type_layout},
        types::ABIType,
    },
    cir::{
        cir::*,
        types::{CIRArrayType, CIREnumType, CIRFuncType, CIRStructType, CIRTupleType, CIRType, CIRUnionType},
    },
};
use cyrusc_typed_ast::types::PlainType;
use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
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
            CIRExprKind::Infix(infix_expr) => self.emit_infix_expr(infix_expr),
            CIRExprKind::Unary(unary_expr) => self.emit_unary_expr(unary_expr),
            CIRExprKind::SizeOf(sizeof_expr) => self.emit_sizeof(sizeof_expr),
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
        };

        unsafe {
            set_debug_location(
                &self.dctx,
                self.llvmctx,
                self.llvmbuilder,
                expr.loc.line.try_into().unwrap(),
                expr.loc.column.try_into().unwrap(),
            )
        };

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
            // Value is not addressable → allocate temp
            let temp = self
                .llvmbuilder
                .build_alloca(data_basic_value.get_type(), "dyn.tmp")
                .unwrap();

            self.llvmbuilder.build_store(temp, data_basic_value).unwrap();

            temp
        };

        let vtable_ir_value = self.lookup_local_ir_value(dynamic.vtable_irv_id).unwrap();

        let vtable_global = vtable_ir_value.as_global().unwrap();
        let vtable_ptr = vtable_global.as_pointer_value();

        // construct pointers struct { data_ptr, vtable_ptr }
        let dynamic_struct_type = self.emit_dynamic_ty(); // { ptr, ptr }

        let mut dynamic_value = dynamic_struct_type.get_undef();

        dynamic_value = self
            .llvmbuilder
            .build_insert_value(dynamic_value, data_ptr.as_basic_value_enum(), 0, "dyn.insert.data")
            .unwrap()
            .into_struct_value();

        dynamic_value = self
            .llvmbuilder
            .build_insert_value(dynamic_value, vtable_ptr.as_basic_value_enum(), 1, "dyn.insert.vtable")
            .unwrap()
            .into_struct_value();

        InternalValue::new(
            self.cir_dynamic_ty(dynamic.data_expr.ty.clone(), dynamic.loc),
            InternalValueKind::RValue(dynamic_value.as_basic_value_enum()),
        )
    }

    pub(crate) fn emit_array_index_on_pointer(
        &mut self,
        lvalue: PointerValue<'ll>,
        index: InternalValue<'ll>,
        cir_elm_ty: CIRType,
    ) -> InternalValue<'ll> {
        let element_type: BasicTypeEnum<'ll> = self.emit_ty(cir_elm_ty.clone()).try_into().unwrap();
        let index_int = index.as_basic_value().into_int_value();

        let element_ptr: PointerValue<'ll> = unsafe {
            self.llvmbuilder
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
        let ptr = self.llvmbuilder.build_alloca(array_value.get_type(), "temp").unwrap();
        self.llvmbuilder
            .build_store(ptr, array_value.as_basic_value_enum())
            .unwrap();
        ptr
    }

    fn emit_assign(&mut self, assign: &CIRAssignExpr) -> InternalValue<'ll> {
        let lhs_lvalue = self.emit_lvalue_address(&assign.lhs);

        let rhs_lvalue = self.emit_expr(&assign.rhs, &Some(assign.lhs.ty.clone()));
        let rhs_value = self.load_rvalue(rhs_lvalue);

        self.llvmbuilder
            .build_store(
                lhs_lvalue.as_basic_value().into_pointer_value(),
                rhs_value.as_basic_value(),
            )
            .unwrap();

        rhs_value
    }

    pub(crate) fn emit_cast_func_arg(
        &self,
        value: BasicValueEnum<'ll>,
        from_cir_type: &CIRType,
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

        if let CIRType::Enum(enum_type) = from_cir_type {
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
                .build_ptr_to_int(value.into_pointer_value(), to_int, "ptr_to_int")
                .unwrap()
                .into(),
            (BasicTypeEnum::IntType(_), BasicTypeEnum::PointerType(to_ptr)) => self
                .llvmbuilder
                .build_int_to_ptr(value.into_int_value(), to_ptr, "int_to_ptr")
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
            _ => {
                let from_size = from_type.size_of();
                let to_size = target_basic_type.size_of();

                if from_size == to_size {
                    self.llvmbuilder
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
                            self.llvmbuilder
                                .build_int_z_extend(basic_value.into_int_value(), int_type, "bool_zext")
                                .unwrap(),
                        )
                    } else {
                        // int -> int
                        AnyValueEnum::IntValue(
                            self.llvmbuilder
                                .build_int_cast(basic_value.into_int_value(), int_type, "cast")
                                .unwrap(),
                        )
                    }
                } else if basic_value.is_pointer_value() {
                    // ptr -> int
                    let ptr_width = self.llvmtm.get_target_data().get_pointer_byte_size(None) * 8;

                    if int_type.get_bit_width() < ptr_width {
                        let ptr_int = self.llvmctx.custom_width_int_type(ptr_width);
                        let tmp = self
                            .llvmbuilder
                            .build_ptr_to_int(basic_value.into_pointer_value(), ptr_int, "ptr_to_int")
                            .unwrap();
                        AnyValueEnum::IntValue(self.llvmbuilder.build_int_truncate(tmp, int_type, "ptr_trunc").unwrap())
                    } else {
                        AnyValueEnum::IntValue(
                            self.llvmbuilder
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
                    basic_value.into()
                }
            }
            AnyTypeEnum::PointerType(ptr_type) => {
                if basic_value.is_pointer_value() {
                    // ptr -> ptr
                    AnyValueEnum::PointerValue(
                        self.llvmbuilder
                            .build_pointer_cast(basic_value.into_pointer_value(), ptr_type, "cast")
                            .unwrap(),
                    )
                } else if basic_value.is_int_value() {
                    let is_signed = value.ty.is_signed_integer();
                    basic_value = self.widen_int_arg(value, is_signed).as_basic_value();

                    // int -> ptr
                    AnyValueEnum::PointerValue(
                        self.llvmbuilder
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
        let ty = self.emit_ty(target_type.clone());
        let casted = self.emit_cast(ty, rvalue);
        InternalValue::new(
            target_type.clone(),
            InternalValueKind::RValue(casted.try_into().unwrap()),
        )
    }

    fn emit_array(&mut self, array: &CIRArrayExpr) -> InternalValue<'ll> {
        let cir_array_type = array.ty.as_array().unwrap();
        let element_ty = cir_array_type.element_type.clone();

        let array_type: ArrayType<'ll> = self.emit_array_type(cir_array_type.clone()).try_into().unwrap();

        let required_len = array.elements.len();
        let mut elements = Vec::with_capacity(required_len);
        let mut all_const = true;

        for expr in &array.elements {
            let lvalue = self.emit_expr(expr, &Some(*cir_array_type.element_type.clone()));
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
        let element_basic_ty: BasicTypeEnum<'ll> = self.emit_ty(*element_ty.clone()).try_into().unwrap();

        while elements.len() < array_type.len() as usize {
            elements.push(element_basic_ty.const_zero());
            all_const = false;
        }

        let array_value = if all_const {
            let mut val = array_type.get_undef();
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
            let mut value = array_type.get_undef();
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
        let operand = self.emit_expr(&addr_of.operand, &None);
        InternalValue::new(
            CIRType::Pointer(Box::new(operand.ty.clone())),
            InternalValueKind::RValue(operand.as_basic_value()),
        )
    }

    pub(crate) fn emit_decay_array_to_pointer(&self, array_lvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        let array_ptr = array_lvalue.as_basic_value().into_pointer_value();
        let element_type = array_lvalue.ty.as_array().unwrap().element_type;

        let zero = self.llvmctx.i32_type().const_int(0, false);
        let first_element = unsafe {
            self.llvmbuilder
                .build_in_bounds_gep(
                    self.emit_ty(array_lvalue.ty.clone()).into_array_type(),
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

                    let layout = type_layout(&self.target.info, &struct_type);
                    let llvm_struct_type = self.emit_ty(struct_type).into_struct_type();

                    let index = layout.lookup_field_index(*index).unwrap();

                    let field_ptr = self
                        .llvmbuilder
                        .build_struct_gep(llvm_struct_type, struct_ptr_value, index, "field_ptr")
                        .unwrap();

                    InternalValue::new(field_type.clone(), InternalValueKind::LValue(field_ptr))
                }
                CIRFieldAccessKind::Union { field_type } => {
                    let mut value = self.emit_lvalue_address(&field_access.operand);
                    value.ty = field_type.clone();
                    value
                },
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
                let inner_ty = rvalue.ty.pointer_inner().unwrap();

                let llvm_ty: BasicTypeEnum<'ll> = self.emit_ty(inner_ty.clone()).try_into().unwrap();
                let loaded_value = self.llvmbuilder.build_load(llvm_ty, ptr, "deref").unwrap();
                InternalValue::new(inner_ty.clone(), InternalValueKind::RValue(loaded_value.into()))
            }
            DerefMode::Store => self.emit_lvalue_address(&CIRExpr {
                kind: CIRExprKind::Deref(deref.clone()),
                ty: deref.operand.ty.pointer_inner().cloned().unwrap(),
                loc: deref.operand.loc,
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
        let lvalue_ptr = lvalue.as_basic_value().into_pointer_value();

        let rvalue = self.load_rvalue(lvalue);

        let ty = rvalue.ty.clone();
        let is_pointer = ty.is_pointer();

        let unit_int_value = {
            if is_pointer {
                self.llvmctx.i64_type().const_int(1, false)
            } else {
                let is_signed = ty.is_signed_integer();
                let basic_type: BasicTypeEnum<'ll> = self.emit_ty(ty.clone()).try_into().unwrap();
                basic_type.into_int_type().const_int(1, is_signed)
            }
        };

        match unary_expr.op {
            UnaryOperator::PreIncrement => {
                let new_value = if is_pointer {
                    // ptr + 1
                    let ptr = rvalue.as_basic_value().into_pointer_value();
                    let value = self.emit_pointer_add(ptr, unit_int_value, ty.clone());
                    value
                } else {
                    // normal integer/float increment
                    let unit_value = InternalValue::new(
                        ty.clone(),
                        InternalValueKind::RValue(BasicValueEnum::IntValue(unit_int_value)),
                    );

                    self.emit_add(rvalue.clone(), unit_value)
                };

                self.llvmbuilder
                    .build_store(lvalue_ptr, new_value.as_basic_value())
                    .unwrap();

                new_value
            }
            UnaryOperator::PreDecrement => {
                let new_value = if is_pointer {
                    // ptr - 1
                    let ptr = rvalue.as_basic_value().into_pointer_value();
                    let value = self.emit_pointer_sub(ptr, unit_int_value, ty.clone());
                    value
                } else {
                    let unit_value = InternalValue::new(
                        ty.clone(),
                        InternalValueKind::RValue(BasicValueEnum::IntValue(unit_int_value)),
                    );

                    self.emit_sub(rvalue.clone(), unit_value)
                };

                self.llvmbuilder
                    .build_store(lvalue_ptr, new_value.as_basic_value())
                    .unwrap();

                new_value
            }
            UnaryOperator::PostIncrement => {
                let old_value = rvalue.clone();
                let new_value = if is_pointer {
                    // ptr + 1
                    let ptr = rvalue.as_basic_value().into_pointer_value();
                    self.emit_pointer_add(ptr, unit_int_value, ty.clone())
                } else {
                    let unit_value = InternalValue::new(
                        ty.clone(),
                        InternalValueKind::RValue(BasicValueEnum::IntValue(unit_int_value)),
                    );
                    self.emit_add(rvalue, unit_value)
                };

                self.llvmbuilder
                    .build_store(lvalue_ptr, new_value.as_basic_value())
                    .unwrap();

                old_value
            }
            UnaryOperator::PostDecrement => {
                let old_value = rvalue.clone();
                let new_value = if is_pointer {
                    let ptr = rvalue.as_basic_value().into_pointer_value();
                    self.emit_pointer_sub(ptr, unit_int_value, ty.clone())
                } else {
                    let unit_value = InternalValue::new(
                        ty.clone(),
                        InternalValueKind::RValue(BasicValueEnum::IntValue(unit_int_value)),
                    );
                    self.emit_sub(rvalue, unit_value)
                };

                self.llvmbuilder
                    .build_store(lvalue_ptr, new_value.as_basic_value())
                    .unwrap();

                old_value
            }
        }
    }

    fn emit_infix_expr(&mut self, infix_expr: &CIRInfixExpr) -> InternalValue<'ll> {
        let lhs_lvalue = self.emit_expr(&infix_expr.lhs, &None);
        let rhs_lvalue = self.emit_expr(&infix_expr.rhs, &None);

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
                    CIRType::Plain(PlainType::Bool),
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
        ty: CIRType,
    ) -> InternalValue<'ll> {
        // cond: lhs == null
        let is_null = self.llvmbuilder.build_is_null(lhs, "lhs_is_null").unwrap();

        let selected = self
            .llvmbuilder
            .build_select(is_null, rhs, lhs, "null_coalesce")
            .unwrap()
            .into_pointer_value();

        InternalValue::new(
            CIRType::Pointer(Box::new(ty)),
            InternalValueKind::RValue(selected.into()),
        )
    }

    fn emit_logical_and(&self, lhs_rvalue: InternalValue<'ll>, rhs_rvalue: InternalValue<'ll>) -> InternalValue<'ll> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvmbuilder.build_and(lhs, rhs, "land").unwrap();
                InternalValue::new(
                    CIRType::Plain(PlainType::Bool),
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
                let and_value = self.llvmbuilder.build_and(lhs, rhs, "xor").unwrap();
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
                let and_value = self.llvmbuilder.build_or(lhs, rhs, "or").unwrap();
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
                let not_rhs = self.llvmbuilder.build_xor(rhs, all_ones, "not_rhs").unwrap();

                // lhs AND (~rhs)
                let and_not_value = self.llvmbuilder.build_and(lhs, not_rhs, "and_not").unwrap();

                InternalValue::new(
                    CIRType::Plain(PlainType::Int), // result is integer, not Bool
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

                let shift_value = self.llvmbuilder.build_right_shift(lhs, rhs, signed, "lshift").unwrap();
                InternalValue::new(
                    CIRType::Plain(PlainType::Bool),
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

    fn emit_pointer_add(
        &self,
        ptr: PointerValue<'ll>,
        index: IntValue<'ll>,
        result_type: CIRType,
    ) -> InternalValue<'ll> {
        let pointee_type: BasicTypeEnum<'ll> = self
            .emit_ty(result_type.pointer_inner().unwrap().clone())
            .try_into()
            .unwrap();

        let i64_type = self.llvmctx.i64_type();
        let gep_index = if index.get_type() == i64_type {
            index
        } else {
            self.llvmbuilder.build_int_cast(index, i64_type, "index.cast").unwrap()
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

    fn emit_pointer_sub(
        &self,
        ptr: PointerValue<'ll>,
        index: IntValue<'ll>,
        result_type: CIRType,
    ) -> InternalValue<'ll> {
        let pointee_type: BasicTypeEnum<'ll> = self
            .emit_ty(result_type.pointer_inner().unwrap().clone())
            .try_into()
            .unwrap();

        // negate the index for subtraction
        let i64_type = self.llvmctx.i64_type();
        let index_i64 = if index.get_type() == i64_type {
            index
        } else {
            self.llvmbuilder.build_int_cast(index, i64_type, "i.cast").unwrap()
        };

        let neg_index = self.llvmbuilder.build_int_neg(index_i64, "i.neg").unwrap();

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

        let result_type = CIRType::Plain(PlainType::ISize);
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
                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
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
            .llvmbuilder
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
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
                    .unwrap();
                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
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
                    .llvmbuilder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
                    .unwrap();
                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
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
                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_float_compare(FloatPredicate::ONE, lhs, rhs, "neq")
                    .unwrap();
                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "neq")
                    .unwrap();
                InternalValue::new(CIRType::Plain(PlainType::Bool), InternalValueKind::RValue(cmp.into()))
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
        let lvalue = self.emit_expr(&prefix_expr.operand, &None);
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

    fn emit_field_access(&mut self, field_access: &CIRFieldAccessExpr) -> InternalValue<'ll> {
        match &field_access.kind {
            CIRFieldAccessKind::Struct { .. } => self.emit_struct_field_access(field_access),
            CIRFieldAccessKind::Union { .. } => self.emit_union_field_access(field_access),
        }
    }

    fn emit_union_field_access(&mut self, field_access: &CIRFieldAccessExpr) -> InternalValue<'ll> {
        let operand = self.emit_lvalue_address(&field_access.operand);

        let union_type: BasicTypeEnum<'ll> = self.emit_ty(field_access.operand.ty.clone()).try_into().unwrap();

        let union_ptr = match operand.kind {
            InternalValueKind::LValue(ptr) => ptr,

            // RValue → spill to stack
            InternalValueKind::RValue(basic_val) => {
                let temp = self.llvmbuilder.build_alloca(union_type, "union.temp").unwrap();
                self.llvmbuilder.build_store(temp, basic_val).unwrap();
                temp
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
        let cir_struct_ty = if let Some(inner) = field_access.operand.ty.pointer_inner() {
            inner.clone().as_struct().unwrap()
        } else {
            field_access.operand.ty.clone().as_struct().unwrap()
        };

        let layout = type_layout(&self.target.info, &CIRType::Struct(cir_struct_ty.clone()));

        let llvm_field_index = layout
            .lookup_field_index(field_index)
            .expect("layout must contain field");

        let llvm_struct_type = self.emit_struct_type(cir_struct_ty);

        match operand.kind {
            InternalValueKind::LValue(ptr_value) => {
                let field_ptr = self
                    .llvmbuilder
                    .build_struct_gep(llvm_struct_type, ptr_value, llvm_field_index, "field_gep")
                    .unwrap();

                InternalValue::new(field_type, InternalValueKind::LValue(field_ptr))
            }
            InternalValueKind::RValue(struct_val) => {
                let struct_value = struct_val.into_struct_value();

                let field_value = self
                    .llvmbuilder
                    .build_extract_value(struct_value, llvm_field_index, "field_extract")
                    .unwrap();

                InternalValue::new(field_type, InternalValueKind::RValue(field_value))
            }
            _ => unreachable!(),
        }
    }

    fn emit_tuple_access(&mut self, tuple_access: &CIRTupleAccessExpr) -> InternalValue<'ll> {
        let operand_value = self.emit_expr(&tuple_access.operand, &None);

        let cir_tuple_type = operand_value.ty.as_tuple().unwrap();

        let layout = type_layout(&self.target.info, &CIRType::Tuple(cir_tuple_type.clone()));
        let field_index = layout.lookup_field_index(tuple_access.index).unwrap();

        let llvm_tuple_type = self.emit_tuple_ty(cir_tuple_type.clone());
        let cir_field_type = &cir_tuple_type.elements[tuple_access.index];

        match operand_value.kind {
            InternalValueKind::LValue(addr) => {
                let field_addr = self
                    .llvmbuilder
                    .build_struct_gep(llvm_tuple_type, addr, field_index, "tuple_gep")
                    .unwrap();

                InternalValue::new(cir_field_type.clone(), InternalValueKind::LValue(field_addr))
            }
            InternalValueKind::RValue(val) => {
                let struct_value = val.into_struct_value();

                let field_val = self
                    .llvmbuilder
                    .build_extract_value(struct_value, field_index, "tuple_extract")
                    .unwrap();

                InternalValue::new(cir_field_type.clone(), InternalValueKind::RValue(field_val))
            }
            _ => unreachable!(),
        }
    }

    fn emit_tuple(&mut self, tuple: &CIRTupleExpr) -> InternalValue<'ll> {
        let element_types = tuple.elements.iter().map(|elm| elm.ty.clone()).collect();

        let fields = tuple.elements.iter().map(|expr| expr.ty.clone()).collect();

        let fields_info = tuple
            .elements
            .iter()
            .enumerate()
            .map(|(i, _)| (i.to_string(), tuple.loc))
            .collect();

        let struct_value = self
            .emit_struct_init(&CIRStructInitExpr {
                ty: CIRStructType {
                    name: None,
                    fields,
                    fields_info,
                    repr_attr: None,
                    align: None,
                    loc: tuple.loc,
                },
                fields: tuple.elements.clone(),
            })
            .as_basic_value()
            .into_struct_value();

        InternalValue::new(
            CIRType::Tuple(CIRTupleType {
                elements: element_types,
                loc: tuple.loc,
            }),
            InternalValueKind::RValue(struct_value.into()),
        )
    }

    fn emit_repr_c_enum_init(
        &mut self,
        enum_init_expr: &CIREnumInitExpr,
        enum_type: &CIREnumType,
    ) -> InternalValue<'ll> {
        let cir_tag_type = enum_type.tag_type_or_infer_or_default();
        let tag_type = self.emit_ty(*cir_tag_type.clone()).into_int_type();
        let tag_value = tag_type.const_int(enum_init_expr.tag.try_into().unwrap(), cir_tag_type.is_signed_integer());

        InternalValue::new(
            CIRType::Enum(enum_type.clone()),
            InternalValueKind::RValue(tag_value.as_basic_value_enum()),
        )
    }

    fn emit_enum_init(&mut self, enum_init_expr: &CIREnumInitExpr) -> InternalValue<'ll> {
        let enum_type = &enum_init_expr.enum_type;

        // handle c-compatible enum init
        if enum_type.is_scalar_optimizable() {
            return self.emit_repr_c_enum_init(enum_init_expr, enum_type);
        }

        let enum_struct_ty = self.emit_enum_type(enum_type.clone()).into_struct_type();
        let (payload_ty, _) = self.emit_enum_buffer_payload_ty(enum_type);

        let mut enum_value = enum_struct_ty.get_undef();

        let cir_tag_type = enum_type.tag_type_or_infer_or_default();
        let tag_type = self.emit_ty(*cir_tag_type.clone()).into_int_type();
        let tag_value = tag_type.const_int(enum_init_expr.tag as u64, false);

        enum_value = self
            .llvmbuilder
            .build_insert_value(enum_value, tag_value, 0, "enum.set_tag")
            .unwrap()
            .into_struct_value();

        match &enum_init_expr.variant {
            CIREnumInitVariant::Unit => {
                let zero_payload = payload_ty.const_zero();
                enum_value = self
                    .llvmbuilder
                    .build_insert_value(enum_value, zero_payload, 1, "enum.zero_payload")
                    .unwrap()
                    .into_struct_value();
            }
            CIREnumInitVariant::Valued(expr) => {
                let lvalue = self.emit_expr(expr, &None);
                let rvalue = self.load_rvalue(lvalue);

                let copied_payload = self.intrinsic_copy_payload_to_buffer(rvalue.as_basic_value(), payload_ty);

                enum_value = self
                    .llvmbuilder
                    .build_insert_value(enum_value, copied_payload, 1, "enum.set_payload")
                    .unwrap()
                    .into_struct_value();
            }
            CIREnumInitVariant::Payload(field_exprs) => {
                let field_types: Vec<BasicTypeEnum<'ll>> = field_exprs
                    .iter()
                    .map(|fld| self.emit_ty(fld.ty.clone()).try_into().unwrap())
                    .collect();

                let payload_struct_type = self.llvmctx.struct_type(&field_types, false);

                let mut payload_value = payload_struct_type.get_undef();

                for (i, field_expr) in field_exprs.iter().enumerate() {
                    let lvalue = self.emit_expr(&field_expr, &None);
                    let rvalue = self.load_rvalue(lvalue);

                    payload_value = self
                        .llvmbuilder
                        .build_insert_value(payload_value, rvalue.as_basic_value(), i as u32, "payload.insert")
                        .unwrap()
                        .into_struct_value();
                }

                let copied_payload =
                    self.intrinsic_copy_payload_to_buffer(payload_value.as_basic_value_enum(), payload_ty);

                enum_value = self
                    .llvmbuilder
                    .build_insert_value(enum_value, copied_payload, 1, "enum.set_payload")
                    .unwrap()
                    .into_struct_value();
            }
        }

        InternalValue::new(
            CIRType::Enum(enum_init_expr.enum_type.clone()),
            InternalValueKind::RValue(enum_value.as_basic_value_enum()),
        )
    }

    pub(crate) fn emit_union_init(&self, union_ty: &CIRUnionType, ptr: PointerValue<'ll>, rvalue: InternalValue<'ll>) {
        let llvm_union_type = self.emit_union_ty(union_ty.clone());

        // union-to-union copy
        if rvalue.ty.is_union() {
            self.intrinsic_optimized_memcpy(ptr, rvalue.as_basic_value());
            return;
        }

        let value = rvalue.as_basic_value();
        let ptr_type = self.llvmctx.ptr_type(AddressSpace::default());

        if llvm_union_type.is_struct_type() {
            // get pointer to storage field (largest field)
            let union_ptr = self
                .llvmbuilder
                .build_struct_gep(llvm_union_type.into_struct_type(), ptr, 0, "union.storage")
                .unwrap();

            let field_ptr = self
                .llvmbuilder
                .build_bit_cast(union_ptr, ptr_type, "union.field.ptr")
                .unwrap()
                .into_pointer_value();

            self.llvmbuilder.build_store(field_ptr, value).unwrap();
        } else {
            // union represented as largest field type (directly, without struct type as wrapper)
            let field_ptr = self
                .llvmbuilder
                .build_bit_cast(ptr, ptr_type, "union.field.ptr")
                .unwrap()
                .into_pointer_value();

            self.llvmbuilder.build_store(field_ptr, value).unwrap();
        }
    }

    fn emit_union_init_value(&mut self, union_init_expr: &CIRUnionInitExpr) -> InternalValue<'ll> {
        self.emit_expr(&union_init_expr.expr, &None)
    }

    // ANCHOR: Check extracting tag of repr-c enums.
    pub(crate) fn extract_enum_tag(&self, struct_value: StructValue<'ll>) -> IntValue<'ll> {
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

    // ANCHOR: Check comparing repr-c enums.
    fn emit_compare_enum_variants(
        &mut self,
        lhs: InternalValue<'ll>,
        rhs: InternalValue<'ll>,
        cmp_eq: bool,
    ) -> InternalValue<'ll> {
        let struct_value1 = lhs.as_basic_value().into_struct_value();
        let struct_value2 = rhs.as_basic_value().into_struct_value();

        let tag1 = self.extract_enum_tag(struct_value1);
        let tag2 = self.extract_enum_tag(struct_value2);

        let tag_concrete_type = CIRType::Plain(PlainType::UInt32);
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

        let current_func = self.cur_func.unwrap();
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
            CIRType::Plain(PlainType::Bool),
            InternalValueKind::RValue(phi.as_basic_value()),
        )
    }

    fn emit_struct_init(&mut self, struct_init: &CIRStructInitExpr) -> InternalValue<'ll> {
        let layout = type_layout(&self.target.info, &CIRType::Struct(struct_init.ty.clone()));
        let struct_type = self.emit_struct_type(struct_init.ty.clone());

        let mut all_const = true;
        let mut values: Vec<(Option<usize>, InternalValue<'ll>)> = Vec::new();

        for field_offset in &layout.field_offsets {
            match field_offset {
                ABIFieldOffsetInfo::Normal { original_index, .. } => {
                    let cir_field_type = &struct_init.ty.fields[*original_index];

                    let expr = &struct_init.fields[*original_index];
                    let lvalue = self.emit_expr(expr, &Some(cir_field_type.clone()));
                    let mut rvalue = self.load_rvalue(lvalue);

                    let field_original_index = field_offset.original_index().unwrap();
                    let target_type = struct_init.ty.fields.get(field_original_index).unwrap();

                    if !self.llvmbuilder.get_insert_block().is_none() {
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
                    let padding_array_value = self.llvmctx.i8_type().array_type(*size).const_zero();
                    values.push((
                        None,
                        InternalValue::new(
                            cir_array_type,
                            InternalValueKind::RValue(padding_array_value.as_basic_value_enum()),
                        ),
                    ));
                }
            }
        }

        let mut struct_value: StructValue<'ll>;

        if must_init_via_memcpy(&struct_init.ty.fields) {
            let field_values = values
                .iter()
                .filter_map(|(original_index, value)| {
                    original_index.map(|i| {
                        let field_ty = struct_init.ty.fields[i].clone();
                        ((Some(i), value.clone()), field_ty)
                    })
                })
                .collect::<Vec<_>>();

            let layout = type_layout(&self.target.info, &CIRType::Struct(struct_init.ty.clone()));
            struct_value = self.emit_struct_init_via_memcpy(&layout, struct_type, &field_values);
        } else {
            if all_const {
                let field_values = values
                    .iter()
                    .map(|(_, value)| value.as_basic_value())
                    .collect::<Vec<_>>();

                struct_value = struct_type.const_named_struct(&field_values);
            } else {
                struct_value = struct_type.get_undef();

                values.iter().enumerate().for_each(|(index, (_, rvalue))| {
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
            CIRType::Struct(struct_init.ty.clone()),
            InternalValueKind::RValue(struct_value.into()),
        )
    }

    fn emit_struct_init_via_memcpy(
        &self,
        layout: &ABITypeLayout,
        struct_type: StructType<'ll>,
        values: &Vec<((Option<usize>, InternalValue<'ll>), CIRType)>,
    ) -> StructValue<'ll> {
        let struct_ptr = self.llvmbuilder.build_alloca(struct_type, "struct.init").unwrap();

        for ((original_index, field_value), field_cir_ty) in values {
            if original_index.is_none() {
                // skip if padding
                continue;
            }

            let llvm_index = layout.lookup_field_index(original_index.unwrap()).unwrap();

            let field_ptr = self
                .llvmbuilder
                .build_struct_gep(struct_type, struct_ptr, llvm_index as u32, "struct.field")
                .unwrap();

            self.emit_store(field_ptr, field_value.clone(), field_cir_ty.clone());
        }

        self.llvmbuilder
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
            .llvmbuilder
            .build_extract_value(dyn_value, 0, "dyn.data")
            .unwrap()
            .into_pointer_value();

        let vtable_ptr = self
            .llvmbuilder
            .build_extract_value(dyn_value, 1, "dyn.vtable")
            .unwrap()
            .into_pointer_value();

        let ptr_type = self.llvmctx.ptr_type(AddressSpace::default());

        let method_gep = unsafe {
            let idx = self.llvmctx.i64_type().const_int(method_idx as u64, false);
            self.llvmbuilder
                .build_gep(ptr_type, vtable_ptr, &[idx], "vtable.method.gep")
                .unwrap()
        };

        let fn_ptr = self
            .llvmbuilder
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

        let llvm_fn_ty = self.emit_func_ty(func_type.clone());

        let call_site = self
            .llvmbuilder
            .build_indirect_call(llvm_fn_ty, fn_ptr, &llvm_args, "ifc.call")
            .unwrap();

        // Attach ABI call attributes
        self.emit_func_call_attributes(&abi_func_info, FuncCallKind::Indirect(call_site));

        if let Some(mut basic_value) = call_site.try_as_basic_value().basic() {
            let actual_return_type: BasicTypeEnum<'ll> = self.emit_ty(*func_type.ret_type.clone()).try_into().unwrap();

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

            let struct_type = fat_ptr_value.ty.as_struct().unwrap();
            let data_type = struct_type.fields.first().cloned().unwrap();

            debug_assert!(data_type.is_pointer()); // always `void*`

            // let llvm_struct_type = self.emit_struct_type(struct_type);
            let data_ptr = self
                .llvmbuilder
                .build_extract_value(AggregateValueEnum::StructValue(fat_ptr_struct_value), 0, "fat_ptr.data")
                .unwrap();

            let data_value = InternalValue::new(
                data_type.clone(),
                InternalValueKind::RValue(data_ptr.as_basic_value_enum()),
            );

            (data_value.clone(), data_value)
        } else {
            if self_meta.is_referenced {
                // always pass a pointer, regardless of ABI decision
                let lvalue = self.emit_lvalue_address(&self_meta.operand);
                let rvalue = lvalue.clone();

                (lvalue, rvalue)
            } else {
                let lvalue = self.emit_expr(&self_meta.operand, &None);
                let rvalue = self.load_rvalue(lvalue.clone());

                (lvalue, rvalue)
            }
        }
    }

    fn emit_direct_call(
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
        func_type: &CIRFuncType,
        ret_type: &CIRType,
        llvm_func_value: FunctionValue<'ll>,
        mut llvm_args: Vec<BasicMetadataValueEnum<'ll>>,
    ) -> InternalValue<'ll> {
        let mut sret_alloca: Option<PointerValue<'ll>> = None;

        if abi_func_info.ret_info.kind.is_indirect_sret() {
            let sret_type: BasicTypeEnum<'ll> = self.emit_ty(*func_type.ret_type.clone()).try_into().unwrap();

            let alloca = self.llvmbuilder.build_alloca(sret_type, "sret").unwrap();

            llvm_args.insert(0, alloca.into());
            sret_alloca = Some(alloca);
        }

        self.emit_func_call_attributes(&abi_func_info, FuncCallKind::Direct(llvm_func_value));

        let call_site = self
            .llvmbuilder
            .build_call(llvm_func_value, &llvm_args, "call")
            .unwrap();

        if let Some(ptr) = sret_alloca {
            InternalValue::new(ret_type.clone(), InternalValueKind::LValue(ptr.into()))
        } else if let Some(mut basic_value) = call_site.try_as_basic_value().basic() {
            // REVIEW: Optimization Required
            // coerce back from abi return type to actual return type
            let actual_return_type: BasicTypeEnum<'ll> = self.emit_ty(*func_type.ret_type.clone()).try_into().unwrap();

            basic_value = self.intrinsic_coerce_through_alloca(basic_value, actual_return_type, "coerce_ret");

            InternalValue::new(ret_type.clone(), InternalValueKind::RValue(basic_value))
        } else {
            self.emit_null(ret_type.clone())
        }
    }

    fn emit_indirect_call(&mut self, func_call: &CIRCall, operand: InternalValue<'ll>) -> InternalValue<'ll> {
        let cir_func_type = operand.ty.as_func().unwrap();
        let llvm_func_type = self.emit_func_ty(cir_func_type.clone());

        let abi_func_info = self.target.target_abi.classify_func(&cir_func_type).unwrap();

        let llvm_args = self.emit_func_args(
            &func_call.args,
            &abi_func_info.params_infos,
            &abi_func_info.params_types,
            &cir_func_type.params,
        );

        let fn_ptr = operand.as_basic_value().into_pointer_value();

        let call_site = self
            .llvmbuilder
            .build_indirect_call(llvm_func_type, fn_ptr, &llvm_args, "indirect_call")
            .unwrap();

        self.emit_func_call_attributes(&abi_func_info, FuncCallKind::Indirect(call_site));

        if let Some(mut basic_value) = call_site.try_as_basic_value().basic() {
            // REVIEW: Optimization Required
            // coerce back from abi return type to actual return type
            let actual_return_type: BasicTypeEnum<'ll> =
                self.emit_ty(*cir_func_type.ret_type.clone()).try_into().unwrap();
            basic_value = self.intrinsic_coerce_through_alloca(basic_value, actual_return_type, "coerce_ret");

            InternalValue::new(func_call.ret_type.clone(), InternalValueKind::RValue(basic_value))
        } else {
            self.emit_null(func_call.ret_type.clone())
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

    fn emit_null(&self, ty: CIRType) -> InternalValue<'ll> {
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

fn must_init_via_memcpy(fields: &Vec<CIRType>) -> bool {
    fields.iter().any(|ty| ty.is_union())
}
