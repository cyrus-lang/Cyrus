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

use crate::builder::{builder::CodeGenIRBuilder, values::InternalValue};
use cyrusc_internal::cir::{cir::CIRCall, types::CIRType};
use cyrusc_typed_ast::{
    builtins::{TypedBuiltinForm, TypedBuiltinKind, TypedBuiltinPhase, TypedBuiltinSpec},
    types::PlainType,
};
use inkwell::{
    AddressSpace,
    types::{ArrayType, BasicType, BasicTypeEnum, StructType},
    values::{ArrayValue, BasicValue, BasicValueEnum, IntValue, PointerValue, StructValue},
};

// These intrinsics published via builtin to user side.
impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_builtin_call(&mut self, call: &CIRCall, builtin_spec: &TypedBuiltinSpec) -> InternalValue<'ll> {
        debug_assert!(builtin_spec.phase == TypedBuiltinPhase::Codegen);
        debug_assert!(builtin_spec.form == TypedBuiltinForm::Expr);

        match builtin_spec.kind {
            TypedBuiltinKind::Memcpy => self.emit_intrinsic_memcpy(call),
            TypedBuiltinKind::Memset => self.emit_intrinsic_memset(call),

            _ => unreachable!("unknown builtin called"),
        }
    }

    fn emit_intrinsic_memcpy(&mut self, call: &CIRCall) -> InternalValue<'ll> {
        let cir_void_ptr = CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void)));
        let cir_int64 = CIRType::Plain(PlainType::Int64);

        // ptr
        let dest = {
            let lvalue = self.emit_expr(&call.args[0], &Some(cir_void_ptr.clone()));
            let rvalue = self.load_rvalue(lvalue);
            let casted = self.emit_implicit_cast(&cir_void_ptr, rvalue);
            casted.as_basic_value().into_pointer_value()
        };

        // ptr
        let src = {
            let lvalue = self.emit_expr(&call.args[1], &Some(cir_void_ptr.clone()));
            let rvalue = self.load_rvalue(lvalue);
            let casted = self.emit_implicit_cast(&cir_void_ptr, rvalue);
            casted.as_basic_value().into_pointer_value()
        };

        // i64
        let size = {
            let lvalue = self.emit_expr(&call.args[2], &Some(cir_int64.clone()));
            let rvalue = self.load_rvalue(lvalue);
            let casted = self.emit_implicit_cast(&cir_int64, rvalue);
            casted.as_basic_value().into_int_value()
        };

        let dest_align = self.target.info.pointer_align();
        let src_align = self.target.info.pointer_align();

        self.llvmbuilder
            .build_memcpy(dest, dest_align, src, src_align, size)
            .unwrap();

        self.emit_null(cir_void_ptr)
    }

    fn emit_intrinsic_memset(&mut self, call: &CIRCall) -> InternalValue<'ll> {
        let cir_void_ptr = CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void)));
        let cir_int8 = CIRType::Plain(PlainType::Int8);
        let cir_int64 = CIRType::Plain(PlainType::Int64);

        // ptr
        let dest = {
            let lvalue = self.emit_expr(&call.args[0], &Some(cir_void_ptr.clone()));
            let rvalue = self.load_rvalue(lvalue);
            let casted = self.emit_implicit_cast(&cir_void_ptr, rvalue);
            casted.as_basic_value().into_pointer_value()
        };

        // i8
        let value = {
            let lvalue = self.emit_expr(&call.args[1], &Some(cir_int8.clone()));
            let rvalue = self.load_rvalue(lvalue);
            let casted = self.emit_implicit_cast(&cir_int8, rvalue);
            casted.as_basic_value().into_int_value()
        };

        // i64
        let size = {
            let lvalue = self.emit_expr(&call.args[2], &Some(cir_int64.clone()));
            let rvalue = self.load_rvalue(lvalue);
            let casted = self.emit_implicit_cast(&cir_int64, rvalue);
            casted.as_basic_value().into_int_value()
        };

        let align = self.target.info.pointer_align();

        self.llvmbuilder.build_memset(dest, align, value, size).unwrap();

        self.emit_null(cir_void_ptr)
    }
}

// These intrinsics only used inside compiler (internally; no-publication).
impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn intrinsic_coerce_through_alloca(
        &self,
        value: BasicValueEnum<'ll>,
        dst_ty: BasicTypeEnum<'ll>,
        name: &str,
    ) -> BasicValueEnum<'ll> {
        let src_ty = value.get_type();

        // if source and destination types are the same, no coercion is needed
        if src_ty == dst_ty {
            return value;
        }

        let dst_alloca = self
            .llvmbuilder
            .build_alloca(dst_ty, &format!("{name}.dst.alloca"))
            .unwrap();

        let src_alloca = self
            .llvmbuilder
            .build_alloca(src_ty, &format!("{name}.src.alloca"))
            .unwrap();

        self.llvmbuilder.build_store(src_alloca, value).unwrap();

        let i8_ptr_type = self.llvmctx.ptr_type(inkwell::AddressSpace::default());

        let dst_ptr = self
            .llvmbuilder
            .build_bit_cast(dst_alloca, i8_ptr_type, &format!("{name}.dst.ptr"))
            .unwrap()
            .into_pointer_value();

        let src_ptr = self
            .llvmbuilder
            .build_bit_cast(src_alloca, i8_ptr_type, &format!("{name}.src.ptr"))
            .unwrap()
            .into_pointer_value();

        let size = dst_ty.size_of().unwrap();

        self.llvmbuilder.build_memcpy(dst_ptr, 1, src_ptr, 1, size).unwrap();

        self.llvmbuilder.build_load(dst_ty, dst_alloca, name).unwrap()
    }

    pub(crate) fn intrinsic_optimized_memcpy(&self, dest: PointerValue<'ll>, rvalue: BasicValueEnum<'ll>) {
        let ty = rvalue.get_type();

        // Fast path: direct store
        if ty.is_int_type()
            || ty.is_float_type()
            || ty.is_pointer_type()
            || ty.is_struct_type()
            || ty.is_array_type()
            || ty.is_vector_type()
        {
            self.llvmbuilder.build_store(dest, rvalue).unwrap();
            return;
        }

        // fallback to memcpy
        self.intrinsic_memcpy(dest, rvalue);
    }

    pub(crate) fn intrinsic_memcpy(&self, dest: PointerValue<'ll>, rvalue: BasicValueEnum<'ll>) {
        let target_data = self.llvmtm.get_target_data();
        let ty = rvalue.get_type();

        let size_in_bytes = target_data.get_store_size(&ty);
        let size_value = self.llvmctx.i64_type().const_int(size_in_bytes, false);

        let src_align = target_data.get_abi_alignment(&ty);
        let dest_align = target_data.get_abi_alignment(&ty);

        let src_ptr = if rvalue.is_const() {
            // use global value if rvalue is a constant
            let global = {
                let module = self.llvmmodule.borrow();
                module.add_global(ty, None, "__const.memcpy")
            };

            global.set_linkage(inkwell::module::Linkage::Private);
            global.set_constant(true);
            global.set_unnamed_address(inkwell::values::UnnamedAddress::Global);
            global.set_initializer(&rvalue);

            global.as_pointer_value()
        } else {
            // fallback
            let tmp = self.llvmbuilder.build_alloca(ty, "memcpy.temp").unwrap();
            self.llvmbuilder.build_store(tmp, rvalue).unwrap();
            tmp
        };

        self.llvmbuilder
            .build_memcpy(dest, dest_align, src_ptr, src_align, size_value)
            .unwrap();
    }

    pub(crate) fn intrinsic_strcmp(&self, lhs_ptr: PointerValue<'ll>, rhs_ptr: PointerValue<'ll>) -> IntValue<'ll> {
        let i32_type = self.llvmctx.i32_type();
        let i8_ptr_type = self.llvmctx.ptr_type(AddressSpace::default());

        let module = self.llvmmodule.borrow();

        let strcmp_func = match module.get_function("strcmp") {
            Some(func) => func,
            None => {
                let fn_type = i32_type.fn_type(
                    &[
                        i8_ptr_type.into(), // const char* lhs
                        i8_ptr_type.into(), // const char* rhs
                    ],
                    false,
                );
                module.add_function("strcmp", fn_type, None)
            }
        };

        let cmp_result = self
            .llvmbuilder
            .build_call(strcmp_func, &[lhs_ptr.into(), rhs_ptr.into()], "strcmp_call")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap();

        drop(module);
        cmp_result.into_int_value()
    }

    pub(crate) fn intrinsic_array_memcmp(&self, lhs_arr: ArrayValue<'ll>, rhs_arr: ArrayValue<'ll>) -> IntValue<'ll> {
        let i32_type = self.llvmctx.i32_type();
        let i8_ptr_type = self.llvmctx.ptr_type(AddressSpace::default());
        let target_data = self.llvmtm.get_target_data();
        let ptr_sized_int_type = self.llvmctx.ptr_sized_int_type(&target_data, None);

        let module = self.llvmmodule.borrow();
        let memcmp = match module.get_function("memcmp") {
            Some(func) => func,
            None => {
                let fn_type = i32_type.fn_type(
                    &[
                        i8_ptr_type.into(),        // const void* lhs
                        i8_ptr_type.into(),        // const void* rhs
                        ptr_sized_int_type.into(), // usize len
                    ],
                    false,
                );
                module.add_function("memcmp", fn_type, None)
            }
        };

        let lhs_alloca = self.llvmbuilder.build_alloca(lhs_arr.get_type(), "lhs_alloca").unwrap();
        let rhs_alloca = self.llvmbuilder.build_alloca(rhs_arr.get_type(), "rhs_alloca").unwrap();

        self.llvmbuilder.build_store(lhs_alloca, lhs_arr).unwrap();
        self.llvmbuilder.build_store(rhs_alloca, rhs_arr).unwrap();

        let zero = self.llvmctx.i32_type().const_zero();
        let gep_idx = &[zero, zero];
        let lhs_ptr = unsafe {
            self.llvmbuilder
                .build_in_bounds_gep(lhs_arr.get_type(), lhs_alloca, gep_idx, "lhs_gep")
                .unwrap()
        };
        let rhs_ptr = unsafe {
            self.llvmbuilder
                .build_in_bounds_gep(rhs_arr.get_type(), rhs_alloca, gep_idx, "rhs_gep")
                .unwrap()
        };

        let lhs_cast = self
            .llvmbuilder
            .build_pointer_cast(lhs_ptr, i8_ptr_type, "lhs_cast")
            .unwrap();
        let rhs_cast = self
            .llvmbuilder
            .build_pointer_cast(rhs_ptr, i8_ptr_type, "rhs_cast")
            .unwrap();

        let byte_size = target_data.get_bit_size(&lhs_arr.get_type()) / 8;
        let len_val = ptr_sized_int_type.const_int(byte_size as u64, false);

        let cmp = self
            .llvmbuilder
            .build_call(
                memcmp,
                &[lhs_cast.into(), rhs_cast.into(), len_val.into()],
                "memcmp_call",
            )
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap();

        drop(module);
        cmp.into_int_value()
    }

    pub(crate) fn intrinsic_copy_buffer_to_struct(
        &self,
        buffer: ArrayValue<'ll>,
        struct_type: StructType<'ll>,
    ) -> StructValue<'ll> {
        self.intrinsic_coerce_through_alloca(
            BasicValueEnum::ArrayValue(buffer),
            BasicTypeEnum::StructType(struct_type),
            "coerce",
        )
        .into_struct_value()
    }

    pub(crate) fn intrinsic_copy_payload_to_buffer(
        &self,
        mut value: BasicValueEnum<'ll>,
        array_type: ArrayType<'ll>,
    ) -> ArrayValue<'ll> {
        let alloca = self.llvmbuilder.build_alloca(array_type, "alloca").unwrap();

        value = self.intrinsic_coerce_through_alloca(value, BasicTypeEnum::ArrayType(array_type), "coerce");

        self.intrinsic_optimized_memcpy(alloca, value);

        // load back the array
        self.llvmbuilder
            .build_load(array_type, alloca, "load")
            .unwrap()
            .into_array_value()
    }
}
