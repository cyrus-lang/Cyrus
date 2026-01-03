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
use crate::builder::builder::IRBuilderCtx;
use inkwell::{
    AddressSpace,
    types::{ArrayType, BasicType, StructType},
    values::{ArrayValue, BasicValueEnum, IntValue, StructValue},
};

impl<'ll> IRBuilderCtx<'ll> {
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
                        ptr_sized_int_type.into(), // size_t len
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
        let struct_alloca = self.llvmbuilder.build_alloca(struct_type, "struct_alloca").unwrap();

        let buffer_alloca = self
            .llvmbuilder
            .build_alloca(buffer.get_type(), "buffer_alloca")
            .unwrap();
        self.llvmbuilder.build_store(buffer_alloca, buffer).unwrap();

        let i8_ptr_type = self.llvmctx.ptr_type(AddressSpace::default());
        let dest_i8_ptr = self
            .llvmbuilder
            .build_pointer_cast(struct_alloca, i8_ptr_type, "dest_i8")
            .unwrap();
        let src_i8_ptr = self
            .llvmbuilder
            .build_pointer_cast(buffer_alloca, i8_ptr_type, "src_i8")
            .unwrap();

        let struct_size = struct_type.size_of().unwrap();
        self.llvmbuilder
            .build_memcpy(dest_i8_ptr, 1, src_i8_ptr, 1, struct_size)
            .unwrap();

        self.llvmbuilder
            .build_load(struct_type, struct_alloca, "load_struct")
            .unwrap()
            .into_struct_value()
    }

    pub(crate) fn intrinsic_copy_payload_to_buffer(
        &self,
        src_value: BasicValueEnum<'ll>,
        dest_array_type: ArrayType<'ll>,
    ) -> ArrayValue<'ll> {
        let builder = &self.llvmbuilder;

        let array_alloca = builder.build_alloca(dest_array_type, "alloca").unwrap();
        builder.build_store(array_alloca, dest_array_type.const_zero()).unwrap(); // zero-init

        let src_ptr = match src_value {
            BasicValueEnum::PointerValue(ptr) => ptr,
            _ => {
                let tmp_alloca = builder.build_alloca(src_value.get_type(), "tmp").unwrap();
                builder.build_store(tmp_alloca, src_value).unwrap();
                tmp_alloca
            }
        };

        let i8_ptr_type = self.llvmctx.ptr_type(AddressSpace::default());
        let dest_i8_ptr = builder
            .build_pointer_cast(array_alloca, i8_ptr_type, "dest_i8")
            .unwrap();
        let src_i8_ptr = builder.build_pointer_cast(src_ptr, i8_ptr_type, "src_i8").unwrap();

        let src_size = src_value.get_type().size_of().unwrap();
        builder.build_memcpy(dest_i8_ptr, 1, src_i8_ptr, 1, src_size).unwrap();

        // Load back the array
        builder
            .build_load(dest_array_type, array_alloca, "load")
            .unwrap()
            .into_array_value()
    }
}
