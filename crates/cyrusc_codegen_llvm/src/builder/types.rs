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
use crate::llvm::abi::abi_type::abi_type_to_llvm_type;
use cyrusc_internal::abi::layout::{ABIFieldOffsetInfo, type_layout};
use cyrusc_internal::cir::cir::CIREnumTyVariant;
use cyrusc_internal::cir::types::{CIRArrayTy, CIREnumTy, CIRFuncTy, CIRStructTy, CIRTupleTy, CIRTy, CIRUnionTy};
use cyrusc_tast::types::PlainType;
use inkwell::{
    AddressSpace,
    llvm_sys::{core::LLVMFunctionType, prelude::LLVMBool},
    types::{AnyType, AnyTypeEnum, ArrayType, AsTypeRef, BasicType, BasicTypeEnum, FunctionType, StructType},
};

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_ty(&self, ty: CIRTy) -> AnyTypeEnum<'ll> {
        match ty {
            CIRTy::Const(inner_ty) => self.emit_ty(*inner_ty),
            CIRTy::PlainType(plain_ty) => self.emit_plain_ty(plain_ty),
            CIRTy::Pointer(_) => self.llvmctx.ptr_type(AddressSpace::default()).as_any_type_enum(),
            CIRTy::Struct(struct_ty) => self.emit_struct_ty(struct_ty).as_any_type_enum(),
            CIRTy::Enum(enum_ty) => self.emit_enum_ty(enum_ty).as_any_type_enum(),
            CIRTy::Union(union_ty) => self.emit_union_ty(union_ty).as_any_type_enum(),
            CIRTy::Tuple(tuple_ty) => self.emit_tuple_ty(tuple_ty).as_any_type_enum(),
            CIRTy::Array(array_ty) => self.emit_arr_ty(array_ty).as_any_type_enum(),
            CIRTy::FuncType(..) => self.llvmctx.ptr_type(AddressSpace::default()).as_any_type_enum(),
            CIRTy::Dynamic(..) => self.emit_dynamic_ty().as_any_type_enum(),
        }
    }

    pub(crate) fn cir_dynamic_ty(&self, data_ptr_inner_ty: CIRTy) -> CIRTy {
        CIRTy::Struct(CIRStructTy {
            fields: vec![
                CIRTy::Pointer(Box::new(data_ptr_inner_ty)),
                CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))),
            ],
            align: None,
            repr_attr: None,
        })
    }

    pub(crate) fn emit_dynamic_ty(&self) -> StructType<'ll> {
        let vtable_ptr = self.llvmctx.ptr_type(AddressSpace::default()).as_basic_type_enum();
        let data_ptr = self.llvmctx.ptr_type(AddressSpace::default()).as_basic_type_enum();

        self.llvmctx.struct_type(&[data_ptr, vtable_ptr], false)
    }

    pub(crate) fn emit_vtable_ty(&self, methods_len: usize) -> StructType<'ll> {
        self.llvmctx.struct_type(
            &(0..methods_len)
                .map(|_| self.llvmctx.ptr_type(AddressSpace::default()).as_basic_type_enum())
                .collect::<Vec<_>>(),
            false,
        )
    }

    pub(crate) fn emit_types(&self, tys: &[CIRTy]) -> Vec<AnyTypeEnum<'ll>> {
        tys.iter().map(|ty| self.emit_ty(ty.clone())).collect()
    }

    pub(crate) fn emit_plain_ty(&self, plain_ty: PlainType) -> AnyTypeEnum<'ll> {
        let llvmctx = &self.llvmctx;

        match plain_ty {
            PlainType::UIntPtr | PlainType::IntPtr | PlainType::USize | PlainType::ISize => llvmctx
                .ptr_sized_int_type(&self.llvmtm.get_target_data(), None)
                .as_any_type_enum(),
            PlainType::Int8 => llvmctx.i8_type().as_any_type_enum(),
            PlainType::Int16 => llvmctx.i16_type().as_any_type_enum(),
            PlainType::Int32 => llvmctx.i32_type().as_any_type_enum(),
            PlainType::Int64 => llvmctx.i64_type().as_any_type_enum(),
            PlainType::Int128 => llvmctx.i128_type().as_any_type_enum(),
            PlainType::UInt8 => llvmctx.i8_type().as_any_type_enum(),
            PlainType::UInt16 => llvmctx.i16_type().as_any_type_enum(),
            PlainType::UInt32 => llvmctx.i32_type().as_any_type_enum(),
            PlainType::UInt64 => llvmctx.i64_type().as_any_type_enum(),
            PlainType::UInt128 => llvmctx.i128_type().as_any_type_enum(),
            PlainType::Int => llvmctx.i32_type().as_any_type_enum(),
            PlainType::UInt => llvmctx.i32_type().as_any_type_enum(),
            PlainType::Float16 => llvmctx.f16_type().as_any_type_enum(),
            PlainType::Float32 => llvmctx.f32_type().as_any_type_enum(),
            PlainType::Float64 => llvmctx.f64_type().as_any_type_enum(),
            PlainType::Float128 => llvmctx.f128_type().as_any_type_enum(),
            PlainType::Char => llvmctx.i8_type().as_any_type_enum(),
            PlainType::Bool => {
                // Booleans are stored as i8 in memory for stable layout and ABI compatibility.
                // i1 is reserved for logical operations only.
                llvmctx.i8_type().as_any_type_enum()
            }
            PlainType::Void => llvmctx.void_type().as_any_type_enum(),
            PlainType::Null => llvmctx.ptr_type(AddressSpace::default()).as_any_type_enum(),
        }
    }

    pub(crate) fn emit_struct_ty(&self, struct_ty: CIRStructTy) -> StructType<'ll> {
        let is_packed = struct_ty.is_packed();
        let layout = type_layout(&self.target.info, &CIRTy::Struct(struct_ty.clone()));

        let mut llvm_field_types: Vec<BasicTypeEnum<'ll>> = Vec::new();
        let mut next_field_index = 0;

        for field_offset in &layout.field_offsets {
            match field_offset {
                ABIFieldOffsetInfo::Normal { .. } => {
                    // get the next actual field from the struct
                    let field_ty = &struct_ty.fields[next_field_index];
                    let llvm_ty: BasicTypeEnum<'ll> = self.emit_ty(field_ty.clone()).try_into().unwrap();
                    llvm_field_types.push(llvm_ty);
                    next_field_index += 1;
                }
                ABIFieldOffsetInfo::Padding { size, .. } => {
                    // create padding array
                    let padding_array = self.llvmctx.i8_type().array_type(*size);
                    llvm_field_types.push(padding_array.as_basic_type_enum());
                }
            }
        }

        assert_eq!(
            next_field_index,
            struct_ty.fields.len(),
            "mismatch between layout fields and struct fields"
        );

        self.llvmctx.struct_type(&llvm_field_types, is_packed)
    }

    pub(crate) fn enum_payload_ty(&self, enum_ty: &CIREnumTy) -> (ArrayType<'ll>, u64) {
        let target_data = self.llvmtm.get_target_data();
        let mut max_payload_size: u64 = 0;
        let mut max_payload_align: u64 = 1;

        for variant in &enum_ty.variants {
            let (payload_size, payload_align) = match variant {
                CIREnumTyVariant::Ident(_) => (0, 1),
                CIREnumTyVariant::Valued(_, expr) => {
                    let llvm_ty: BasicTypeEnum<'ll> = self
                        .emit_ty(expr.ty.clone())
                        .try_into()
                        .expect("Enum value payload must be a valid llvm type.");
                    let size = target_data.get_store_size(&llvm_ty);
                    let align = target_data.get_abi_alignment(&llvm_ty) as u64;
                    (size, align)
                }
                CIREnumTyVariant::Fielded(_, field_tys) => {
                    if field_tys.is_empty() {
                        (0, 1)
                    } else {
                        let llvm_fields: Vec<BasicTypeEnum<'ll>> = self
                            .emit_types(field_tys)
                            .iter()
                            .map(|ty| (*ty).try_into().unwrap())
                            .collect();

                        let struct_ty = self.llvmctx.struct_type(&llvm_fields, false);
                        let size = target_data.get_store_size(&struct_ty);
                        let align = target_data.get_abi_alignment(&struct_ty) as u64;
                        (size, align)
                    }
                }
            };

            if payload_size > max_payload_size {
                max_payload_size = payload_size;
                max_payload_align = payload_align;
            }
        }

        if max_payload_size == 0 {
            if enum_ty.includes_payload() {
                max_payload_size = 1;
                max_payload_align = 1;
            } else {
                // simple enum
                max_payload_size = 0;
                max_payload_align = 1;
            }
        }

        // round up size to alignment boundary
        let aligned_size = ((max_payload_size + (max_payload_align - 1)) / max_payload_align) * max_payload_align;

        let payload_buffer_ty = self.llvmctx.i8_type().array_type(aligned_size as u32);
        (payload_buffer_ty, aligned_size)
    }

    fn emit_repr_c_enum_ty(&self, enum_ty: &CIREnumTy) -> BasicTypeEnum<'ll> {
        let cir_tag_type = enum_ty
            .tag_type
            .clone()
            .unwrap_or(Box::new(CIRTy::PlainType(PlainType::UInt32)));

        self.emit_ty(*cir_tag_type.clone()).try_into().unwrap()
    }

    pub(crate) fn emit_enum_ty(&self, enum_ty: CIREnumTy) -> BasicTypeEnum<'ll> {
        if enum_ty.is_repr_c() || !enum_ty.includes_payload() {
            // c-compatible enum
            self.emit_repr_c_enum_ty(&enum_ty)
        } else {
            // cyrus special enum
            let cir_tag_type = enum_ty
                .tag_type
                .clone()
                .unwrap_or(Box::new(CIRTy::PlainType(PlainType::UInt32)));

            let tag_type: BasicTypeEnum<'ll> = self.emit_ty(*cir_tag_type.clone()).try_into().unwrap();

            let (payload_ty, _) = self.enum_payload_ty(&enum_ty);
            self.llvmctx
                .struct_type(&[tag_type.as_basic_type_enum(), payload_ty.into()], false)
                .as_basic_type_enum()
        }
    }

    pub(crate) fn emit_union_ty(&self, union_ty: CIRUnionTy) -> StructType<'ll> {
        let mut largest: Option<BasicTypeEnum<'ll>> = None;
        let mut max_size = 0u64;

        let target_data = self.llvmtm.get_target_data();

        for field_ty in union_ty.fields {
            let llvm_ty: BasicTypeEnum<'ll> = self
                .emit_ty(field_ty.clone())
                .try_into()
                .expect("Union variant must be a valid basic type");

            let size = target_data.get_store_size(&llvm_ty);
            if size > max_size {
                max_size = size;
                largest = Some(llvm_ty);
            }
        }

        let largest_ty = largest.expect("Union must have at least one field");
        self.llvmctx.struct_type(&[largest_ty], false)
    }

    pub(crate) fn emit_tuple_ty(&self, tuple_ty: CIRTupleTy) -> StructType<'ll> {
        let element_types = self
            .emit_types(&tuple_ty.elements)
            .iter()
            .map(|ty| (*ty).try_into().unwrap())
            .collect::<Vec<BasicTypeEnum<'ll>>>();
        self.llvmctx.struct_type(&element_types, false)
    }

    pub(crate) fn emit_arr_ty(&self, array_ty: CIRArrayTy) -> AnyTypeEnum<'ll> {
        let elm_ty: BasicTypeEnum<'ll> = self
            .emit_ty(*array_ty.ty)
            .try_into()
            .expect("Array element must be a valid llvm type.");
        elm_ty.array_type(array_ty.len as u32).as_any_type_enum()
    }

    pub(crate) fn emit_func_ty(&self, func_ty: CIRFuncTy) -> FunctionType<'ll> {
        let abi_func_info = func_ty.abi_func_info.as_ref().unwrap();

        let ret_type = self.emit_ty(*func_ty.ret);

        let mut param_types = Vec::new();

        for (idx, abi_type) in abi_func_info.params_types.iter().enumerate() {
            // FIXME: VAArgs not handled when accessing params_infos.
            let abi_arg_info = &abi_func_info.params_infos[idx];

            let param_type_ref = if abi_arg_info.is_indirect_by_val() {
                self.llvmctx.ptr_type(AddressSpace::default()).as_type_ref()
            } else {
                abi_type_to_llvm_type(self.llvmctx, &self.target.info, abi_type).as_type_ref()
            };

            param_types.push(param_type_ref);
        }

        let fn_ty = unsafe {
            LLVMFunctionType(
                ret_type.as_type_ref(),
                param_types.as_mut_ptr(),
                param_types.len().try_into().unwrap(),
                func_ty.is_var as LLVMBool,
            )
        };

        unsafe { FunctionType::new(fn_ty) }
    }
}
