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
use cyrusc_cir::{
    CIREnumTyVariant,
    types::{CIRArrayTy, CIREnumTy, CIRStructTy, CIRTupleTy, CIRTy, CIRUnionTy},
};
use cyrusc_tast::types::PlainType;
use inkwell::{
    AddressSpace,
    llvm_sys::{
        core::LLVMFunctionType,
        prelude::{LLVMBool, LLVMTypeRef},
    },
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
        }
    }

    pub(crate) fn emit_tys(&self, tys: &[CIRTy]) -> Vec<AnyTypeEnum<'ll>> {
        tys.iter().map(|ty| self.emit_ty(ty.clone())).collect()
    }

    pub(crate) fn emit_plain_ty(&self, plain_ty: PlainType) -> AnyTypeEnum<'ll> {
        let llvmctx = &self.llvmctx;

        match plain_ty {
            PlainType::UIntPtr | PlainType::IntPtr | PlainType::SizeT => llvmctx
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
        let field_types = self
            .emit_tys(&struct_ty.fields)
            .iter()
            .map(|ty| (*ty).try_into().unwrap())
            .collect::<Vec<BasicTypeEnum<'ll>>>();

        self.llvmctx.struct_type(&field_types, struct_ty.is_packed)
    }

    pub(crate) fn enum_payload_ty(&self, enum_ty: &CIREnumTy) -> (ArrayType<'ll>, u64) {
        let target_data = self.llvmtm.get_target_data();
        let mut max_payload_size: u64 = 0;
        let mut max_payload_align: u64 = 1;

        for variant in &enum_ty.variants {
            let (payload_size, payload_align) = match variant {
                CIREnumTyVariant::Identifier => (0, 1),
                CIREnumTyVariant::Valued(expr) => {
                    let llvm_ty: BasicTypeEnum<'ll> = self
                        .emit_ty(expr.ty.clone())
                        .try_into()
                        .expect("Enum value payload must be a valid llvm type.");
                    let size = target_data.get_store_size(&llvm_ty);
                    let align = target_data.get_abi_alignment(&llvm_ty) as u64;
                    (size, align)
                }
                CIREnumTyVariant::Fielded(field_tys) => {
                    if field_tys.is_empty() {
                        (0, 1)
                    } else {
                        let llvm_fields: Vec<BasicTypeEnum<'ll>> = self
                            .emit_tys(field_tys)
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
            max_payload_size = 1;
            max_payload_align = 1;
        }

        // round up size to alignment boundary
        let aligned_size = ((max_payload_size + (max_payload_align - 1)) / max_payload_align) * max_payload_align;

        let payload_buffer_ty = self.llvmctx.i8_type().array_type(aligned_size as u32);
        (payload_buffer_ty, aligned_size)
    }

    pub(crate) fn emit_enum_ty(&self, enum_ty: CIREnumTy) -> StructType<'ll> {
        let tag_type = self.llvmctx.i32_type();
        let (payload_ty, _) = self.enum_payload_ty(&enum_ty);
        self.llvmctx
            .struct_type(&[tag_type.as_basic_type_enum(), payload_ty.into()], false)
    }

    pub(crate) fn emit_union_ty(&self, union_ty: CIRUnionTy) -> StructType<'ll> {
        let mut largest: Option<BasicTypeEnum<'ll>> = None;
        let mut max_size = 0u64;

        let target_data = self.llvmtm.get_target_data();

        for field_ty in union_ty.fields {
            let llvm_ty = self
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
            .emit_tys(&tuple_ty.items)
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

    pub(crate) fn emit_func_ty(&self, func_ty: cyrusc_cir::types::CIRFuncTy) -> FunctionType<'ll> {
        let ret_ty = self.emit_ty(*func_ty.ret);

        let mut param_tys = self
            .emit_tys(&func_ty.params)
            .iter()
            .map(|ty| ty.as_type_ref())
            .collect::<Vec<LLVMTypeRef>>();

        let fn_ty = unsafe {
            LLVMFunctionType(
                ret_ty.as_type_ref(),
                param_tys.as_mut_ptr(),
                param_tys.len().try_into().unwrap(),
                func_ty.is_var as LLVMBool,
            )
        };

        unsafe { FunctionType::new(fn_ty) }
    }
}
