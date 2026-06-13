// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_internal::abi::{
    target::ABITargetInfo,
    types::{ABIFloatKind, ABIType},
};
use inkwell::{
    AddressSpace,
    context::Context,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum},
};

pub(crate) fn abi_type_to_llvm_type<'ll>(
    llvm_ctx: &'ll Context,
    info: &ABITargetInfo,
    abi_type: &ABIType,
) -> AnyTypeEnum<'ll> {
    match abi_type {
        ABIType::Integer(bits) => llvm_ctx.custom_width_int_type(*bits).into(),
        ABIType::Float(float_kind) => match float_kind {
            ABIFloatKind::F16 => llvm_ctx.f16_type().into(),
            ABIFloatKind::F32 => llvm_ctx.f32_type().into(),
            ABIFloatKind::F64 => llvm_ctx.f64_type().into(),
            ABIFloatKind::F128 => llvm_ctx.f128_type().into(),
        },
        ABIType::Pointer => llvm_ctx.ptr_type(AddressSpace::default()).into(),
        ABIType::Vector { element_ty, lanes } => {
            let element_llvm_ty: BasicTypeEnum<'ll> =
                abi_type_to_llvm_type(llvm_ctx, info, element_ty).try_into().unwrap();

            match element_llvm_ty {
                BasicTypeEnum::IntType(int_type) => AnyTypeEnum::VectorType(int_type.vec_type(*lanes)),
                BasicTypeEnum::FloatType(float_type) => AnyTypeEnum::VectorType(float_type.vec_type(*lanes)),
                _ => panic!("Unsupported vector element type: {:?}", element_llvm_ty),
            }
        }
        ABIType::Array { element_ty, count } => {
            let element_llvm_ty: BasicTypeEnum<'ll> =
                abi_type_to_llvm_type(llvm_ctx, info, element_ty).try_into().unwrap();

            element_llvm_ty.array_type(*count as u32).into()
        }
        ABIType::Struct(fields, is_packed) => {
            let field_types: Vec<BasicTypeEnum<'ll>> = fields
                .iter()
                .map(|ty| abi_type_to_llvm_type(llvm_ctx, info, ty).try_into().unwrap())
                .collect();

            llvm_ctx.struct_type(&field_types, *is_packed).into()
        }
        ABIType::Union(fields) => {
            if fields.is_empty() {
                // empty union? use i8 (1 byte minimum)
                llvm_ctx.i8_type().into()
            } else {
                // find the largest field type
                let max_size_ty = fields.iter().max_by_key(|ty| info.abi_size_of(ty)).unwrap();

                // union is represented as an array of bytes of the max size
                let max_size = info.abi_size_of(max_size_ty) as usize;
                llvm_ctx.i8_type().array_type(max_size as u32).into()
            }
        }
        ABIType::TargetIntegerType(target_integer_type) => llvm_ctx
            .custom_width_int_type(target_integer_type.bit_width(info))
            .into(),
        ABIType::Void => llvm_ctx.void_type().into(),
    }
}
