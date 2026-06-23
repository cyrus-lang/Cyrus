// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    abi::{
        layout::type_layout,
        target::ABITargetArch,
        targets::x86_64::types::X86_64TargetDependentType,
        types::{ABIFloatKind, ABIType, TargetIntegerType},
    },
    cir::{cir::CIREnumVariant, typectx::CIRTypeContext, types::CIRType},
};
use cyrusc_typed_ast::types::PlainType;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, Default)]
pub struct Registers {
    pub int_regs: u32,
    pub sse_regs: u32,
}

pub(crate) fn align_offset(offset: u32, align: u32) -> u32 {
    (offset + align - 1) / align * align
}

pub fn cir_type_to_abi_type(tctx: Arc<CIRTypeContext>, cir_type: &CIRType) -> ABIType {
    use PlainType::*;

    let info = &tctx.target_info;

    match cir_type {
        CIRType::Struct(type_id) => {
            let struct_type = tctx.get_struct(*type_id);

            let fields = struct_type
                .fields
                .iter()
                .map(|ty| cir_type_to_abi_type(tctx.clone(), ty))
                .collect();

            ABIType::Struct(fields, struct_type.is_packed())
        }
        CIRType::Union(type_id) => {
            let union_type = tctx.get_union(*type_id);

            let fields = union_type
                .fields
                .iter()
                .map(|ty| cir_type_to_abi_type(tctx.clone(), ty))
                .collect();

            ABIType::Union(fields)
        }
        CIRType::Enum(type_id) => {
            let enum_type = tctx.get_enum(*type_id);

            // enums are represented as a struct with tag and payload
            // first, determine if this is a simple C-style enum (no payload)
            if !enum_type.includes_payload() {
                // c-style enum without payload
                ABIType::Integer(32)
            } else {
                // enum with payload is struct { i32, [i8; N] }
                // need to compute max payload size to determine the byte array size
                let mut max_payload_size = 0;

                for variant in &enum_type.variants {
                    match variant {
                        CIREnumVariant::Unit(_, _) => {
                            // no payload
                        }
                        CIREnumVariant::Valued(_, value_type, _) => {
                            let layout = type_layout(info, value_type);
                            max_payload_size = max_payload_size.max(layout.size);
                        }
                        CIREnumVariant::Payload(_, struct_type, _) => {
                            let mut total_size = 0;
                            let mut max_align = 1;

                            for field_type in &struct_type.fields {
                                let layout = type_layout(info, field_type);
                                let field_align = layout.align;
                                let field_size = layout.size;

                                let padding = (field_align - (total_size % field_align)) % field_align;
                                total_size += padding + field_size;
                                max_align = max_align.max(field_align);
                            }

                            total_size = ((total_size + max_align - 1) / max_align) * max_align;
                            max_payload_size = max_payload_size.max(total_size);
                        }
                    }
                }

                // round payload size to at least 1 if there are any payload variants
                if max_payload_size == 0 && enum_type.includes_payload() {
                    max_payload_size = 1;
                }

                let payload_array = ABIType::Array {
                    element_ty: Box::new(ABIType::Integer(8)),
                    count: max_payload_size as usize,
                };

                ABIType::Struct(
                    vec![
                        ABIType::Integer(32), // tag
                        payload_array,        // payload
                    ],
                    false, // not packed
                )
            }
        }
        CIRType::Array(array_ty) => {
            let element_ty = Box::new(cir_type_to_abi_type(tctx, &array_ty.element_type));

            ABIType::Array {
                element_ty,
                count: array_ty.len,
            }
        }

        CIRType::Plain(plain_type) => {
            match plain_type {
                // Target-dependent types
                UIntPtr | IntPtr | ISize | USize | Int | UInt => match info.arch {
                    ABITargetArch::X86_64 => ABIType::TargetIntegerType(TargetIntegerType::X86_64(
                        X86_64TargetDependentType::from(plain_type),
                    )),
                    ABITargetArch::Aarch64 => todo!(),
                    ABITargetArch::RiscV64 => todo!(),
                    ABITargetArch::Wasm32 => todo!(),
                },

                Int8 | UInt8 => ABIType::Integer(8),
                Int16 | UInt16 => ABIType::Integer(16),
                Int32 | UInt32 => ABIType::Integer(32),
                Int64 | UInt64 => ABIType::Integer(64),
                Int128 | UInt128 => ABIType::Integer(128),

                Float16 => ABIType::Float(ABIFloatKind::F16),
                Float32 => ABIType::Float(ABIFloatKind::F32),
                Float64 => ABIType::Float(ABIFloatKind::F64),
                Float128 => ABIType::Float(ABIFloatKind::F128),

                Char | Bool => ABIType::Integer(8),
                Void => ABIType::Void,
                Null => ABIType::Pointer,
            }
        }

        CIRType::Const(ty) => cir_type_to_abi_type(tctx, ty),
        CIRType::Pointer(_) => ABIType::Pointer,
        CIRType::FuncType(_) => ABIType::Pointer,

        CIRType::Dynamic(_) => {
            ABIType::Struct(
                vec![
                    ABIType::Pointer, // data pointer
                    ABIType::Pointer, // vtable pointer
                ],
                false,
            )
        }
    }
}

#[inline]
pub(crate) fn is_cir_type_abi_aggregate(cir_type: &CIRType) -> bool {
    match cir_type {
        CIRType::Struct(_) | CIRType::Enum(_) | CIRType::Union(_) => true,
        _ => false,
    }
}
