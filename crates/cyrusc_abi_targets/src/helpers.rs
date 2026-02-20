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

use crate::types::{ABIFloatKind, ABIType, TargetDependentType};
use cyrusc_cir::types::CIRTy;
use cyrusc_tast::types::PlainType;

pub(crate) fn align_offset(offset: u32, align: u32) -> u32 {
    (offset + align - 1) / align * align
}

pub(crate) fn cir_type_to_abi_type(cir_type: &CIRTy) -> ABIType {
    use PlainType::*;

    match cir_type {
        CIRTy::PlainType(plain_type) => {
            match plain_type {
                // Target-dependent types
                UIntPtr | IntPtr | ISize | USize | Int | UInt => {
                    ABIType::TargetDependent(Box::new(TargetDependentType::from(plain_type)))
                }

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
        CIRTy::Const(ty) => cir_type_to_abi_type(ty),
        CIRTy::Pointer(_) => ABIType::Pointer,
        CIRTy::Struct(cirstruct_ty) => {
            let fields = cirstruct_ty
                .fields
                .iter()
                .map(|field_ty| cir_type_to_abi_type(field_ty))
                .collect();
            ABIType::Struct(fields)
        }
        CIRTy::Union(cirunion_ty) => {
            let fields = cirunion_ty
                .fields
                .iter()
                .map(|field_ty| cir_type_to_abi_type(field_ty))
                .collect();
            ABIType::Union(fields)
        }
        CIRTy::FuncType(_) => ABIType::Pointer,
        CIRTy::Tuple(tuple_ty) => {
            let elements = tuple_ty
                .elements
                .iter()
                .map(|elem_ty| cir_type_to_abi_type(elem_ty))
                .collect();
            ABIType::Struct(elements)
        }
        CIRTy::Array(array_ty) => {
            let element_ty = Box::new(cir_type_to_abi_type(&array_ty.ty));
            ABIType::Array {
                element_ty,
                count: array_ty.len,
            }
        }
        CIRTy::Dynamic(_) => {
            ABIType::Struct(vec![
                ABIType::Pointer, // data pointer
                ABIType::Pointer, // vtable pointer
            ])
        }
        CIRTy::Enum(_) => {
            // Enums are typically represented as integers in ABIs
            // The size depends on the discriminant type
            // match &cirenum_ty.discriminant_type {
            //     Some(disc_ty) => cir_type_to_abi_type(disc_ty),
            //     None => ABIType::Integer(32), // Default enum size
            // }
            // TODO
            unimplemented!()
        }
    }
}
