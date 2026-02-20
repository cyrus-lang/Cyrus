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

use crate::{ABITargetArch, ABITargetInfo, ABITypeLayout, helpers::align_offset};
use cyrusc_cir::types::CIRTy;
use cyrusc_tast::types::PlainType;

pub fn type_layout(info: &ABITargetInfo, ty: &CIRTy) -> ABITypeLayout {
    match ty {
        CIRTy::PlainType(plain_type) => plain_type_layout(info, plain_type),
        CIRTy::Const(ty) => type_layout(info, ty.const_inner()),
        CIRTy::Pointer(_) => {
            let size = info.pointer_size();
            ABITypeLayout::normal(size, size)
        }
        CIRTy::Struct(struct_ty) => {
            let mut offset = 0;
            let mut max_align = 1;

            for ty in &struct_ty.fields {
                let field_layout = type_layout(info, ty);
                offset = align_offset(offset, field_layout.align);
                offset += field_layout.size;
                max_align = max_align.max(field_layout.align);
            }

            let total_size = align_offset(offset, max_align);
            ABITypeLayout::aggregate(total_size, max_align)
        }
        CIRTy::Union(union_ty) => {
            let mut max_size = 0;
            let mut max_align = 1;

            for ty in &union_ty.fields {
                let field_layout = type_layout(info, ty);

                max_size = max_size.max(field_layout.size);
                max_align = max_size.max(field_layout.align);
            }

            let total_size = align_offset(max_size, max_align);
            ABITypeLayout::aggregate(total_size, max_align)
        }
        CIRTy::Enum(_enum_ty) => todo!(),
        CIRTy::FuncType(_) => {
            let size = info.pointer_size();
            ABITypeLayout::normal(size, size)
        }
        CIRTy::Tuple(tuple_ty) => {
            // same layout as struct (lowered to struct in codegen)

            let mut offset = 0;
            let mut max_align = 0;

            for ty in &tuple_ty.items {
                let element_layout = type_layout(info, ty);
                offset = align_offset(offset, element_layout.align);
                offset += element_layout.size;
                max_align = max_align.max(element_layout.align);
            }

            let total_size = align_offset(offset, max_align);
            ABITypeLayout::aggregate(total_size, max_align)
        }
        CIRTy::Array(array_ty) => {
            let element_layout = type_layout(info, &array_ty.ty);
            let total_size = element_layout.size * array_ty.len as u32;
            ABITypeLayout::aggregate(total_size, element_layout.align)
        }
        CIRTy::Dynamic(_) => {
            let size = info.pointer_size() * 2; // data_ptr + vtable_ptr
            ABITypeLayout::normal(size, info.pointer_size())
        }
    }
}

fn plain_type_layout(info: &ABITargetInfo, plain_type: &PlainType) -> ABITypeLayout {
    use PlainType::*;

    match plain_type {
        UIntPtr | IntPtr | ISize | USize => {
            let size = info.pointer_size();
            ABITypeLayout::normal(size, size)
        }

        Int8 | UInt8 | Bool => ABITypeLayout::normal(1, 1),
        Int16 | UInt16 => ABITypeLayout::normal(2, 2),
        Int32 | UInt32 | Int | UInt => ABITypeLayout::normal(4, 4),
        Int64 | UInt64 => ABITypeLayout::normal(8, 8),
        Int128 | UInt128 => {
            let align = match info.arch {
                ABITargetArch::X86_64 | ABITargetArch::Aarch64 => 16,
                ABITargetArch::RiscV64 => 16,
                ABITargetArch::Wasm32 => 8,
            };
            ABITypeLayout::normal(16, align)
        }

        Float16 => ABITypeLayout::normal(2, 2),
        Float32 => ABITypeLayout::normal(4, 4),
        Float64 => ABITypeLayout::normal(8, 8),
        Float128 => {
            let align = match info.arch {
                ABITargetArch::X86_64 | ABITargetArch::Aarch64 => 16,
                ABITargetArch::RiscV64 => 16,
                ABITargetArch::Wasm32 => 8,
            };
            ABITypeLayout::normal(16, align)
        }

        Char => ABITypeLayout::normal(1, 1),
        Void => ABITypeLayout::normal(0, 1),
        Null => {
            let size = info.pointer_size();
            ABITypeLayout::normal(size, size)
        }
    }
}
