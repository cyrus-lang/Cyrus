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
    abi::{
        helpers::align_offset,
        target::{ABITargetArch, ABITargetInfo},
    },
    cir::types::CIRTy,
};
use cyrusc_tast::types::PlainType;

pub(crate) struct ABITypeLayout {
    pub size: u32,
    pub align: u32,
    pub field_offsets: Vec<u32>,
    pub is_aggregate: bool,
}

pub(crate) fn type_layout(info: &ABITargetInfo, ty: &CIRTy) -> ABITypeLayout {
    match ty {
        CIRTy::PlainType(plain_type) => plain_type_layout(info, plain_type),
        CIRTy::Const(ty) => type_layout(info, ty.const_inner()),
        CIRTy::Pointer(_) => {
            let size = info.pointer_size();
            ABITypeLayout::normal(size, size, Vec::new())
        }
        CIRTy::Struct(struct_ty) => {
            let mut offset = 0;
            let mut max_align = 1;
            let mut field_offsets = Vec::new();

            for ty in &struct_ty.fields {
                let field_layout = type_layout(info, ty);

                // add padding before field
                let padding = (field_layout.align - (offset % field_layout.align)) % field_layout.align;
                offset += padding;

                field_offsets.push(offset);

                offset += field_layout.size;
                max_align = max_align.max(field_layout.align);
            }

            let total_size = align_offset(offset, max_align);
            ABITypeLayout::aggregate(total_size, max_align, field_offsets)
        }
        CIRTy::Union(union_ty) => {
            let mut max_size = 0;
            let mut max_align = 1;
            let mut field_offsets = Vec::new();

            for ty in &union_ty.fields {
                let field_layout = type_layout(info, ty);

                max_size = max_size.max(field_layout.size);
                max_align = max_align.max(field_layout.align);

                // Unions: all fields start at offset 0
                field_offsets.push(0);
            }

            let total_size = align_offset(max_size, max_align);
            ABITypeLayout::aggregate(total_size, max_align, field_offsets)
        }
        CIRTy::Enum(_enum_ty) => todo!(),
        CIRTy::FuncType(_) => {
            let size = info.pointer_size();
            ABITypeLayout::normal(size, size, Vec::new())
        }
        CIRTy::Tuple(tuple_ty) => {
            let mut offset = 0;
            let mut max_align = 0;
            let mut field_offsets = Vec::new();

            for ty in &tuple_ty.elements {
                let element_layout = type_layout(info, ty);

                // add padding before element
                let padding = (element_layout.align - (offset % element_layout.align)) % element_layout.align;
                offset += padding;

                field_offsets.push(offset);

                offset += element_layout.size;
                max_align = max_align.max(element_layout.align);
            }

            let total_size = align_offset(offset, max_align);
            ABITypeLayout::aggregate(total_size, max_align, field_offsets)
        }
        CIRTy::Array(array_ty) => {
            let element_layout = type_layout(info, &array_ty.ty);
            let total_size = element_layout.size * array_ty.len as u32;

            let mut field_offsets = Vec::new();
            for i in 0..array_ty.len {
                field_offsets.push(i as u32 * element_layout.size);
            }

            ABITypeLayout::aggregate(total_size, element_layout.align, field_offsets)
        }
        CIRTy::Dynamic(_) => {
            let size = info.pointer_size() * 2; // data_ptr + vtable_ptr
            ABITypeLayout::normal(size, info.pointer_size(), Vec::new())
        }
    }
}

fn plain_type_layout(info: &ABITargetInfo, plain_type: &PlainType) -> ABITypeLayout {
    use PlainType::*;

    match plain_type {
        UIntPtr | IntPtr | ISize | USize => {
            let size = info.pointer_size();
            ABITypeLayout::normal(size, size, Vec::new())
        }

        Int8 | UInt8 | Bool => ABITypeLayout::normal(1, 1, Vec::new()),
        Int16 | UInt16 => ABITypeLayout::normal(2, 2, Vec::new()),
        Int32 | UInt32 | Int | UInt => ABITypeLayout::normal(4, 4, Vec::new()),
        Int64 | UInt64 => ABITypeLayout::normal(8, 8, Vec::new()),
        Int128 | UInt128 => {
            let align = match info.arch {
                ABITargetArch::X86_64 | ABITargetArch::Aarch64 => 16,
                ABITargetArch::RiscV64 => 16,
                ABITargetArch::Wasm32 => 8,
            };
            ABITypeLayout::normal(16, align, Vec::new())
        }

        Float16 => ABITypeLayout::normal(2, 2, Vec::new()),
        Float32 => ABITypeLayout::normal(4, 4, Vec::new()),
        Float64 => ABITypeLayout::normal(8, 8, Vec::new()),
        Float128 => {
            let align = match info.arch {
                ABITargetArch::X86_64 | ABITargetArch::Aarch64 => 16,
                ABITargetArch::RiscV64 => 16,
                ABITargetArch::Wasm32 => 8,
            };
            ABITypeLayout::normal(16, align, Vec::new())
        }

        Char => ABITypeLayout::normal(1, 1, Vec::new()),
        Void => ABITypeLayout::normal(0, 1, Vec::new()),
        Null => {
            let size = info.pointer_size();
            ABITypeLayout::normal(size, size, Vec::new())
        }
    }
}

impl ABITypeLayout {
    pub fn normal(size: u32, align: u32, field_offsets: Vec<u32>) -> Self {
        Self {
            size,
            align,
            field_offsets,
            is_aggregate: false,
        }
    }

    pub fn aggregate(size: u32, align: u32, field_offsets: Vec<u32>) -> Self {
        Self {
            size,
            align,
            field_offsets,
            is_aggregate: true,
        }
    }
}
