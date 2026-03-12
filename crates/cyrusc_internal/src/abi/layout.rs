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
    cir::{
        cir::CIREnumTyVariant,
        types::{CIRStructTy, CIRTy},
    },
};
use cyrusc_tast::types::PlainType;

#[derive(Debug, Clone)]
pub struct ABITypeLayout {
    pub size: u32,
    pub align: u32,
    pub field_offsets: Vec<ABIFieldOffsetInfo>,

    #[allow(unused)]
    pub is_aggregate: bool,
}

#[derive(Debug, Clone)]
pub enum ABIFieldOffsetInfo {
    Normal {
        index: u32,
        offset: u32,
        original_index: usize,
    },
    Padding {
        index: u32,
        offset: u32,
        size: u32,
    },
}

pub fn type_layout(info: &ABITargetInfo, ty: &CIRTy) -> ABITypeLayout {
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
            let is_packed = struct_ty.is_packed();

            let mut field_offset_index = 0u32;

            for (field_original_index, ty) in struct_ty.fields.iter().enumerate() {
                let field_layout = type_layout(info, ty);

                let effective_field_align = if is_packed { 1 } else { field_layout.align };

                // add padding before field (if not packed)
                if !is_packed {
                    let padding = (effective_field_align - (offset % effective_field_align)) % effective_field_align;

                    if padding > 0 {
                        // add padding field before real field
                        field_offsets.push(ABIFieldOffsetInfo::padding(field_offset_index, offset, padding));
                        field_offset_index += 1;
                        offset += padding;
                    }
                }

                // add the actual field
                field_offsets.push(ABIFieldOffsetInfo::normal(
                    field_offset_index,
                    offset,
                    field_original_index,
                ));
                field_offset_index += 1;

                offset += field_layout.size;

                max_align = max_align.max(field_layout.align);
            }

            if let Some(explicit_align) = struct_ty.align {
                max_align = max_align.max(explicit_align.try_into().unwrap());
            }

            if is_packed {
                max_align = 1;
            }

            let total_size = align_offset(offset, max_align);

            // add trailing padding if needed
            if total_size > offset {
                field_offsets.push(ABIFieldOffsetInfo::padding(
                    field_offsets.len().try_into().unwrap(),
                    offset,
                    total_size - offset,
                ));
            }

            ABITypeLayout::aggregate(total_size, max_align, field_offsets)
        }
        CIRTy::Union(union_ty) => {
            let mut max_size = 0;
            let mut max_align = 1;
            let mut field_offsets = Vec::new();

            for (original_index, ty) in union_ty.fields.iter().enumerate() {
                let field_layout = type_layout(info, ty);

                max_size = max_size.max(field_layout.size);
                max_align = max_align.max(field_layout.align);

                // all fields start at offset 0
                field_offsets.push(ABIFieldOffsetInfo::normal(
                    original_index.try_into().unwrap(),
                    0,
                    original_index,
                ));
            }

            let total_size = align_offset(max_size, max_align);
            ABITypeLayout::aggregate(total_size, max_align, field_offsets)
        }
        CIRTy::Enum(enum_ty) => {
            let tag_type = enum_ty.tag_type_or_infer_or_default();

            if enum_ty.is_scalar_optimizable() {
                return type_layout(info, &tag_type);
            }

            let tag_layout = type_layout(info, &tag_type);
            let tag_size = tag_layout.size;
            let tag_align = tag_layout.align;

            let mut max_payload_size = 0;
            let mut max_payload_align = 1;

            for variant in &enum_ty.variants {
                let (variant_size, variant_align) = match variant {
                    CIREnumTyVariant::Ident(_) => (0, 1),

                    CIREnumTyVariant::Valued(_, expr) => {
                        let layout = type_layout(info, &expr.ty);
                        (layout.size, layout.align)
                    }

                    CIREnumTyVariant::Fielded(_, field_types) => {
                        let struct_ty = CIRStructTy {
                            fields: field_types.clone(),
                            repr_attr: None,
                            align: None,
                        };

                        let layout = type_layout(info, &CIRTy::Struct(struct_ty));
                        (layout.size, layout.align)
                    }
                };

                max_payload_size = max_payload_size.max(variant_size);
                max_payload_align = max_payload_align.max(variant_align);
            }

            if max_payload_size == 0 && enum_ty.includes_payload() {
                max_payload_size = 1;
                max_payload_align = 1;
            }

            if let Some(align) = enum_ty.align {
                max_payload_align = max_payload_align.max(align as u32);
            }

            // payload offset must respect payload alignment
            let payload_offset = ((tag_size + (max_payload_align - 1)) / max_payload_align) * max_payload_align;

            let mut total_align = tag_align.max(max_payload_align);

            if let Some(align) = enum_ty.align {
                total_align = total_align.max(align as u32);
            }

            let mut total_size = payload_offset + max_payload_size;

            // struct size must be aligned
            total_size = ((total_size + (total_align - 1)) / total_align) * total_align;

            ABITypeLayout::aggregate(total_size, total_align, Vec::new())
        }
        CIRTy::FuncType(_) => {
            let size = info.pointer_size();
            ABITypeLayout::normal(size, size, Vec::new())
        }
        CIRTy::Tuple(tuple_ty) => {
            // tuple lowered as struct in codegen
            let struct_ty = CIRStructTy {
                fields: tuple_ty.elements.clone(),
                repr_attr: None,
                align: None,
            };
            type_layout(info, &CIRTy::Struct(struct_ty))
        }
        CIRTy::Array(array_ty) => {
            let element_layout = type_layout(info, &array_ty.ty);
            let total_size = element_layout.size * array_ty.len as u32;

            let mut field_offsets = Vec::new();
            for i in 0..array_ty.len {
                field_offsets.push(i as u32 * element_layout.size);
            }

            ABITypeLayout::aggregate(total_size, element_layout.align, Vec::new())
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
    pub fn normal(size: u32, align: u32, field_offsets: Vec<ABIFieldOffsetInfo>) -> Self {
        Self {
            size,
            align,
            field_offsets,
            is_aggregate: false,
        }
    }

    pub fn aggregate(size: u32, align: u32, field_offsets: Vec<ABIFieldOffsetInfo>) -> Self {
        Self {
            size,
            align,
            field_offsets,
            is_aggregate: true,
        }
    }

    pub fn lookup_field_index(&self, original_index: usize) -> Option<u32> {
        self.field_offsets
            .iter()
            .find(|field_offset| match field_offset.original_index() {
                Some(idx) => idx == original_index,
                None => false,
            })
            .map(|field_offset| field_offset.index())
    }
}

impl ABIFieldOffsetInfo {
    pub fn normal(index: u32, offset: u32, original_index: usize) -> Self {
        ABIFieldOffsetInfo::Normal {
            index,
            offset,
            original_index,
        }
    }

    pub fn padding(index: u32, offset: u32, size: u32) -> Self {
        ABIFieldOffsetInfo::Padding { index, offset, size }
    }

    pub fn offset(&self) -> u32 {
        match self {
            ABIFieldOffsetInfo::Normal { offset, .. } => *offset,
            ABIFieldOffsetInfo::Padding { offset, .. } => *offset,
        }
    }

    pub fn index(&self) -> u32 {
        match self {
            ABIFieldOffsetInfo::Normal { index, .. } => *index,
            ABIFieldOffsetInfo::Padding { index, .. } => *index,
        }
    }

    pub fn is_padding(&self) -> bool {
        matches!(self, ABIFieldOffsetInfo::Padding { .. })
    }

    pub fn size(&self) -> Option<u32> {
        match self {
            ABIFieldOffsetInfo::Normal { .. } => None,
            ABIFieldOffsetInfo::Padding { size, .. } => Some(*size),
        }
    }

    pub fn original_index(&self) -> Option<usize> {
        match self {
            ABIFieldOffsetInfo::Normal { original_index, .. } => Some(*original_index),
            ABIFieldOffsetInfo::Padding { .. } => None,
        }
    }
}
