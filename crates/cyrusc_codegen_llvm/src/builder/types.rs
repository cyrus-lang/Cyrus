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

use crate::builder::builder::CodeGenIRBuilder;
use crate::llvm::abi::abi_type::abi_type_to_llvm_type;
use crate::llvm::debug_info::{
    debug_array_type, debug_const_type, debug_dynamic_type, debug_enum_type, debug_member_type, debug_pointer_type,
    debug_scalar_enum_type, debug_simple_type, debug_struct_type, debug_union_type,
};
use crate::llvm::dwarf::{DW_ATE_BOOLEAN, DW_ATE_FLOAT, DW_ATE_SIGNED, DW_ATE_UNSIGNED, DW_ATE_UNSIGNED_CHAR};
use cyrusc_source_loc::Loc;
use cyrusc_internal::abi::args::{ABIArgKind, ABIFunctionInfo, ExpandKind};
use cyrusc_internal::abi::layout::{ABIFieldOffsetInfo, type_layout};
use cyrusc_internal::cir::cir::CIREnumVariant;
use cyrusc_internal::cir::types::{CIRArrayType, CIREnumType, CIRFuncType, CIRStructType, CIRTupleType, CIRType, CIRUnionType};
use cyrusc_typed_ast::types::PlainType;
use inkwell::llvm_sys::prelude::{LLVMMetadataRef, LLVMTypeRef};
use inkwell::{
    AddressSpace,
    llvm_sys::{core::LLVMFunctionType, prelude::LLVMBool},
    types::{AnyType, AnyTypeEnum, ArrayType, AsTypeRef, BasicType, BasicTypeEnum, FunctionType, StructType},
};

impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_debug_ty_metadata(&self, ty: &CIRType) -> LLVMMetadataRef {
        let llvm_ty = self.emit_ty(ty.clone());

        if let Some(metadata) = self.dctx.type_cache.get(&llvm_ty.as_type_ref()) {
            return *metadata;
        }

        match ty {
            CIRType::Plain(plain_type) => {
                let name = plain_type.to_string();
                let layout = type_layout(&self.target.info, &CIRType::Plain(plain_type.clone()));
                let bits = layout.size * 8;

                let encoding = match plain_type {
                    PlainType::Int
                    | PlainType::Int8
                    | PlainType::Int16
                    | PlainType::Int32
                    | PlainType::Int64
                    | PlainType::Int128
                    | PlainType::ISize
                    | PlainType::IntPtr => DW_ATE_SIGNED,

                    PlainType::UInt
                    | PlainType::UInt8
                    | PlainType::UInt16
                    | PlainType::UInt32
                    | PlainType::UInt64
                    | PlainType::UInt128
                    | PlainType::USize
                    | PlainType::UIntPtr => DW_ATE_UNSIGNED,

                    PlainType::Float16 | PlainType::Float32 | PlainType::Float64 | PlainType::Float128 => DW_ATE_FLOAT,
                    PlainType::Bool => DW_ATE_BOOLEAN,
                    PlainType::Char => DW_ATE_UNSIGNED_CHAR,

                    PlainType::Void | PlainType::Null => {
                        return std::ptr::null_mut() as LLVMMetadataRef;
                    }
                };

                unsafe { debug_simple_type(&self.dctx, &name, bits as u64, encoding as u32) }
            }
            CIRType::Const(inner) => {
                let inner_ty_metadata = self.emit_debug_ty_metadata(inner);
                unsafe { debug_const_type(&self.dctx, inner_ty_metadata) }
            }
            CIRType::Pointer(inner) => {
                let inner_ty_metadata = self.emit_debug_ty_metadata(inner);
                let ptr_size_bits = self.target.info.pointer_size() * 8;
                let ptr_align = self.target.info.pointer_align() * 8;
                unsafe { debug_pointer_type(&self.dctx, inner_ty_metadata, ptr_size_bits as u64, ptr_align, "T*") }
            }
            CIRType::Struct(struct_type) => unsafe {
                let layout = type_layout(&self.target.info, &CIRType::Struct(struct_type.clone()));
                let size_bits = layout.size * 8;
                let align_bits = layout.align * 8;

                let mut elements_metadata: Vec<LLVMMetadataRef> = struct_type
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| {
                        let field_type_metadata = self.emit_debug_ty_metadata(ty);
                        let offset_bits = layout.lookup_field_offset(i) * 8;

                        let (name, loc) = &struct_type.fields_info[i];

                        debug_member_type(
                            &self.dctx,
                            &name,
                            field_type_metadata,
                            offset_bits as u64,
                            loc.line as u32,
                        )
                    })
                    .collect();

                let struct_name = struct_type.name.clone().unwrap_or("<unnamed_struct>".to_string());

                debug_struct_type(
                    &self.dctx,
                    &struct_name,
                    &mut elements_metadata,
                    size_bits as u64,
                    align_bits,
                    struct_type.loc.line.try_into().unwrap(),
                )
            },
            CIRType::Tuple(tuple_type) => {
                let layout = type_layout(&self.target.info, &CIRType::Tuple(tuple_type.clone()));

                let mut elements_metadata: Vec<LLVMMetadataRef> = tuple_type
                    .elements
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| {
                        let field_type_metadata = self.emit_debug_ty_metadata(ty);
                        let offset_bits = layout.lookup_field_offset(i) * 8;

                        let name = i.to_string();

                        unsafe {
                            debug_member_type(
                                &self.dctx,
                                &name,
                                field_type_metadata,
                                offset_bits as u64,
                                // FIXME: Expected to have exact location of the element
                                // but hence it's not implemented correctly in the AST
                                // using tuple_type.loc for now.
                                tuple_type.loc.line as u32,
                            )
                        }
                    })
                    .collect();

                let tuple_name = "<tuple>".to_string();

                unsafe {
                    debug_struct_type(
                        &self.dctx,
                        &tuple_name,
                        &mut elements_metadata,
                        layout.size as u64,
                        layout.align,
                        tuple_type.loc.line.try_into().unwrap(),
                    )
                }
            }
            CIRType::Enum(enum_type) => {
                let layout = type_layout(&self.target.info, &CIRType::Enum(enum_type.clone()));
                let size_bits = layout.size * 8;
                let align_bits = layout.align * 8;

                let enum_name = enum_type.name.clone().unwrap_or("<unnamed_enum>".to_string());

                let tag_type = self.emit_debug_ty_metadata(&enum_type.tag_type_or_infer_or_default());

                if enum_type.is_scalar_optimizable() {
                    let variants: Vec<(String, i64)> = enum_type
                        .variants
                        .iter()
                        .map(|variant| match variant {
                            CIREnumVariant::Unit(ident) => {
                                let tag = enum_type.compute_variant_tag(ident).unwrap();
                                (ident.clone(), tag as i64)
                            }
                            CIREnumVariant::Valued(ident, _) => {
                                let tag = enum_type.compute_variant_tag(ident).unwrap();
                                (ident.clone(), tag as i64)
                            }
                            CIREnumVariant::Payload(..) => unreachable!(),
                        })
                        .collect();

                    unsafe {
                        debug_scalar_enum_type(
                            &self.dctx,
                            &enum_name,
                            &variants,
                            size_bits as u64,
                            align_bits,
                            enum_type.loc.line as u32,
                            tag_type,
                        )
                    }
                } else {
                    let variants: Vec<(String, i64, LLVMMetadataRef)> = enum_type
                        .variants
                        .iter()
                        .map(|variant| {
                            let ident = variant.ident();
                            let tag = enum_type.compute_variant_tag(ident).unwrap();

                            match variant {
                                CIREnumVariant::Unit(_) => {
                                    (ident.clone(), tag as i64, std::ptr::null_mut() as LLVMMetadataRef)
                                }
                                CIREnumVariant::Valued(_, _) => {
                                    (ident.clone(), tag as i64, std::ptr::null_mut() as LLVMMetadataRef)
                                }
                                CIREnumVariant::Payload(_, elements) => {
                                    let tuple_type = CIRTupleType {
                                        elements: elements.to_vec(),
                                        loc: enum_type.loc,
                                    };

                                    let tuple_type_metadata = self.emit_debug_ty_metadata(&CIRType::Tuple(tuple_type));

                                    (ident.clone(), tag as i64, tuple_type_metadata)
                                }
                            }
                        })
                        .collect();

                    unsafe {
                        debug_enum_type(
                            &self.dctx,
                            &enum_name,
                            enum_type.loc.line as u32,
                            tag_type,
                            &variants,
                            size_bits as u64,
                            align_bits,
                        )
                    }
                }
            }
            CIRType::Union(union_type) => {
                let layout = type_layout(&self.target.info, &CIRType::Union(union_type.clone()));

                let mut elements_metadata: Vec<LLVMMetadataRef> = union_type
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| {
                        let field_type_metadata = self.emit_debug_ty_metadata(ty);
                        let offset_bits = layout.lookup_field_offset(i) * 8;

                        let (name, loc) = &union_type.fields_info[i];

                        unsafe {
                            debug_member_type(
                                &self.dctx,
                                &name,
                                field_type_metadata,
                                offset_bits as u64,
                                loc.line as u32,
                            )
                        }
                    })
                    .collect();

                let union_name = union_type.name.clone().unwrap_or("<unnamed_union>".to_string());

                unsafe {
                    debug_union_type(
                        &self.dctx,
                        &union_name,
                        &mut elements_metadata,
                        layout.size as u64,
                        layout.align,
                        union_type.loc.line.try_into().unwrap(),
                    )
                }
            }
            CIRType::FuncType(func_type) => self.emit_func_metadata(func_type),
            CIRType::Array(array_type) => {
                let element_ty_metadata = self.emit_debug_ty_metadata(&array_type.element_type);
                let layout = type_layout(&self.target.info, &CIRType::Array(array_type.clone()));

                unsafe {
                    debug_array_type(
                        &self.dctx,
                        element_ty_metadata,
                        array_type.len as u64,
                        layout.size as u64,
                        layout.align,
                    )
                }
            }
            CIRType::Dynamic(dynamic_type) => {
                let layout = type_layout(&self.target.info, &CIRType::Dynamic(dynamic_type.clone()));
                let ptr_size_bits = layout.size * 8;
                let align_bits = layout.align * 8;

                let cir_void_ptr_ty = CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void)));
                let data_ptr_ty = self.emit_debug_ty_metadata(&cir_void_ptr_ty);
                let vtable_ptr_ty = self.emit_debug_ty_metadata(&cir_void_ptr_ty);

                unsafe { debug_dynamic_type(&self.dctx, data_ptr_ty, vtable_ptr_ty, ptr_size_bits as u64, align_bits) }
            }
        }
    }

    pub(crate) fn emit_ty(&self, ty: CIRType) -> AnyTypeEnum<'ll> {
        match ty {
            CIRType::Const(inner_ty) => self.emit_ty(*inner_ty),
            CIRType::Plain(plain_ty) => self.emit_plain_ty(plain_ty),
            CIRType::Pointer(_) => self.llvmctx.ptr_type(AddressSpace::default()).as_any_type_enum(),
            CIRType::Struct(struct_type) => self.emit_struct_type(struct_type).as_any_type_enum(),
            CIRType::Enum(enum_type) => self.emit_enum_type(enum_type).as_any_type_enum(),
            CIRType::Union(union_ty) => self.emit_union_ty(union_ty).as_any_type_enum(),
            CIRType::Tuple(tuple_type) => self.emit_tuple_ty(tuple_type).as_any_type_enum(),
            CIRType::Array(array_ty) => self.emit_arr_ty(array_ty).as_any_type_enum(),
            CIRType::FuncType(..) => self.llvmctx.ptr_type(AddressSpace::default()).as_any_type_enum(),
            CIRType::Dynamic(..) => self.emit_dynamic_ty().as_any_type_enum(),
        }
    }

    pub(crate) fn cir_dynamic_ty(&self, data_ptr_inner_ty: CIRType, loc: Loc) -> CIRType {
        CIRType::Struct(CIRStructType {
            name: None,
            fields: vec![
                CIRType::Pointer(Box::new(data_ptr_inner_ty)),
                CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void))),
            ],
            fields_info: vec![
                ("data_ptr".to_string(), loc),
                ("vtable_ptr".to_string(), loc),
            ],
            align: None,
            repr_attr: None,
            loc
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

    pub(crate) fn emit_types(&self, tys: &[CIRType]) -> Vec<AnyTypeEnum<'ll>> {
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

    pub(crate) fn emit_struct_type(&self, struct_type: CIRStructType) -> StructType<'ll> {
        let is_packed = struct_type.is_packed();
        let layout = type_layout(&self.target.info, &CIRType::Struct(struct_type.clone()));

        let mut llvm_field_types: Vec<BasicTypeEnum<'ll>> = Vec::new();
        let mut next_field_index = 0;

        for field_offset in &layout.field_offsets {
            match field_offset {
                ABIFieldOffsetInfo::Normal { .. } => {
                    // get the next actual field from the struct
                    let field_ty = &struct_type.fields[next_field_index];
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
            struct_type.fields.len(),
            "mismatch between layout fields and struct fields"
        );

        self.llvmctx.struct_type(&llvm_field_types, is_packed)
    }

    pub(crate) fn emit_enum_fielded_variant_payload_ty(
        &self,
        variant_idx: usize,
        enum_type: &CIREnumType,
    ) -> Option<StructType<'ll>> {
        if !enum_type.includes_payload() {
            return None;
        }

        let variant = &enum_type.variants[variant_idx];
        let elements = variant.as_fielded()?.clone();
        let tuple_type = CIRTupleType {
            elements,
            loc: enum_type.loc,
        };
        let struct_tuple_type = tuple_type.as_struct_ty();

        Some(self.emit_struct_type(CIRStructType {
            name: None,
            fields: struct_tuple_type.fields,
            fields_info: struct_tuple_type.fields_info,
            repr_attr: None,
            align: None,
            loc: enum_type.loc,
        }))
    }

    pub(crate) fn emit_enum_buffer_payload_ty(&self, enum_type: &CIREnumType) -> (ArrayType<'ll>, u64) {
        let target_data = self.llvmtm.get_target_data();
        let mut max_payload_size: u64 = 0;
        let mut max_payload_align: u64 = 1;

        for variant in &enum_type.variants {
            let (payload_size, payload_align) = match variant {
                CIREnumVariant::Unit(_) => (0, 1),
                CIREnumVariant::Valued(_, expr) => {
                    let llvm_ty: BasicTypeEnum<'ll> = self.emit_ty(expr.ty.clone()).try_into().unwrap();
                    let size = target_data.get_store_size(&llvm_ty);
                    let align = target_data.get_abi_alignment(&llvm_ty) as u64;
                    (size, align)
                }
                CIREnumVariant::Payload(_, field_tys) => {
                    if field_tys.is_empty() {
                        (0, 1)
                    } else {
                        let llvm_fields: Vec<BasicTypeEnum<'ll>> = self
                            .emit_types(field_tys)
                            .iter()
                            .map(|ty| (*ty).try_into().unwrap())
                            .collect();

                        let struct_type = self.llvmctx.struct_type(&llvm_fields, false);
                        let size = target_data.get_store_size(&struct_type);
                        let align = target_data.get_abi_alignment(&struct_type) as u64;
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
            if enum_type.includes_payload() {
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

    fn emit_repr_c_enum_ty(&self, enum_type: &CIREnumType) -> BasicTypeEnum<'ll> {
        let cir_tag_type = enum_type.tag_type_or_infer_or_default();
        self.emit_ty(*cir_tag_type.clone()).try_into().unwrap()
    }

    pub(crate) fn emit_enum_type(&self, enum_type: CIREnumType) -> BasicTypeEnum<'ll> {
        if enum_type.is_scalar_optimizable() {
            // c-compatible enum
            self.emit_repr_c_enum_ty(&enum_type)
        } else {
            // cyrus special enum
            let cir_tag_type = enum_type.tag_type_or_infer_or_default();
            let tag_type: BasicTypeEnum<'ll> = self.emit_ty(*cir_tag_type.clone()).try_into().unwrap();

            let (payload_ty, _) = self.emit_enum_buffer_payload_ty(&enum_type);
            self.llvmctx
                .struct_type(&[tag_type.as_basic_type_enum(), payload_ty.into()], false)
                .as_basic_type_enum()
        }
    }

    pub(crate) fn emit_union_ty(&self, union_ty: CIRUnionType) -> BasicTypeEnum<'ll> {
        let layout = type_layout(&self.target.info, &CIRType::Union(union_ty.clone()));
        let target_data = self.llvmtm.get_target_data();

        let mut ty = None;
        let mut max_align = 0;
        let mut max_size = 0;

        for field_ty in &union_ty.fields {
            let llvm_ty: BasicTypeEnum = self.emit_ty(field_ty.clone()).try_into().unwrap();

            let align = target_data.get_abi_alignment(&llvm_ty);
            let size = target_data.get_store_size(&llvm_ty);

            if align > max_align || (align == max_align && size > max_size) {
                ty = Some(llvm_ty);
                max_align = align;
                max_size = size;
            }
        }

        if max_size < layout.size as u64 {
            let mut fields = vec![ty.unwrap()];

            fields.push(
                self.llvmctx
                    .i8_type()
                    .array_type((layout.size as u64 - max_size) as u32)
                    .into(),
            );

            self.llvmctx.struct_type(&fields, false).into()
        } else {
            ty.unwrap()
        }
    }

    pub(crate) fn emit_tuple_ty(&self, tuple_type: CIRTupleType) -> StructType<'ll> {
        let struct_type = tuple_type.as_struct_ty();
        self.emit_struct_type(struct_type)
    }

    pub(crate) fn emit_arr_ty(&self, array_ty: CIRArrayType) -> AnyTypeEnum<'ll> {
        let elm_ty: BasicTypeEnum<'ll> = self
            .emit_ty(*array_ty.element_type)
            .try_into()
            .expect("Array element must be a valid llvm type.");
        elm_ty.array_type(array_ty.len as u32).as_any_type_enum()
    }

    fn emit_func_ty_params(&self, abi_func_info: &ABIFunctionInfo) -> Vec<LLVMTypeRef> {
        let mut param_types = Vec::new();

        for abi_arg_info in &abi_func_info.params_infos {
            match &abi_arg_info.kind {
                ABIArgKind::DirectPair { lo, hi } => {
                    let lo_type = abi_type_to_llvm_type(self.llvmctx, &self.target.info, lo);
                    let hi_type = abi_type_to_llvm_type(self.llvmctx, &self.target.info, hi);

                    param_types.push(lo_type.as_type_ref());
                    param_types.push(hi_type.as_type_ref());
                }
                ABIArgKind::Direct { coerce_to } => {
                    let param_type = if let Some(coerce_ty) = coerce_to {
                        abi_type_to_llvm_type(self.llvmctx, &self.target.info, coerce_ty)
                    } else {
                        // for direct without coercion, we need to find the type from params_types
                        let i = abi_arg_info.param_index_start as usize;
                        let abi_type = &abi_func_info.params_types[i];
                        abi_type_to_llvm_type(self.llvmctx, &self.target.info, abi_type)
                    };
                    param_types.push(param_type.as_type_ref());
                }
                ABIArgKind::DirectCoerce { ty } => {
                    let param_type = abi_type_to_llvm_type(self.llvmctx, &self.target.info, ty);
                    param_types.push(param_type.as_type_ref());
                }
                ABIArgKind::Indirect { .. } => {
                    let ptr_ty = self.llvmctx.ptr_type(AddressSpace::default());
                    param_types.push(ptr_ty.as_type_ref());
                }
                ABIArgKind::Expand { kind } => match kind {
                    ExpandKind::Coerced { lo, hi, .. } => {
                        let lo_type = abi_type_to_llvm_type(self.llvmctx, &self.target.info, lo);
                        param_types.push(lo_type.as_type_ref());

                        let hi_type = abi_type_to_llvm_type(self.llvmctx, &self.target.info, hi);
                        param_types.push(hi_type.as_type_ref());

                        for i in abi_arg_info.param_index_start..=abi_arg_info.param_index_end {
                            let abi_type = &abi_func_info.params_types[i as usize];
                            let param_type = abi_type_to_llvm_type(self.llvmctx, &self.target.info, abi_type);
                            param_types.push(param_type.as_type_ref());
                        }
                    }
                    ExpandKind::Simple | ExpandKind::Struct { .. } => {
                        for i in abi_arg_info.param_index_start..=abi_arg_info.param_index_end {
                            let abi_type = &abi_func_info.params_types[i as usize];
                            let param_type = abi_type_to_llvm_type(self.llvmctx, &self.target.info, abi_type);
                            param_types.push(param_type.as_type_ref());
                        }
                    }
                },
                ABIArgKind::Extend { .. } => {
                    let i = abi_arg_info.param_index_start as usize;
                    let abi_type = &abi_func_info.params_types[i];
                    let param_type = abi_type_to_llvm_type(self.llvmctx, &self.target.info, abi_type);
                    param_types.push(param_type.as_type_ref());
                }
                ABIArgKind::Ignore => {
                    // skip ignored parameters
                    continue;
                }
            }
        }

        param_types
    }

    pub(crate) fn emit_func_ty(&self, func_ty: CIRFuncType) -> FunctionType<'ll> {
        let abi_func_info = func_ty.abi_func_info.as_ref().unwrap();

        let ret_type = if abi_func_info.ret_info.kind.is_indirect_sret() {
            AnyTypeEnum::VoidType(self.llvmctx.void_type())
        } else {
            abi_type_to_llvm_type(self.llvmctx, &self.target.info, &abi_func_info.ret_info.abi_type)
        };

        let mut param_types = self.emit_func_ty_params(abi_func_info);

        if abi_func_info.ret_info.kind.is_indirect_sret() {
            let ptr_type = self.llvmctx.ptr_type(AddressSpace::default());
            param_types.insert(0, ptr_type.as_type_ref());
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
