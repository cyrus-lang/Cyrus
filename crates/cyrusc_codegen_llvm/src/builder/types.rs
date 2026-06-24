// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::builder::builder::CodeGenIRBuilder;
use crate::llvm::abi::abi_type::abi_type_to_llvm_type;
use crate::llvm::debug_info::{
    debug_array_type, debug_const_type, debug_dynamic_type, debug_enum_type, debug_member_type, debug_pointer_type,
    debug_scalar_enum_type, debug_simple_type, debug_struct_type, debug_union_type,
};
use crate::llvm::dwarf::{DW_ATE_BOOLEAN, DW_ATE_FLOAT, DW_ATE_SIGNED, DW_ATE_UNSIGNED, DW_ATE_UNSIGNED_CHAR};
use cyrusc_internal::abi::args::{ABIArgKind, ABIFunctionInfo, ExpandKind};
use cyrusc_internal::abi::layout::ABIFieldOffsetInfo;
use cyrusc_internal::cir::cir::CIREnumVariant;
use cyrusc_internal::cir::typectx::CIRTypeContextID;
use cyrusc_internal::cir::types::{CIRArrayType, CIREnumType, CIRFuncType, CIRType};
use cyrusc_typed_ast::types::PlainType;
use fx_hash::{FxHashMap, FxHashMapExt};
use inkwell::llvm_sys::prelude::{LLVMMetadataRef, LLVMTypeRef};
use inkwell::{
    AddressSpace,
    llvm_sys::{core::LLVMFunctionType, prelude::LLVMBool},
    types::{AnyType, AnyTypeEnum, ArrayType, AsTypeRef, BasicType, BasicTypeEnum, FunctionType, StructType},
};
use std::cell::RefCell;

impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_debug_type_metadata(&mut self, ty: &CIRType) -> LLVMMetadataRef {
        assert!(self.dctx.is_some());

        let llvm_type = self.emit_type(ty.clone());

        if let Some(meta) = self.dctx.as_ref().unwrap().type_cache.get(&llvm_type.as_type_ref()) {
            return *meta;
        }

        let meta = match ty {
            CIRType::Plain(plain_type) => {
                let name = plain_type.to_string();
                let layout = self.tctx.layout_of(&CIRType::Plain(plain_type.clone()));
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

                unsafe { debug_simple_type(self.dctx.as_ref().unwrap(), &name, bits as u64, encoding as u32) }
            }
            CIRType::Const(inner) => {
                let inner_meta = self.emit_debug_type_metadata(inner);

                unsafe { debug_const_type(self.dctx.as_ref().unwrap(), inner_meta) }
            }
            CIRType::Pointer(inner) => {
                let inner_meta = self.emit_debug_type_metadata(inner);

                let ptr_size_bits = self.target.info.pointer_size() * 8;
                let ptr_align = self.target.info.pointer_align() * 8;

                unsafe {
                    debug_pointer_type(
                        self.dctx.as_ref().unwrap(),
                        inner_meta,
                        ptr_size_bits as u64,
                        ptr_align,
                        "T*",
                    )
                }
            }
            CIRType::Struct(type_id) => {
                // return cached metadata if already emitted
                if let Some(metadata) = self.dctx.as_ref().unwrap().type_cache.get(&llvm_type.as_type_ref()) {
                    return *metadata;
                }

                // insert placeholder to break recursion
                self.dctx
                    .as_mut()
                    .unwrap()
                    .type_cache
                    .insert(llvm_type.as_type_ref(), std::ptr::null_mut());

                let struct_type = self.tctx.get_struct(*type_id);
                let layout = self.tctx.get_or_compute_layout(*type_id);

                let size_bits = layout.size * 8;
                let align_bits = layout.align * 8;

                let mut elements_metadata: Vec<LLVMMetadataRef> = struct_type
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| {
                        let field_meta = self.emit_debug_type_metadata(ty);

                        let offset_bits = layout.lookup_field_offset(i) * 8;

                        let (name, loc) = &struct_type.fields_info[i];

                        unsafe {
                            debug_member_type(
                                self.dctx.as_ref().unwrap(),
                                &name,
                                field_meta,
                                offset_bits as u64,
                                loc.line as u32,
                            )
                        }
                    })
                    .collect();

                let struct_name = struct_type.name.clone().unwrap_or("<unnamed_struct>".to_string());

                let meta = unsafe {
                    debug_struct_type(
                        self.dctx.as_ref().unwrap(),
                        &struct_name,
                        &mut elements_metadata,
                        size_bits as u64,
                        align_bits,
                        struct_type.loc.line.try_into().unwrap(),
                    )
                };

                // replace placeholder with real metadata
                self.dctx
                    .as_mut()
                    .unwrap()
                    .type_cache
                    .insert(llvm_type.as_type_ref(), meta);

                meta
            }
            CIRType::Enum(type_id) => {
                // return cached metadata if already emitted
                if let Some(metadata) = self.dctx.as_ref().unwrap().type_cache.get(&llvm_type.as_type_ref()) {
                    return *metadata;
                }

                // insert placeholder to break recursion
                self.dctx
                    .as_mut()
                    .unwrap()
                    .type_cache
                    .insert(llvm_type.as_type_ref(), std::ptr::null_mut());

                let enum_type = self.tctx.get_enum(*type_id);
                let layout = self.tctx.get_or_compute_layout(*type_id);

                let size_bits = layout.size * 8;
                let align_bits = layout.align * 8;

                let enum_name = enum_type.name.clone().unwrap_or("<unnamed_enum>".to_string());

                let tag_meta = self.emit_debug_type_metadata(&enum_type.tag_type_or_infer_or_default());

                if enum_type.is_scalar_optimizable() {
                    let variants: Vec<(String, i64)> = enum_type
                        .variants
                        .iter()
                        .map(|variant| match variant {
                            CIREnumVariant::Unit(ident, tag) => (ident.clone(), *tag as i64),
                            CIREnumVariant::Valued(ident, _, tag) => (ident.clone(), *tag as i64),
                            CIREnumVariant::Payload(..) => unreachable!(),
                        })
                        .collect();

                    unsafe {
                        debug_scalar_enum_type(
                            self.dctx.as_ref().unwrap(),
                            &enum_name,
                            &variants,
                            size_bits as u64,
                            align_bits,
                            enum_type.loc.line as u32,
                            tag_meta,
                        )
                    }
                } else {
                    let variants: Vec<(String, i64, LLVMMetadataRef)> = enum_type
                        .variants
                        .iter()
                        .map(|variant| {
                            let ident = variant.ident();

                            match variant {
                                CIREnumVariant::Unit(_, tag) => {
                                    (ident.clone(), *tag as i64, std::ptr::null_mut() as LLVMMetadataRef)
                                }
                                CIREnumVariant::Valued(_, _, tag) => {
                                    (ident.clone(), *tag as i64, std::ptr::null_mut() as LLVMMetadataRef)
                                }
                                CIREnumVariant::Payload(_, struct_type, tag) => {
                                    let type_id = self.tctx.insert_struct(struct_type.clone());

                                    let tuple_meta = self.emit_debug_type_metadata(&CIRType::Struct(type_id));

                                    (ident.clone(), *tag as i64, tuple_meta)
                                }
                            }
                        })
                        .collect();

                    let meta = unsafe {
                        debug_enum_type(
                            self.dctx.as_ref().unwrap(),
                            &enum_name,
                            enum_type.loc.line as u32,
                            tag_meta,
                            &variants,
                            size_bits as u64,
                            align_bits,
                        )
                    };

                    // replace placeholder with real metadata
                    self.dctx
                        .as_mut()
                        .unwrap()
                        .type_cache
                        .insert(llvm_type.as_type_ref(), meta);

                    meta
                }
            }
            CIRType::Union(type_id) => {
                // return cached metadata if already emitted
                if let Some(metadata) = self.dctx.as_ref().unwrap().type_cache.get(&llvm_type.as_type_ref()) {
                    return *metadata;
                }

                // insert placeholder to break recursion
                self.dctx
                    .as_mut()
                    .unwrap()
                    .type_cache
                    .insert(llvm_type.as_type_ref(), std::ptr::null_mut());

                let union_type = self.tctx.get_union(*type_id);
                let layout = self.tctx.get_or_compute_layout(*type_id);

                let mut elements_metadata: Vec<LLVMMetadataRef> = union_type
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| {
                        let field_meta = self.emit_debug_type_metadata(ty);

                        let offset_bits = layout.lookup_field_offset(i) * 8;

                        let (name, loc) = &union_type.fields_info[i];

                        unsafe {
                            debug_member_type(
                                self.dctx.as_ref().unwrap(),
                                &name,
                                field_meta,
                                offset_bits as u64,
                                loc.line as u32,
                            )
                        }
                    })
                    .collect();

                let union_name = union_type.name.clone().unwrap_or("<unnamed_union>".to_string());

                let meta = unsafe {
                    debug_union_type(
                        self.dctx.as_ref().unwrap(),
                        &union_name,
                        &mut elements_metadata,
                        layout.size as u64,
                        layout.align,
                        union_type.loc.line.try_into().unwrap(),
                    )
                };

                // replace placeholder with real metadata
                self.dctx
                    .as_mut()
                    .unwrap()
                    .type_cache
                    .insert(llvm_type.as_type_ref(), meta);

                meta
            }
            CIRType::FuncType(func_type) => {
                let subroutine_type = self.emit_func_meta(func_type);

                let ptr_size_bits = self.target.info.pointer_size() * 8;
                let ptr_align = self.target.info.pointer_align() * 8;

                unsafe {
                    debug_pointer_type(
                        self.dctx.as_ref().unwrap(),
                        subroutine_type,
                        ptr_size_bits as u64,
                        ptr_align,
                        "T*",
                    )
                }
            }
            CIRType::Array(array_type) => {
                let element_meta = self.emit_debug_type_metadata(&array_type.element_type);

                let layout = self.tctx.layout_of(&CIRType::Array(array_type.clone()));

                unsafe {
                    debug_array_type(
                        self.dctx.as_ref().unwrap(),
                        element_meta,
                        array_type.len as u64,
                        layout.size as u64,
                        layout.align,
                    )
                }
            }
            CIRType::Dynamic(_) => {
                let layout = self.tctx.layout_of(ty);

                let ptr_size_bits = layout.size * 8;
                let align_bits = layout.align * 8;

                let cir_void_ptr_ty = CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void)));
                let data_ptr_meta = self.emit_debug_type_metadata(&cir_void_ptr_ty);
                let vtable_ptr_meta = self.emit_debug_type_metadata(&cir_void_ptr_ty);

                unsafe {
                    debug_dynamic_type(
                        self.dctx.as_ref().unwrap(),
                        data_ptr_meta,
                        vtable_ptr_meta,
                        ptr_size_bits as u64,
                        align_bits,
                    )
                }
            }
        };

        // store real metadata
        self.dctx
            .as_mut()
            .unwrap()
            .type_cache
            .insert(llvm_type.as_type_ref(), meta);

        meta
    }

    pub(crate) fn emit_type(&self, ty: CIRType) -> AnyTypeEnum<'ll> {
        match ty {
            CIRType::Struct(type_id) => self.emit_struct_type(type_id).as_any_type_enum(),
            CIRType::Enum(type_id) => self.emit_enum_type(type_id).as_any_type_enum(),
            CIRType::Union(type_id) => self.emit_union_type(type_id).as_any_type_enum(),
            CIRType::Const(inner_ty) => self.emit_type(*inner_ty),
            CIRType::Plain(plain_ty) => self.emit_plain_type(plain_ty),
            CIRType::Pointer(_) => self.llvm_ctx.ptr_type(AddressSpace::default()).as_any_type_enum(),
            CIRType::Array(array_ty) => self.emit_array_type(array_ty).as_any_type_enum(),
            CIRType::FuncType(..) => self.llvm_ctx.ptr_type(AddressSpace::default()).as_any_type_enum(),
            CIRType::Dynamic(..) => self.emit_dynamic_type().as_any_type_enum(),
        }
    }

    pub(crate) fn emit_dynamic_type(&self) -> StructType<'ll> {
        let vtable_ptr = self.llvm_ctx.ptr_type(AddressSpace::default()).as_basic_type_enum();
        let data_ptr = self.llvm_ctx.ptr_type(AddressSpace::default()).as_basic_type_enum();

        self.llvm_ctx.struct_type(&[data_ptr, vtable_ptr], false)
    }

    pub(crate) fn emit_types(&self, types: &[CIRType]) -> Vec<AnyTypeEnum<'ll>> {
        types.iter().map(|ty| self.emit_type(ty.clone())).collect()
    }

    pub(crate) fn emit_plain_type(&self, plain_type: PlainType) -> AnyTypeEnum<'ll> {
        let ctx = &self.llvm_ctx;

        match plain_type {
            // Platform dependent types
            PlainType::UIntPtr | PlainType::IntPtr | PlainType::USize | PlainType::ISize => ctx
                .ptr_sized_int_type(&self.llvmtm.get_target_data(), None)
                .as_any_type_enum(),

            PlainType::Int8 => ctx.i8_type().as_any_type_enum(),
            PlainType::Int16 => ctx.i16_type().as_any_type_enum(),
            PlainType::Int32 => ctx.i32_type().as_any_type_enum(),
            PlainType::Int64 => ctx.i64_type().as_any_type_enum(),
            PlainType::Int128 => ctx.i128_type().as_any_type_enum(),
            PlainType::UInt8 => ctx.i8_type().as_any_type_enum(),
            PlainType::UInt16 => ctx.i16_type().as_any_type_enum(),
            PlainType::UInt32 => ctx.i32_type().as_any_type_enum(),
            PlainType::UInt64 => ctx.i64_type().as_any_type_enum(),
            PlainType::UInt128 => ctx.i128_type().as_any_type_enum(),
            PlainType::Int => ctx.i32_type().as_any_type_enum(),
            PlainType::UInt => ctx.i32_type().as_any_type_enum(),
            PlainType::Float16 => ctx.f16_type().as_any_type_enum(),
            PlainType::Float32 => ctx.f32_type().as_any_type_enum(),
            PlainType::Float64 => ctx.f64_type().as_any_type_enum(),
            PlainType::Float128 => ctx.f128_type().as_any_type_enum(),
            PlainType::Char => ctx.i8_type().as_any_type_enum(),
            PlainType::Void => ctx.void_type().as_any_type_enum(),
            PlainType::Null => ctx.ptr_type(AddressSpace::default()).as_any_type_enum(),
            PlainType::Bool => {
                // Booleans are stored as i8 in memory for stable layout and ABI compatibility.
                // i1 is reserved for logical operations only.
                ctx.i8_type().as_any_type_enum()
            }
        }
    }

    pub(crate) fn emit_struct_type(&self, type_id: CIRTypeContextID) -> StructType<'ll> {
        if let Some(cached_struct_type) = self.type_cache.get_struct(type_id) {
            return cached_struct_type;
        }

        let struct_type = self.tctx.get_struct(type_id);
        let name = struct_type.name.as_deref().unwrap_or(".anon");
        let llvm_struct_type = self.llvm_ctx.opaque_struct_type(name);
        self.type_cache.insert_struct(type_id, llvm_struct_type);

        let layout = self.tctx.get_or_compute_layout(type_id);
        let is_packed = struct_type.is_packed();

        let mut llvm_field_types: Vec<BasicTypeEnum<'ll>> = Vec::new();
        let mut next_field_index = 0;

        for field_offset in &layout.field_offsets {
            match field_offset {
                ABIFieldOffsetInfo::Normal { .. } => {
                    let field_type = &struct_type.fields[next_field_index];
                    let llvm_ty: BasicTypeEnum<'ll> = self.emit_type(field_type.clone()).try_into().unwrap();
                    llvm_field_types.push(llvm_ty);
                    next_field_index += 1;
                }
                ABIFieldOffsetInfo::Padding { size, .. } => {
                    let padding_array = self.llvm_ctx.i8_type().array_type(*size);
                    llvm_field_types.push(padding_array.as_basic_type_enum());
                }
            }
        }

        assert_eq!(
            next_field_index,
            struct_type.fields.len(),
            "mismatch between layout fields and struct fields"
        );

        llvm_struct_type.set_body(&llvm_field_types, is_packed);
        llvm_struct_type
    }

    pub(crate) fn emit_enum_type(&self, type_id: CIRTypeContextID) -> BasicTypeEnum<'ll> {
        if let Some(cached_basic_type) = self.type_cache.get_enum(type_id) {
            return cached_basic_type;
        }

        let enum_type = self.tctx.get_enum(type_id);

        if enum_type.is_scalar_optimizable() {
            // c-compatible enum
            let llvm_basic_type = self.emit_repr_c_enum_ty(&enum_type);
            self.type_cache.insert_enum(type_id, llvm_basic_type);
            return llvm_basic_type;
        } else {
            // cyrus special enum
            let name = enum_type.name.as_deref().unwrap_or(".anon");
            let llvm_struct_type = self.llvm_ctx.opaque_struct_type(name);
            self.type_cache
                .insert_enum(type_id, llvm_struct_type.as_basic_type_enum());

            let cir_tag_type = enum_type.tag_type_or_infer_or_default();
            let tag_type: BasicTypeEnum<'ll> = self.emit_type(*cir_tag_type.clone()).try_into().unwrap();
            let (payload_type, _) = self.emit_enum_buffer_payload_type(&enum_type);

            llvm_struct_type.set_body(&[tag_type.as_basic_type_enum(), payload_type.into()], false);
            llvm_struct_type.as_basic_type_enum()
        }
    }

    pub(crate) fn emit_union_type(&self, type_id: CIRTypeContextID) -> BasicTypeEnum<'ll> {
        if let Some(cached) = self.type_cache.get_union(type_id) {
            return cached.as_basic_type_enum();
        }

        let union_type = self.tctx.get_union(type_id);
        let name = union_type.name.as_deref().unwrap_or(".anon");
        let llvm_struct = self.llvm_ctx.opaque_struct_type(name);
        self.type_cache.insert_union(type_id, llvm_struct);

        let layout = self.tctx.get_or_compute_layout(type_id);
        let target_data = self.llvmtm.get_target_data();

        let mut ty = None;
        let mut max_align = 0;
        let mut max_size = 0;

        for field_type in &union_type.fields {
            let llvm_ty: BasicTypeEnum = self.emit_type(field_type.clone()).try_into().unwrap();
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
                self.llvm_ctx
                    .i8_type()
                    .array_type((layout.size as u64 - max_size) as u32)
                    .into(),
            );
            llvm_struct.set_body(&fields, false);
        } else {
            llvm_struct.set_body(&[ty.unwrap()], false);
        }

        llvm_struct.as_basic_type_enum()
    }

    pub(crate) fn emit_enum_fielded_variant_payload_type(
        &self,
        variant_idx: usize,
        enum_type: &CIREnumType,
    ) -> Option<StructType<'ll>> {
        if !enum_type.includes_payload() {
            return None;
        }

        let variant = &enum_type.variants[variant_idx];

        let struct_type = variant.as_payload()?.clone();

        let type_id = self.tctx.insert_struct(struct_type);

        Some(self.emit_struct_type(type_id))
    }

    pub(crate) fn emit_enum_buffer_payload_type(&self, enum_type: &CIREnumType) -> (ArrayType<'ll>, u64) {
        let target_data = self.llvmtm.get_target_data();
        let mut max_payload_size: u64 = 0;
        let mut max_payload_align: u64 = 1;

        for variant in &enum_type.variants {
            let (payload_size, payload_align) = match variant {
                CIREnumVariant::Unit(_, _) => (0, 1),
                CIREnumVariant::Valued(_, value_type, _) => {
                    let llvm_ty: BasicTypeEnum<'ll> = self.emit_type(value_type.clone()).try_into().unwrap();
                    let size = target_data.get_store_size(&llvm_ty);
                    let align = target_data.get_abi_alignment(&llvm_ty) as u64;
                    (size, align)
                }
                CIREnumVariant::Payload(_, struct_type, _) => {
                    if struct_type.fields.is_empty() {
                        (0, 1)
                    } else {
                        let llvm_fields: Vec<BasicTypeEnum<'ll>> = self
                            .emit_types(&struct_type.fields)
                            .iter()
                            .map(|ty| (*ty).try_into().unwrap())
                            .collect();

                        let struct_type = self.llvm_ctx.struct_type(&llvm_fields, false);
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

        let payload_buffer_ty = self.llvm_ctx.i8_type().array_type(aligned_size as u32);
        (payload_buffer_ty, aligned_size)
    }

    fn emit_repr_c_enum_ty(&self, enum_type: &CIREnumType) -> BasicTypeEnum<'ll> {
        let cir_tag_type = enum_type.tag_type_or_infer_or_default();
        self.emit_type(*cir_tag_type.clone()).try_into().unwrap()
    }

    pub(crate) fn emit_array_type(&self, array_ty: CIRArrayType) -> AnyTypeEnum<'ll> {
        let elm_ty: BasicTypeEnum<'ll> = self
            .emit_type(*array_ty.element_type)
            .try_into()
            .expect("Array element must be a valid llvm type.");
        elm_ty.array_type(array_ty.len as u32).as_any_type_enum()
    }

    fn emit_func_ty_params(&self, abi_func_info: &ABIFunctionInfo) -> Vec<LLVMTypeRef> {
        let mut param_types = Vec::new();

        for abi_arg_info in &abi_func_info.params_infos {
            match &abi_arg_info.kind {
                ABIArgKind::DirectPair { lo, hi } => {
                    let lo_type = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, lo);
                    let hi_type = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, hi);

                    param_types.push(lo_type.as_type_ref());
                    param_types.push(hi_type.as_type_ref());
                }
                ABIArgKind::Direct { coerce_to } => {
                    let param_type = if let Some(coerce_ty) = coerce_to {
                        abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, coerce_ty)
                    } else {
                        // for direct without coercion, we need to find the type from params_types
                        let i = abi_arg_info.param_index_start as usize;
                        let abi_type = &abi_func_info.params_types[i];
                        abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, abi_type)
                    };
                    param_types.push(param_type.as_type_ref());
                }
                ABIArgKind::DirectCoerce { ty } => {
                    let param_type = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, ty);
                    param_types.push(param_type.as_type_ref());
                }
                ABIArgKind::Indirect { .. } => {
                    let ptr_ty = self.llvm_ctx.ptr_type(AddressSpace::default());
                    param_types.push(ptr_ty.as_type_ref());
                }
                ABIArgKind::Expand { kind } => match kind {
                    ExpandKind::Coerced { lo, hi, .. } => {
                        let lo_type = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, lo);
                        param_types.push(lo_type.as_type_ref());

                        let hi_type = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, hi);
                        param_types.push(hi_type.as_type_ref());

                        for i in abi_arg_info.param_index_start..=abi_arg_info.param_index_end {
                            let abi_type = &abi_func_info.params_types[i as usize];
                            let param_type = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, abi_type);
                            param_types.push(param_type.as_type_ref());
                        }
                    }
                    ExpandKind::Simple | ExpandKind::Struct { .. } => {
                        for i in abi_arg_info.param_index_start..=abi_arg_info.param_index_end {
                            let abi_type = &abi_func_info.params_types[i as usize];
                            let param_type = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, abi_type);
                            param_types.push(param_type.as_type_ref());
                        }
                    }
                },
                ABIArgKind::Extend { .. } => {
                    let i = abi_arg_info.param_index_start as usize;
                    let abi_type = &abi_func_info.params_types[i];
                    let param_type = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, abi_type);
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

    pub(crate) fn emit_func_type(&self, func_ty: CIRFuncType) -> FunctionType<'ll> {
        let abi_func_info = func_ty.abi_func_info.as_ref().unwrap();

        let ret_type = if abi_func_info.ret_info.kind.is_indirect_sret() {
            AnyTypeEnum::VoidType(self.llvm_ctx.void_type())
        } else if abi_func_info.ret_info.kind.is_ignore() {
            AnyTypeEnum::VoidType(self.llvm_ctx.void_type())
        } else {
            abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, &abi_func_info.ret_info.abi_type)
        };

        let mut param_types = self.emit_func_ty_params(abi_func_info);

        if abi_func_info.ret_info.kind.is_indirect_sret() {
            let ptr_type = self.llvm_ctx.ptr_type(AddressSpace::default());
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

pub struct CodegenIRBuilderTypeCache<'ll> {
    struct_cache: RefCell<FxHashMap<CIRTypeContextID, StructType<'ll>>>,
    union_cache: RefCell<FxHashMap<CIRTypeContextID, StructType<'ll>>>,
    enum_cache: RefCell<FxHashMap<CIRTypeContextID, BasicTypeEnum<'ll>>>,
}

impl<'ll> CodegenIRBuilderTypeCache<'ll> {
    pub fn new() -> Self {
        Self {
            struct_cache: RefCell::new(FxHashMap::new()),
            union_cache: RefCell::new(FxHashMap::new()),
            enum_cache: RefCell::new(FxHashMap::new()),
        }
    }

    pub fn get_struct(&self, id: CIRTypeContextID) -> Option<StructType<'ll>> {
        self.struct_cache.borrow().get(&id).copied()
    }

    pub fn insert_struct(&self, id: CIRTypeContextID, ty: StructType<'ll>) {
        self.struct_cache.borrow_mut().insert(id, ty);
    }

    pub fn get_union(&self, id: CIRTypeContextID) -> Option<StructType<'ll>> {
        self.union_cache.borrow().get(&id).copied()
    }

    pub fn insert_union(&self, id: CIRTypeContextID, ty: StructType<'ll>) {
        self.union_cache.borrow_mut().insert(id, ty);
    }

    pub fn get_enum(&self, id: CIRTypeContextID) -> Option<BasicTypeEnum<'ll>> {
        self.enum_cache.borrow().get(&id).copied()
    }

    pub fn insert_enum(&self, id: CIRTypeContextID, ty: BasicTypeEnum<'ll>) {
        self.enum_cache.borrow_mut().insert(id, ty);
    }
}
