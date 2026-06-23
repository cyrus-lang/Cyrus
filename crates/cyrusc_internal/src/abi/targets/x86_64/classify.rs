// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    abi::{
        args::{ABIArgAttrs, ABIArgInfo, ABIArgKind, ABIFunctionInfo, ABIRetInfo, ABIRetInfoKind, ExpandKind},
        helpers::{Registers, cir_type_to_abi_type, is_cir_type_abi_aggregate},
        target::{ABITargetInfo, ABITargetOS, RegisterClass, TargetABI},
        types::{ABIFloatKind, ABIType},
    },
    cir::{
        typectx::CIRTypeContext,
        types::{CIRArrayType, CIRFuncType, CIRStructType, CIRType, CIRUnionType},
    },
    is_integer_type,
};
use cyrusc_ast::abi::CallConv;
use cyrusc_typed_ast::types::PlainType;
use std::sync::Arc;

const MIN_ABI_STACK_ALIGN: u32 = 16;

pub struct X86_64 {
    info: ABITargetInfo,
    tctx: Arc<CIRTypeContext>,
}

impl X86_64 {
    pub fn new(info: ABITargetInfo, tctx: Arc<CIRTypeContext>) -> Self {
        Self { info, tctx }
    }

    fn classify_parameter(&self, ty: &CIRType, available_regs: &mut Registers, is_named: bool) -> ABIArgInfo {
        let (abi_arg_info, needed_regs) = self.classify_argument(ty, available_regs.int_regs, is_named);

        if try_use_registers(available_regs, &needed_regs) {
            abi_arg_info
        } else {
            self.abi_arg_info_indirect_result(ty, available_regs.int_regs)
        }
    }

    fn abi_arg_info_indirect_result(&self, ty: &CIRType, free_int_regs: u32) -> ABIArgInfo {
        // if this is a scalar LLVM value then assume LLVM will pass it in the right place naturally
        if !is_cir_type_abi_aggregate(ty) {
            if ty.is_integer_or_bool() {
                let abi_ty = cir_type_to_abi_type(self.tctx.clone(), ty);
                let is_signed = ty.is_signed_integer();

                return ABIArgInfo {
                    kind: ABIArgKind::DirectCoerce { ty: abi_ty },
                    attrs: ABIArgAttrs {
                        zero_ext: !is_signed,
                        sign_ext: is_signed,
                        ..Default::default()
                    },
                    param_index_start: 0,
                    param_index_end: 0,
                };
            }

            // no change, just put it on the stack
            return ABIArgInfo::direct();
        }

        // byval alignment
        let layout = self.tctx.layout_of(ty);
        let align = layout.align;
        let size = layout.size;

        // pass as arguments if there are no more free int regs
        if free_int_regs == 0 {
            if align <= 8 && size <= 8 {
                return ABIArgInfo {
                    kind: ABIArgKind::DirectCoerce {
                        ty: ABIType::Integer(64),
                    },
                    attrs: ABIArgAttrs::default(),
                    param_index_start: 0,
                    param_index_end: 0,
                };
            }
        }

        if align < 8 {
            // realigned indirect (with specified alignment)
            let abi_type = cir_type_to_abi_type(self.tctx.clone(), ty);

            ABIArgInfo {
                kind: ABIArgKind::Indirect { align: 8, ty: abi_type },
                attrs: ABIArgAttrs {
                    by_val: true,
                    ..Default::default()
                },
                param_index_start: 0,
                param_index_end: 0,
            }
        } else {
            // regular byval indirect
            let abi_type = cir_type_to_abi_type(self.tctx.clone(), ty);

            ABIArgInfo {
                kind: ABIArgKind::Indirect { align, ty: abi_type },
                attrs: ABIArgAttrs {
                    by_val: true,
                    ..Default::default()
                },
                param_index_start: 0,
                param_index_end: 0,
            }
        }
    }

    // https://github.com/llvm/llvm-project/blob/a08cc6e0d5e3fa653649a7826f1ffafc2b3ea2dd/clang/lib/CodeGen/Targets/X86.cpp#L2486
    fn get_int_type_at_offset(&self, ty: &CIRType, offset: u32, source_type: &CIRType, source_offset: u32) -> ABIType {
        match ty {
            CIRType::Plain(plain_type) => match plain_type {
                PlainType::UIntPtr
                | PlainType::IntPtr
                | PlainType::ISize
                | PlainType::USize
                | PlainType::Int64
                | PlainType::UInt64 => {
                    if offset != 0 {
                        return cir_type_to_abi_type(self.tctx.clone(), ty);
                    }
                }

                PlainType::Bool
                | PlainType::Char
                | PlainType::UInt8
                | PlainType::Int8
                | PlainType::UInt16
                | PlainType::Int16
                | PlainType::UInt32
                | PlainType::Int32
                | PlainType::UInt
                | PlainType::Int => {
                    if offset != 0 {
                        // fallback
                    } else {
                        let layout = self.tctx.layout_of(ty);

                        if self.bits_contain_no_user_data(source_type, source_offset + layout.size, source_offset + 8) {
                            return cir_type_to_abi_type(self.tctx.clone(), ty);
                        }
                    }
                }

                PlainType::UInt128 | PlainType::Int128 => {
                    // fallback
                }

                PlainType::Float16 | PlainType::Float32 | PlainType::Float64 | PlainType::Float128 => {
                    // fallback
                }

                PlainType::Null | PlainType::Void => unreachable!(),
            },
            CIRType::Const(ty) => {
                return self.get_int_type_at_offset(ty, offset, source_type, source_offset);
            }
            CIRType::Pointer(_) | CIRType::FuncType(_) => {
                if offset == 0 {
                    return cir_type_to_abi_type(self.tctx.clone(), ty);
                }
            }
            CIRType::Struct(type_id) => {
                let struct_type = self.tctx.get_struct(*type_id);

                if let Some((field_type, field_offset)) = self.get_struct_member_at_offset(&struct_type, offset) {
                    return self.get_int_type_at_offset(&field_type, offset - field_offset, source_type, source_offset);
                }
            }
            CIRType::Array(array_type) => {
                let element_ty = &array_type.element_type;
                let element_layout = self.tctx.layout_of(element_ty);
                let element_offset = (offset / element_layout.size) * element_layout.size;
                return self.get_int_type_at_offset(element_ty, offset - element_offset, source_type, source_offset);
            }
            CIRType::Dynamic(_) => {
                if offset < 8 {
                    return ABIType::Pointer; // data_ptr (first 8 bytes)
                }
                if offset < 16 {
                    return ABIType::Pointer; // vtable_ptr (next 8 bytes)
                }
            }
            CIRType::Union(_) | CIRType::Enum(_) => {
                // fallback
            }
        }

        let layout = self.tctx.layout_of(source_type);
        assert!(layout.size != source_offset);

        let remaining_bytes = layout.size - source_offset;

        if remaining_bytes > 8 {
            // fits in eightbyte register
            ABIType::Integer(64)
        } else {
            // span across multiple registers
            ABIType::Integer((layout.size - source_offset) * 8)
        }
    }

    fn get_union_member_at_offset(&self, union_type: &CIRUnionType, offset: u32) -> Option<(CIRType, u32)> {
        let layout = self.tctx.compute_union_layout(union_type);

        if layout.size < offset {
            return None;
        }

        for field_type in &union_type.fields {
            let field_layout = self.tctx.layout_of(field_type);

            if offset < field_layout.size {
                return Some((field_type.clone(), 0));
            }
        }

        None
    }

    fn get_struct_member_at_offset(&self, struct_type: &CIRStructType, offset: u32) -> Option<(CIRType, u32)> {
        let layout = self.tctx.compute_struct_layout(struct_type);

        if layout.size < offset {
            return None;
        }

        let mut current_offset = 0;

        let fields = &struct_type.fields;

        for field_type in fields {
            let field_layout = self.tctx.layout_of(&field_type);
            let align = field_layout.align;

            let padding = (align - (current_offset % align)) % align;
            current_offset += padding;

            if current_offset > offset {
                break;
            }

            if current_offset <= offset && offset < current_offset + field_layout.size {
                return Some((field_type.clone(), current_offset));
            }

            current_offset += field_layout.size;
        }

        None
    }

    fn get_fp_type_at_offset(&self, ty: &CIRType, offset: u32) -> Option<ABIType> {
        if offset == 0 && ty.is_float() {
            return Some(cir_type_to_abi_type(self.tctx.clone(), ty));
        }

        if let CIRType::Struct(id) = ty {
            let struct_type = self.tctx.get_struct(*id);
            if let Some((field_type, field_offset)) = self.get_struct_member_at_offset(&struct_type, offset) {
                return self.get_fp_type_at_offset(&field_type, offset - field_offset);
            }
        }

        if let CIRType::Union(id) = ty {
            let union_type = self.tctx.get_union(*id);
            if let Some((field_type, field_offset)) = self.get_union_member_at_offset(&union_type, offset) {
                return self.get_fp_type_at_offset(&field_type, offset - field_offset);
            }
        }

        if let Some(array_type) = ty.as_array() {
            let element_type = &array_type.element_type;
            let element_layout = self.tctx.layout_of(&element_type);
            let element_start = (offset / element_layout.size) * element_layout.size;
            let element_offset = offset - element_start;
            return self.get_fp_type_at_offset(&element_type, element_offset);
        }

        None
    }

    fn get_sse_type_at_offset(
        &self,
        ty: &CIRType,
        offset: u32,
        source_type: &CIRType,
        source_offset: u32,
    ) -> Option<ABIType> {
        let abi_float_type = self.get_fp_type_at_offset(ty, offset)?;

        let float_kind = match abi_float_type {
            ABIType::Float(kind) => kind,
            _ => return Some(ABIType::Float(ABIFloatKind::F64)), // fallback
        };

        if float_kind == ABIFloatKind::F64 {
            return Some(ABIType::Float(ABIFloatKind::F64));
        }

        let source_layout = self.tctx.layout_of(source_type);
        let source_size = source_layout.size - source_offset;
        let float_size = self.float_size(&float_kind);

        let mut float_type2 = if source_size > float_size {
            self.get_fp_type_at_offset(ty, offset + float_size)
        } else {
            None
        };

        if float_type2.is_none() {
            if float_kind == ABIFloatKind::F16 && source_size > 4 {
                float_type2 = self.get_fp_type_at_offset(ty, offset + 4);
            }

            if float_type2.is_none() {
                return Some(ABIType::Float(float_kind));
            }
        }

        let float_kind2 = match float_type2.unwrap() {
            ABIType::Float(kind) => kind,
            _ => return Some(ABIType::Float(ABIFloatKind::F64)),
        };

        if float_kind == ABIFloatKind::F32 && float_kind2 == ABIFloatKind::F32 {
            return Some(ABIType::Vector {
                element_ty: Box::new(ABIType::Float(ABIFloatKind::F32)),
                lanes: 2,
            });
        }

        if float_kind == ABIFloatKind::F16 && float_kind2 == ABIFloatKind::F16 {
            let has_following_float = source_size > 4 && self.get_fp_type_at_offset(ty, offset + 4).is_some();

            return Some(ABIType::Vector {
                element_ty: Box::new(ABIType::Float(ABIFloatKind::F16)),
                lanes: if has_following_float { 4 } else { 2 },
            });
        }

        if float_kind == ABIFloatKind::F16 || float_kind2 == ABIFloatKind::F16 {
            return Some(ABIType::Vector {
                element_ty: Box::new(ABIType::Float(ABIFloatKind::F16)),
                lanes: 4,
            });
        }

        Some(ABIType::Float(ABIFloatKind::F64))
    }

    // https://github.com/llvm/llvm-project/blob/a08cc6e0d5e3fa653649a7826f1ffafc2b3ea2dd/clang/lib/CodeGen/Targets/X86.cpp#L2321
    fn bits_contain_no_user_data(&self, ty: &CIRType, start: u32, end: u32) -> bool {
        let check_for_struct_or_union = |fields: &Vec<CIRType>, is_union: bool| {
            let mut current_offset = 0;

            for field_type in fields {
                let field_layout = self.tctx.layout_of(field_type);
                let align = field_layout.align;

                // add padding before field (for structs, unions don't have padding)
                if !is_union {
                    let padding = (align - (current_offset % align)) % align;
                    current_offset += padding;
                }

                let field_offset = current_offset;

                if field_offset >= end {
                    break;
                }

                let field_start = if field_offset < start { start - field_offset } else { 0 };

                if !self.bits_contain_no_user_data(field_type, field_start, end - field_offset) {
                    return false;
                }

                current_offset += field_layout.size;
            }

            true
        };
        let layout = self.tctx.layout_of(ty);

        // if the bytes being queried are off the end of the type, there is no user data
        if layout.size <= start {
            return true;
        }

        if let Some(array_type) = ty.as_array() {
            let element_ty = &array_type.element_type;
            let element_layout = self.tctx.layout_of(element_ty);
            let element_size = element_layout.size;

            for i in 0..array_type.len {
                let offset = (i as u32) * element_size;

                // if the field is after the span we care about, then we're done
                if offset >= end {
                    break;
                }

                let element_start = if offset < start { start - offset } else { 0 };

                if !self.bits_contain_no_user_data(element_ty, element_start, end - offset) {
                    return false;
                }
            }

            // no overlap found
            true
        } else if let Some(struct_type) = ty.as_struct(&self.tctx) {
            check_for_struct_or_union(&struct_type.fields, false)
        } else if let Some(union_ty) = ty.as_union(&self.tctx) {
            check_for_struct_or_union(&union_ty.fields, true)
        } else {
            false
        }
    }

    fn float_size(&self, float_kind: &ABIFloatKind) -> u32 {
        match float_kind {
            ABIFloatKind::F16 => 2,
            ABIFloatKind::F32 => 4,
            ABIFloatKind::F64 => 8,
            ABIFloatKind::F128 => 16,
        }
    }

    fn param_types_from_arg_info(&self, abi_arg: &ABIArgInfo, param_type: &CIRType) -> Vec<ABIType> {
        let mut types = Vec::new();

        match &abi_arg.kind {
            ABIArgKind::Direct { coerce_to } => {
                if let Some(ty) = coerce_to {
                    types.push(ty.clone());
                } else {
                    types.push(cir_type_to_abi_type(self.tctx.clone(), param_type));
                }
            }
            ABIArgKind::DirectCoerce { ty } => {
                types.push(ty.clone());
            }
            ABIArgKind::DirectPair { lo, hi } => {
                types.push(lo.clone());
                types.push(hi.clone());
            }
            ABIArgKind::Indirect { ty, .. } => {
                types.push(ty.clone());
            }
            ABIArgKind::Extend { .. } => {
                types.push(cir_type_to_abi_type(self.tctx.clone(), param_type));
            }
            ABIArgKind::Expand { kind } => match kind {
                ExpandKind::Struct { .. } => {
                    assert!(param_type.is_struct() || param_type.is_union());
                    let fields = param_type.struct_or_union_fields(&self.tctx).unwrap();

                    for field in fields {
                        types.push(cir_type_to_abi_type(self.tctx.clone(), &field));
                    }
                }
                _ => return vec![cir_type_to_abi_type(self.tctx.clone(), param_type)],
            },
            ABIArgKind::Ignore => {
                // skip
            }
        }

        types
    }
}

impl X86_64 {
    fn classify_func_naked(&self, cir_func_type: &CIRFuncType) -> ABIFunctionInfo {
        // naked functions just pass arguments as-is, NO ABI TRANSFORMATIONS
        let mut params_types = Vec::new();
        let mut params_infos = Vec::new();

        for param_ty in &cir_func_type.params {
            let abi_param_type = cir_type_to_abi_type(self.tctx.clone(), param_ty);

            params_types.push(abi_param_type);
            params_infos.push(ABIArgInfo::direct());
        }

        let ret_abi_type = cir_type_to_abi_type(self.tctx.clone(), &cir_func_type.ret_type);

        let ret_info = if cir_func_type.ret_type.is_void() {
            ABIRetInfo {
                abi_type: ret_abi_type,
                kind: ABIRetInfoKind::Ignore,
                cir_ret_type: cir_func_type.ret_type.clone(),
            }
        } else {
            ABIRetInfo {
                abi_type: ret_abi_type,
                kind: ABIRetInfoKind::Direct { coerce_to: None },
                cir_ret_type: cir_func_type.ret_type.clone(),
            }
        };

        ABIFunctionInfo {
            params_infos,
            params_types,
            ret_info,
        }
    }

    fn classify_func_sysv(&self, fn_ty: &CIRFuncType) -> ABIFunctionInfo {
        let mut available_regs = Registers {
            int_regs: 6,
            sse_regs: 8,
        };

        let mut params_types = Vec::new();
        let mut params_infos = Vec::new();

        let ret_info = self.classify_return(&fn_ty.ret_type);

        // sret consumes one integer register in sysv
        if ret_info.kind.is_indirect() {
            available_regs.int_regs -= 1;
        }

        for param_type in &fn_ty.params {
            let abi_arg = self.classify_parameter(param_type, &mut available_regs, true);

            // index before expansion
            let start = params_types.len() as u16;

            // expand ABI arg into LLVM param types
            params_types.extend(self.param_types_from_arg_info(&abi_arg, param_type));

            // index after expansion
            let end = params_types.len() as u16;

            params_infos.push(abi_arg.with_indices(start, end));
        }

        ABIFunctionInfo {
            params_infos,
            params_types,
            ret_info,
        }
    }
}

impl TargetABI for X86_64 {
    // https://github.com/llvm/llvm-project/blob/a08cc6e0d5e3fa653649a7826f1ffafc2b3ea2dd/clang/lib/CodeGen/Targets/X86.cpp#L2732
    fn classify_argument(&self, ty: &CIRType, free_int_regs: u32, is_named: bool) -> (ABIArgInfo, Registers) {
        let ty = {
            if !is_named {
                &self.apply_variadic_argument_promote(ty)
            } else {
                ty
            }
        };

        let mut lo_class = RegisterClass::NoClass;
        let mut hi_class = RegisterClass::NoClass;
        classify(&self.info, &self.tctx, ty, 0, &mut lo_class, &mut hi_class);

        assert!(
            hi_class != RegisterClass::Memory || lo_class == RegisterClass::Memory,
            "Invalid memory classification."
        );
        assert!(
            hi_class != RegisterClass::SSEUP || lo_class == RegisterClass::SSE,
            "Invalid SSEUp classification."
        );

        #[allow(unused_assignments)]
        let mut result_type = None;
        let mut needed_regs = Registers::default();

        match lo_class {
            RegisterClass::NoClass => {
                assert!(hi_class == RegisterClass::NoClass);
                return (ABIArgInfo::ignore(), needed_regs);
            }
            RegisterClass::Integer => {
                needed_regs.int_regs += 1;

                if ty.is_integer_or_bool() {
                    result_type = Some(cir_type_to_abi_type(self.tctx.clone(), ty));
                } else if let Some(enum_type) = ty.as_enum(&self.tctx) {
                    if enum_type.is_scalar_optimizable() {
                        let tag_type = enum_type.tag_type_or_infer_or_default();
                        result_type = Some(cir_type_to_abi_type(self.tctx.clone(), &tag_type));
                    }
                } else {
                    result_type = Some(self.get_int_type_at_offset(ty, 0, ty, 0));
                }

                if hi_class == RegisterClass::NoClass && ty.is_integer_or_bool() {
                    let attrs: ABIArgAttrs;

                    if ty.is_signed_integer() {
                        attrs = ABIArgAttrs {
                            sign_ext: true,
                            zero_ext: false,
                            ..Default::default()
                        };
                    } else {
                        attrs = ABIArgAttrs {
                            sign_ext: false,
                            zero_ext: true,
                            ..Default::default()
                        };
                    }

                    if is_integer_type!(ty, PlainType::Int128 | PlainType::UInt128) {
                        needed_regs.int_regs += 1;
                    }

                    return (
                        ABIArgInfo::direct_coerce(result_type.unwrap()).with_attrs(attrs),
                        needed_regs,
                    );
                }
            }
            RegisterClass::Memory => {
                // indirect uses 1 int reg for pointer
                return (self.abi_arg_info_indirect_result(ty, free_int_regs), needed_regs);
            }
            RegisterClass::SSE => {
                needed_regs.sse_regs += 1;
                result_type = Some(self.get_sse_type_at_offset(ty, 0, ty, 0).unwrap());
            }
            RegisterClass::SSEUP => unreachable!(),
        }

        let mut high_part = None;

        match hi_class {
            RegisterClass::Memory => unreachable!(),
            RegisterClass::NoClass => {}
            RegisterClass::Integer => {
                needed_regs.int_regs += 1;
                high_part = Some(self.get_int_type_at_offset(ty, 8, ty, 8));

                assert!(lo_class != RegisterClass::NoClass, "empty first 8 bytes not allowed");
            }
            RegisterClass::SSE => {
                needed_regs.sse_regs += 1;

                let sse_ty = self
                    .get_sse_type_at_offset(ty, 8, ty, 8)
                    .unwrap_or(ABIType::Float(ABIFloatKind::F64)); // fallback

                high_part = Some(sse_ty);

                assert!(lo_class != RegisterClass::NoClass, "empty first 8 bytes not allowed");
            }
            RegisterClass::SSEUP => {
                unreachable!() // NOTE: Vector type not supported already.
            }
        }

        if let (Some(lo), Some(hi)) = (&result_type, &high_part) {
            return (ABIArgInfo::direct_pair(lo.clone(), hi.clone()), needed_regs);
        }

        if let Some(result) = &result_type {
            let abi_type = cir_type_to_abi_type(self.tctx.clone(), ty);

            // if abi type already matches, pass directly
            if *result == abi_type {
                return (ABIArgInfo::direct(), needed_regs);
            }

            // only allow integer-width coercion for scalar integer/bool types
            if ty.is_integer_or_bool() {
                if result.as_integer_bits(&self.info) == abi_type.as_integer_bits(&self.info) {
                    return (ABIArgInfo::direct(), needed_regs);
                }
            }

            // coerce to the abi register type otherwise
            return (ABIArgInfo::direct_coerce(result.clone()), needed_regs);
        }

        // fallback
        (ABIArgInfo::direct(), needed_regs)
    }

    fn classify_func(&self, fn_ty: &CIRFuncType) -> Result<ABIFunctionInfo, String> {
        match fn_ty.callconv {
            // SysV64, explicit System V AMD64 ABI
            CallConv::SysV64 => Ok(self.classify_func_sysv(fn_ty)),

            // C default convention, platform dependent
            CallConv::System | CallConv::C => match self.info.os {
                ABITargetOS::Linux | ABITargetOS::MacOS => Ok(self.classify_func_sysv(fn_ty)),
                ABITargetOS::Windows => unimplemented!("Windows ABI not implemented yet."),
                ABITargetOS::Unknown => unreachable!(),
            },

            // Win64, explicit Windows x64 ABI
            CallConv::Win64 => unimplemented!("Windows ABI not implemented yet."),

            // Naked, no prologue/epilogue, just bare function
            CallConv::Naked => Ok(self.classify_func_naked(fn_ty)),

            // Interrupt handler
            CallConv::Interrupt => Err("Interrupt calling convention not supported yet.".to_string()),

            // Fast optimization hint, same as C convention
            CallConv::Fast => match self.info.os {
                ABITargetOS::Linux | ABITargetOS::MacOS => Ok(self.classify_func_sysv(fn_ty)),
                ABITargetOS::Windows => unimplemented!("Windows ABI not implemented yet."),
                _ => unreachable!(),
            },

            // Cold optimization hint, same as C convention
            CallConv::Cold => match self.info.os {
                ABITargetOS::Linux | ABITargetOS::MacOS => Ok(self.classify_func_sysv(fn_ty)),
                ABITargetOS::Windows => unimplemented!("Windows ABI not implemented yet."),
                _ => unreachable!(),
            },

            // ARM convention on x86-64 - error
            CallConv::Aapcs => Err("AAPCS is ARM-only and not supported on x86-64.".to_string()),

            // 32-bit x86 conventions - not supported on x86-64
            CallConv::Stdcall => Err("Stdcall is a 32-bit x86 convention, not supported on x86-64.".to_string()),
            CallConv::Fastcall => Err("Fastcall is a 32-bit x86 convention, not supported on x86-64.".to_string()),
            CallConv::Thiscall => Err("Thiscall is a 32-bit x86 convention, not supported on x86-64.".to_string()),

            // Vectorcall - available on both x86 and x86-64
            CallConv::Vectorcall => {
                // Vectorcall has its own rules - would need separate implementation
                // For now, fallback to platform default
                match self.info.os {
                    ABITargetOS::Windows => unimplemented!("Vectorcall on Windows not implemented yet."),
                    _ => Ok(self.classify_func_sysv(fn_ty)), // Vectorcall on non-Windows? Probably fallback
                }
            }
        }
    }

    fn apply_variadic_argument_promote(&self, ty: &CIRType) -> CIRType {
        match ty {
            CIRType::Const(ty) => self.apply_variadic_argument_promote(ty),

            CIRType::Plain(plain_type) => match plain_type {
                // pointers and pointer-sized types remain as-is
                PlainType::UIntPtr | PlainType::IntPtr | PlainType::ISize | PlainType::USize => ty.clone(),

                // integer types smaller than int (32-bit) get promoted to int
                PlainType::Int8 | PlainType::Int16 => {
                    if plain_type.is_signed() {
                        CIRType::Plain(PlainType::Int)
                    } else {
                        CIRType::Plain(PlainType::UInt32)
                    }
                }
                PlainType::UInt8 | PlainType::UInt16 => CIRType::Plain(PlainType::UInt32),

                // int and int32 remain as-is (int-sized)
                PlainType::Int | PlainType::Int32 => ty.clone(),
                PlainType::UInt | PlainType::UInt32 => ty.clone(),

                // larger integers remain as-is
                PlainType::Int64 | PlainType::UInt64 => ty.clone(),
                PlainType::Int128 | PlainType::UInt128 => ty.clone(),

                // float16 promotes to double
                PlainType::Float16 => CIRType::Plain(PlainType::Float64),

                // float32 promotes to float64
                PlainType::Float32 => CIRType::Plain(PlainType::Float64),

                // float64 and float128 remain as-is
                PlainType::Float64 | PlainType::Float128 => ty.clone(),

                // char promotes to int
                PlainType::Char => CIRType::Plain(PlainType::Int),

                // bool promotes to int
                PlainType::Bool => CIRType::Plain(PlainType::Int),

                PlainType::Void | PlainType::Null => panic!("void or null type in varargs"),
            },

            CIRType::Array(array_type) => CIRType::Pointer(array_type.element_type.clone()),

            CIRType::Pointer(_) | CIRType::FuncType(_) => ty.clone(),
            CIRType::Struct(_) | CIRType::Dynamic(_) | CIRType::Enum(_) | CIRType::Union(_) => ty.clone(),
        }
    }

    fn classify_return(&self, cir_ret_type: &CIRType) -> ABIRetInfo {
        let mut lo_class = RegisterClass::NoClass;
        let mut hi_class = RegisterClass::NoClass;
        classify(&self.info, &self.tctx, cir_ret_type, 0, &mut lo_class, &mut hi_class);

        assert!(
            hi_class != RegisterClass::Memory || lo_class == RegisterClass::Memory,
            "Invalid memory classification."
        );
        assert!(
            hi_class != RegisterClass::SSEUP || lo_class == RegisterClass::SSE,
            "Invalid SSEUp classification."
        );

        let mut result_type = None;

        match lo_class {
            RegisterClass::NoClass => {
                if hi_class == RegisterClass::NoClass {
                    return ABIRetInfo {
                        abi_type: cir_type_to_abi_type(self.tctx.clone(), cir_ret_type),
                        kind: ABIRetInfoKind::Ignore,
                        cir_ret_type: Box::new(cir_ret_type.clone()),
                    };
                }

                assert!(
                    hi_class == RegisterClass::SSE || hi_class == RegisterClass::Integer,
                    "Expected SSE or Integer for high class when low is NoClass"
                );
            }
            RegisterClass::SSEUP => unreachable!(),
            RegisterClass::Memory => {
                return ABIRetInfo {
                    abi_type: cir_type_to_abi_type(self.tctx.clone(), cir_ret_type),
                    kind: ABIRetInfoKind::Indirect { sret: true },
                    cir_ret_type: Box::new(cir_ret_type.clone()),
                };
            }
            RegisterClass::Integer => {
                result_type = Some(self.get_int_type_at_offset(cir_ret_type, 0, cir_ret_type, 0));

                if hi_class == RegisterClass::NoClass && cir_ret_type.is_integer_or_bool() {
                    return ABIRetInfo {
                        abi_type: result_type.clone().unwrap(),
                        kind: ABIRetInfoKind::Direct { coerce_to: result_type },
                        cir_ret_type: Box::new(cir_ret_type.clone()),
                    };
                }
            }
            RegisterClass::SSE => {
                result_type = Some(self.get_sse_type_at_offset(cir_ret_type, 0, cir_ret_type, 0).unwrap());
            }
        }

        let mut high_part = None;

        match hi_class {
            RegisterClass::Memory | RegisterClass::NoClass => {}
            RegisterClass::Integer => {
                assert!(lo_class != RegisterClass::NoClass, "empty first 8 bytes not allowed");
                high_part = Some(self.get_int_type_at_offset(cir_ret_type, 8, cir_ret_type, 8));
            }
            RegisterClass::SSE => {
                assert!(lo_class != RegisterClass::NoClass, "empty first 8 bytes not allowed");
                high_part = Some(self.get_sse_type_at_offset(cir_ret_type, 8, cir_ret_type, 8).unwrap());
            }
            RegisterClass::SSEUP => {
                unreachable!() // NOTE: Vector type not supported already.
            }
        }

        // if a high part was specified, return as direct pair
        if let (Some(lo), Some(hi)) = (&result_type, high_part) {
            // combine lo and hi into a struct type for direct pair return
            return ABIRetInfo {
                abi_type: ABIType::Struct(vec![lo.clone(), hi.clone()], false),
                kind: ABIRetInfoKind::DirectPair { lo: lo.clone(), hi },
                cir_ret_type: Box::new(cir_ret_type.clone()),
            };
        }

        if let Some(result) = result_type {
            let abi_type = cir_type_to_abi_type(self.tctx.clone(), cir_ret_type);

            if result == abi_type {
                return ABIRetInfo {
                    abi_type: result,
                    kind: ABIRetInfoKind::Direct { coerce_to: None },
                    cir_ret_type: Box::new(cir_ret_type.clone()),
                };
            }

            return ABIRetInfo {
                abi_type: result.clone(),
                kind: ABIRetInfoKind::Direct {
                    coerce_to: Some(result),
                },
                cir_ret_type: Box::new(cir_ret_type.clone()),
            };
        }

        // fallback
        let abi_type = cir_type_to_abi_type(self.tctx.clone(), cir_ret_type);
        ABIRetInfo {
            abi_type,
            kind: ABIRetInfoKind::Direct { coerce_to: None },
            cir_ret_type: Box::new(cir_ret_type.clone()),
        }
    }

    fn stack_alignment(&self) -> u32 {
        MIN_ABI_STACK_ALIGN
    }
}

fn classify(
    info: &ABITargetInfo,
    tctx: &CIRTypeContext,
    ty: &CIRType,
    offset_base: u32,
    lo_class: *mut RegisterClass,
    hi_class: *mut RegisterClass,
) {
    unsafe { *lo_class = RegisterClass::NoClass };
    unsafe { *hi_class = RegisterClass::NoClass };

    let current = if offset_base < 8 { lo_class } else { hi_class };
    unsafe { *current = RegisterClass::Memory };

    match ty {
        CIRType::Struct(_) | CIRType::Union(_) => {
            classify_struct_or_union(info, tctx, ty, offset_base, current, lo_class, hi_class)
        }

        CIRType::Enum(_) => classify_enum(info, tctx, ty, offset_base, lo_class, hi_class),

        CIRType::Plain(plain_type) => classify_plain_type(plain_type, offset_base, lo_class, hi_class),
        CIRType::Const(inner) => classify(info, tctx, inner, offset_base, lo_class, hi_class),
        CIRType::Pointer(_) | CIRType::FuncType(_) => {
            unsafe { *current = RegisterClass::Integer };
        }
        CIRType::Array(array_type) => classify_array(info, tctx, array_type, offset_base, lo_class, hi_class),
        CIRType::Dynamic(_) => classify_dynamic(info, tctx, offset_base, lo_class, hi_class),
    }
}

fn classify_dynamic(
    info: &ABITargetInfo,
    tctx: &CIRTypeContext,
    offset_base: u32,
    lo_class: *mut RegisterClass,
    hi_class: *mut RegisterClass,
) {
    // dynamic type is construct of two pointers: data_ptr and vtable_ptr
    // each pointer is 8 bytes and integer-class

    let mut temp_lo = RegisterClass::NoClass;
    let mut temp_hi = RegisterClass::NoClass;

    // first pointer (data_ptr)
    classify(
        info,
        tctx,
        &CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void))),
        offset_base,
        &mut temp_lo,
        &mut temp_hi,
    );

    unsafe { *lo_class = classify_merge(*lo_class, temp_lo) };
    unsafe { *hi_class = classify_merge(*hi_class, temp_hi) };

    // second pointer (vtable_ptr) at offset + 8
    let mut temp_lo2 = RegisterClass::NoClass;
    let mut temp_hi2 = RegisterClass::NoClass;

    classify(
        info,
        tctx,
        &CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void))),
        offset_base + 8,
        &mut temp_lo2,
        &mut temp_hi2,
    );

    unsafe { *lo_class = classify_merge(*lo_class, temp_lo2) };
    unsafe { *hi_class = classify_merge(*hi_class, temp_hi2) };
}

fn classify_array(
    info: &ABITargetInfo,
    tctx: &CIRTypeContext,
    array_type: &CIRArrayType,
    offset_base: u32,
    lo_class: *mut RegisterClass,
    hi_class: *mut RegisterClass,
) {
    let layout = tctx.layout_of(&CIRType::Array(array_type.clone()));
    let element_ty = &array_type.element_type;
    let element_layout = tctx.layout_of(element_ty);

    if layout.size > 16 {
        return; // keep memory class
    }

    if offset_base % element_layout.align != 0 {
        unsafe { *lo_class = RegisterClass::Memory };
        classify_post_merge(layout.size, lo_class, hi_class);
        return;
    }

    // re-classify
    if offset_base < 8 {
        unsafe { *lo_class = RegisterClass::NoClass };
    } else {
        unsafe { *hi_class = RegisterClass::NoClass };
    }

    // check for vector-sized array
    if layout.size > 16 && (layout.size != element_layout.size) {
        unsafe { *lo_class = RegisterClass::Memory };
        return;
    }

    let mut offset = offset_base;
    for _ in 0..array_type.len {
        let mut field_lo = RegisterClass::NoClass;
        let mut field_hi = RegisterClass::NoClass;

        classify(info, tctx, element_ty, offset, &mut field_lo, &mut field_hi);

        offset += element_layout.size;

        unsafe { *lo_class = classify_merge(*lo_class, field_lo) };
        unsafe { *hi_class = classify_merge(*hi_class, field_hi) };

        if unsafe { *lo_class } == RegisterClass::Memory || unsafe { *hi_class } == RegisterClass::Memory {
            break;
        }
    }

    classify_post_merge(layout.size, lo_class, hi_class);
}

fn classify_enum(
    info: &ABITargetInfo,
    tctx: &CIRTypeContext,
    ty: &CIRType,
    offset_base: u32,
    lo_class: *mut RegisterClass,
    hi_class: *mut RegisterClass,
) {
    let enum_type = ty.as_enum(tctx).unwrap();

    if enum_type.is_scalar_optimizable() {
        // enum is an integer
        let tag_ty = enum_type.tag_type_or_infer_or_default();
        classify(info, tctx, &tag_ty, offset_base, lo_class, hi_class);
        return;
    }

    // enums is a struct { tag, [i8; N] payload }

    let layout = tctx.layout_of(ty);

    // if size > 16 bytes, keep default memory class
    if layout.size > 16 {
        return;
    }

    // re-classify
    if offset_base < 8 {
        unsafe { *lo_class = RegisterClass::NoClass };
    } else {
        unsafe { *hi_class = RegisterClass::NoClass };
    }

    // first, classify the tag field at offset 0
    let tag_ty = enum_type.tag_type_or_infer_or_default();
    let tag_offset = offset_base;
    let tag_layout = tctx.layout_of(&tag_ty);
    let tag_size = tag_layout.size;

    let mut tag_lo = RegisterClass::NoClass;
    let mut tag_hi = RegisterClass::NoClass;
    classify(info, tctx, &tag_ty, tag_offset, &mut tag_lo, &mut tag_hi);

    unsafe { *lo_class = classify_merge(*lo_class, tag_lo) };
    unsafe { *hi_class = classify_merge(*hi_class, tag_hi) };

    if unsafe { *lo_class } == RegisterClass::Memory || unsafe { *hi_class } == RegisterClass::Memory {
        classify_post_merge(layout.size, lo_class, hi_class);
        return;
    }

    // classify the payload
    let payload_size = layout.size - tag_size;
    let payload_offset = offset_base + tag_size;

    if payload_size > 0 {
        // payload is a byte array, classify based on its size
        let payload_ty = CIRType::Array(CIRArrayType {
            element_type: Box::new(CIRType::Plain(PlainType::UInt8)),
            len: payload_size as usize,
        });

        let mut payload_lo = RegisterClass::NoClass;
        let mut payload_hi = RegisterClass::NoClass;
        classify(
            info,
            tctx,
            &payload_ty,
            payload_offset,
            &mut payload_lo,
            &mut payload_hi,
        );

        unsafe { *lo_class = classify_merge(*lo_class, payload_lo) };
        unsafe { *hi_class = classify_merge(*hi_class, payload_hi) };
    }

    classify_post_merge(layout.size, lo_class, hi_class);
}

fn classify_struct_or_union(
    info: &ABITargetInfo,
    tctx: &CIRTypeContext,
    ty: &CIRType,
    offset_base: u32,
    current: *mut RegisterClass,
    lo_class: *mut RegisterClass,
    hi_class: *mut RegisterClass,
) {
    let (fields, is_union, loc) = match ty {
        CIRType::Struct(type_id) => {
            let struct_type = tctx.get_struct(*type_id);
            (struct_type.fields, false, struct_type.loc)
        }
        CIRType::Union(type_id) => {
            let union_type = tctx.get_union(*type_id);
            (union_type.fields, true, union_type.loc)
        }
        _ => unreachable!(),
    };

    let layout = if is_union {
        tctx.compute_union_layout(&CIRUnionType {
            decl_key: None,
            name: None,
            fields: fields.clone(),
            fields_info: vec![],
            repr_attr: None,
            align: None,
            loc,
        })
    } else {
        tctx.compute_struct_layout(&CIRStructType {
            decl_key: None,
            name: None,
            fields: fields.clone(),
            fields_info: vec![],
            repr_attr: None,
            align: None,
            loc,
        })
    };
    let size = layout.size;

    if size > 16 {
        return;
    }

    unsafe { *current = RegisterClass::NoClass };

    for (i, field_type) in fields.iter().enumerate() {
        let field_layout = tctx.layout_of(field_type);

        let field_offset = if is_union {
            offset_base
        } else {
            offset_base + layout.lookup_field_offset(i)
        };

        if field_offset % field_layout.align != 0 {
            unsafe { *lo_class = RegisterClass::Memory };
            classify_post_merge(size, lo_class, hi_class);
            return;
        }

        let mut field_lo = RegisterClass::NoClass;
        let mut field_hi = RegisterClass::NoClass;

        classify(info, tctx, field_type, field_offset, &mut field_lo, &mut field_hi);

        unsafe { *lo_class = classify_merge(*lo_class, field_lo) };
        unsafe { *hi_class = classify_merge(*hi_class, field_hi) };

        if unsafe { *lo_class } == RegisterClass::Memory || unsafe { *hi_class } == RegisterClass::Memory {
            break;
        }
    }

    classify_post_merge(size, lo_class, hi_class);
}

fn classify_plain_type(
    plain_type: &PlainType,
    offset_base: u32,
    lo_class: *mut RegisterClass,
    hi_class: *mut RegisterClass,
) {
    match plain_type {
        PlainType::Int8
        | PlainType::UInt8
        | PlainType::Int16
        | PlainType::UInt16
        | PlainType::Int32
        | PlainType::UInt32
        | PlainType::Int64
        | PlainType::UInt64
        | PlainType::Char
        | PlainType::Bool
        | PlainType::Int
        | PlainType::UInt
        | PlainType::ISize
        | PlainType::USize
        | PlainType::IntPtr
        | PlainType::UIntPtr => {
            if offset_base < 8 {
                unsafe { *lo_class = RegisterClass::Integer };
            } else {
                unsafe { *hi_class = RegisterClass::Integer };
            }
        }

        PlainType::Int128 | PlainType::UInt128 => {
            unsafe { *lo_class = RegisterClass::Integer };
            unsafe { *hi_class = RegisterClass::Integer };
        }

        PlainType::Float16 | PlainType::Float32 | PlainType::Float64 => {
            if offset_base < 8 {
                unsafe { *lo_class = RegisterClass::SSE };
            } else {
                unsafe { *hi_class = RegisterClass::SSE };
            }
        }

        PlainType::Float128 => {
            unsafe { *lo_class = RegisterClass::SSE };
            unsafe { *hi_class = RegisterClass::SSEUP };
        }

        PlainType::Void => {
            if offset_base < 8 {
                unsafe { *lo_class = RegisterClass::NoClass };
            } else {
                unsafe { *hi_class = RegisterClass::NoClass };
            }
        }

        PlainType::Null => unreachable!(),
    }
}

fn classify_merge(current: RegisterClass, field: RegisterClass) -> RegisterClass {
    if current == field {
        return current;
    }

    if current == RegisterClass::NoClass {
        return field;
    }
    if field == RegisterClass::NoClass {
        return current;
    }

    if current == RegisterClass::Integer || field == RegisterClass::Integer {
        return RegisterClass::Integer;
    }

    if (current == RegisterClass::SSE && field == RegisterClass::SSEUP)
        || (current == RegisterClass::SSEUP && field == RegisterClass::SSE)
    {
        return RegisterClass::SSE;
    }

    RegisterClass::Memory
}

fn classify_post_merge(size: u32, lo_class: *mut RegisterClass, hi_class: *mut RegisterClass) {
    if unsafe { *hi_class } == RegisterClass::Memory {
        unsafe { *lo_class = RegisterClass::Memory };
        return;
    }

    // if size > 16 and lo isn't SSE or hi isn't SSEUP, default to memory
    if size > 16 && (unsafe { *lo_class } != RegisterClass::SSE || unsafe { *hi_class } != RegisterClass::SSEUP) {
        unsafe { *lo_class = RegisterClass::Memory };
        return;
    }

    // if hi is SSEUP but lo isn't SSE or SSEUP, convert hi to SSE
    if unsafe { *hi_class } == RegisterClass::SSEUP
        && unsafe { *lo_class } != RegisterClass::SSE
        && unsafe { *lo_class } != RegisterClass::SSEUP
    {
        unsafe { *hi_class = RegisterClass::SSE };
    }
}

fn try_use_registers(available_regs: &mut Registers, used: &Registers) -> bool {
    if available_regs.sse_regs < used.sse_regs {
        return false;
    }
    if available_regs.int_regs < used.int_regs {
        return false;
    }
    available_regs.int_regs -= used.int_regs;
    available_regs.sse_regs -= used.sse_regs;
    true
}
