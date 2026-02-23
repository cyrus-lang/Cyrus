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

use cyrusc_abi::ast_defs::CallConv;
use cyrusc_tast::types::PlainType;

use crate::{
    abi::{
        args::{ABIArgAttrs, ABIArgInfo, ABIArgKind, ABIFunctionInfo, ABIRetInfo, ABIRetInfoKind, ExpandKind},
        helpers::{Registers, cir_type_to_abi_type, is_cir_type_abi_aggregate},
        layout::type_layout,
        target::{ABITargetInfo, ABITargetOS, RegisterClass, TargetABI},
        types::{ABIFloatKind, ABIType},
    },
    cir::types::{CIRArrayTy, CIRFuncTy, CIRStructTy, CIRTy},
    is_integer_type,
};

pub struct X86_64 {
    info: ABITargetInfo,
}

impl X86_64 {
    pub fn new(info: ABITargetInfo) -> Self {
        Self { info }
    }

    fn classify_parameter(&self, ty: &CIRTy, available_regs: &mut Registers, is_named: bool) -> ABIArgInfo {
        let (abi_arg_info, needed_regs) = self.classify_argument(ty, available_regs.int_regs, is_named);

        if try_use_registers(available_regs, &needed_regs) {
            abi_arg_info
        } else {
            self.abi_arg_info_indirect_result(ty, available_regs.int_regs)
        }
    }

    fn abi_arg_info_indirect_result(&self, ty: &CIRTy, free_int_regs: u32) -> ABIArgInfo {
        // if this is a scalar LLVM value then assume LLVM will pass it in the right place naturally
        if !is_cir_type_abi_aggregate(ty) {
            if ty.is_integer_or_bool() {
                let abi_ty = cir_type_to_abi_type(&self.info, ty);
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
            // No change, just put it on the stack
            return ABIArgInfo::direct();
        }

        // byval alignment
        let layout = type_layout(&self.info, ty);
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
            let abi_type = cir_type_to_abi_type(&self.info, ty);

            ABIArgInfo {
                kind: ABIArgKind::Indirect {
                    align: 8,
                    ty: abi_type,
                },
                attrs: ABIArgAttrs {
                    by_val: true,
                    ..Default::default()
                },
                param_index_start: 0,
                param_index_end: 0,
            }
        } else {
            // regular byval indirect
            let abi_type = cir_type_to_abi_type(&self.info, ty);

            ABIArgInfo {
                kind: ABIArgKind::Indirect {
                    align,
                    ty: abi_type,
                },
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
    fn get_int_type_at_offset(&self, ty: &CIRTy, offset: u32, source_type: &CIRTy, source_offset: u32) -> ABIType {
        match ty {
            CIRTy::PlainType(plain_type) => match plain_type {
                PlainType::UIntPtr
                | PlainType::IntPtr
                | PlainType::ISize
                | PlainType::USize
                | PlainType::Int64
                | PlainType::UInt64 => {
                    if offset != 0 {
                        return cir_type_to_abi_type(&self.info, ty);
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
                        let layout = type_layout(&self.info, ty);

                        if self.bits_contain_no_user_data(source_type, source_offset + layout.size, source_offset + 8) {
                            return cir_type_to_abi_type(&self.info, ty);
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
            CIRTy::Const(ty) => {
                return self.get_int_type_at_offset(ty, offset, source_type, source_offset);
            }
            CIRTy::Pointer(_) | CIRTy::FuncType(_) => {
                if offset == 0 {
                    return cir_type_to_abi_type(&self.info, ty);
                }
            }
            CIRTy::Tuple(_) | CIRTy::Struct(_) => {
                if let Some((field_ty, field_offset)) = self.get_member_at_offset(ty, offset) {
                    return self.get_int_type_at_offset(&field_ty, offset - field_offset, source_type, source_offset);
                }
            }
            CIRTy::Array(array_ty) => {
                let element_ty = &array_ty.ty;
                let element_layout = type_layout(&self.info, element_ty);
                let element_offset = (offset / element_layout.size) * element_layout.size;
                return self.get_int_type_at_offset(element_ty, offset - element_offset, source_type, source_offset);
            }
            CIRTy::Dynamic(_) => {
                if offset < 8 {
                    return ABIType::Pointer; // data_ptr (first 8 bytes)
                }
                if offset < 16 {
                    return ABIType::Pointer; // vtable_ptr (next 8 bytes)
                }
            }
            CIRTy::Union(_) | CIRTy::Enum(_) => {
                // fallback
            }
        }

        let layout = type_layout(&self.info, source_type);
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

    fn get_member_at_offset(&self, ty: &CIRTy, offset: u32) -> Option<(CIRTy, u32)> {
        let fields = ty.as_struct().unwrap().fields;

        let layout = type_layout(&self.info, ty);

        if layout.size < offset {
            return None;
        }

        let mut current_offset = 0;

        for field_ty in fields {
            let field_layout = type_layout(&self.info, &field_ty);
            let align = field_layout.align;

            let padding = (align - (current_offset % align)) % align;
            current_offset += padding;

            if current_offset > offset {
                break;
            }

            if current_offset <= offset && offset < current_offset + field_layout.size {
                return Some((field_ty, current_offset));
            }

            current_offset += field_layout.size;
        }

        None
    }

    fn get_fp_type_at_offset(&self, ty: &CIRTy, offset: u32) -> Option<ABIType> {
        if offset == 0 && ty.is_float() {
            return Some(cir_type_to_abi_type(&self.info, ty));
        }

        if ty.is_struct() || ty.is_union() {
            if let Some((field_ty, field_offset)) = self.get_member_at_offset(ty, offset) {
                dbg!(&field_ty, field_offset);
                return self.get_fp_type_at_offset(&field_ty, offset - field_offset);
            }
        }

        if let Some(array_ty) = ty.as_array() {
            let element_ty = &array_ty.ty;
            let element_layout = type_layout(&self.info, &element_ty);
            let element_start = (offset / element_layout.size) * element_layout.size;
            let element_offset = offset - element_start;
            return self.get_fp_type_at_offset(&element_ty, element_offset);
        }

        None
    }

    fn get_sse_type_at_offset(
        &self,
        ty: &CIRTy,
        offset: u32,
        source_type: &CIRTy,
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

        let source_layout = type_layout(&self.info, source_type);
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
    fn bits_contain_no_user_data(&self, ty: &CIRTy, start: u32, end: u32) -> bool {
        let check_for_struct_or_union = |fields: &Vec<CIRTy>, is_union: bool| {
            let mut current_offset = 0;

            for field in fields {
                let field_layout = type_layout(&self.info, field);
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

                if !self.bits_contain_no_user_data(field, field_start, end - field_offset) {
                    return false;
                }

                current_offset += field_layout.size;
            }

            true
        };
        let layout = type_layout(&self.info, ty);

        // if the bytes being queried are off the end of the type, there is no user data
        if layout.size <= start {
            return true;
        }

        if let Some(array_ty) = ty.as_array() {
            let element_ty = &array_ty.ty;
            let element_layout = type_layout(&self.info, element_ty);
            let element_size = element_layout.size;

            for i in 0..array_ty.len {
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
        } else if let Some(struct_ty) = ty.as_struct() {
            check_for_struct_or_union(&struct_ty.fields, false)
        } else if let Some(union_ty) = ty.as_union() {
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

    fn param_types_from_arg_info(&self, abi_arg: &ABIArgInfo, param_type: &CIRTy) -> Vec<ABIType> {
        let mut types = Vec::new();

        match &abi_arg.kind {
            ABIArgKind::Direct { coerce_to } => {
                if let Some(ty) = coerce_to {
                    types.push(ty.clone());
                } else {
                    types.push(cir_type_to_abi_type(&self.info, param_type));
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
                types.push(cir_type_to_abi_type(&self.info, param_type));
            }
            ABIArgKind::Expand { kind } => match kind {
                ExpandKind::Struct { .. } => {
                    assert!(param_type.is_struct() || param_type.is_union());
                    let fields = param_type.struct_or_union_fields().unwrap();

                    for field in fields {
                        types.push(cir_type_to_abi_type(&self.info, &field));
                    }
                }
                _ => return vec![cir_type_to_abi_type(&self.info, param_type)],
            },
            ABIArgKind::Ignore => {
                // skip
            }
        }

        types
    }

    // fn get_eightbyte_type(&self, ty: &CIRTy, offset: u32) -> ABIType {
    //     if let Some((field_ty, _)) = self.get_member_at_offset(ty, offset) {
    //         let field_layout = type_layout(&self.info, &field_ty);

    //         // if the field occupies the entire eightbyte, use its type
    //         if field_layout.size >= 8 {
    //             if field_ty.is_float() {
    //                 return cir_type_to_abi_type(&self.info, &field_ty);
    //             } else if field_ty.is_integer() || field_ty.is_pointer() {
    //                 // return ABIType::Integer(64);
    //                 return cir_type_to_abi_type(&self.info, &field_ty);
    //             }
    //         }

    //         ABIType::Integer(64)
    //     } else {
    //         ABIType::Integer(64)
    //     }
    // }
}

impl X86_64 {
    fn classify_func_naked(&self, fn_ty: &CIRFuncTy) -> ABIFunctionInfo {
        // naked functions just pass arguments as-is, no ABI transformations
        let mut params_types = Vec::new();
        let mut params_infos = Vec::new();

        for param_ty in &fn_ty.params {
            let abi_param_type = cir_type_to_abi_type(&self.info, param_ty);

            params_types.push(abi_param_type);
            params_infos.push(ABIArgInfo::direct());
        }

        let ret_abi_type = cir_type_to_abi_type(&self.info, &fn_ty.ret);

        let ret_info = if fn_ty.ret.is_void() {
            ABIRetInfo {
                ty: ret_abi_type,
                kind: ABIRetInfoKind::Ignore,
            }
        } else {
            ABIRetInfo {
                ty: ret_abi_type,
                kind: ABIRetInfoKind::Direct { coerce_to: None },
            }
        };

        ABIFunctionInfo {
            params_infos,
            params_types,
            ret_info,
        }
    }

    fn classify_func_sysv(&self, fn_ty: &CIRFuncTy) -> ABIFunctionInfo {
        let mut available_regs = Registers {
            int_regs: 6,
            sse_regs: 8,
        };

        let mut params_types = Vec::new();
        let mut params_infos = Vec::new();

        let ret_info = self.classify_return(&fn_ty.ret);

        if ret_info.kind.is_indirect() {
            available_regs.int_regs -= 1;

            // add sret parameter type
            params_types.push(ABIType::Pointer);

            // TODO: Set in context return_by_ref + sret
            // and handle RetStmt
            todo!();
        }

        for param_type in &fn_ty.params {
            let abi_arg = self.classify_parameter(param_type, &mut available_regs, true);

            params_types.extend(self.param_types_from_arg_info(&abi_arg, param_type));
            params_infos.push(abi_arg);
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
    fn classify_argument(
        &self,
        ty: &CIRTy,
        free_int_regs: u32,
        #[allow(unused)] is_named: bool,
    ) -> (ABIArgInfo, Registers) {
        let mut lo_class = RegisterClass::NoClass;
        let mut hi_class = RegisterClass::NoClass;
        classify(&self.info, ty, 0, &mut lo_class, &mut hi_class);

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
                result_type = Some(self.get_int_type_at_offset(ty, 0, ty, 0));

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
                high_part = Some(self.get_sse_type_at_offset(ty, 8, ty, 8).unwrap());
                assert!(lo_class != RegisterClass::NoClass, "empty first 8 bytes not allowed");
            }
            RegisterClass::SSEUP => {
                unreachable!() // NOTE: Vector type not supported already.
            }
        }

        if let (Some(lo), Some(hi)) = (&result_type, high_part) {
            return (ABIArgInfo::direct_pair(lo.clone(), hi), needed_regs);
        }

        if let Some(result) = &result_type {
            let abi_type = cir_type_to_abi_type(&self.info, ty);

            if *result == abi_type {
                return (ABIArgInfo::direct(), needed_regs);
            }

            if result.as_integer_bits(&self.info) == abi_type.as_integer_bits(&self.info) {
                return (ABIArgInfo::direct(), needed_regs);
            }

            return (ABIArgInfo::direct_coerce(result.clone()), needed_regs);
        }

        // fallback
        (ABIArgInfo::direct_coerce(ABIType::Integer(64)), needed_regs)
    }

    fn classify_func(&self, fn_ty: &CIRFuncTy) -> Result<ABIFunctionInfo, String> {
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

    // REVIEW: Maybe consider to use ABIArgInfo and remove ABIRetInfo entirely.
    fn classify_return(&self, ty: &CIRTy) -> ABIRetInfo {
        let mut lo_class = RegisterClass::NoClass;
        let mut hi_class = RegisterClass::NoClass;
        classify(&self.info, ty, 0, &mut lo_class, &mut hi_class);

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
                        ty: cir_type_to_abi_type(&self.info, ty),
                        kind: ABIRetInfoKind::Ignore,
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
                    ty: cir_type_to_abi_type(&self.info, ty),
                    kind: ABIRetInfoKind::Indirect { sret: true },
                };
            }
            RegisterClass::Integer => {
                let result_type = self.get_int_type_at_offset(ty, 0, ty, 0);

                if hi_class == RegisterClass::NoClass && ty.is_integer_or_bool() {
                    return ABIRetInfo {
                        ty: result_type.clone(),
                        kind: ABIRetInfoKind::Direct {
                            coerce_to: Some(result_type),
                        },
                    };
                }
            }
            RegisterClass::SSE => {
                result_type = Some(self.get_sse_type_at_offset(ty, 0, ty, 0).unwrap());
            }
        }

        let mut high_part = None;
        match hi_class {
            RegisterClass::Memory | RegisterClass::NoClass => {}
            RegisterClass::Integer => {
                assert!(lo_class != RegisterClass::NoClass, "empty first 8 bytes not allowed");
                high_part = Some(self.get_int_type_at_offset(ty, 8, ty, 8));
            }
            RegisterClass::SSE => {
                assert!(lo_class != RegisterClass::NoClass, "empty first 8 bytes not allowed");
                high_part = Some(self.get_sse_type_at_offset(ty, 8, ty, 8).unwrap());
            }
            RegisterClass::SSEUP => {
                unreachable!() // NOTE: Vector type not supported already.
            }
        }

        // if a high part was specified, return as direct pair
        if let (Some(lo), Some(hi)) = (&result_type, high_part) {
            // combine lo and hi into a struct type for direct pair return
            let struct_ty = ABIType::Struct(vec![lo.clone(), hi.clone()], false);
            return ABIRetInfo {
                ty: struct_ty,
                kind: ABIRetInfoKind::Direct { coerce_to: None },
            };
        }

        if let Some(result) = result_type {
            let abi_type = cir_type_to_abi_type(&self.info, ty);

            if result == abi_type {
                return ABIRetInfo {
                    ty: result,
                    kind: ABIRetInfoKind::Direct { coerce_to: None },
                };
            }

            return ABIRetInfo {
                ty: result.clone(),
                kind: ABIRetInfoKind::Direct {
                    coerce_to: Some(result),
                },
            };
        }

        // fallback
        ABIRetInfo {
            ty: ABIType::Integer(64),
            kind: ABIRetInfoKind::Direct { coerce_to: None },
        }
    }

    // REVIEW
    fn stack_alignment(&self) -> u32 {
        16
    }
}

fn classify(
    info: &ABITargetInfo,
    ty: &CIRTy,
    offset_base: u32,
    lo_class: &mut RegisterClass,
    hi_class: &mut RegisterClass,
) {
    *lo_class = RegisterClass::NoClass;
    *hi_class = RegisterClass::NoClass;

    // set default
    if offset_base < 8 {
        *lo_class = RegisterClass::Memory;
    } else {
        *hi_class = RegisterClass::Memory;
    }

    match ty {
        CIRTy::PlainType(plain_type) => classify_plain_type(plain_type, offset_base, lo_class, hi_class),
        CIRTy::Const(ty) => classify(info, ty, offset_base, lo_class, hi_class),
        CIRTy::Pointer(_) | CIRTy::FuncType(_) => classify_pointer(offset_base, lo_class, hi_class),
        CIRTy::Struct(_) => classify_struct_or_union(info, ty, offset_base, lo_class, hi_class),
        CIRTy::Union(_) => classify_struct_or_union(info, ty, offset_base, lo_class, hi_class),
        CIRTy::Tuple(tuple_ty) => {
            // tuple lowered as struct in codegen
            let struct_ty = CIRStructTy {
                fields: tuple_ty.elements.clone(),
                is_packed: false,
            };
            classify_struct_or_union(info, &CIRTy::Struct(struct_ty), offset_base, lo_class, hi_class)
        }
        CIRTy::Array(array_ty) => classify_array(info, array_ty, offset_base, lo_class, hi_class),
        CIRTy::Dynamic(_) => classify_dynamic(info, offset_base, lo_class, hi_class),
        CIRTy::Enum(_) => todo!(),
    }
}

fn classify_dynamic(
    info: &ABITargetInfo,
    offset_base: u32,
    lo_class: &mut RegisterClass,
    hi_class: &mut RegisterClass,
) {
    // dynamic type is construct of two pointers: data_ptr and vtable_ptr
    // each pointer is 8 bytes and integer-class

    let mut temp_lo = RegisterClass::NoClass;
    let mut temp_hi = RegisterClass::NoClass;

    // first pointer (data_ptr)
    classify(
        info,
        &CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))),
        offset_base,
        &mut temp_lo,
        &mut temp_hi,
    );

    *lo_class = classify_merge(*lo_class, temp_lo);
    *hi_class = classify_merge(*hi_class, temp_hi);

    // second pointer (vtable_ptr) at offset + 8
    let mut temp_lo2 = RegisterClass::NoClass;
    let mut temp_hi2 = RegisterClass::NoClass;

    classify(
        info,
        &CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))),
        offset_base + 8,
        &mut temp_lo2,
        &mut temp_hi2,
    );

    *lo_class = classify_merge(*lo_class, temp_lo2);
    *hi_class = classify_merge(*hi_class, temp_hi2);
}

fn classify_array(
    info: &ABITargetInfo,
    array_ty: &CIRArrayTy,
    offset_base: u32,
    lo_class: &mut RegisterClass,
    hi_class: &mut RegisterClass,
) {
    let layout = type_layout(info, &CIRTy::Array(array_ty.clone()));
    let element_ty = &array_ty.ty;
    let element_layout = type_layout(info, element_ty);

    if layout.size > 64 {
        return; // keep memory class
    }

    if offset_base % element_layout.align != 0 {
        *lo_class = RegisterClass::Memory;
        classify_post_merge(layout.size, lo_class, hi_class);
        return;
    }

    // re-classify
    if offset_base < 8 {
        *lo_class = RegisterClass::NoClass;
    } else {
        *hi_class = RegisterClass::NoClass;
    }

    // check for vector-sized array
    if layout.size > 16 && (layout.size != element_layout.size) {
        *lo_class = RegisterClass::Memory;
        return;
    }

    let mut offset = offset_base;
    for _ in 0..array_ty.len {
        let mut field_lo = RegisterClass::NoClass;
        let mut field_hi = RegisterClass::NoClass;

        classify(info, element_ty, offset, &mut field_lo, &mut field_hi);

        offset += element_layout.size;

        *lo_class = classify_merge(*lo_class, field_lo);
        *hi_class = classify_merge(*hi_class, field_hi);

        if *lo_class == RegisterClass::Memory || *hi_class == RegisterClass::Memory {
            break;
        }
    }

    classify_post_merge(layout.size, lo_class, hi_class);
}

fn classify_struct_or_union(
    info: &ABITargetInfo,
    ty: &CIRTy,
    offset_base: u32,
    lo_class: &mut RegisterClass,
    hi_class: &mut RegisterClass,
) {
    let is_union = ty.is_union();
    let fields = if let Some(struct_ty) = ty.as_struct() {
        struct_ty.fields.clone()
    } else if let Some(union_ty) = ty.as_union() {
        union_ty.fields.clone()
    } else {
        unreachable!()
    };

    let layout = type_layout(info, ty);

    // if size > 64 bytes, keep default memory class
    if layout.size > 64 {
        return;
    }

    // re-classify
    if offset_base < 8 {
        *lo_class = RegisterClass::NoClass;
    } else {
        *hi_class = RegisterClass::NoClass;
    }

    // for unions, all fields share the same offset
    if is_union {
        let mut union_lo = RegisterClass::NoClass;
        let mut union_hi = RegisterClass::NoClass;

        for field_ty in &fields {
            let mut field_lo = RegisterClass::NoClass;
            let mut field_hi = RegisterClass::NoClass;
            classify(info, field_ty, offset_base, &mut field_lo, &mut field_hi);

            union_lo = classify_merge(union_lo, field_lo);
            union_hi = classify_merge(union_hi, field_hi);

            if union_lo == RegisterClass::Memory || union_hi == RegisterClass::Memory {
                break;
            }
        }

        *lo_class = union_lo;
        *hi_class = union_hi;
        classify_post_merge(layout.size, lo_class, hi_class);
        return;
    }

    // for structs, we need the actual field offsets from the type
    let field_offsets = layout.field_offsets;

    for (idx, field_ty) in fields.iter().enumerate() {
        let field_offset = field_offsets[idx];
        let field_abs_offset = offset_base + field_offset;

        // alignment check
        let field_layout = type_layout(info, field_ty);

        if field_abs_offset % field_layout.align != 0 {
            *lo_class = RegisterClass::Memory;
            classify_post_merge(layout.size, lo_class, hi_class);
            return;
        }

        // vector size check
        if layout.size > 16 && layout.size != field_layout.size {
            *lo_class = RegisterClass::Memory;
            classify_post_merge(layout.size, lo_class, hi_class);
            return;
        }

        let mut field_lo = RegisterClass::NoClass;
        let mut field_hi = RegisterClass::NoClass;

        // classify the field at its absolute offset
        classify(info, field_ty, field_abs_offset, &mut field_lo, &mut field_hi);

        // merge field's classes into the overall struct classes
        *lo_class = classify_merge(*lo_class, field_lo);
        *hi_class = classify_merge(*hi_class, field_hi);

        if *lo_class == RegisterClass::Memory || *hi_class == RegisterClass::Memory {
            break;
        }
    }

    classify_post_merge(layout.size, lo_class, hi_class);
}

fn classify_pointer(offset_base: u32, lo_class: &mut RegisterClass, hi_class: &mut RegisterClass) {
    // pointers are always 8 bytes on x86-64
    if offset_base < 8 {
        *lo_class = RegisterClass::Integer;
    } else {
        *hi_class = RegisterClass::Integer;
    }
}

fn classify_plain_type(
    plain_type: &PlainType,
    offset_base: u32,
    lo_class: &mut RegisterClass,
    hi_class: &mut RegisterClass,
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
                *lo_class = RegisterClass::Integer;
            } else {
                *hi_class = RegisterClass::Integer;
            }
        }

        PlainType::Int128 | PlainType::UInt128 => {
            *lo_class = RegisterClass::Integer;
            *hi_class = RegisterClass::Integer;
        }

        PlainType::Float16 | PlainType::Float32 | PlainType::Float64 => {
            if offset_base < 8 {
                *lo_class = RegisterClass::SSE;
            } else {
                *hi_class = RegisterClass::SSE;
            }
        }

        PlainType::Float128 => {
            *lo_class = RegisterClass::SSE;
            *hi_class = RegisterClass::SSEUP;
        }

        PlainType::Void => {
            if offset_base < 8 {
                *lo_class = RegisterClass::NoClass;
            } else {
                *hi_class = RegisterClass::NoClass;
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

fn classify_post_merge(size: u32, lo_class: &mut RegisterClass, hi_class: &mut RegisterClass) {
    if *hi_class == RegisterClass::Memory {
        *lo_class = RegisterClass::Memory;
        return;
    }

    // if size > 16 and lo isn't SSE or hi isn't SSEUP, default to memory
    if size > 16 && (*lo_class != RegisterClass::SSE || *hi_class != RegisterClass::SSEUP) {
        *lo_class = RegisterClass::Memory;
        return;
    }

    // if hi is SSEUP but lo isn't SSE or SSEUP, convert hi to SSE
    if *hi_class == RegisterClass::SSEUP && *lo_class != RegisterClass::SSE && *lo_class != RegisterClass::SSEUP {
        *hi_class = RegisterClass::SSE;
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
