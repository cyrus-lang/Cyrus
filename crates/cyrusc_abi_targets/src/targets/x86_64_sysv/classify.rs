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
    ABIArgAttrs, ABIArgInfo, ABITargetInfo, RegisterClass, TargetABI,
    helpers::{NeededRegisters, cir_type_to_abi_type},
    layout::type_layout,
    types::ABIType,
};
use cyrusc_cir::{
    is_integer_type,
    types::{CIRArrayTy, CIRFuncTy, CIRStructTy, CIRTy},
};
use cyrusc_tast::types::PlainType;

pub struct X86_64SysV<'a> {
    info: &'a ABITargetInfo,
}

impl<'a> X86_64SysV<'a> {
    pub fn new(info: &'a ABITargetInfo) -> Self {
        Self { info }
    }

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
                        return cir_type_to_abi_type(self.info, ty);
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
                        let layout = type_layout(self.info, ty);

                        if self.bits_contain_no_user_data(source_type, source_offset + layout.size, source_offset + 8) {
                            return cir_type_to_abi_type(self.info, ty);
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
                    return cir_type_to_abi_type(self.info, ty);
                }
            }
            CIRTy::Tuple(_) | CIRTy::Struct(_) => {
                if let Some((field_ty, field_offset)) = self.get_member_at_offset(ty, offset) {
                    return self.get_int_type_at_offset(&field_ty, offset - field_offset, source_type, source_offset);
                }
            }
            CIRTy::Array(array_ty) => {
                let element_ty = &array_ty.ty;
                let element_layout = type_layout(self.info, element_ty);
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

        let layout = type_layout(self.info, source_type);
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

        let layout = type_layout(self.info, ty);

        if layout.size < offset {
            return None;
        }

        let mut current_offset = 0;

        for field_ty in fields {
            let field_layout = type_layout(self.info, &field_ty);
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

    fn get_sse_type_at_offset(&self, ty: &CIRTy, offset: u32) -> ABIType {
        todo!();
    }

    fn bits_contain_no_user_data(&self, source_type: &CIRTy, start: u32, end: u32) -> bool {
        todo!();
    }
}

impl<'a> TargetABI for X86_64SysV<'a> {
    // https://github.com/llvm/llvm-project/blob/main/clang/lib/CodeGen/Targets/X86.cpp
    fn classify_argument(&self, ty: &CIRTy, #[allow(unused)] is_named: bool) -> ABIArgInfo {
        let mut lo_class = RegisterClass::NoClass;
        let mut hi_class = RegisterClass::NoClass;
        classify(self.info, ty, 0, &mut lo_class, &mut hi_class);

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
        let mut needed_regs = NeededRegisters::default();

        match lo_class {
            RegisterClass::NoClass => {
                assert!(hi_class == RegisterClass::NoClass);
                return ABIArgInfo::ignore();
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

                    return ABIArgInfo::direct_coerce(result_type.unwrap()).with_attrs(attrs);
                }
            }
            RegisterClass::Memory => {
                let abi_type = cir_type_to_abi_type(self.info, &ty);
                return ABIArgInfo::indirect(abi_type, self.stack_alignment());
            }
            RegisterClass::SSE => {
                needed_regs.sse_regs += 1;
                result_type = Some(self.get_sse_type_at_offset(ty, 0));
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
                high_part = Some(self.get_sse_type_at_offset(ty, 8));
                assert!(lo_class != RegisterClass::NoClass, "empty first 8 bytes not allowed");
            }
            RegisterClass::SSEUP => {
                unreachable!() // NOTE: Vector type not supported already.
            }
        }

        if let (Some(lo), Some(hi)) = (&result_type, high_part) {
            return ABIArgInfo::direct_pair(lo.clone(), hi);
        }

        if let Some(result) = &result_type {
            let abi_type = cir_type_to_abi_type(self.info, ty);

            if *result == abi_type {
                return ABIArgInfo::direct();
            }

            if result.as_integer_bits(self.info) == abi_type.as_integer_bits(self.info) {
                return ABIArgInfo::direct();
            }

            return ABIArgInfo::direct_coerce(result.clone());
        }

        // fallback
        ABIArgInfo::direct_coerce(ABIType::Integer(64))
    }

    // TODO
    fn classify_func(&self, ty: &CIRFuncTy) -> ABIArgInfo {
        todo!();

        // let mut params_types = Vec::new();
        // let mut params_infos = Vec::new();
        // let mut has_sret = false;

        // // classify return type

        // let ret_layout = type_layout(&target.info, &fn_ty.ret);

        // let ret_info = {
        //     if fn_ty.ret.is_scalar() {
        //         ABIArgInfo::Direct { coerce_to: None }
        //     } else if ret_layout.size <= 16 {
        //         ABIArgInfo::Direct { coerce_to: None }
        //     } else {
        //         has_sret = true;
        //         params_types.push(CIRTy::Pointer(fn_ty.ret.clone()));
        //         ABIArgInfo::Indirect { by_val: false }
        //     }
        // };

        // // classify static params

        // for param_ty in &fn_ty.params {
        //     let param_layout = type_layout(&target.info, param_ty);

        //     let arg_info = {
        //         if param_ty.is_scalar() {
        //             params_types.push(param_ty.clone());
        //             ABIArgInfo::Direct { coerce_to: None }
        //         } else if param_layout.size <= 16 {
        //             params_types.push(param_ty.clone());
        //             ABIArgInfo::Direct { coerce_to: None }
        //         } else {
        //             params_types.push(CIRTy::Pointer(Box::new(param_ty.clone())));
        //             ABIArgInfo::Indirect { by_val: false }
        //         }
        //     };

        //     params_infos.push(arg_info);
        // }

        // ABIFunctionInfo {
        //     params_infos,
        //     params_types,
        //     ret_info,
        // }
    }

    // TODO
    fn classify_return(&self, ty: &CIRTy) -> ABIArgInfo {
        todo!();
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
    let struct_union_fields = ty
        .as_struct()
        .map(|struct_ty| struct_ty.fields)
        .or(ty.as_union().map(|union_ty| union_ty.fields))
        .unwrap();

    let layout = type_layout(info, ty);

    if layout.size > 64 {
        return; // keep memory class
    }

    // re-classify
    if offset_base < 8 {
        *lo_class = RegisterClass::NoClass;
    } else {
        *hi_class = RegisterClass::NoClass;
    }

    for field_ty in &struct_union_fields {
        let field_layout = type_layout(info, field_ty);
        let field_offset = offset_base + field_layout.align;

        if layout.size > 16 && (!is_union && field_layout.size != layout.size) {
            *lo_class = RegisterClass::Memory;
            classify_post_merge(layout.size, lo_class, hi_class);
            return;
        }

        // check alignment (not aligned?)
        if field_offset % field_layout.align != 0 {
            *lo_class = RegisterClass::Memory;
            classify_post_merge(layout.size, lo_class, hi_class);
            return;
        }

        let mut field_lo_class = RegisterClass::NoClass;
        let mut field_hi_class = RegisterClass::NoClass;
        classify(info, field_ty, offset_base, &mut field_lo_class, &mut field_hi_class);
        *lo_class = classify_merge(*lo_class, field_lo_class);
        *hi_class = classify_merge(*hi_class, field_hi_class);

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

    // if size > 16 and lo isn't SSE or hi isn't SSEUP, default to Memory
    if size > 16 && (*lo_class != RegisterClass::SSE || *hi_class != RegisterClass::SSEUP) {
        *lo_class = RegisterClass::Memory;
        return;
    }

    // if hi is SSEUP but lo isn't SSE or SSEUP, convert hi to SSE
    if *hi_class == RegisterClass::SSEUP && *lo_class != RegisterClass::SSE && *lo_class != RegisterClass::SSEUP {
        *hi_class = RegisterClass::SSE;
    }
}
