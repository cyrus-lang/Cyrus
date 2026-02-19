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
use crate::x86_64_sysv::{X86_64SysV, classify_function_x86_64_sysv};
use cyrusc_abi::target::{
    ABIArgInfo, Target, TargetABI, TargetArch, TargetInfo, TargetOS, TargetObjectFormat, TypeLayout,
};
use cyrusc_cir::types::{CIRFuncTy, CIRTy};
use cyrusc_tast::types::PlainType;
use inkwell::llvm_sys::{
    core::{
        LLVMArrayType2, LLVMDoubleTypeInContext, LLVMFP128TypeInContext, LLVMFloatTypeInContext, LLVMHalfTypeInContext,
        LLVMIntTypeInContext, LLVMStructTypeInContext, LLVMX86FP80TypeInContext,
    },
    prelude::{LLVMContextRef, LLVMTypeRef},
};

pub mod x86_64_sysv;

pub struct ABIFunctionInfo {
    pub ret_info: ABIArgInfo,
    pub params_infos: Vec<ABIArgInfo>,
    pub params_types: Vec<CIRTy>,
    pub has_sret: bool,
}

/// Factory function to create a TargetAbi instance
pub fn create_target_abi(target_info: &TargetInfo) -> Result<Box<dyn TargetABI>, String> {
    match (target_info.arch, target_info.os, target_info.format) {
        (TargetArch::X86_64, TargetOS::Linux, TargetObjectFormat::Elf) => Ok(Box::new(X86_64SysV::new())),
        (TargetArch::X86_64, TargetOS::MacOS, TargetObjectFormat::MachO) => {
            // TODO: implement X86_64 MacOS ABI
            unimplemented!("X86_64 MacOS ABI not implemented yet")
        }
        (TargetArch::X86_64, TargetOS::Windows, TargetObjectFormat::Coff) => {
            // TODO: implement Windows x64 ABI
            unimplemented!("X86_64 Windows ABI not implemented yet")
        }
        (TargetArch::Aarch64, TargetOS::Linux, TargetObjectFormat::Elf) => {
            // TODO: implement AArch64 Linux ABI
            unimplemented!("AArch64 Linux ABI not implemented yet")
        }
        _ => Err(format!("Unsupported target: {}.", target_info.triple())),
    }
}

pub fn type_layout(info: &TargetInfo, ty: &CIRTy) -> TypeLayout {
    match ty {
        CIRTy::PlainType(plain_type) => plain_type_layout(info, plain_type),
        CIRTy::Const(ty) => type_layout(info, ty.const_inner()),
        CIRTy::Pointer(_) => {
            let size = info.pointer_size();
            TypeLayout::normal(size, size)
        }
        CIRTy::Struct(struct_ty) => {
            let mut offset = 0;
            let mut max_align = 1;

            for ty in &struct_ty.fields {
                let field_layout = type_layout(info, ty);
                offset = align_to(offset, field_layout.align);
                offset += field_layout.size;
                max_align = max_align.max(field_layout.align);
            }

            let total_size = align_to(offset, max_align);
            TypeLayout::aggregate(total_size, max_align)
        }
        CIRTy::Union(union_ty) => {
            let mut max_size = 0;
            let mut max_align = 1;

            for ty in &union_ty.fields {
                let field_layout = type_layout(info, ty);

                max_size = max_size.max(field_layout.size);
                max_align = max_size.max(field_layout.align);
            }

            let total_size = align_to(max_size, max_align);
            TypeLayout::aggregate(total_size, max_align)
        }
        CIRTy::Enum(enum_ty) => todo!(),
        CIRTy::FuncType(_) => {
            let size = info.pointer_size();
            TypeLayout::normal(size, size)
        }
        CIRTy::Tuple(tuple_ty) => {
            // same layout as struct (lowered to struct in codegen)

            let mut offset = 0;
            let mut max_align = 0;

            for ty in &tuple_ty.items {
                let element_layout = type_layout(info, ty);
                offset = align_to(offset, element_layout.align);
                offset += element_layout.size;
                max_align = max_align.max(element_layout.align);
            }

            let total_size = align_to(offset, max_align);
            TypeLayout::aggregate(total_size, max_align)
        }
        CIRTy::Array(array_ty) => {
            let element_layout = type_layout(info, &array_ty.ty);
            let total_size = element_layout.size * array_ty.len as u32;
            TypeLayout::aggregate(total_size, element_layout.align)
        }
        CIRTy::Dynamic(_) => {
            let size = info.pointer_size() * 2; // data_ptr + vtable_ptr
            TypeLayout::normal(size, info.pointer_size())
        }
    }
}

fn plain_type_layout(info: &TargetInfo, plain_type: &PlainType) -> TypeLayout {
    use PlainType::*;

    match plain_type {
        UIntPtr | IntPtr | ISize | USize => {
            let size = info.pointer_size();
            TypeLayout::normal(size, size)
        }

        Int8 | UInt8 | Bool => TypeLayout::normal(1, 1),
        Int16 | UInt16 => TypeLayout::normal(2, 2),
        Int32 | UInt32 | Int | UInt => TypeLayout::normal(4, 4),
        Int64 | UInt64 => TypeLayout::normal(8, 8),
        Int128 | UInt128 => {
            let align = match info.arch {
                TargetArch::X86_64 | TargetArch::Aarch64 => 16,
                TargetArch::RiscV64 => 16,
                TargetArch::Wasm32 => 8,
            };
            TypeLayout::normal(16, align)
        }

        Float16 => TypeLayout::normal(2, 2),
        Float32 => TypeLayout::normal(4, 4),
        Float64 => TypeLayout::normal(8, 8),
        Float128 => {
            let align = match info.arch {
                TargetArch::X86_64 | TargetArch::Aarch64 => 16,
                TargetArch::RiscV64 => 16,
                TargetArch::Wasm32 => 8,
            };
            TypeLayout::normal(16, align)
        }

        Char => TypeLayout::normal(1, 1),
        Void => TypeLayout::normal(0, 1),
        Null => {
            let size = info.pointer_size();
            TypeLayout::normal(size, size)
        }
    }
}

pub fn classify_function(target: &Target, fn_ty: &CIRFuncTy) -> ABIFunctionInfo {
    match &target.info.arch {
        TargetArch::X86_64 => classify_function_x86_64_sysv(target, fn_ty),
        TargetArch::Aarch64 => unimplemented!(), // TODO: Classify function for TargetArch::Aarch64
        TargetArch::RiscV64 => unimplemented!(), // TODO: Classify function for TargetArch::RiscV64
        TargetArch::Wasm32 => unimplemented!(), // TODO: Classify function for TargetArch::Wasm32
    }
}

pub fn llvm_type_from_coerce_str(ctx: LLVMContextRef, ty_str: &str) -> LLVMTypeRef {
    unsafe {
        let ty_str = ty_str.trim();

        // integers
        if ty_str.starts_with('i') {
            let bits = &ty_str[1..];
            if let Ok(n) = bits.parse::<u32>() {
                return LLVMIntTypeInContext(ctx, n);
            } else {
                panic!("Invalid integer bits: {}", bits);
            }
        }

        // floats
        match ty_str {
            "half" => return LLVMHalfTypeInContext(ctx),
            "float" => return LLVMFloatTypeInContext(ctx),
            "double" => return LLVMDoubleTypeInContext(ctx),
            "fp128" => return LLVMFP128TypeInContext(ctx),
            "x86_fp80" => return LLVMX86FP80TypeInContext(ctx),
            _ => {}
        }

        // structs: { ... }
        if ty_str.starts_with('{') && ty_str.ends_with('}') {
            let inner = &ty_str[1..ty_str.len() - 1];
            let types: Vec<LLVMTypeRef> = inner
                .split(',')
                .map(|x| llvm_type_from_coerce_str(ctx, x.trim()))
                .collect();
            return LLVMStructTypeInContext(ctx, types.as_ptr() as *mut LLVMTypeRef, types.len() as u32, 0);
        }

        // arrays [type x N]
        if ty_str.starts_with('[') && ty_str.ends_with(']') {
            let inner = &ty_str[1..ty_str.len() - 1]; // remove [ ]
            let mut parts = inner.split('x').map(|x| x.trim());
            let elem_str = parts.next().expect("Invalid array type");
            let count_str = parts.next().expect("Invalid array type");
            let elem_ty = llvm_type_from_coerce_str(ctx, elem_str);
            let count = count_str.parse::<usize>().expect("Invalid array count");
            return LLVMArrayType2(elem_ty, count as u64);
        }

        panic!("Unsupported type string: '{}'", ty_str);
    }
}

/// Aligns offset to align-bytes
fn align_to(offset: u32, align: u32) -> u32 {
    (offset + align - 1) / align * align
}
