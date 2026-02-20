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

use inkwell::llvm_sys::{
    core::{
        LLVMArrayType2, LLVMDoubleTypeInContext, LLVMFP128TypeInContext, LLVMFloatTypeInContext, LLVMHalfTypeInContext,
        LLVMIntTypeInContext, LLVMStructTypeInContext, LLVMX86FP80TypeInContext,
    },
    prelude::{LLVMContextRef, LLVMTypeRef},
};

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
