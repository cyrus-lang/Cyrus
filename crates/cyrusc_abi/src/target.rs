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

pub trait TargetAbi {
    /// Classifies how a return value should be passed
    fn classify_return(&self, layout: &TypeLayout) -> ArgRequirement;

    /// Classifies how a function argument should be passed
    fn classify_arg(&self, layout: &TypeLayout) -> ArgRequirement;

    /// Specific alignment requirements for the stack (e.g., 16-byte for x64)
    fn stack_alignment(&self) -> u32;

    /// Returns the name of the calling convention (e.g., "ccc", "win64", "sysv")
    fn calling_convention(&self) -> &str;
}

pub enum ArgRequirement {
    Direct { coerce_to: Option<String> }, // String represents LLVM type (e.g., "i64")
    Extend { signed: bool },              // Sign/Zero extension for small ints
    Indirect { by_val: bool },            // Pass by pointer
    Expand,                               // Break struct into individual fields
    Ignore,                               // For ZSTs like `void` or `struct {}`
}

pub enum Arch {
    X86_64,
    Aarch64,
    RiscV64,
    Wasm32,
}

pub struct Target {
    pub arch: Arch,
    pub triple: String,
    pub data_layout: String, // LLVM DataLayout string
    pub abi: Box<dyn TargetAbi>,
}

impl Target {
    pub fn new(triple_str: &str) -> Self {
        let triple = triple_str.to_lowercase();

        // Simple triple parsing logic
        let arch = if triple.contains("x86_64") {
            Arch::X86_64
        } else if triple.contains("aarch64") {
            Arch::Aarch64
        } else if triple.contains("riscv64") {
            Arch::RiscV64
        } else if triple.contains("wasm32") {
            Arch::Wasm32
        } else {
            panic!("Unsupported architecture in triple: {}", triple_str);
        };

        // Determine ABI based on OS/Arch combo
        let abi: Box<dyn TargetAbi> = match arch {
            Arch::X86_64 => {
                if triple.contains("windows") || triple.contains("msvc") {
                    Box::new(X86_64Win64Abi)
                } else {
                    Box::new(X86_64SysVAbi)
                }
            }
            Arch::Aarch64 => Box::new(AArch64GenericAbi),
            _ => todo!("Implement other ABIs"),
        };

        // Standard LLVM DataLayout strings
        let data_layout = match arch {
            Arch::X86_64 => "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128",
            Arch::Aarch64 => "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128",
            _ => "",
        };

        Self {
            arch,
            triple: triple_str.to_string(),
            data_layout: data_layout.to_string(),
            abi,
        }
    }
}
