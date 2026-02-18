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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetArch {
    X86_64,
    Aarch64,
    RiscV64,
    Wasm32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetOS {
    Linux,
    Windows,
    MacOS,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetObjectFormat {
    Elf,
    MachO,
    Coff,
}

pub struct TargetInfo {
    pub arch: TargetArch,
    pub os: TargetOS,
    pub format: TargetObjectFormat,
}

impl TargetInfo {
    /// Generates the triple string used by LLVM
    pub fn triple(&self) -> String {
        let arch_str = match self.arch {
            TargetArch::X86_64 => "x86_64",
            TargetArch::Aarch64 => "aarch64",
            TargetArch::RiscV64 => "riscv64",
            TargetArch::Wasm32 => "wasm32",
        };

        let os_str = match self.os {
            TargetOS::Linux => "unknown-linux-gnu",
            TargetOS::Windows => "pc-windows-msvc",
            TargetOS::MacOS => "apple-darwin",
            TargetOS::Unknown => "unknown-unknown",
        };

        format!("{}-{}", arch_str, os_str)
    }

    pub fn emit_target_data_layout(&self) -> &'static str {
        use TargetArch::*;
        use TargetOS::*;
        match (self.arch, self.os) {
            (X86_64, Windows) => "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128",
            (X86_64, MacOS) => "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128",
            (X86_64, _) => "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128",
            (Aarch64, MacOS) => "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32",
            (Aarch64, Windows) => {
                "e-m:w-p270:32:32-p271:32:32-p272:64:64-p:64:64-i32:32-i64:64-i128:128-n32:64-S128-Fn32"
            }
            (Wasm32, _) => "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-n32:64-S128-ni:1:10:20",
            _ => panic!("Target combination not supported yet!"),
        }
    }

    pub fn is_64bit(&self) -> bool {
        self.int_bit_width() == 64
    }

    pub fn int_bit_width(&self) -> u32 {
        match self.arch {
            TargetArch::X86_64 | TargetArch::Aarch64 | TargetArch::RiscV64 => 64,
            TargetArch::Wasm32 => 32,
        }
    }

    pub fn pointer_bit_width(&self) -> u32 {
        match self.arch {
            TargetArch::X86_64 | TargetArch::Aarch64 | TargetArch::RiscV64 => 64,
            TargetArch::Wasm32 => 32,
        }
    }

    pub fn pointer_size(&self) -> u32 {
        match self.arch {
            TargetArch::X86_64 | TargetArch::Aarch64 | TargetArch::RiscV64 => 8,
            TargetArch::Wasm32 => 4,
        }
    }
}

pub enum AbiArgInfo {
    Direct { coerce_to: Option<String> },
    Extend { signed: bool },
    Indirect { by_val: bool },
    Expand,
    Ignore,
}

pub struct TypeLayout {
    pub size: u32,
    pub align: u32,
    pub is_aggregate: bool,
}

impl TypeLayout {
    pub fn normal(size: u32, align: u32) -> Self {
        Self {
            size,
            align,
            is_aggregate: false,
        }
    }

    pub fn aggregate(size: u32, align: u32) -> Self {
        Self {
            size,
            align,
            is_aggregate: true,
        }
    }
}

pub trait TargetAbi: Send + Sync {
    /// Classify how a return value should be handled based on its physical layout
    fn classify_return(&self, layout: &TypeLayout) -> AbiArgInfo;

    /// Classify how a function argument should be handled
    fn classify_arg(&self, layout: &TypeLayout) -> AbiArgInfo;

    /// The stack alignment required by the ABI (usually 16 for modern 64-bit)
    fn stack_alignment(&self) -> u32;
}

pub struct Target {
    pub info: TargetInfo,
    pub data_layout: String,
    /// The dynamic ABI handler
    pub target_abi: Box<dyn TargetAbi>,
}

impl Target {
    pub fn new(info: TargetInfo, target_abi: Box<dyn TargetAbi>) -> Self {
        Self {
            data_layout: info.emit_target_data_layout().to_string(),
            info,
            target_abi,
        }
    }
}
