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
    targets::x86_64_sysv::{X86_64SysV, classify_func_x86_64_sysv},
    types::ABIType,
};
use cyrusc_cir::types::{CIRFuncTy, CIRTy};

mod helpers;
pub mod layouts;
mod targets;
mod types;

pub trait TargetABI: Send + Sync {
    fn classify_return(&self, layout: &ABITypeLayout) -> ABIArgInfo;
    fn classify_arg(&self, layout: &ABITypeLayout) -> ABIArgInfo;
    fn stack_alignment(&self) -> u32;
}

pub fn create_target_abi(target_info: &ABITargetInfo) -> Result<Box<dyn TargetABI>, String> {
    match (target_info.arch, target_info.os, target_info.format) {
        (ABITargetArch::X86_64, ABITargetOS::Linux, ABITargetObjectFormat::Elf) => Ok(Box::new(X86_64SysV::new())),
        (ABITargetArch::X86_64, ABITargetOS::MacOS, ABITargetObjectFormat::MachO) => {
            // TODO: implement X86_64 MacOS ABI
            unimplemented!("X86_64 MacOS ABI not implemented yet")
        }
        (ABITargetArch::X86_64, ABITargetOS::Windows, ABITargetObjectFormat::Coff) => {
            // TODO: implement Windows x64 ABI
            unimplemented!("X86_64 Windows ABI not implemented yet")
        }
        (ABITargetArch::Aarch64, ABITargetOS::Linux, ABITargetObjectFormat::Elf) => {
            // TODO: implement AArch64 Linux ABI
            unimplemented!("AArch64 Linux ABI not implemented yet")
        }
        _ => Err(format!("Unsupported target: {}.", target_info.triple())),
    }
}

pub fn classify_func(target: &ABITarget, fn_ty: &CIRFuncTy) -> ABIFunctionInfo {
    match (target.info.os, target.info.arch) {
        (ABITargetOS::Linux, ABITargetArch::X86_64) => classify_func_x86_64_sysv(target, fn_ty),
        _ => unimplemented!("Target not supported currently."),
    }
}

#[derive(Debug, Clone)]
pub enum ABIArgInfo {
    Direct { coerce_to: Option<ABIType> },
    Extend { signed: bool },
    Indirect { by_val: bool },
    Expand,
    Ignore,
}

pub struct ABITypeLayout {
    pub size: u32,
    pub align: u32,
    pub is_aggregate: bool,
}

pub struct ABITarget {
    pub info: ABITargetInfo,
    pub data_layout: String,
    /// The dynamic ABI handler
    pub target_abi: Box<dyn TargetABI>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ABITargetArch {
    X86_64,
    Aarch64,
    RiscV64,
    Wasm32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ABITargetOS {
    Linux,
    Windows,
    MacOS,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ABITargetObjectFormat {
    Elf,
    MachO,
    Coff,
}

pub struct ABITargetInfo {
    pub arch: ABITargetArch,
    pub os: ABITargetOS,
    pub format: ABITargetObjectFormat,
}

pub struct ABIFunctionInfo {
    pub ret_info: ABIArgInfo,
    pub params_infos: Vec<ABIArgInfo>,
    pub params_types: Vec<CIRTy>,
    pub has_sret: bool,
}

impl ABITargetInfo {
    /// Generates the triple string used by LLVM
    pub fn triple(&self) -> String {
        let arch_str = match self.arch {
            ABITargetArch::X86_64 => "x86_64",
            ABITargetArch::Aarch64 => "aarch64",
            ABITargetArch::RiscV64 => "riscv64",
            ABITargetArch::Wasm32 => "wasm32",
        };

        let os_str = match self.os {
            ABITargetOS::Linux => "unknown-linux-gnu",
            ABITargetOS::Windows => "pc-windows-msvc",
            ABITargetOS::MacOS => "apple-darwin",
            ABITargetOS::Unknown => "unknown-unknown",
        };

        format!("{}-{}", arch_str, os_str)
    }

    pub fn emit_target_data_layout(&self) -> &'static str {
        use ABITargetArch::*;
        use ABITargetOS::*;
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
            ABITargetArch::X86_64 | ABITargetArch::Aarch64 | ABITargetArch::RiscV64 => 64,
            ABITargetArch::Wasm32 => 32,
        }
    }

    pub fn pointer_bit_width(&self) -> u32 {
        match self.arch {
            ABITargetArch::X86_64 | ABITargetArch::Aarch64 | ABITargetArch::RiscV64 => 64,
            ABITargetArch::Wasm32 => 32,
        }
    }

    pub fn pointer_size(&self) -> u32 {
        match self.arch {
            ABITargetArch::X86_64 | ABITargetArch::Aarch64 | ABITargetArch::RiscV64 => 8,
            ABITargetArch::Wasm32 => 4,
        }
    }
}

impl ABITypeLayout {
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

impl ABITarget {
    pub fn new(info: ABITargetInfo, target_abi: Box<dyn TargetABI>) -> Self {
        Self {
            data_layout: info.emit_target_data_layout().to_string(),
            info,
            target_abi,
        }
    }
}

impl ABIArgInfo {
    pub fn is_indirect_by_val(&self) -> bool {
        match self {
            ABIArgInfo::Indirect { by_val } => *by_val,
            _ => false,
        }
    }
}
