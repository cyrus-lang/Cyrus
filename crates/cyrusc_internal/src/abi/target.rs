// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    abi::{
        args::{ABIArgInfo, ABIFunctionInfo, ABIRetInfo},
        helpers::Registers,
        targets::x86_64::classify::X86_64,
        types::{ABIFloatKind, ABIType},
    },
    cir::types::{CIRFuncType, CIRType},
};

pub trait TargetABI: Send + Sync {
    fn stack_alignment(&self) -> u32;
    fn classify_return(&self, ty: &CIRType) -> ABIRetInfo;
    fn classify_argument(&self, ty: &CIRType, free_int_regs: u32, is_named: bool) -> (ABIArgInfo, Registers);
    fn classify_func(&self, fn_ty: &CIRFuncType) -> Result<ABIFunctionInfo, String>;
    fn apply_variadic_argument_promote(&self, ty: &CIRType) -> CIRType;
}

pub fn create_target_abi<'a>(target_info: ABITargetInfo) -> Result<Box<dyn TargetABI + 'a>, String> {
    match (target_info.arch, target_info.os, target_info.format) {
        (ABITargetArch::X86_64, ABITargetOS::Linux, ABITargetObjectFormat::Elf) => {
            Ok(Box::new(X86_64::new(target_info)))
        }
        (ABITargetArch::Aarch64, ABITargetOS::Linux, ABITargetObjectFormat::Elf) => {
            unimplemented!("AArch64 Linux ABI not implemented yet")
        }
        _ => Err(format!("Unsupported target: {}.", target_info.triple())),
    }
}

pub struct ABITarget {
    pub info: ABITargetInfo,
    pub data_layout: String,
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

#[derive(Debug, Clone)]
pub struct ABITargetInfo {
    pub arch: ABITargetArch,
    pub os: ABITargetOS,
    pub format: ABITargetObjectFormat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RegisterClass {
    NoClass,
    Memory,
    Integer,
    SSE,
    SSEUP,
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

    pub fn abi_size_of(&self, abi_type: &ABIType) -> u64 {
        match abi_type {
            ABIType::Integer(bits) => (*bits as u64 + 7) / 8,
            ABIType::Float(kind) => match kind {
                ABIFloatKind::F16 => 2,
                ABIFloatKind::F32 => 4,
                ABIFloatKind::F64 => 8,
                ABIFloatKind::F128 => 16,
            },
            ABIType::Pointer => 8, // 64-bit pointers
            ABIType::Vector { element_ty, lanes } => self.abi_size_of(element_ty) * (*lanes as u64),
            ABIType::Array { element_ty, count } => self.abi_size_of(element_ty) * (*count as u64),
            ABIType::Struct(fields, _) => fields.iter().map(|ty| self.abi_size_of(ty)).sum(),
            ABIType::Union(fields) => fields.iter().map(|ty| self.abi_size_of(ty)).max().unwrap_or(1),
            ABIType::TargetIntegerType(target_int) => target_int.size(self).into(),
            ABIType::Void => 0,
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

    pub fn pointer_align(&self) -> u32 {
        match self.arch {
            ABITargetArch::X86_64 | ABITargetArch::Aarch64 | ABITargetArch::RiscV64 => 8,
            ABITargetArch::Wasm32 => 4,
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
