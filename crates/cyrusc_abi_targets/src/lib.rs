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
    targets::x86_64_sysv::classify::X86_64SysV,
    types::{ABIFloatKind, ABIType},
};
use cyrusc_cir::types::{CIRFuncTy, CIRTy};

mod helpers;
pub mod layout;
mod targets;
pub mod types;

pub trait TargetABI: Send + Sync {
    fn stack_alignment(&self) -> u32;
    fn classify_func(&self, fn_ty: &CIRFuncTy) -> ABIFunctionInfo;
    fn classify_return(&self, ty: &CIRTy) -> ABIArgInfo;
    fn classify_argument(&self, ty: &CIRTy, is_named: bool) -> ABIArgInfo;
}

pub fn create_target_abi<'a>(target_info: ABITargetInfo) -> Result<Box<dyn TargetABI + 'a>, String> {
    match (target_info.arch, target_info.os, target_info.format) {
        (ABITargetArch::X86_64, ABITargetOS::Linux, ABITargetObjectFormat::Elf) => {
            Ok(Box::new(X86_64SysV::new(target_info)))
        }
        (ABITargetArch::Aarch64, ABITargetOS::Linux, ABITargetObjectFormat::Elf) => {
            unimplemented!("AArch64 Linux ABI not implemented yet")
        }
        _ => Err(format!("Unsupported target: {}.", target_info.triple())),
    }
}

#[derive(Debug, Clone)]
pub struct ABIArgInfo {
    /// Parameter index range (for LLVM function arguments)
    pub param_index_start: u16,
    pub param_index_end: u16,

    pub kind: ABIArgKind,
    pub attrs: ABIArgAttrs,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ABIArgAttrs {
    /// Passed in register
    pub by_reg: bool,
    /// Zero extend (for bool/small ints)
    pub zero_ext: bool,
    /// Sign extend
    pub sign_ext: bool,
    /// Realign argument
    pub realign: bool,
    /// Pass by value (indirect)
    pub by_val: bool,
}

#[derive(Debug, Clone)]
pub enum ABIArgKind {
    /// Direct register passing (maybe coerced)
    Direct { coerce_to: Option<ABIType> },

    /// Passed in a pair of registers (lo/hi)
    DirectPair { lo: ABIType, hi: ABIType },

    /// Coerced to a different type and passed directly
    DirectCoerce { ty: ABIType },

    /// Expanded into multiple arguments
    Expand {
        /// How to expand
        kind: ExpandKind,
    },

    /// Integer extension
    Extend { signed: bool },

    /// Indirect passing (by pointer)
    Indirect {
        /// Required alignment
        alignment: u32,
        /// Type to pass indirectly
        ty: ABIType,
    },

    /// Ignored argument
    Ignore,
}

#[derive(Debug, Clone)]
pub enum ExpandKind {
    /// Simple expansion (default)
    Simple,

    /// Coerced expansion with offset info
    Coerced {
        offset_hi: u8,
        packed: bool,
        lo: ABIType,
        hi: ABIType,
    },

    /// Struct expansion with field count
    Struct { field_count: u8 },
}

pub struct ABITypeLayout {
    pub size: u32,
    pub align: u32,
    pub is_aggregate: bool,
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

pub struct ABIFunctionInfo {
    pub params_types: Vec<CIRTy>,
    pub params_infos: Vec<ABIArgInfo>,
    pub ret_info: ABIRetInfo,
}

pub struct ABIRetInfo {
    pub ty: ABIType,
    pub kind: ABIRetInfoKind,
}

pub enum ABIRetInfoKind {
    Direct { coerce_to: Option<ABIType> },
    Indirect { sret: bool },
    Ignore,
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

impl Default for ABIArgAttrs {
    fn default() -> Self {
        Self {
            by_reg: false,
            zero_ext: false,
            sign_ext: false,
            realign: false,
            by_val: false,
        }
    }
}

impl ABIArgInfo {
    pub fn direct() -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Direct { coerce_to: None },
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn direct_coerce(ty: ABIType) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::DirectCoerce { ty },
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn direct_pair(lo: ABIType, hi: ABIType) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::DirectPair { lo, hi },
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn indirect(ty: ABIType, alignment: u32) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Indirect { ty, alignment },
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn extend(signed: bool) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Extend { signed },
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn ignore() -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Ignore,
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn with_attrs(mut self, attrs: ABIArgAttrs) -> Self {
        self.attrs = attrs;
        self
    }

    pub fn with_indices(mut self, start: u16, end: u16) -> Self {
        self.param_index_start = start;
        self.param_index_end = end;
        self
    }

    pub fn is_direct(&self) -> bool {
        matches!(
            self.kind,
            ABIArgKind::Direct { .. } | ABIArgKind::DirectCoerce { .. } | ABIArgKind::DirectPair { .. }
        )
    }

    pub fn is_indirect(&self) -> bool {
        matches!(self.kind, ABIArgKind::Indirect { .. })
    }

    pub fn is_indirect_by_val(&self) -> bool {
        self.is_indirect() && self.attrs.by_val
    }

    pub fn is_ignore(&self) -> bool {
        matches!(self.kind, ABIArgKind::Ignore)
    }
}
