// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_ast::abi::Callconv;

#[derive(Debug, Clone, Copy)]
pub enum LLVMCallConv {
    CCallConv = 0,
    FastCallConv = 8,
    ColdCallConv = 9,
    X86StdcallCallConv = 64,
    X86FastcallCallConv = 65,
    ArmAapcsCallConv = 67,
    X86ThisCall = 70,
    X86_64SysV = 78,
    X86_64Win64 = 79,
    X86VectorCall = 80,
}

impl From<&Callconv> for LLVMCallConv {
    fn from(conv: &Callconv) -> Self {
        match conv {
            Callconv::C => LLVMCallConv::CCallConv,
            Callconv::Fast => LLVMCallConv::FastCallConv,
            Callconv::Cold => LLVMCallConv::ColdCallConv,
            Callconv::Aapcs => LLVMCallConv::ArmAapcsCallConv,
            Callconv::Stdcall => LLVMCallConv::X86StdcallCallConv,
            Callconv::Fastcall => LLVMCallConv::X86FastcallCallConv,
            Callconv::Thiscall => LLVMCallConv::X86ThisCall,
            Callconv::Vectorcall => LLVMCallConv::X86VectorCall,
            Callconv::SysV64 => LLVMCallConv::X86_64SysV,
            Callconv::Win64 => LLVMCallConv::X86_64Win64,
            Callconv::System => {
                if cfg!(target_os = "windows") {
                    LLVMCallConv::X86_64Win64
                } else {
                    LLVMCallConv::X86_64SysV
                }
            }
            Callconv::Naked | Callconv::Interrupt => LLVMCallConv::CCallConv,
        }
    }
}

impl LLVMCallConv {
    pub fn as_u32(&self) -> u32 {
        *self as u32
    }
}
