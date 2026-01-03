// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use cyrusc_abi::callconv::CallConv;

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

impl From<&CallConv> for LLVMCallConv {
    fn from(conv: &CallConv) -> Self {
        match conv {
            CallConv::C => LLVMCallConv::CCallConv,
            CallConv::Fast => LLVMCallConv::FastCallConv,
            CallConv::Cold => LLVMCallConv::ColdCallConv,
            CallConv::Aapcs => LLVMCallConv::ArmAapcsCallConv,
            CallConv::Stdcall => LLVMCallConv::X86StdcallCallConv,
            CallConv::Fastcall => LLVMCallConv::X86FastcallCallConv,
            CallConv::Thiscall => LLVMCallConv::X86ThisCall,
            CallConv::Vectorcall => LLVMCallConv::X86VectorCall,
            CallConv::SysV64 => LLVMCallConv::X86_64SysV,
            CallConv::Win64 => LLVMCallConv::X86_64Win64,
            CallConv::System => {
                if cfg!(target_os = "windows") {
                    LLVMCallConv::X86_64Win64
                } else {
                    LLVMCallConv::X86_64SysV
                }
            }
            CallConv::Naked | CallConv::Interrupt => LLVMCallConv::CCallConv,
        }
    }
}

impl LLVMCallConv {
    pub fn as_u32(&self) -> u32 {
        *self as u32
    }
}
