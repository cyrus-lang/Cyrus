// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                                                         │
// │  Cyrus Programming Language                                             │
// │  https://github.com/cyrus-lang/Cyrus                                    │
// │                                                                         │
// │  A general-purpose, statically-typed, manually memory-managed           │
// │  programming language designed for performance-critical applications.   │
// │                                                                         │
// │  Copyright (c) 2026 The Cyrus Programming Language Project              │
// │                                                                         │
// │  This program is free software: you can redistribute it and/or modify   │
// │  it under the terms of the GNU General Public License as published by   │
// │  the Free Software Foundation, either version 3 of the License, or      │
// │  (at your option) any later version.                                    │
// │                                                                         │
// │  This program is distributed in the hope that it will be useful,        │
// │  but WITHOUT ANY WARRANTY; without even the implied warranty of         │
// │  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the           │
// │  GNU General Public License for more details.                           │
// │                                                                         │
// │  You should have received a copy of the GNU General Public License      │
// │  along with this program. If not, see <https://www.gnu.org/licenses/>.  │
// │                                                                         │
// └─────────────────────────────────────────────────────────────────────────┘

/* 
 * Copyright (c) 2026 The Cyrus Programming Language Project
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
