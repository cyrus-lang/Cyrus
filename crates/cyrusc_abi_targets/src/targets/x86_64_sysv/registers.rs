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

use crate::types::RegisterClass;

pub const GENERAL_PURPOSE_REGISTERS_COUNT: usize = 16; // 16x 64-bit registers
pub const XMM_REGISTERS_COUNT: usize = 32; // XMM0-XMM31
pub const YMM_REGISTERS_COUNT: usize = 32; // YMM0-YMM31  
pub const ZMM_REGISTERS_COUNT: usize = 32; // ZMM0-ZMM31
pub const FPU_REGISTERS_COUNT: usize = 8; // ST0-ST7
pub const MMX_REGISTERS_COUNT: usize = 8; // MM0-MM7
pub const MASK_REGISTERS_COUNT: usize = 8; // K0-K7
pub const SEGMENT_REGISTERS_COUNT: usize = 6; // CS, DS, SS, ES, FS, GS
pub const CONTROL_REGISTERS_COUNT: usize = 6; // CR0, CR1, CR2, CR3, CR4, CR8
pub const DEBUG_REGISTERS_COUNT: usize = 6; // DR0, DR1, DR2, DR3, DR6, DR7

/// x86_64 general purpose registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GeneralPurposeRegister {
    // 64-bit registers
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,

    // 32-bit subsets
    EAX,
    ECX,
    EDX,
    EBX,
    ESP,
    EBP,
    ESI,
    EDI,
    R8D,
    R9D,
    R10D,
    R11D,
    R12D,
    R13D,
    R14D,
    R15D,

    // 16-bit subsets
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
    R8W,
    R9W,
    R10W,
    R11W,
    R12W,
    R13W,
    R14W,
    R15W,

    // 8-bit subsets
    AL,
    CL,
    DL,
    BL,
    SPL,
    BPL,
    SIL,
    DIL,
    R8B,
    R9B,
    R10B,
    R11B,
    R12B,
    R13B,
    R14B,
    R15B,
    AH,
    CH,
    DH,
    BH,
}

/// x86_64 SSE/AVX registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum XMMRegister {
    // 128-bit SSE registers
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,
    XMM16,
    XMM17,
    XMM18,
    XMM19,
    XMM20,
    XMM21,
    XMM22,
    XMM23,
    XMM24,
    XMM25,
    XMM26,
    XMM27,
    XMM28,
    XMM29,
    XMM30,
    XMM31,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum YMMRegister {
    // 256-bit AVX registers
    YMM0,
    YMM1,
    YMM2,
    YMM3,
    YMM4,
    YMM5,
    YMM6,
    YMM7,
    YMM8,
    YMM9,
    YMM10,
    YMM11,
    YMM12,
    YMM13,
    YMM14,
    YMM15,
    YMM16,
    YMM17,
    YMM18,
    YMM19,
    YMM20,
    YMM21,
    YMM22,
    YMM23,
    YMM24,
    YMM25,
    YMM26,
    YMM27,
    YMM28,
    YMM29,
    YMM30,
    YMM31,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ZMMRegister {
    // 512-bit AVX-512 registers
    ZMM0,
    ZMM1,
    ZMM2,
    ZMM3,
    ZMM4,
    ZMM5,
    ZMM6,
    ZMM7,
    ZMM8,
    ZMM9,
    ZMM10,
    ZMM11,
    ZMM12,
    ZMM13,
    ZMM14,
    ZMM15,
    ZMM16,
    ZMM17,
    ZMM18,
    ZMM19,
    ZMM20,
    ZMM21,
    ZMM22,
    ZMM23,
    ZMM24,
    ZMM25,
    ZMM26,
    ZMM27,
    ZMM28,
    ZMM29,
    ZMM30,
    ZMM31,
}

/// x87 FPU registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FpuRegister {
    ST0,
    ST1,
    ST2,
    ST3,
    ST4,
    ST5,
    ST6,
    ST7,
}

/// MMX registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MMXRegister {
    MM0,
    MM1,
    MM2,
    MM3,
    MM4,
    MM5,
    MM6,
    MM7,
}

/// Mask registers (AVX-512)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MaskRegister {
    K0,
    K1,
    K2,
    K3,
    K4,
    K5,
    K6,
    K7,
}

/// Segment registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SegmentRegister {
    CS,
    DS,
    SS,
    ES,
    FS,
    GS,
}

/// Control registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ControlRegister {
    CR0,
    CR1,
    CR2,
    CR3,
    CR4,
    CR8,
}

/// Debug registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DebugRegister {
    DR0,
    DR1,
    DR2,
    DR3,
    DR6,
    DR7,
}

/// Flags register
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RFlags;

/// Unified x86_64 register enum
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum X86_64Register {
    // General purpose
    GPR(GeneralPurposeRegister),

    // SIMD registers
    XMM(XMMRegister),
    YMM(YMMRegister),
    ZMM(ZMMRegister),

    // FPU/MMX
    FPU(FpuRegister),
    MMX(MMXRegister),

    // Special
    Mask(MaskRegister),
    Segment(SegmentRegister),
    Control(ControlRegister),
    Debug(DebugRegister),
    RFlags,

    // Pseudo-registers for ABI classification
    RegisterPair {
        first: Box<X86_64Register>,
        second: Box<X86_64Register>,
    },
}

/// Register class mapping for x86_64
impl X86_64Register {
    /// Get the register class for ABI classification
    pub fn register_class(&self) -> RegisterClass {
        match self {
            X86_64Register::GPR(gpr) => match gpr {
                // 64-bit integer registers
                GeneralPurposeRegister::RAX
                | GeneralPurposeRegister::RCX
                | GeneralPurposeRegister::RDX
                | GeneralPurposeRegister::RBX
                | GeneralPurposeRegister::RSP
                | GeneralPurposeRegister::RBP
                | GeneralPurposeRegister::RSI
                | GeneralPurposeRegister::RDI
                | GeneralPurposeRegister::R8
                | GeneralPurposeRegister::R9
                | GeneralPurposeRegister::R10
                | GeneralPurposeRegister::R11
                | GeneralPurposeRegister::R12
                | GeneralPurposeRegister::R13
                | GeneralPurposeRegister::R14
                | GeneralPurposeRegister::R15 => RegisterClass::Integer,

                // 32-bit integer registers (still INTEGER class)
                GeneralPurposeRegister::EAX
                | GeneralPurposeRegister::ECX
                | GeneralPurposeRegister::EDX
                | GeneralPurposeRegister::EBX
                | GeneralPurposeRegister::ESP
                | GeneralPurposeRegister::EBP
                | GeneralPurposeRegister::ESI
                | GeneralPurposeRegister::EDI
                | GeneralPurposeRegister::R8D
                | GeneralPurposeRegister::R9D
                | GeneralPurposeRegister::R10D
                | GeneralPurposeRegister::R11D
                | GeneralPurposeRegister::R12D
                | GeneralPurposeRegister::R13D
                | GeneralPurposeRegister::R14D
                | GeneralPurposeRegister::R15D => RegisterClass::Integer,

                // 16-bit integer registers
                GeneralPurposeRegister::AX
                | GeneralPurposeRegister::CX
                | GeneralPurposeRegister::DX
                | GeneralPurposeRegister::BX
                | GeneralPurposeRegister::SP
                | GeneralPurposeRegister::BP
                | GeneralPurposeRegister::SI
                | GeneralPurposeRegister::DI
                | GeneralPurposeRegister::R8W
                | GeneralPurposeRegister::R9W
                | GeneralPurposeRegister::R10W
                | GeneralPurposeRegister::R11W
                | GeneralPurposeRegister::R12W
                | GeneralPurposeRegister::R13W
                | GeneralPurposeRegister::R14W
                | GeneralPurposeRegister::R15W => RegisterClass::Integer,

                // 8-bit integer registers
                GeneralPurposeRegister::AL
                | GeneralPurposeRegister::CL
                | GeneralPurposeRegister::DL
                | GeneralPurposeRegister::BL
                | GeneralPurposeRegister::SPL
                | GeneralPurposeRegister::BPL
                | GeneralPurposeRegister::SIL
                | GeneralPurposeRegister::DIL
                | GeneralPurposeRegister::R8B
                | GeneralPurposeRegister::R9B
                | GeneralPurposeRegister::R10B
                | GeneralPurposeRegister::R11B
                | GeneralPurposeRegister::R12B
                | GeneralPurposeRegister::R13B
                | GeneralPurposeRegister::R14B
                | GeneralPurposeRegister::R15B
                | GeneralPurposeRegister::AH
                | GeneralPurposeRegister::CH
                | GeneralPurposeRegister::DH
                | GeneralPurposeRegister::BH => RegisterClass::Integer,
            },

            X86_64Register::XMM(_) => RegisterClass::SSE,
            X86_64Register::YMM(_) => RegisterClass::SSE, // YMM is extension of XMM
            X86_64Register::ZMM(_) => RegisterClass::SSE, // ZMM is extension of XMM

            X86_64Register::FPU(_) => RegisterClass::X87,
            X86_64Register::MMX(_) => RegisterClass::MMX,

            X86_64Register::Mask(_) => RegisterClass::Mask,
            X86_64Register::Segment(_) => RegisterClass::Segment,
            X86_64Register::Control(_) => RegisterClass::Control,
            X86_64Register::Debug(_) => RegisterClass::Debug,
            X86_64Register::RFlags => RegisterClass::Flags,

            X86_64Register::RegisterPair { first, second } => {
                // Pairs are used for 128-bit integers or complex numbers
                // They combine two register classes
                match (first.register_class(), second.register_class()) {
                    (RegisterClass::Integer, RegisterClass::Integer) => RegisterClass::Integer,
                    (RegisterClass::SSE, RegisterClass::SSE) => RegisterClass::SSE,
                    (RegisterClass::Integer, RegisterClass::SSE) => RegisterClass::IntegerSSE,
                    (RegisterClass::SSE, RegisterClass::Integer) => RegisterClass::IntegerSSE,
                    _ => RegisterClass::Memory,
                }
            }
        }
    }

    /// Get the size of the register in bytes
    pub fn size(&self) -> usize {
        match self {
            X86_64Register::GPR(gpr) => match gpr {
                GeneralPurposeRegister::RAX
                | GeneralPurposeRegister::RCX
                | GeneralPurposeRegister::RDX
                | GeneralPurposeRegister::RBX
                | GeneralPurposeRegister::RSP
                | GeneralPurposeRegister::RBP
                | GeneralPurposeRegister::RSI
                | GeneralPurposeRegister::RDI
                | GeneralPurposeRegister::R8
                | GeneralPurposeRegister::R9
                | GeneralPurposeRegister::R10
                | GeneralPurposeRegister::R11
                | GeneralPurposeRegister::R12
                | GeneralPurposeRegister::R13
                | GeneralPurposeRegister::R14
                | GeneralPurposeRegister::R15 => 8,

                GeneralPurposeRegister::EAX
                | GeneralPurposeRegister::ECX
                | GeneralPurposeRegister::EDX
                | GeneralPurposeRegister::EBX
                | GeneralPurposeRegister::ESP
                | GeneralPurposeRegister::EBP
                | GeneralPurposeRegister::ESI
                | GeneralPurposeRegister::EDI
                | GeneralPurposeRegister::R8D
                | GeneralPurposeRegister::R9D
                | GeneralPurposeRegister::R10D
                | GeneralPurposeRegister::R11D
                | GeneralPurposeRegister::R12D
                | GeneralPurposeRegister::R13D
                | GeneralPurposeRegister::R14D
                | GeneralPurposeRegister::R15D => 4,

                GeneralPurposeRegister::AX
                | GeneralPurposeRegister::CX
                | GeneralPurposeRegister::DX
                | GeneralPurposeRegister::BX
                | GeneralPurposeRegister::SP
                | GeneralPurposeRegister::BP
                | GeneralPurposeRegister::SI
                | GeneralPurposeRegister::DI
                | GeneralPurposeRegister::R8W
                | GeneralPurposeRegister::R9W
                | GeneralPurposeRegister::R10W
                | GeneralPurposeRegister::R11W
                | GeneralPurposeRegister::R12W
                | GeneralPurposeRegister::R13W
                | GeneralPurposeRegister::R14W
                | GeneralPurposeRegister::R15W => 2,

                GeneralPurposeRegister::AL
                | GeneralPurposeRegister::CL
                | GeneralPurposeRegister::DL
                | GeneralPurposeRegister::BL
                | GeneralPurposeRegister::SPL
                | GeneralPurposeRegister::BPL
                | GeneralPurposeRegister::SIL
                | GeneralPurposeRegister::DIL
                | GeneralPurposeRegister::R8B
                | GeneralPurposeRegister::R9B
                | GeneralPurposeRegister::R10B
                | GeneralPurposeRegister::R11B
                | GeneralPurposeRegister::R12B
                | GeneralPurposeRegister::R13B
                | GeneralPurposeRegister::R14B
                | GeneralPurposeRegister::R15B
                | GeneralPurposeRegister::AH
                | GeneralPurposeRegister::CH
                | GeneralPurposeRegister::DH
                | GeneralPurposeRegister::BH => 1,
            },

            X86_64Register::XMM(_) => 16,
            X86_64Register::YMM(_) => 32,
            X86_64Register::ZMM(_) => 64,

            X86_64Register::FPU(_) => 10, // 80-bit extended precision
            X86_64Register::MMX(_) => 8,

            X86_64Register::Mask(_) => 8,
            X86_64Register::Segment(_) => 2,
            X86_64Register::Control(_) => 8,
            X86_64Register::Debug(_) => 8,
            X86_64Register::RFlags => 8,

            X86_64Register::RegisterPair { first, second } => first.size() + second.size(),
        }
    }

    /// Check if register is volatile (caller-saved) for System V ABI
    pub fn is_volatile_sysv(&self) -> bool {
        match self {
            X86_64Register::GPR(gpr) => match gpr {
                GeneralPurposeRegister::RAX
                | GeneralPurposeRegister::RCX
                | GeneralPurposeRegister::RDX
                | GeneralPurposeRegister::RSI
                | GeneralPurposeRegister::RDI
                | GeneralPurposeRegister::R8
                | GeneralPurposeRegister::R9
                | GeneralPurposeRegister::R10
                | GeneralPurposeRegister::R11 => true,
                GeneralPurposeRegister::RBX
                | GeneralPurposeRegister::RBP
                | GeneralPurposeRegister::RSP
                | GeneralPurposeRegister::R12
                | GeneralPurposeRegister::R13
                | GeneralPurposeRegister::R14
                | GeneralPurposeRegister::R15 => false,
                _ => true, // Sub-registers follow same rules
            },
            X86_64Register::XMM(xmm) => match xmm {
                XMMRegister::XMM0
                | XMMRegister::XMM1
                | XMMRegister::XMM2
                | XMMRegister::XMM3
                | XMMRegister::XMM4
                | XMMRegister::XMM5
                | XMMRegister::XMM6
                | XMMRegister::XMM7 => true,

                XMMRegister::XMM8
                | XMMRegister::XMM9
                | XMMRegister::XMM10
                | XMMRegister::XMM11
                | XMMRegister::XMM12
                | XMMRegister::XMM13
                | XMMRegister::XMM14
                | XMMRegister::XMM15 => false,

                XMMRegister::XMM16
                | XMMRegister::XMM17
                | XMMRegister::XMM18
                | XMMRegister::XMM19
                | XMMRegister::XMM20
                | XMMRegister::XMM21
                | XMMRegister::XMM22
                | XMMRegister::XMM23
                | XMMRegister::XMM24
                | XMMRegister::XMM25
                | XMMRegister::XMM26
                | XMMRegister::XMM27
                | XMMRegister::XMM28
                | XMMRegister::XMM29
                | XMMRegister::XMM30
                | XMMRegister::XMM31 => true,
            },

            X86_64Register::YMM(ymm) => {
                // YMM registers follow same volatile rules as their XMM counterparts
                let idx = *ymm as usize;
                idx <= 7 || idx >= 16
            }

            X86_64Register::ZMM(zmm) => {
                // ZMM registers follow same volatile rules as their XMM counterparts
                let idx = *zmm as usize;
                idx <= 7 || idx >= 16
            }

            X86_64Register::FPU(_) => true, // All x87 registers are volatile
            X86_64Register::MMX(_) => true, // All MMX registers are volatile

            X86_64Register::Mask(mask) => match mask {
                MaskRegister::K0
                | MaskRegister::K1
                | MaskRegister::K2
                | MaskRegister::K3
                | MaskRegister::K4
                | MaskRegister::K5
                | MaskRegister::K6
                | MaskRegister::K7 => true,
            },
            X86_64Register::Segment(_) => false, // Segment registers are preserved
            X86_64Register::Control(_) => false, // Control registers are not touched
            X86_64Register::Debug(_) => false,   // Debug registers are preserved
            X86_64Register::RFlags => true,      // Flags are volatile
            X86_64Register::RegisterPair { first, second } => first.is_volatile_sysv() || second.is_volatile_sysv(),
        }
    }

    /// Check if register is volatile (caller-saved) for Microsoft x64 ABI
    pub fn is_volatile_ms(&self) -> bool {
        match self {
            X86_64Register::GPR(gpr) => match gpr {
                GeneralPurposeRegister::RAX
                | GeneralPurposeRegister::RCX
                | GeneralPurposeRegister::RDX
                | GeneralPurposeRegister::R8
                | GeneralPurposeRegister::R9
                | GeneralPurposeRegister::R10
                | GeneralPurposeRegister::R11 => true,
                GeneralPurposeRegister::RBX
                | GeneralPurposeRegister::RBP
                | GeneralPurposeRegister::RDI
                | GeneralPurposeRegister::RSI
                | GeneralPurposeRegister::RSP
                | GeneralPurposeRegister::R12
                | GeneralPurposeRegister::R13
                | GeneralPurposeRegister::R14
                | GeneralPurposeRegister::R15 => false,
                _ => true,
            },

            X86_64Register::XMM(xmm) => match xmm {
                XMMRegister::XMM0
                | XMMRegister::XMM1
                | XMMRegister::XMM2
                | XMMRegister::XMM3
                | XMMRegister::XMM4
                | XMMRegister::XMM5 => true,

                XMMRegister::XMM6
                | XMMRegister::XMM7
                | XMMRegister::XMM8
                | XMMRegister::XMM9
                | XMMRegister::XMM10
                | XMMRegister::XMM11
                | XMMRegister::XMM12
                | XMMRegister::XMM13
                | XMMRegister::XMM14
                | XMMRegister::XMM15 => false,

                x if x == &XMMRegister::XMM16
                    || x == &XMMRegister::XMM17
                    || x == &XMMRegister::XMM18
                    || x == &XMMRegister::XMM19
                    || x == &XMMRegister::XMM20
                    || x == &XMMRegister::XMM21
                    || x == &XMMRegister::XMM22
                    || x == &XMMRegister::XMM23
                    || x == &XMMRegister::XMM24
                    || x == &XMMRegister::XMM25
                    || x == &XMMRegister::XMM26
                    || x == &XMMRegister::XMM27
                    || x == &XMMRegister::XMM28
                    || x == &XMMRegister::XMM29
                    || x == &XMMRegister::XMM30
                    || x == &XMMRegister::XMM31 =>
                {
                    true
                }

                _ => true,
            },

            X86_64Register::YMM(ymm) => {
                let idx = *ymm as usize;
                idx <= 5 || idx >= 16
            }

            X86_64Register::ZMM(zmm) => {
                let idx = *zmm as usize;
                idx <= 5 || idx >= 16
            }

            _ => self.is_volatile_sysv(), // Other registers follow same rules
        }
    }
}

/// Register sets for different purposes
impl X86_64Register {
    /// Integer argument registers for System V ABI (first 6)
    pub fn sysv_int_args() -> Vec<X86_64Register> {
        vec![
            X86_64Register::GPR(GeneralPurposeRegister::RDI),
            X86_64Register::GPR(GeneralPurposeRegister::RSI),
            X86_64Register::GPR(GeneralPurposeRegister::RDX),
            X86_64Register::GPR(GeneralPurposeRegister::RCX),
            X86_64Register::GPR(GeneralPurposeRegister::R8),
            X86_64Register::GPR(GeneralPurposeRegister::R9),
        ]
    }

    /// SSE argument registers for System V ABI (first 8)
    pub fn sysv_sse_args() -> Vec<X86_64Register> {
        vec![
            X86_64Register::XMM(XMMRegister::XMM0),
            X86_64Register::XMM(XMMRegister::XMM1),
            X86_64Register::XMM(XMMRegister::XMM2),
            X86_64Register::XMM(XMMRegister::XMM3),
            X86_64Register::XMM(XMMRegister::XMM4),
            X86_64Register::XMM(XMMRegister::XMM5),
            X86_64Register::XMM(XMMRegister::XMM6),
            X86_64Register::XMM(XMMRegister::XMM7),
        ]
    }

    /// Integer return registers for System V ABI
    pub fn sysv_int_returns() -> Vec<X86_64Register> {
        vec![
            X86_64Register::GPR(GeneralPurposeRegister::RAX),
            X86_64Register::GPR(GeneralPurposeRegister::RDX),
        ]
    }

    /// SSE return registers for System V ABI
    pub fn sysv_sse_returns() -> Vec<X86_64Register> {
        vec![
            X86_64Register::XMM(XMMRegister::XMM0),
            X86_64Register::XMM(XMMRegister::XMM1),
        ]
    }

    /// Integer argument registers for Microsoft x64 ABI (first 4)
    pub fn ms_int_args() -> Vec<X86_64Register> {
        vec![
            X86_64Register::GPR(GeneralPurposeRegister::RCX),
            X86_64Register::GPR(GeneralPurposeRegister::RDX),
            X86_64Register::GPR(GeneralPurposeRegister::R8),
            X86_64Register::GPR(GeneralPurposeRegister::R9),
        ]
    }

    /// SSE argument registers for Microsoft x64 ABI (first 4)
    pub fn ms_sse_args() -> Vec<X86_64Register> {
        vec![
            X86_64Register::XMM(XMMRegister::XMM0),
            X86_64Register::XMM(XMMRegister::XMM1),
            X86_64Register::XMM(XMMRegister::XMM2),
            X86_64Register::XMM(XMMRegister::XMM3),
        ]
    }

    /// Integer return registers for Microsoft x64 ABI
    pub fn ms_int_returns() -> Vec<X86_64Register> {
        vec![X86_64Register::GPR(GeneralPurposeRegister::RAX)]
    }

    /// SSE return registers for Microsoft x64 ABI
    pub fn ms_sse_returns() -> Vec<X86_64Register> {
        vec![X86_64Register::XMM(XMMRegister::XMM0)]
    }
}
