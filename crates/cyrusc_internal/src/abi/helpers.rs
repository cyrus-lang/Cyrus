// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::cir::types::CIRType;

#[derive(Debug, Clone, Copy, Default)]
pub struct Registers {
    pub int_regs: u32,
    pub sse_regs: u32,
}

pub(crate) fn align_offset(offset: u32, align: u32) -> u32 {
    (offset + align - 1) / align * align
}

#[inline]
pub(crate) fn is_cir_type_abi_aggregate(cir_type: &CIRType) -> bool {
    match cir_type {
        CIRType::Struct(_) | CIRType::Enum(_) | CIRType::Union(_) => true,
        _ => false,
    }
}
