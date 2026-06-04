// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::abi::target::ABITargetInfo;
use cyrusc_typed_ast::types::PlainType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum X86_64TargetDependentType {
    IntPtr,
    UIntPtr,
    ISize,
    USize,
    Int,
    UInt,
}

impl X86_64TargetDependentType {
    pub fn size(&self, target: &ABITargetInfo) -> u32 {
        match self {
            X86_64TargetDependentType::IntPtr
            | X86_64TargetDependentType::UIntPtr
            | X86_64TargetDependentType::ISize
            | X86_64TargetDependentType::USize => target.pointer_size(),

            X86_64TargetDependentType::Int | X86_64TargetDependentType::UInt => 4, // 32 bits on x86-64
        }
    }

    pub fn align(&self, target: &ABITargetInfo) -> u32 {
        match self {
            X86_64TargetDependentType::IntPtr
            | X86_64TargetDependentType::UIntPtr
            | X86_64TargetDependentType::ISize
            | X86_64TargetDependentType::USize => target.pointer_align(),

            X86_64TargetDependentType::Int | X86_64TargetDependentType::UInt => 4, // 4-byte alignment
        }
    }

    pub fn bit_width(&self, target: &ABITargetInfo) -> u32 {
        self.size(target) * 8
    }
}

impl From<&PlainType> for X86_64TargetDependentType {
    fn from(plain_type: &PlainType) -> Self {
        match plain_type {
            PlainType::IntPtr => X86_64TargetDependentType::IntPtr,
            PlainType::UIntPtr => X86_64TargetDependentType::UIntPtr,
            PlainType::ISize => X86_64TargetDependentType::ISize,
            PlainType::USize => X86_64TargetDependentType::USize,
            PlainType::Int => X86_64TargetDependentType::Int,
            PlainType::UInt => X86_64TargetDependentType::UInt,
            _ => panic!("Not a target-dependent type: {:?}", plain_type),
        }
    }
}
