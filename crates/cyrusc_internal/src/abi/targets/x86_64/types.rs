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

use crate::abi::target::ABITargetInfo;
use cyrusc_tast::types::PlainType;

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
