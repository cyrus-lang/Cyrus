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
use crate::x86_64_sysv::X86_64SysV;
use cyrusc_abi::target::{Arch, TargetAbi, TargetInfo, TypeLayout};
use cyrusc_tast::types::PlainType;

pub mod x86_64_sysv;

pub fn get_abi_handler(info: &TargetInfo) -> Box<dyn TargetAbi> {
    match (info.arch, info.os) {
        (Arch::X86_64, _) => Box::new(X86_64SysV::new()),
        // (Arch::X86_64, OS::Windows) => Box::new(X86_64Win64::new()),
        // (Arch::Aarch64, _) => Box::new(AArch64Generic::new()),
        _ => panic!("No ABI implementation found for this target"),
    }
}

fn type_layout_of_plain(info: &TargetInfo, plain_type: &PlainType) -> TypeLayout {
    let ptr_size = if info.is_64bit() { 8 } else { 4 };

    match plain_type {
        // Target-Dependent Pointer-Sized Types
        PlainType::UIntPtr | PlainType::IntPtr | PlainType::ISize | PlainType::USize => {
            TypeLayout::new(ptr_size, ptr_size)
        }

        // Fixed-Width Integers
        PlainType::Int8 | PlainType::UInt8 | PlainType::Bool => TypeLayout::new(1, 1),
        PlainType::Int16 | PlainType::UInt16 => TypeLayout::new(2, 2),
        PlainType::Int32 | PlainType::UInt32 | PlainType::Int | PlainType::UInt => TypeLayout::new(4, 4),
        PlainType::Int64 | PlainType::UInt64 => TypeLayout::new(8, 8),
        PlainType::Int128 | PlainType::UInt128 => {
            // On x86_64/AArch64, i128 is 16-byte aligned
            TypeLayout::new(16, 16)
        }

        // Floats
        PlainType::Float16 => TypeLayout::new(2, 2),
        PlainType::Float32 => TypeLayout::new(4, 4),
        PlainType::Float64 => TypeLayout::new(8, 8),
        PlainType::Float128 => TypeLayout::new(16, 16),

        // Special Types
        PlainType::Char => TypeLayout::new(1, 1),
        PlainType::Void => TypeLayout::new(0, 1),
        PlainType::Null => TypeLayout::new(ptr_size, ptr_size),
    }
}
