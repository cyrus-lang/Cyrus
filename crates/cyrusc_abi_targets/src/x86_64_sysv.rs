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
use cyrusc_abi::target::{AbiArgInfo, TargetAbi, TypeLayout};

pub struct X86_64SysV;

impl X86_64SysV {
    pub fn new() -> Self {
        Self
    }
}

impl TargetAbi for X86_64SysV {
    fn classify_arg(&self, layout: &TypeLayout) -> AbiArgInfo {
        if layout.is_aggregate {
            // 1. If the size is greater than 16 bytes, it passes in memory.
            if layout.size > 16 {
                return AbiArgInfo::Indirect { by_val: true };
            }

            // 2. If it fits in 16 bytes, System V tries to pack it into registers.
            // For LLVM, the cleanest way to do this is to coerce the struct
            // into an array of i64s or a specific integer.
            match layout.size {
                1..=8 => AbiArgInfo::Direct {
                    coerce_to: Some("i64".to_string()),
                },
                9..=16 => AbiArgInfo::Direct {
                    coerce_to: Some("{ i64, i64 }".to_string()),
                },
                _ => AbiArgInfo::Ignore, // ZSTs
            }
        } else {
            // Primitives like i32, i64, ptr pass directly in single registers.
            AbiArgInfo::Direct { coerce_to: None }
        }
    }

    fn classify_return(&self, layout: &TypeLayout) -> AbiArgInfo {
        // System V return rules for structs are nearly identical to arguments.
        self.classify_arg(layout)
    }

    fn stack_alignment(&self) -> u32 {
        16
    }
}
