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

use crate::{ABITargetInfo, targets::x86_64_sysv::types::X86_64TargetDependentType};
use std::fmt::Debug;

/// Target-agnostic ABI type representation
#[derive(Debug, PartialEq, Eq)]
pub enum ABIType {
    Void,
    Integer(u32),
    Float(ABIFloatKind),
    Pointer,
    Vector { element_ty: Box<ABIType>, lanes: u32 },
    Array { element_ty: Box<ABIType>, count: usize },
    Struct(Vec<ABIType>, bool),
    Union(Vec<ABIType>),
    TargetIntegerType(TargetIntegerType),
    // Enum(...) // TODO
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetIntegerType {
    X86_64(X86_64TargetDependentType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ABIFloatKind {
    F16,
    F32,
    F64,
    F128,
}

impl Clone for ABIType {
    fn clone(&self) -> Self {
        match self {
            ABIType::Void => ABIType::Void,
            ABIType::Integer(size) => ABIType::Integer(*size),
            ABIType::Float(kind) => ABIType::Float(kind.clone()),
            ABIType::Pointer => ABIType::Pointer,
            ABIType::Vector { element_ty, lanes } => ABIType::Vector {
                element_ty: element_ty.clone(),
                lanes: *lanes,
            },
            ABIType::Array { element_ty, count } => ABIType::Array {
                element_ty: element_ty.clone(),
                count: *count,
            },
            ABIType::Struct(types, is_packed) => ABIType::Struct(types.clone(), *is_packed),
            ABIType::Union(types) => ABIType::Union(types.clone()),
            ABIType::TargetIntegerType(target_integer_type) => ABIType::TargetIntegerType(target_integer_type.clone()),
        }
    }
}

impl ABIType {
    pub fn as_integer_bits(&self, info: &ABITargetInfo) -> Option<u32> {
        match self {
            ABIType::Integer(bit_width) => Some(*bit_width),
            ABIType::TargetIntegerType(target_integer_type) => Some(target_integer_type.bit_width(info)),
            _ => None,
        }
    }
}

impl TargetIntegerType {
    pub fn size(&self, info: &ABITargetInfo) -> u32 {
        match self {
            TargetIntegerType::X86_64(x86_64_target_dependent_type) => x86_64_target_dependent_type.size(info),
        }
    }

    pub fn align(&self, target: &ABITargetInfo) -> u32 {
        match self {
            TargetIntegerType::X86_64(x86_64_target_dependent_type) => x86_64_target_dependent_type.align(target),
        }
    }

    pub fn bit_width(&self, target: &ABITargetInfo) -> u32 {
        match self {
            TargetIntegerType::X86_64(x86_64_target_dependent_type) => x86_64_target_dependent_type.bit_width(target),
        }
    }
}
