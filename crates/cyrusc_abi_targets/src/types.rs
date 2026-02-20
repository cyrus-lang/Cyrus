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

use crate::{ABIArgInfo, ABITargetInfo};
use std::fmt::Debug;

/// Target-agnostic ABI type representation
#[derive(Debug)]
pub enum ABIType {
    Void,
    Integer(u32),
    Float(FloatKind),
    Pointer,
    Vector { element_ty: Box<ABIType>, lanes: u32 },
    Array { element_ty: Box<ABIType>, count: usize },
    Struct(Vec<ABIType>),
    Union(Vec<ABIType>),
    TargetDependent(Box<dyn ABITargetDependentType>),
    // Enum(...) // TODO
}

/// Trait for target-dependent ABI types
pub trait ABITargetDependentType: Debug + Send + Sync {
    /// Clone boxed trait object
    fn clone_box(&self) -> Box<dyn ABITargetDependentType>;

    /// Get size in bytes for this target
    fn size(&self, target: &ABITargetInfo) -> usize;

    /// Get alignment for this target
    fn align(&self, target: &ABITargetInfo) -> usize;

    /// Get register class for this target
    fn register_class(&self, target: &ABITargetInfo) -> RegisterClass;

    /// Provide target-specific classification
    fn classify(&self, _target: &ABITargetInfo) -> Option<ABIArgInfo>;
}

/// Simple floating point kinds
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FloatKind {
    F16,
    F32,
    F64,
    F128,
}

/// Register class for argument passing
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RegisterClass {
    Integer,
    Float,
    Vector,
    Memory,
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
            ABIType::Struct(types) => ABIType::Struct(types.clone()),
            ABIType::Union(types) => ABIType::Union(types.clone()),
            ABIType::TargetDependent(t) => ABIType::TargetDependent(t.clone_box()),
        }
    }
}
