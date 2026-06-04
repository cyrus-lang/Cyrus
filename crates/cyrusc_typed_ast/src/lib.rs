// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::stmts::TypedStmt;
use cyrusc_source_loc::FileID;
use std::fmt;

pub mod builtins;
pub mod decls;
pub mod exprs;
pub mod format;
pub mod stmts;
pub mod substitute;
pub mod types;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolID(pub u32);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct LabelID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VTableID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenericParamID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyID(pub u32);

#[derive(Debug, Clone)]
pub struct TypedProgramTree {
    pub file_id: FileID,
    pub body: Vec<TypedStmt>,
    pub file_name: String,
    pub module_name: String,
}

unsafe impl Send for TypedProgramTree {}
unsafe impl Sync for TypedProgramTree {}

impl SymbolID {
    pub const fn placeholder() -> Self {
        Self(0)
    }
}

impl From<u32> for SymbolID {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl fmt::Display for SymbolID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for LabelID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for VTableID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.to_string())
    }
}
