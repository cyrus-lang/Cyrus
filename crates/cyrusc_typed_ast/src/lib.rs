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

use cyrusc_source_loc::FileID;

use crate::stmts::TypedStmt;
use std::fmt;

pub mod exprs;
pub mod format;
pub mod generics;
pub mod sigs;
pub mod stmts;
mod tests;
pub mod types;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolID(pub u32);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct LabelID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VTableID(pub u32);

#[derive(Debug, Clone)]
pub struct TypedProgramTree {
    pub file_id: FileID,
    pub body: Vec<TypedStmt>,
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
