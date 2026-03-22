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

use crate::stmts::TypedStmt;
use rand::Rng;
use std::{fmt, path::PathBuf};

pub mod exprs;
pub mod format;
pub mod generics;
pub mod sigs;
pub mod stmts;
mod tests;
pub mod types;
pub mod vtable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolID(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleID(u64);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct LabelID(u32);

impl ModuleID {
    pub fn new() -> Self {
        let mut rng = rand::rng();
        Self(rng.random::<u64>())
    }
}

impl SymbolID {
    pub fn new() -> Self {
        let mut rng = rand::rng();
        Self(rng.random::<u32>())
    }
}

impl LabelID {
    pub fn new() -> Self {
        let mut rng = rand::rng();
        Self(rng.random::<u32>())
    }
}

#[derive(Debug, Clone)]
pub struct TypedProgramTree {
    pub body: Vec<TypedStmt>,
    pub file_path: PathBuf,
    pub module_id: ModuleID,
    pub module_name: String,
}

unsafe impl Send for TypedProgramTree {}
unsafe impl Sync for TypedProgramTree {}

impl fmt::Display for ModuleID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
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
