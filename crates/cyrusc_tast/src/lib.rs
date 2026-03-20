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
use std::path::PathBuf;

pub mod exprs;
pub mod format;
pub mod generics;
pub mod sigs;
pub mod stmts;
mod tests;
pub mod types;
pub mod vtable;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ScopeID(u32);

// FIXME: Optimization required here!
pub type SymbolID = u32;
pub type ModuleID = u64;

// FIXME: Consider to remove it after optimizing `SymbolID`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct DeclID {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
}

pub type LabelID = u32;

#[derive(Debug, Clone)]
pub struct TypedProgramTree {
    pub body: Vec<TypedStmt>,
    pub file_path: PathBuf,
    pub module_id: ModuleID,
    pub module_name: String,
}

unsafe impl Send for TypedProgramTree {}
unsafe impl Sync for TypedProgramTree {}
