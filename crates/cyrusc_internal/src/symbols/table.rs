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

use crate::symbols::symbols::{
    ResolvedEnum, ResolvedFunc, ResolvedGlobalVar, ResolvedInterface, ResolvedMethod, ResolvedStruct, ResolvedTypedef,
    ResolvedUnion, ResolvedVar, SymbolEntry,
};
use cyrusc_typed_ast::{ModuleID, SymbolID};
use std::collections::HashMap;

pub trait GlobalSymbolQuery {
    fn lookup_symbol_id(&self, name: &str) -> Option<SymbolID>;
    fn lookup_symbol_id_in_module(&self, module_id: ModuleID, name: &str) -> Option<SymbolID>;
    fn lookup_symbol_entry(&self, name: &str) -> Option<SymbolEntry>;
    fn lookup_global_symbol(&self, symbol: SymbolID) -> Option<SymbolEntry>;
    fn lookup_global_symbol_deep(&self, symbol: SymbolID) -> Option<SymbolEntry>;
}

pub trait SymbolQuery {
    fn lookup_var(&self, symbol_id: SymbolID) -> Option<ResolvedVar>;
    fn lookup_global_var(&self, symbol_id: SymbolID) -> Option<ResolvedGlobalVar>;
    fn lookup_method(&self, symbol_id: SymbolID) -> Option<ResolvedMethod>;
    fn lookup_func(&self, symbol_id: SymbolID) -> Option<ResolvedFunc>;
    fn lookup_typedef(&self, symbol_id: SymbolID) -> Option<ResolvedTypedef>;
    fn lookup_union(&self, symbol_id: SymbolID) -> Option<ResolvedUnion>;
    fn lookup_enum(&self, symbol_id: SymbolID) -> Option<ResolvedEnum>;
    fn lookup_struct(&self, symbol_id: SymbolID) -> Option<ResolvedStruct>;
    fn lookup_interface(&self, symbol_id: SymbolID) -> Option<ResolvedInterface>;
}

/// A collection of symbols and their metadata within a specific scope or module.
pub struct SymbolTable {
    /// Mapping from unique symbol identifiers to their semantic entries.
    pub entries: HashMap<SymbolID, SymbolEntry>,
    /// Mapping from symbol names to their identifiers for fast lookup.
    pub names: HashMap<String, SymbolID>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            names: HashMap::new(),
        }
    }
}
