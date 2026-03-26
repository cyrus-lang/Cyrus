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
use cyrusc_typed_ast::{
    ModuleID, SymbolID,
    generics::monomorph::{MonomorphFuncEntry, MonomorphID, SpecializedFuncEntry},
};
use std::collections::HashMap;

pub trait SymbolQuery: Sync + Send {
    fn lookup_var(&self, symbol_id: SymbolID) -> Option<ResolvedVar>;
    fn lookup_global_var(&self, symbol_id: SymbolID) -> Option<ResolvedGlobalVar>;
    fn lookup_method(&self, symbol_id: SymbolID) -> Option<ResolvedMethod>;
    fn lookup_func(&self, symbol_id: SymbolID) -> Option<ResolvedFunc>;
    fn lookup_typedef(&self, symbol_id: SymbolID) -> Option<ResolvedTypedef>;
    fn lookup_union(&self, symbol_id: SymbolID) -> Option<ResolvedUnion>;
    fn lookup_enum(&self, symbol_id: SymbolID) -> Option<ResolvedEnum>;
    fn lookup_struct(&self, symbol_id: SymbolID) -> Option<ResolvedStruct>;
    fn lookup_interface(&self, symbol_id: SymbolID) -> Option<ResolvedInterface>;

    fn lookup_symbol_id(&self, name: &str) -> Option<SymbolID>;
    fn lookup_symbol_id_in_module(&self, module_id: ModuleID, name: &str) -> Option<SymbolID>;
    fn lookup_symbol_entry(&self, name: &str) -> Option<SymbolEntry>;
    fn lookup_global_symbol(&self, symbol_id: SymbolID) -> Option<SymbolEntry>;

    fn lookup_monomorph_func(&self, monomorph_id: MonomorphID) -> Option<MonomorphFuncEntry>;
    fn lookup_specialized_func_instance(&self, monomorph_id: MonomorphID) -> Option<SpecializedFuncEntry>;

    /// Returns the declaration name of a symbol visible from the given module
    /// and optional local scope.
    ///
    /// If the symbol cannot be resolved, a fallback string is returned.
    fn format_symbol_name(&self, symbol_id: SymbolID) -> String;

    fn lookup_module_name(&self, module_id: ModuleID) -> Option<String>;
}

pub trait SymbolEntryMut {
    fn with_var_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedVar) -> R;

    fn with_global_var_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedGlobalVar) -> R;

    fn with_method_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedMethod) -> R;

    fn with_func_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedFunc) -> R;

    fn with_typedef_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedTypedef) -> R;

    fn with_union_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedUnion) -> R;

    fn with_enum_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedEnum) -> R;

    fn with_struct_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedStruct) -> R;

    fn with_interface_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedInterface) -> R;
}

/// A collection of symbols and their metadata within a specific scope or module.
#[derive(Debug)]
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
