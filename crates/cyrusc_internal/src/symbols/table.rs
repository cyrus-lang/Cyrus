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
use cyrusc_source_loc::FileID;
use cyrusc_typed_ast::{
    SymbolID,
    generics::monomorph::{MonomorphFuncEntry, MonomorphID, SpecializedFuncEntry},
};
use std::collections::HashMap;

pub trait Query: Sync + Send {
    fn get_var(&self, symbol_id: SymbolID) -> Option<ResolvedVar>;
    fn get_global_var(&self, symbol_id: SymbolID) -> Option<ResolvedGlobalVar>;
    fn get_method(&self, symbol_id: SymbolID) -> Option<ResolvedMethod>;
    fn get_func(&self, symbol_id: SymbolID) -> Option<ResolvedFunc>;
    fn get_typedef(&self, symbol_id: SymbolID) -> Option<ResolvedTypedef>;
    fn get_union(&self, symbol_id: SymbolID) -> Option<ResolvedUnion>;
    fn get_enum(&self, symbol_id: SymbolID) -> Option<ResolvedEnum>;
    fn get_struct(&self, symbol_id: SymbolID) -> Option<ResolvedStruct>;
    fn get_interface(&self, symbol_id: SymbolID) -> Option<ResolvedInterface>;

    fn lookup_symbol_id(&self, scope_id: SymbolID, name: &str) -> Option<SymbolID>;
    fn lookup_symbol_id_in_scope(&self, scope_id: SymbolID, name: &str) -> Option<SymbolID>;
    fn lookup_symbol_entry(&self, scope_id: SymbolID, name: &str) -> Option<SymbolEntry>;
    fn get_symbol(&self, symbol_id: SymbolID) -> Option<SymbolEntry>;

    fn lookup_monomorph_func(&self, monomorph_id: MonomorphID) -> Option<MonomorphFuncEntry>;
    fn lookup_specialized_func_instance(&self, monomorph_id: MonomorphID) -> Option<SpecializedFuncEntry>;

    /// Returns the declaration name of a symbol visible from the given module
    /// and optional local scope.
    ///
    /// If the symbol cannot be resolved, a fallback string is returned.
    fn format_symbol_name(&self, symbol_id: SymbolID) -> String;

    fn lookup_module_name(&self, file_id: FileID) -> Option<String>;
}

///  Analyzer‑Only Mutation Helpers
///
///  These accessors MUST NOT be used during symbol resolution.
///
///  - Resolver phase is responsible for assigning the final `SymbolEntryKind`
///    using `with_global_symbol_mut` (Unresolved -> Var/Func/Struct/...).
///
///  - These helpers are ONLY valid in the analyzer/type‑checking phase, where
///    symbol kinds are already fixed. They mutate the *interior* of an already-
///    resolved variant and will assert/fail if called on an Unresolved entry.
///
///  Using these in the resolver will silently no‑op or corrupt symbol state.
///  Resolver code MUST set `entry.kind` explicitly and never rely on these.
///
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
#[derive(Debug, Clone)]
pub struct ScopeTable {
    /// Mapping from symbol names to their identifiers for fast lookup.
    names: HashMap<String, SymbolID>,
}

impl ScopeTable {
    pub fn new() -> Self {
        Self { names: HashMap::new() }
    }

    /// Lookup a symbol symbol_id by name in the current scope only.
    pub fn lookup(&self, name: &str) -> Option<SymbolID> {
        self.names.get(name).copied()
    }

    /// Bind a name to SymbolID in this scope.
    /// Returns the previous symbol symbol_id if it was shadowing or duplicate.
    pub fn bind(&mut self, name: String, symbol_id: SymbolID) -> Option<SymbolID> {
        self.names.insert(name, symbol_id)
    }

    /// Check if name already exists.
    pub fn contains(&self, name: &str) -> bool {
        self.names.contains_key(name)
    }
}
