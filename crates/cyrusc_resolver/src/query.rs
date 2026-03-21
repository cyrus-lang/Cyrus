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

use crate::Resolver;
use cyrusc_internal::symbols::{
    symbols::{
        ResolvedEnum, ResolvedFunc, ResolvedGlobalVar, ResolvedInterface, ResolvedMethod, ResolvedStruct,
        ResolvedTypedef, ResolvedUnion, ResolvedVar, SymbolEntry, SymbolEntryKind,
    },
    table::{GlobalSymbolQuery, SymbolQuery},
};
use cyrusc_tast::{ModuleID, SymbolID};

impl<'diag> GlobalSymbolQuery for Resolver<'diag> {
    /// Look up a symbol identifier by name within a specific module.
    fn lookup_symbol_id(&self, module: ModuleID, name: &str) -> Option<SymbolID> {
        let registry = self.global_symbols_registry.inner.lock().unwrap();
        registry.get(&module)?.names.get(name).cloned()
    }

    /// Retrieve the full semantic entry for a symbol by its name.
    fn resolve_symbol_entry(&self, module: ModuleID, name: &str) -> Option<SymbolEntry> {
        let symbol_id = self.lookup_symbol_id(module, name)?;
        self.resolve_global_symbol(symbol_id)
    }

    /// Retrieve the semantic entry for a specific symbol identifier.
    fn resolve_global_symbol(&self, symbol: SymbolID) -> Option<SymbolEntry> {
        let registry = self.global_symbols_registry.inner.lock().unwrap();

        for table in registry.values() {
            if let Some(entry) = table.entries.get(&symbol) {
                return Some(entry.clone());
            }
        }
        None
    }

    /// Perform a deep resolution of a symbol, following aliases or redirects.
    fn resolve_global_symbol_deep(&self, symbol: SymbolID) -> Option<SymbolEntry> {
        let mut current_entry = self.resolve_global_symbol(symbol)?;

        // if the entry is an Alias, resolve what it points to.
        while let SymbolEntryKind::ProxiedSymbol(module_id, symbol_id) = &current_entry.kind {
            current_entry = self.resolve_global_symbol(*symbol_id)?;
        }

        Some(current_entry)
    }
}

impl<'diag> SymbolQuery for Resolver<'diag> {
    /// Resolve a symbol ID to a variable within the given scope.
    fn resolve_var(&self, id: SymbolID) -> Option<ResolvedVar> {
        match self.resolve_global_symbol(id)?.kind {
            SymbolEntryKind::Var(resolved_var) => Some(resolved_var),
            _ => None,
        }
    }

    /// Resolve a symbol ID to a method definition.
    fn resolve_method(&self, id: SymbolID) -> Option<ResolvedMethod> {
        match self.resolve_global_symbol(id)?.kind {
            SymbolEntryKind::Method(resolved_method) => Some(resolved_method),
            _ => None,
        }
    }

    /// Resolve a symbol ID to a global function.
    fn resolve_func(&self, id: SymbolID) -> Option<ResolvedFunc> {
        match self.resolve_global_symbol(id)?.kind {
            SymbolEntryKind::Func(resolved_func) => Some(resolved_func),
            _ => None,
        }
    }

    /// Resolve a symbol ID to a type definition (typedef).
    fn resolve_typedef(&self, id: SymbolID) -> Option<ResolvedTypedef> {
        match self.resolve_global_symbol(id)?.kind {
            SymbolEntryKind::Typedef(resolved_typedef) => Some(resolved_typedef),
            _ => None,
        }
    }

    /// Resolve a symbol ID to a global variable.
    fn resolve_global_var(&self, id: SymbolID) -> Option<ResolvedGlobalVar> {
        match self.resolve_global_symbol(id)?.kind {
            SymbolEntryKind::GlobalVar(resolved_global_var) => Some(resolved_global_var),
            _ => None,
        }
    }

    /// Resolve a symbol ID to an interface/trait definition.
    fn resolve_interface(&self, id: SymbolID) -> Option<ResolvedInterface> {
        match self.resolve_global_symbol(id)?.kind {
            SymbolEntryKind::Interface(resolved_interface) => Some(resolved_interface),
            _ => None,
        }
    }

    /// Resolve a symbol ID to a union definition.
    fn resolve_union(&self, id: SymbolID) -> Option<ResolvedUnion> {
        match self.resolve_global_symbol(id)?.kind {
            SymbolEntryKind::Union(resolved_union) => Some(resolved_union),
            _ => None,
        }
    }

    /// Resolve a symbol ID to an enum definition.
    fn resolve_enum(&self, id: SymbolID) -> Option<ResolvedEnum> {
        match self.resolve_global_symbol(id)?.kind {
            SymbolEntryKind::Enum(resolved_enum) => Some(resolved_enum),
            _ => None,
        }
    }

    /// Resolve a symbol ID to a struct definition.
    fn resolve_struct(&self, id: SymbolID) -> Option<ResolvedStruct> {
        match self.resolve_global_symbol(id)?.kind {
            SymbolEntryKind::Struct(resolved_struct) => Some(resolved_struct),
            _ => None,
        }
    }
}
