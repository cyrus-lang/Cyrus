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
use cyrusc_typed_ast::{ModuleID, SymbolID};

impl GlobalSymbolQuery for Resolver {
    /// Look up a symbol identifier by name.
    fn lookup_symbol_id(&self, name: &str) -> Option<SymbolID> {
        let registry = self.global_symbols_registry.inner.lock().unwrap();

        for table in registry.values() {
            if let Some(symbol_id) = table.names.get(name) {
                return Some(*symbol_id);
            }
        }

        None
    }

    /// Look up a symbol identifier by name within a specific module.
    fn lookup_symbol_id_in_module(&self, module_id: ModuleID, name: &str) -> Option<SymbolID> {
        let registry = self.global_symbols_registry.inner.lock().unwrap();
        let table = registry.get(&module_id)?;
        table.names.get(name).copied()
    }

    /// Retrieve the full semantic entry for a symbol by its name.
    fn lookup_symbol_entry(&self, name: &str) -> Option<SymbolEntry> {
        let symbol_id = self.lookup_symbol_id(name)?;
        self.lookup_global_symbol(symbol_id)
    }

    /// Retrieve the semantic entry for a specific symbol identifier.
    fn lookup_global_symbol(&self, symbol_id: SymbolID) -> Option<SymbolEntry> {
        let registry = self.global_symbols_registry.inner.lock().unwrap();

        for table in registry.values() {
            if let Some(entry) = table.entries.get(&symbol_id) {
                return Some(entry.clone());
            }
        }

        None
    }

    /// Perform a deep resolution of a symbol, following aliases or redirects.
    fn lookup_global_symbol_deep(&self, symbol_id: SymbolID) -> Option<SymbolEntry> {
        let mut current_entry = self.lookup_global_symbol(symbol_id)?;

        // if the entry is an alias, resolve what it points to
        while let SymbolEntryKind::ProxiedSymbol(_, symbol_id) = &current_entry.kind {
            current_entry = self.lookup_global_symbol(*symbol_id)?;
        }

        Some(current_entry)
    }
}

impl SymbolQuery for Resolver {
    /// Resolve a symbol id to a variable within the given scope.
    fn lookup_var(&self, symbol_id: SymbolID) -> Option<ResolvedVar> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Var(resolved_var) => Some(resolved_var),
            _ => None,
        }
    }

    /// Resolve a symbol id to a method definition.
    fn lookup_method(&self, symbol_id: SymbolID) -> Option<ResolvedMethod> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Method(resolved_method) => Some(resolved_method),
            _ => None,
        }
    }

    /// Resolve a symbol id to a global function.
    fn lookup_func(&self, symbol_di: SymbolID) -> Option<ResolvedFunc> {
        match self.lookup_global_symbol(symbol_di)?.kind {
            SymbolEntryKind::Func(resolved_func) => Some(resolved_func),
            _ => None,
        }
    }

    /// Resolve a symbol id to a type definition (typedef).
    fn lookup_typedef(&self, symbol_id: SymbolID) -> Option<ResolvedTypedef> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Typedef(resolved_typedef) => Some(resolved_typedef),
            _ => None,
        }
    }

    /// Resolve a symbol id to a global variable.
    fn lookup_global_var(&self, symbol_id: SymbolID) -> Option<ResolvedGlobalVar> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::GlobalVar(resolved_global_var) => Some(resolved_global_var),
            _ => None,
        }
    }

    /// Resolve a symbol id to an interface/trait definition.
    fn lookup_interface(&self, symbol_id: SymbolID) -> Option<ResolvedInterface> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Interface(resolved_interface) => Some(resolved_interface),
            _ => None,
        }
    }

    /// Resolve a symbol id to a union definition.
    fn lookup_union(&self, symbol_id: SymbolID) -> Option<ResolvedUnion> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Union(resolved_union) => Some(resolved_union),
            _ => None,
        }
    }

    /// Resolve a symbol id to an enum definition.
    fn lookup_enum(&self, symbol_id: SymbolID) -> Option<ResolvedEnum> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Enum(resolved_enum) => Some(resolved_enum),
            _ => None,
        }
    }

    /// Resolve a symbol id to a struct definition.
    fn lookup_struct(&self, symbol_id: SymbolID) -> Option<ResolvedStruct> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Struct(resolved_struct) => Some(resolved_struct),
            _ => None,
        }
    }
}

impl Resolver {
    /// Resolves a symbol by searching the active scope stack.
    ///
    /// Lookup proceeds from the innermost scope outward until a matching
    /// symbol binding is found.
    pub(crate) fn resolve_scope_symbol(&self, name: &str) -> Option<SymbolID> {
        for scope in self.scopes_into_iter() {
            if let Some(symbol_id) = scope.resolve(name) {
                return Some(symbol_id);
            }
        }

        None
    }
}
