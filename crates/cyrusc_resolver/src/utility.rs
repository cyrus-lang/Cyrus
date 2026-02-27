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
use crate::{
    Resolver,
    symbols::{
        LocalOrGlobalSymbol, LocalScopeRef, LocalSymbol, ResolvedEnum, ResolvedFunction, ResolvedGlobalVar,
        ResolvedInterface, ResolvedMethod, ResolvedStruct, ResolvedTypedef, ResolvedUnion, ResolvedVariable,
        SymbolEntry, SymbolEntryKind,
    },
};
use cyrusc_tast::{ModuleID, ScopeID, SymbolID};

impl Resolver {
    /// Safely gets a reference to a local scope. Returns `None` if the
    /// module or scope ID is invalid, instead of panicking.
    pub fn resolve_local_scope(&self, module_id: ModuleID, scope_id: ScopeID) -> Option<LocalScopeRef> {
        let global_symbols = self.global_symbols.lock().unwrap();
        global_symbols.get(&module_id)?.scopes.get(&scope_id).cloned()
    }

    /// Looks up a symbol's ID by name within a specific module.
    pub fn lookup_symbol_id(&self, module_id: ModuleID, name: &str) -> Option<SymbolID> {
        let global_symbols = self.global_symbols.lock().unwrap();
        global_symbols
            .get(&module_id)
            .and_then(|table| table.names.get(name).copied())
    }

    pub fn lookup_symbol_entry(&self, module_id: ModuleID, name: &str) -> Option<SymbolEntry> {
        let global_symbols = self.global_symbols.lock().unwrap();
        let symbol_id = global_symbols
            .get(&module_id)
            .and_then(|table| table.names.get(name).copied())?;
        drop(global_symbols);
        self.resolve_global_symbol_deep(symbol_id)
    }

    pub fn lookup_symbol_entry_with_id(&self, symbol_id: SymbolID) -> Option<SymbolEntry> {
        self.resolve_global_symbol_deep(symbol_id)
    }

    pub fn lookup_symbol_id_in_modules(&self, symbol_id: SymbolID) -> Option<ModuleID> {
        let global_symbols = self.global_symbols.lock().unwrap();
        global_symbols
            .iter()
            .find(|(_, symbol_table)| symbol_table.entries.contains_key(&symbol_id))
            .map(|(module_id, _)| *module_id)
    }

    /// Shallow resolution: returns the symbol entry as stored in its module.
    pub fn resolve_global_symbol(&self, symbol_id: SymbolID) -> Option<SymbolEntry> {
        let global_symbols = self.global_symbols.lock().unwrap();

        let (_, table) = global_symbols
            .iter()
            .find(|(_, tbl)| tbl.entries.contains_key(&symbol_id))?;

        table.entries.get(&symbol_id).cloned()
    }

    /// Deep resolution: recursively resolves proxied symbols until a real definition.
    fn resolve_global_symbol_deep(&self, mut symbol_id: SymbolID) -> Option<SymbolEntry> {
        loop {
            let entry = self.resolve_global_symbol(symbol_id.clone())?;

            match entry.kind {
                SymbolEntryKind::ProxiedSymbol(_, target_symbol_id) => {
                    symbol_id = target_symbol_id;
                    continue;
                }
                _ => return Some(entry),
            }
        }
    }

    /// Resolves a symbol id from a specific local scope.
    pub fn resolve_symbol_id_from_local_scope(&self, scope_rc: LocalScopeRef, name: &str) -> Option<SymbolID> {
        scope_rc.borrow().with_symbol(name, |x| x.symbol_id())
    }

    /// Resolves a symbol from a specific local scope. Simplified to be a single, expressive statement.
    pub fn resolve_symbol_from_local_scope(&self, scope_rc: LocalScopeRef, symbol_id: SymbolID) -> Option<LocalSymbol> {
        scope_rc.borrow().with_symbol_id(symbol_id, |sym| sym.clone())
    }

    pub fn resolve_variable_symbol(
        &self,
        scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedVariable> {
        self.resolve_local_or_global_symbol(scope_opt, symbol_id)?
            .as_variable()
            .cloned()
    }

    pub fn resolve_method_symbol(
        &self,
        scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedMethod> {
        self.resolve_local_or_global_symbol(scope_opt, symbol_id)?
            .as_method()
            .cloned()
    }

    pub fn resolve_func_symbol(
        &self,
        scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedFunction> {
        self.resolve_local_or_global_symbol(scope_opt, symbol_id)?
            .as_func()
            .cloned()
    }

    pub fn resolve_typedef_symbol(
        &self,
        scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedTypedef> {
        self.resolve_local_or_global_symbol(scope_opt, symbol_id)?
            .as_typedef()
            .cloned()
    }

    pub fn resolve_global_var_symbol(
        &self,
        scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedGlobalVar> {
        self.resolve_local_or_global_symbol(scope_opt, symbol_id)?
            .as_global_var()
            .cloned()
    }

    pub fn resolve_interface_symbol(
        &self,
        scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedInterface> {
        self.resolve_local_or_global_symbol(scope_opt, symbol_id)?
            .as_interface()
            .cloned()
    }

    pub fn resolve_union_symbol(&self, scope_opt: Option<LocalScopeRef>, symbol_id: SymbolID) -> Option<ResolvedUnion> {
        self.resolve_local_or_global_symbol(scope_opt, symbol_id)?
            .as_union()
            .cloned()
    }

    pub fn resolve_enum_symbol(&self, scope_opt: Option<LocalScopeRef>, symbol_id: SymbolID) -> Option<ResolvedEnum> {
        self.resolve_local_or_global_symbol(scope_opt, symbol_id)?
            .as_enum()
            .cloned()
    }

    pub fn resolve_struct_symbol(
        &self,
        scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedStruct> {
        self.resolve_local_or_global_symbol(scope_opt, symbol_id)?
            .as_struct()
            .cloned()
    }

    pub fn resolve_local_or_global_symbol(
        &self,
        scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<LocalOrGlobalSymbol> {
        if let Some(scope) = scope_opt {
            if let Some(local_symbol) = self.resolve_symbol_from_local_scope(scope, symbol_id) {
                return Some(LocalOrGlobalSymbol::LocalSymbol(local_symbol));
            }
        }

        self.resolve_global_symbol_deep(symbol_id)
            .map(LocalOrGlobalSymbol::GlobalSymbol)
    }
}
