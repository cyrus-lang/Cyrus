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

pub trait ScopeQuery {
    fn lookup_local(&self, scope: ScopeID, name: &str) -> Option<LocalSymbol>;
    fn lookup_in_scope_chain(&self, scope: ScopeID, name: &str) -> Option<LocalOrGlobalSymbol>;
    fn local_scope_of_decl(&self, decl: DeclID) -> Option<LocalScope>;
}

pub trait GlobalSymbolQuery {
    fn lookup_symbol_id(&self, module: ModuleID, name: &str) -> Option<SymbolID>;
    fn resolve_symbol_entry(&self, module: ModuleID, name: &str) -> Option<SymbolEntry>;
    fn resolve_global_symbol(&self, symbol: SymbolID) -> Option<SymbolEntry>;
    fn resolve_global_symbol_deep(&self, symbol: SymbolID) -> Option<SymbolEntry>;
}

pub trait SymbolQuery {
    fn resolve_variable(&self, scope: ScopeID, id: SymbolID) -> Option<ResolvedVariable>;
    fn resolve_method(&self, scope: ScopeID, id: SymbolID) -> Option<ResolvedMethod>;
    fn resolve_function(&self, scope: ScopeID, id: SymbolID) -> Option<ResolvedFunction>;
    fn resolve_typedef(&self, scope: ScopeID, id: SymbolID) -> Option<ResolvedTypedef>;
    fn resolve_global_var(&self, scope: ScopeID, id: SymbolID) -> Option<ResolvedGlobalVar>;
    fn resolve_interface(&self, scope: ScopeID, id: SymbolID) -> Option<ResolvedInterface>;
    fn resolve_union(&self, scope: ScopeID, id: SymbolID) -> Option<ResolvedUnion>;
    fn resolve_enum(&self, scope: ScopeID, id: SymbolID) -> Option<ResolvedEnum>;
    fn resolve_struct(&self, scope: ScopeID, id: SymbolID) -> Option<ResolvedStruct>;
}

#[derive(Debug)]
pub struct SymbolTable {
    pub entries: HashMap<SymbolID, SymbolEntry>,
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
