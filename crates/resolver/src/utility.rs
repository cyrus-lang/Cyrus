use crate::{
    Resolver,
    scope::{LocalOrGlobalSymbol, LocalScopeRef, LocalSymbol, SymbolEntry, SymbolEntryKind},
};
use typed_ast::{ModuleID, ScopeID, SymbolID};

impl Resolver {
    /// Safely gets a reference to a local scope. Returns `None` if the
    /// module or scope ID is invalid, instead of panicking.
    pub fn get_scope_ref(&self, module_id: ModuleID, scope_id: ScopeID) -> Option<LocalScopeRef> {
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

    pub fn lookup_symbol(&self, module_id: ModuleID, name: &str) -> Option<SymbolEntry> {
        let global_symbols = self.global_symbols.lock().unwrap();
        global_symbols.get(&module_id).and_then(|table| {
            table
                .names
                .get(name)
                .and_then(|symbol_id| table.entries.get(symbol_id).cloned())
        })
    }

    pub fn lookup_symbol_entry_with_id(&self, module_id: ModuleID, symbol_id: SymbolID) -> Option<SymbolEntry> {
        let global_symbols = self.global_symbols.lock().unwrap();
        global_symbols
            .get(&module_id)
            .and_then(|table| table.entries.get(&symbol_id).cloned())
    }

    pub fn lookup_symbol_id_in_modules(&self, symbol_id: SymbolID) -> Option<ModuleID> {
        let global_symbols = self.global_symbols.lock().unwrap();
        global_symbols
            .iter()
            .find(|(_, symbol_table)| symbol_table.entries.contains_key(&symbol_id))
            .map(|(module_id, _)| *module_id)
    }

    pub fn find_function(&self, module_id: ModuleID, name: &str) -> Option<SymbolEntry> {
        self.lookup_symbol(module_id, name)
            .filter(|entry| matches!(entry.kind, SymbolEntryKind::Func { .. }))
    }

    /// Finds a symbol by name and returns it *only if* it's a struct.
    pub fn find_struct(&self, module_id: ModuleID, name: &str) -> Option<SymbolEntry> {
        self.lookup_symbol(module_id, name)
            .filter(|entry| matches!(entry.kind, SymbolEntryKind::Struct { .. }))
    }

    /// Resolves a global symbol from its ID, searching across all modules.
    pub fn resolve_global_symbol(&self, symbol_id: SymbolID) -> Option<SymbolEntry> {
        // The logic is chained cleanly: find the module, then get the symbol from it.
        let module_id = self.lookup_symbol_id_in_modules(symbol_id)?;
        self.lookup_symbol_entry_with_id(module_id, symbol_id)
    }

    /// Resolves a symbol from a specific local scope. Simplified to be a single, expressive statement.
    pub fn resolve_symbol_from_local_scope(
        &self,
        local_scope_rc: LocalScopeRef,
        symbol_id: SymbolID,
    ) -> Option<LocalSymbol> {
        local_scope_rc
            .borrow()
            .symbols
            .values()
            .find(|symbol| symbol.get_symbol_id() == symbol_id)
            .cloned()
    }

    /// Resolves a symbol that could be either local or global.
    /// This version is cleaner and has no duplicated logic.
    pub fn resolve_local_or_global_symbol(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<LocalOrGlobalSymbol> {
        if let Some(scope) = local_scope_opt {
            if let Some(local_symbol) = self.resolve_symbol_from_local_scope(scope, symbol_id) {
                return Some(LocalOrGlobalSymbol::LocalSymbol(local_symbol));
            }
        }

        self.resolve_global_symbol(symbol_id)
            .map(LocalOrGlobalSymbol::GlobalSymbol)
    }
}
