use crate::{
    Resolver,
    symbols::{
        LocalOrGlobalSymbol, LocalScopeRef, LocalSymbol, ResolvedEnum, ResolvedFunction, ResolvedGlobalVar,
        ResolvedInterface, ResolvedMethod, ResolvedStruct, ResolvedTypedef, ResolvedUnion, ResolvedVariable,
        SymbolEntry,
    },
};
use tast::{ModuleID, ScopeID, SymbolID};

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

    /// Resolves a global symbol from its ID, searching across all modules.
    pub fn resolve_global_symbol(&self, symbol_id: SymbolID) -> Option<SymbolEntry> {
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

    pub fn resolve_variable_symbol(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedVariable> {
        self.resolve_local_or_global_symbol(local_scope_opt, symbol_id)?
            .as_variable()
            .cloned()
    }

    pub fn resolve_method_symbol(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedMethod> {
        self.resolve_local_or_global_symbol(local_scope_opt, symbol_id)?
            .as_method()
            .cloned()
    }

    pub fn resolve_func_symbol(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedFunction> {
        self.resolve_local_or_global_symbol(local_scope_opt, symbol_id)?
            .as_func()
            .cloned()
    }

    pub fn resolve_typedef_symbol(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedTypedef> {
        self.resolve_local_or_global_symbol(local_scope_opt, symbol_id)?
            .as_typedef()
            .cloned()
    }

    pub fn resolve_global_var_symbol(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedGlobalVar> {
        self.resolve_local_or_global_symbol(local_scope_opt, symbol_id)?
            .as_global_var()
            .cloned()
    }

    pub fn resolve_interface_symbol(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedInterface> {
        self.resolve_local_or_global_symbol(local_scope_opt, symbol_id)?
            .as_interface()
            .cloned()
    }

    pub fn resolve_union_symbol(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedUnion> {
        self.resolve_local_or_global_symbol(local_scope_opt, symbol_id)?
            .as_union()
            .cloned()
    }

    pub fn resolve_enum_symbol(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedEnum> {
        self.resolve_local_or_global_symbol(local_scope_opt, symbol_id)?
            .as_enum()
            .cloned()
    }

    pub fn resolve_struct_symbol(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<ResolvedStruct> {
        self.resolve_local_or_global_symbol(local_scope_opt, symbol_id)?
            .as_struct()
            .cloned()
    }

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

    pub fn resolve_base_symbol_id(&self, symbol: &LocalOrGlobalSymbol) -> SymbolID {
        if let Some(td) = symbol.as_typedef() {
            if let Some(gt) = td.typedef_sig.ty.as_generic_type() {
                return gt.base;
            }
        }
        symbol.get_symbol_id()
    }
}
