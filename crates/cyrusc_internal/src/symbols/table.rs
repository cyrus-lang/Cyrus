// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use std::collections::HashMap;
use cyrusc_typed_ast::SymbolID;

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
