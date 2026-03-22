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

use cyrusc_typed_ast::{LabelID, SymbolID};
use std::collections::HashMap;

/// Represents a lexical local scope.
///
/// A `LocalScope` stores symbol bindings and control‑flow labels declared
/// within the scope. Parent relationships are tracked via `ScopeID`, but
/// parent traversal is performed by the resolver rather than this type.
///
/// This structure intentionally performs **no recursive lookup**. It only
/// queries symbols defined directly within the current scope.
#[derive(Debug, Clone)]
pub struct LocalScope {
    /// Symbol bindings declared in this scope.
    pub symbols: HashMap<String, SymbolID>,

    /// Control‑flow labels declared in this scope.
    pub labels: HashMap<String, LabelID>,
}

impl LocalScope {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            labels: HashMap::new(),
        }
    }

    /// Inserts a symbol binding into the current scope.
    ///
    /// If a symbol with the same name already exists in this scope,
    /// it will be replaced.
    pub fn insert(&mut self, name: String, symbol_id: SymbolID) {
        self.symbols.insert(name, symbol_id);
    }

    /// Looks up a symbol defined **directly in this scope**.
    ///
    /// This function does not search parent scopes. Recursive lookup
    /// must be handled by the resolver.
    pub fn resolve(&self, name: &str) -> Option<SymbolID> {
        self.symbols.get(name).copied()
    }

    /// Returns `true` if a symbol with the given name exists in this scope only.
    pub fn contains(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    /// Returns `true` if a label with the given name exists in this scope only.
    pub fn contains_label(&self, name: &str) -> bool {
        self.labels.contains_key(name)
    }

    /// Inserts a control‑flow label into the current scope.
    pub fn insert_label(&mut self, name: String, label_id: LabelID) {
        self.labels.insert(name, label_id);
    }

    /// Looks up a label defined **directly in this scope**.
    ///
    /// Parent scope traversal is handled by the resolver.
    pub fn resolve_label(&self, name: &str) -> Option<LabelID> {
        self.labels.get(name).copied()
    }
}
