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

use crate::symbols::{symbols::LocalSymbol, *};
use cyrusc_tast::{LabelID, ScopeID, SymbolID, stmts::TypedGenericParamsList};
use std::collections::HashMap;

/// Represents a local scope containing variable symbols and control flow labels.
#[derive(Debug, Clone)]
pub struct LocalScope {
    pub symbols: HashMap<String, LocalSymbol>,
    pub parent: Option<ScopeID>,
    pub labels: HashMap<String, LabelID>,
}

impl LocalScope {
    pub fn new(parent: Option<ScopeID>) -> Self {
        Self {
            labels: HashMap::new(),
            symbols: HashMap::new(),
            parent,
        }
    }
}
