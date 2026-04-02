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

use cyrusc_typed_ast::types::SemanticType;
use fx_hash::FxHashMap;

#[derive(Debug, Clone)]
pub(crate) struct FieldEnv {
    fields: FxHashMap<String, SemanticType>,
}

impl FieldEnv {
    pub fn new() -> Self {
        Self { fields: FxHashMap::default() }
    }

    pub fn insert(&mut self, key: String, value: SemanticType) {
        self.fields.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&SemanticType> {
        self.fields.get(key)
    }
}
