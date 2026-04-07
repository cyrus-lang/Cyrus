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

use cyrusc_typed_ast::GenericParamID;
use fx_hash::FxHashMap;

#[derive(Debug, Clone)]
pub struct GenericScope {
    params: FxHashMap<String, GenericParamID>,
}

impl GenericScope {
    #[inline]
    pub fn new() -> Self {
        Self {
            params: FxHashMap::default(),
        }
    }

    #[inline]
    pub fn insert(&mut self, name: String, generic_param_id: GenericParamID) -> Option<GenericParamID> {
        self.params.insert(name, generic_param_id)
    }

    #[inline]
    pub fn lookup(&self, name: &str) -> Option<GenericParamID> {
        self.params.get(name).cloned()
    }

    #[inline]
    pub fn contains(&self, name: &str) -> bool {
        self.params.contains_key(name)
    }
}
