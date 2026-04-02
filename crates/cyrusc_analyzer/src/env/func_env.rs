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

use cyrusc_typed_ast::{
    SymbolID,
    types::{SemanticType, TypedFuncType},
};

#[derive(Debug, Clone)]
pub(crate) struct FuncEnv {
    pub(crate) current_func_type: Option<TypedFuncType>,
    pub(crate) current_self_type: Option<SemanticType>,
    pub(crate) current_object_type: Option<SemanticType>,
    pub(crate) current_method_symbol_id: Option<SymbolID>,
}

impl FuncEnv {
    pub fn new() -> Self {
        Self {
            current_func_type: None,
            current_self_type: None,
            current_object_type: None,
            current_method_symbol_id: None,
        }
    }
}
