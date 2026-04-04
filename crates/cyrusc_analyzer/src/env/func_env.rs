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
    decls::MethodDeclID,
    types::{SemanticType, TypedFuncType},
};

#[derive(Debug, Clone)]
pub(crate) struct FuncEnv {
    pub(crate) current_func: Option<TypedFuncType>,
    pub(crate) current_object: Option<SemanticType>,
    pub(crate) current_method: Option<MethodDeclID>,
}

impl FuncEnv {
    pub fn new() -> Self {
        Self {
            current_func: None,
            current_object: None,
            current_method: None,
        }
    }
}
