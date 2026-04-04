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

use crate::{
    abi::args::ABIFunctionInfo,
    cir::{
        cir::{CIRBlockStmt, CIRFuncParams, IRValueID},
        types::CIRFuncType,
    },
};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::decls::MonomorphID;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct CIRInstanceRegistry {
    funcs: HashMap<MonomorphID, CIRInstanceFunc>,
}

#[derive(Debug, Clone)]
pub struct CIRInstanceFunc {
    pub irv_id: IRValueID,
    pub func_type: CIRFuncType,
    pub func_params: CIRFuncParams,
    pub func_body: CIRInstanceFuncBody,
    pub abi_func_info: ABIFunctionInfo,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum CIRInstanceFuncBody {
    Placeholder,
    Body(Box<CIRBlockStmt>),
}

impl CIRInstanceFunc {
    pub fn body(&self) -> Option<&CIRBlockStmt> {
        match &self.func_body {
            CIRInstanceFuncBody::Placeholder => None,
            CIRInstanceFuncBody::Body(body) => Some(body),
        }
    }
}

impl CIRInstanceRegistry {
    pub fn new() -> Self {
        Self { funcs: HashMap::new() }
    }

    pub fn contains_key(&self, key: &MonomorphID) -> bool {
        self.funcs.contains_key(key)
    }

    pub fn get_func(&self, key: &MonomorphID) -> Option<&CIRInstanceFunc> {
        self.funcs.get(key)
    }

    pub fn get_func_mut(&mut self, key: &MonomorphID) -> Option<&mut CIRInstanceFunc> {
        self.funcs.get_mut(key)
    }

    pub fn insert_func(&mut self, key: MonomorphID, entry: CIRInstanceFunc) {
        self.funcs.insert(key, entry);
    }
}
