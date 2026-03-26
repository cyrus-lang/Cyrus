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
        cir::{CIRBlockStmt, CIRFuncParams, IRValueID, cir_func_decl_as_func_ty},
        traverse::CIRTraverse,
        types::CIRFuncTy,
    },
};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{generics::monomorph::MonomorphID, sigs::FuncSig};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct CIRMonomorphRegistry {
    map: HashMap<MonomorphID, CIRMonomorphEntry>,
}

#[derive(Debug, Clone)]
pub struct CIRMonomorphFuncEntry {
    pub irv_id: IRValueID,
    pub func_type: CIRFuncTy,
    pub func_params: CIRFuncParams,
    pub func_body: CIRMonomorphFuncBody,
    pub abi_func_info: ABIFunctionInfo,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum CIRMonomorphFuncBody {
    Placeholder,
    Body(Box<CIRBlockStmt>),
}

impl CIRMonomorphFuncEntry {
    pub fn body(&self) -> Option<&CIRBlockStmt> {
        match &self.func_body {
            CIRMonomorphFuncBody::Placeholder => None,
            CIRMonomorphFuncBody::Body(body) => Some(body),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CIRMonomorphEntry {
    Func(CIRMonomorphFuncEntry),
}

impl CIRMonomorphRegistry {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn contains_key(&self, key: &MonomorphID) -> bool {
        self.map.contains_key(key)
    }

    pub fn get(&self, key: &MonomorphID) -> Option<&CIRMonomorphEntry> {
        self.map.get(key)
    }

    pub fn get_mut(&mut self, key: &MonomorphID) -> Option<&mut CIRMonomorphEntry> {
        self.map.get_mut(key)
    }

    pub fn insert(&mut self, key: MonomorphID, entry: CIRMonomorphEntry) {
        self.map.insert(key, entry);
    }
}

impl<'resolver> CIRTraverse<'resolver> {
    pub fn insert_monomorph_func_instance(&mut self, monomorph_id: MonomorphID, func_sig: &FuncSig) {
        {
            let cir_monomorph_registry = self.cir_monomorph_registry.lock().unwrap();
            if cir_monomorph_registry.contains_key(&monomorph_id) {
                // if it's a Placeholder, we are already lowering it (recursion detected).
                // if it's a body, we've already finished it.
                // either way, STOP here to break the loop.
                return;
            }
        }

        let monomorph_func_entry = self.query.lookup_monomorph_func(monomorph_id).unwrap();
        let specialized_func_entry = self.query.lookup_specialized_func_instance(monomorph_id).unwrap();

        let irv_id: IRValueID = monomorph_func_entry.id.0.try_into().unwrap();

        let cir_func_decl = self.lower_func_sig(irv_id, func_sig);

        let cir_func_params = cir_func_decl.params.clone();
        let cir_func_type = cir_func_decl_as_func_ty(&cir_func_decl);

        let abi_func_info = self.target.target_abi.classify_func(&cir_func_type).unwrap();

        {
            // body placeholder to prevent infinite recursion during lowering

            let mut cir_monomorph_registry = self.cir_monomorph_registry.lock().unwrap();
            cir_monomorph_registry.insert(
                monomorph_id.clone(),
                CIRMonomorphEntry::Func(CIRMonomorphFuncEntry {
                    irv_id,
                    func_params: cir_func_params,
                    func_type: cir_func_type,
                    func_body: CIRMonomorphFuncBody::Placeholder,
                    abi_func_info,
                    loc: cir_func_decl.loc,
                }),
            );
        }

        // lower body
        let cir_func_body = self.lower_body(&specialized_func_entry.body);

        {
            let mut cir_monomorph_registry = self.cir_monomorph_registry.lock().unwrap();
            let monomorph_entry = cir_monomorph_registry.get_mut(&monomorph_id).unwrap();

            match monomorph_entry {
                CIRMonomorphEntry::Func(monomorph_func_entry) => {
                    monomorph_func_entry.func_body = CIRMonomorphFuncBody::Body(Box::new(cir_func_body));
                }
            }
        }
    }
}
