/* 
 * Copyright (c) 2026 The Cyrus Programming Language Project
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
use crate::{CIRBlockStmt, CIRFuncParams, IRValueID, cir_func_decl_as_func_ty, types::CIRFuncTy, walk::CIRWalk};
use cyrusc_tast::{
    SymbolID,
    generics::monomorph::{MonomorphEntry, MonomorphFuncEntry, MonomorphKey},
    sigs::FuncSig,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct CIRMonomorphRegistry {
    map: HashMap<MonomorphKey, CIRMonomorphEntry>,
}

#[derive(Debug, Clone)]
pub struct CIRMonomorphFuncEntry {
    pub irv_id: IRValueID,
    pub func_ty: CIRFuncTy,
    pub func_params: CIRFuncParams,
    pub func_body: Box<CIRBlockStmt>,
}

#[derive(Debug, Clone)]
pub enum CIRMonomorphEntry {
    Func(CIRMonomorphFuncEntry),
}

impl CIRMonomorphRegistry {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn get(&self, key: &MonomorphKey) -> Option<&CIRMonomorphEntry> {
        self.map.get(key)
    }

    pub fn insert(&mut self, key: MonomorphKey, entry: CIRMonomorphEntry) {
        self.map.insert(key, entry);
    }
}

impl<'resolver> CIRWalk<'resolver> {
    pub fn get_monomorph_func_entry(&self, monomorph_key: &MonomorphKey) -> Option<MonomorphFuncEntry> {
        let monomorph_registry = self.resolver.monomorph_registry.lock().unwrap();
        let monomorph_entry = monomorph_registry.get(monomorph_key).unwrap();
        let monomorph_func_entry = match monomorph_entry.clone() {
            MonomorphEntry::Func(monomorph_func_entry) => monomorph_func_entry,
        };
        drop(monomorph_registry);
        Some(monomorph_func_entry)
    }

    pub fn insert_monomorph_func_instance(
        &mut self,
        scope_id_opt: Option<SymbolID>,
        monomorph_key: &MonomorphKey,
        func_sig: &FuncSig,
    ) {
        let monomorph_func_entry = self.get_monomorph_func_entry(monomorph_key).unwrap();
        let monomorph_registry = self.resolver.monomorph_registry.lock().unwrap();

        let irv_id = monomorph_func_entry.id;

        let cir_func_decl = self.lower_func_sig(scope_id_opt, monomorph_func_entry.id, &func_sig);
        let cir_func_params = cir_func_decl.params.clone();
        let cir_func_ty = cir_func_decl_as_func_ty(&cir_func_decl);

        let specialized_func_entry = monomorph_registry
            .get_specialized_func_instance(monomorph_key.clone())
            .unwrap();

        let cir_func_body = self.lower_body(&specialized_func_entry.body);

        drop(monomorph_registry);

        // insert discovered function instance to registry

        let mut cir_monomorph_registry = self.cir_monomorph_registry.lock().unwrap();

        cir_monomorph_registry.insert(
            monomorph_key.clone(),
            CIRMonomorphEntry::Func(CIRMonomorphFuncEntry {
                irv_id,
                func_params: cir_func_params,
                func_ty: cir_func_ty,
                func_body: Box::new(cir_func_body),
            }),
        );

        drop(cir_monomorph_registry);
    }
}
