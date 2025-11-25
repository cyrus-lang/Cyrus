use crate::{CIRBlockStmt, CIRFuncParams, IRValueID, cir_func_decl_as_func_ty, types::CIRFuncTy, walk::CIRWalk};
use cyrusc_tast::{
    SymbolID,
    generics::{
        monomorph::{MonomorphEntry, MonomorphKey},
        substitute::substitute_func_sig,
    },
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

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
    pub fn insert_monomorph_func_instance(&mut self, scope_id_opt: Option<SymbolID>, monomorph_key: &MonomorphKey) {
        let mut monomorph_registry = self.resolver.monomorph_registry.lock().unwrap();
        let monomorph_entry = monomorph_registry.get(monomorph_key).unwrap();
        let monomorph_func_entry = match monomorph_entry {
            MonomorphEntry::Func(monomorph_func_entry) => monomorph_func_entry,
        };

        let irv_id = monomorph_func_entry.id;

        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let resolved_func = self
            .resolver
            .resolve_func_symbol(local_scope_opt, monomorph_func_entry.base_symbol)
            .unwrap();

        let func_sig = substitute_func_sig(
            &resolved_func.func_sig,
            Rc::new(RefCell::new(monomorph_func_entry.mapping_ctx.clone())),
        )
        .unwrap();

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
