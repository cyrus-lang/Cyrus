// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_typed_ast::{
    BodyID,
    decls::{FuncDeclID, MethodDeclID, MonomorphID},
    stmts::{TypedBlockStmt, TypedFuncParams, TypedTypeArgs},
    types::SemaType,
};
use fx_hash::{FxHashMap, FxHashMapExt};
use std::sync::RwLock;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonomorphKey {
    pub template_id: MonomorphizableTemplateID,
    pub type_args: TypedTypeArgs,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MonomorphizableTemplateID {
    Func(FuncDeclID),
    Method(MethodDeclID),
}

#[derive(Debug, Clone)]
pub struct MonomorphInstance {
    pub monomorph_id: MonomorphID,
    pub template_id: MonomorphizableTemplateID,
    pub type_args: TypedTypeArgs,

    pub params: TypedFuncParams,
    pub ret_type: SemaType,
    pub body: Option<BodyID>,

    pub analyzed: bool,
}

#[derive(Debug, Default)]
struct MonomorphRegistryInner {
    key_map: FxHashMap<MonomorphKey, MonomorphID>,
    func_to_monomorphs: FxHashMap<MonomorphizableTemplateID, Vec<MonomorphID>>,
    instances: Vec<MonomorphInstance>,
    monomorph_body: Vec<TypedBlockStmt>,
}

#[derive(Debug, Default)]
pub struct MonomorphRegistry {
    inner: RwLock<MonomorphRegistryInner>,
}

impl MonomorphRegistry {
    pub fn new() -> Self {
        Self {
            inner: RwLock::new(MonomorphRegistryInner {
                key_map: FxHashMap::new(),
                func_to_monomorphs: FxHashMap::new(),
                instances: Vec::new(),
                monomorph_body: Vec::new(),
            }),
        }
    }

    pub fn get_or_create(
        &self,
        template_id: MonomorphizableTemplateID,
        type_args: TypedTypeArgs,
        params: TypedFuncParams,
        ret_type: SemaType,
    ) -> MonomorphID {
        {
            let inner = self.inner.read().unwrap();
            let key = MonomorphKey {
                template_id,
                type_args: type_args.clone(),
            };

            if let Some(id) = inner.key_map.get(&key) {
                return *id;
            }
        }

        let mut inner = self.inner.write().unwrap();

        let key = MonomorphKey {
            template_id,
            type_args: type_args.clone(),
        };

        if let Some(monomorph_id) = inner.key_map.get(&key) {
            return *monomorph_id;
        }

        let monomorph_id = MonomorphID(inner.instances.len());

        let instance = MonomorphInstance {
            monomorph_id,
            template_id,
            type_args: type_args.clone(),

            params,
            ret_type,
            body: None,

            analyzed: false,
        };

        inner
            .func_to_monomorphs
            .entry(template_id)
            .or_default()
            .push(monomorph_id);

        inner.key_map.insert(key, monomorph_id);
        inner.instances.push(instance);

        monomorph_id
    }

    pub fn get(&self, monomorph_id: MonomorphID) -> MonomorphInstance {
        let inner = self.inner.read().unwrap();
        inner.instances[monomorph_id.0].clone()
    }

    pub fn update<F>(&self, monomorph_id: MonomorphID, f: F)
    where
        F: FnOnce(&mut MonomorphInstance),
    {
        let mut inner = self.inner.write().unwrap();
        f(&mut inner.instances[monomorph_id.0]);
    }

    pub fn insert_monomorph_body(&self, body: TypedBlockStmt) -> BodyID {
        let mut inner = self.inner.write().unwrap();
        let body_id = BodyID(inner.monomorph_body.len() as u32);
        inner.monomorph_body.push(body);
        body_id
    }

    pub fn get_monomorph_body(&self, body_id: BodyID) -> Option<TypedBlockStmt> {
        let inner = self.inner.read().unwrap();
        inner.monomorph_body.get(body_id.0 as usize).cloned()
    }

    pub fn get_func_monomorphs(&self, template_id: MonomorphizableTemplateID) -> Vec<MonomorphID> {
        let inner = self.inner.read().unwrap();
        inner.func_to_monomorphs.get(&template_id).cloned().unwrap_or_default()
    }
}

impl MonomorphizableTemplateID {
    #[inline]
    pub fn as_func(&self) -> Option<FuncDeclID> {
        match self {
            MonomorphizableTemplateID::Func(func_decl_id) => Some(*func_decl_id),
            _ => None,
        }
    }

    #[inline]
    pub fn as_method(&self) -> Option<MethodDeclID> {
        match self {
            MonomorphizableTemplateID::Method(method_decl_id) => Some(*method_decl_id),
            _ => None,
        }
    }
}
