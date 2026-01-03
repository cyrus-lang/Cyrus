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
    SymbolID,
    generics::mapping_ctx::{GenericMappingCtx, mapping_ctx_eq_refcell},
    stmts::TypedBlockStmt,
};
use rand::Rng;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type MonomorphID = u32;

#[derive(Debug, Clone)]
pub struct GenericTemplateRegistry {
    map: HashMap<SymbolID, GenericTemplateEntry>,
}

#[derive(Debug, Clone)]
pub struct GenericTemplateEntry {
    pub body: TypedBlockStmt,
}

#[derive(Debug, Clone)]
pub enum SpecializedInstance {
    Func(SpecializedFuncEntry),
}

#[derive(Debug, Clone)]
pub struct SpecializedFuncEntry {
    pub body: TypedBlockStmt,
}

impl GenericTemplateRegistry {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn register_template(&mut self, base: SymbolID, body: TypedBlockStmt) {
        self.map.insert(base, GenericTemplateEntry { body });
    }

    pub fn get_template(&self, base: SymbolID) -> Option<&GenericTemplateEntry> {
        self.map.get(&base)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonomorphKey(pub u32);

impl MonomorphKey {
    pub fn new_instance_key() -> Self {
        let mut rng = rand::rng();
        let key = rng.random::<u32>();
        MonomorphKey(key)
    }
}

#[derive(Debug, Clone)]
pub struct MonomorphFuncEntry {
    pub id: MonomorphID,
    pub base_symbol: SymbolID,
    pub mapping_ctx: GenericMappingCtx,
    pub analyzed: bool,
}

#[derive(Debug, Clone)]
pub enum MonomorphEntry {
    Func(MonomorphFuncEntry),
}

impl MonomorphEntry {
    pub fn id(&self) -> MonomorphID {
        match self {
            MonomorphEntry::Func(e) => e.id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MonomorphRegistry {
    next_id: MonomorphID,
    map: HashMap<MonomorphKey, MonomorphEntry>,
    pub templates: GenericTemplateRegistry,
    pub specialized_func_instances: HashMap<MonomorphKey, SpecializedFuncEntry>,
}

impl MonomorphRegistry {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            map: HashMap::new(),
            templates: GenericTemplateRegistry::new(),
            specialized_func_instances: HashMap::new(),
        }
    }

    pub fn register_template(&mut self, base: SymbolID, body: TypedBlockStmt) {
        self.templates.register_template(base, body);
    }

    pub fn register_specialized_func_instance(&mut self, monomorph_key: MonomorphKey, instance: SpecializedFuncEntry) {
        self.specialized_func_instances.insert(monomorph_key, instance);
    }

    pub fn get_specialized_func_instance(&self, monomorph_key: MonomorphKey) -> Option<&SpecializedFuncEntry> {
        self.specialized_func_instances.get(&monomorph_key)
    }

    pub fn get_template(&self, base: SymbolID) -> Option<&GenericTemplateEntry> {
        self.templates.get_template(base)
    }

    pub fn get(&self, key: &MonomorphKey) -> Option<&MonomorphEntry> {
        self.map.get(key)
    }

    pub fn get_func_entry_by_mapping_ctx(
        &self,
        func_symbol_id: SymbolID,
        mapping_ctx: Rc<RefCell<GenericMappingCtx>>,
    ) -> Option<&MonomorphKey> {
        self.map
            .iter()
            .find(|(_, monomorph_entry)| match monomorph_entry {
                MonomorphEntry::Func(monomorph_func_entry) => {
                    mapping_ctx_eq_refcell(
                        &Rc::new(RefCell::new(monomorph_func_entry.mapping_ctx.clone())),
                        &mapping_ctx,
                    ) && monomorph_entry.id() == func_symbol_id
                }
            })
            .map(|(monomorph_key, _)| monomorph_key)
    }

    pub fn register_func(
        &mut self,
        base_symbol: SymbolID,
        mapping_ctx: GenericMappingCtx,
    ) -> (MonomorphKey, MonomorphID) {
        let key = MonomorphKey::new_instance_key();

        let id = self.next_id;
        self.next_id += 1;

        let entry = MonomorphEntry::Func(MonomorphFuncEntry {
            id,
            base_symbol,
            mapping_ctx,
            analyzed: false,
        });

        self.map.insert(key.clone(), entry);

        (key, id)
    }

    pub fn mark_analyzed(&mut self, key: &MonomorphKey) {
        if let Some(MonomorphEntry::Func(e)) = self.map.get_mut(key) {
            e.analyzed = true;
        }
    }

    pub fn needs_analysis(&self, key: &MonomorphKey) -> bool {
        match self.map.get(key) {
            Some(MonomorphEntry::Func(e)) => !e.analyzed,
            _ => false,
        }
    }
}

#[macro_export]
macro_rules! with_monomorph_registry {
    ($self:ident, $ctx:ident, $body:block) => {{
        let mut $ctx = $self.monomorph_registry.lock().unwrap();
        $body
    }};
}
