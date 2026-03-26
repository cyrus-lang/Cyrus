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
    generics::{
        mapping_ctx::{GenericMappingCtx, mapping_ctx_eq_refcell},
        mapping_ctx_arena::GenericMappingCtxArena,
    },
    stmts::{TypedBlockStmt, TypedGenericParamsList},
};
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    sync::{Arc, Mutex},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MonomorphID(pub usize);

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

    pub fn resolve_template(&self, base: SymbolID) -> Option<&GenericTemplateEntry> {
        self.map.get(&base)
    }
}

#[derive(Debug, Clone)]
pub struct MonomorphFuncEntry {
    pub id: MonomorphID,
    pub base_symbol: SymbolID,
    pub mapping_ctx: GenericMappingCtx,
    pub generic_params: TypedGenericParamsList,
    pub analyzed: bool,
}

#[derive(Debug, Clone)]
pub enum MonomorphEntry {
    Func(MonomorphFuncEntry),
}

impl MonomorphEntry {
    pub fn id(&self) -> MonomorphID {
        match self {
            MonomorphEntry::Func(entry) => entry.id,
        }
    }

    pub fn base_symbol(&self) -> SymbolID {
        match self {
            MonomorphEntry::Func(entry) => entry.base_symbol,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MonomorphRegistry {
    entries: Vec<MonomorphEntry>,
    pub templates: GenericTemplateRegistry,
    pub specialized_func_instances: HashMap<MonomorphID, SpecializedFuncEntry>,
}

impl MonomorphRegistry {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            templates: GenericTemplateRegistry::new(),
            specialized_func_instances: HashMap::new(),
        }
    }

    #[inline]
    pub fn register_template(&mut self, base: SymbolID, body: TypedBlockStmt) {
        self.templates.register_template(base, body);
    }

    #[inline]
    pub fn register_specialized_func_instance(&mut self, monomorph_id: MonomorphID, instance: SpecializedFuncEntry) {
        self.specialized_func_instances.insert(monomorph_id, instance);
    }

    #[inline]
    pub fn resolve_specialized_func_instance(&self, monomorph_id: MonomorphID) -> Option<&SpecializedFuncEntry> {
        self.specialized_func_instances.get(&monomorph_id)
    }

    #[inline]
    pub fn resolve_template(&self, base: SymbolID) -> Option<&GenericTemplateEntry> {
        self.templates.resolve_template(base)
    }

    #[inline]
    pub fn resolve_by_monomorph_id(&self, id: MonomorphID) -> Option<&MonomorphEntry> {
        self.entries.get(id.0)
    }

    pub fn resolve_func_entry_by_mapping_ctx(
        &self,
        mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
        func_symbol_id: SymbolID,
        generic_params: TypedGenericParamsList,
        mapping_ctx: Rc<RefCell<GenericMappingCtx>>,
    ) -> Option<MonomorphID> {
        self.entries
            .iter()
            .find(|monomorph_entry| match monomorph_entry {
                MonomorphEntry::Func(monomorph_func_entry) => {
                    mapping_ctx_eq_refcell(
                        mapping_ctx_arena.clone(),
                        &generic_params,
                        &Rc::new(RefCell::new(monomorph_func_entry.mapping_ctx.clone())),
                        &monomorph_func_entry.generic_params,
                        &mapping_ctx,
                    ) && monomorph_entry.base_symbol() == func_symbol_id
                }
            })
            .map(|index| index.id())
    }

    pub fn register_func(
        &mut self,
        base_symbol: SymbolID,
        generic_params: TypedGenericParamsList,
        mapping_ctx: GenericMappingCtx,
    ) -> MonomorphID {
        let monomorph_id = MonomorphID(self.entries.len() + 1);

        let entry = MonomorphEntry::Func(MonomorphFuncEntry {
            id: monomorph_id,
            base_symbol,
            generic_params,
            mapping_ctx,
            analyzed: false,
        });

        self.entries.push(entry);
        monomorph_id
    }
}

#[macro_export]
macro_rules! monomorph_registry {
    ($self:ident, $ctx:ident, $body:block) => {{
        let mut $ctx = $self.monomorph_registry.lock().unwrap();
        $body
    }};
}
