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
    decls::{FuncDeclID, MonomorphID},
    stmts::{TypedBlockStmt, TypedTypeArgs},
};
use fx_hash::FxHashMap;
use std::sync::RwLock;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonomorphKey {
    pub template: FuncDeclID,
    pub type_args: TypedTypeArgs,
}

#[derive(Debug, Clone)]
pub struct MonomorphInstance {
    pub monomorph_id: MonomorphID,
    pub func_decl_id: FuncDeclID,
    pub type_args: TypedTypeArgs,
    pub body: Option<TypedBlockStmt>,
    pub analyzed: bool,
}

#[derive(Debug, Default)]
struct MonomorphRegistryInner {
    key_map: FxHashMap<MonomorphKey, MonomorphID>,
    instances: Vec<MonomorphInstance>,
}

#[derive(Debug, Default)]
pub struct MonomorphRegistry {
    inner: RwLock<MonomorphRegistryInner>,
}

impl MonomorphRegistry {
    pub fn new() -> Self {
        Self {
            inner: RwLock::new(MonomorphRegistryInner {
                key_map: FxHashMap::default(),
                instances: Vec::new(),
            }),
        }
    }

    pub fn get_or_create(&self, template: FuncDeclID, type_args: TypedTypeArgs) -> MonomorphID {
        {
            let inner = self.inner.read().unwrap();
            let key = MonomorphKey {
                template,
                type_args: type_args.clone(),
            };

            if let Some(id) = inner.key_map.get(&key) {
                return *id;
            }
        }

        let mut inner = self.inner.write().unwrap();

        let key = MonomorphKey {
            template,
            type_args: type_args.clone(),
        };

        if let Some(id) = inner.key_map.get(&key) {
            return *id;
        }

        let id = MonomorphID(inner.instances.len());

        let instance = MonomorphInstance {
            monomorph_id: id,
            func_decl_id: template,
            type_args: type_args.clone(),
            body: None,
            analyzed: false,
        };

        inner.key_map.insert(key, id);
        inner.instances.push(instance);

        id
    }

    pub fn get(&self, id: MonomorphID) -> MonomorphInstance {
        let inner = self.inner.read().unwrap();
        inner.instances[id.0].clone()
    }

    pub fn update<F>(&self, id: MonomorphID, f: F)
    where
        F: FnOnce(&mut MonomorphInstance),
    {
        let mut inner = self.inner.write().unwrap();
        f(&mut inner.instances[id.0]);
    }
}
