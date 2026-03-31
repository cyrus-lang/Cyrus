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
    stmts::TypedBlockStmt,
    types::SemanticType,
};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonomorphKey {
    pub template: FuncDeclID,
    pub type_args: Vec<SemanticType>,
}

#[derive(Debug, Clone)]
pub struct MonomorphInstance {
    pub id: MonomorphID,
    pub template: FuncDeclID,
    pub type_args: Vec<SemanticType>,

    pub body: Option<TypedBlockStmt>,
    pub analyzed: bool,
}

#[derive(Debug, Default)]
pub struct MonomorphRegistry {
    key_map: HashMap<MonomorphKey, MonomorphID>,
    instances: Vec<MonomorphInstance>,
}

impl MonomorphRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_or_create(&mut self, template: FuncDeclID, type_args: Vec<SemanticType>) -> MonomorphID {
        let key = MonomorphKey { template, type_args };

        if let Some(id) = self.key_map.get(&key) {
            return *id;
        }

        let id = MonomorphID(self.instances.len());
        let instance = MonomorphInstance {
            id,
            template: key.template,
            type_args: key.type_args.clone(),
            body: None,
            analyzed: false,
        };

        self.key_map.insert(key, id);
        self.instances.push(instance);
        id
    }

    pub fn get(&self, id: MonomorphID) -> &MonomorphInstance {
        &self.instances[id.0]
    }

    pub fn get_mut(&mut self, id: MonomorphID) -> &mut MonomorphInstance {
        &mut self.instances[id.0]
    }
}

#[macro_export]
macro_rules! monomorph_registry {
    ($self:ident, $ctx:ident, $body:block) => {{
        let mut $ctx = $self.monomorph_registry.lock().unwrap();
        $body
    }};
}
