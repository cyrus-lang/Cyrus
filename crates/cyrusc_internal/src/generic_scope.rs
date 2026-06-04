// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_typed_ast::GenericParamID;
use fx_hash::FxHashMap;

#[derive(Debug, Clone)]
pub struct GenericScope {
    params: FxHashMap<String, GenericParamID>,
}

impl GenericScope {
    #[inline]
    pub fn new() -> Self {
        Self {
            params: FxHashMap::default(),
        }
    }

    #[inline]
    pub fn insert(&mut self, name: String, generic_param_id: GenericParamID) -> Option<GenericParamID> {
        self.params.insert(name, generic_param_id)
    }

    #[inline]
    pub fn lookup(&self, name: &str) -> Option<GenericParamID> {
        self.params.get(name).cloned()
    }

    #[inline]
    pub fn contains(&self, name: &str) -> bool {
        self.params.contains_key(name)
    }
}
