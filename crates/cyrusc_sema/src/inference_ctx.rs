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
    sigs::StructSig,
    types::{SemanticType, TypedUnnamedStructType},
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(crate) struct InferenceCtx {
    map: HashMap<String, SemanticType>,
}

impl Default for InferenceCtx {
    fn default() -> Self {
        Self {
            map: Default::default(),
        }
    }
}

impl InferenceCtx {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn insert(&mut self, key: String, value: SemanticType) {
        self.map.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&SemanticType> {
        self.map.get(key)
    }
}

pub(crate) fn unnamed_struct_type_as_inference_ctx(unnamed_struct_type: &TypedUnnamedStructType) -> InferenceCtx {
    let mut infer_ctx = InferenceCtx::new();
    for field in &unnamed_struct_type.fields {
        infer_ctx.insert(field.name.clone(), *field.ty.clone());
    }
    infer_ctx
}

pub(crate) fn struct_sig_as_inference_ctx(struct_sig: &StructSig) -> InferenceCtx {
    let mut infer_ctx = InferenceCtx::new();
    for field in &struct_sig.fields {
        infer_ctx.insert(field.name.clone(), field.ty.clone());
    }
    infer_ctx
}
