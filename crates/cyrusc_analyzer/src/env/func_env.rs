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

use crate::{context::AnalysisContext, infer::InferCtx};
use cyrusc_typed_ast::{
    decls::MethodDeclID,
    types::{SemaType, TypedFuncType},
};

#[derive(Debug, Clone)]
pub(crate) struct FuncEnv {
    pub(crate) current_func: Option<TypedFuncType>,
    pub(crate) current_object: Option<SemaType>,
    pub(crate) current_method: Option<MethodDeclID>,
    pub(crate) infer: Option<InferCtx>,
}

impl FuncEnv {
    pub(crate) fn new() -> Self {
        Self {
            current_func: None,
            current_object: None,
            current_method: None,
            infer: None,
        }
    }
}

impl<'a> AnalysisContext<'a> {
    pub fn with_object<T>(&mut self, object_type: Option<SemaType>, f: impl FnOnce(&mut Self) -> T) -> T {
        let old = self.func_env.current_object.take();
        self.func_env.current_object = object_type;
        let result = f(self);
        self.func_env.current_object = old;
        result
    }
}

impl<'a> AnalysisContext<'a> {
    #[inline]
    pub(crate) fn with_func_env<T>(&mut self, new_env: FuncEnv, f: impl FnOnce(&mut Self) -> T) -> T {
        let old_env = std::mem::replace(&mut self.func_env, new_env);
        let result = f(self);
        self.func_env = old_env;
        result
    }

    #[inline]
    pub(crate) fn create_func_def_env(&self, func_type: TypedFuncType) -> FuncEnv {
        FuncEnv {
            current_func: Some(func_type),
            current_method: None,
            current_object: None,
            infer: Some(InferCtx::new()),
        }
    }

    #[inline]
    pub(crate) fn create_method_env(&self, method_decl_id: MethodDeclID, func_type: TypedFuncType, parent_infer_ctx: Option<InferCtx>) -> FuncEnv {
        FuncEnv {
            current_func: Some(func_type),
            current_method: Some(method_decl_id),
            current_object: None,
            infer: parent_infer_ctx,
        }
    }
}
