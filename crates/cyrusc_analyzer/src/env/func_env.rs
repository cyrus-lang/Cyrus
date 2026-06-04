// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, infer::InferCtx};
use cyrusc_typed_ast::{
    decls::MethodDeclID,
    types::{SemaType, TypedFuncType},
};

#[derive(Debug, Clone)]
pub(crate) struct FuncEnv {
    pub(crate) current_func_name: Option<String>,
    pub(crate) current_func: Option<TypedFuncType>,
    pub(crate) current_object: Option<SemaType>,
    pub(crate) current_method: Option<MethodDeclID>,
    pub(crate) infer: Option<InferCtx>,
}

impl FuncEnv {
    pub(crate) fn new() -> Self {
        Self {
            current_func_name: None,
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
    pub(crate) fn create_func_def_env(
        &self,
        name: &str,
        func_type: TypedFuncType,
        use_parent_infer_ctx: Option<InferCtx>,
    ) -> FuncEnv {
        FuncEnv {
            current_func_name: Some(name.to_string()),
            current_func: Some(func_type),
            current_method: None,
            current_object: None,
            infer: use_parent_infer_ctx.or(Some(InferCtx::new())),
        }
    }

    #[inline]
    pub(crate) fn create_method_env(
        &self,
        method_decl_id: MethodDeclID,
        func_type: TypedFuncType,
        use_parent_infer_ctx: Option<InferCtx>,
    ) -> FuncEnv {
        FuncEnv {
            current_func_name: None,
            current_func: Some(func_type),
            current_method: Some(method_decl_id),
            current_object: None,
            infer: use_parent_infer_ctx.or(Some(InferCtx::new())),
        }
    }
}
