// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::context::AnalysisContext;
use cyrusc_typed_ast::{
    exprs::TypedLambdaExpr,
    types::{SemaType, TypedFuncType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_lambda(&mut self, lambda: &mut TypedLambdaExpr) -> Option<SemaType> {
        self.normalize_func_params(&mut lambda.params, true, 0);

        let params = lambda.params.as_func_type_params();

        lambda.ret_type = self.normalize_and_check_type_formation(lambda.ret_type.clone(), lambda.loc, 0)?;

        let mut func_type = TypedFuncType {
            params,
            ret_type: Box::new(lambda.ret_type.clone()),
            is_public: true,
            loc: lambda.loc,
        };

        func_type = self
            .expand_sema_type(SemaType::FuncType(func_type), lambda.loc)
            .as_func_type()
            .unwrap().clone();

        let func_name = "<unnamed>";

        let lambda_env = self.create_func_def_env(func_name, func_type.clone(), None);

        self.with_func_env(lambda_env, |this| {
            this.analyze_block_stmt(&mut lambda.body);
        });

        Some(SemaType::FuncType(func_type))
    }
}
