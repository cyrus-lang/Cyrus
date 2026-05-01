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

use crate::context::AnalysisContext;
use cyrusc_typed_ast::{
    exprs::TypedLambdaExpr,
    types::{SemaType, TypedFuncType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_lambda(&mut self, lambda: &mut TypedLambdaExpr) -> Option<SemaType> {
        self.normalize_func_params(&mut lambda.params, true);

        let params = lambda.params.as_func_type_params();

        lambda.ret_type = self.normalize_and_check_type_formation(lambda.ret_type.clone(), lambda.loc)?;

        let func_type = TypedFuncType {
            params,
            ret_type: Box::new(lambda.ret_type.clone()),
            is_public: true,
            loc: lambda.loc,
        };

        let lambda_env = self.create_func_def_env(func_type.clone());

        self.with_func_env(lambda_env, |this| {
            this.analyze_block_stmt(&mut lambda.body);
        });

        Some(SemaType::FuncType(func_type))
    }
}
