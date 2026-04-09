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
    types::{SemanticType, TypedFuncType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_lambda(&mut self, lambda: &mut TypedLambdaExpr) -> Option<SemanticType> {
        let parent_func = self.func_env.current_func.clone();

        self.normalize_func_params(&mut lambda.params, lambda.loc);
        let params = lambda.params.as_func_type_params();

        lambda.ret_type = self.normalize_and_check_type_formation(lambda.ret_type.clone(), lambda.loc)?;

        let func_type = TypedFuncType {
            symbol_id: None,
            params,
            ret_type: Box::new(lambda.ret_type.clone()),
            is_public: true,
            loc: lambda.loc,
        };

        self.func_env.current_func = Some(func_type.clone());
        self.analyze_block_stmt(&mut lambda.body);

        self.func_env.current_func = parent_func;
        Some(SemanticType::FuncType(func_type))
    }
}
