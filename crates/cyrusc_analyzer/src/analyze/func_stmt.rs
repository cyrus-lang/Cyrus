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

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{
    stmts::{TypedFuncDeclStmt, TypedFuncDefStmt},
    types::TypedFuncType,
};

impl<'a> AnalysisContext<'a> {
    fn analyze_func_def(&mut self, func_def: &mut TypedFuncDefStmt) {
        self.analyze_entry_func(func_def);

        let is_public = func_def.modifiers.vis.is_public();
        let is_generic_func = func_def.is_generic();

        // if let Some(generic_params) = &func_def.generic_params {
        //     self.analyze_generic_params(generic_params);
        // }

        let func_type_params = func_def.params.as_func_type_params();

        self.fenv.current_func_type = Some(TypedFuncType {
            symbol_id: Some(func_def.symbol_id),
            params: func_type_params,
            ret_type: Box::new(func_def.ret_type.clone()),
            is_public,
            loc: func_def.loc,
        });

        self.normalize_func_params(&mut func_def.params, func_def.loc);

        let Some(ret_type) = self.normalize_sema_type(func_def.ret_type.clone(), func_def.loc) else {
            return;
        };

        func_def.ret_type = ret_type;

        if !is_generic_func {
            self.analyze_func_body(&mut func_def.body, &func_def.ret_type);
        }

        let func_decl_id = self.query.get_func(func_def.symbol_id).unwrap();
        self.decl_tables.with_func_decl_mut(func_decl_id, |func_decl| {
            func_decl.params = func_def.params.clone();
            func_decl.ret_type = func_def.ret_type.clone();
        });
    }

    fn analyze_func_decl(&mut self, func_decl_stmt: &mut TypedFuncDeclStmt) {
        if func_decl_stmt.generic_params.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::GenericFunctionDeclaration),
                loc: Some(func_decl_stmt.loc),
                hint: None,
            });
        }

        self.report_if_duplicate_param_names(
            &func_decl_stmt.params.list,
            func_decl_stmt.params.variadic.as_ref(),
            func_decl_stmt.loc,
        );

        func_decl_stmt.ret_type = match self.normalize_sema_type(func_decl_stmt.ret_type.clone(), func_decl_stmt.loc) {
            Some(sema_type) => sema_type,
            None => return,
        };

        self.normalize_func_params(&mut func_decl_stmt.params, func_decl_stmt.loc);

        let func_decl_id = self.query.get_func(func_decl_stmt.symbol_id).unwrap();
        self.decl_tables.with_func_decl_mut(func_decl_id, |func_decl| {
            func_decl.params = func_decl_stmt.params.clone();
            func_decl.ret_type = func_decl_stmt.ret_type.clone();
        });
    }
}
