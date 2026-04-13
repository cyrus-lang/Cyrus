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

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, infer::InferCtx};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::flow_state::FlowState;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::FuncDecl,
    stmts::{TypedBlockStmt, TypedFuncDeclStmt, TypedFuncDefStmt, TypedFuncParams},
    types::SemaType,
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_func_def(&mut self, func_def: &mut TypedFuncDefStmt) {
        let is_generic_func = func_def.is_generic();

        self.analyze_entry_func(func_def);

        let mut func_decl = self.decl_tables.func_decl(func_def.func_decl_id);

        self.analyze_func_decl(&mut func_decl);

        func_def.params = func_decl.params.clone();
        func_def.ret_type = func_decl.ret_type.clone();

        self.decl_tables
            .with_func_decl_mut(func_def.func_decl_id, |_func_decl| {
                _func_decl.params = func_decl.params.clone();
                _func_decl.ret_type = func_decl.ret_type.clone();
            });

        if !is_generic_func {
            let func_type = func_decl.as_func_type();
            let func_env = self.create_func_def_env(func_type.clone());

            self.with_func_env(func_env, |this| {
                this.analyze_func_body(&mut func_def.body, &func_def.ret_type);
            });
        }
    }

    pub(crate) fn analyze_func_decl_stmt(&mut self, func_decl_stmt: &mut TypedFuncDeclStmt) {
        if func_decl_stmt.is_generic() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::GenericFunctionDeclaration),
                loc: Some(func_decl_stmt.loc),
                hint: None,
            });
        }

        let mut func_decl = self.decl_tables.func_decl(func_decl_stmt.func_decl_id);

        self.analyze_func_decl(&mut func_decl);

        func_decl_stmt.params = func_decl.params.clone();
        func_decl_stmt.ret_type = func_decl.ret_type.clone();

        self.decl_tables.with_func_decl_mut(func_decl_stmt.func_decl_id, |_func_decl| {
            _func_decl.params = func_decl.params.clone();
            _func_decl.ret_type = func_decl.ret_type.clone();
        });
    }

    pub(crate) fn analyze_func_decl(&mut self, func_decl: &mut FuncDecl) {
        self.report_if_duplicate_param_names(
            &func_decl.params.list,
            func_decl.params.variadic.as_ref(),
            func_decl.loc,
        );

        func_decl.ret_type = match self.normalize_and_check_type_formation(func_decl.ret_type.clone(), func_decl.loc) {
            Some(sema_type) => sema_type,
            None => return,
        };

        self.normalize_func_params(&mut func_decl.params, func_decl.loc);
    }

    pub(crate) fn analyze_func_body(&mut self, body: &mut TypedBlockStmt, ret_type: &SemaType) {
        let created_infer = {
            if self.func_env.infer.is_none() {
                self.func_env.infer = Some(InferCtx::new());
                true
            } else {
                false
            }
        };

        let state = self.analyze_block_stmt(body);

        if !ret_type.is_void() && state != FlowState::Returns {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MissingReturn),
                loc: Some(body.loc),
                hint: Some("Not all control paths return a value.".to_string()),
            });
        }

        if created_infer {
            self.func_env.infer = None;
        }
    }

    fn analyze_entry_func(&mut self, func_def: &mut TypedFuncDefStmt) {
        let is_public = func_def.modifiers.vis.is_public();

        if func_def.name == "main" {
            self.entry_points.add(func_def.loc);

            if !is_public {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::PrivateEntryPoint),
                    loc: Some(func_def.loc),
                    hint: Some("Declare it as 'pub' so the runtime and linker can reliably discover it.".to_string()),
                });
            }
        }
    }

    /// Takes a cloned parameter list and returns a new list with
    /// substituted, inference-resolved concrete types.
    ///
    /// Zero global mutation. Pure transformation.
    pub(crate) fn update_param_variables_type(&mut self, mut params: TypedFuncParams) -> TypedFuncParams {
        for param_kind in &mut params.list {
            let var_decl_id = param_kind.var_decl_id().unwrap();

            let original_type = self.decl_tables.var_decl(var_decl_id).ty.unwrap();

            let mut sema_ty = self.substitute_type(&original_type);

            if let Some(infer) = &self.func_env.infer {
                sema_ty = infer.resolve(&sema_ty);
            }

            *param_kind.param_type_mut() = sema_ty;
        }

        params
    }

    pub(crate) fn validate_param_type(&mut self, sema_type: &SemaType, loc: Loc) {
        let sema_type = sema_type.const_inner();

        if sema_type.is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidParameterType),
                loc: Some(loc),
                hint: None,
            });
        }

        self.check_type_arity(sema_type.clone(), loc);
    }
}
