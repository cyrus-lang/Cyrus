// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_const_eval::value::is_expr_const_evaluable;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    format::format_sema_type,
    stmts::{TypedGlobalVarStmt, TypedVarStmt},
    types::SemaType,
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_global_var(&mut self, global_var: &mut TypedGlobalVarStmt) {
        if let Some(mut expr) = global_var.expr.clone() {
            match self.analyze_expr(&mut expr, global_var.ty.clone()) {
                Some(sema_type) => Some(sema_type),
                None => return,
            };

            if !is_expr_const_evaluable(&expr.kind) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::GlobalVariableExprNotComptimeValid),
                    loc: Some(global_var.loc),
                    hint: None,
                });
                return;
            }

            global_var.expr = Some(expr);
        }

        global_var.ty = match &global_var.ty {
            Some(ty) => self.normalize_and_check_type_formation(ty.clone(), global_var.loc, 0),
            None => match global_var.expr.as_ref().and_then(|expr| expr.ty.clone()) {
                Some(ty) => Some(ty),
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::GlobalVarRequiresTypeAnnotation),
                        loc: Some(global_var.loc),
                        hint: None,
                    });
                    return;
                }
            },
        };

        if let Some(ty) = &global_var.ty {
            if !self.validate_variable_type(ty, global_var.expr.is_some(), global_var.loc) {
                return;
            }
        }

        if let Some(expr) = &global_var.expr {
            if !is_expr_const_evaluable(&expr.kind) && !matches!(global_var.ty, Some(SemaType::Const(..))) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::GlobalVariableExprNotComptimeValid),
                    loc: Some(global_var.loc),
                    hint: None,
                });
                return;
            }
        }

        if let Some(expr) = &global_var.expr {
            if let Some(target_type) = &global_var.ty {
                if self.is_const_qualified_type_assigned_to_non_const_variable(&target_type, global_var.is_const) {
                    self.report_const_qualified_type_assigned_to_non_const_variable(global_var.loc);
                }

                let expr_type = expr.ty.clone().unwrap();

                if *expr_type.const_inner() != *target_type.const_inner() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                            lhs_type: format_sema_type(target_type.clone(), self.formatter),
                            rhs_type: format_sema_type(expr_type.clone(), self.formatter),
                        }),
                        loc: Some(global_var.loc),
                        hint: Some("Global variable initializers must exactly match the declared type.".into()),
                    });
                }
            }
        }

        self.decl_tables
            .with_global_var_decl_mut(global_var.global_var_decl_id, |global_var_decl| {
                global_var_decl.rhs = global_var.expr.clone();
                global_var_decl.ty = global_var.ty.clone();
            });
    }

    pub(crate) fn analyze_variable(&mut self, var: &mut TypedVarStmt) {
        if let Some(ty) = &var.ty {
            var.ty = self.normalize_and_check_type_formation(ty.clone(), var.loc, 0);
        }

        if let Some(expr) = &mut var.rhs {
            let Some(inferred_type) = self.analyze_expr(expr, var.ty.clone()) else {
                return;
            };

            if var.ty.is_none() {
                var.ty = Some(inferred_type);
            }

            var.ty = Some(self.coerce_interface_as_interface_object_if_possible(&var.ty.as_ref().unwrap(), &expr));
        }

        if let Some(ty) = &var.ty {
            if self.is_const_qualified_type_assigned_to_non_const_variable(ty, var.is_const) {
                self.report_const_qualified_type_assigned_to_non_const_variable(var.loc);
            }

            if !self.validate_variable_type(ty, var.rhs.is_some(), var.loc) {
                return;
            }
        }

        if let Some(expr) = &mut var.rhs {
            if let Some(target_type) = &var.ty {
                if !self.is_assignable_to(expr.ty.clone().unwrap(), target_type.clone(), var.loc) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                            lhs_type: format_sema_type(target_type.clone(), self.formatter),
                            rhs_type: format_sema_type(expr.ty.clone().unwrap(), self.formatter),
                        }),
                        loc: Some(var.loc),
                        hint: None,
                    });
                }
            }
        }

        self.decl_tables.with_var_decl_mut(var.var_decl_id, |var_decl| {
            var_decl.rhs = var.rhs.clone();
            var_decl.ty = var.ty.clone();
        });
    }

    fn validate_variable_type(&mut self, ty: &SemaType, is_init: bool, loc: Loc) -> bool {
        if ty.const_inner().is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidVariableType),
                loc: Some(loc),
                hint: None,
            });
        }

        if ty.const_inner().is_const() && !is_init {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ConstVariableMustBeInitialized),
                loc: Some(loc),
                hint: Some("Declare the variable with an initializer or remove the 'const' qualifier.".to_string()),
            });
        }

        if ty.const_inner().is_interface() && !is_init {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InterfaceTypeMustBeInitialized),
                loc: Some(loc),
                hint: None,
            });
        }

        if ty.const_inner().is_func_type() && !is_init {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::LambdaTypeMustBeInitialized),
                loc: Some(loc),
                hint: None,
            });
        }

        self.check_type_arity(ty.clone(), loc).is_some()
    }
}
