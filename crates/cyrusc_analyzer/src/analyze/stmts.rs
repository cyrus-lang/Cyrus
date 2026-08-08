// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::flow_state::FlowState;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    builtins::{TypedBuiltin, is_builtin_unreachable}, exprs::{TypedExpr, TypedExprKind, ValueCategory}, stmts::{TypedStmt, TypedStmtKind},
};

impl<'a> AnalysisContext<'a> {
    // Traverse TypedAST
    pub fn analyze(&mut self) {
        let mut body = {
            let mut tree_borrowed = self.program_tree.borrow_mut();
            std::mem::take(&mut tree_borrowed.body)
        };

        self.analyze_toplevel_stmts(&mut body);

        self.program_tree.borrow_mut().body = body;
    }

    pub(crate) fn analyze_toplevel_stmts(&mut self, typed_stmts: &mut [TypedStmt]) {
        for stmt in &mut *typed_stmts {
            match &mut stmt.kind {
                TypedStmtKind::GlobalVar(global_var) => self.analyze_global_var(global_var),
                _ => continue,
            }
        }

        for stmt in &mut *typed_stmts {
            self.analyze_toplevel_stmt(&mut stmt.kind);
        }
    }

    fn analyze_toplevel_stmt(&mut self, typed_stmt: &mut TypedStmtKind) {
        match typed_stmt {
            TypedStmtKind::GlobalVar(_) => {
                // Skipped, because it's intended to be analyzed
                // before all of the other top level statements.
                return;
            }

            TypedStmtKind::FuncDef(func_def_stmt) => self.analyze_func_def(func_def_stmt),
            TypedStmtKind::FuncDecl(func_decl_stmt) => self.analyze_func_decl_stmt(func_decl_stmt),
            TypedStmtKind::Interface(interface) => self.analyze_interface(interface),
            TypedStmtKind::Struct(struct_stmt) => self.analyze_struct_stmt(struct_stmt),
            TypedStmtKind::Enum(enum_stmt) => self.analyze_enum_stmt(enum_stmt),
            TypedStmtKind::Union(union_stmt) => self.analyze_union_stmt(union_stmt),
            TypedStmtKind::Typedef(typedef) => self.analyze_typedef(typedef),

            TypedStmtKind::Builtin(_) => {
                self.analyze_builtin(typed_stmt, true);
            }

            // invalid at toplevel
            TypedStmtKind::Variable(_)
            | TypedStmtKind::TupleExport(_)
            | TypedStmtKind::BlockStmt(_)
            | TypedStmtKind::Defer(_)
            | TypedStmtKind::If(_)
            | TypedStmtKind::Return(_)
            | TypedStmtKind::Break(_)
            | TypedStmtKind::Continue(_)
            | TypedStmtKind::For(_)
            | TypedStmtKind::While(_)
            | TypedStmtKind::Switch(_)
            | TypedStmtKind::Label(_)
            | TypedStmtKind::Goto(_)
            | TypedStmtKind::Expr(_) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidStatement),
                    loc: Some(typed_stmt.loc()),
                    hint: None,
                });
            }
        }
    }

    pub(crate) fn analyze_stmt(&mut self, typed_stmt: &mut TypedStmtKind) -> FlowState {
        if let TypedStmtKind::Builtin(builtin) = typed_stmt {
            return match builtin {
                TypedBuiltin::BuiltinFunc(builtin_func) => {
                    let loc = builtin_func.loc;
                    let name = builtin_func.name.value.clone();
                    let builtin_func_clone = builtin_func.clone();

                    let mut builtin_expr = TypedExpr {
                        kind: TypedExprKind::Builtin(TypedBuiltin::BuiltinFunc(builtin_func_clone)),
                        ty: None,
                        val_cat: ValueCategory::RValue,
                        analyzed: false,
                        loc,
                    };

                    if self.analyze_expr(&mut builtin_expr, None).is_none() {
                        return FlowState::Reachable;
                    }

                    *typed_stmt = TypedStmtKind::Expr(builtin_expr);

                    if is_builtin_unreachable(&name) {
                        FlowState::Unreachable
                    } else {
                        FlowState::Reachable
                    }
                }
                TypedBuiltin::BuiltinBlock(_) => self.analyze_builtin(typed_stmt, false),
            };
        }

        match typed_stmt {
            TypedStmtKind::BlockStmt(block) => self.analyze_block_stmt(block),

            TypedStmtKind::Expr(expr) => {
                self.analyze_expr(expr, expr.ty.clone());
                FlowState::Reachable
            }

            TypedStmtKind::Variable(var) => {
                self.analyze_var(var);
                FlowState::Reachable
            }

            TypedStmtKind::TupleExport(export_tuple) => {
                self.analyze_export_tuple_values(export_tuple);
                FlowState::Reachable
            }

            TypedStmtKind::If(if_stmt) => self.analyze_if_stmt(if_stmt),
            TypedStmtKind::For(for_stmt) => self.analyze_for_loop(for_stmt),
            TypedStmtKind::While(while_stmt) => self.analyze_while_loop(while_stmt),
            TypedStmtKind::Break(break_stmt) => self.analyze_break(break_stmt),
            TypedStmtKind::Continue(continue_stmt) => self.analyze_continue(continue_stmt),
            TypedStmtKind::Return(return_stmt) => self.analyze_return(return_stmt),
            TypedStmtKind::Switch(switch_stmt) => self.analyze_switch(switch_stmt),

            // skipped
            TypedStmtKind::Goto(_) => FlowState::Reachable,
            TypedStmtKind::Label(_) => FlowState::Reachable,

            // invalid statements
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidStatement),
                    loc: Some(typed_stmt.loc()),
                    hint: None,
                });
                return FlowState::Reachable;
            }
        }
    }

    pub(crate) fn validate_align(&mut self, align: &Option<usize>, loc: Loc) {
        if let Some(align) = align {
            if !align.is_power_of_two() {
                self.reporter.report(Diag {
                    kind: Box::new(AnalyzerDiagKind::InvalidAlign { value: *align }),
                    level: DiagLevel::Error,
                    loc: Some(loc),
                    hint: Some("Alignment values must be powers of two (1, 2, 4, 8, 16, ...).".to_string()),
                });
            }
        }
    }
}
