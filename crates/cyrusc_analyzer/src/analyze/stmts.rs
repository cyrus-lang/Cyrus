// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::flow_state::FlowState;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    builtins::{TypedBuiltin, is_builtin_unreachable},
    exprs::{TypedExpr, TypedExprKind, ValueCategory},
    stmts::TypedStmt,
};

impl<'a> AnalysisContext<'a> {
    // Traverse TypedAST
    pub fn analyze(&mut self) {
        let mut body = {
            let mut tree_borrowed = self.program_tree.borrow_mut();
            std::mem::take(&mut tree_borrowed.body)
        };

        for typed_stmt in &mut body {
            self.analyze_toplevel_stmt(typed_stmt);
        }

        self.program_tree.borrow_mut().body = body;
    }

    pub(crate) fn analyze_toplevel_stmt(&mut self, typed_stmt: &mut TypedStmt) {
        match typed_stmt {
            TypedStmt::GlobalVar(global_var) => self.analyze_global_var(global_var),
            TypedStmt::FuncDef(func_def_stmt) => self.analyze_func_def(func_def_stmt),
            TypedStmt::FuncDecl(func_decl_stmt) => self.analyze_func_decl_stmt(func_decl_stmt),
            TypedStmt::Interface(interface) => self.analyze_interface(interface),
            TypedStmt::Struct(struct_stmt) => self.analyze_struct_stmt(struct_stmt),
            TypedStmt::Enum(enum_stmt) => self.analyze_enum_stmt(enum_stmt),
            TypedStmt::Union(union_stmt) => self.analyze_union_stmt(union_stmt),
            TypedStmt::Typedef(typedef) => self.analyze_typedef(typedef),

            TypedStmt::Builtin(_) => {
                self.analyze_builtin(typed_stmt, true);
            }

            // invalid at toplevel
            TypedStmt::Variable(_)
            | TypedStmt::TupleExport(_)
            | TypedStmt::BlockStmt(_)
            | TypedStmt::Defer(_)
            | TypedStmt::If(_)
            | TypedStmt::Return(_)
            | TypedStmt::Break(_)
            | TypedStmt::Continue(_)
            | TypedStmt::For(_)
            | TypedStmt::While(_)
            | TypedStmt::Switch(_)
            | TypedStmt::Label(_)
            | TypedStmt::Goto(_)
            | TypedStmt::Expr(_) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidStatement),
                    loc: Some(typed_stmt.loc()),
                    hint: None,
                });
            }
        }
    }

    pub(crate) fn analyze_stmt(&mut self, typed_stmt: &mut TypedStmt) -> FlowState {
        if let TypedStmt::Builtin(builtin) = typed_stmt {
            return match builtin {
                TypedBuiltin::BuiltinFunc(builtin_func) => {
                    let loc = builtin_func.loc;
                    let name = builtin_func.name.value.clone();
                    let builtin_func_clone = builtin_func.clone();

                    let mut builtin_expr = TypedExpr {
                        kind: TypedExprKind::Builtin(TypedBuiltin::BuiltinFunc(builtin_func_clone)),
                        ty: None,
                        val_cat: ValueCategory::RValue,
                        loc,
                    };

                    if self.analyze_expr(&mut builtin_expr, None).is_none() {
                        return FlowState::Reachable;
                    }

                    *typed_stmt = TypedStmt::Expr(builtin_expr);

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
            TypedStmt::Expr(expr) => {
                self.analyze_expr(expr, expr.ty.clone());
                FlowState::Reachable
            }
            TypedStmt::Variable(var) => {
                self.analyze_variable(var);
                FlowState::Reachable
            }
            TypedStmt::BlockStmt(block) => self.analyze_block_stmt(block),
            TypedStmt::TupleExport(export_tuple) => {
                self.analyze_export_tuple_values(export_tuple);
                FlowState::Reachable
            }
            TypedStmt::If(if_stmt) => self.analyze_if_stmt(if_stmt),
            TypedStmt::For(for_stmt) => self.analyze_for_loop(for_stmt),
            TypedStmt::While(while_stmt) => self.analyze_while_loop(while_stmt),
            TypedStmt::Break(break_stmt) => self.analyze_break(break_stmt),
            TypedStmt::Continue(continue_stmt) => self.analyze_continue(continue_stmt),
            TypedStmt::Return(return_stmt) => self.analyze_return(return_stmt),
            TypedStmt::Switch(switch_stmt) => self.analyze_switch(switch_stmt),

            // skipped
            TypedStmt::Goto(_) => FlowState::Reachable,
            TypedStmt::Label(_) => FlowState::Reachable,

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
