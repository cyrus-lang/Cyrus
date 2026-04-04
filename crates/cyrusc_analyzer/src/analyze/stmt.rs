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
use cyrusc_internal::flow_state::FlowState;
use cyrusc_typed_ast::stmts::TypedStmt;

impl<'a> AnalysisContext<'a> {
    // Traverse TypedAST
    pub fn analyze(&mut self) {
        let mut body = {
            let mut tree_borrowed = self.program_tree.borrow_mut();
            std::mem::take(&mut tree_borrowed.body)
        };

        for mut typed_stmt in &mut body {
            match &mut typed_stmt {
                TypedStmt::GlobalVar(typed_global_var) => self.analyze_global_var(typed_global_var),
                TypedStmt::FuncDef(typed_func_def) => self.analyze_func_def(typed_func_def),
                TypedStmt::FuncDecl(typed_func_decl) => self.analyze_func_decl(typed_func_decl),
                TypedStmt::Interface(typed_interface) => self.analyze_interface(typed_interface),
                TypedStmt::Struct(struct_stmt) => self.analyze_struct_stmt(struct_stmt),
                TypedStmt::Enum(typed_enum) => {
                    // self.analyze_enum(typed_enum)
                    // FIXME
                    todo!()
                }
                TypedStmt::Union(union_stmt) => {
                    // self.analyze_union(union_stmt)
                    // FIXME
                    todo!()
                }
                TypedStmt::Typedef(typed_typedef) => self.analyze_typedef(typed_typedef),
                TypedStmt::Variable(_)
                | TypedStmt::ExportTuple(_)
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
                    unreachable!()
                }
                TypedStmt::Builtin(_typed_builtin) => todo!(),
            }
        }

        self.program_tree.borrow_mut().body = body;
    }

    pub(crate) fn analyze_stmt(&mut self, typed_stmt: &mut TypedStmt) -> FlowState {
        match typed_stmt {
            TypedStmt::Expr(typed_expr) => {
                self.analyze_expr(typed_expr, typed_expr.sema_type.clone());
                FlowState::Reachable
            }
            TypedStmt::Variable(typed_variable) => {
                self.analyze_variable(typed_variable);
                FlowState::Reachable
            }
            TypedStmt::BlockStmt(typed_block_statement) => self.analyze_block_stmt(typed_block_statement),
            TypedStmt::ExportTuple(typed_export_tuple_values) => {
                // self.analyze_export_tuple_values(typed_export_tuple_values);
                // FlowState::Reachable

                // FIXME
                todo!()
            }
            TypedStmt::If(typed_if) => self.analyze_if_stmt(typed_if, None),
            TypedStmt::Return(typed_return) => self.analyze_return(typed_return),
            TypedStmt::Break(typed_break) => {
                self.analyze_break(typed_break);
                FlowState::Unreachable
            }
            TypedStmt::Continue(typed_continue) => {
                self.analyze_continue(typed_continue);
                FlowState::Unreachable
            }
            TypedStmt::Goto(typed_goto) => {
                // FIXME
                // self.analyze_goto(typed_goto);
                // FlowState::Reachable
                todo!()
            }
            TypedStmt::For(typed_for) => self.analyze_for_loop(typed_for),
            TypedStmt::While(typed_while) => self.analyze_while_loop(typed_while),
            TypedStmt::Switch(typed_switch) => self.analyze_switch(typed_switch),

            // skipped
            TypedStmt::Label(..) => FlowState::Reachable,

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
}
