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
use cyrusc_internal::flow_state::{ControlRegion, FlowState};
use cyrusc_typed_ast::{
    format::format_sema_type,
    stmts::{TypedBreakStmt, TypedContinueStmt, TypedForStmt, TypedIfStmt, TypedReturnStmt, TypedWhileStmt},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_if_stmt(&mut self, if_stmt: &mut TypedIfStmt) -> FlowState {
        let then_state = self.analyze_block_stmt(&mut if_stmt.then_block);

        self.analyze_cond_expr(&mut if_stmt.cond);

        let else_state = {
            if let Some(block_stmt) = &mut if_stmt.else_block {
                self.analyze_block_stmt(&mut *block_stmt)
            } else {
                FlowState::Reachable
            }
        };

        if_stmt.branches.iter_mut().for_each(|branch| {
            self.analyze_if_stmt(branch);
        });

        then_state.merge(else_state)
    }

    pub(crate) fn analyze_while_loop(&mut self, typed_while: &mut TypedWhileStmt) -> FlowState {
        self.analyze_cond_expr(&mut typed_while.cond);

        self.control_stack.push(ControlRegion::Loop);
        self.analyze_block_stmt(&mut typed_while.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    pub(crate) fn analyze_for_loop(&mut self, typed_for: &mut TypedForStmt) -> FlowState {
        if let Some(initializer) = &mut typed_for.initializer {
            self.analyze_variable(initializer);
        }

        if let Some(cond) = &mut typed_for.cond {
            self.analyze_cond_expr(cond);
        }

        if let Some(typed_expr) = &mut typed_for.increment {
            self.analyze_expr(typed_expr, None);
        }

        self.control_stack.push(ControlRegion::Loop);
        self.analyze_block_stmt(&mut typed_for.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    pub(crate) fn analyze_return(&mut self, ret: &mut TypedReturnStmt) -> FlowState {
        let func_type = self.func_env.current_func.clone().unwrap();
        let ret_type = self
            .normalize_and_check_type_formation(*func_type.ret_type, ret.loc)
            .unwrap();

        if ret_type.is_void() && ret.arg.is_some() {
            dbg!(ret.clone());

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidFunctionReturnsValue),
                loc: Some(ret.loc),
                hint: None,
            });
        } else if let Some(expr) = &mut ret.arg {
            if let Some(sema_type) = self.analyze_expr(expr, Some(ret_type.clone())) {
                if !self.is_assignable_to(sema_type.clone(), ret_type.clone(), expr.loc) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ReturnStatementTypeMismatch {
                            expected: format_sema_type(ret_type.const_inner().clone(), self.formatter),
                            got: format_sema_type(sema_type.const_inner().clone(), self.formatter),
                        }),
                        loc: Some(ret.loc),
                        hint: None,
                    });
                }
            }
        } else if !ret_type.is_void() && ret.arg.is_none() {
            let argument_type = format_sema_type(ret_type.clone(), self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ReturnStatementNeedsAnArgument { argument_type }),
                loc: Some(ret.loc),
                hint: None,
            });
        }

        FlowState::Returns
    }

    pub(crate) fn analyze_break(&mut self, break_stmt: &TypedBreakStmt) -> FlowState {
        let valid = self.control_stack.iter().rev().any(|c| c.allows_break());

        if !valid {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidBreakStatement),
                loc: Some(break_stmt.loc),
                hint: None,
            });
            FlowState::Reachable
        } else {
            FlowState::Unreachable
        }
    }

    pub(crate) fn analyze_continue(&mut self, continue_stmt: &TypedContinueStmt) -> FlowState {
        let valid = self.control_stack.iter().rev().any(|c| c.allows_continue());

        if !valid {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidContinueStatement),
                loc: Some(continue_stmt.loc),
                hint: None,
            });
            FlowState::Reachable
        } else {
            FlowState::Unreachable
        }
    }
}
