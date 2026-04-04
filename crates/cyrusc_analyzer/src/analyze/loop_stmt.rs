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
    stmts::{TypedBreakStmt, TypedContinueStmt, TypedForStmt, TypedReturnStmt, TypedWhileStmt},
    types::{PlainType, SemanticType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_while_loop(&mut self, typed_while: &mut TypedWhileStmt) -> FlowState {
        // REVIEW: Replace it with analyze_cond_expr.
        if let Some(sema_type) = self.analyze_expr(&mut typed_while.cond, Some(SemanticType::Plain(PlainType::Bool))) {
            self.report_if_not_cond_expr(sema_type, typed_while.loc);
        }

        self.control_stack.push(ControlRegion::Loop);
        self.analyze_block_stmt(&mut typed_while.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    pub(crate) fn analyze_for_loop(&mut self, typed_for: &mut TypedForStmt) -> FlowState {
        if let Some(initializer) = &mut typed_for.initializer {
            self.analyze_variable(initializer);
        }

        if let Some(typed_expr) = &mut typed_for.cond {
            // REVIEW: Replace it with analyze_cond_expr.
            if let Some(sema_type) = self.analyze_expr(typed_expr, Some(SemanticType::Plain(PlainType::Bool))) {
                self.report_if_not_cond_expr(sema_type, typed_for.loc);
            }
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
        let func_type = self.fenv.current_func_type.clone().unwrap();
        let ret_type = self.normalize_sema_type(*func_type.ret_type, ret.loc).unwrap();

        if ret_type.is_void() && ret.arg.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidFunctionReturnsValue),
                loc: Some(ret.loc),
                hint: None,
            });
        } else if let Some(typed_expr) = &mut ret.arg {
            if let Some(sema_type) = self.analyze_expr(typed_expr, Some(ret_type.clone())) {
                if !self.is_assignable_to(sema_type.clone(), ret_type.clone(), ret.loc) {
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
