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
use cyrusc_typed_ast::stmts::TypedBlockStmt;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_block_stmt(&mut self, block_stmt: &mut TypedBlockStmt) -> FlowState {
        let mut flow_state = FlowState::Reachable;
        let mut terminated = false;

        let stmts = std::mem::take(&mut block_stmt.stmts);
        let mut final_stmts = Vec::with_capacity(block_stmt.stmts.len());

        for mut stmt in stmts {
            let stmt_state = self.analyze_stmt(&mut stmt);

            if terminated {
                if self.config.warnings.enabled {
                    self.reporter.report(Diag {
                        level: DiagLevel::Warning,
                        kind: Box::new(AnalyzerDiagKind::UnreachableCode),
                        loc: Some(stmt.loc()),
                        hint: None,
                    });
                }
                continue;
            }

            match stmt_state {
                FlowState::Reachable => {
                    final_stmts.push(stmt);
                }
                FlowState::Unreachable => {
                    final_stmts.push(stmt);
                    flow_state = FlowState::Unreachable;
                    terminated = true;
                }
                FlowState::Returns => {
                    final_stmts.push(stmt);
                    flow_state = FlowState::Returns;
                    terminated = true;
                }
            }
        }

        block_stmt.stmts = final_stmts;

        for defer in &mut block_stmt.defers {
            self.analyze_stmt(&mut defer.operand);
        }

        flow_state
    }
}
