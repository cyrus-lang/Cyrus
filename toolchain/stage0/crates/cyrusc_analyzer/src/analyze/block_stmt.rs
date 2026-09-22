// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::flow_state::FlowState;
use cyrusc_typed_ast::stmts::{TypedBlockStmt, TypedDeferStmt, TypedStmtKind};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_defer_stmt(&mut self, defer_stmt: &mut TypedDeferStmt) {
        // IMPORTANT Only analyze when traversing tree!
        // analyze_block_stmt will handle defer management
        // according to flow state.
        self.analyze_stmt(&mut defer_stmt.operand.kind);
    }

    pub(crate) fn analyze_block_stmt(&mut self, block_stmt: &mut TypedBlockStmt) -> FlowState {
        let mut flow_state = FlowState::Reachable;
        let mut terminated = false;

        let stmts = std::mem::take(&mut block_stmt.stmts);
        let mut final_stmts = Vec::with_capacity(stmts.len());
        let mut block_defers = Vec::new();

        for mut stmt in stmts {
            let stmt_state = self.analyze_stmt(&mut stmt.kind);

            // We collect defer statements if block is not terminated yet
            if let TypedStmtKind::Defer(defer) = &mut stmt.kind {
                defer.operand.is_dead = terminated;
                block_defers.push(defer.clone());
            }

            if terminated {
                stmt.is_dead = true;

                if self.config.warnings.enabled && !self.reporter.has_errors() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Warning,
                        kind: Box::new(AnalyzerDiagKind::UnreachableCode),
                        loc: Some(stmt.kind.loc()),
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
        block_stmt.defers = block_defers;

        flow_state
    }
}
