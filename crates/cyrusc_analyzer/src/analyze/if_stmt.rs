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

use crate::{context::AnalysisContext};
use cyrusc_internal::flow_state::FlowState;
use cyrusc_typed_ast::{stmts::TypedIfStmt, types::SemanticType};

impl<'a> AnalysisContext<'a> {
    fn analyze_if_stmt(&mut self, if_stmt: &mut TypedIfStmt, expected_type: Option<SemanticType>) -> FlowState {
        let then_state = self.analyze_block_stmt(&mut if_stmt.then_block);

        if let Some(sema_type) = self.analyze_expr(&mut if_stmt.cond, expected_type.clone()) {
            self.report_if_not_cond_expr(sema_type, if_stmt.loc);
        }

        let else_state = {
            if let Some(block_stmt) = &mut if_stmt.else_block {
                self.analyze_block_stmt(&mut *block_stmt)
            } else {
                FlowState::Reachable
            }
        };

        if_stmt.branches.iter_mut().for_each(|branch| {
            self.analyze_if_stmt(branch, expected_type.clone());
        });

        then_state.merge(else_state)
    }
}
