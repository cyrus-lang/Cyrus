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

use crate::{context::AnalysisContext, lower::lower_assign::lower_assign_to_infix_expr};
use cyrusc_ast::{AssignKind, operators::PrefixOperator};
use cyrusc_typed_ast::{
    exprs::{TypedExprKind, TypedExprStmt},
    types::SemanticType,
};

pub(crate) mod lower_assign;
pub(crate) mod lower_prefix_not;

impl<'a> AnalysisContext<'a> {
    /// Rewrites special expression forms into their canonical AST representation.
    pub(crate) fn lower_special_exprs(&mut self, typed_expr: &mut TypedExprStmt, expected_type: Option<SemanticType>) {
        match &mut typed_expr.kind {
            TypedExprKind::Assign(assign) => {
                if assign.kind != AssignKind::Default {
                    typed_expr.kind = lower_assign_to_infix_expr(assign);
                }
            }
            TypedExprKind::Prefix(prefix) => {
                match prefix.op {
                    PrefixOperator::Bang => {
                        if let Some(lowered_typed_expr) = self.lower_prefix_not_pointer(expected_type.clone(), prefix) {
                            *typed_expr = lowered_typed_expr;
                        }
                    }
                    _ => {}
                };
            }
            _ => {}
        };
    }
}
