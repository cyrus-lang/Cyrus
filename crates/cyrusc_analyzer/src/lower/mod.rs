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
    types::SemaType,
};

pub(crate) mod lower_assign;
pub(crate) mod lower_enum_init;
pub(crate) mod lower_prefix_not;
pub(crate) mod lower_struct_init;
pub(crate) mod lower_union_init;

impl<'a> AnalysisContext<'a> {
    /// Rewrites special expression forms into their canonical AST representation.
    pub(crate) fn lower_expr_pre_analysis(&mut self, typed_expr: &mut TypedExprStmt, expected_type: Option<SemaType>) {
        match &mut typed_expr.kind {
            TypedExprKind::Assign(assign) => {
                if assign.kind != AssignKind::Default {
                    typed_expr.kind = lower_assign_to_infix_expr(assign);
                }
            }
            TypedExprKind::Prefix(prefix) => {
                if let PrefixOperator::Bang = prefix.op {
                    if let Some(lowered_typed_expr) = self.lower_prefix_not_pointer(expected_type.clone(), prefix) {
                        *typed_expr = lowered_typed_expr;
                    }
                };
            }
            _ => {
                self.lower_field_access_as_enum_init(typed_expr);
                self.lower_method_call_as_enum_init(typed_expr);
                self.lower_struct_init_as_union_init(typed_expr);
            }
        };
    }

    pub(crate) fn lower_expr_post_analysis(&mut self, typed_expr: &mut TypedExprStmt) {
        self.lower_unnamed_struct_value_as_struct_init(typed_expr);
        self.lower_unnamed_enum_value_as_enum_init(typed_expr);
        self.lower_unnamed_union_value_as_union_init(typed_expr);
        self.lower_enum_struct_variant_init_as_enum_init(typed_expr);
    }
}
