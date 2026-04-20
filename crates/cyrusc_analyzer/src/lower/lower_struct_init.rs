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

use crate::context::AnalysisContext;
use cyrusc_typed_ast::exprs::{TypedExpr, TypedExprKind, TypedFieldInit, TypedStructInitExpr};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn lower_unnamed_struct_value_as_struct_init(&self, typed_expr: &mut TypedExpr) {
        let TypedExprKind::UnnamedStructValue(struct_value) = &typed_expr.kind else {
            return;
        };

        let Some(operand) = &typed_expr.sema_type else {
            return;
        };

        let fields = struct_value
            .fields
            .iter()
            .map(|field| TypedFieldInit {
                name: field.name.clone(),
                value: *field.value.clone(),
                loc: field.loc,
            })
            .collect();

        let struct_init = TypedStructInitExpr {
            operand: operand.clone(),
            fields,
            loc: struct_value.loc,
        };

        *typed_expr = TypedExpr {
            kind: TypedExprKind::StructInit(struct_init),
            sema_type: typed_expr.sema_type.clone(),
            val_cat: typed_expr.val_cat,
            loc: typed_expr.loc,
        };
    }
}
