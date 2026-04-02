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

use cyrusc_ast::AssignKind;
use cyrusc_typed_ast::exprs::{MemoryLocation, TypedAssignExpr, TypedExprKind, TypedExprStmt, TypedInfixExpr};

/// Lowers a compound assignment into an equivalent infix expression assignment.
pub(crate) fn lower_assign_to_infix_expr(assign: &mut TypedAssignExpr) -> TypedExprKind {
    let infix_expr = TypedExprKind::Infix(TypedInfixExpr {
        op: assign.kind.to_infix_operator(),
        lhs: assign.lhs.clone(),
        rhs: assign.rhs.clone(),
        loc: assign.loc,
    });

    TypedExprKind::Assign(TypedAssignExpr {
        lhs: assign.lhs.clone(),
        rhs: Box::new(TypedExprStmt {
            kind: infix_expr,
            sema_type: None,
            loc: assign.loc,
            mloc: MemoryLocation::RValue,
        }),
        kind: AssignKind::Default,
        loc: assign.loc,
    })
}
