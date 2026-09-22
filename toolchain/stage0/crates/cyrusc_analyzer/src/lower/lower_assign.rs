// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_ast::AssignKind;
use cyrusc_typed_ast::exprs::{TypedAssignExpr, TypedExpr, TypedExprKind, TypedInfixExpr, ValueCategory};

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
        rhs: Box::new(TypedExpr {
            kind: infix_expr,
            ty: None,
            loc: assign.loc,
            val_cat: ValueCategory::RValue,
            analyzed: false,
        }),
        kind: AssignKind::Default,
        loc: assign.loc,
    })
}
