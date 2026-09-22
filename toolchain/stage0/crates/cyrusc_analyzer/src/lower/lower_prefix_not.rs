// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::context::AnalysisContext;
use cyrusc_ast::operators::InfixOperator;
use cyrusc_tokens::literals::LiteralKind;
use cyrusc_typed_ast::{
    exprs::{TypedExpr, TypedExprKind, TypedInfixExpr, TypedLiteralExpr, TypedPrefixExpr, ValueCategory},
    types::{PlainType, SemaType},
};

/// Lowers a logical negation on a pointer operand into a null comparison.
impl<'a> AnalysisContext<'a> {
    pub(crate) fn lower_cond_pointer_as_null_check(&mut self, expr: &mut TypedExpr) -> Option<TypedExpr> {
        let null_literal_expr = TypedExpr {
            kind: TypedExprKind::Literal(TypedLiteralExpr {
                ty: Some(SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Void)))),
                kind: LiteralKind::Null,
                loc: expr.loc,
            }),
            ty: expr.ty.clone(),
            val_cat: ValueCategory::RValue,
            analyzed: false,
            loc: expr.loc,
        };

        if expr.ty.as_ref()?.is_pointer() {
            let lhs = expr.clone();

            let new_infix_expr = TypedExprKind::Infix(TypedInfixExpr {
                op: InfixOperator::NotEqual,
                lhs: Box::new(lhs),
                rhs: Box::new(null_literal_expr),
                loc: expr.loc,
            });

            Some(TypedExpr {
                kind: new_infix_expr,
                ty: Some(SemaType::Plain(PlainType::Bool)),
                val_cat: ValueCategory::RValue,
                analyzed: false,
                loc: expr.loc,
            })
        } else {
            None
        }
    }

    pub(crate) fn lower_prefix_not_pointer(
        &mut self,
        expected_type: Option<SemaType>,
        prefix_expr: &mut TypedPrefixExpr,
    ) -> Option<TypedExpr> {
        let operand_type = match self.analyze_expr(&mut prefix_expr.operand, expected_type.clone()) {
            Some(sema_type) => sema_type,
            None => return None,
        };

        let null_literal_expr = TypedExpr {
            kind: TypedExprKind::Literal(TypedLiteralExpr {
                ty: Some(SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Void)))),
                kind: LiteralKind::Null,
                loc: prefix_expr.loc,
            }),
            ty: None,
            val_cat: ValueCategory::RValue,
            analyzed: false,
            loc: prefix_expr.loc,
        };

        if operand_type.is_pointer() {
            let lhs = prefix_expr.operand.clone();

            let new_infix_expr = TypedExprKind::Infix(TypedInfixExpr {
                op: InfixOperator::Equal,
                lhs,
                rhs: Box::new(null_literal_expr),
                loc: prefix_expr.loc,
            });

            Some(TypedExpr {
                kind: new_infix_expr,
                ty: None,
                val_cat: ValueCategory::RValue,
                analyzed: false,
                loc: prefix_expr.loc,
            })
        } else {
            None
        }
    }
}
