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

use crate::{evaluator::ConstEvaluator, resolver::ConstResolver};
use cyrusc_typed_ast::{ScopeID, exprs::*};
use cyrusc_tokens::literals::LiteralKind;

pub struct ConstFolder<'a, R: ConstResolver> {
    evaluator: ConstEvaluator<'a, R>,
}

impl<'a, R: ConstResolver> ConstFolder<'a, R> {
    pub fn new(resolver: &'a mut R) -> Self {
        Self {
            evaluator: ConstEvaluator::new(resolver),
        }
    }

    pub fn fold_expr(&mut self, scope: Option<ScopeID>, expr: &mut TypedExprStmt) {
        match &mut expr.kind {
            TypedExprKind::Prefix(e) => {
                self.fold_expr(scope, &mut e.operand);
            }
            TypedExprKind::Infix(e) => {
                self.fold_expr(scope, &mut e.lhs);
                self.fold_expr(scope, &mut e.rhs);
            }
            TypedExprKind::ArrayIndex(e) => {
                self.fold_expr(scope, &mut e.operand);
                self.fold_expr(scope, &mut e.index);
            }
            TypedExprKind::Array(arr) => {
                for el in &mut arr.elements {
                    self.fold_expr(scope, el);
                }
            }
            _ => {}
        }

        if let Ok(const_value) = self.evaluator.eval_expr(scope, expr) {
            if let Some(int_value) = const_value.as_int() {
                let literal = TypedLiteralExpr {
                    ty: expr.sema_ty.clone(),
                    kind: LiteralKind::Integer(int_value, None),
                    loc: expr.loc,
                };

                expr.kind = TypedExprKind::Literal(literal);
            }
        }
    }

    pub fn expr_as_const_int(&mut self, scope: Option<ScopeID>, expr: &TypedExprStmt) -> Option<i128> {
        if let TypedExprKind::Literal(lit) = &expr.kind {
            if let LiteralKind::Integer(v, ..) = &lit.kind {
                return Some(*v);
            }
        }

        // try const evaluation
        self.evaluator.eval_expr(scope, expr).ok()?.as_int()
    }
}
