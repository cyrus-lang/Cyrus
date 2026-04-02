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

use cyrusc_ast::operators::InfixOperator;
use cyrusc_tokens::literals::LiteralKind;
use cyrusc_typed_ast::{
    exprs::{MemoryLocation, TypedExprKind, TypedExprStmt, TypedInfixExpr, TypedLiteralExpr, TypedPrefixExpr},
    types::{PlainType, SemanticType},
};
use crate::context::AnalysisContext;

/// Lowers a logical negation on a pointer operand into a null comparison.
impl<'a> AnalysisContext<'a> {
    pub(crate) fn lower_prefix_not_pointer(
        &mut self,
        expected_type: Option<SemanticType>,
        prefix_expr: &mut TypedPrefixExpr,
    ) -> Option<TypedExprStmt> {
        let operand_type = match self.analyze_expr(&mut prefix_expr.operand, expected_type.clone()) {
            Some(sema_type) => sema_type,
            None => return None,
        };

        let null_literal_expr = TypedExprStmt {
            kind: TypedExprKind::Literal(TypedLiteralExpr {
                ty: Some(SemanticType::Pointer(Box::new(SemanticType::Plain(PlainType::Void)))),
                kind: LiteralKind::Null,
                loc: prefix_expr.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_type: None,
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

            Some(TypedExprStmt {
                kind: new_infix_expr,
                mloc: MemoryLocation::RValue,
                sema_type: None,
                loc: prefix_expr.loc,
            })
        } else {
            None
        }
    }
}
