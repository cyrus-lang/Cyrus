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
use crate::analyze::AnalysisContext;
use cyrusc_ast::{
    AssignKind,
    operators::{InfixOperator, PrefixOperator},
};
use cyrusc_typed_ast::{
    ScopeID,
    exprs::{
        MemoryLocation, TypedAssignExpr, TypedExprKind, TypedExprStmt, TypedInfixExpr, TypedLiteralExpr,
        TypedPrefixExpr,
    },
    types::{PlainType, SemanticType},
};
use cyrusc_tokens::literals::LiteralKind;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn deduce_special_exprs(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &mut TypedExprStmt,
        expected_type: Option<SemanticType>,
    ) {
        match &mut typed_expr.kind {
            TypedExprKind::Assign(typed_assignment) => {
                if typed_assignment.kind != AssignKind::Default {
                    typed_expr.kind = self.lower_assign_to_infix_expr(typed_assignment);
                }
            }
            TypedExprKind::Prefix(prefix_expr) => {
                match prefix_expr.op {
                    PrefixOperator::Bang => {
                        if let Some(lowered_typed_expr) =
                            self.deduce_prefix_not_pointer(scope_id_opt, expected_type.clone(), prefix_expr)
                        {
                            *typed_expr = lowered_typed_expr;
                        }
                    }
                    _ => {}
                };
            }
            _ => {}
        };
    }

    fn deduce_prefix_not_pointer(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        expected_type: Option<SemanticType>,
        prefix_expr: &mut TypedPrefixExpr,
    ) -> Option<TypedExprStmt> {
        let operand_type = match self.analyze_expr(scope_id_opt, &mut prefix_expr.operand, expected_type.clone()) {
            Some(sema_ty) => sema_ty,
            None => return None,
        };

        let null_literal_expr = TypedExprStmt {
            kind: TypedExprKind::Literal(TypedLiteralExpr {
                ty: Some(SemanticType::Pointer(Box::new(SemanticType::PlainType(
                    PlainType::Void,
                )))),
                kind: LiteralKind::Null,
                loc: prefix_expr.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
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
                sema_ty: None,
                loc: prefix_expr.loc,
            })
        } else {
            None
        }
    }

    pub(crate) fn lower_assign_to_infix_expr(&self, assign: &mut TypedAssignExpr) -> TypedExprKind {
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
                sema_ty: None,
                loc: assign.loc,
                mloc: MemoryLocation::RValue,
            }),
            kind: AssignKind::Default,
            loc: assign.loc,
        })
    }
}
