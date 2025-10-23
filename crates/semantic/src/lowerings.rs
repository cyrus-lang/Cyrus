use crate::context::AnalysisContext;
use ast::{
    AssignmentKind, LiteralKind,
    operators::{InfixOperator, PrefixOperator},
};
use typed_ast::{
    ScopeID, TypedAssignment, TypedExpression, TypedExpressionKind, TypedInfixExpression, TypedLiteral,
    TypedPrefixExpression, ValueCategory,
    types::{BasicSemanticType, SemanticType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn apply_possible_expr_lowerings(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &mut TypedExpression,
        expected_type: Option<SemanticType>,
    ) {
        match &mut typed_expr.kind {
            TypedExpressionKind::Assignment(typed_assignment) => {
                if typed_assignment.kind != AssignmentKind::Default {
                    typed_expr.kind = self.lower_assign_to_infix_expr(typed_assignment);
                }
            }
            TypedExpressionKind::Prefix(prefix_expr) => {
                match prefix_expr.op {
                    PrefixOperator::Bang => {
                        if let Some(lowered_typed_expr) = self.lower_prefix_bang_with_pointer_operand(
                            scope_id_opt,
                            expected_type.clone(),
                            prefix_expr,
                        ) {
                            *typed_expr = lowered_typed_expr;
                        }
                    }
                    _ => {}
                };
            }
            _ => {}
        };
    }

    fn lower_prefix_bang_with_pointer_operand(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        expected_type: Option<SemanticType>,
        prefix_expr: &mut TypedPrefixExpression,
    ) -> Option<TypedExpression> {
        let operand_type =
            match self.analyze_typed_expr_type(scope_id_opt, &mut prefix_expr.operand, expected_type.clone()) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        let null_literal_expr = TypedExpression {
            kind: TypedExpressionKind::Literal(TypedLiteral {
                ty: Some(SemanticType::Pointer(Box::new(SemanticType::BasicType(
                    BasicSemanticType::Void,
                )))),
                kind: LiteralKind::Null,
                loc: prefix_expr.loc.clone(),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: prefix_expr.loc.clone(),
        };

        if operand_type.is_pointer() {
            let lhs = prefix_expr.operand.clone();

            let new_infix_expr = TypedExpressionKind::Infix(TypedInfixExpression {
                op: InfixOperator::Equal,
                lhs,
                rhs: Box::new(null_literal_expr),
                loc: prefix_expr.loc.clone(),
            });

            Some(TypedExpression {
                kind: new_infix_expr,
                value_category: ValueCategory::Rvalue,
                concrete_type: None,
                loc: prefix_expr.loc.clone(),
            })
        } else {
            None
        }
    }

    pub(crate) fn lower_assign_to_infix_expr(&self, assign: &mut TypedAssignment) -> TypedExpressionKind {
        let infix_expr = TypedExpressionKind::Infix(TypedInfixExpression {
            op: assign.kind.to_infix_operator(),
            lhs: assign.lhs.clone(),
            rhs: assign.rhs.clone(),
            loc: assign.loc.clone(),
        });

        TypedExpressionKind::Assignment(TypedAssignment {
            lhs: assign.lhs.clone(),
            rhs: Box::new(TypedExpression {
                kind: infix_expr,
                concrete_type: None,
                loc: assign.loc.clone(),
                value_category: ValueCategory::Rvalue,
            }),
            kind: AssignmentKind::Default,
            loc: assign.loc.clone(),
        })
    }
}
