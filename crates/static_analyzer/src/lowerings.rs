use ast::{
    AssignmentKind, LiteralKind, operators::{InfixOperator, PrefixOperator}
};
use typed_ast::{
    ScopeID, TypedExpression, TypedExpressionKind, TypedInfixExpression, TypedLiteral, TypedPrefixExpression,
    ValueCategory,
    types::{BasicConcreteType, ConcreteType},
};

use crate::context::AnalysisContext;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn apply_possible_expr_lowerings(&mut self, scope_id_opt: Option<ScopeID>, typed_expr: &mut TypedExpression, expected_type: Option<ConcreteType>) {
        match &mut typed_expr.kind {
            TypedExpressionKind::Assignment(typed_assignment) => {
                if typed_assignment.kind != AssignmentKind::Default {
                    typed_expr.kind = self.lower_assign_to_infix_expr(typed_assignment);
                }
            }
            TypedExpressionKind::Prefix(prefix_expr) => match &prefix_expr.op {
                PrefixOperator::Bang => {
                    if let Some(lowered_typed_expr) =
                        self.lower_prefix_bang_with_pointer_operand(scope_id_opt, expected_type.clone(), prefix_expr)
                    {
                        *typed_expr = lowered_typed_expr;
                    };
                }
                _ => {}
            },
            _ => {}
        }
    }

    fn lower_prefix_bang_with_pointer_operand(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        expected_type: Option<ConcreteType>,
        prefix_expr: &mut TypedPrefixExpression,
    ) -> Option<TypedExpression> {
        let operand_type =
            match self.analyze_typed_expr_type(scope_id_opt, &mut prefix_expr.operand, expected_type.clone()) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        let null_literal_expr = TypedExpression {
            kind: TypedExpressionKind::Literal(TypedLiteral {
                ty: Some(ConcreteType::Pointer(Box::new(ConcreteType::BasicType(
                    BasicConcreteType::Void,
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
}
