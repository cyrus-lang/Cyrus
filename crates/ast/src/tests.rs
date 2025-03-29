#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::token::{Location, Span, Token, TokenKind};

    #[test]
    fn test_token_identifier() {
        let identifier = TokenKind::Identifier {
            name: String::from("sample"),
        };
        assert_eq!(identifier.to_string(), "sample");
    }

    #[test]
    fn test_token_arrays() {
        let arrays = TokenKind::Array(
            Box::new(TokenKind::I32),
            vec![
                TokenKind::Literal(Literal::Integer(IntegerLiteral::I32(3))),
                TokenKind::Literal(Literal::Integer(IntegerLiteral::I32(4))),
                TokenKind::Literal(Literal::Integer(IntegerLiteral::I32(5))),
            ],
        );
        assert_eq!(arrays.to_string(), "i32[3][4][5]");
    }

    #[test]
    fn test_binary_expression() {
        let loc = Location { line: 7, column: 3 };
        let bin_expr = BinaryExpression {
            operator: Token {
                kind: TokenKind::Plus,
                span: Span::default(),
            },
            left: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(5)))),
            right: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(3)))),
            span: Span::default(),
            loc,
        };
        assert_eq!(bin_expr.operator.kind, TokenKind::Plus);
    }

    #[test]
    fn test_unary_expression() {
        let loc = Location { line: 8, column: 4 };
        let unary_expr = UnaryExpression {
            operator: Token {
                kind: TokenKind::Percent,
                span: Span::default(),
            },
            operand: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(7)))),
            span: Span::default(),
            loc,
        };
        assert_eq!(unary_expr.operator.kind, TokenKind::Percent);
    }

    #[test]
    fn test_for_loop() {
        let initializer = Some(Variable {
            name: "i".to_string(),
            ty: Some(TokenKind::I32),
            expr: Some(Expression::Literal(Literal::Integer(IntegerLiteral::I32(0)))),
            span: Span::default(),
            loc: Location::default(),
        });
        let condition = Some(Expression::Infix(BinaryExpression {
            operator: Token {
                kind: TokenKind::LessThan,
                span: Span::default(),
            },
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                span: Span::default(),
                loc: Location::default(),
            })),
            right: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(10)))),
            span: Span::default(),
            loc: Location::default(),
        }));
        let increment = Some(Expression::UnaryOperator(UnaryOperator {
            module_import: ModuleImport {
                identifier: Identifier {
                    name: "i".to_string(),
                    span: Span::default(),
                    loc: Location::default(),
                },
                sub_modules: vec![],
                span: Span::default(),
                loc: Location::default(),
            },
            ty: UnaryOperatorType::PostIncrement,
            span: Span::default(),
            loc: Location::default(),
        }));
        let body = Box::new(BlockStatement {
            exprs: vec![],
            span: Span::default(),
            loc: Location::default(),
        });
        let for_loop = For {
            initializer,
            condition,
            increment,
            body,
            span: Span::default(),
            loc: Location::default(),
        };

        assert!(for_loop.condition.is_some());
    }

    #[test]
    fn test_variable() {
        let variable = Variable {
            name: "x".to_string(),
            ty: Some(TokenKind::I32),
            expr: Some(Expression::Literal(Literal::Integer(IntegerLiteral::I32(42)))),
            span: Span::default(),
            loc: Location::default(),
        };

        assert_eq!(variable.name, "x");
    }

    #[test]
    fn test_assignment() {
        let assignment = Assignment {
            module_import: ModuleImport {
                sub_modules: vec![],
                identifier: Identifier {
                    name: "x".to_string(),
                    span: Span::default(),
                    loc: Location::default(),
                },
                span: Span::default(),
                loc: Location::default(),
            },
            expr: Expression::Literal(Literal::Integer(IntegerLiteral::I32(10))),
            span: Span::default(),
            loc: Location::default(),
        };

        assert_eq!(assignment.module_import.identifier.name, "x");
    }

    #[test]
    fn test_func_def() {
        let param = FuncParam {
            identifier: Identifier {
                name: "a".to_string(),
                span: Span::default(),
                loc: Location::default(),
            },
            ty: Some(TokenKind::I32),
            default_value: None,
            span: Span::default(),
            loc: Location::default(),
        };

        let func = FuncDef {
            name: "add".to_string(),
            params: FuncParams {
                list: vec![param.clone()],
                variadic: None,
            },
            body: Box::new(BlockStatement {
                exprs: vec![],
                span: Span::default(),
                loc: Location::default(),
            }),
            return_type: Some(Token {
                kind: TokenKind::I32,
                span: Span::default(),
            }),
            vis_type: VisType::Pub,
            span: Span::default(),
            loc: Location::default(),
        };

        assert_eq!(func.name, "add");
        assert_eq!(func.params.list[0].identifier.name, "a");
    }

    #[test]
    fn test_if_statement() {
        let if_statement = If {
            condition: Expression::Literal(Literal::Bool(BoolLiteral {
                raw: true,
                span: Span::default(),
            })),
            consequent: Box::new(BlockStatement {
                exprs: vec![],
                span: Span::default(),
                loc: Location::default(),
            }),
            branches: vec![],
            alternate: None,
            span: Span::default(),
            loc: Location::default(),
        };

        assert!(matches!(if_statement.condition, Expression::Literal(Literal::Bool(_))));
    }

    #[test]
    fn test_cast_as() {
        let cast_as_expression = CastAs {
            expr: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(10)))),
            type_token: TokenKind::F64,
            span: Span::default(),
            loc: Location::default(),
        };

        assert_eq!(cast_as_expression.to_string(), "10 as f64");
    }

    #[test]
    fn test_field_access_or_method_call() {
        let chains: Vec<FieldAccessOrMethodCall> = vec![
            FieldAccessOrMethodCall {
                method_call: Some(FuncCall {
                    func_name: ModuleImport {
                        sub_modules: vec![ModulePath::SubModule(Identifier {
                            name: String::from("my_module"),
                            span: Span::default(),
                            loc: Location::default(),
                        })],
                        identifier: Identifier {
                            name: String::from("sample"),
                            span: Span::default(),
                            loc: Location::default(),
                        },
                        span: Span::default(),
                        loc: Location::default(),
                    },
                    arguments: vec![],
                    span: Span::default(),
                    loc: Location::default(),
                }),
                field_access: None,
            },
            FieldAccessOrMethodCall {
                method_call: Some(FuncCall {
                    func_name: ModuleImport {
                        sub_modules: vec![],
                        identifier: Identifier {
                            name: String::from("nested_method"),
                            span: Span::default(),
                            loc: Location::default(),
                        },
                        span: Span::default(),
                        loc: Location::default(),
                    },
                    arguments: vec![],
                    span: Span::default(),
                    loc: Location::default(),
                }),
                field_access: None,
            },
            FieldAccessOrMethodCall {
                field_access: Some(FieldAccess {
                    identifier: Identifier {
                        name: String::from("some_field"),
                        span: Span::default(),
                        loc: Location::default(),
                    },
                    span: Span::default(),
                    loc: Location::default(),
                }),
                method_call: None,
            },
        ];

        assert_eq!(
            Expression::FieldAccessOrMethodCall(chains).to_string(),
            "my_module.sample().nested_method().some_field"
        );
    }
}
