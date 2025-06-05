#[cfg(test)]
mod tests {
    use crate::Parser;
    use ast::ast::*;
    use ast::token::*;
    use lexer::Lexer;

    fn parse(test_name: String, input: &'static str) -> ProgramTree {
        let mut lexer = Lexer::new(input.to_string(), format!("cyrus_parser_{}_test.cyr", test_name));
        let mut parser = Parser::new(&mut lexer);

        match parser.parse() {
            Ok(program) => {
                return match program {
                    Node::ProgramTree(program) => program,
                    _ => panic!("Expected a program tree but got something else."),
                };
            }
            Err(parse_errors) => {
                panic!("{:#?}", parse_errors);
            }
        }
    }

    macro_rules! define_test {
        ($name:ident, $input:expr, $body:expr) => {
            #[test]
            fn $name() {
                let test_name = stringify!($name);
                let program = parse(test_name.to_string(), $input);
                $body(program)
            }
        };
    }

    define_test!(simple_arithmetic_expression, "1 + 2 * 3;", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Infix(binary_expression) = expression {
                assert_eq!(binary_expression.operator.kind, TokenKind::Plus);
                assert_eq!(
                    binary_expression.left,
                    Box::new(Expression::Literal(Literal::Integer(1)))
                );
                if let Expression::Infix(right_expression) = *binary_expression.right.clone() {
                    assert_eq!(right_expression.operator.kind, TokenKind::Asterisk);
                    assert_eq!(
                        right_expression.left,
                        Box::new(Expression::Literal(Literal::Integer(2)))
                    );
                    assert_eq!(
                        right_expression.right,
                        Box::new(Expression::Literal(Literal::Integer(3)))
                    );
                } else {
                    panic!("Expected an infix expression but got something else.");
                }
            } else {
                panic!("Expected an infix expression but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(unary_minus, "-5;", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Prefix(unary_expression) = expression {
                assert_eq!(unary_expression.operator.kind, TokenKind::Minus);
                assert_eq!(
                    unary_expression.operand,
                    Box::new(Expression::Literal(Literal::Integer(5)))
                );
            } else {
                panic!("Expected a prefix expression but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(unary_neg, "!0;", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Prefix(unary_expression) = expression {
                assert_eq!(unary_expression.operator.kind, TokenKind::Bang);
                assert_eq!(
                    unary_expression.operand,
                    Box::new(Expression::Literal(Literal::Integer(0)))
                );
            } else {
                panic!("Expected a prefix expression but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(func_call, "foo(1, 2);", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::FuncCall(func_call) = expression.clone() {
                assert_eq!(
                    func_call,
                    FuncCall {
                        operand: Box::new(Expression::ModuleImport(ModuleImport {
                            segments: vec![ModuleSegment::SubModule(Identifier {
                                name: "foo".to_string(),
                                span: Span::new(0, 2),
                                loc: Location::new(0, 5)
                            })],
                            span: Span::new(0, 3),
                            loc: Location::new(0, 5)
                        })),
                        arguments: vec![
                            Expression::Literal(Literal::Integer(1)),
                            Expression::Literal(Literal::Integer(2))
                        ],
                        span: Span::new(0, 8),
                        loc: Location::new(0, 11)
                    }
                );
            } else {
                panic!("Expected a function call but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(
        array_construction,
        "int[] { 1, 2, func_call(1, 2) };",
        |program: ProgramTree| {
            if let Statement::Expression(expression) = &program.body[0] {
                if let Expression::Array(array) = expression {
                    assert_eq!(array.elements.len(), 3);
                    assert_eq!(array.elements[0], Expression::Literal(Literal::Integer(1)));
                    assert_eq!(array.elements[1], Expression::Literal(Literal::Integer(2)));
                    assert_eq!(
                        array.elements[2],
                        Expression::FuncCall(FuncCall {
                            operand: Box::new(Expression::ModuleImport(ModuleImport {
                                segments: vec![ModuleSegment::SubModule(Identifier {
                                    name: "func_call".to_string(),
                                    span: Span::new(14, 22),
                                    loc: Location::new(0, 25)
                                })],
                                span: Span::new(14, 23),
                                loc: Location::new(0, 25)
                            })),
                            arguments: vec![
                                Expression::Literal(Literal::Integer(1)),
                                Expression::Literal(Literal::Integer(2))
                            ],
                            span: Span::new(14, 28),
                            loc: Location::new(0, 32)
                        })
                    );
                } else {
                    panic!("Expected an array literal but got something else.");
                }
            } else {
                panic!("Expected an expression but got something else.");
            }
        }
    );

    define_test!(variable_assignment, "x = 5;", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Assignment(assignment) = expression {
                assert_eq!(
                    assignment.assign_to,
                    Expression::ModuleImport(ModuleImport {
                        segments: vec![ModuleSegment::SubModule(Identifier {
                            name: "x".to_string(),
                            span: Span::new(0, 0),
                            loc: Location::new(0, 4)
                        })],
                        span: Span::new(0, 1),
                        loc: Location::new(0, 4)
                    })
                );
                assert_eq!(assignment.expr, Expression::Literal(Literal::Integer(5)));
            } else {
                panic!("Expected an assignment but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(
        struct_initialization,
        "MyStruct { field1: 10, field2: true };",
        |program: ProgramTree| {
            if let Statement::Expression(expression) = &program.body[0] {
                if let Expression::StructInit(struct_init) = expression {
                    assert_eq!(
                        struct_init.struct_name.segments,
                        vec![ModuleSegment::SubModule(Identifier {
                            name: "MyStruct".to_string(),
                            span: Span::new(0, 7),
                            loc: Location::new(0, 11)
                        })]
                    );
                    assert_eq!(struct_init.field_inits.len(), 2);
                    assert_eq!(struct_init.field_inits[0].name, "field1");
                    assert_eq!(
                        struct_init.field_inits[0].value,
                        Expression::Literal(Literal::Integer(10))
                    );
                    assert_eq!(struct_init.field_inits[1].name, "field2");
                    assert_eq!(
                        struct_init.field_inits[1].value,
                        Expression::Literal(Literal::Bool(true))
                    );
                } else {
                    panic!("Expected a struct initialization but got something else.");
                }
            } else {
                panic!("Expected an expression but got something else.");
            }
        }
    );

    define_test!(array_index, "arr[0];", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::ArrayIndex(array_index) = expression {
                assert_eq!(array_index.index, Box::new(Expression::Literal(Literal::Integer(0))));
            } else {
                panic!("Expected an array index but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(field_access, "object.field;", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::FieldAccess(field_access) = expression.clone() {
                assert_eq!(
                    field_access,
                    FieldAccess {
                        operand: Box::new(Expression::ModuleImport(ModuleImport {
                            segments: vec![ModuleSegment::SubModule(Identifier {
                                name: "object".to_string(),
                                span: Span::new(0, 5),
                                loc: Location::new(0, 8),
                            })],
                            span: Span::new(0, 6),
                            loc: Location::new(0, 8)
                        })),
                        field_name: Identifier {
                            name: "field".to_string(),
                            span: Span { start: 7, end: 12 },
                            loc: Location { line: 0, column: 14 },
                        },
                        span: Span::new(7, 12),
                        loc: Location::new(0, 14)
                    }
                );
            } else {
                panic!("Expected a field access or field access but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });
    define_test!(method_call, "object.method(1, 2);", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::MethodCall(method_call) = expression.clone() {
                assert_eq!(
                    method_call,
                    MethodCall {
                        operand: Box::new(Expression::ModuleImport(ModuleImport {
                            segments: vec![ModuleSegment::SubModule(Identifier {
                                name: "object".to_string(),
                                span: Span::new(0, 5),
                                loc: Location::new(0, 8),
                            })],
                            span: Span::new(0, 6),
                            loc: Location::new(0, 8)
                        })),
                        method_name: Identifier {
                            name: "method".to_string(),
                            span: Span { start: 7, end: 13 },
                            loc: Location { line: 0, column: 15 },
                        },
                        arguments: vec![
                            Expression::Literal(Literal::Integer(1)),
                            Expression::Literal(Literal::Integer(2)),
                        ],
                        span: Span::new(7, 18),
                        loc: Location::new(0, 15)
                    }
                );
            } else {
                panic!("Expected a field access or method call but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(cast_expression, "(float64) 10;", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Cast(cast_as) = expression {
                assert_eq!(*cast_as.expr, Expression::Literal(Literal::Integer(10)));
                assert_eq!(
                    cast_as.target_type,
                    TypeSpecifier::TypeToken(Token {
                        kind: TokenKind::Float64,
                        span: Span::new(1, 8)
                    })
                );
            } else {
                panic!("Expected a cast expression but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(pre_increment, "++my_var;", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::UnaryOperator(unary_operator) = expression {
                assert_eq!(
                    unary_operator.module_import.segments,
                    vec![ModuleSegment::SubModule(Identifier {
                        name: "my_var".to_string(),
                        span: Span::new(2, 7),
                        loc: Location::new(0, 10)
                    })]
                );
                assert_eq!(unary_operator.ty, UnaryOperatorType::PreIncrement);
            } else {
                panic!("Expected a unary operator expression.");
            }
        } else {
            panic!("Expected an expression.");
        }
    });

    define_test!(post_decrement, "my_var--;", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::UnaryOperator(unary_operator) = expression {
                assert_eq!(
                    unary_operator.module_import.segments,
                    vec![ModuleSegment::SubModule(Identifier {
                        name: "my_var".to_string(),
                        span: Span { start: 0, end: 5 },
                        loc: Location { line: 0, column: 9 }
                    })]
                );
                assert_eq!(unary_operator.ty, UnaryOperatorType::PostDecrement);
            } else {
                panic!("Expected a unary operator expression.");
            }
        } else {
            panic!("Expected an expression.");
        }
    });

    define_test!(
        complete_for_loop,
        "for (#i: int = 0; i < 10; i++) { }",
        |program: ProgramTree| {
            if let Statement::For(for_statement) = &program.body[0] {
                assert_eq!(
                    for_statement.initializer,
                    Some(Variable {
                        name: "i".to_string(),
                        ty: Some(TypeSpecifier::TypeToken(Token {
                            kind: TokenKind::Int,
                            span: Span::new(9, 12)
                        })),
                        expr: Some(Expression::Literal(Literal::Integer(0))),
                        span: Span::new(5, 16),
                        loc: Location::new(0, 20)
                    })
                );
                assert_eq!(
                    for_statement.increment,
                    Some(Expression::UnaryOperator(UnaryOperator {
                        module_import: ModuleImport {
                            segments: vec![ModuleSegment::SubModule(Identifier {
                                name: "i".to_string(),
                                span: Span::new(26, 26),
                                loc: Location::new(0, 30)
                            })],
                            span: Span::new(26, 27),
                            loc: Location::new(0, 30)
                        },
                        ty: UnaryOperatorType::PostIncrement,
                        span: Span::new(26, 28),
                        loc: Location::new(0, 31)
                    }))
                );
                assert_eq!(
                    for_statement.condition,
                    Some(Expression::Infix(BinaryExpression {
                        operator: Token {
                            kind: TokenKind::LessThan,
                            span: Span::new(20, 20)
                        },
                        left: Box::new(Expression::ModuleImport(ModuleImport {
                            segments: vec![ModuleSegment::SubModule(Identifier {
                                name: "i".to_string(),
                                span: Span::new(18, 18),
                                loc: Location::new(0, 22)
                            })],
                            span: Span::new(18, 19),
                            loc: Location::new(0, 22)
                        })),
                        right: Box::new(Expression::Literal(Literal::Integer(10))),
                        span: Span::new(18, 24),
                        loc: Location::new(0, 26)
                    }))
                );
            } else {
                panic!("Expected an expression.");
            }
        }
    );

    define_test!(
        only_initializer_for_loop,
        "for (#i: int = 0;) { }",
        |program: ProgramTree| {
            if let Statement::For(for_statement) = &program.body[0] {
                assert_eq!(
                    for_statement.initializer,
                    Some(Variable {
                        name: "i".to_string(),
                        ty: Some(TypeSpecifier::TypeToken(Token {
                            kind: TokenKind::Int,
                            span: Span::new(9, 12)
                        })),
                        expr: Some(Expression::Literal(Literal::Integer(0))),
                        span: Span::new(5, 16),
                        loc: Location::new(0, 19)
                    })
                );
                assert_eq!(for_statement.increment, None);
                assert_eq!(for_statement.condition, None);
            } else {
                panic!("Expected an expression.");
            }
        }
    );

    define_test!(infinite_for_loop, "for { }", |program: ProgramTree| {
        if let Statement::For(for_statement) = &program.body[0] {
            assert_eq!(for_statement.initializer, None);
            assert_eq!(for_statement.increment, None);
            assert_eq!(for_statement.condition, None);
        } else {
            panic!("Expected an expression.");
        }
    });

    define_test!(
        without_initializer_for_loop,
        "for (; i < 0; i++) { }",
        |program: ProgramTree| {
            if let Statement::For(for_statement) = &program.body[0] {
                assert_eq!(for_statement.initializer, None);
                assert_eq!(
                    for_statement.increment,
                    Some(Expression::UnaryOperator(UnaryOperator {
                        module_import: ModuleImport {
                            segments: vec![ModuleSegment::SubModule(Identifier {
                                name: "i".to_string(),
                                span: Span::new(14, 14),
                                loc: Location::new(0, 18)
                            })],
                            span: Span::new(14, 15),
                            loc: Location::new(0, 18)
                        },
                        ty: UnaryOperatorType::PostIncrement,
                        span: Span::new(14, 16),
                        loc: Location::new(0, 19)
                    }))
                );
                assert_eq!(
                    for_statement.condition,
                    Some(Expression::Infix(BinaryExpression {
                        operator: Token {
                            kind: TokenKind::LessThan,
                            span: Span::new(9, 9)
                        },
                        left: Box::new(Expression::ModuleImport(ModuleImport {
                            segments: vec![ModuleSegment::SubModule(Identifier {
                                name: "i".to_string(),
                                span: Span::new(7, 7),
                                loc: Location::new(0, 11)
                            })],
                            span: Span::new(7, 8),
                            loc: Location::new(0, 11)
                        })),
                        right: Box::new(Expression::Literal(Literal::Integer(0))),
                        span: Span::new(7, 12),
                        loc: Location::new(0, 14)
                    }))
                );
            } else {
                panic!("Expected an expression.");
            }
        }
    );
}
