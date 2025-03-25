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

    define_test!(simple_arithmetic_expression, "1 + 2 * 3", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Infix(binary_expression) = expression {
                assert_eq!(binary_expression.operator.kind, TokenKind::Plus);
                assert_eq!(
                    binary_expression.left,
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(1))))
                );
                if let Expression::Infix(right_expression) = *binary_expression.right.clone() {
                    assert_eq!(right_expression.operator.kind, TokenKind::Asterisk);
                    assert_eq!(
                        right_expression.left,
                        Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(2))))
                    );
                    assert_eq!(
                        right_expression.right,
                        Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(3))))
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

    define_test!(unary_minus, "-5", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Prefix(unary_expression) = expression {
                assert_eq!(unary_expression.operator.kind, TokenKind::Minus);
                assert_eq!(
                    unary_expression.operand,
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(5))))
                );
            } else {
                panic!("Expected a prefix expression but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(unary_neg, "!0", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Prefix(unary_expression) = expression {
                assert_eq!(unary_expression.operator.kind, TokenKind::Bang);
                assert_eq!(
                    unary_expression.operand,
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(0))))
                );
            } else {
                panic!("Expected a prefix expression but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(function_call, "foo(1, 2)", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::FieldAccessOrMethodCall(field_accesses) = expression {
                if let Some(FuncCall {
                    func_name, arguments, ..
                }) = &field_accesses[0].method_call
                {
                    assert_eq!(func_name.identifier.name, "foo");
                    assert_eq!(arguments.len(), 2);
                    assert_eq!(
                        arguments[0],
                        Expression::Literal(Literal::Integer(IntegerLiteral::I32(1)))
                    );
                    assert_eq!(
                        arguments[1],
                        Expression::Literal(Literal::Integer(IntegerLiteral::I32(2)))
                    );
                } else {
                    panic!("Expected function call.");
                }
            } else {
                panic!("Expected a function call but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(array_literal, "[1, 2, 3, func_call()]", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Array(array) = expression {
                assert_eq!(array.elements.len(), 4);
                assert_eq!(
                    array.elements[0],
                    Expression::Literal(Literal::Integer(IntegerLiteral::I32(1)))
                );
                assert_eq!(
                    array.elements[1],
                    Expression::Literal(Literal::Integer(IntegerLiteral::I32(2)))
                );
                assert_eq!(
                    array.elements[2],
                    Expression::Literal(Literal::Integer(IntegerLiteral::I32(3)))
                );
                assert_eq!(
                    array.elements[3],
                    Expression::FieldAccessOrMethodCall(vec![FieldAccessOrMethodCall {
                        method_call: Some(FuncCall {
                            func_name: ModuleImport {
                                sub_modules: vec![],
                                identifier: Identifier {
                                    name: "func_call".to_string(),
                                    span: Span::new(10, 18),
                                    loc: Location::new(0, 21)
                                },
                                span: Span::new(10, 19),
                                loc: Location::new(0, 21)
                            },
                            arguments: vec![],
                            span: Span::new(10, 20),
                            loc: Location::new(0, 23)
                        }),
                        field_access: None
                    }])
                );
            } else {
                panic!("Expected an array literal but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(variable_assignment, "x = 5;", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Assignment(assignment) = expression {
                assert_eq!(assignment.identifier.identifier.name, "x");
                assert_eq!(
                    assignment.expr,
                    Expression::Literal(Literal::Integer(IntegerLiteral::I32(5)))
                );
            } else {
                panic!("Expected an assignment but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    // define_test!(
    //     struct_initialization,
    //     "MyStruct { field1: 10, field2: true };",
    //     |program: ProgramTree| {
    //         if let Statement::Expression(expression) = &program.body[0] {
    //             if let Expression::StructInit(struct_init) = expression {
    //                 assert_eq!(struct_init.struct_name.identifier.name, "MyStruct");
    //                 assert_eq!(struct_init.field_inits.len(), 2);
    //                 assert_eq!(struct_init.field_inits[0].name, "field1");
    //                 assert_eq!(
    //                     struct_init.field_inits[0].value,
    //                     Expression::Literal(Literal::Integer(IntegerLiteral::I32(10)))
    //                 );
    //                 assert_eq!(struct_init.field_inits[1].name, "field2");
    //                 assert_eq!(
    //                     struct_init.field_inits[1].value,
    //                     Expression::Literal(Literal::Bool(BoolLiteral {
    //                         raw: true,
    //                         span: Span::new(31, 35)
    //                     }))
    //                 );
    //             } else {
    //                 panic!("Expected a struct initialization but got something else.");
    //             }
    //         } else {
    //             panic!("Expected an expression but got something else.");
    //         }
    //     }
    // );

    define_test!(array_index, "arr[0][1]", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::ArrayIndex(array_index) = expression {
                assert_eq!(array_index.dimensions.len(), 2);
                assert_eq!(
                    array_index.dimensions[0],
                    Expression::Array(Array {
                        elements: vec![Expression::Literal(Literal::Integer(IntegerLiteral::I32(0)))],
                        span: Span::new(3, 5),
                        loc: Location { line: 0, column: 8 }
                    })
                );
                assert_eq!(
                    array_index.dimensions[1],
                    Expression::Array(Array {
                        elements: vec![Expression::Literal(Literal::Integer(IntegerLiteral::I32(1)))],
                        span: Span::new(6, 8),
                        loc: Location { line: 0, column: 10 }
                    })
                );
            } else {
                panic!("Expected an array index but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(module_import, "object.field;", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::ModuleImport(module_import) = expression {
                assert_eq!(
                    module_import.sub_modules[0],
                    ModulePath::SubModule(Identifier {
                        name: "object".to_string(),
                        span: Span::new(0, 5),
                        loc: Location::new(0, 8),
                    })
                );
                assert_eq!(module_import.identifier.name, "field");
            } else {
                panic!("Expected a module import but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(
        field_access_or_method_call,
        "object.method(1, 2)",
        |program: ProgramTree| {
            if let Statement::Expression(expression) = &program.body[0] {
                if let Expression::FieldAccessOrMethodCall(field_access_or_method_call) = expression {
                    let method_call = field_access_or_method_call[0].method_call.clone().unwrap();
                    assert_eq!(
                        method_call.arguments[0],
                        Expression::Literal(Literal::Integer(IntegerLiteral::I32(1)))
                    );
                    assert_eq!(
                        method_call.arguments[1],
                        Expression::Literal(Literal::Integer(IntegerLiteral::I32(2)))
                    );
                    assert_eq!(
                        method_call.func_name.identifier,
                        Identifier {
                            name: "method".to_string(),
                            span: Span::new(7, 12),
                            loc: Location { line: 0, column: 14 }
                        }
                    );
                    assert_eq!(
                        method_call.func_name.sub_modules,
                        vec![ModulePath::SubModule(Identifier {
                            name: "object".to_string(),
                            span: Span::new(0, 5),
                            loc: Location { line: 0, column: 8 }
                        })]
                    )
                } else {
                    panic!("Expected a field access or method call but got something else.");
                }
            } else {
                panic!("Expected an expression but got something else.");
            }
        }
    );

    define_test!(cast_as, "10 as float", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::CastAs(cast_as) = expression {
                assert_eq!(
                    *cast_as.expr,
                    Expression::Literal(Literal::Integer(IntegerLiteral::I32(10)))
                );
                assert_eq!(cast_as.type_token, TokenKind::Float);
            } else {
                panic!("Expected a cast as but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    define_test!(pre_increment, "++my_var", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::UnaryOperator(unary_operator) = expression {
                assert_eq!(
                    unary_operator.module_import.identifier,
                    Identifier {
                        name: "my_var".to_string(),
                        span: Span::new(2, 7),
                        loc: Location::new(0, 9)
                    }
                );
                assert_eq!(unary_operator.ty, UnaryOperatorType::PreIncrement);
            } else {
                panic!("Expected a unary operator expression.");
            }
        } else {
            panic!("Expected an expression.");
        }
    });

    define_test!(post_decrement, "my_var--", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::UnaryOperator(unary_operator) = expression {
                assert_eq!(
                    unary_operator.module_import.identifier,
                    Identifier {
                        name: "my_var".to_string(),
                        span: Span { start: 0, end: 5 },
                        loc: Location { line: 0, column: 9 }
                    }
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
        "for #i: i32 = 0; i < 10; i++ { }",
        |program: ProgramTree| {
            if let Statement::For(for_statement) = &program.body[0] {
                assert_eq!(
                    for_statement.initializer,
                    Some(Variable {
                        name: "i".to_string(),
                        ty: Some(TokenKind::I32),
                        expr: Some(Expression::Literal(Literal::Integer(IntegerLiteral::I32(0)))),
                        span: Span::new(4, 15),
                        loc: Location::new(0, 19)
                    })
                );
                assert_eq!(
                    for_statement.increment,
                    Some(Expression::UnaryOperator(UnaryOperator {
                        module_import: ModuleImport {
                            sub_modules: vec![],
                            identifier: Identifier {
                                name: "i".to_string(),
                                span: Span::new(25, 25),
                                loc: Location::new(0, 29)
                            },
                            span: Span::new(25, 26),
                            loc: Location::new(0, 29)
                        },
                        ty: UnaryOperatorType::PostIncrement,
                        span: Span::new(25, 26),
                        loc: Location::new(0, 31)
                    }))
                );
                assert_eq!(
                    for_statement.condition,
                    Some(Expression::Infix(BinaryExpression {
                        operator: Token {
                            kind: TokenKind::LessThan,
                            span: Span::new(19, 19)
                        },
                        left: Box::new(Expression::ModuleImport(ModuleImport {
                            sub_modules: vec![],
                            identifier: Identifier {
                                name: "i".to_string(),
                                span: Span::new(17, 17),
                                loc: Location::new(0, 21)
                            },
                            span: Span::new(17, 18),
                            loc: Location::new(0, 21)
                        })),
                        right: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(10)))),
                        span: Span::new(17, 23),
                        loc: Location::new(0, 27)
                    }))
                );
            } else {
                panic!("Expected an expression.");
            }
        }
    );

    define_test!(
        only_initializer_for_loop,
        "for #i: i32 = 0; { }",
        |program: ProgramTree| {
            if let Statement::For(for_statement) = &program.body[0] {
                assert_eq!(
                    for_statement.initializer,
                    Some(Variable {
                        name: "i".to_string(),
                        ty: Some(TokenKind::I32),
                        expr: Some(Expression::Literal(Literal::Integer(IntegerLiteral::I32(0)))),
                        span: Span::new(4, 15),
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
        "for ; i < 0; i++ { }",
        |program: ProgramTree| {
            if let Statement::For(for_statement) = &program.body[0] {
                assert_eq!(for_statement.initializer, None);
                assert_eq!(
                    for_statement.increment,
                    Some(Expression::UnaryOperator(UnaryOperator {
                        module_import: ModuleImport {
                            sub_modules: vec![],
                            identifier: Identifier {
                                name: "i".to_string(),
                                span: Span::new(13, 13),
                                loc: Location::new(0, 17)
                            },
                            span: Span::new(13, 14),
                            loc: Location::new(0, 17)
                        },
                        ty: UnaryOperatorType::PostIncrement,
                        span: Span::new(13, 14),
                        loc: Location::new(0, 19)
                    }))
                );
                assert_eq!(
                    for_statement.condition,
                    Some(Expression::Infix(BinaryExpression {
                        operator: Token {
                            kind: TokenKind::LessThan,
                            span: Span::new(8, 8)
                        },
                        left: Box::new(Expression::ModuleImport(ModuleImport {
                            sub_modules: vec![],
                            identifier: Identifier {
                                name: "i".to_string(),
                                span: Span::new(6, 6),
                                loc: Location::new(0, 10)
                            },
                            span: Span::new(6, 7),
                            loc: Location::new(0, 10)
                        })),
                        right: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(0)))),
                        span: Span::new(6, 11),
                        loc: Location::new(0, 15)
                    }))
                );
            } else {
                panic!("Expected an expression.");
            }
        }
    );

    define_test!(control_flow_1, "if a == 1 {}", |program: ProgramTree| {
        if let Statement::If(if_statement) = &program.body[0] {
            assert_eq!(
                if_statement.condition,
                Expression::Infix(BinaryExpression {
                    operator: Token {
                        kind: TokenKind::Equal,
                        span: Span::new(5, 6)
                    },
                    left: Box::new(Expression::ModuleImport(ModuleImport {
                        sub_modules: vec![],
                        identifier: Identifier {
                            name: "a".to_string(),
                            span: Span::new(3, 3),
                            loc: Location::new(0, 8)
                        },
                        span: Span::new(3, 4),
                        loc: Location::new(0, 8)
                    })),
                    right: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(1)))),
                    span: Span::new(3, 9),
                    loc: Location::new(0, 13)
                })
            );
            assert_eq!(if_statement.alternate, None);
            assert_eq!(if_statement.branches, vec![]);
        } else {
            panic!("Expected an expression.");
        }
    });

    // define_test!(control_flow_2, "if some_value {}", |program: ProgramTree| {
    //     if let Statement::If(if_statement) = &program.body[0] {
    //         assert_eq!(
    //             if_statement.condition,
    //             Expression::Identifier(Identifier {
    //                 name: "some_value".to_string(),
    //                 span: Span::new(3, 13),
    //                 loc: Location::new(0, 16)
    //             })
    //         );
    //         assert_eq!(if_statement.alternate, None);
    //         assert_eq!(if_statement.branches, vec![]);
    //     } else {
    //         panic!("Expected an expression.");
    //     }
    // });
}
