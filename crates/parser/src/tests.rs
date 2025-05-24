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

    define_test!(unary_minus, "-5", |program: ProgramTree| {
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

    define_test!(unary_neg, "!0", |program: ProgramTree| {
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

    // define_test!(func_call, "foo(1, 2);", |program: ProgramTree| {
    //     if let Statement::Expression(expression) = &program.body[0] {
    //         if let Expression::FieldAccessOrMethodCall(field_access_or_method_call) = expression {
    //             assert_eq!(field_access_or_method_call.chains.len(), 0);
    //             assert_eq!(
    //                 field_access_or_method_call.expr,
    //                 Box::new(Expression::FuncCall(FuncCall {
    //                     identifier: Identifier {
    //                         name: "foo".to_string(),
    //                         span: Span::new(0, 3),
    //                         loc: Location::new(0, 5)
    //                     },
    //                     arguments: vec![
    //                         Expression::Literal(Literal::Integer(1)),
    //                         Expression::Literal(Literal::Integer(2))
    //                     ],
    //                     span: Span::new(0, 8),
    //                     loc: Location::new(0, 11)
    //                 }))
    //             );
    //         } else {
    //             panic!("Expected a function call but got something else.");
    //         }
    //     } else {
    //         panic!("Expected an expression but got something else.");
    //     }
    // });

    // define_test!(array_construction, "[]i32{1, 2, 3, func_call(1, 2)}", |program: ProgramTree| {
    //     if let Statement::Expression(expression) = &program.body[0] {
    //         if let Expression::Array(array) = expression {
    //             assert_eq!(array.elements.len(), 4);
    //             assert_eq!(
    //                 array.elements[0],
    //                 Expression::Literal(Literal::Integer(1))
    //             );
    //             assert_eq!(
    //                 array.elements[1],
    //                 Expression::Literal(Literal::Integer(2))
    //             );
    //             assert_eq!(
    //                 array.elements[2],
    //                 Expression::Literal(Literal::Integer(3))
    //             );
    //             assert_eq!(
    //                 array.elements[3],
    //                 Expression::FieldAccessOrMethodCall(FieldAccessOrMethodCall {
    //                     expr: Box::new(Expression::FuncCall(FuncCall {
    //                         identifier: Identifier {
    //                             name: "func_call".to_string(),
    //                             span: Span::new(15, 24),
    //                             loc: Location::new(0, 26)
    //                         },
    //                         arguments: vec![
    //                             Expression::Literal(Literal::Integer(1)),
    //                             Expression::Literal(Literal::Integer(2))
    //                         ],
    //                         span: Span::new(15, 29),
    //                         loc: Location::new(0, 32)
    //                     })),
    //                     chains: Vec::new()
    //                 })
    //             );
    //         } else {
    //             panic!("Expected an array literal but got something else.");
    //         }
    //     } else {
    //         panic!("Expected an expression but got something else.");
    //     }
    // });

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
                assert_eq!(
                    assignment.expr,
                    Expression::Literal(Literal::Integer(5))
                );
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

    define_test!(array_index, "arr[0][1]", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::ArrayIndex(array_index) = expression {
                assert_eq!(array_index.dimensions.len(), 2);
                assert_eq!(
                    array_index.dimensions[0],
                    Expression::Literal(Literal::Integer(0))
                );
                assert_eq!(
                    array_index.dimensions[1],
                    Expression::Literal(Literal::Integer(1))
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
                    module_import.segments[0],
                    ModuleSegment::SubModule(Identifier {
                        name: "object".to_string(),
                        span: Span::new(0, 5),
                        loc: Location::new(0, 8),
                    })
                );
                assert_eq!(
                    module_import.segments[1],
                    ModuleSegment::SubModule(Identifier {
                        name: "field".to_string(),
                        span: Span::new(7, 11),
                        loc: Location::new(0, 14),
                    })
                );
            } else {
                panic!("Expected a module import but got something else.");
            }
        } else {
            panic!("Expected an expression but got something else.");
        }
    });

    // define_test!(
    //     field_access_or_method_call_1,
    //     "object.method(1, 2)",
    //     |program: ProgramTree| {
    //         if let Statement::Expression(expression) = &program.body[0] {
    //             if let Expression::FieldAccessOrMethodCall(field_access_or_method_call) = expression {
    //                 assert_eq!(
    //                     field_access_or_method_call.expr,
    //                     Box::new(Expression::ModuleImport(ModuleImport {
    //                         segments: vec![ModuleSegment::SubModule(Identifier {
    //                             name: "object".to_string(),
    //                             span: Span::new(0, 5),
    //                             loc: Location::new(0, 8),
    //                         })],
    //                         span: Span::new(0, 13),
    //                         loc: Location::new(0, 15)
    //                     }))
    //                 );
    //                 assert_eq!(
    //                     field_access_or_method_call.chains,
    //                     vec![Either::Left(FuncCall {
    //                         identifier: Identifier {
    //                             name: "method".to_string(),
    //                             span: Span::new(7, 13),
    //                             loc: Location::new(0, 15)
    //                         },
    //                         arguments: vec![
    //                             Expression::Literal(Literal::Integer(1)),
    //                             Expression::Literal(Literal::Integer(2))
    //                         ],
    //                         span: Span::new(7, 19),
    //                         loc: Location::new(0, 20)
    //                     })]
    //                 )
    //             } else {
    //                 panic!("Expected a field access or method call but got something else.");
    //             }
    //         } else {
    //             panic!("Expected an expression but got something else.");
    //         }
    //     }
    // );

    // define_test!(
    //     field_access_or_method_call_2,
    //     "object.method(1, 2).field_access",
    //     |program: ProgramTree| {
    //         if let Statement::Expression(expression) = &program.body[0] {
    //             if let Expression::FieldAccessOrMethodCall(field_access_or_method_call) = expression {
    //                 assert_eq!(
    //                     field_access_or_method_call.expr,
    //                     Box::new(Expression::ModuleImport(ModuleImport {
    //                         segments: vec![ModuleSegment::SubModule(Identifier {
    //                             name: "object".to_string(),
    //                             span: Span::new(0, 5),
    //                             loc: Location::new(0, 8),
    //                         })],
    //                         span: Span::new(0, 13),
    //                         loc: Location::new(0, 15)
    //                     }))
    //                 );
    //                 assert_eq!(
    //                     field_access_or_method_call.chains,
    //                     vec![
    //                         Either::Left(FuncCall {
    //                             identifier: Identifier {
    //                                 name: "method".to_string(),
    //                                 span: Span::new(7, 13),
    //                                 loc: Location::new(0, 15)
    //                             },
    //                             arguments: vec![
    //                                 Expression::Literal(Literal::Integer(1)),
    //                                 Expression::Literal(Literal::Integer(2))
    //                             ],
    //                             span: Span::new(7, 19),
    //                             loc: Location::new(0, 33)
    //                         }),
    //                         Either::Right(FieldAccess {
    //                             identifier: Identifier {
    //                                 name: "field_access".to_string(),
    //                                 span: Span::new(20, 32),
    //                                 loc: Location::new(0, 33)
    //                             },
    //                             span: Span::new(20, 32),
    //                             loc: Location::new(0, 33)
    //                         })
    //                     ]
    //                 )
    //             } else {
    //                 panic!("Expected a field access or method call but got something else.");
    //             }
    //         } else {
    //             panic!("Expected an expression but got something else.");
    //         }
    //     }
    // );

    define_test!(cast_as, "10 as f64", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Cast(cast_as) = expression {
                assert_eq!(
                    *cast_as.expr,
                    Expression::Literal(Literal::Integer(10))
                );
                assert_eq!(cast_as.type_token, TokenKind::F64);
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
                    unary_operator.module_import.segments,
                    vec![ModuleSegment::SubModule(Identifier {
                        name: "my_var".to_string(),
                        span: Span::new(2, 7),
                        loc: Location::new(0, 9)
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

    define_test!(post_decrement, "my_var--", |program: ProgramTree| {
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
        "for (#i: i32 = 0; i < 10; i++) { }",
        |program: ProgramTree| {
            if let Statement::For(for_statement) = &program.body[0] {
                assert_eq!(
                    for_statement.initializer,
                    Some(Variable {
                        name: "i".to_string(),
                        ty: Some(TokenKind::I32),
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
                        span: Span::new(26, 27),
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
        "for (#i: i32 = 0;) { }",
        |program: ProgramTree| {
            if let Statement::For(for_statement) = &program.body[0] {
                assert_eq!(
                    for_statement.initializer,
                    Some(Variable {
                        name: "i".to_string(),
                        ty: Some(TokenKind::I32),
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
                        span: Span::new(14, 15),
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

    define_test!(control_flow_1, "if (a == 1) {}", |program: ProgramTree| {
        if let Statement::If(if_statement) = &program.body[0] {
            assert_eq!(
                if_statement.condition,
                Expression::Infix(BinaryExpression {
                    operator: Token {
                        kind: TokenKind::Equal,
                        span: Span::new(6, 7)
                    },
                    left: Box::new(Expression::ModuleImport(ModuleImport {
                        segments: vec![ModuleSegment::SubModule(Identifier {
                            name: "a".to_string(),
                            span: Span::new(4, 4),
                            loc: Location::new(0, 9)
                        })],
                        span: Span::new(4, 5),
                        loc: Location::new(0, 9)
                    })),
                    right: Box::new(Expression::Literal(Literal::Integer(1))),
                    span: Span::new(4, 10),
                    loc: Location::new(0, 12)
                })
            );
            assert_eq!(if_statement.alternate, None);
            assert_eq!(if_statement.branches, vec![]);
        } else {
            panic!("Expected an expression.");
        }
    });

    define_test!(control_flow_2, "if (some_value) {}", |program: ProgramTree| {
        if let Statement::If(if_statement) = &program.body[0] {
            assert_eq!(
                if_statement.condition,
                Expression::ModuleImport(ModuleImport {
                    segments: vec![ModuleSegment::SubModule(Identifier {
                        name: "some_value".to_string(),
                        span: Span::new(4, 13),
                        loc: Location::new(0, 16)
                    })],
                    span: Span::new(4, 14),
                    loc: Location::new(0, 16)
                })
            );
            assert_eq!(if_statement.alternate, None);
            assert_eq!(if_statement.branches, vec![]);
        } else {
            panic!("Expected an expression.");
        }
    });

    // define_test!(
    //     enum_1,
    //     "enum Color { RED, BLUE(variant: string, opacity: f64) }",
    //     |program: ProgramTree| {
    //         if let Statement::Enum(enum_decl) = &program.body[0] {
    //             assert_eq!(enum_decl.name.name, "Color");
    //             assert_eq!(enum_decl.variants.len(), 2);
    //             assert_eq!(
    //                 enum_decl.variants,
    //                 vec![
    //                     EnumVariant {
    //                         name: Identifier {
    //                             name: "RED".to_string(),
    //                             span: Span::new(13, 16),
    //                             loc: Location::new(0, 18)
    //                         },
    //                         fields: None,
    //                         loc: Location::default()
    //                     },
    //                     EnumVariant {
    //                         name: Identifier {
    //                             name: "BLUE".to_string(),
    //                             span: Span::new(18, 22),
    //                             loc: Location::new(0, 24)
    //                         },
    //                         fields: Some(vec![
    //                             EnumField {
    //                                 name: Identifier {
    //                                     name: "variant".to_string(),
    //                                     span: Span::new(23, 30),
    //                                     loc: Location::new(0, 32)
    //                                 },
    //                                 field_type: TokenKind::String,
    //                             },
    //                             EnumField {
    //                                 name: Identifier {
    //                                     name: "opacity".to_string(),
    //                                     span: Span::new(40, 47),
    //                                     loc: Location::new(0, 49)
    //                                 },
    //                                 field_type: TokenKind::F64,
    //                             }
    //                         ]),
    //                         loc: Location::default()
    //                     }
    //                 ]
    //             );
    //         } else {
    //             panic!("Expected an expression.");
    //         }
    //     }
    // );

    // define_test!(enum_2, "enum Color { RED, BLUE }", |program: ProgramTree| {
    //     if let Statement::Enum(enum_decl) = &program.body[0] {
    //         assert_eq!(enum_decl.name.name, "Color");
    //         assert_eq!(enum_decl.variants.len(), 2);
    //         assert_eq!(
    //             enum_decl.variants,
    //             vec![
    //                 EnumVariant {
    //                     name: Identifier {
    //                         name: "RED".to_string(),
    //                         span: Span::new(13, 16),
    //                         loc: Location::new(0, 18)
    //                     },
    //                     fields: None,
    //                     loc: Location::default()
    //                 },
    //                 EnumVariant {
    //                     name: Identifier {
    //                         name: "BLUE".to_string(),
    //                         span: Span::new(18, 22),
    //                         loc: Location::new(0, 25)
    //                     },
    //                     fields: None,
    //                     loc: Location::default()
    //                 }
    //             ]
    //         );
    //     } else {
    //         panic!("Expected an expression.");
    //     }
    // });

    define_test!(enum_3, "enum Color { }", |program: ProgramTree| {
        if let Statement::Enum(enum_decl) = &program.body[0] {
            assert_eq!(enum_decl.name.name, "Color");
            assert_eq!(enum_decl.variants.len(), 0);
            assert_eq!(enum_decl.variants, vec![]);
        } else {
            panic!("Expected an expression.");
        }
    });

    define_test!(import_1, "import module1.module2.module3;", |program: ProgramTree| {
        if let Statement::Import(import) = &program.body[0] {
            assert_eq!(
                import.paths,
                vec![ModulePath {
                    alias: None,
                    segments: vec![
                        ModuleSegment::SubModule(Identifier {
                            name: "module1".to_string(),
                            span: Span::new(7, 14),
                            loc: Location::new(0, 23)
                        }),
                        ModuleSegment::SubModule(Identifier {
                            name: "module2".to_string(),
                            span: Span::new(15, 22),
                            loc: Location::new(0, 31)
                        }),
                        ModuleSegment::SubModule(Identifier {
                            name: "module3".to_string(),
                            span: Span::new(23, 30),
                            loc: Location::new(0, 32)
                        }),
                    ]
                }]
            );
        } else {
            panic!("Expected an expression.");
        }
    });

    define_test!(
        import_2,
        "import ( module_alias: module1.module2.module3; )",
        |program: ProgramTree| {
            if let Statement::Import(import) = &program.body[0] {
                assert_eq!(
                    import.paths,
                    vec![ModulePath {
                        alias: Some("module_alias".to_string()),
                        segments: vec![
                            ModuleSegment::SubModule(Identifier {
                                name: "module1".to_string(),
                                span: Span::new(23, 30),
                                loc: Location::new(0, 39)
                            }),
                            ModuleSegment::SubModule(Identifier {
                                name: "module2".to_string(),
                                span: Span::new(31, 38),
                                loc: Location::new(0, 47)
                            }),
                            ModuleSegment::SubModule(Identifier {
                                name: "module3".to_string(),
                                span: Span::new(39, 46),
                                loc: Location::new(0, 50)
                            }),
                        ]
                    }]
                );
            } else {
                panic!("Expected an expression.");
            }
        }
    );
}
