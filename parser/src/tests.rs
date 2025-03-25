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

    define_test!(array_literal, "[1, 2, 3]", |program: ProgramTree| {
        if let Statement::Expression(expression) = &program.body[0] {
            if let Expression::Array(array) = expression {
                assert_eq!(array.elements.len(), 3);
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

    define_test!(
        struct_initialization,
        "MyStruct { field1: 10, field2: true };",
        |program: ProgramTree| {
            if let Statement::Expression(expression) = &program.body[0] {
                if let Expression::StructInit(struct_init) = expression {
                    assert_eq!(struct_init.struct_name.identifier.name, "MyStruct");
                    assert_eq!(struct_init.field_inits.len(), 2);
                    assert_eq!(struct_init.field_inits[0].name, "field1");
                    assert_eq!(
                        struct_init.field_inits[0].value,
                        Expression::Literal(Literal::Integer(IntegerLiteral::I32(10)))
                    );
                    assert_eq!(struct_init.field_inits[1].name, "field2");
                    assert_eq!(
                        struct_init.field_inits[1].value,
                        Expression::Literal(Literal::Bool(BoolLiteral {
                            raw: true,
                            span: Span::new(31, 35)
                        }))
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
                        span: Span { start: 0, end: 5 },
                        loc: Location { line: 0, column: 8 },
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
                            span: Span { start: 7, end: 12 },
                            loc: Location { line: 0, column: 14 }
                        }
                    );
                    assert_eq!(method_call.func_name.sub_modules, vec![
                        ModulePath::SubModule(Identifier {
                            name: "object".to_string(),
                            span: Span { start: 0, end: 5 },
                            loc: Location { line: 0, column: 8 }
                        })
                    ])
                } else {
                    panic!("Expected a field access or method call but got something else.");
                }
            } else {
                panic!("Expected an expression but got something else.");
            }
        }
    );
}
