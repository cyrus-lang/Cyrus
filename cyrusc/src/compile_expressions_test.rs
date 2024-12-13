#[cfg(test)]
mod tests {
    use crate::*;
    use ast::token::Span;
    use ast::token::Token;
    use ast::token::TokenKind;
    use llvm_sys::core::*;
    use llvm_sys::prelude::*;

    #[test]
    fn test_compile_literal_i32() {
        let mut ec = Compiler {
            builder: unsafe { LLVMCreateBuilder() },
        };

        let value = ec
            .compile_literal(Literal::Integer(IntegerLiteral::I32(10)))
            .unwrap();
        unsafe {
            let extracted_value: i32 = LLVMConstIntGetSExtValue(value).try_into().unwrap();
            assert_eq!(extracted_value, 10);
        }
    }

    #[test]
    fn test_compile_literal_i64() {
        let mut ec = Compiler {
            builder: unsafe { LLVMCreateBuilder() },
        };

        let value = ec
            .compile_literal(Literal::Integer(IntegerLiteral::I64(10)))
            .unwrap();

        unsafe {
            let extracted_value: i64 = LLVMConstIntGetSExtValue(value).try_into().unwrap();
            assert_eq!(extracted_value, 10);
        }
    }

    #[test]
    fn test_compile_literal_u32() {
        let mut ec = Compiler {
            builder: unsafe { LLVMCreateBuilder() },
        };

        let value = ec
            .compile_literal(Literal::Integer(IntegerLiteral::U32(5)))
            .unwrap();

        unsafe {
            let extracted_value: u32 = LLVMConstIntGetSExtValue(value).try_into().unwrap();
            assert_eq!(extracted_value, 5);
        }
    }

    #[test]
    fn test_compile_literal_u64() {
        let mut ec = Compiler {
            builder: unsafe { LLVMCreateBuilder() },
        };

        let value = ec
            .compile_literal(Literal::Integer(IntegerLiteral::U64(5)))
            .unwrap();

        unsafe {
            let extracted_value: u64 = LLVMConstIntGetSExtValue(value).try_into().unwrap();
            assert_eq!(extracted_value, 5);
        }
    }

    #[test]
    fn test_compile_literal_f32() {
        let mut ec = Compiler {
            builder: unsafe { LLVMCreateBuilder() },
        };

        let value = ec
            .compile_literal(Literal::Float(FloatLiteral::F32(5.5)))
            .unwrap();

        unsafe {
            let mut loses_info: LLVMBool = 0;
            let extracted_value = LLVMConstRealGetDouble(value, &mut loses_info);
            assert_eq!(extracted_value, 5.5);
        }
    }

    #[test]
    fn test_compile_literal_f64() {
        let mut ec = Compiler {
            builder: unsafe { LLVMCreateBuilder() },
        };

        let value = ec
            .compile_literal(Literal::Float(FloatLiteral::F64(5.5)))
            .unwrap();

        unsafe {
            let mut loses_info: LLVMBool = 0;
            let extracted_value = LLVMConstRealGetDouble(value, &mut loses_info);
            assert_eq!(extracted_value, 5.5);
            assert_eq!(loses_info, 0);
        }
    }

    #[test]
    fn test_compile_literal_string() {
        let mut ec = Compiler {
            builder: unsafe { LLVMCreateBuilder() },
        };

        let value_str = String::from("Cyrus!");

        ec.compile_literal(Literal::String(StringLiteral {
            raw: value_str.clone(),
            span: Span::new_empty_span(),
        }))
        .unwrap();
    }

    #[test]
    fn test_compile_literal_boolean() {
        let mut ec = Compiler {
            builder: unsafe { LLVMCreateBuilder() },
        };

        let boolean_literal_test_cases: Vec<(bool, i32)> = vec![(true, 1), (false, 0)];

        for tc in boolean_literal_test_cases {
            let value = ec
                .compile_literal(Literal::Boolean(BooleanLiteral {
                    raw: tc.0,
                    span: Span::new_empty_span(),
                }))
                .unwrap();

            unsafe {
                let extracted_value: u64 = LLVMConstIntGetZExtValue(value).try_into().unwrap();
                assert_eq!(extracted_value, tc.1.try_into().unwrap());
            }
        }
    }

    #[test]
    fn test_compile_prefix_expression() {
        let mut ec = Compiler {
            builder: unsafe { LLVMCreateBuilder() },
        };

        let value = ec
            .compile_prefix_expression(UnaryExpression{
                operator: Token { kind: TokenKind::Minus, span: Span::new_empty_span()  },
                operand: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral::I32(10)))),
                span: Span::new_empty_span(),
            })
            .unwrap();

        unsafe {
            let extracted_value: i32 = LLVMConstIntGetSExtValue(value).try_into().unwrap();
            assert_eq!(extracted_value, -10);
        }
    }

    // TODO 
    // Write unit test for prefix_expression->bang kind.
    // After reviewing the code.

    
}
