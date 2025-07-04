#[cfg(test)]
mod tests {
    use crate::Lexer;
    use ast::ast::Literal;
    use ast::token::{Span, TokenKind};

    #[test]
    fn test_skip_whitespace() {
        let mut lexer = Lexer::new(
            String::from(
                "
            Hello World
        ",
            ),
            String::from("test.cyr"),
        );

        lexer.skip_whitespace();
    }

    fn assert_tokens(input: &'static str, expected_tokens: Option<&Vec<TokenKind>>, spans: Option<&Vec<Span>>) {
        let lexer = Lexer::new(input.to_string(), String::from("test_package.cyr"));
        let tokens = lexer;

        for (i, token) in tokens.into_iter().enumerate() {
            println!("{:?}", token);

            if let Some(list) = expected_tokens {
                assert_eq!(token.kind, list[i]);
            }

            if let Some(list) = spans {
                assert_eq!(token.span.start, list[i].start);
                assert_eq!(token.span.end, list[i].end);
            }
        }
    }

    #[test]
    fn test_code_1() {
        let code = "
        if a == 2 {
            puts \"Hello World\";
        }

        puts();
        ";

        assert_tokens(code, None, None);
    }

    #[test]
    fn test_code_2() {
        let code = "
        fn divide(num1: i32, num2: i32) {
            if num2 == 0 {
                1 + 2;
            }

            return num1 / num2;
        }

        divide(10, 2);
        ";

        assert_tokens(code, None, None);
    }

    #[test]
    fn test_code_3() {
        let code = "// Here is sample for loop
        for (#i = 0; i < 10; i++) {
            puts(\"i -> {i}\");
        }";

        assert_tokens(code, None, None);
    }

    #[test]
    fn test_code_4() {
        let code = "public fn main(): i32 {
            
        }";

        assert_tokens(code, None, None);
    }

    #[test]
    fn test_boolean_values() {
        assert_tokens(
            "true == false",
            Some(&vec![TokenKind::True, TokenKind::Equal, TokenKind::False]),
            None,
        );
        assert_tokens("#my_var: bool = true;", None, None);
    }

    #[test]
    fn test_operators() {
        assert_tokens(
            "+ - * / % =",
            Some(&vec![
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Asterisk,
                TokenKind::Slash,
                TokenKind::Percent,
                TokenKind::Assign,
            ]),
            None,
        );
    }

    #[test]
    fn test_comments() {
        assert_tokens("// Single line comment", None, None);

        let code = String::from(
            "
        // Sample comments
        // Another comment line
        1 + 2
        // After expression comments work too!
        \"It works very well!\"

        /*  Multi 
            Line 
            Comments 
        Also works! */

        1 + 2
        print();

        // Another comment after multi-line comment.
        ",
        );

        let lexer = Lexer::new(code, String::from("test_package.cyr"));
        for token in lexer {
            println!("{:?}", token);
        }
    }

    #[test]
    fn test_symbols() {
        assert_tokens(
            "() {} , # |",
            Some(&vec![
                TokenKind::LeftParen,
                TokenKind::RightParen,
                TokenKind::LeftBrace,
                TokenKind::RightBrace,
                TokenKind::Comma,
                TokenKind::Hashtag,
                TokenKind::Pipe,
            ]),
            None,
        );
    }

    #[test]
    fn test_equals() {
        assert_tokens(
            "!= , ==",
            Some(&vec![TokenKind::NotEqual, TokenKind::Comma, TokenKind::Equal]),
            None,
        );
    }

    #[test]
    fn test_keywords() {
        assert_tokens(
            "fn match if else return for break continue",
            Some(&vec![
                TokenKind::Function,
                TokenKind::Match,
                TokenKind::If,
                TokenKind::Else,
                TokenKind::Return,
                TokenKind::For,
                TokenKind::Break,
                TokenKind::Continue,
            ]),
            None,
        );
    }

    #[test]
    fn test_less_greater() {
        assert_tokens(
            "<= >=",
            Some(&vec![TokenKind::LessEqual, TokenKind::GreaterEqual]),
            None,
        );
    }

    #[test]
    fn test_and_or() {
        assert_tokens("&& ||", Some(&vec![TokenKind::And, TokenKind::Or]), None);
    }

    #[test]
    fn test_reading_identifier() {
        assert_tokens(
            "fn foo() {}",
            Some(&vec![
                TokenKind::Function,
                TokenKind::Identifier {
                    name: "foo".to_string(),
                },
                TokenKind::LeftParen,
                TokenKind::RightParen,
                TokenKind::LeftBrace,
                TokenKind::RightBrace,
            ]),
            None,
        );
    }

    #[test]
    fn test_variable_declaration() {
        assert_tokens(
            "#my_var = 10;",
            Some(&vec![
                TokenKind::Hashtag,
                TokenKind::Identifier {
                    name: "my_var".to_string(),
                },
                TokenKind::Assign,
                TokenKind::Literal(Literal::Integer(10)),
                TokenKind::Semicolon,
            ]),
            None,
        );
    }

    #[test]
    fn test_function_declaration() {
        assert_tokens(
            "fn foo_bar(a, b) { return a + b; }",
            Some(&vec![
                TokenKind::Function,
                TokenKind::Identifier {
                    name: "foo_bar".to_string(),
                },
                TokenKind::LeftParen,
                TokenKind::Identifier { name: "a".to_string() },
                TokenKind::Comma,
                TokenKind::Identifier { name: "b".to_string() },
                TokenKind::RightParen,
                TokenKind::LeftBrace,
                TokenKind::Return,
                TokenKind::Identifier { name: "a".to_string() },
                TokenKind::Plus,
                TokenKind::Identifier { name: "b".to_string() },
                TokenKind::Semicolon,
                TokenKind::RightBrace,
            ]),
            None,
        );
    }

    #[test]
    fn test_function_call() {
        assert_tokens(
            "foo_bar()",
            Some(&vec![
                TokenKind::Identifier {
                    name: "foo_bar".to_string(),
                },
                TokenKind::LeftParen,
                TokenKind::RightParen,
            ]),
            None,
        );

        assert_tokens(
            "foo_bar(1, 2)",
            Some(&vec![
                TokenKind::Identifier {
                    name: "foo_bar".to_string(),
                },
                TokenKind::LeftParen,
                TokenKind::Literal(Literal::Integer(1)),
                TokenKind::Comma,
                TokenKind::Literal(Literal::Integer(2)),
                TokenKind::RightParen,
            ]),
            None,
        );
    }

    #[test]
    fn test_str() {
        assert_tokens(
            "\"Cyrus-Lang\"",
            Some(&vec![TokenKind::Literal(Literal::String("Cyrus-Lang".to_string()))]),
            None,
        );
    }

    #[test]
    fn test_floating_numbers() {
        assert_tokens("2.56", Some(&vec![TokenKind::Literal(Literal::Float(2.56))]), None);
    }

    #[test]
    fn test_increment_and_decrement() {
        assert_tokens(
            "i++",
            Some(&vec![
                TokenKind::Identifier { name: "i".to_string() },
                TokenKind::Increment,
            ]),
            None,
        );

        assert_tokens(
            "i--",
            Some(&vec![
                TokenKind::Identifier { name: "i".to_string() },
                TokenKind::Decrement,
            ]),
            None,
        );
    }

    #[test]
    fn test_tokenizing_emoji() {
        assert_tokens("\"This is 🖤 made by a string.\"", None, None);
        assert_tokens("printf(\"Hello 🖤\");", None, None);
    }
}
