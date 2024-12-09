#[cfg(test)]
mod tests {
    use lexer::Lexer;
    use token::TokenKind;

    use crate::Parser;

    fn assert_parse(input: &str) {
        match Parser::parse(input.to_string()) {
            Ok(program) => {
                println!("{:#?}", program);
            }
            Err(parse_errors) => {
                println!("{:#?}", parse_errors);
            }
        }
    }

    #[test]
    fn test_parser_simple_expression() {
        assert_parse("1 == 11");
        assert_parse("1 != 1");
        assert_parse("1 < 1");
        assert_parse("1 > 1");
        assert_parse("100 <= 100");
        assert_parse("100 >= 100");
        assert_parse("1 + 2");
        assert_parse("1 - 2");
        assert_parse("1 * 2");
        assert_parse("1 / 2");
        assert_parse("1 + 2 / 2");
        assert_parse("1 + 2 / 2 - 10");
    }

    #[test]
    fn test_parse_bool_expressions() {
        assert_parse("true");
        assert_parse("false");
        assert_parse("true == true");
        assert_parse("false == false");
        assert_parse("true == false");
        assert_parse("false == true");
    }

    #[test]
    fn test_variable_declaration() {
        assert_parse("#my_var = 1 + 2 * 3;");
    }

    #[test]
    fn test_if_statement() {
        assert_parse(
            "
        if (1 < 2)
        {
            print(1);
        }
        else if (2 == 2) {
            print(2);
        }
        else {
            print(3);
        }
        ",
        );
    }

    #[test]
    fn test_if_statement2() {
        assert_parse(
            "
        if (next() == 0) {
            print(10 / 2 * 3 + 1);
        }
        ",
        );
    }

    #[test]
    fn test_parse_block_statement() {
        let input = "{ 1 + 2; hello(); }";
        let mut binding = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut binding);
        let block = parser.parse_block_statement().unwrap();
        println!("{:#?}", block);
    }

    #[test]
    fn test_return_statement() {
        assert_parse("ret 1 + 2");
    }

    #[test]
    fn test_parse_function_params() {
        let mut lexer = Lexer::new("(a, b, c)".to_string());
        let mut parser = Parser::new(&mut lexer);
        let params = parser.parse_function_params();

        println!("{:#?}", params);
    }

    #[test]
    fn test_parse_expression_series() {
        let mut lexer = Lexer::new("[1, 2, 3, ]".to_string());
        let mut parser = Parser::new(&mut lexer);
        let params = parser
            .parse_expression_series(TokenKind::RightBracket)
            .unwrap();

        println!("{:#?}", params.0);
    }

    #[test]
    fn test_function_statement() {
        assert_parse("fn foo_bar(a, b) { ret a + b; } foo_bar(1, 2);");
    }

    #[test]
    fn test_function_call_expresion() {
        assert_parse("foo_bar(1, 2);");
        assert_parse("foo_bar();");
    }

    #[test]
    fn test_parse_1() {
        assert_parse(
            "
        print(1);
        print(1 + 2);
        print(1 > 2);
        print(1 >= 2);
        print(1 < 2);
        print(1 <= 2);
        print(1 == 2);
        print(1 != 2);
        print(!true);
        print(!false);
        print(\"Cyrus Lang =)\");
",
        );
    }

    #[test]
    fn test_parse_2() {
        assert_parse("print(1 + 2);");
    }

    #[test]
    fn test_parse_modulo() {
        assert_parse("3 * 2");
    }

    #[test]
    fn test_comparative_expression() {
        assert_parse("i < 10;");
        // assert_parse("i > 10;");
        // assert_parse("i <= 10");
        // assert_parse("i >= 10");
        // assert_parse("i == 10");
        // assert_parse("i != 10");
    }

    #[test]
    fn test_increment_decrement_expression() {
        assert_parse("++i");
        assert_parse("--i");
        assert_parse("i++");
        assert_parse("i--");
    }

    #[test]
    fn test_parse_for_statement() {
        assert_parse(
            "
            for #i = 0; i < 10; i++ {
                print(i);
            }
            ",
        );
    }

    #[test]
    fn test_identifier_and_comparative() {
        assert_parse(
            "
            i < j
            ",
        );
        assert_parse(
            "
            i < 10
            ",
        );
        assert_parse(
            "
            10 < j
            ",
        );
    }
}
