#[cfg(test)]
mod tests {
    use crate::compile_time_errors::{errors::CompileTimeError, parser_errors::ParserErrorType};
    use ast::token::Location;

    #[test]
    fn test_error() {
        let code_raw = String::from("#a: i32 = ;");
        CompileTimeError {
            location: Location { line: 0, column: 10 },
            etype: ParserErrorType::InvalidToken(ast::token::TokenKind::Semicolon),
            file_name: Some(String::from("test.cyr")),
            code_raw: Some(code_raw),
            verbose: None,
            caret: true,
        }
        .print();
    }
}
