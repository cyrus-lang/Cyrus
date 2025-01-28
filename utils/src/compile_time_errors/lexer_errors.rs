use super::errors::CompileTypeErrorType;
use crate::compile_time_errors::errors::ERROR_PIPE_STR;
use core::fmt;

#[derive(Debug)]
pub enum LexicalErrorType {
    UnterminatedStringLiteral,
    InvalidFloatLiteral,
    InvalidIntegerLiteral,
    UnterminatedMultiLineComment,
    EmptyCharLiteral,
}

impl fmt::Display for LexicalErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexicalErrorType::UnterminatedStringLiteral => write!(f, "UnterminatedStringLiteral"),
            LexicalErrorType::InvalidFloatLiteral => write!(f, "InvalidFloatLiteral"),
            LexicalErrorType::InvalidIntegerLiteral => write!(f, "InvalidIntegerLiteral"),
            LexicalErrorType::UnterminatedMultiLineComment => write!(f, "UnterminatedMultiLineComment"),
            LexicalErrorType::EmptyCharLiteral => write!(f, "EmptyCharLiteral"),
        }
    }
}

impl CompileTypeErrorType for LexicalErrorType {
    fn context(&self) -> String {
        String::from(match self {
            LexicalErrorType::UnterminatedStringLiteral => "expected terminate string literal with double quote.",
            LexicalErrorType::InvalidFloatLiteral => "invalid float literal",
            LexicalErrorType::InvalidIntegerLiteral => "invalid integer literal",
            LexicalErrorType::UnterminatedMultiLineComment => "unterminated multi-line comment",
            LexicalErrorType::EmptyCharLiteral => "empty char literal is invalid",
        })
    }
}

pub fn lexer_invalid_char_error(file_name: String, line: usize, column: usize, ch: char) {
    println!("| Error: Lexination Failed {}", ERROR_PIPE_STR);
    println!("| Path: {}", file_name);
    println!("| At: {}:{}\n", line, column);
    println!(
        "\tLexical error at line {}, column {} because of invalid char '{}'.\n",
        line, column, ch
    );
}

pub fn lexer_unknown_char_error(file_name: String, line: usize, column: usize) {
    println!("| Error: Lexination Failed {}", ERROR_PIPE_STR);
    println!("| Path: {}", file_name);
    println!("| At: {}:{}\n", line, column);
    println!(
        "\tLexical error at line {}, column {} because of invalid char.\n",
        line, column
    );
}
