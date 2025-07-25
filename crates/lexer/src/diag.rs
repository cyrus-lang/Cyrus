use core::fmt;

use ast::token::{Location, Span};
use diag::errors::{CompileTimeError, CompileTypeErrorType};

#[derive(Debug)]
pub enum LexicalErrorType {
    UnterminatedStringLiteral,
    InvalidFloatLiteral,
    InvalidIntegerLiteral,
    UnterminatedMultiLineComment,
    InvalidChar(char),
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
            LexicalErrorType::InvalidChar(_) => write!(f, "InvalidChar"),
        }
    }
}

impl CompileTypeErrorType for LexicalErrorType {
    fn context(&self) -> String {
        String::from(match self {
            LexicalErrorType::UnterminatedStringLiteral => "Expected terminate string literal with double quote",
            LexicalErrorType::InvalidFloatLiteral => "Invalid float literal",
            LexicalErrorType::InvalidIntegerLiteral => "Invalid integer literal",
            LexicalErrorType::UnterminatedMultiLineComment => "Unterminated multi-line comment",
            LexicalErrorType::EmptyCharLiteral => "Empty char literal is invalid",
            LexicalErrorType::InvalidChar(ch) => {
                return format!("Invalid char: '{}'", ch);
            }
        })
    }
}

pub fn lexer_invalid_char_error(
    file_name: String,
    line: usize,
    column: usize,
    ch: char,
    span: Span,
    source_content: Box<String>,
) {
    CompileTimeError {
        location: Location::new(line, column),
        etype: LexicalErrorType::InvalidChar(ch),
        file_name: Some(file_name),
        verbose: None,
        caret: Some(span),
        source_content,
    }
    .print();
}
