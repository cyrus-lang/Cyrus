use std::fmt;

use ast::token::{Location, Span};
use diagcentral::{Diag, DiagLevel, DiagLoc};

#[derive(Debug, Clone)]
pub enum LexicalDiagKind {
    UnterminatedStringLiteral,
    InvalidFloatLiteral,
    InvalidIntegerLiteral,
    UnterminatedMultiLineComment,
    InvalidChar(char),
    EmptyCharLiteral,
}

impl fmt::Display for LexicalDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexicalDiagKind::UnterminatedStringLiteral => {
                write!(f, "unterminated string literal, expected a closing quote")
            }
            LexicalDiagKind::InvalidFloatLiteral => {
                write!(f, "invalid float literal")
            }
            LexicalDiagKind::InvalidIntegerLiteral => {
                write!(f, "invalid integer literal")
            }
            LexicalDiagKind::UnterminatedMultiLineComment => {
                write!(f, "unterminated multi-line comment")
            }
            LexicalDiagKind::EmptyCharLiteral => {
                write!(f, "empty char literal is invalid")
            }
            LexicalDiagKind::InvalidChar(c) => {
                write!(f, "invalid character: '{}'", c)
            }
        }
    }
}

pub fn lexer_invalid_char_error(
    file: String,
    line: usize,
    column: usize,
    ch: char,
    span: Span,
) -> Diag<LexicalDiagKind> {
    Diag {
        level: DiagLevel::Error,
        kind: LexicalDiagKind::InvalidChar(ch),
        location: Some(DiagLoc::new(file, Location::new(line, column), span.end)),
        hint: None,
    }
}
