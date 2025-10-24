use ast::source_loc::SourceLoc;
use diagcentral::{Diag, DiagLevel, DiagLoc, display_single_diag};
use std::fmt;

#[derive(Debug, Clone)]
pub enum LexicalDiagKind {
    UnterminatedStringLiteral,
    InvalidFloatLiteral,
    InvalidIntegerLiteral,
    UnterminatedMultiLineComment,
    InvalidChar(char),
    EmptyCharLiteral,
    CharLiteralMustBeASingleUnit,
}

impl fmt::Display for LexicalDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexicalDiagKind::CharLiteralMustBeASingleUnit => {
                write!(f, "Character literal must be a single unit.")
            }
            LexicalDiagKind::UnterminatedStringLiteral => {
                write!(f, "Unterminated string literal, expected a closing quote.")
            }
            LexicalDiagKind::InvalidFloatLiteral => {
                write!(f, "Invalid float literal.")
            }
            LexicalDiagKind::InvalidIntegerLiteral => {
                write!(f, "Invalid integer literal.")
            }
            LexicalDiagKind::UnterminatedMultiLineComment => {
                write!(f, "Unterminated multi-line comment.")
            }
            LexicalDiagKind::EmptyCharLiteral => {
                write!(f, "Empty char literal is invalid.")
            }
            LexicalDiagKind::InvalidChar(c) => {
                write!(f, "Invalid character: '{}'.", c)
            }
        }
    }
}

pub fn lexer_invalid_char_error(file: String, line: usize, column: usize, ch: char) -> Diag<LexicalDiagKind> {
    display_single_diag!(Diag {
        level: DiagLevel::Error,
        kind: LexicalDiagKind::InvalidChar(ch),
        location: Some(DiagLoc::new(SourceLoc {
            file_path: file,
            column,
            line,
        })),
        hint: None,
    });
}
