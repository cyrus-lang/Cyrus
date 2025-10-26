use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc, display_single_diag};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum LexicalDiagKind {
    #[error("Character literal must be a single unit.")]
    CharLiteralMustBeASingleUnit,

    #[error("Unterminated string literal, expected a closing quote.")]
    UnterminatedStringLiteral,

    #[error("Invalid float literal.")]
    InvalidFloatLiteral,

    #[error("Invalid integer literal.")]
    InvalidIntegerLiteral,

    #[error("Unterminated multi-line comment.")]
    UnterminatedMultiLineComment,

    #[error("Empty char literal is invalid.")]
    EmptyCharLiteral,

    #[error("Invalid character: '{0}'.")]
    InvalidChar(char),
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
