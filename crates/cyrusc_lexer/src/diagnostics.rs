// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_diagcentral::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag};
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

impl DiagKind for LexicalDiagKind {}

pub fn lexer_invalid_char_error(file: String, line: usize, column: usize, ch: char) -> Diag {
    display_single_diag!(Diag {
        level: DiagLevel::Error,
        kind: Box::new(LexicalDiagKind::InvalidChar(ch)),
        location: Some(DiagLoc::new(SourceLoc {
            file_path: file,
            column,
            line,
        })),
        hint: None,
    });
}
