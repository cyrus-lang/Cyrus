/* 
 * Copyright (c) 2026 The Cyrus Language
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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
