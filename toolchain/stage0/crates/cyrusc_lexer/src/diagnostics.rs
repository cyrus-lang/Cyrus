// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language
use cyrusc_diagcentral::DiagKind;
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

    #[error("Invalid escape sequence.")]
    InvalidEscapeSequence,

    #[error("Invalid character: '{0}'.")]
    InvalidChar(char),
}

impl DiagKind for LexicalDiagKind {}
