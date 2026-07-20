// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language
use cyrusc_tokens::TokenKind;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,      // =
    Or,          // ||
    NullCoalesce, // ??
    And,         // &&
    Equals,      // ==, !=
    LessGreater, // >, <, >=, <=
    Sum,         // +, -
    Product,     // *, /, %
    Bitwise,     // &, |, ~, &~, ^, <<, >>
    Prefix,      // -X, !X, &X
    Call,        // my_function(x)
    Index,       // array[index]
    Field,       // . and ->
}

pub fn token_precedence_of(token_kind: TokenKind) -> Option<Precedence> {
    match token_kind {
        TokenKind::Assign => Some(Precedence::Assign),
        TokenKind::Or => Some(Precedence::Or),
        TokenKind::QuestionQuestion => Some(Precedence::NullCoalesce),
        TokenKind::And => Some(Precedence::And),
        TokenKind::Equal | TokenKind::NotEqual => Some(Precedence::Equals),

        // Comparison
        TokenKind::LessThan | TokenKind::LessEqual | TokenKind::GreaterThan | TokenKind::GreaterEqual => {
            Some(Precedence::LessGreater)
        }

        // Arithmetic
        TokenKind::Plus | TokenKind::Minus => Some(Precedence::Sum),
        TokenKind::Asterisk | TokenKind::Slash | TokenKind::Percent => Some(Precedence::Product),

        // Bitwise
        TokenKind::Ampersand
        | TokenKind::Pipe
        | TokenKind::Tilde
        | TokenKind::AmpTilde
        | TokenKind::Caret
        | TokenKind::ShiftLeft
        | TokenKind::ShiftRight => Some(Precedence::Bitwise),

        TokenKind::Dot | TokenKind::ThinArrow => Some(Precedence::Field),

        // Calls and indexing
        TokenKind::LeftParen => Some(Precedence::Call),
        TokenKind::LeftBracket => Some(Precedence::Index),

        _ => None,
    }
}
