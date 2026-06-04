// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language
use cyrusc_tokens::TokenKind;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,      // =
    Or,          // ||
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

pub fn token_precedence_of(token_kind: TokenKind) -> Precedence {
    match token_kind {
        TokenKind::Assign => Precedence::Assign,
        TokenKind::Or => Precedence::Or,
        TokenKind::And => Precedence::And,
        TokenKind::Equal | TokenKind::NotEqual => Precedence::Equals,

        // Comparison
        TokenKind::LessThan | TokenKind::LessEqual | TokenKind::GreaterThan | TokenKind::GreaterEqual => {
            Precedence::LessGreater
        }

        // Arithmetic
        TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
        TokenKind::Asterisk | TokenKind::Slash | TokenKind::Percent => Precedence::Product,

        // Bitwise
        TokenKind::Ampersand
        | TokenKind::Pipe
        | TokenKind::Tilde
        | TokenKind::AmpTilde
        | TokenKind::Caret
        | TokenKind::ShiftLeft
        | TokenKind::ShiftRight => Precedence::Bitwise,

        TokenKind::Dot | TokenKind::ThinArrow => Precedence::Field,

        // Calls and indexing
        TokenKind::LeftParen => Precedence::Call,
        TokenKind::LeftBracket => Precedence::Index,

        _ => Precedence::Lowest,
    }
}
