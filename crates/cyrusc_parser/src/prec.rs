/* 
 * Copyright (c) 2026 The Cyrus Team
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
 */use cyrusc_ast::token::TokenKind;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign, // =
    Sizeof,
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

        TokenKind::SizeOf => Precedence::Sizeof,

        TokenKind::Dot | TokenKind::ThinArrow => Precedence::Field,

        // Calls and indexing
        TokenKind::LeftParen => Precedence::Call,
        TokenKind::LeftBracket => Precedence::Index,

        _ => Precedence::Lowest,
    }
}
