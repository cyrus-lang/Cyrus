use ast::token::TokenKind;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Sizeof,
    Or,          // ||
    And,         // &&
    Equals,      // ==, !=
    LessGreater, // >, <, >=, <=
    Sum,         // +, -
    Product,     // *, /, %
    Bitwise,     // &, |, ~, &~
    Prefix,      // -X, !X
    Call,        // my_function(x)
    Index,       // array[index]
    Cast,        // Type conversion
}

pub fn token_precedence_of(token_kind: TokenKind) -> Precedence {
    match token_kind {
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

        // Calls and indexing
        TokenKind::LeftParen => Precedence::Call,
        TokenKind::LeftBracket => Precedence::Index,

        _ => Precedence::Lowest,
    }
}

#[cfg(test)]
mod tests {
    use super::Precedence;

    #[test]
    fn test_compare_precedences() {
        assert!(Precedence::Lowest < Precedence::LessGreater);
        assert!(Precedence::Call > Precedence::Sum);
        assert!(Precedence::Call < Precedence::Cast);
    }
}
