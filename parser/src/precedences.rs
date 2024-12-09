use core::fmt;

use token::TokenKind;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // + or =
    Product,     // * or /
    Prefix,      // -X or !X
    Call,        // my_function(x)
    Index,       // array[index]
}

pub fn determine_token_precedence(token_kind: TokenKind) -> Precedence {
    match token_kind {
        TokenKind::Equal => Precedence::Equals,
        TokenKind::NotEqual => Precedence::Equals,
        TokenKind::LessThan => Precedence::LessGreater,
        TokenKind::LessEqual => Precedence::LessGreater,
        TokenKind::GreaterThan => Precedence::LessGreater,
        TokenKind::GreaterEqual => Precedence::LessGreater,
        TokenKind::Plus => Precedence::Sum,
        TokenKind::Minus => Precedence::Sum,
        TokenKind::Asterisk => Precedence::Product,
        TokenKind::Slash => Precedence::Product,
        TokenKind::LeftParen => Precedence::Call,
        TokenKind::LeftBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}

impl fmt::Display for Precedence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Precedence::Lowest => write!(f, "lowest"),
            Precedence::Equals => write!(f, "equals"),
            Precedence::LessGreater => write!(f, "less_greater"),
            Precedence::Sum => write!(f, "sum"),
            Precedence::Product => write!(f, "product"),
            Precedence::Prefix => write!(f, "prefix"),
            Precedence::Call => write!(f, "call"),
            Precedence::Index => write!(f, "index"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Precedence;

    #[test]
    fn test_compare_precedences() {
        assert!(Precedence::Lowest < Precedence::LessGreater);
        assert!(Precedence::Call > Precedence::Sum);
    }
}
