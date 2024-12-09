use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Illegal,
    EOF,
    Identifier { name: String },
    Integer(i64),
    String(String),

    // Operators
    Plus,
    Minus,
    Slash,
    Asterisk,
    Modulo,
    Increment,
    Decrement,

    // Symbols
    Assign,
    Equal,
    NotEqual,
    Bang,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Hashtag,
    DoubleQuote,
    SingleQuote,
    Pipe,
    Ampersand,
    Semicolon,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    True,
    False,

    // Keywords
    Function,
    Match,
    If,
    Else,
    Return,
    For,
    Break,
    Continue,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier { name } => write!(f, "{}", name),
            Self::Integer(value) => write!(f, "{}", value),
            Self::String(value) => write!(f, "{}", value),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
            Self::Assign => write!(f, "="),
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::LeftBracket => write!(f, "[["),
            Self::RightBracket => write!(f, "]]"),
            Self::Comma => write!(f, ","),
            Self::Hashtag => write!(f, "#"),
            Self::DoubleQuote => write!(f, "\""),
            Self::SingleQuote => write!(f, "'"),
            Self::Pipe => write!(f, "|"),
            Self::Ampersand => write!(f, "&"),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessEqual => write!(f, "<="),
            Self::GreaterEqual => write!(f, ">="),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::Semicolon => write!(f, ";"),
            Self::Function => write!(f, "fn"),
            Self::Match => write!(f, "match"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Return => write!(f, "ret"),
            Self::For => write!(f, "for"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Illegal => write!(f, "ILLEGAL"),
            Self::EOF => write!(f, "EOF"),
            _ => write!(f, "INVALID_TOKEN"),
        }
    }
}

// Span essentially pinpoints the token's exact location within the source file.
// That is really useful for error-reporting, syntax-hightlighting, and etc.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn new_empty_span() -> Self {
        Self { start: 0, end: 0 }
    }
}
