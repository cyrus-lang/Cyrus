use crate::ast::{Identifier, Literal};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Illegal,
    EOF,
    Identifier { name: String },
    Literal(Literal),
    // Operators
    Plus,
    Minus,
    Slash,
    Asterisk,
    Percent,
    Increment,
    Decrement,
    // Symbols
    BackTick,
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
    Dot,
    DoubleQuote,
    SingleQuote,
    Pipe,
    Ampersand,
    Semicolon,
    Colon,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    // Keywords
    Function,
    Match,
    If,
    Else,
    Return,
    For,
    Break,
    Continue,
    Struct,
    Import,
    Decl,
    // Types
    UserDefinedType(Identifier),
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    Char,
    Float,
    Double,
    CSize,
    Void,
    String,
    Bool,
    True,
    False,
    Null,
    As,

    AddressOf(Box<TokenKind>),
    Dereference(Box<TokenKind>),

    // DataType, Dimensions
    Array(Box<TokenKind>, Vec<Option<TokenKind>>),

    // Object Visibility Keywords
    Extern,
    Pub,
    Inline,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier { name } => write!(f, "{}", name),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Percent => write!(f, "%"),
            Self::Assign => write!(f, "="),
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::LeftBracket => write!(f, "[["),
            Self::RightBracket => write!(f, "]]"),
            Self::Comma => write!(f, ","),
            Self::Hashtag => write!(f, "#"),
            Self::Dot => write!(f, "."),
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
            Self::Colon => write!(f, ":"),
            // Keywords
            Self::Function => write!(f, "fn"),
            Self::Match => write!(f, "match"),
            Self::Struct => write!(f, "struct"),
            Self::Import => write!(f, "import"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Return => write!(f, "return"),
            Self::For => write!(f, "for"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Null => write!(f, "null"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::Float => write!(f, "float"),
            Self::Double => write!(f, "double"),
            Self::Void => write!(f, "void"),
            Self::Array(data_type, array) => {
                write!(f, "{}", data_type)?;

                for item in array {
                    if let Some(dimension) = item {
                        write!(f, "[{}]", dimension)?;
                    } else {
                        write!(f, "[]")?;
                    }
                }

                todo!()
            }
            Self::String => write!(f, "string"),
            // ETC
            Self::Illegal => write!(f, "ILLEGAL"),
            Self::EOF => write!(f, "EOF"),
            _ => write!(f, "INVALID_TOKEN"),
        }
    }
}

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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}
