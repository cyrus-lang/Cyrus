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
    DoubleDot,
    TripleDot,
    DoubleQuote,
    SingleQuote,
    Pipe,
    Ampersand,
    Semicolon,
    Colon,
    DoubleColon,
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
    SizeT,
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
            Self::DoubleDot => write!(f, ".."),
            Self::TripleDot => write!(f, "..."),
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
            Self::DoubleColon => write!(f, "::"),
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
            Self::As => write!(f, "as"),
            Self::Literal(literal) => match literal {
                Literal::Integer(integer_literal) => match integer_literal {
                    crate::ast::IntegerLiteral::I8(value) => write!(f, "{}", value),
                    crate::ast::IntegerLiteral::I16(value) => write!(f, "{}", value),
                    crate::ast::IntegerLiteral::I32(value) => write!(f, "{}", value),
                    crate::ast::IntegerLiteral::I64(value) => write!(f, "{}", value),
                    crate::ast::IntegerLiteral::I128(value) => write!(f, "{}", value),
                    crate::ast::IntegerLiteral::U8(value) => write!(f, "{}", value),
                    crate::ast::IntegerLiteral::U16(value) => write!(f, "{}", value),
                    crate::ast::IntegerLiteral::U32(value) => write!(f, "{}", value),
                    crate::ast::IntegerLiteral::U64(value) => write!(f, "{}", value),
                    crate::ast::IntegerLiteral::U128(value) => write!(f, "{}", value),
                    crate::ast::IntegerLiteral::SizeT(value) => write!(f, "{}", value),
                },
                Literal::Float(float_literal) => match float_literal {
                    crate::ast::FloatLiteral::Float(value) => write!(f, "{}", value),
                    crate::ast::FloatLiteral::Double(value) => write!(f, "{}", value),
                },
                Literal::Bool(bool_literal) => write!(f, "{}", bool_literal.raw),
                Literal::String(string_literal) => write!(f, "\"{}\"", string_literal.raw),
                Literal::Char(char_literal) => write!(f, "\'{}\'", char_literal),
                Literal::Null => write!(f, "null"),
            },
            Self::Array(data_type, array) => {
                write!(f, "{}", data_type)?;

                for item in array {
                    if let Some(dimension) = item {
                        write!(f, "[{}]", dimension)?;
                    } else {
                        write!(f, "[]")?;
                    }
                }

                write!(f, "")
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

impl Default for Span {
    fn default() -> Self {
        Self { start: Default::default(), end: Default::default() }
    }
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

impl Default for Location {
    fn default() -> Self {
        Self {
            line: Default::default(),
            column: Default::default(),
        }
    }
}
