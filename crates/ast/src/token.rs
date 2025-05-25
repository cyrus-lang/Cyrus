use crate::ast::Literal;
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
    // Types
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    UInt,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UInt128,
    Float16,
    Float32,
    Float64,
    Float128,
    Char,
    Void,
    String,
    Bool,
    Macro,
    In,
    Enum,
    True,
    False,
    Null,
    As,

    Const,
    ConstOf(Box<TokenKind>),
    AddressOf(Box<TokenKind>),
    Dereference(Box<TokenKind>),

    // DataType, Dimensions
    Array(Box<TokenKind>, Vec<ArrayCapacity>),

    // Object Visibility Keywords
    Extern,
    Public,
    Inline,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayCapacity {
    Static(TokenKind), // token_kind->literal
    Dynamic,
}

pub const PRIMITIVE_TYPES: &[TokenKind] = &[
    TokenKind::Int,
    TokenKind::Int8,
    TokenKind::Int16,
    TokenKind::Int32,
    TokenKind::Int64,
    TokenKind::Int128,
    TokenKind::UInt,
    TokenKind::UInt8,
    TokenKind::UInt16,
    TokenKind::UInt32,
    TokenKind::UInt64,
    TokenKind::UInt128,
    TokenKind::Float16,
    TokenKind::Float32,
    TokenKind::Float64,
    TokenKind::Float128,
    TokenKind::Char,
    TokenKind::Bool,
    TokenKind::Void,
    TokenKind::String,
];

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
            Self::Equal => write!(f, "=="),
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
            Self::Int => write!(f, "int"),
            Self::Int8 => write!(f, "int8"),
            Self::Int16 => write!(f, "int16"),
            Self::Int32 => write!(f, "int32"),
            Self::Int64 => write!(f, "int64"),
            Self::Int128 => write!(f, "int128"),
            Self::UInt => write!(f, "uint"),
            Self::UInt16 => write!(f, "uint16"),
            Self::UInt32 => write!(f, "uint32"),
            Self::UInt64 => write!(f, "uint64"),
            Self::UInt128 => write!(f, "uint128"),
            Self::Float16 => write!(f, "float16"),
            Self::Float32 => write!(f, "float32"),
            Self::Float64 => write!(f, "float64"),
            Self::Float128 => write!(f, "float128"),
            Self::Void => write!(f, "void"),
            Self::Enum => write!(f, "enum"),
            Self::Macro => write!(f, "macro"),
            Self::In => write!(f, "in"),
            Self::As => write!(f, "as"),
            Self::Extern => write!(f, "extern"),
            Self::Inline => write!(f, "inline"),
            Self::Public => write!(f, "public"),
            Self::Const => write!(f, "const"),
            Self::Literal(literal) => match literal {
                Literal::Integer(v) => write!(f, "{}", v),
                Literal::Float(v) => write!(f, "{}", v),
                Literal::String(v) => write!(f, "{}", v),
                Literal::Char(v) => write!(f, "{}", v),
                Literal::Bool(v) => write!(f, "{}", v),
                Literal::Null => write!(f, "null"),
            },
            Self::Array(data_type, array) => {
                write!(f, "{}", data_type)?;

                for item in array {
                    write!(
                        f,
                        "[{}]",
                        match item {
                            ArrayCapacity::Static(token_kind) => token_kind.to_string(),
                            ArrayCapacity::Dynamic => "".to_string(),
                        }
                    )?;
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

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: Default::default(),
            end: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl Default for Location {
    fn default() -> Self {
        Self {
            line: Default::default(),
            column: Default::default(),
        }
    }
}
