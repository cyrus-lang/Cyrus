/*
 * Copyright (c) 2026 The Cyrus Language
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
 */
use std::fmt;

use crate::{
    literals::Literal,
    loc::{Location, Span},
};

pub mod literals;
pub mod loc;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    EOF,
    Ident(String),
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
    ShiftRight,
    ShiftLeft,
    Caret,
    AmpTilde,
    Tilde,
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
    ThinArrow,
    FatArrow,
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
    Var,
    Dynamic,
    Goto,
    Defer,
    Union,
    Interface,
    Function,
    Typedef,
    Typecast,
    Switch,
    Case,
    Default,
    If,
    Else,
    Return,
    For,
    While,
    Foreach,
    Break,
    Continue,
    Struct,
    Bits,
    Import,
    SizeOf,
    Macro,
    In,
    Enum,
    True,
    False,
    Null,
    As,

    // Types
    UIntPtr,
    IntPtr,
    ISize,
    USize,
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
    Bool,
    Const,

    // Modifiers
    Weak,
    LinkOnce,
    Callconv,
    Naked,
    NoReturn,
    Hot,
    Cold,
    Extern,
    Public,
    Inline,
    NoInline,
    AlwaysInline,
    DllImport,
    DllExport,
    OptSize,
    OptNone,
    NoSanitize,
    NoUnwind,
    Section,
}

pub const PRIMITIVE_TYPES: &[TokenKind] = &[
    // signed integers
    TokenKind::Int,
    TokenKind::Int8,
    TokenKind::Int16,
    TokenKind::Int32,
    TokenKind::Int64,
    TokenKind::Int128,
    TokenKind::IntPtr,
    TokenKind::ISize,
    // unsigned integers
    TokenKind::UInt,
    TokenKind::UInt8,
    TokenKind::UInt16,
    TokenKind::UInt32,
    TokenKind::UInt64,
    TokenKind::UInt128,
    TokenKind::UIntPtr,
    TokenKind::USize,
    // floats
    TokenKind::Float16,
    TokenKind::Float32,
    TokenKind::Float64,
    TokenKind::Float128,
    TokenKind::Char,
    TokenKind::Bool,
    TokenKind::Void,
];

impl TokenKind {
    pub fn is_unsigned(&self) -> bool {
        match self {
            TokenKind::UInt
            | TokenKind::UInt8
            | TokenKind::UInt16
            | TokenKind::UInt32
            | TokenKind::UInt64
            | TokenKind::UInt128
            | TokenKind::UIntPtr
            | TokenKind::USize => true,
            _ => false,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EOF => write!(f, "EOF"),
            Self::Ident(ident) => write!(f, "{}", ident),
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
            Self::ThinArrow => write!(f, "->"),
            Self::FatArrow => write!(f, "=>"),
            Self::Dynamic => write!(f, "dynamic"),
            Self::Var => write!(f, "var"),
            Self::Interface => write!(f, "interface"),
            Self::Typedef => write!(f, "type"),
            Self::Typecast => write!(f, "cast"),
            Self::Function => write!(f, "fn"),
            Self::Switch => write!(f, "switch"),
            Self::Case => write!(f, "case"),
            Self::Default => write!(f, "default"),
            Self::Struct => write!(f, "struct"),
            Self::Bits => write!(f, "bits"),
            Self::Import => write!(f, "import"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Return => write!(f, "return"),
            Self::For => write!(f, "for"),
            Self::Foreach => write!(f, "foreach"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Null => write!(f, "null"),
            Self::UIntPtr => write!(f, "uintptr"),
            Self::IntPtr => write!(f, "intptr"),
            Self::ISize => write!(f, "isize"),
            Self::USize => write!(f, "usize"),
            Self::Int => write!(f, "int"),
            Self::Int8 => write!(f, "int8"),
            Self::Int16 => write!(f, "int16"),
            Self::Int32 => write!(f, "int32"),
            Self::Int64 => write!(f, "int64"),
            Self::Int128 => write!(f, "int128"),
            Self::UInt => write!(f, "uint"),
            Self::UInt8 => write!(f, "uint8"),
            Self::UInt16 => write!(f, "uint16"),
            Self::UInt32 => write!(f, "uint32"),
            Self::UInt64 => write!(f, "uint64"),
            Self::UInt128 => write!(f, "uint128"),
            Self::Float16 => write!(f, "float16"),
            Self::Float32 => write!(f, "float32"),
            Self::Float64 => write!(f, "float64"),
            Self::Float128 => write!(f, "float128"),
            Self::Char => write!(f, "char"),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Enum => write!(f, "enum"),
            Self::Macro => write!(f, "macro"),
            Self::In => write!(f, "in"),
            Self::As => write!(f, "as"),
            Self::Extern => write!(f, "extern"),
            Self::Inline => write!(f, "inline"),
            Self::Public => write!(f, "pub"),
            Self::Const => write!(f, "const"),
            Self::SizeOf => write!(f, "sizeof"),
            Self::Weak => write!(f, "weak"),
            Self::LinkOnce => write!(f, "linkonce"),
            Self::Callconv => write!(f, "callconv"),
            Self::Naked => write!(f, "naked"),
            Self::NoReturn => write!(f, "noreturn"),
            Self::Hot => write!(f, "hot"),
            Self::Cold => write!(f, "cold"),
            Self::DllImport => write!(f, "dllimport"),
            Self::DllExport => write!(f, "dllexport"),
            Self::OptSize => write!(f, "optsize"),
            Self::OptNone => write!(f, "optnone"),
            Self::NoSanitize => write!(f, "no_sanitize"),
            Self::NoUnwind => write!(f, "nounwind"),
            Self::Section => write!(f, "section"),
            Self::Increment => write!(f, "++"),
            Self::Decrement => write!(f, "--"),
            Self::ShiftRight => write!(f, ">>"),
            Self::ShiftLeft => write!(f, "<<"),
            Self::Caret => write!(f, "^"),
            Self::AmpTilde => write!(f, "&~"),
            Self::Tilde => write!(f, "~"),
            Self::NotEqual => write!(f, "!="),
            Self::Bang => write!(f, "!"),
            Self::DoubleColon => write!(f, "\""),
            Self::Goto => write!(f, "goto"),
            Self::Defer => write!(f, "defer"),
            Self::Union => write!(f, "union"),
            Self::While => write!(f, "while"),
            Self::NoInline => write!(f, "noinline"),
            Self::AlwaysInline => write!(f, "alwaysinline"),
            Self::Literal(literal) => write!(f, "{}", literal),
        }
    }
}
