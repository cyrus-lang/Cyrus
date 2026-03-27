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

use cyrusc_source_loc::Loc;

use crate::literals::ASTLiteralExpr;
use std::fmt;

pub mod literals;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    EOF,
    Invalid,
    Ident(String),
    Literal(ASTLiteralExpr),

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
    At,

    // Keywords
    Module,
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
    Repr,
    Align,
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

    pub fn is_eof(&self) -> bool {
        match self {
            TokenKind::EOF => true,
            _ => false,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::Invalid => write!(f, "INVALID"),
            TokenKind::Ident(ident) => write!(f, "{}", ident),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Assign => write!(f, "="),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::LeftBracket => write!(f, "[["),
            TokenKind::RightBracket => write!(f, "]]"),
            TokenKind::At => write!(f, "@"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::DoubleDot => write!(f, ".."),
            TokenKind::TripleDot => write!(f, "..."),
            TokenKind::Dot => write!(f, "."),
            TokenKind::DoubleQuote => write!(f, "\""),
            TokenKind::SingleQuote => write!(f, "'"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Ampersand => write!(f, "&"),
            TokenKind::LessThan => write!(f, "<"),
            TokenKind::GreaterThan => write!(f, ">"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::And => write!(f, "&&"),
            TokenKind::Or => write!(f, "||"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::ThinArrow => write!(f, "->"),
            TokenKind::FatArrow => write!(f, "=>"),
            TokenKind::Dynamic => write!(f, "dynamic"),
            TokenKind::Var => write!(f, "var"),
            TokenKind::Module => write!(f, "mod"),
            TokenKind::Interface => write!(f, "interface"),
            TokenKind::Typedef => write!(f, "type"),
            TokenKind::Typecast => write!(f, "cast"),
            TokenKind::Function => write!(f, "fn"),
            TokenKind::Switch => write!(f, "switch"),
            TokenKind::Case => write!(f, "case"),
            TokenKind::Default => write!(f, "default"),
            TokenKind::Struct => write!(f, "struct"),
            TokenKind::Import => write!(f, "import"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::For => write!(f, "for"),
            TokenKind::Foreach => write!(f, "foreach"),
            TokenKind::Break => write!(f, "break"),
            TokenKind::Continue => write!(f, "continue"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Null => write!(f, "null"),
            TokenKind::UIntPtr => write!(f, "uintptr"),
            TokenKind::IntPtr => write!(f, "intptr"),
            TokenKind::ISize => write!(f, "isize"),
            TokenKind::USize => write!(f, "usize"),
            TokenKind::Int => write!(f, "int"),
            TokenKind::Int8 => write!(f, "int8"),
            TokenKind::Int16 => write!(f, "int16"),
            TokenKind::Int32 => write!(f, "int32"),
            TokenKind::Int64 => write!(f, "int64"),
            TokenKind::Int128 => write!(f, "int128"),
            TokenKind::UInt => write!(f, "uint"),
            TokenKind::UInt8 => write!(f, "uint8"),
            TokenKind::UInt16 => write!(f, "uint16"),
            TokenKind::UInt32 => write!(f, "uint32"),
            TokenKind::UInt64 => write!(f, "uint64"),
            TokenKind::UInt128 => write!(f, "uint128"),
            TokenKind::Float16 => write!(f, "float16"),
            TokenKind::Float32 => write!(f, "float32"),
            TokenKind::Float64 => write!(f, "float64"),
            TokenKind::Float128 => write!(f, "float128"),
            TokenKind::Char => write!(f, "char"),
            TokenKind::Bool => write!(f, "bool"),
            TokenKind::Void => write!(f, "void"),
            TokenKind::Enum => write!(f, "enum"),
            TokenKind::Macro => write!(f, "macro"),
            TokenKind::In => write!(f, "in"),
            TokenKind::As => write!(f, "as"),
            TokenKind::Extern => write!(f, "extern"),
            TokenKind::Inline => write!(f, "inline"),
            TokenKind::Public => write!(f, "pub"),
            TokenKind::Const => write!(f, "const"),
            TokenKind::SizeOf => write!(f, "sizeof"),
            TokenKind::Weak => write!(f, "weak"),
            TokenKind::LinkOnce => write!(f, "linkonce"),
            TokenKind::Callconv => write!(f, "callconv"),
            TokenKind::Naked => write!(f, "naked"),
            TokenKind::NoReturn => write!(f, "noreturn"),
            TokenKind::Hot => write!(f, "hot"),
            TokenKind::Cold => write!(f, "cold"),
            TokenKind::DllImport => write!(f, "dllimport"),
            TokenKind::DllExport => write!(f, "dllexport"),
            TokenKind::OptSize => write!(f, "optsize"),
            TokenKind::OptNone => write!(f, "optnone"),
            TokenKind::NoSanitize => write!(f, "no_sanitize"),
            TokenKind::NoUnwind => write!(f, "nounwind"),
            TokenKind::Section => write!(f, "section"),
            TokenKind::Repr => write!(f, "repr"),
            TokenKind::Increment => write!(f, "++"),
            TokenKind::Decrement => write!(f, "--"),
            TokenKind::ShiftRight => write!(f, ">>"),
            TokenKind::ShiftLeft => write!(f, "<<"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::AmpTilde => write!(f, "&~"),
            TokenKind::Tilde => write!(f, "~"),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::DoubleColon => write!(f, "\""),
            TokenKind::Goto => write!(f, "goto"),
            TokenKind::Defer => write!(f, "defer"),
            TokenKind::Union => write!(f, "union"),
            TokenKind::While => write!(f, "while"),
            TokenKind::NoInline => write!(f, "noinline"),
            TokenKind::AlwaysInline => write!(f, "alwaysinline"),
            TokenKind::Align => write!(f, "align"),
            TokenKind::Literal(literal) => write!(f, "{}", literal),
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Token {}
