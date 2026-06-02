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

use crate::TokenKind;
use cyrusc_source_loc::Loc;
use std::fmt;

pub trait Integer {
    fn is_signed() -> bool;
    fn size_of() -> usize;
    fn to_i128(&self) -> i128;
    fn to_u128(&self) -> u128;
    fn from_i128(val: i128) -> Self;
    fn from_u128(val: u128) -> Self;
}

macro_rules! impl_integer {
    ($($t:ty => $signed:expr),* $(,)?) => {
        $(
            impl Integer for $t {
                #[inline]
                fn is_signed() -> bool {
                    $signed
                }

                #[inline]
                fn size_of() -> usize {
                    std::mem::size_of::<Self>()
                }

                #[inline]
                fn to_i128(&self) -> i128 {
                    *self as i128
                }

                #[inline]
                fn to_u128(&self) -> u128 {
                    *self as u128
                }

                #[inline]
                fn from_i128(val: i128) -> Self { val as $t }

                #[inline]
                fn from_u128(val: u128) -> Self { val as $t }
            }
        )*
    };
}

impl_integer!(
    i8    => true,
    i16   => true,
    i32   => true,
    i64   => true,
    i128  => true,
    isize => true,
    u8    => false,
    u16   => false,
    u32   => false,
    u64   => false,
    u128  => false,
    usize => false,
);

#[derive(Debug, Clone)]
pub struct ASTLiteralExpr {
    pub kind: LiteralKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Integer(IntLiteralKind, Option<Box<TokenKind>>),
    Float(f64, Option<Box<TokenKind>>),
    String(String, Option<StringPrefix>),
    Bool(bool),
    Char(char),
    Null,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntLiteralKind {
    Signed(i128),
    Unsigned(u128),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPrefix {
    C, // C-style string which is const char*
    B, // Bytes string
}

impl IntLiteralKind {
    pub fn as_int<T: Integer>(&self) -> T {
        match *self {
            IntLiteralKind::Signed(value) => T::from_i128(value),
            IntLiteralKind::Unsigned(value) => T::from_u128(value),
        }
    }
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            LiteralKind::Integer(integer, integer_type_opt) => {
                write!(f, "{}", integer)?;
                if let Some(integer_type) = integer_type_opt {
                    write!(f, "{}", (**integer_type).to_string())?
                }
                write!(f, "")
            }
            LiteralKind::Bool(bool) => write!(f, "{}", bool),
            LiteralKind::String(string_type, prefix) => {
                if let Some(prefix) = prefix {
                    match prefix {
                        StringPrefix::C => write!(f, "c")?,
                        StringPrefix::B => write!(f, "b")?,
                    };
                }
                write!(f, "\"{}\"", string_type)
            }
            LiteralKind::Float(float, float_type_opt) => {
                write!(f, "{}", float)?;
                if let Some(float_type) = float_type_opt {
                    write!(f, "{}", (**float_type).to_string())?
                }
                write!(f, "")
            }
            LiteralKind::Char(ch) => write!(f, "{}", ch),
            LiteralKind::Null => write!(f, "null"),
        }
    }
}

impl fmt::Display for ASTLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Display for IntLiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntLiteralKind::Signed(value) => value.fmt(f),
            IntLiteralKind::Unsigned(value) => value.fmt(f),
        }
    }
}

impl PartialEq for ASTLiteralExpr {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for ASTLiteralExpr {}
