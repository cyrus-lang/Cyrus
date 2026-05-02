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

#[derive(Debug, Clone)]
pub struct ASTLiteralExpr {
    pub kind: LiteralKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Integer(i128, Option<Box<TokenKind>>),
    Float(f64, Option<Box<TokenKind>>),
    String(String, Option<StringPrefix>),
    Bool(bool),
    Char(char),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPrefix {
    C, // C-style string which is const char*
    B, // Bytes string
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

impl PartialEq for ASTLiteralExpr {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for ASTLiteralExpr {}
