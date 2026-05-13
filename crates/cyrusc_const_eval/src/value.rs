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

use cyrusc_typed_ast::{exprs::TypedExprKind, types::SemaType};

#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Int(i128),
    Bool(bool),
    Float(f64),
    String(String),
    Type(SemaType),
}

impl ConstValue {
    #[inline]
    pub fn as_int(&self) -> Option<i128> {
        match self {
            ConstValue::Int(int_value) => Some(*int_value),
            ConstValue::Bool(bool_value) => Some(if *bool_value { 1 } else { 0 }),
            ConstValue::Float(_) => None,
            ConstValue::String(_) => None,
            ConstValue::Type(_) => None,
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ConstValue::Bool(v) => Some(*v),
            ConstValue::Int(v) => Some(*v != 0),
            ConstValue::Float(_) => None,
            ConstValue::String(_) => None,
            ConstValue::Type(_) => None,
        }
    }

    #[inline]
    pub fn as_float(&self) -> Option<f64> {
        match self {
            ConstValue::Float(float_value) => Some(*float_value),
            ConstValue::Int(_) => None,
            ConstValue::Bool(_) => None,
            ConstValue::String(_) => None,
            ConstValue::Type(_) => None,
        }
    }

    #[inline]
    pub fn as_string(&self) -> Option<&String> {
        match self {
            ConstValue::String(string_value) => Some(string_value),
            ConstValue::Int(_) => None,
            ConstValue::Bool(_) => None,
            ConstValue::Float(_) => None,
            ConstValue::Type(_) => None,
        }
    }

    #[inline]
    pub fn as_type(&self) -> Option<&SemaType> {
        match self {
            ConstValue::Type(ty) => Some(ty),
            ConstValue::String(_) => None,
            ConstValue::Int(_) => None,
            ConstValue::Bool(_) => None,
            ConstValue::Float(_) => None,
        }
    }
}

pub fn is_comptime_valid(expr: &TypedExprKind) -> bool {
    match expr {
        TypedExprKind::Literal(_) => true,
        TypedExprKind::Lambda(_) => true,
        TypedExprKind::Prefix(prefix) => is_comptime_valid(&prefix.operand.kind),
        TypedExprKind::Infix(infix) => is_comptime_valid(&infix.lhs.kind) && is_comptime_valid(&infix.rhs.kind),
        TypedExprKind::Unary(unary) => is_comptime_valid(&unary.operand.kind),
        TypedExprKind::Tuple(_) | TypedExprKind::Array(_) => false,

        TypedExprKind::StructInit(_)
        | TypedExprKind::UnionInit(_)
        | TypedExprKind::UnnamedStructValue(_)
        | TypedExprKind::UnnamedEnumValue(_)
        | TypedExprKind::EnumStructVariantInit(_)
        | TypedExprKind::EnumInit(_)
        | TypedExprKind::UnnamedUnionValue(_)
        | TypedExprKind::Symbol(_)
        | TypedExprKind::ArrayIndex(_)
        | TypedExprKind::TupleAccess(_)
        | TypedExprKind::Deref(_)
        | TypedExprKind::FieldAccess(_)
        | TypedExprKind::MethodCall(_)
        | TypedExprKind::FuncCall(_)
        | TypedExprKind::Assign(_)
        | TypedExprKind::Dynamic(_)
        | TypedExprKind::AddrOf(_)
        | TypedExprKind::SemaType { .. } => false,

        TypedExprKind::Builtin(_) => false,

        TypedExprKind::Poisoned => unreachable!(),
    }
}
