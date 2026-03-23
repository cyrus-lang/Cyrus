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

use cyrusc_typed_ast::exprs::{TypedExprKind, TypedUnnamedEnumValueKind};

#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Int(i128),
    Bool(bool),
    Float(f64),
}

impl ConstValue {
    pub fn as_int(&self) -> Option<i128> {
        match self {
            ConstValue::Int(int_value) => Some(*int_value),
            ConstValue::Bool(bool_value) => Some(if *bool_value { 1 } else { 0 }),
            ConstValue::Float(_) => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ConstValue::Bool(v) => Some(*v),
            ConstValue::Int(v) => Some(*v != 0),
            ConstValue::Float(_) => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            ConstValue::Float(float_value) => Some(*float_value),
            ConstValue::Int(_) => None,
            ConstValue::Bool(_) => None,
        }
    }
}

pub fn is_comptime_valid(expr: &TypedExprKind) -> bool {
    match expr {
        TypedExprKind::Literal(_) => true,
        TypedExprKind::Lambda(_) => true,
        TypedExprKind::Tuple(tuple_value) => tuple_value.elements.iter().any(|expr| is_comptime_valid(&expr.kind)),
        TypedExprKind::Prefix(prefix) => is_comptime_valid(&prefix.operand.kind),
        TypedExprKind::Infix(infix) => is_comptime_valid(&infix.lhs.kind) && is_comptime_valid(&infix.rhs.kind),
        TypedExprKind::Unary(unary) => is_comptime_valid(&unary.operand.kind),
        TypedExprKind::Cast(cast) => is_comptime_valid(&cast.operand.kind),
        TypedExprKind::Array(array) => array.elements.iter().all(|expr| is_comptime_valid(&expr.kind)),
        TypedExprKind::StructInit(struct_init) => struct_init
            .fields
            .iter()
            .all(|field_init| is_comptime_valid(&field_init.value.kind)),
        TypedExprKind::UnnamedStructValue(unnamed_struct_value) => unnamed_struct_value
            .fields
            .iter()
            .all(|field| is_comptime_valid(&field.field_value.kind)),
        TypedExprKind::UnnamedEnumValue(unnamed_enum_value) => match &unnamed_enum_value.kind {
            TypedUnnamedEnumValueKind::Plain => true,
            TypedUnnamedEnumValueKind::Fielded(values) => values.iter().any(|expr| is_comptime_valid(&expr.kind)),
        },
        TypedExprKind::UnnamedUnionValue(unnamed_union_value) => {
            is_comptime_valid(&unnamed_union_value.field_value.kind)
        }
        TypedExprKind::Symbol(..)
        | TypedExprKind::ArrayIndex(_)
        | TypedExprKind::TupleAccess(_)
        | TypedExprKind::Deref(_)
        | TypedExprKind::FieldAccess(_)
        | TypedExprKind::MethodCall(_)
        | TypedExprKind::FuncCall(_)
        | TypedExprKind::Assign(_)
        | TypedExprKind::SizeOf(_)
        | TypedExprKind::Dynamic(_)
        | TypedExprKind::SemanticType(_)
        | TypedExprKind::AddrOf(_) => false,

        // TODO
        TypedExprKind::Builtin(_typed_builtin) => todo!(),
    }
}
