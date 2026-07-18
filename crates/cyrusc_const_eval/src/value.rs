// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

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

pub fn is_expr_const_evaluable(expr: &TypedExprKind) -> bool {
    match expr {
        TypedExprKind::SemaType { .. } => true,

        TypedExprKind::Literal(_) => true,
        TypedExprKind::Lambda(_) => true,
        TypedExprKind::Infix(infix) => {
            is_expr_const_evaluable(&infix.lhs.kind) && is_expr_const_evaluable(&infix.rhs.kind)
        }
        TypedExprKind::Prefix(prefix) => is_expr_const_evaluable(&prefix.operand.kind),
        TypedExprKind::Unary(unary) => is_expr_const_evaluable(&unary.operand.kind),

        TypedExprKind::StructInit(_)
        | TypedExprKind::UnionInit(_)
        | TypedExprKind::UnnamedStructValue(_)
        | TypedExprKind::UnnamedEnumValue(_)
        | TypedExprKind::EnumStructVariantInit(_)
        | TypedExprKind::EnumInit(_)
        | TypedExprKind::UnnamedUnionValue(_)
        | TypedExprKind::Tuple(_)
        | TypedExprKind::Array(_)
        | TypedExprKind::Symbol(_)
        | TypedExprKind::ArrayIndex(_)
        | TypedExprKind::TupleAccess(_)
        | TypedExprKind::Deref(_)
        | TypedExprKind::FieldAccess(_)
        | TypedExprKind::MethodCall(_)
        | TypedExprKind::FuncCall(_)
        | TypedExprKind::Assign(_)
        | TypedExprKind::Dynamic(_)
        | TypedExprKind::Try(_)
        | TypedExprKind::AddrOf(_) => false,

        // IMPORTANT:
        // if a builtin achieves this point means it has a family other than `TypedBuiltinFamily::ConstEval`
        // and cannot be evaluted in comptime.
        TypedExprKind::Builtin(_) => false,

        TypedExprKind::Poisoned => unreachable!(),
    }
}
