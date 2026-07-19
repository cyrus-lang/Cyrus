// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOperator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equal,
    NotEqual,
    Or,
    NullCoalesce,
    And,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAndNot,
    ShiftRight,
    ShiftLeft,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrefixOperator {
    Bang,
    Minus,
    BitwiseNot,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
}
