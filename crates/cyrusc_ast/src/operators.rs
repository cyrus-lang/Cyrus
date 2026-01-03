// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#[derive(Debug, Clone, PartialEq)]
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
    And,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAndNot,
    ShiftRight,
    ShiftLeft,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOperator {
    Bang,
    Minus,
    BitwiseNot,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
}
