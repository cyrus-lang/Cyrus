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
