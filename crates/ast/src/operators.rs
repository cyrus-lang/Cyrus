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
}

#[derive(Debug, Clone)]
pub enum PrefixOperator {
    SizeOf,
    Bang,
    Minus,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
}
