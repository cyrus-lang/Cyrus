use core::fmt;
use token::{Span, Token};

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Prefix(UnaryExpression),
    Infix(BinaryExpression),
    FunctionCall(FunctionCall),
    UnaryOperator(UnaryOperator),
}

#[derive(Debug, Clone)]
pub enum UnaryOperatorType {
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
}

#[derive(Debug, Clone)]
pub struct UnaryOperator {
    pub identifer: Identifier,
    pub ty: UnaryOperatorType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub call: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(Integer),
    Boolean(Boolean),
    String(StringType),
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: Token,
    pub operand: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub operator: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub raw: i64,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub raw: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StringType {
    pub raw: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub elements: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Hash {
    pub pairs: Vec<(Expression, Expression)>,
    pub span: Span,
}

pub fn format_expressions(exprs: &Vec<Expression>) -> String {
    exprs.iter().map(|expr| expr.to_string()).collect()
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl fmt::Display for StringType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(integer) => write!(f, "{}", integer),
            Literal::Boolean(boolean) => write!(f, "{}", boolean),
            Literal::String(string_type) => write!(f, "{}", string_type),
        }
    }
}

impl fmt::Display for UnaryOperatorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperatorType::PreIncrement => write!(f, "++"),
            UnaryOperatorType::PreDecrement => write!(f, "--"),
            UnaryOperatorType::PostIncrement => write!(f, "++"),
            UnaryOperatorType::PostDecrement => write!(f, "--"),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::UnaryOperator(unop) => write!(f, "{}{}", unop.identifer, unop.ty),
            Expression::Identifier(identifier) => write!(f, "{}", identifier.name),
            Expression::Literal(literal) => write!(f, "{}", literal),
            Expression::Prefix(UnaryExpression {
                operand, operator, ..
            }) => {
                write!(f, "({}{})", operator.kind, operand)
            }
            Expression::Infix(BinaryExpression {
                operator,
                left,
                right,
                ..
            }) => {
                write!(f, "({} {} {})", left, operator.kind, right)
            }
            Expression::FunctionCall(FunctionCall {
                call, arguments, ..
            }) => {
                write!(f, "{}({})", call, format_expressions(arguments))
            }
        }
    }
}
