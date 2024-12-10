use core::fmt;
use crate::ast::*;

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_statements(&self.body))
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for FloatLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
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
            Literal::Float(float) => write!(f, "{}", float),
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

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Program(program) => write!(f, "{}", program),
            Node::Statement(stmt) => write!(f, "{}", stmt),
            Node::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_statements(&self.body))
    }
}