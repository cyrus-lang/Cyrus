use crate::ast::*;
use core::fmt;

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

impl fmt::Display for BoolLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl fmt::Display for CharLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(integer) => write!(f, "{}", integer),
            Literal::Bool(bool) => write!(f, "{}", bool),
            Literal::String(string_type) => write!(f, "{}", string_type),
            Literal::Float(float) => write!(f, "{}", float),
            Literal::Char(ch) => write!(f, "{}", ch),
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
            Expression::Prefix(UnaryExpression { operand, operator, .. }) => {
                write!(f, "({}{})", operator.kind, operand)
            }
            Expression::Infix(BinaryExpression {
                operator, left, right, ..
            }) => {
                write!(f, "({} {} {})", left, operator.kind, right)
            }
            Expression::FunctionCall(FunctionCall {
                function_name,
                arguments,
                ..
            }) => {
                write!(f, "{}({})", function_name, format_expressions(arguments))
            }
            Expression::Array(array) => {
                write!(f, "[{}]", array_items_to_string(array.clone()))
            }
            Expression::ArrayIndex(array_index) => {
                let mut dimensions_str = String::new();

                for array in array_index.dimensions.clone() {
                    let arr_str = format!("[{}]", array_items_to_string(array));
                    dimensions_str += arr_str.as_str();
                }

                write!(f, "{}{}", array_index.identifier.name, dimensions_str)
            }
        }
    }
}

fn array_items_to_string(array: Array) -> String {
    let arr_str = array
        .elements
        .iter()
        .map(|c| c.to_string())
        .collect::<Vec<String>>()
        .join(", ");
    arr_str
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
