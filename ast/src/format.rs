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
            Literal::Null => write!(f, "null"),
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
                todo!()
            }
            Expression::Assignment(assignment) => write!(f, "{} = {}", assignment.identifier, assignment.expr),
            Expression::ArrayIndexAssign(array_index_assign) => {
                let array_index = ArrayIndex {
                    identifier: array_index_assign.identifier.clone(),
                    dimensions: array_index_assign.dimensions.clone(),
                    span: array_index_assign.span.clone(),
                    loc: array_index_assign.loc.clone(),
                };

                write!(
                    f,
                    "{} = {}",
                    Expression::ArrayIndex(array_index).to_string(),
                    array_index_assign.expr.to_string()
                )
            }
            Expression::AddressOf(expression) => write!(f, "&({})", expression),
            Expression::Dereference(expression) => write!(f, "*({})", expression),
            Expression::StructInit(struct_init) => todo!(),
            Expression::StructFieldAccess(struct_field_access) => todo!(),
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
