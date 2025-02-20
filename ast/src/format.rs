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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntegerLiteral::I8(value) => write!(f, "{}", value),
            IntegerLiteral::I16(value) => write!(f, "{}", value),
            IntegerLiteral::I32(value) => write!(f, "{}", value),
            IntegerLiteral::I64(value) => write!(f, "{}", value),
            IntegerLiteral::I128(value) => write!(f, "{}", value),
            IntegerLiteral::U8(value) => write!(f, "{}", value),
            IntegerLiteral::U16(value) => write!(f, "{}", value),
            IntegerLiteral::U32(value) => write!(f, "{}", value),
            IntegerLiteral::U64(value) => write!(f, "{}", value),
            IntegerLiteral::U128(value) => write!(f, "{}", value),
            IntegerLiteral::SizeT(value) => write!(f, "{}", value),
        }
    }
}

impl fmt::Display for FloatLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FloatLiteral::Float(value) => write!(f, "{}", value),
            FloatLiteral::Double(value) => write!(f, "{}", value),
        }
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
            Literal::String(string_type) => write!(f, "\"{}\"", string_type),
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

impl fmt::Display for CastAs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} as {}", self.expr, self.cast_as)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::UnaryOperator(unop) => write!(f, "{}{}", Expression::FromPackage(unop.identifier.clone()).to_string(), unop.ty),
            Expression::Identifier(identifier) => write!(f, "{}", identifier.name),
            Expression::Literal(literal) => write!(f, "{}", literal.to_string()),
            Expression::Prefix(UnaryExpression { operand, operator, .. }) => {
                write!(f, "({}{})", operator.kind, operand)
            }
            Expression::Infix(BinaryExpression {
                operator, left, right, ..
            }) => {
                write!(f, "({} {} {})", left, operator.kind, right)
            }
            Expression::FieldAccessOrMethodCall(chains) => {
                let mut chains = chains.clone();

                let first = chains.first().unwrap().method_call.clone().unwrap();
                write!(
                    f,
                    "{}({})",
                    Expression::FromPackage(FromPackage {
                        sub_packages: first.func_name.sub_packages,
                        identifier: first.func_name.identifier,
                        span: first.span,
                        loc: first.loc
                    })
                    .to_string(),
                    format_expressions(&first.arguments)
                )?;
                chains.remove(0);

                for item in chains {
                    if let Some(method_call) = &item.method_call {
                        write!(
                            f,
                            ".{}({})",
                            method_call.func_name.identifier.name,
                            format_expressions(&method_call.arguments)
                        )?;
                    }

                    if let Some(field_access) = &item.field_access {
                        write!(f, ".{}", field_access.identifier.name,)?;
                    }
                }
                write!(f, "")
            }
            Expression::Array(array) => {
                write!(f, "[{}]", array_items_to_string(array.clone()))
            }
            Expression::ArrayIndex(array_index) => {
                write!(f, "{}", array_index.identifier)?;
                for item in &array_index.dimensions {
                    write!(f, "[{}]", item)?;
                }
                write!(f, "")
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
            Expression::Dereference(expression) => write!(f, "(*{})", expression),
            Expression::StructInit(struct_init) => {
                write!(f, "{} {{", Expression::FromPackage(struct_init.struct_name.clone()).to_string())?;
                for field in &struct_init.field_inits {
                    write!(f, "{}: {};", field.name, field.value)?;
                }
                write!(f, "}}")
            }
            Expression::StructFieldAccess(struct_field_access) => {
                write!(f, "{}", struct_field_access.expr)?;

                for item in &struct_field_access.chains {
                    if let Some(field_access) = item.field_access.clone() {
                        write!(f, ".{}", field_access.identifier.name)?;
                    }

                    if let Some(method_call) = item.method_call.clone() {
                        write!(
                            f,
                            ".{}(",
                            Expression::FromPackage(FromPackage {
                                sub_packages: method_call.func_name.sub_packages,
                                identifier: method_call.func_name.identifier,
                                span: method_call.span,
                                loc: method_call.loc
                            })
                            .to_string()
                        )?;

                        if method_call.arguments.len() > 0 {
                            for (idx, arg) in method_call.arguments.iter().enumerate() {
                                if idx == method_call.arguments.len() - 1 {
                                    write!(f, "{})", arg)?;
                                } else {
                                    write!(f, "{}, ", arg)?;
                                }
                            }
                        } else {
                            write!(f, ")")?;
                        }
                    }
                }

                write!(f, "")
            }
            Expression::CastAs(cast_as) => {
                write!(f, "{}", cast_as)
            }
            Expression::FromPackage(from_package) => {
                if from_package.sub_packages.len() > 0 {
                    write!(
                        f,
                        "{}::{}",
                        package_path_as_string(from_package.sub_packages.clone()),
                        from_package.identifier.name
                    )
                } else {
                    write!(f, "{}", from_package.identifier.name)
                }
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
