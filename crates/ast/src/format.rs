use crate::ast::*;
use core::fmt;

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_statements(&self.exprs))
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
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

impl fmt::Display for Cast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} as {}", self.expr, self.type_token)
    }
}

impl fmt::Display for FuncCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".{}(", self.identifier.name)?;
        self.arguments.iter().map(|s| s).fold(String::new(), |mut acc, s| {
            if !acc.is_empty() {
                acc.push_str(", ");
            }
            acc.push_str(&s.to_string());
            acc
        });
        write!(f, ")")
    }
}

impl fmt::Display for ModuleSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleSegment::SubModule(identifier) => write!(f, "{}", identifier.name),
        }
    }
}

impl fmt::Display for TypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSpecifier::TypeToken(token) => write!(f, "{}", token.kind),
            TypeSpecifier::Identifier(identifier) => write!(f, "{}", identifier),
            TypeSpecifier::ModuleImport(module_import) => write!(f, "{}", module_import),
            TypeSpecifier::Const(type_specifier) => write!(f, "const {}", type_specifier),
            TypeSpecifier::AddressOf(type_specifier) => write!(f, "{}&", type_specifier),
            TypeSpecifier::Dereference(type_specifier) => write!(f, "{}*", type_specifier),
            TypeSpecifier::Array(type_specifier, items) => {
                write!(f, "{}", type_specifier)?;

                for item in items {
                    write!(
                        f,
                        "[{}]",
                        match item {
                            ArrayCapacity::Static(token_kind) => token_kind.to_string(),
                            ArrayCapacity::Dynamic => "".to_string(),
                        }
                    )?;
                }

                write!(f, "")
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::UnaryOperator(unary_operator) => write!(
                f,
                "{}{}",
                Expression::ModuleImport(unary_operator.module_import.clone()).to_string(),
                unary_operator.ty
            ),
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
            Expression::FuncCall(func_call) => {
                write!(
                    f,
                    "{}({})",
                    func_call.identifier.name,
                    expression_series_to_string(func_call.arguments.clone())
                )
            }
            Expression::FieldAccess(field_access) => {
                write!(f, "{}.{}", field_access.operand, field_access.field_name)
            }
            Expression::MethodCall(method_call) => {
                write!(
                    f,
                    "{}.{}({})",
                    method_call.operand,
                    method_call.method_name,
                    expression_series_to_string(method_call.arguments.clone())
                )
            }
            Expression::Array(array) => {
                write!(f, "[{}]", expression_series_to_string(array.elements.clone()))
            }
            Expression::ArrayIndex(array_index) => {
                write!(f, "{}", array_index.expr.to_string())?;
                for item in &array_index.dimensions {
                    write!(f, "[{}]", item)?;
                }
                write!(f, "")
            }
            Expression::Assignment(assignment) => {
                write!(f, "{} = {}", assignment.assign_to.to_string(), assignment.expr)
            }
            Expression::AddressOf(expression) => write!(f, "&({})", expression),
            Expression::Dereference(expression) => write!(f, "(*{})", expression),
            Expression::StructInit(struct_init) => {
                write!(
                    f,
                    "{} {{",
                    Expression::ModuleImport(struct_init.struct_name.clone()).to_string()
                )?;
                for field in &struct_init.field_inits {
                    write!(f, "{}: {};", field.name, field.value)?;
                }
                write!(f, "}}")
            }
            Expression::Cast(cast_as) => {
                write!(f, "{}", cast_as)
            }
            Expression::ModuleImport(module_import) => {
                write!(f, "{}", module_import.to_string())
            }
            Expression::TypeSpecifier(type_specifier) => write!(f, "{}", type_specifier),
        }
    }
}

fn expression_series_to_string(exprs: Vec<Expression>) -> String {
    let str = exprs.iter().map(|c| c.to_string()).collect::<Vec<String>>().join(", ");
    str
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::ProgramTree(program) => write!(f, "{}", program),
            Node::Statement(stmt) => write!(f, "{}", stmt),
            Node::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

impl fmt::Display for ProgramTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_statements(&self.body))
    }
}

impl fmt::Display for ModuleImport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", module_segments_as_string(self.segments.clone()))
    }
}

pub fn module_segments_as_string(segments: Vec<ModuleSegment>) -> String {
    segments
        .iter()
        .map(|p| p.to_string())
        .collect::<Vec<String>>()
        .join(".")
}
