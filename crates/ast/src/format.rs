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

impl fmt::Display for FieldAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".{}", self.identifier.name)
    }
}

impl fmt::Display for ModuleSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleSegment::Wildcard => write!(f, "*"),
            ModuleSegment::SubModule(identifier) => write!(f, "{}", identifier.name),
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
            Expression::FieldAccessOrMethodCall(field_access_or_method_call) => {
                write!(f, "{}", field_access_or_method_call.expr.to_string())?;

                for item in field_access_or_method_call.chains.clone() {
                    match item {
                        either::Either::Left(expr) => expr.to_string(),
                        either::Either::Right(expr) => expr.to_string(),
                    };
                }

                Ok(())
            }
            Expression::Array(array) => {
                write!(f, "[{}]", expression_series_to_string(array.elements.clone()))
            }
            Expression::ArrayIndex(array_index) => {
                write!(
                    f,
                    "{}",
                    Expression::ModuleImport(array_index.module_import.clone()).to_string()
                )?;
                for item in &array_index.dimensions {
                    write!(f, "[{}]", item)?;
                }
                write!(f, "")
            }
            Expression::Assignment(assignment) => write!(
                f,
                "{} = {}",
                Expression::ModuleImport(assignment.module_import.clone()).to_string(),
                assignment.expr
            ),
            Expression::ArrayIndexAssign(array_index_assign) => {
                let array_index = ArrayIndex {
                    module_import: array_index_assign.module_import.clone(),
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
            Expression::CastAs(cast_as) => {
                write!(f, "{}", cast_as)
            }
            Expression::ModuleImport(module_import) => {
                write!(f, "{}", module_import.to_string())
            }
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
        todo!();
        // write!(f, "import (")?;
        // for (idx, module_path) in self.sub_modules.iter().enumerate().clone() {
        //     if idx == self.sub_modules.len() - 1 {
        //         write!(f, "{}", module_path.to_string())?;
        //     } else {
        //         write!(f, "{}.", module_path.to_string())?;
        //     }
        // }
        // write!(f, "import )")?;
        // Ok(())
    }
}
