use crate::*;
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
        match &self.kind {
            LiteralKind::Integer(integer) => write!(f, "{}", integer),
            LiteralKind::Bool(bool) => write!(f, "{}", bool),
            LiteralKind::String(string_type, prefix) => {
                if let Some(prefix) = prefix {
                    match prefix {
                        StringPrefix::C => write!(f, "c")?,
                        StringPrefix::B => write!(f, "b")?,
                    };
                }
                write!(f, "\"{}\"", string_type)
            }
            LiteralKind::Float(float) => write!(f, "{}", float),
            LiteralKind::Char(ch) => write!(f, "{}", ch),
            LiteralKind::Null => write!(f, "null"),
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::PreIncrement => write!(f, "++"),
            UnaryOperator::PreDecrement => write!(f, "--"),
            UnaryOperator::PostIncrement => write!(f, "++"),
            UnaryOperator::PostDecrement => write!(f, "--"),
        }
    }
}

impl fmt::Display for Cast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} as {}", self.expr, self.target_type)
    }
}

impl fmt::Display for FuncCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.operand,
            expression_series_to_string(self.args.clone())
        )
    }
}

impl fmt::Display for ModuleSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleSegment::SubModule(identifier) => write!(f, "{}", identifier.name),
            ModuleSegment::Single(singles) => {
                write!(f, "{{")?;
                for (idx, item) in singles.iter().enumerate() {
                    if let Some(renamed) = &item.renamed {
                        write!(f, "{}: ", renamed)?;
                    }

                    write!(f, "{}", item.identifier)?;

                    if !(singles.len() - 1 == idx) {
                        write!(f, ",")?;
                    }
                }
                write!(f, "}}")
            }
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
            TypeSpecifier::Dereference(type_specifier) => write!(f, "{}*", type_specifier),
            TypeSpecifier::Array(array_type_specifier) => {
                write!(
                    f,
                    "{}[{}]",
                    array_type_specifier.element_type,
                    match &array_type_specifier.size {
                        ArrayCapacity::Fixed(size) => size.to_string(),
                        ArrayCapacity::Dynamic => "".to_string(),
                    }
                )
            }
            TypeSpecifier::UnnamedStruct(unnamed_struct) => {
                write!(f, "struct {{ ")?;
                for (idx, field) in unnamed_struct.fields.iter().enumerate() {
                    write!(f, "{}: {}", field.field_name, field.field_type)?;

                    if idx == unnamed_struct.fields.len() - 1 {
                        write!(f, " ")?;
                    } else {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Display for UnnamedStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {{ ")?;
        for (idx, field) in self.fields.iter().enumerate() {
            write!(f, "{}: {}", field.field_name, field.field_type)?;
            if idx == self.fields.len() - 1 {
                write!(f, ", ",)?;
            }
        }
        write!(f, " }}")
    }
}

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOperator::SizeOf => write!(f, "sizeof"),
            PrefixOperator::Bang => write!(f, "&"),
            PrefixOperator::Minus => write!(f, "-"),
        }
    }
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOperator::Add => write!(f, "+"),
            InfixOperator::Sub => write!(f, "-"),
            InfixOperator::Mul => write!(f, "*"),
            InfixOperator::Div => write!(f, "/"),
            InfixOperator::Rem => write!(f, "%"),
            InfixOperator::Equal => write!(f, "=="),
            InfixOperator::NotEqual => write!(f, "!="),
            InfixOperator::LessThan => write!(f, "<"),
            InfixOperator::GreaterThan => write!(f, ">"),
            InfixOperator::LessEqual => write!(f, "<="),
            InfixOperator::GreaterEqual => write!(f, ">="),
            InfixOperator::And => write!(f, "&&"),
            InfixOperator::Or => write!(f, "||"),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Unary(unary_expr) => {
                write!(f, "{}{}", unary_expr.op.clone(), unary_expr.operand)
            }
            Expression::Identifier(identifier) => write!(f, "{}", identifier.name),
            Expression::Literal(literal) => write!(f, "{}", literal.to_string()),
            Expression::Prefix(prefix_expr) => {
                write!(f, "({}{})", prefix_expr.op.clone(), prefix_expr.operand.clone())
            }
            Expression::Infix(infix_expr) => {
                write!(f, "({} {} {})", infix_expr.lhs, infix_expr.op, infix_expr.rhs)
            }
            Expression::FuncCall(func_call) => {
                write!(f, "{}", func_call)
            }
            Expression::FieldAccess(field_access) => {
                if field_access.is_fat_arrow {
                    write!(f, "{}->{}", field_access.operand, field_access.field_name)
                } else {
                    write!(f, "{}.{}", field_access.operand, field_access.field_name)
                }
            }
            Expression::MethodCall(method_call) => {
                write!(
                    f,
                    "{}.{}({})",
                    method_call.operand,
                    method_call.method_name,
                    expression_series_to_string(method_call.args.clone())
                )
            }
            Expression::Array(array) => {
                write!(f, "[{}]", expression_series_to_string(array.elements.clone()))
            }
            Expression::ArrayIndex(array_index) => {
                write!(f, "{}[{}]", array_index.operand, array_index.index)
            }
            Expression::Assignment(assignment) => {
                write!(f, "{} = {}", assignment.lhs, assignment.rhs)
            }
            Expression::AddressOf(address_of) => write!(f, "&({})", address_of.expr),
            Expression::Dereference(dereference) => write!(f, "(*{})", dereference.expr),
            Expression::StructInit(struct_init) => {
                write!(
                    f,
                    "{} {{",
                    Expression::ModuleImport(struct_init.struct_name.clone()).to_string()
                )?;
                for field in &struct_init.field_inits {
                    write!(f, "{}: {};", field.identifier, field.value)?;
                }
                write!(f, "}}")
            }
            Expression::UnnamedStructValue(unnamed_struct_value) => {
                write!(f, "struct {{ ")?;
                for (idx, field) in unnamed_struct_value.fields.iter().enumerate() {
                    if let Some(field_type) = &field.field_type {
                        write!(f, "{}: {} = {}", field.field_name, field_type, field.field_value)?;
                    } else {
                        write!(f, "{} = {}", field.field_name, field.field_value)?;
                    }

                    if idx == unnamed_struct_value.fields.len() - 1 {
                        write!(f, " ")?;
                    } else {
                        write!(f, ", ")?;
                    }
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

impl fmt::Display for UnnamedStructValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.packed {
            write!(f, "bits")?;
        } else {
            write!(f, "struct")?;
        }
        write!(f, " {{ ")?;
        for field in self.fields.clone() {
            write!(f, "{}", field.field_name.name)?;
            if let Some(field_type) = field.field_type {
                write!(f, ": {}", field_type)?;
            }

            write!(f, " = {}", *field.field_value)?;
        }
        write!(f, " }}")
    }
}

pub fn module_segments_as_string(segments: Vec<ModuleSegment>) -> String {
    let mut format = String::new();

    for (idx, item) in segments.iter().enumerate() {
        match item {
            ModuleSegment::SubModule(identifier) => {
                format.push_str(&identifier.name);
                if idx != segments.len() - 1 {
                    format.push_str("::");
                }
            }
            ModuleSegment::Single(module_segment_singles) => {
                format.push('{');
                for (jdx, single) in module_segment_singles.iter().enumerate() {
                    if let Some(renamed) = &single.renamed {
                        format.push_str(&format!("{}:", renamed.name));
                    }
                    format.push_str(&single.identifier.name);
                    if jdx != module_segment_singles.len() - 1 {
                        format.push(',');
                        format.push(' ');
                    }
                }
                format.push('}');
            }
        }
    }

    format
}

pub fn format_expressions(exprs: &Vec<Expression>) -> String {
    exprs.iter().map(|expr| expr.to_string()).collect()
}

pub fn format_statements(stmts: &Vec<Statement>) -> String {
    stmts.iter().map(|stmt| stmt.to_string()).collect()
}
