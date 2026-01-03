// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                                                         │
// │  Cyrus Programming Language                                             │
// │  https://github.com/cyrus-lang/Cyrus                                    │
// │                                                                         │
// │  A general-purpose, statically-typed, manually memory-managed           │
// │  programming language designed for performance-critical applications.   │
// │                                                                         │
// │  Copyright (c) 2026 The Cyrus Programming Language Project              │
// │                                                                         │
// │  This program is free software: you can redistribute it and/or modify   │
// │  it under the terms of the GNU General Public License as published by   │
// │  the Free Software Foundation, either version 3 of the License, or      │
// │  (at your option) any later version.                                    │
// │                                                                         │
// │  This program is distributed in the hope that it will be useful,        │
// │  but WITHOUT ANY WARRANTY; without even the implied warranty of         │
// │  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the           │
// │  GNU General Public License for more details.                           │
// │                                                                         │
// │  You should have received a copy of the GNU General Public License      │
// │  along with this program. If not, see <https://www.gnu.org/licenses/>.  │
// │                                                                         │
// └─────────────────────────────────────────────────────────────────────────┘

use crate::*;
use core::fmt;

impl fmt::Display for BlockStmt {
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
            LiteralKind::Integer(integer, integer_type_opt) => {
                write!(f, "{}", integer)?;
                if let Some(integer_type) = integer_type_opt {
                    write!(f, "{}", (**integer_type).to_string())?
                }
                write!(f, "")
            }
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
            LiteralKind::Float(float, float_type_opt) => {
                write!(f, "{}", float)?;
                if let Some(float_type) = float_type_opt {
                    write!(f, "{}", (**float_type).to_string())?
                }
                write!(f, "")
            }
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
            TypeSpecifier::FuncType(func_type) => {
                let mut params = func_type
                    .params
                    .list
                    .iter()
                    .map(|param| param.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                if let Some(variadic) = &func_type.params.variadic {
                    match variadic {
                        FuncTypeVariadicParams::UntypedCStyle => params.push_str(", ..."),
                        FuncTypeVariadicParams::Typed(type_specifier) => {
                            params.push_str(&format!(", {}...", type_specifier.to_string()))
                        }
                    }
                }

                write!(f, "fn ({}) {}", params, func_type.return_type.to_string())
            }
            TypeSpecifier::TypeToken(token) => write!(f, "{}", token.kind),
            TypeSpecifier::Identifier(identifier) => write!(f, "{}", identifier),
            TypeSpecifier::ModuleImport(module_import) => write!(f, "{}", module_import),
            TypeSpecifier::Const(type_specifier) => write!(f, "const {}", type_specifier),
            TypeSpecifier::Deref(type_specifier) => write!(f, "{}*", type_specifier),
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
                    write!(f, "{}: {}", field.field_name, field.field_ty)?;

                    if idx == unnamed_struct.fields.len() - 1 {
                        write!(f, " ")?;
                    } else {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
            TypeSpecifier::Tuple(tuple_type) => {
                write!(
                    f,
                    "({})",
                    tuple_type
                        .type_list
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TypeSpecifier::GenericInst(generic_inst) => {
                let type_args = generic_inst
                    .type_args
                    .iter()
                    .map(|type_arg| match type_arg {
                        TypeArg::Positional(type_specifier) => type_specifier.to_string(),
                        TypeArg::Named { ty, .. } => ty.to_string(),
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "{}<{}>", generic_inst.base, type_args)
            }
            TypeSpecifier::SelfType(_) => {
                write!(f, "Self")
            }
        }
    }
}

impl fmt::Display for UnnamedStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {{ ")?;
        for (idx, field) in self.fields.iter().enumerate() {
            write!(f, "{}: {}", field.field_name, field.field_ty)?;
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
            PrefixOperator::Bang => write!(f, "&"),
            PrefixOperator::Minus => write!(f, "-"),
            PrefixOperator::BitwiseNot => write!(f, "~"),
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
            InfixOperator::BitwiseAnd => write!(f, "&"),
            InfixOperator::BitwiseOr => write!(f, "|"),
            InfixOperator::BitwiseXor => write!(f, "~"),
            InfixOperator::BitwiseAndNot => write!(f, "&~"),
            InfixOperator::ShiftLeft => write!(f, "<<"),
            InfixOperator::ShiftRight => write!(f, ">>"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Lambda(lambda) => {
                write!(f, "fn(")?;
                lambda.params.list.iter().for_each(|param| match param {
                    FuncParamKind::FuncParam(func_param) => {
                        write!(
                            f,
                            "{}: {}",
                            func_param.identifier.as_string(),
                            func_param.ty.clone().unwrap()
                        )
                        .unwrap();
                    }
                    FuncParamKind::SelfModifier(..) => unreachable!(),
                });
                write!(f, ") {}", lambda.return_type.clone())?;
                write!(f, "{{ {} }}", format_statements(&lambda.body.exprs))
            }
            Expr::Unary(unary_expr) => {
                write!(f, "{}{}", unary_expr.op.clone(), unary_expr.operand)
            }
            Expr::Identifier(identifier) => write!(f, "{}", identifier.name),
            Expr::Literal(literal) => write!(f, "{}", literal.to_string()),
            Expr::Prefix(prefix_expr) => {
                write!(f, "({}{})", prefix_expr.op.clone(), prefix_expr.operand.clone())
            }
            Expr::Infix(infix_expr) => {
                write!(f, "({} {} {})", infix_expr.lhs, infix_expr.op, infix_expr.rhs)
            }
            Expr::FuncCall(func_call) => {
                write!(f, "{}", func_call)
            }
            Expr::FieldAccess(field_access) => {
                if field_access.is_fat_arrow {
                    write!(f, "{}->{}", field_access.operand, field_access.field_name)
                } else {
                    write!(f, "{}.{}", field_access.operand, field_access.field_name)
                }
            }
            Expr::MethodCall(method_call) => {
                write!(
                    f,
                    "{}.{}({})",
                    method_call.operand,
                    method_call.method_name,
                    expression_series_to_string(method_call.args.clone())
                )
            }
            Expr::Array(array) => {
                write!(f, "[{}]", expression_series_to_string(array.elements.clone()))
            }
            Expr::ArrayIndex(array_index) => {
                write!(f, "{}[{}]", array_index.operand, array_index.index)
            }
            Expr::Assign(assignment) => {
                write!(f, "{} = {}", assignment.lhs, assignment.rhs)
            }
            Expr::AddrOf(address_of) => write!(f, "&({})", address_of.expr),
            Expr::Deref(dereference) => write!(f, "(*{})", dereference.expr),
            Expr::StructInit(struct_init) => {
                write!(
                    f,
                    "{} {{",
                    Expr::ModuleImport(struct_init.struct_name.clone()).to_string()
                )?;
                for field in &struct_init.field_inits {
                    write!(f, "{}: {};", field.identifier, field.value)?;
                }
                write!(f, "}}")
            }
            Expr::UStructValue(unnamed_struct_value) => {
                write!(f, "struct {{ ")?;
                for (idx, field) in unnamed_struct_value.fields.iter().enumerate() {
                    if let Some(field_ty) = &field.field_ty {
                        write!(f, "{}: {} = {}", field.field_name, field_ty, field.field_value)?;
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
            Expr::Cast(cast_as) => {
                write!(f, "{}", cast_as)
            }
            Expr::ModuleImport(module_import) => {
                write!(f, "{}", module_import.to_string())
            }
            Expr::TypeSpecifier(type_specifier) => write!(f, "{}", type_specifier),
            Expr::SizeOf(size_of_expression) => {
                write!(f, "sizeof {}", size_of_expression.expr)
            }
            Expr::Tuple(tuple_value) => {
                write!(
                    f,
                    "({})",
                    tuple_value
                        .expr_list
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::TupleAccess(tuple_member_access) => {
                write!(f, "{}.{}", tuple_member_access.operand, tuple_member_access.index)
            }
        }
    }
}

fn expression_series_to_string(exprs: Vec<Expr>) -> String {
    let str = exprs.iter().map(|c| c.to_string()).collect::<Vec<String>>().join(", ");
    str
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
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

impl fmt::Display for UStructValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_packed {
            write!(f, "bits")?;
        } else {
            write!(f, "struct")?;
        }
        write!(f, " {{ ")?;
        for field in self.fields.clone() {
            write!(f, "{}", field.field_name.name)?;
            if let Some(field_ty) = field.field_ty {
                write!(f, ": {}", field_ty)?;
            }

            write!(f, " = {}", *field.field_value)?;
        }
        write!(f, " }}")
    }
}

pub fn module_segments_as_string(segments: Vec<ModuleSegment>) -> String {
    let mut out = String::new();

    for (idx, item) in segments.iter().enumerate() {
        match item {
            ModuleSegment::SubModule(identifier) => {
                out.push_str(&identifier.name);
                // add '::' only if the next segment exists and is a SubModule
                if matches!(segments.get(idx + 1), Some(ModuleSegment::SubModule(_))) {
                    out.push_str("::");
                }
            }
            ModuleSegment::Single(module_segment_singles) => {
                out.push('{');
                for (jdx, single) in module_segment_singles.iter().enumerate() {
                    if let Some(renamed) = &single.renamed {
                        out.push_str(&format!("{}:", renamed.name));
                    }
                    out.push_str(&single.identifier.name);
                    if jdx != module_segment_singles.len() - 1 {
                        out.push_str(", ");
                    }
                }
                out.push('}');
            }
        }
    }

    out
}

pub fn format_expressions(exprs: &Vec<Expr>) -> String {
    exprs.iter().map(|expr| expr.to_string()).collect()
}

pub fn format_statements(stmts: &Vec<Stmt>) -> String {
    stmts.iter().map(|stmt| stmt.to_string()).collect()
}
