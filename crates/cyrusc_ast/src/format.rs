/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

use crate::*;
use core::fmt;

fn format_expr_series(exprs: &[ASTExpr]) -> String {
    exprs.iter().map(|expr| expr.to_string()).collect::<Vec<_>>().join(", ")
}

fn format_stmts(stmts: &[ASTStmt]) -> String {
    stmts.iter().map(|stmt| stmt.to_string()).collect()
}

pub fn format_sub_modules(sub_modules: &[Ident]) -> String {
    let mut out = String::new();

    for (i, ident) in sub_modules.iter().enumerate() {
        out.push_str(&ident.value);
        // add '::' only if the next segment exists and is a SubModule
        if matches!(sub_modules.get(i + 1), Some(_)) {
            out.push_str("::");
        }
    }

    out
}

pub fn format_module_segments(segments: &[ModuleSegment]) -> String {
    let mut out = String::new();

    for (i, item) in segments.iter().enumerate() {
        match item {
            ModuleSegment::SubModule(ident) => {
                out.push_str(&ident.value);
                // add '::' only if the next segment exists and is a SubModule
                if matches!(segments.get(i + 1), Some(ModuleSegment::SubModule(_))) {
                    out.push_str("::");
                }
            }
            ModuleSegment::Single(module_segment_singles) => {
                out.push('{');
                for (jdx, single) in module_segment_singles.iter().enumerate() {
                    if let Some(renamed) = &single.renamed {
                        out.push_str(&format!("{}:", renamed.value));
                    }
                    out.push_str(&single.ident.value);
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

impl fmt::Display for ASTBlockStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_stmts(&self.stmts))
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::PreIncrement | UnaryOperator::PostIncrement => write!(f, "++"),
            UnaryOperator::PreDecrement | UnaryOperator::PostDecrement => write!(f, "--"),
        }
    }
}

impl fmt::Display for ASTFuncCallExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.operand, format_expr_series(&self.args))
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Builtin::BuiltinFunc(builtin_func) => write!(f, "{}", builtin_func),
            Builtin::BuiltinScope(builtin_scope) => write!(f, "{}", builtin_scope),
        }
    }
}

impl fmt::Display for BuiltinFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}({})", self.name, format_expr_series(&self.args))
    }
}

impl fmt::Display for BuiltinScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "@{}({}) {{ {} }}",
            self.name,
            format_expr_series(&self.args),
            self.block
        )
    }
}

impl fmt::Display for ModuleSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_module_segments(&[self.clone()]))
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
                        FuncTypeVariadicParams::Typed(type_spec) => {
                            params.push_str(&format!(", {}...", type_spec.to_string()))
                        }
                    }
                }

                write!(f, "fn ({}) {}", params, func_type.ret_type.to_string())
            }
            TypeSpecifier::TypeToken(token) => write!(f, "{}", token.kind),
            TypeSpecifier::Ident(ident) => write!(f, "{}", ident),
            TypeSpecifier::ModuleImport(module_import) => write!(f, "{}", module_import),
            TypeSpecifier::Const(type_spec) => write!(f, "const {}", type_spec),
            TypeSpecifier::Deref(type_spec) => write!(f, "{}*", type_spec),
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
            TypeSpecifier::UnnamedStruct(unnamed_struct) => write!(f, "{}", unnamed_struct),
            TypeSpecifier::UnnamedUnion(unnamed_union) => write!(f, "{}", unnamed_union),
            TypeSpecifier::UnnamedEnum(unnamed_enum) => write!(f, "{}", unnamed_enum),
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
                        TypeArg::Positional(type_spec) => type_spec.to_string(),
                        TypeArg::Named { ty, .. } => ty.to_string(),
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "{}<{}>", generic_inst.base, type_args)
            }
            TypeSpecifier::SelfType(_) => write!(f, "Self"),
        }
    }
}

impl fmt::Display for UnnamedUnionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "union {{ ")?;
        for (i, field) in self.fields.iter().enumerate() {
            write!(f, "{} = {}", field.field_name, field.field_ty)?;
            if i == self.fields.len() - 1 {
                write!(f, " ")?;
            } else {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

impl fmt::Display for EnumVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EnumVariant::Unit(ident) => {
                write!(f, "{}", ident)
            }
            EnumVariant::Valued { ident, value } => {
                write!(f, "{}({})", ident, value)
            }
            EnumVariant::Tuple { ident, fields } => {
                write!(f, "{}(", ident)?;

                for (i, field_type) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", field_type)?;
                }

                write!(f, ")")
            }
            EnumVariant::Struct { ident, fields } => {
                write!(f, "{} {{", ident)?; // Start with identifier and opening brace

                for (i, field_info) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}: {}", field_info.name, field_info.ty)?;
                }

                write!(f, " }}")
            }
        }
    }
}

impl fmt::Display for UnnamedEnumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "enum {{ ")?;

        for (i, variant) in self.variants.iter().enumerate() {
            write!(f, "{}", variant)?;

            if i == self.variants.len() - 1 {
                write!(f, " ")?;
            } else {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

impl fmt::Display for UnnamedStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {{ ")?;
        for (i, field) in self.fields.iter().enumerate() {
            write!(f, "{}: {}", field.name, field.ty)?;
            if i == self.fields.len() - 1 {
                write!(f, " ")?;
            } else {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
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

impl fmt::Display for ASTExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTExpr::Lambda(lambda) => {
                write!(f, "fn(")?;
                lambda.params.list.iter().for_each(|param| match param {
                    FuncParamKind::FuncParam(func_param) => {
                        write!(
                            f,
                            "{}: {}",
                            func_param.ident.as_string(),
                            func_param.ty.clone().unwrap()
                        )
                        .unwrap();
                    }
                    FuncParamKind::SelfModifier(..) => unreachable!(),
                });
                write!(f, ") {}", lambda.ret_type.clone())?;
                write!(f, "{{ {} }}", format_stmts(&lambda.body.stmts))
            }
            ASTExpr::Unary(unary_expr) => {
                write!(f, "{}{}", unary_expr.op.clone(), unary_expr.operand)
            }
            ASTExpr::Ident(ident) => write!(f, "{}", ident.value),
            ASTExpr::Literal(literal) => write!(f, "{}", literal.to_string()),
            ASTExpr::Prefix(prefix_expr) => {
                write!(f, "({}{})", prefix_expr.op.clone(), prefix_expr.operand.clone())
            }
            ASTExpr::Infix(infix_expr) => {
                write!(f, "({} {} {})", infix_expr.lhs, infix_expr.op, infix_expr.rhs)
            }
            ASTExpr::FuncCall(func_call) => write!(f, "{}", func_call.to_string()),
            ASTExpr::Builtin(builtin) => write!(f, "{}", builtin.to_string()),
            ASTExpr::FieldAccess(field_access) => {
                if field_access.is_fat_arrow {
                    write!(f, "{}->{}", field_access.operand, field_access.field_name)
                } else {
                    write!(f, "{}.{}", field_access.operand, field_access.field_name)
                }
            }
            ASTExpr::MethodCall(method_call) => {
                write!(
                    f,
                    "{}.{}({})",
                    method_call.operand,
                    method_call.method_name,
                    format_expr_series(&method_call.args)
                )
            }
            ASTExpr::Array(array) => write!(f, "[{}]", format_expr_series(&array.elements)),
            ASTExpr::UntypedArray(untyped_array) => write!(f, "[{}]", format_expr_series(&untyped_array.elements)),
            ASTExpr::ArrayIndex(array_index) => write!(f, "{}[{}]", array_index.operand, array_index.index),
            ASTExpr::Assign(assignment) => write!(f, "{} = {}", assignment.lhs, assignment.rhs),
            ASTExpr::AddrOf(address_of) => write!(f, "&({})", address_of.expr),
            ASTExpr::Deref(dereference) => write!(f, "(*{})", dereference.expr),
            ASTExpr::StructInit(struct_init) => {
                write!(
                    f,
                    "{} {{",
                    ASTExpr::ModuleImport(struct_init.struct_name.clone()).to_string()
                )?;
                for field in &struct_init.field_inits {
                    write!(f, "{}: {};", field.ident, field.value)?;
                }
                write!(f, "}}")
            }
            ASTExpr::UnnamedStructValue(unnamed_struct_value) => write!(f, "{}", unnamed_struct_value),
            ASTExpr::ModuleImport(module_import) => write!(f, "{}", module_import),
            ASTExpr::TypeSpecifier(type_spec) => write!(f, "{}", type_spec),
            ASTExpr::Tuple(tuple_value) => {
                write!(
                    f,
                    "({})",
                    tuple_value
                        .elements
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            ASTExpr::TupleAccess(tuple_member_access) => {
                write!(f, "{}.{}", tuple_member_access.operand, tuple_member_access.index)
            }
            ASTExpr::Dynamic(dynamic) => write!(f, "dynamic {}", dynamic.operand),
            ASTExpr::UnnamedEnumValue(unnamed_enum_value) => {
                write!(f, ".{}", unnamed_enum_value.ident.as_string())?;

                match &unnamed_enum_value.kind {
                    UnnamedEnumValueKind::Plain => todo!(),
                    UnnamedEnumValueKind::Fielded(exprs) => {
                        write!(f, "({})", format_expr_series(exprs))?;
                    }
                }

                Ok(())
            }
            ASTExpr::UnnamedUnionValue(unnamed_union_value) => {
                if unnamed_union_value.is_const {
                    write!(f, "const ")?;
                }
                write!(f, "union {{ {}", unnamed_union_value.field_name.as_string())?;
                write!(f, " = {} }}", unnamed_union_value.field_value)?;
                Ok(())
            }
        }
    }
}

impl fmt::Display for ASTStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for ProgramTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_stmts(&self.body))
    }
}

impl fmt::Display for ASTModuleImport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_module_segments(&self.segments))
    }
}

impl fmt::Display for ASTUnnamedStructValueExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(repr_attr) = &self.repr_attr {
            write!(f, "{} ", repr_attr)?;
        }

        write!(f, "struct")?;

        if let Some(align) = self.align {
            write!(f, " align({})", align)?;
        }

        write!(f, " {{ ")?;

        let mut field_iter = self.fields.iter().peekable();
        while let Some(field) = field_iter.next() {
            write!(f, "{} = {}", field.name.value, *field.value)?;

            if field_iter.peek().is_some() {
                write!(f, ", ")?;
            }
        }

        write!(f, " }}")
    }
}
