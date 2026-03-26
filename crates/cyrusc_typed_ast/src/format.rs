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

use crate::{
    SymbolID,
    exprs::{TypedExprKind, TypedExprStmt, TypedLambdaExpr, TypedSymbolExpr, TypedUnnamedEnumValueKind},
    stmts::{TypedBuiltin, TypedFuncParamKind, TypedFuncTypeVariadicParams, TypedFuncVariadicParams},
    types::{
        ResolvedSymbol, SemanticType, TypedArrayCapacity, TypedFuncType, TypedUnnamedEnumType, TypedUnnamedEnumVariant,
        TypedUnnamedStructType, TypedUnnamedUnionType,
    },
};
use cyrusc_ast::{AssignKind, operators::UnaryOperator};
use cyrusc_source_loc::{Loc, SourceMap};

pub type SymbolFormatterFn<'a> = &'a dyn Fn(SymbolID) -> String;

fn join_exprs(exprs: &[TypedExprStmt], fmt_symbol: SymbolFormatterFn) -> String {
    exprs
        .iter()
        .map(|e| format_typed_expr(e, fmt_symbol))
        .collect::<Vec<_>>()
        .join(", ")
}

pub fn format_typed_expr(expr: &TypedExprStmt, fmt_symbol: SymbolFormatterFn) -> String {
    use TypedExprKind::*;

    match &expr.kind {
        Symbol(TypedSymbolExpr { symbol_id, .. }) => fmt_symbol(*symbol_id),
        Literal(literal) => literal.to_string(),
        Prefix(p) => format!("{}{}", p.op, format_typed_expr(&p.operand, fmt_symbol)),

        Infix(inf) => format!(
            "{}{}{}",
            format_typed_expr(&inf.lhs, fmt_symbol),
            inf.op,
            format_typed_expr(&inf.rhs, fmt_symbol)
        ),
        Unary(unary) => {
            let operand = format_typed_expr(&unary.operand, fmt_symbol);
            match unary.op {
                UnaryOperator::PreIncrement => format!("++{}", operand),
                UnaryOperator::PreDecrement => format!("--{}", operand),
                UnaryOperator::PostIncrement => format!("{}++", operand),
                UnaryOperator::PostDecrement => format!("{}--", operand),
            }
        }
        Assign(assign) => {
            let lhs = format_typed_expr(&assign.lhs, fmt_symbol);
            let rhs = format_typed_expr(&assign.rhs, fmt_symbol);
            let operator = match assign.kind {
                AssignKind::Default => "=",
                AssignKind::AddAssign => "+=",
                AssignKind::SubAssign => "-=",
                AssignKind::MulAssign => "*=",
                AssignKind::DivAssign => "/=",
                AssignKind::ModAssign => "%=",
                AssignKind::BitwiseAndAssign => "&=",
                AssignKind::BitwiseXorAssign => "^=",
                AssignKind::BitwiseAndNotAssign => "&~=",
                AssignKind::LeftShiftAssign => "<<=",
                AssignKind::RightShiftAssign => ">>=",
            };
            format!("{}{}{}", lhs, operator, rhs)
        }
        Array(array) => {
            let ty = array
                .ty
                .as_ref()
                .map(|t| format_sema_type(t.clone(), fmt_symbol))
                .unwrap_or_default();
            format!("{}{{{}}}", ty, join_exprs(&array.elements, fmt_symbol))
        }
        ArrayIndex(array_index) => format!(
            "{}[{}]",
            format_typed_expr(&array_index.operand, fmt_symbol),
            format_typed_expr(&array_index.index, fmt_symbol)
        ),
        AddrOf(addr_of) => format!("&{}", format_typed_expr(&addr_of.operand, fmt_symbol)),
        Deref(deref) => format!("*{}", format_typed_expr(&deref.operand, fmt_symbol)),
        StructInit(struct_init) => {
            let name = fmt_symbol(struct_init.symbol_id);
            let fields = struct_init
                .fields
                .iter()
                .map(|f| format!("{}: {}", f.name, format_typed_expr(&f.value, fmt_symbol)))
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}{{ {} }}", name, fields)
        }
        FuncCall(func_call) => format!(
            "{}({})",
            format_typed_expr(&func_call.operand, fmt_symbol),
            join_exprs(&func_call.args, fmt_symbol)
        ),
        FieldAccess(field_access) => {
            let op = format_typed_expr(&field_access.operand, fmt_symbol);
            let sep = if field_access.is_fat_arrow { "->" } else { "." };
            format!("{}{}{}", op, sep, field_access.field_name)
        }
        TupleAccess(tuple_access) => format!(
            "{}.{}",
            format_typed_expr(&tuple_access.operand, fmt_symbol),
            tuple_access.index
        ),
        MethodCall(method_call) => {
            let op = format_typed_expr(&method_call.operand, fmt_symbol);
            let sep = if method_call.is_fat_arrow { "->" } else { "." };
            let name = method_call.func_sig.clone().unwrap().name;
            format!("{}{}{}", op, sep, name)
        }
        UnnamedUnionValue(unnamed_union_value) => format!(
            "union {{ {} = {} }}",
            unnamed_union_value.field_name.as_string(),
            format_typed_expr(&unnamed_union_value.field_value, fmt_symbol)
        ),
        UnnamedStructValue(unnamed_struct_value) => {
            let mut out = String::new();

            if let Some(r) = &unnamed_struct_value.repr_attr {
                out.push_str(&r.to_string());
                out.push(' ');
            }
            out.push_str("struct");

            if let Some(al) = unnamed_struct_value.align {
                out.push_str(&format!(" align({})", al));
            }

            out.push_str(" {{ ");

            out.push_str(
                &unnamed_struct_value
                    .fields
                    .iter()
                    .map(|f| {
                        let mut x = String::new();
                        x.push_str(&f.name);
                        if let Some(ty) = &f.ty {
                            x.push_str(": ");
                            x.push_str(&format_sema_type(ty.clone(), fmt_symbol));
                        }
                        x.push_str(&format_typed_expr(&f.field_value, fmt_symbol));
                        x
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
            );

            out.push_str(" }}");
            out
        }
        UnnamedEnumValue(unnamed_enum_value) => {
            let mut out = format!(".{}", unnamed_enum_value.ident.as_string());
            if let TypedUnnamedEnumValueKind::Fielded(vals) = &unnamed_enum_value.kind {
                out.push_str(&format!("({})", join_exprs(vals, fmt_symbol)));
            }
            out
        }
        SemanticType(sema_type) => format_sema_type(sema_type.clone(), fmt_symbol),
        Lambda(lambda) => format_lambda(lambda, fmt_symbol),
        Tuple(tuple) => format!(
            "({})",
            tuple
                .elements
                .iter()
                .map(|v| format_typed_expr(v, fmt_symbol))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Dynamic(dynamic) => format!("dynamic {}", format_typed_expr(&dynamic.operand, fmt_symbol)),
        Builtin(builtin) => match builtin {
            TypedBuiltin::BuiltinFunc(f) => format!("@{}({})", f.name, join_exprs(&f.args, fmt_symbol)),
            TypedBuiltin::BuiltinScope(s) => format!("@{}({}) {{ ... }}", s.name, join_exprs(&s.args, fmt_symbol)),
        },
    }
}

pub fn format_unnamed_union_type(unnamed_union_type: &TypedUnnamedUnionType, fmt_symbol: SymbolFormatterFn) -> String {
    let mut out = String::new();

    if let Some(repr_attr) = &unnamed_union_type.repr_attr {
        out.push_str(&repr_attr.to_string());
        out.push_str(" ");
    }

    out.push_str("union");

    if let Some(align) = unnamed_union_type.align {
        out.push_str(&format!(" align({})", align));
    }

    out.push_str(" { ");

    out.push_str(
        &unnamed_union_type
            .fields
            .iter()
            .map(|field| format!("{}: {}", field.name, format_sema_type(*field.ty.clone(), fmt_symbol)))
            .collect::<Vec<String>>()
            .join(", "),
    );

    out.push_str(" }");
    out
}

pub fn format_unnamed_struct_type(
    unnamed_struct_type: &TypedUnnamedStructType,
    fmt_symbol: SymbolFormatterFn,
) -> String {
    let mut out = String::new();

    if let Some(repr_attr) = &unnamed_struct_type.repr_attr {
        out.push_str(&repr_attr.to_string());
        out.push_str(" ");
    }

    out.push_str("struct");

    if let Some(align) = unnamed_struct_type.align {
        out.push_str(&format!(" align({})", align));
    }

    out.push_str(" { ");

    out.push_str(
        &unnamed_struct_type
            .fields
            .iter()
            .map(|field| format!("{}: {}", field.name, format_sema_type(*field.ty.clone(), fmt_symbol)))
            .collect::<Vec<String>>()
            .join(", "),
    );

    out.push_str(" }");
    out
}

pub fn format_unnamed_enum_type<'a>(unnamed_enum_type: &TypedUnnamedEnumType, fmt_symbol: SymbolFormatterFn) -> String {
    let mut out = String::new();

    if let Some(repr_attr) = &unnamed_enum_type.repr_attr {
        out.push_str(&repr_attr.to_string());
        out.push_str(" ");
    }

    out.push_str("enum");

    if let Some(align) = unnamed_enum_type.align {
        out.push_str(&format!(" align({})", align));
    }

    out.push_str(" { ");

    let variant_strings: Vec<String> = unnamed_enum_type
        .variants
        .iter()
        .map(|variant| match variant {
            TypedUnnamedEnumVariant::Ident(ident) => ident.as_string(),
            TypedUnnamedEnumVariant::Valued(ident, expr) => {
                format!("{} = {}", ident.as_string(), format_typed_expr(expr, fmt_symbol))
            }
            TypedUnnamedEnumVariant::Variant(ident, valued_fields) => {
                let valued_fields_fmt = valued_fields
                    .iter()
                    .map(|field| format_sema_type(field.ty.clone(), fmt_symbol))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{}({})", ident.as_string(), valued_fields_fmt)
            }
        })
        .collect();

    out.push_str(&variant_strings.join(", "));
    out.push_str(" }");
    out
}

pub fn format_sema_type<'a>(sema_type: SemanticType, fmt_symbol: SymbolFormatterFn) -> String {
    match sema_type {
        SemanticType::UnresolvedSymbol(..) => format!("<unresolved_symbol>"),
        SemanticType::GenericParam(generic_param) => generic_param.param_name.name.clone(),
        SemanticType::ResolvedSymbol(resolved_symbol) => match resolved_symbol {
            ResolvedSymbol::Enum(symbol_id) => fmt_symbol(symbol_id),
            ResolvedSymbol::Typedef(symbol_id) => fmt_symbol(symbol_id),
            ResolvedSymbol::Struct(symbol_id) => fmt_symbol(symbol_id),
            ResolvedSymbol::Interface(symbol_id) => fmt_symbol(symbol_id),
            ResolvedSymbol::GlobalVar(symbol_id) => fmt_symbol(symbol_id),
            ResolvedSymbol::Variable(symbol_id) => fmt_symbol(symbol_id),
            ResolvedSymbol::Func(symbol_id) => fmt_symbol(symbol_id),
            ResolvedSymbol::Method(symbol_id) => fmt_symbol(symbol_id),
            ResolvedSymbol::Union(symbol_id) => fmt_symbol(symbol_id),
        },
        SemanticType::PlainType(plain_type) => plain_type.to_string(),
        SemanticType::Array(typed_array_type) => {
            let mut fmt = String::new();
            fmt.push_str(&format_sema_type(*typed_array_type.element_type, fmt_symbol));
            fmt.push_str("[");
            match typed_array_type.capacity {
                TypedArrayCapacity::Fixed(expr) => {
                    fmt.push_str(&format_typed_expr(&expr, fmt_symbol));
                }
                TypedArrayCapacity::Dynamic => {}
            }
            fmt.push_str("]");
            fmt
        }
        SemanticType::Const(sema_type) => {
            format!("const {}", format_sema_type(*sema_type, fmt_symbol))
        }
        SemanticType::Pointer(sema_type) => {
            format!("{}*", format_sema_type(*sema_type, fmt_symbol))
        }
        SemanticType::UnnamedStruct(unnamed_struct_type) => {
            format_unnamed_struct_type(&unnamed_struct_type, fmt_symbol)
        }
        SemanticType::UnnamedUnion(unnamed_union_type) => format_unnamed_union_type(&unnamed_union_type, fmt_symbol),
        SemanticType::UnnamedEnum(unnamed_enum_type) => format_unnamed_enum_type(&unnamed_enum_type, fmt_symbol),
        SemanticType::FuncType(func_type) => format_func_type(&func_type, fmt_symbol),
        SemanticType::Tuple(tuple_type) => {
            format!(
                "({})",
                tuple_type
                    .elements
                    .iter()
                    .map(|t| format_sema_type(t.clone(), fmt_symbol))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        SemanticType::GenericType(generic_type) => generic_type.format(fmt_symbol),
        SemanticType::Interface(interface_type) => fmt_symbol(interface_type.symbol_id),
        SemanticType::DynamicType(dynamic_type) => {
            format!("dynamic {}", fmt_symbol(dynamic_type.interface_id))
        }
        SemanticType::SelfType(_) => "Self".to_string(),
    }
}

pub fn format_func_type<'a>(func_type: &TypedFuncType, fmt_symbol: SymbolFormatterFn) -> String {
    let mut params = func_type
        .params
        .list
        .iter()
        .map(|param| format_sema_type(param.clone(), fmt_symbol))
        .collect::<Vec<String>>()
        .join(", ");

    if let Some(variadic) = func_type.params.variadic.clone() {
        match *variadic {
            TypedFuncTypeVariadicParams::UntypedCStyle => params.push_str(", ..."),
            TypedFuncTypeVariadicParams::Typed(sema_type) => {
                params.push_str(&format!(", {}...", format_sema_type(sema_type, fmt_symbol)))
            }
        }
    }

    let ret = format_sema_type(*func_type.ret_type.clone(), fmt_symbol);
    format!("fn({}) {}", params, ret)
}

pub fn format_lambda(lambda: &TypedLambdaExpr, fmt_symbol: SymbolFormatterFn) -> String {
    let mut params = lambda
        .params
        .list
        .iter()
        .map(|param| match param {
            TypedFuncParamKind::FuncParam(typed_func_param) => {
                format!(
                    "{}: {}",
                    typed_func_param.name,
                    format_sema_type(typed_func_param.ty.clone(), fmt_symbol)
                )
            }
            TypedFuncParamKind::SelfModifier(..) => unreachable!(),
        })
        .collect::<Vec<String>>()
        .join(", ");

    if let Some(variadic) = lambda.params.variadic.clone() {
        match &variadic {
            TypedFuncVariadicParams::UntypedCStyle => params.push_str(", ..."),
            TypedFuncVariadicParams::Typed(ident, sema_type) => params.push_str(&format!(
                ", {}: ...{}",
                ident.name,
                format_sema_type(sema_type.clone(), fmt_symbol)
            )),
        }
    }

    let ret = format_sema_type(lambda.ret_type.clone(), fmt_symbol);
    format!("fn({}) {} {{ ... }}", params, ret)
}

pub fn format_missing_fields(list: &Vec<String>) -> String {
    list.iter()
        .map(|str| format!("'{str}'"))
        .collect::<Vec<String>>()
        .join(", ")
}

pub fn format_loc(source_map: &SourceMap, loc: Loc) -> String {
    let source_file = { source_map.get_file(loc.file_id).unwrap().clone() };
    format!("{}:{}:{}", source_file.name, loc.line, loc.column)
}
