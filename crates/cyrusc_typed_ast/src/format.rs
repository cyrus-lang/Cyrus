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
    stmts::{
        TypedBuiltin, TypedFuncParamKind, TypedFuncTypeVariadicParams, TypedFuncVariadicParams, TypedTypeArg,
        TypedTypeArgs,
    },
    types::{SemanticType, TypeDeclID, TypedArrayCapacity, TypedFuncType, UnresolvedType},
};
use crate::{
    decls::{EnumDecl, StructDecl, UnionDecl},
    format::format_sema_type,
};
use cyrusc_ast::operators::UnaryOperator;
use cyrusc_source_loc::{Loc, SourceMap};

pub struct Formatter<'a> {
    pub fmt_symbol: &'a dyn Fn(SymbolID) -> String,
    pub fmt_decl: &'a dyn Fn(TypeDeclID) -> String,
}

fn join_exprs(exprs: &[TypedExprStmt], f: &Formatter) -> String {
    exprs
        .iter()
        .map(|expr| format_typed_expr(expr, f))
        .collect::<Vec<_>>()
        .join(", ")
}

pub fn format_struct_decl(struct_decl: &StructDecl, f: &Formatter) -> String {
    if let Some(name) = &struct_decl.name {
        name.clone()
    } else {
        let mut out = String::from("struct {{ ");
        for field in &struct_decl.fields {
            out.push_str(&format!("{}: {},", field.name, format_sema_type(field.ty.clone(), f)));
        }
        out.push_str(" }}");
        out
    }
}

pub fn format_typed_expr(expr: &TypedExprStmt, f: &Formatter) -> String {
    use TypedExprKind::*;

    match &expr.kind {
        Symbol(TypedSymbolExpr { symbol_id, .. }) => (f.fmt_symbol)(*symbol_id),
        Literal(literal) => literal.to_string(),
        Prefix(p) => format!("{}{}", p.op, format_typed_expr(&p.operand, f)),

        Infix(inf) => format!(
            "{}{}{}",
            format_typed_expr(&inf.lhs, f),
            inf.op,
            format_typed_expr(&inf.rhs, f)
        ),
        Unary(unary) => {
            let operand = format_typed_expr(&unary.operand, f);
            match unary.op {
                UnaryOperator::PreIncrement => format!("++{}", operand),
                UnaryOperator::PreDecrement => format!("--{}", operand),
                UnaryOperator::PostIncrement => format!("{}++", operand),
                UnaryOperator::PostDecrement => format!("{}--", operand),
            }
        }
        Assign(assign) => {
            let lhs = format_typed_expr(&assign.lhs, f);
            let rhs = format_typed_expr(&assign.rhs, f);
            format!("{}{}{}", lhs, assign.kind, rhs)
        }
        Array(array) => {
            let ty = array
                .ty
                .as_ref()
                .map(|t| format_sema_type(t.clone(), f))
                .unwrap_or_default();
            format!("{}{{{}}}", ty, join_exprs(&array.elements, f))
        }
        ArrayIndex(array_index) => format!(
            "{}[{}]",
            format_typed_expr(&array_index.operand, f),
            format_typed_expr(&array_index.index, f)
        ),
        AddrOf(addr_of) => format!("&{}", format_typed_expr(&addr_of.operand, f)),
        Deref(deref) => format!("*{}", format_typed_expr(&deref.operand, f)),
        StructInit(struct_init) => {
            let name = (f.fmt_symbol)(struct_init.symbol_id);
            let fields = struct_init
                .fields
                .iter()
                .map(|field_init| format!("{}: {}", field_init.name, format_typed_expr(&field_init.value, f)))
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}{{ {} }}", name, fields)
        }
        FuncCall(func_call) => format!(
            "{}({})",
            format_typed_expr(&func_call.operand, f),
            join_exprs(&func_call.args, f)
        ),
        FieldAccess(field_access) => {
            let op = format_typed_expr(&field_access.operand, f);
            let sep = if field_access.is_fat_arrow { "->" } else { "." };
            format!("{}{}{}", op, sep, field_access.field_name)
        }
        TupleAccess(tuple_access) => format!("{}.{}", format_typed_expr(&tuple_access.operand, f), tuple_access.index),
        MethodCall(method_call) => {
            let operand = format_typed_expr(&method_call.operand, f);
            let separator = if method_call.is_fat_arrow { "->" } else { "." };
            format!("{}{}{}", operand, separator, method_call.method_name)
        }
        UnnamedUnionValue(unnamed_union_value) => format!(
            "union {{ {} = {} }}",
            unnamed_union_value.name.as_string(),
            format_typed_expr(&unnamed_union_value.value, f)
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
                    .map(|field| {
                        let mut x = String::new();
                        x.push_str(&field.name);
                        if let Some(ty) = &field.ty {
                            x.push_str(": ");
                            x.push_str(&format_sema_type(ty.clone(), f));
                        }
                        x.push_str(&format_typed_expr(&field.value, f));
                        x
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
            );

            out.push_str(" }}");
            out
        }
        UnnamedEnumValue(unnamed_enum_value) => {
            let mut out = format!(".{}", unnamed_enum_value.variant_name.as_string());
            if let TypedUnnamedEnumValueKind::Fielded(vals) = &unnamed_enum_value.kind {
                out.push_str(&format!("({})", join_exprs(vals, f)));
            }
            out
        }
        SemanticType(sema_type) => format_sema_type(sema_type.clone(), f),
        Lambda(lambda) => format_lambda(lambda, f),
        Tuple(tuple) => format!(
            "({})",
            tuple
                .elements
                .iter()
                .map(|v| format_typed_expr(v, f))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Dynamic(dynamic) => format!("dynamic {}", format_typed_expr(&dynamic.operand, f)),
        Builtin(builtin) => match builtin {
            TypedBuiltin::BuiltinFunc(builtin_func) => {
                format!("@{}({})", builtin_func.name, join_exprs(&builtin_func.args, f))
            }
            TypedBuiltin::BuiltinScope(builtin_scope) => format!(
                "@{}({}) {{ ... }}",
                builtin_scope.name,
                join_exprs(&builtin_scope.args, f)
            ),
        },
    }
}

pub fn format_sema_type(sema_type: SemanticType, f: &Formatter) -> String {
    match sema_type {
        SemanticType::Unresolved(unresolved_type) => match unresolved_type {
            UnresolvedType::Symbol(_) => format!("<unresolved_symbol>"),
            UnresolvedType::GenericInst { .. } => format!("<unresolved_generic_inst>"),
        },
        SemanticType::GenericParam(generic_param) => generic_param.name.as_string(),
        SemanticType::Named(named_type) => {
            let name = (f.fmt_decl)(named_type.decl_id);
            format!("{}{}", name, format_type_args(&named_type.type_args, f))
        }
        SemanticType::PlainType(plain_type) => plain_type.to_string(),
        SemanticType::Array(typed_array_type) => {
            let mut fmt = String::new();
            fmt.push_str(&format_sema_type(*typed_array_type.element_type, f));
            fmt.push_str("[");
            match typed_array_type.capacity {
                TypedArrayCapacity::Fixed(expr) => {
                    fmt.push_str(&format_typed_expr(&expr, f));
                }
                TypedArrayCapacity::Dynamic => {}
            }
            fmt.push_str("]");
            fmt
        }
        SemanticType::Const(sema_type) => {
            format!("const {}", format_sema_type(*sema_type, f))
        }
        SemanticType::Pointer(sema_type) => {
            format!("{}*", format_sema_type(*sema_type, f))
        }
        SemanticType::FuncType(func_type) => format_func_type(&func_type, f),
        SemanticType::Tuple(tuple_type) => {
            format!(
                "({})",
                tuple_type
                    .elements
                    .iter()
                    .map(|t| format_sema_type(t.clone(), f))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        SemanticType::DynamicType(dynamic_type) => {
            format!("dynamic {}", (f.fmt_symbol)(dynamic_type.interface_id))
        }
        SemanticType::SelfType(_) => "Self".to_string(),
    }
}

fn format_type_args(type_args: &TypedTypeArgs, f: &Formatter) -> String {
    if type_args.is_empty() {
        return String::new();
    }

    let mut out = String::from('<');
    for type_arg in type_args {
        match type_arg {
            TypedTypeArg::Positional { ty, .. } => {
                out.push_str(&format_sema_type(ty.clone(), f));
            }
            TypedTypeArg::Named { key, ty, .. } => {
                out.push_str(&format!("{} = {}", key, format_sema_type(ty.clone(), f)));
            }
        }
    }
    out.push('>');
    out
}

pub fn format_func_type<'a>(func_type: &TypedFuncType, f: &Formatter) -> String {
    let mut params = func_type
        .params
        .list
        .iter()
        .map(|param| format_sema_type(param.clone(), f))
        .collect::<Vec<String>>()
        .join(", ");

    if let Some(variadic) = func_type.params.variadic.clone() {
        match *variadic {
            TypedFuncTypeVariadicParams::UntypedCStyle => params.push_str(", ..."),
            TypedFuncTypeVariadicParams::Typed(sema_type) => {
                params.push_str(&format!(", {}...", format_sema_type(sema_type, f)))
            }
        }
    }

    let ret = format_sema_type(*func_type.ret_type.clone(), f);
    format!("fn({}) {}", params, ret)
}

pub fn format_lambda(lambda: &TypedLambdaExpr, f: &Formatter) -> String {
    let mut params = lambda
        .params
        .list
        .iter()
        .map(|param_kind| match param_kind {
            TypedFuncParamKind::FuncParam(param) => {
                format!("{}: {}", param.name, format_sema_type(param.ty.clone(), f))
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
                format_sema_type(sema_type.clone(), f)
            )),
        }
    }

    let ret = format_sema_type(lambda.ret_type.clone(), f);
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
    format!(
        "{}:{}:{}",
        source_file.file_path.to_str().unwrap(),
        loc.line,
        loc.column
    )
}
