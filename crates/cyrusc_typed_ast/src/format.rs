// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::builtins::TypedBuiltin;
use crate::decls::{DeclID, EnumDecl, StructDecl, UnionDecl};
use crate::exprs::TypedEnumInitArgs;
use crate::stmts::{TypedEnumVariant, TypedEnumVariantStructField, TypedTypeArg};
use crate::types::TypeDeclID;
use crate::{GenericParamID, SymbolID};
use crate::{
    exprs::{TypedExpr, TypedExprKind, TypedLambdaExpr, TypedSymbolExpr, TypedUnnamedEnumValueKind},
    stmts::{TypedFuncParamKind, TypedFuncTypeVariadicParam, TypedFuncVariadicParam, TypedTypeArgs},
    types::{SemaType, TypedArrayCapacity, TypedFuncType, UnresolvedType},
};
use cyrusc_ast::operators::UnaryOperator;
use cyrusc_source_loc::{Loc, SourceMap};

/// Provides human‑readable formatting utilities for compiler diagnostics
/// and debugging output.
pub trait Formatter: Send + Sync {
    fn format_symbol_name(&self, symbol_id: SymbolID) -> String;
    fn format_decl(&self, decl_id: DeclID) -> String;
    fn format_type_decl(&self, type_decl_id: TypeDeclID) -> String;
    fn format_generic_param(&self, generic_param_id: GenericParamID) -> String;
}

fn join_exprs(exprs: &[TypedExpr], f: &dyn Formatter) -> String {
    exprs
        .iter()
        .map(|expr| format_typed_expr(expr, f))
        .collect::<Vec<_>>()
        .join(", ")
}

pub fn format_union_decl(union_decl: &UnionDecl, f: &dyn Formatter) -> String {
    // named union: just return the name
    if let Some(name) = &union_decl.name {
        return name.clone();
    }

    // unnamed struct
    let mut out = String::from("union { ");

    for (i, field) in union_decl.fields.iter().enumerate() {
        let formatted_ty = format_sema_type(field.ty.clone(), f);

        out.push_str(&format!("{}: {}", field.name, formatted_ty));

        if i + 1 != union_decl.fields.len() {
            out.push_str(", ");
        }
    }

    out.push_str(" }");
    out
}

pub fn format_enum_decl(enum_decl: &EnumDecl, f: &dyn Formatter) -> String {
    // named enum: just return the name
    if let Some(name) = &enum_decl.name {
        return name.clone();
    }

    // unnamed enum
    let mut out = String::from("enum { ");

    let variants: Vec<String> = enum_decl
        .variants
        .iter()
        .map(|variant| match variant {
            TypedEnumVariant::Unit(ident) => ident.value.clone(),
            TypedEnumVariant::Valued { ident, value } => {
                format!("{} = {}", ident.value, format_typed_expr(value, f))
            }
            TypedEnumVariant::Tuple { ident, fields } => {
                let types = fields
                    .iter()
                    .map(|tuple_field| format_sema_type(tuple_field.ty.clone(), f))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", ident.value, types)
            }
            TypedEnumVariant::Struct { ident, fields } => {
                let fields_str = fields
                    .iter()
                    .map(|fld| format!("{}: {}", fld.name.value, format_sema_type(fld.ty.clone(), f)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} {{ {} }}", ident.value, fields_str)
            }
        })
        .collect();

    out.push_str(&variants.join(", "));
    out.push_str(" }");
    out
}

pub fn format_struct_decl(struct_decl: &StructDecl, f: &dyn Formatter) -> String {
    // named struct: just return the name
    if let Some(name) = &struct_decl.name {
        return name.clone();
    }

    // unnamed struct
    let mut out = String::from("struct { ");

    for (i, field) in struct_decl.fields.iter().enumerate() {
        let formatted_ty = format_sema_type(field.ty.clone(), f);

        out.push_str(&format!("{}: {}", field.name, formatted_ty));

        if i + 1 != struct_decl.fields.len() {
            out.push_str(", ");
        }
    }

    out.push_str(" }");
    out
}

pub fn format_typed_expr(expr: &TypedExpr, formatter: &dyn Formatter) -> String {
    use TypedExprKind::*;

    match &expr.kind {
        Symbol(symbol_expr) => match symbol_expr {
            TypedSymbolExpr::Unresolved { symbol_id, .. } => formatter.format_symbol_name(*symbol_id),
            TypedSymbolExpr::Resolved { decl_id, .. } => formatter.format_decl(*decl_id),
        },
        Literal(literal) => literal.to_string(),
        Prefix(p) => format!("{}{}", p.op, format_typed_expr(&p.operand, formatter)),
        Infix(inf) => format!(
            "{}{}{}",
            format_typed_expr(&inf.lhs, formatter),
            inf.op,
            format_typed_expr(&inf.rhs, formatter)
        ),
        Unary(unary) => {
            let operand = format_typed_expr(&unary.operand, formatter);
            match unary.op {
                UnaryOperator::PreIncrement => format!("++{}", operand),
                UnaryOperator::PreDecrement => format!("--{}", operand),
                UnaryOperator::PostIncrement => format!("{}++", operand),
                UnaryOperator::PostDecrement => format!("{}--", operand),
            }
        }
        Assign(assign) => {
            let lhs = format_typed_expr(&assign.lhs, formatter);
            let rhs = format_typed_expr(&assign.rhs, formatter);
            format!("{}{}{}", lhs, assign.kind, rhs)
        }
        Array(array) => {
            let ty = array
                .ty
                .as_ref()
                .map(|t| format_sema_type(t.clone(), formatter))
                .unwrap_or_default();
            format!("{}{{{}}}", ty, join_exprs(&array.elements, formatter))
        }
        ArrayIndex(array_index) => format!(
            "{}[{}]",
            format_typed_expr(&array_index.operand, formatter),
            format_typed_expr(&array_index.index, formatter)
        ),
        AddrOf(addr_of) => format!("&{}", format_typed_expr(&addr_of.operand, formatter)),
        Deref(deref) => format!("*{}", format_typed_expr(&deref.operand, formatter)),
        StructInit(struct_init) => {
            let fields = struct_init
                .fields
                .iter()
                .map(|field_init| {
                    format!(
                        "{} = {}",
                        field_init.name,
                        format_typed_expr(&field_init.value, formatter)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");

            format!("struct {{ {} }}", fields)
        }
        UnionInit(union_init) => {
            format!(
                "union {{ {} }}",
                format!(
                    "{}: {}",
                    union_init.field.name,
                    format_typed_expr(&union_init.field.value, formatter)
                )
            )
        }
        FuncCall(func_call) => format!(
            "{}({})",
            format_typed_expr(&func_call.operand, formatter),
            join_exprs(&func_call.args, formatter)
        ),
        FieldAccess(field_access) => {
            let op = format_typed_expr(&field_access.operand, formatter);
            let sep = if field_access.is_thin_arrow { "->" } else { "." };
            format!("{}{}{}", op, sep, field_access.name)
        }
        TupleAccess(tuple_access) => format!(
            "{}.{}",
            format_typed_expr(&tuple_access.operand, formatter),
            tuple_access.index
        ),
        MethodCall(method_call) => {
            let operand = format_typed_expr(&method_call.operand, formatter);
            let separator = if method_call.is_thin_arrow { "->" } else { "." };
            format!("{}{}{}", operand, separator, method_call.name)
        }
        UnnamedUnionValue(unnamed_union_value) => {
            let mut out = String::new();

            out.push_str("union");

            out.push_str(" {{ ");

            out.push_str(
                &unnamed_union_value
                    .fields
                    .iter()
                    .map(|field| {
                        let mut x = String::new();
                        x.push_str(&field.name);
                        x.push_str(&format_typed_expr(&field.value, formatter));
                        x
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
            );

            out.push_str(" }}");
            out
        }
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
                        x.push_str(&format_typed_expr(&field.value, formatter));
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
            if let TypedUnnamedEnumValueKind::Tuple(vals) = &unnamed_enum_value.kind {
                out.push_str(&format!("({})", join_exprs(vals, formatter)));
            }
            out
        }
        EnumStructVariantInit(struct_init) => {
            let mut out = format!(
                "{}.{}",
                format_typed_expr(&struct_init.operand, formatter),
                struct_init.ident
            );

            if !struct_init.field_inits.is_empty() {
                let fields = struct_init
                    .field_inits
                    .iter()
                    .map(|field_init| {
                        format!(
                            "{}: {}",
                            field_init.name,
                            format_typed_expr(&field_init.value, formatter)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                out.push_str(&format!(" {{ {} }}", fields));
            } else {
                out.push_str(" {}");
            }

            out
        }
        EnumInit(enum_init) => {
            let enum_name = format_sema_type(enum_init.operand.clone(), formatter);

            let mut out = String::from(format!("{}.{}", enum_name, enum_init.name));

            match &enum_init.arg {
                TypedEnumInitArgs::Unit => {}

                TypedEnumInitArgs::Tuple(vals) => {
                    out.push_str(&format!("({})", join_exprs(vals, formatter)));
                }

                TypedEnumInitArgs::Struct(fields) => {
                    if !fields.is_empty() {
                        let fields = fields
                            .iter()
                            .map(|field_init| {
                                format!(
                                    "{}: {}",
                                    field_init.name,
                                    format_typed_expr(&field_init.value, formatter)
                                )
                            })
                            .collect::<Vec<_>>()
                            .join(", ");

                        out.push_str(&format!(" {{ {} }}", fields));
                    } else {
                        out.push_str(" {}");
                    }
                }
            }

            out
        }
        Lambda(lambda) => format_lambda(lambda, formatter),
        Tuple(tuple) => format!(
            "({})",
            tuple
                .elements
                .iter()
                .map(|v| format_typed_expr(v, formatter))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Dynamic(dynamic) => format!("dynamic {}", format_typed_expr(&dynamic.operand, formatter)),
        Builtin(builtin) => match builtin {
            TypedBuiltin::BuiltinFunc(builtin_func) => {
                format!("@{}({})", builtin_func.name, join_exprs(&builtin_func.args, formatter))
            }
            TypedBuiltin::BuiltinBlock(builtin_block) => format!(
                "@{}({}) {{ ... }}",
                builtin_block.name,
                join_exprs(&builtin_block.args, formatter)
            ),
        },

        SemaType { ty, .. } => format_sema_type(ty.clone(), formatter),
        
        Poisoned => unreachable!(),
        Try(inner) => format!("try {}", format_typed_expr(inner, formatter)),
    }
}

pub fn format_enum_struct_variant_fields(
    fields: &Vec<TypedEnumVariantStructField>,
    formatter: &dyn Formatter,
) -> String {
    fields
        .iter()
        .map(|field| format!("{}: {}", field.name, format_sema_type(field.ty.clone(), formatter)))
        .collect::<Vec<_>>()
        .join(", ")
}

pub fn format_sema_type(ty: SemaType, formatter: &dyn Formatter) -> String {
    match ty {
        SemaType::Unresolved(unresolved_type) => match unresolved_type {
            UnresolvedType::Decl(_) => format!("<unresolved_symbol>"),
            UnresolvedType::GenericInst { .. } => format!("<unresolved_generic_inst>"),
            UnresolvedType::BuiltinFunc(_) => format!("<unresolved_builtin_func>"),
        },
        SemaType::GenericParam(generic_param_id) => formatter.format_generic_param(generic_param_id),
        SemaType::InterfaceObject(interface_object) => {
            format_sema_type(SemaType::Named(interface_object.interface_type), formatter)
        }
        SemaType::Named(named_type) => {
            let name = formatter.format_type_decl(named_type.type_decl_id);
            format!("{}{}", name, format_type_args(&named_type.type_args, formatter))
        }
        SemaType::Plain(plain_type) => plain_type.to_string(),
        SemaType::Array(typed_array_type) => {
            let mut fmt = String::new();
            fmt.push_str(&format_sema_type(*typed_array_type.element_type, formatter));
            fmt.push_str("[");
            match typed_array_type.capacity {
                TypedArrayCapacity::Fixed(expr) => {
                    fmt.push_str(&format_typed_expr(&expr, formatter));
                }
                TypedArrayCapacity::Dynamic => {}
            }
            fmt.push_str("]");
            fmt
        }
        SemaType::Const(sema_type) => {
            format!("const {}", format_sema_type(*sema_type, formatter))
        }
        SemaType::Pointer(sema_type) => {
            format!("{}*", format_sema_type(*sema_type, formatter))
        }
        SemaType::FuncType(func_type) => format_func_type(&func_type, formatter),
        SemaType::Tuple(tuple_type) => {
            format!(
                "({})",
                tuple_type
                    .elements
                    .iter()
                    .map(|(ty, _)| format_sema_type(ty.clone(), formatter))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        SemaType::SelfType(_) => "Self".to_string(),
        SemaType::Err(_) => "<error>".to_string(),
        SemaType::InferVar(_) => "_".to_string(),
        SemaType::Placeholder => "<placeholder>".to_string(),
    }
}

fn format_type_args(type_args: &TypedTypeArgs, formatter: &dyn Formatter) -> String {
    if type_args.0.is_empty() {
        return String::new();
    }

    let args = type_args
        .0
        .iter()
        .map(|type_arg| match type_arg {
            TypedTypeArg::Type(sema_type, _) => format_sema_type(sema_type.clone(), formatter),
            TypedTypeArg::Infer => String::from('_'),
        })
        .collect::<Vec<_>>()
        .join(", ");

    format!("<{}>", args)
}

pub fn format_func_type<'a>(func_type: &TypedFuncType, formatter: &dyn Formatter) -> String {
    let mut params = func_type
        .params
        .list
        .iter()
        .map(|param| format_sema_type(param.clone(), formatter))
        .collect::<Vec<String>>()
        .join(", ");

    if let Some(variadic) = func_type.params.variadic.clone() {
        match *variadic {
            TypedFuncTypeVariadicParam::UntypedCStyle => params.push_str(", ..."),
            TypedFuncTypeVariadicParam::Typed(sema_type) => {
                params.push_str(&format!(", {}...", format_sema_type(sema_type, formatter)))
            }
        }
    }

    let ret = format_sema_type(*func_type.ret_type.clone(), formatter);
    format!("fn({}) {}", params, ret)
}

pub fn format_lambda(lambda: &TypedLambdaExpr, formatter: &dyn Formatter) -> String {
    let mut params = lambda
        .params
        .list
        .iter()
        .map(|param_kind| match param_kind {
            TypedFuncParamKind::FuncParam(param) => {
                format!("{}: {}", param.ident, format_sema_type(param.ty.clone(), formatter))
            }
            TypedFuncParamKind::SelfModifier(..) => unreachable!(),
        })
        .collect::<Vec<String>>()
        .join(", ");

    if let Some(variadic) = lambda.params.variadic.clone() {
        match &variadic {
            TypedFuncVariadicParam::UntypedCStyle => params.push_str(", ..."),
            TypedFuncVariadicParam::Typed { var_decl_id, ty, .. } => {
                let name = formatter.format_decl(DeclID::Var(*var_decl_id));

                params.push_str(&format!(", {}: ...{}", name, format_sema_type(ty.clone(), formatter)))
            }
        }
    }

    let ret = format_sema_type(lambda.ret_type.clone(), formatter);
    format!("fn({}) {} {{ ... }}", params, ret)
}

#[inline]
pub fn format_missing_fields(list: &[&str]) -> String {
    list.iter()
        .map(|str| format!("'{str}'"))
        .collect::<Vec<String>>()
        .join(", ")
}

#[inline]
pub fn format_loc(source_map: &SourceMap, loc: Loc) -> String {
    let source_file = { source_map.get_file(loc.file_id).unwrap().clone() };
    format!(
        "{}:{}:{}",
        source_file.file_path.to_str().unwrap(),
        loc.line,
        loc.column
    )
}
