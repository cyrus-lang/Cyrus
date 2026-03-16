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
    exprs::{TypedExprKind, TypedExprStmt, TypedLambdaExpr, TypedUnnamedEnumValueKind},
    stmts::{TypedFuncParamKind, TypedFuncTypeVariadicParams, TypedFuncVariadicParams},
    types::{
        ResolvedSymbol, SemanticType, TypedArrayCapacity, TypedArrayFixedCapacityValue, TypedFuncType,
        TypedUnnamedEnumType, TypedUnnamedEnumVariant, TypedUnnamedStructType, TypedUnnamedUnionType,
    },
};
use cyrusc_ast::{AssignKind, operators::UnaryOperator};
use cyrusc_tokens::literals::{LiteralKind, StringPrefix};

pub fn format_typed_exprs<'a>(exprs: &Vec<TypedExprStmt>, format_symbol: &(dyn Fn(SymbolID) -> String + 'a)) -> String {
    exprs
        .iter()
        .map(|expr| format_typed_expr(expr, format_symbol))
        .collect::<Vec<String>>()
        .join(", ")
}

pub fn format_typed_expr<'a>(typed_expr: &TypedExprStmt, format_symbol: &(dyn Fn(SymbolID) -> String + 'a)) -> String {
    match &typed_expr.kind {
        TypedExprKind::Symbol(symbol_id, ..) => format_symbol(*symbol_id),
        TypedExprKind::Literal(typed_literal) => match &typed_literal.kind {
            LiteralKind::Integer(value, token_kind_opt) => {
                let mut fmt = String::new();
                fmt.push_str(&value.to_string());
                if let Some(token_kind) = token_kind_opt {
                    fmt.push_str(&token_kind.to_string());
                }
                fmt
            }
            LiteralKind::Float(value, token_kind_opt) => {
                let mut fmt = String::new();
                fmt.push_str(&value.to_string());
                if let Some(token_kind) = token_kind_opt {
                    fmt.push_str(&token_kind.to_string());
                }
                fmt
            }
            LiteralKind::String(value, string_prefix) => {
                let mut fmt = String::new();

                if let Some(prefix) = string_prefix {
                    match prefix {
                        StringPrefix::C => fmt.push_str("c"),
                        StringPrefix::B => fmt.push_str("b"),
                    }
                }

                fmt.push_str(&format!("\"{}\"", value));
                fmt
            }
            LiteralKind::Bool(value) => {
                if *value {
                    return "true".to_string();
                } else {
                    return "false".to_string();
                }
            }
            LiteralKind::Char(value) => value.to_string(),
            LiteralKind::Null => "null".to_string(),
        },
        TypedExprKind::Prefix(typed_prefix_expr) => {
            let mut fmt = String::new();
            fmt.push_str(&typed_prefix_expr.op.to_string());
            fmt.push_str(&format_typed_expr(&typed_prefix_expr.operand, format_symbol));
            fmt
        }
        TypedExprKind::Infix(typed_infix_expr) => {
            let mut fmt = String::new();
            fmt.push_str(&format_typed_expr(&typed_infix_expr.lhs, format_symbol));
            fmt.push_str(&typed_infix_expr.op.to_string());
            fmt.push_str(&format_typed_expr(&typed_infix_expr.rhs, format_symbol));
            fmt
        }
        TypedExprKind::Unary(typed_unary_expr) => {
            let operand_fmt = &format_typed_expr(&typed_unary_expr.operand, format_symbol);
            match typed_unary_expr.op {
                UnaryOperator::PreIncrement => format!("++{}", operand_fmt),
                UnaryOperator::PreDecrement => format!("--{}", operand_fmt),
                UnaryOperator::PostIncrement => format!("{}++", operand_fmt),
                UnaryOperator::PostDecrement => format!("{}--", operand_fmt),
            }
        }
        TypedExprKind::Assign(typed_assign) => {
            let mut fmt = String::new();
            fmt.push_str(&format_typed_expr(&typed_assign.lhs, format_symbol));
            match &typed_assign.kind {
                AssignKind::Default => fmt.push_str("="),
                AssignKind::AddAssign => fmt.push_str("+="),
                AssignKind::SubAssign => fmt.push_str("-="),
                AssignKind::MulAssign => fmt.push_str("*="),
                AssignKind::DivAssign => fmt.push_str("/="),
                AssignKind::ModAssign => fmt.push_str("%="),
                AssignKind::BitwiseAndAssign => fmt.push_str("&="),
                AssignKind::BitwiseXorAssign => fmt.push_str("^="),
                AssignKind::BitwiseAndNotAssign => fmt.push_str("&~="),
                AssignKind::LeftShiftAssign => fmt.push_str("<<="),
                AssignKind::RightShiftAssign => fmt.push_str(">>="),
            };
            fmt.push_str(&format_typed_expr(&typed_assign.rhs, format_symbol));
            fmt
        }
        TypedExprKind::Cast(typed_cast) => {
            let mut fmt = String::new();
            let operand_fmt = &format_typed_expr(&typed_cast.operand, format_symbol);
            let target_type_fmt = format_sema_ty(typed_cast.target_type.clone(), format_symbol);
            fmt.push_str(&format!("cast({}, {})", operand_fmt, target_type_fmt));
            fmt
        }
        TypedExprKind::Array(typed_array) => {
            let mut fmt = String::new();
            let array_type_fmt = typed_array
                .array_type
                .as_ref()
                .and_then(|sema_ty| Some(format_sema_ty(sema_ty.clone(), format_symbol)))
                .unwrap_or("".to_string());
            fmt.push_str(&format!(
                "{}{{{}}}",
                array_type_fmt,
                format_typed_exprs(&typed_array.elements, format_symbol)
            ));
            fmt
        }
        TypedExprKind::ArrayIndex(typed_array_index) => {
            let operand_fmt = &format_typed_expr(&typed_array_index.operand, format_symbol);
            let index_fmt = &format_typed_expr(&typed_array_index.index, format_symbol);
            format!("{}[{}]", operand_fmt, index_fmt)
        }
        TypedExprKind::AddrOf(typed_addr_of) => {
            let operand_fmt = &format_typed_expr(&typed_addr_of.operand, format_symbol);
            format!("&{}", operand_fmt)
        }
        TypedExprKind::Deref(typed_deref) => {
            let operand_fmt = &format_typed_expr(&typed_deref.operand, format_symbol);
            format!("*{}", operand_fmt)
        }
        TypedExprKind::StructInit(typed_struct_init) => {
            let mut fmt = String::new();
            let struct_name = format_symbol(typed_struct_init.symbol_id);
            fmt.push_str(&struct_name);
            fmt.push_str("{{ ");
            fmt.push_str(
                &typed_struct_init
                    .fields
                    .iter()
                    .map(|field| {
                        let value_fmt = &format_typed_expr(&field.value, format_symbol);
                        format!("{}: {}", field.name, value_fmt)
                    })
                    .collect::<Vec<String>>()
                    .join(", "),
            );
            fmt.push_str(" }}");
            fmt
        }
        TypedExprKind::FuncCall(typed_func_call) => {
            format!(
                "{}({})",
                format_typed_expr(&typed_func_call.operand, format_symbol),
                format_typed_exprs(&typed_func_call.args, format_symbol)
            )
        }
        TypedExprKind::FieldAccess(field_access) => {
            let mut fmt = String::new();
            let operand_fmt = &format_typed_expr(&field_access.operand, format_symbol);
            fmt.push_str(operand_fmt);
            if field_access.is_fat_arrow {
                fmt.push_str("->");
            } else {
                fmt.push_str(".");
            }
            fmt.push_str(&field_access.field_name);
            fmt
        }
        TypedExprKind::TupleAccess(tuple_member_access) => {
            format!(
                "{}.{}",
                format_typed_expr(&tuple_member_access.operand, format_symbol),
                format_typed_expr(&tuple_member_access.operand, format_symbol)
            )
        }
        TypedExprKind::MethodCall(typed_method_call) => {
            let mut fmt = String::new();
            let operand_fmt = &format_typed_expr(&typed_method_call.operand, format_symbol);
            fmt.push_str(operand_fmt);
            if typed_method_call.is_fat_arrow {
                fmt.push_str("->");
            } else {
                fmt.push_str(".");
            }
            fmt.push_str(&typed_method_call.func_sig.clone().unwrap().name);
            fmt
        }
        TypedExprKind::UnnamedUnionValue(unnamed_union_value) => {
            format!(
                "union {{ {} = {} }}",
                unnamed_union_value.field_name.as_string(),
                format_typed_expr(&unnamed_union_value.field_value, format_symbol)
            )
        }
        TypedExprKind::UnnamedStructValue(unnamed_struct_value) => {
            let mut fmt = String::new();

            if let Some(repr_attr) = &unnamed_struct_value.repr_attr {
                fmt.push_str(&repr_attr.to_string());
                fmt.push_str(" ");
            }

            fmt.push_str("struct");

            if let Some(align) = unnamed_struct_value.align {
                fmt.push_str(&format!(" align({})", align));
            }

            fmt.push_str(" {{ ");

            fmt.push_str(
                &unnamed_struct_value
                    .fields
                    .iter()
                    .map(|field| {
                        let mut lfmt = String::new();
                        lfmt.push_str(&field.name);
                        if let Some(sema_ty) = &field.ty {
                            let type_fmt = format_sema_ty(sema_ty.clone(), format_symbol);
                            lfmt.push_str(": ");
                            lfmt.push_str(&type_fmt);
                        }
                        lfmt.push_str(&format_typed_expr(&field.field_value, format_symbol));
                        lfmt
                    })
                    .collect::<Vec<String>>()
                    .join(", "),
            );
            fmt.push_str(" }}");
            fmt
        }
        TypedExprKind::UnnamedEnumValue(unnamed_enum_value) => {
            let mut fmt = format!(".{}", unnamed_enum_value.ident.as_string());

            match &unnamed_enum_value.kind {
                TypedUnnamedEnumValueKind::Plain => {}
                TypedUnnamedEnumValueKind::Fielded(values) => {
                    fmt.push_str(&format!("({})", format_typed_exprs(values, format_symbol)));
                }
            }

            fmt
        }
        TypedExprKind::SizeOf(typed_size_of_expr) => {
            let operand_fmt = &format_typed_expr(&typed_size_of_expr.operand, format_symbol);
            format!("sizeof({})", operand_fmt)
        }
        TypedExprKind::SemanticType(sema_ty) => format_sema_ty(sema_ty.clone(), format_symbol),
        TypedExprKind::Lambda(typed_lambda) => format_lambda(typed_lambda, format_symbol),
        TypedExprKind::Tuple(tuple_value) => {
            format!(
                "({})",
                tuple_value
                    .expr_list
                    .iter()
                    .map(|v| format_typed_expr(v, format_symbol))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        TypedExprKind::Dynamic(typed_dynamic_expr) => {
            format!(
                "dynamic {}",
                format_typed_expr(&typed_dynamic_expr.operand, format_symbol)
            )
        }
    }
}

pub fn format_unnamed_union_ty<'a>(
    unnamed_union_type: &TypedUnnamedUnionType,
    format_symbol: &(dyn Fn(SymbolID) -> String + 'a),
) -> String {
    let mut fmt = String::new();

    if let Some(repr_attr) = &unnamed_union_type.repr_attr {
        fmt.push_str(&repr_attr.to_string());
        fmt.push_str(" ");
    }

    fmt.push_str("union");

    if let Some(align) = unnamed_union_type.align {
        fmt.push_str(&format!(" align({})", align));
    }

    fmt.push_str(" { ");

    fmt.push_str(
        &unnamed_union_type
            .fields
            .iter()
            .map(|field| format!("{}: {}", field.name, format_sema_ty(*field.ty.clone(), format_symbol)))
            .collect::<Vec<String>>()
            .join(", "),
    );

    fmt.push_str(" }");
    fmt
}

pub fn format_unnamed_struct_ty<'a>(
    unnamed_struct_type: &TypedUnnamedStructType,
    format_symbol: &(dyn Fn(SymbolID) -> String + 'a),
) -> String {
    let mut fmt = String::new();

    if let Some(repr_attr) = &unnamed_struct_type.repr_attr {
        fmt.push_str(&repr_attr.to_string());
        fmt.push_str(" ");
    }

    fmt.push_str("struct");

    if let Some(align) = unnamed_struct_type.align {
        fmt.push_str(&format!(" align({})", align));
    }

    fmt.push_str(" { ");

    fmt.push_str(
        &unnamed_struct_type
            .fields
            .iter()
            .map(|field| format!("{}: {}", field.name, format_sema_ty(*field.ty.clone(), format_symbol)))
            .collect::<Vec<String>>()
            .join(", "),
    );

    fmt.push_str(" }");
    fmt
}

pub fn format_unnamed_enum_ty<'a>(
    unnamed_enum_type: &TypedUnnamedEnumType,
    format_symbol: &(dyn Fn(SymbolID) -> String + 'a),
) -> String {
    let mut fmt = String::new();

    if let Some(repr_attr) = &unnamed_enum_type.repr_attr {
        fmt.push_str(&repr_attr.to_string());
        fmt.push_str(" ");
    }

    fmt.push_str("enum");

    if let Some(align) = unnamed_enum_type.align {
        fmt.push_str(&format!(" align({})", align));
    }

    fmt.push_str(" { ");

    let variant_strings: Vec<String> = unnamed_enum_type
        .variants
        .iter()
        .map(|variant| match variant {
            TypedUnnamedEnumVariant::Ident(ident) => ident.as_string(),
            TypedUnnamedEnumVariant::Valued(ident, expr) => {
                format!("{} = {}", ident.as_string(), format_typed_expr(expr, format_symbol))
            }
            TypedUnnamedEnumVariant::Variant(ident, valued_fields) => {
                let valued_fields_fmt = valued_fields
                    .iter()
                    .map(|field| format_sema_ty(field.ty.clone(), format_symbol))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{}({})", ident.as_string(), valued_fields_fmt)
            }
        })
        .collect();

    fmt.push_str(&variant_strings.join(", "));
    fmt.push_str(" }");
    fmt
}

pub fn format_sema_ty<'a>(sema_ty: SemanticType, format_symbol: &(dyn Fn(SymbolID) -> String + 'a)) -> String {
    match sema_ty {
        SemanticType::UnresolvedSymbol(..) => format!("UNKNOWN"),
        SemanticType::GenericParam(generic_param) => generic_param.param_name.name.clone(),
        SemanticType::ResolvedSymbol(resolved_symbol) => match resolved_symbol {
            ResolvedSymbol::Enum(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Typedef(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Struct(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Interface(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::GlobalVar(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Variable(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Func(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Method(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Union(symbol_id) => format_symbol(symbol_id),
        },
        SemanticType::PlainType(plain_type) => plain_type.to_string(),
        SemanticType::Array(typed_array_type) => {
            let mut fmt = String::new();
            fmt.push_str(&format_sema_ty(*typed_array_type.element_type, format_symbol));
            fmt.push_str("[");
            match typed_array_type.capacity {
                TypedArrayCapacity::Fixed(capacity_value) => match capacity_value {
                    TypedArrayFixedCapacityValue::Expr(typed_expr) => {
                        fmt.push_str(&format_typed_expr(&typed_expr, format_symbol));
                    }
                    TypedArrayFixedCapacityValue::Value(value) => {
                        fmt.push_str(&value.to_string());
                    }
                },
                TypedArrayCapacity::Dynamic => {}
            }
            fmt.push_str("]");
            fmt
        }
        SemanticType::Const(sema_ty) => {
            format!("const {}", format_sema_ty(*sema_ty, format_symbol))
        }
        SemanticType::Pointer(sema_ty) => {
            format!("{}*", format_sema_ty(*sema_ty, format_symbol))
        }
        SemanticType::UnnamedStruct(unnamed_struct_type) => {
            format_unnamed_struct_ty(&unnamed_struct_type, format_symbol)
        }
        SemanticType::UnnamedUnion(unnamed_union_type) => format_unnamed_union_ty(&unnamed_union_type, format_symbol),
        SemanticType::UnnamedEnum(unnamed_enum_type) => format_unnamed_enum_ty(&unnamed_enum_type, format_symbol),
        SemanticType::FuncType(func_type) => format_func_ty(&func_type, format_symbol),
        SemanticType::Tuple(tuple_type) => {
            format!(
                "({})",
                tuple_type
                    .type_list
                    .iter()
                    .map(|t| format_sema_ty(t.clone(), format_symbol))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        SemanticType::GenericType(generic_type) => generic_type.format(format_symbol),
        SemanticType::DynamicType(dynamic_type) => {
            format!("dynamic {}", format_symbol(dynamic_type.interface_symbol_id))
        }
        SemanticType::SelfType(_) => "Self".to_string(),
        SemanticType::Interface(interface_type) => format_symbol(interface_type.symbol_id),
    }
}

pub fn format_func_ty<'a>(func_type: &TypedFuncType, format_symbol: &(dyn Fn(SymbolID) -> String + 'a)) -> String {
    let mut params = func_type
        .params
        .list
        .iter()
        .map(|param| format_sema_ty(param.clone(), format_symbol))
        .collect::<Vec<String>>()
        .join(", ");

    if let Some(variadic) = func_type.params.variadic.clone() {
        match *variadic {
            TypedFuncTypeVariadicParams::UntypedCStyle => params.push_str(", ..."),
            TypedFuncTypeVariadicParams::Typed(sema_ty) => {
                params.push_str(&format!(", {}...", format_sema_ty(sema_ty, format_symbol)))
            }
        }
    }

    let ret = format_sema_ty(*func_type.return_type.clone(), format_symbol);
    format!("fn({}) {}", params, ret)
}

pub fn format_lambda<'a>(lambda: &TypedLambdaExpr, format_symbol: &(dyn Fn(SymbolID) -> String + 'a)) -> String {
    let mut params = lambda
        .params
        .list
        .iter()
        .map(|param| match param {
            TypedFuncParamKind::FuncParam(typed_func_param) => {
                format!(
                    "{}: {}",
                    typed_func_param.name,
                    format_sema_ty(typed_func_param.ty.clone(), format_symbol)
                )
            }
            TypedFuncParamKind::SelfModifier(..) => unreachable!(),
        })
        .collect::<Vec<String>>()
        .join(", ");

    if let Some(variadic) = lambda.params.variadic.clone() {
        match &variadic {
            TypedFuncVariadicParams::UntypedCStyle => params.push_str(", ..."),
            TypedFuncVariadicParams::Typed(ident, sema_ty) => params.push_str(&format!(
                ", {}: ...{}",
                ident.name,
                format_sema_ty(sema_ty.clone(), format_symbol)
            )),
        }
    }

    let ret = format_sema_ty(lambda.return_type.clone(), format_symbol);
    format!("fn({}) {} {{ ... }}", params, ret)
}
