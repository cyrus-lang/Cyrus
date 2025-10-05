use ast::{AssignmentKind, LiteralKind, StringPrefix, operators::UnaryOperator};

use crate::{
    SymbolID, TypedExpression, TypedExpressionKind, TypedFuncParamKind, TypedFuncTypeVariadicParams,
    TypedFuncVariadicParams, TypedLambda,
    types::{
        BasicConcreteType, ConcreteType, ResolvedSymbol, TypedArrayCapacity, TypedArrayFixedCapacityValue,
        TypedFuncType, TypedUnnamedStructType,
    },
};

pub fn format_typed_exprs<'a>(
    exprs: &Vec<TypedExpression>,
    format_symbol: &(dyn Fn(SymbolID) -> String + 'a),
) -> String {
    exprs
        .iter()
        .map(|expr| format_typed_expr(expr, format_symbol))
        .collect::<Vec<String>>()
        .join(", ")
}

pub fn format_typed_expr<'a>(
    typed_expr: &TypedExpression,
    format_symbol: &(dyn Fn(SymbolID) -> String + 'a),
) -> String {
    match &typed_expr.kind {
        TypedExpressionKind::Symbol(symbol_id, ..) => format_symbol(*symbol_id),
        TypedExpressionKind::Literal(typed_literal) => match &typed_literal.kind {
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
            LiteralKind::Char(value) => return value.to_string(),
            LiteralKind::Null => return "null".to_string(),
        },
        TypedExpressionKind::Prefix(typed_prefix_expr) => {
            let mut fmt = String::new();
            fmt.push_str(&typed_prefix_expr.op.to_string());
            fmt.push_str(&format_typed_expr(&typed_prefix_expr.operand, format_symbol));
            return fmt;
        }
        TypedExpressionKind::Infix(typed_infix_expr) => {
            let mut fmt = String::new();
            fmt.push_str(&format_typed_expr(&typed_infix_expr.lhs, format_symbol));
            fmt.push_str(&typed_infix_expr.op.to_string());
            fmt.push_str(&format_typed_expr(&typed_infix_expr.rhs, format_symbol));
            return fmt;
        }
        TypedExpressionKind::Unary(typed_unary_expr) => {
            let operand_fmt = &format_typed_expr(&typed_unary_expr.operand, format_symbol);
            match typed_unary_expr.op {
                UnaryOperator::PreIncrement => return format!("++{}", operand_fmt),
                UnaryOperator::PreDecrement => return format!("--{}", operand_fmt),
                UnaryOperator::PostIncrement => return format!("{}++", operand_fmt),
                UnaryOperator::PostDecrement => return format!("{}--", operand_fmt),
            }
        }
        TypedExpressionKind::Assignment(typed_assign) => {
            let mut fmt = String::new();
            fmt.push_str(&format_typed_expr(&typed_assign.lhs, format_symbol));
            match &typed_assign.kind {
                AssignmentKind::Default => fmt.push_str("="),
                AssignmentKind::AddAssign => fmt.push_str("+="),
                AssignmentKind::SubAssign => fmt.push_str("-="),
                AssignmentKind::MulAssign => fmt.push_str("*="),
                AssignmentKind::DivAssign => fmt.push_str("/="),
                AssignmentKind::ModAssign => fmt.push_str("%="),
                AssignmentKind::BitwiseAndAssign => fmt.push_str("&="),
                AssignmentKind::BitwiseXorAssign => fmt.push_str("^="),
                AssignmentKind::BitwiseAndNotAssign => fmt.push_str("&~="),
                AssignmentKind::LeftShiftAssign => fmt.push_str("<<="),
                AssignmentKind::RightShiftAssign => fmt.push_str(">>="),
            };
            fmt.push_str(&format_typed_expr(&typed_assign.rhs, format_symbol));
            fmt
        }
        TypedExpressionKind::Cast(typed_cast) => {
            let mut fmt = String::new();
            let operand_fmt = &format_typed_expr(&typed_cast.operand, format_symbol);
            let target_type_fmt = format_concrete_type(typed_cast.target_type.clone(), format_symbol);
            fmt.push_str(&format!("cast({}, {})", operand_fmt, target_type_fmt));
            fmt
        }
        TypedExpressionKind::Array(typed_array) => {
            let mut fmt = String::new();
            let array_type_fmt = format_concrete_type(typed_array.array_type.clone(), format_symbol);
            fmt.push_str(&format!(
                "{}{{{}}}",
                array_type_fmt,
                format_typed_exprs(&typed_array.elements, format_symbol)
            ));
            fmt
        }
        TypedExpressionKind::ArrayIndex(typed_array_index) => {
            let operand_fmt = &format_typed_expr(&typed_array_index.operand, format_symbol);
            let index_fmt = &format_typed_expr(&typed_array_index.index, format_symbol);
            return format!("{}[{}]", operand_fmt, index_fmt);
        }
        TypedExpressionKind::AddressOf(typed_address_of) => {
            let operand_fmt = &format_typed_expr(&typed_address_of.operand, format_symbol);
            return format!("&{}", operand_fmt);
        }
        TypedExpressionKind::Dereference(typed_dereference) => {
            let operand_fmt = &format_typed_expr(&typed_dereference.operand, format_symbol);
            return format!("*{}", operand_fmt);
        }
        TypedExpressionKind::StructInit(typed_struct_init) => {
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
            return fmt;
        }
        TypedExpressionKind::FuncCall(typed_func_call) => {
            let func_name = format_symbol(typed_func_call.symbol_id);
            format!(
                "{}({})",
                func_name,
                format_typed_exprs(&typed_func_call.args, format_symbol)
            )
        }
        TypedExpressionKind::FieldAccess(typed_field_access) => {
            let mut fmt = String::new();
            let operand_fmt = &format_typed_expr(&typed_field_access.operand, format_symbol);
            fmt.push_str(operand_fmt);
            if typed_field_access.is_fat_arrow {
                fmt.push_str("->");
            } else {
                fmt.push_str(".");
            }
            fmt.push_str(&typed_field_access.field_name);
            fmt
        }
        TypedExpressionKind::MethodCall(typed_method_call) => {
            let mut fmt = String::new();
            let operand_fmt = &format_typed_expr(&typed_method_call.operand, format_symbol);
            fmt.push_str(operand_fmt);
            if typed_method_call.is_fat_arrow {
                fmt.push_str("->");
            } else {
                fmt.push_str(".");
            }
            fmt.push_str(&typed_method_call.method_name);
            fmt
        }
        TypedExpressionKind::UnnamedStructValue(typed_unnamed_struct_value) => {
            let mut fmt = String::new();
            if typed_unnamed_struct_value.is_const {
                fmt.push_str("const ");
            }
            if typed_unnamed_struct_value.packed {
                fmt.push_str("bits ");
            } else {
                fmt.push_str("struct ");
            }
            fmt.push_str("{{ ");
            fmt.push_str(
                &typed_unnamed_struct_value
                    .fields
                    .iter()
                    .map(|field| {
                        let mut lfmt = String::new();
                        lfmt.push_str(&field.field_name);
                        if let Some(concrete_type) = &field.field_type {
                            let type_fmt = format_concrete_type(concrete_type.clone(), format_symbol);
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
        TypedExpressionKind::SizeOfExpression(typed_size_of_expr) => {
            let operand_fmt = &format_typed_expr(&typed_size_of_expr.expr, format_symbol);
            return format!("sizeof({})", operand_fmt);
        }
        TypedExpressionKind::ConcreteType(concrete_type) => format_concrete_type(concrete_type.clone(), format_symbol),
        TypedExpressionKind::Lambda(typed_lambda) => format_lambda(typed_lambda, format_symbol),
    }
}

pub fn format_unnamed_struct_type<'a>(
    typed_unnamed_struct_type: &TypedUnnamedStructType,
    format_symbol: &(dyn Fn(SymbolID) -> String + 'a),
) -> String {
    let mut fmt = String::new();

    if typed_unnamed_struct_type.packed {
        fmt.push_str("bits");
    } else {
        fmt.push_str("struct");
    };

    fmt.push_str(" { ");

    fmt.push_str(
        &typed_unnamed_struct_type
            .fields
            .iter()
            .map(|field| {
                format!(
                    "{}: {}",
                    field.field_name,
                    format_concrete_type(*field.field_type.clone(), format_symbol)
                )
            })
            .collect::<Vec<String>>()
            .join(", "),
    );

    fmt.push_str(" }");

    fmt
}

pub fn format_concrete_type<'a>(
    concrete_type: ConcreteType,
    format_symbol: &(dyn Fn(SymbolID) -> String + 'a),
) -> String {
    match concrete_type {
        ConcreteType::UnresolvedSymbol(..) => unreachable!(),
        ConcreteType::ResolvedSymbol(resolved_symbol) => match resolved_symbol {
            ResolvedSymbol::Enum(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Typedef(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::NamedStruct(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Interface(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::GlobalVar(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Variable(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Func(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Method(symbol_id) => format_symbol(symbol_id),
            ResolvedSymbol::Union(symbol_id) => format_symbol(symbol_id),
        },
        ConcreteType::BasicType(basic_concrete_type) => match basic_concrete_type {
            BasicConcreteType::UIntPtr => "uintptr".to_string(),
            BasicConcreteType::IntPtr => "intptr".to_string(),
            BasicConcreteType::SizeT => "size_t".to_string(),
            BasicConcreteType::Int => "int".to_string(),
            BasicConcreteType::Int8 => "int8".to_string(),
            BasicConcreteType::Int16 => "int16".to_string(),
            BasicConcreteType::Int32 => "int32".to_string(),
            BasicConcreteType::Int64 => "int64".to_string(),
            BasicConcreteType::Int128 => "int128".to_string(),
            BasicConcreteType::UInt => "uint".to_string(),
            BasicConcreteType::UInt8 => "uint8".to_string(),
            BasicConcreteType::UInt16 => "uint16".to_string(),
            BasicConcreteType::UInt32 => "uint32".to_string(),
            BasicConcreteType::UInt64 => "uint64".to_string(),
            BasicConcreteType::UInt128 => "uint128".to_string(),
            BasicConcreteType::Float16 => "float16".to_string(),
            BasicConcreteType::Float32 => "float32".to_string(),
            BasicConcreteType::Float64 => "float64".to_string(),
            BasicConcreteType::Float128 => "float128".to_string(),
            BasicConcreteType::Char => "char".to_string(),
            BasicConcreteType::Bool => "bool".to_string(),
            BasicConcreteType::Void => "void".to_string(),
            BasicConcreteType::Null => "null".to_string(),
        },
        ConcreteType::Array(typed_array_type) => {
            let mut fmt = String::new();
            fmt.push_str(&format_concrete_type(*typed_array_type.element_type, format_symbol));
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
        ConcreteType::Const(concrete_type) => {
            format!("const {}", format_concrete_type(*concrete_type, format_symbol))
        }
        ConcreteType::Pointer(concrete_type) => {
            format!("{}*", format_concrete_type(*concrete_type, format_symbol))
        }
        ConcreteType::UnnamedStruct(unnamed_struct_type) => {
            format_unnamed_struct_type(&unnamed_struct_type, format_symbol)
        }
        ConcreteType::FuncType(func_type) => format_func_type(&func_type, format_symbol),
    }
}

pub fn format_func_type<'a>(func_type: &TypedFuncType, format_symbol: &(dyn Fn(SymbolID) -> String + 'a)) -> String {
    let mut params = func_type
        .params
        .list
        .iter()
        .map(|param| format_concrete_type(param.clone(), format_symbol))
        .collect::<Vec<String>>()
        .join(", ");

    if let Some(variadic) = func_type.params.variadic.clone() {
        match *variadic {
            TypedFuncTypeVariadicParams::UntypedCStyle => params.push_str(", ..."),
            TypedFuncTypeVariadicParams::Typed(concrete_type) => {
                params.push_str(&format!(", {}...", format_concrete_type(concrete_type, format_symbol)))
            }
        }
    }

    let ret = format_concrete_type(*func_type.return_type.clone(), format_symbol);
    format!("fn({}) {}", params, ret)
}

pub fn format_lambda<'a>(lambda: &TypedLambda, format_symbol: &(dyn Fn(SymbolID) -> String + 'a)) -> String {
    let mut params = lambda
        .params
        .list
        .iter()
        .map(|param| match param {
            TypedFuncParamKind::FuncParam(typed_func_param) => {
                format!(
                    "{}: {}",
                    typed_func_param.name,
                    format_concrete_type(typed_func_param.ty.clone(), format_symbol)
                )
            }
            TypedFuncParamKind::SelfModifier(..) => unreachable!(),
        })
        .collect::<Vec<String>>()
        .join(", ");

    if let Some(variadic) = lambda.params.variadic.clone() {
        match &variadic {
            TypedFuncVariadicParams::UntypedCStyle => params.push_str(", ..."),
            TypedFuncVariadicParams::Typed(identifier, concrete_type) => params.push_str(&format!(
                ", {}: ...{}",
                identifier,
                format_concrete_type(concrete_type.clone(), format_symbol)
            )),
        }
    }

    let ret = format_concrete_type(lambda.return_type.clone(), format_symbol);
    format!("fn({}) {} {{ ... }}", params, ret)
}
