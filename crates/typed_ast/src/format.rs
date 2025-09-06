use crate::{
    SymbolID,
    types::{BasicConcreteType, ConcreteType, ResolvedSymbol, TypedArrayCapacity},
};

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
                TypedArrayCapacity::Fixed(fixed) => {
                    fmt.push_str(&fixed.to_string());
                }
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
        ConcreteType::UnnamedStruct(typed_unnamed_struct_type) => {
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
    }
}
