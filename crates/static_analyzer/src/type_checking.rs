use crate::context::AnalysisContext;
use typed_ast::{
    SymbolID,
    types::{
        BasicConcreteType::{self, *},
        ConcreteType, TypedArrayCapacity,
    },
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_type_mismatch(&self, value_type: ConcreteType, target_type: ConcreteType) -> bool {
        match (value_type, target_type) {
            (ConcreteType::BasicType(basic_concrete_type1), ConcreteType::BasicType(basic_concrete_type2)) => {
                self.check_basic_type_mismatch(basic_concrete_type1, basic_concrete_type2)
            }
            (ConcreteType::Const(inner_concrete_type1), ConcreteType::Const(inner_concrete_type2)) => {
                self.check_type_mismatch(*inner_concrete_type1, *inner_concrete_type2)
            }
            (ConcreteType::Const(inner_concrete_type1), concrete_type2) => {
                self.check_type_mismatch(*inner_concrete_type1, concrete_type2)
            }
            (concrete_type1, ConcreteType::Const(inner_concrete_type2)) => {
                self.check_type_mismatch(concrete_type1, *inner_concrete_type2)
            }
            (ConcreteType::Array(array_type1), ConcreteType::Array(array_type2)) => {
                let capacity = {
                    match (array_type1.capacity, array_type2.capacity) {
                        (TypedArrayCapacity::Fixed(size1), TypedArrayCapacity::Fixed(size2)) => size1 == size2,
                        (TypedArrayCapacity::Dynamic, TypedArrayCapacity::Dynamic) => true,
                        _ => false,
                    }
                };

                capacity && self.check_type_mismatch(*array_type1.element_type, *array_type2.element_type)
            }
            (ConcreteType::Pointer(inner_concrete_type1), ConcreteType::Pointer(inner_concrete_type2)) => {
                self.check_type_mismatch(*inner_concrete_type1, *inner_concrete_type2)
            }
            (ConcreteType::UnnamedStruct(unnamed_struct1), ConcreteType::UnnamedStruct(unnamed_struct2)) => {
                let packed = unnamed_struct1.packed == unnamed_struct2.packed;
                let mut fields = true;
                for (field1, field2) in unnamed_struct1.fields.iter().zip(unnamed_struct2.fields) {
                    if *field1 != field2 {
                        fields = false;
                        break;
                    }
                }
                packed && fields
            }
            (ConcreteType::Symbol(symbol_id), concrete_type) => {
                todo!();
            }
            (concrete_type, ConcreteType::Symbol(symbol_id)) => {
                todo!();
            }
            _ => false,
        }
    }

    pub(crate) fn check_type_conversion(&self, value: ConcreteType, target: ConcreteType) -> bool {
        todo!();
    }

    pub(crate) fn check_basic_type_mismatch(&self, value: BasicConcreteType, target: BasicConcreteType) -> bool {
        match (value, target) {
            // Same type is always compatible
            (a, b) if a == b => true,

            // Integer compatibility (widening is allowed)
            (Int8, Int16 | Int32 | Int64 | Int128 | Int) => true,
            (Int16, Int32 | Int64 | Int128 | Int) => true,
            (Int32, Int64 | Int128 | Int) => true,
            (Int64, Int128) => true,
            (Int, Int64 | Int128) => true,

            (UInt8, UInt16 | UInt32 | UInt64 | UInt128 | UInt) => true,
            (UInt16, UInt32 | UInt64 | UInt128 | UInt) => true,
            (UInt32, UInt64 | UInt128 | UInt) => true,
            (UInt64, UInt128) => true,
            (UInt, UInt64 | UInt128) => true,

            // Cross unsigned-to-signed conversions (only if target is wider)
            (UInt8, Int16 | Int32 | Int64 | Int128 | Int) => true,
            (UInt16, Int32 | Int64 | Int128 | Int) => true,
            (UInt32, Int64 | Int128 | Int) => true,
            (UInt64, Int128 | Int) => true,

            // Floating-point widening
            (Float16, Float32 | Float64 | Float128) => true,
            (Float32, Float64 | Float128) => true,
            (Float64, Float128) => true,

            // Pointer int compatibility (if same bit width)
            (UIntPtr, IntPtr) | (IntPtr, UIntPtr) => true,

            // Integer to intptr (safe if value fits)
            (
                BasicConcreteType::Int | BasicConcreteType::Int8 | BasicConcreteType::Int16 | BasicConcreteType::Int32,
                BasicConcreteType::IntPtr,
            ) => true,

            // Unsigned to intptr (less safe, maybe allow some)
            (
                BasicConcreteType::UInt
                | BasicConcreteType::UInt8
                | BasicConcreteType::UInt16
                | BasicConcreteType::UInt32,
                BasicConcreteType::UIntPtr,
            ) => true,

            (Null, Null) => true,

            // Char to Int
            (Char, Int8 | UInt8) => true,

            // Bool to Int
            (Bool, Int8 | UInt8) => true,

            _ => false,
        }
    }

    pub(crate) fn is_basic_concrete_type_integer(&self, basic_concrete_type: BasicConcreteType) -> bool {
        matches!(
            basic_concrete_type,
            BasicConcreteType::UIntPtr
                | BasicConcreteType::IntPtr
                | BasicConcreteType::SizeT
                | BasicConcreteType::Int
                | BasicConcreteType::Int8
                | BasicConcreteType::Int16
                | BasicConcreteType::Int32
                | BasicConcreteType::Int64
                | BasicConcreteType::Int128
                | BasicConcreteType::UInt
                | BasicConcreteType::UInt8
                | BasicConcreteType::UInt16
                | BasicConcreteType::UInt32
                | BasicConcreteType::UInt64
                | BasicConcreteType::UInt128
        )
    }

    pub(crate) fn is_basic_concrete_type_float(&self, basic_concrete_type: BasicConcreteType) -> bool {
        matches!(
            basic_concrete_type,
            BasicConcreteType::Float16
                | BasicConcreteType::Float32
                | BasicConcreteType::Float64
                | BasicConcreteType::Float128
        )
    }

    pub(crate) fn get_symbol_formatter(&self) -> Box<&dyn Fn(SymbolID) -> String> {
        Box::new(&move |symbol_id: SymbolID| -> String {
            todo!();
        })
    }
}
