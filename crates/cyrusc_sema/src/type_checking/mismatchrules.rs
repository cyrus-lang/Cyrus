use crate::analyze::AnalysisContext;
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_tast::{
    ScopeID,
    generics::mapping_ctx::mapping_ctx_eq_refcell,
    types::{PlainType, SemanticType, TypedArrayCapacity, TypedArrayFixedCapacityValue, TypedArrayType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_type_mismatch(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        value_type: SemanticType,
        target_type: SemanticType,
        loc: SourceLoc,
    ) -> bool {
        match (
            value_type.get_const_inner().clone(),
            target_type.get_const_inner().clone(),
        ) {
            (SemanticType::ResolvedSymbol(resolved_symbol1), SemanticType::ResolvedSymbol(resolved_symbol2)) => {
                resolved_symbol1 == resolved_symbol2
            }
            (SemanticType::PlainType(basic_concrete_type1), SemanticType::PlainType(basic_concrete_type2)) => {
                self.check_basic_type_mismatch(basic_concrete_type1, basic_concrete_type2)
            }
            (SemanticType::Array(array_type1), SemanticType::Array(array_type2)) => {
                let valid_capacity = self.check_const_str_to_array_assignment(array_type1.clone(), array_type2.clone());

                valid_capacity
                    && self.check_type_mismatch(scope_id_opt, *array_type1.element_type, *array_type2.element_type, loc)
            }
            (SemanticType::Pointer(inner_concrete_type1), SemanticType::Pointer(inner_concrete_type2)) => {
                if let Some(arr_type) = inner_concrete_type1.as_array_type() {
                    *arr_type.element_type == *inner_concrete_type2
                } else {
                    (inner_concrete_type1.is_void() || inner_concrete_type2.is_void())
                        || self.check_type_mismatch(scope_id_opt, *inner_concrete_type1, *inner_concrete_type2, loc)
                }
            }
            (SemanticType::UnnamedStruct(unnamed_struct1), SemanticType::UnnamedStruct(unnamed_struct2)) => {
                let is_packed = unnamed_struct1.is_packed == unnamed_struct2.is_packed;
                let mut fields = true;
                for (field1, field2) in unnamed_struct1.fields.iter().zip(unnamed_struct2.fields) {
                    if !self.check_type_mismatch(
                        scope_id_opt,
                        *field1.field_ty.clone(),
                        *field2.field_ty.clone(),
                        loc.clone(),
                    ) {
                        fields = false;
                        break;
                    }
                }
                is_packed && fields
            }
            (SemanticType::GenericType(resolved_generic1), SemanticType::GenericType(resolved_generic2)) => {
                mapping_ctx_eq_refcell(&resolved_generic1.mapping_ctx, &resolved_generic2.mapping_ctx)
            }
            (SemanticType::FuncType(func_type1), SemanticType::FuncType(func_type2)) => func_type1 == func_type2,
            (SemanticType::Tuple(tuple_type1), SemanticType::Tuple(tuple_type2)) => tuple_type1 == tuple_type2,
            (SemanticType::PlainType(PlainType::Null), SemanticType::Pointer(..)) => true,
            _ => false,
        }
    }

    pub(crate) fn check_basic_type_mismatch(&self, value: PlainType, target: PlainType) -> bool {
        use PlainType::*;

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
            (PlainType::Int | PlainType::Int8 | PlainType::Int16 | PlainType::Int32, PlainType::IntPtr) => true,

            // Unsigned to intptr (less safe, maybe allow some)
            (PlainType::UInt | PlainType::UInt8 | PlainType::UInt16 | PlainType::UInt32, PlainType::UIntPtr) => true,

            (Null, Null) => true,

            // char to int
            (Char, Int8 | Int16 | Int32 | Int64 | Int128 | Int) => true,

            // int8 to char
            (Int8, Char) => true,

            // Bool to Int
            (Bool, Int8 | UInt8) => true,

            (Bool, Bool) => true,

            _ => false,
        }
    }

    fn check_const_str_to_array_assignment(&self, value_type: TypedArrayType, target_type: TypedArrayType) -> bool {
        match (value_type.capacity, target_type.capacity) {
            (
                TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(value_capacity)),
                TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(target_capacity)),
            ) => value_capacity == target_capacity,
            _ => false, // not valid
        }
    }

    pub(crate) fn check_explicit_typecast(&mut self, value_type: SemanticType, target_type: SemanticType) -> bool {
        match (value_type, target_type) {
            // Same type, always fine
            (a, b) if a == b => true,

            // Any integer to any integer
            (SemanticType::PlainType(value), SemanticType::PlainType(target))
                if value.is_integer() && target.is_integer() =>
            {
                true
            }

            // Any float to any float
            (SemanticType::PlainType(value), SemanticType::PlainType(target))
                if value.is_float() && target.is_float() =>
            {
                true
            }

            // Any integer <-> float
            (SemanticType::PlainType(v), SemanticType::PlainType(t))
                if (v.is_integer() && t.is_float()) || (v.is_float() && t.is_integer()) =>
            {
                true
            }

            // Bool to anything integer-ish (common in C-style languages)
            (SemanticType::PlainType(PlainType::Bool), SemanticType::PlainType(target)) if target.is_integer() => true,

            // Char to integer and back
            (SemanticType::PlainType(PlainType::Char), SemanticType::PlainType(target)) if target.is_integer() => true,
            (SemanticType::PlainType(value), SemanticType::PlainType(PlainType::Char)) if value.is_integer() => true,

            // void* <-> intptr/uintptr
            (SemanticType::Pointer(..), SemanticType::PlainType(PlainType::IntPtr))
            | (SemanticType::Pointer(..), SemanticType::PlainType(PlainType::UIntPtr))
            | (SemanticType::PlainType(PlainType::IntPtr), SemanticType::Pointer(..))
            | (SemanticType::PlainType(PlainType::UIntPtr), SemanticType::Pointer(..)) => true,

            _ => false,
        }
    }
}
