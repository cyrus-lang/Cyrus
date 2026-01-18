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
use crate::analyze::AnalysisContext;
use cyrusc_diagcentral::source_loc::SourceLoc;
use cyrusc_tast::{
    ScopeID,
    generics::{mapping_ctx::mapping_ctx_eq_refcell, substitute::substitute_struct_sig},
    mapping_ctx_arena,
    sigs::StructSig,
    types::{
        PlainType, ResolvedSymbol, SemanticType, TypedArrayCapacity, TypedArrayFixedCapacityValue, TypedArrayType,
        TypedUStructType,
    },
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_type_mismatch(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        value_type: SemanticType,
        target_type: SemanticType,
        loc: SourceLoc,
    ) -> bool {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

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
                (inner_concrete_type1.is_void() || inner_concrete_type2.is_void())
                    || self.check_type_mismatch(scope_id_opt, *inner_concrete_type1, *inner_concrete_type2, loc)
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
            (
                SemanticType::UnnamedStruct(unnamed_struct),
                SemanticType::ResolvedSymbol(ResolvedSymbol::NamedStruct(named_struct_symbol_id)),
            ) => {
                let named_struct = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, named_struct_symbol_id)
                    .unwrap();
                let resolved_struct = named_struct.as_struct().unwrap();

                self.check_unnamed_struct_and_named_struct_type_mismatch(&unnamed_struct, &resolved_struct.struct_sig)
            }
            (
                SemanticType::ResolvedSymbol(ResolvedSymbol::NamedStruct(named_struct_symbol_id)),
                SemanticType::UnnamedStruct(unnamed_struct),
            ) => {
                let named_struct = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, named_struct_symbol_id)
                    .unwrap();
                let resolved_struct = named_struct.as_struct().unwrap();

                self.check_unnamed_struct_and_named_struct_type_mismatch(&unnamed_struct, &resolved_struct.struct_sig)
            }
            (SemanticType::UnnamedStruct(unnamed_struct), SemanticType::GenericType(generic_type)) => {
                let named_struct = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, generic_type.base)
                    .unwrap();

                match named_struct.as_struct().cloned() {
                    Some(mut resolved_struct) => {
                        mapping_ctx_arena!(self, mapping_ctx_arena, {
                            resolved_struct.struct_sig = substitute_struct_sig(
                                &*mapping_ctx_arena,
                                &resolved_struct.struct_sig,
                                generic_type.mapping_ctx.clone(),
                            )
                            .unwrap();
                            self.check_unnamed_struct_and_named_struct_type_mismatch(
                                &unnamed_struct,
                                &resolved_struct.struct_sig,
                            )
                        })
                    }
                    None => false, // not compatible!
                }
            }
            (SemanticType::GenericType(generic_type), SemanticType::UnnamedStruct(unnamed_struct)) => {
                let named_struct = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, generic_type.base)
                    .unwrap();

                match named_struct.as_struct().cloned() {
                    Some(mut resolved_struct) => {
                        mapping_ctx_arena!(self, mapping_ctx_arena, {
                            resolved_struct.struct_sig = substitute_struct_sig(
                                &*mapping_ctx_arena,
                                &resolved_struct.struct_sig,
                                generic_type.mapping_ctx.clone(),
                            )
                            .unwrap();
                            self.check_unnamed_struct_and_named_struct_type_mismatch(
                                &unnamed_struct,
                                &resolved_struct.struct_sig,
                            )
                        })
                    }
                    None => false, // not compatible!
                }
            }
            (SemanticType::GenericType(generic_type1), SemanticType::GenericType(generic_type2)) => {
                mapping_ctx_eq_refcell(
                    self.mapping_ctx_arena.clone(),
                    &generic_type1.mapping_ctx,
                    &generic_type2.mapping_ctx,
                )
            }
            (SemanticType::FuncType(func_type1), SemanticType::FuncType(func_type2)) => func_type1 == func_type2,
            (SemanticType::Tuple(tuple_type1), SemanticType::Tuple(tuple_type2)) => tuple_type1 == tuple_type2,
            (SemanticType::DynamicType(dynamic_type1), SemanticType::DynamicType(dynamic_type2)) => {
                dynamic_type1.method_sigs == dynamic_type2.method_sigs
            }
            (SemanticType::PlainType(PlainType::Null), SemanticType::Pointer(..)) => true,
            _ => false,
        }
    }

    fn check_unnamed_struct_and_named_struct_type_mismatch(
        &self,
        unnamed_struct: &TypedUStructType,
        struct_sig: &StructSig,
    ) -> bool {
        let unnamed_struct_fields = unnamed_struct
            .fields
            .iter()
            .map(|field| *field.field_ty.clone())
            .collect::<Vec<_>>();

        let named_struct_fields = struct_sig
            .fields
            .iter()
            .map(|field| field.ty.clone())
            .collect::<Vec<_>>();

        unnamed_struct.is_packed == struct_sig.is_packed && unnamed_struct_fields == named_struct_fields
    }

    pub(crate) fn check_basic_type_mismatch(&self, value: PlainType, target: PlainType) -> bool {
        // REVIEW Consider to refactor this!
        // if value.is_integer() && target.is_integer() {
        //     let signedness = value.is_signed() && target.is_signed();;
        //     let valid_size = ...
        // }

        use PlainType::*;

        match (value, target) {
            // Same plain type is always compatible
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

            // Unsigned to uintptr
            (PlainType::UInt | PlainType::UInt8 | PlainType::UInt16 | PlainType::UInt32, PlainType::UIntPtr) => true,

            // char to integer
            (Char, plain_type) => plain_type.is_integer(),

            // int8 to char
            (Int8, Char) => true,

            // Bool to Int
            (Bool, Int8 | UInt8) => true,

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
