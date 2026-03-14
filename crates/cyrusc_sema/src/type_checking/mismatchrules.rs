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
    generics::{
        mapping_ctx::mapping_ctx_eq_refcell,
        substitute::{substitute_enum_sig, substitute_struct_sig, substitute_union_sig},
    },
    sigs::{EnumSig, StructSig, UnionSig},
    stmts::TypedEnumVariant,
    types::{
        PlainType, ResolvedSymbol, SemanticType, TypedArrayCapacity, TypedArrayFixedCapacityValue, TypedArrayType,
        TypedUnnamedEnumType, TypedUnnamedEnumVariant, TypedUnnamedStructType, TypedUnnamedUnionType,
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
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        match (value_type.const_inner().clone(), target_type.const_inner().clone()) {
            (SemanticType::ResolvedSymbol(resolved_symbol1), SemanticType::ResolvedSymbol(resolved_symbol2)) => {
                resolved_symbol1 == resolved_symbol2
            }
            (SemanticType::PlainType(basic_concrete_type1), SemanticType::PlainType(basic_concrete_type2)) => {
                self.check_plain_type_mismatch(basic_concrete_type1, basic_concrete_type2)
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
            (SemanticType::UnnamedEnum(unnamed_enum1), SemanticType::UnnamedEnum(unnamed_enum2)) => {
                let mut variants = true;
                for (variant1, variant2) in unnamed_enum1.variants.iter().zip(unnamed_enum2.variants) {
                    let valid = match (variant1, variant2) {
                        (TypedUnnamedEnumVariant::Ident(ident1), TypedUnnamedEnumVariant::Ident(ident2)) => {
                            *ident1 == ident2
                        }
                        (
                            TypedUnnamedEnumVariant::Valued(ident1, expr1),
                            TypedUnnamedEnumVariant::Valued(ident2, expr2),
                        ) => *ident1 == ident2 && *expr1 == expr2,
                        (
                            TypedUnnamedEnumVariant::Variant(ident1, enum_valued_fields1),
                            TypedUnnamedEnumVariant::Variant(ident2, enum_valued_fields2),
                        ) => *ident1 == ident2 && *enum_valued_fields1 == enum_valued_fields2,
                        _ => false,
                    };

                    if !valid {
                        variants = false;
                        break;
                    }
                }
                variants
            }
            (
                SemanticType::UnnamedEnum(unnamed_enum_type),
                SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(struct_symbol_id)),
            ) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, struct_symbol_id)
                    .unwrap();
                let resolved_enum = sym.as_enum().unwrap();

                self.check_unnamed_enum_and_named_enum_type_mismatch(&unnamed_enum_type, &resolved_enum.enum_sig)
            }
            (
                SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(struct_symbol_id)),
                SemanticType::UnnamedEnum(unnamed_enum_type),
            ) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, struct_symbol_id)
                    .unwrap();
                let resolved_enum = sym.as_enum().unwrap();

                self.check_unnamed_enum_and_named_enum_type_mismatch(&unnamed_enum_type, &resolved_enum.enum_sig)
            }
            (SemanticType::UnnamedEnum(unnamed_enum_type), SemanticType::GenericType(generic_type)) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, generic_type.base)
                    .unwrap();

                match sym.as_enum().cloned() {
                    Some(mut resolved_enum) => {
                        resolved_enum.enum_sig = substitute_enum_sig(
                            self.mapping_ctx_arena.clone(),
                            &resolved_enum.enum_sig,
                            generic_type.mapping_ctx.clone(),
                        )
                        .unwrap();

                        self.check_unnamed_enum_and_named_enum_type_mismatch(
                            &unnamed_enum_type,
                            &resolved_enum.enum_sig,
                        )
                    }
                    None => false, // not compatible!
                }
            }
            (SemanticType::GenericType(generic_type), SemanticType::UnnamedEnum(unnamed_enum_type)) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, generic_type.base)
                    .unwrap();

                match sym.as_enum().cloned() {
                    Some(mut resolved_enum) => {
                        resolved_enum.enum_sig = substitute_enum_sig(
                            self.mapping_ctx_arena.clone(),
                            &resolved_enum.enum_sig,
                            generic_type.mapping_ctx.clone(),
                        )
                        .unwrap();

                        self.check_unnamed_enum_and_named_enum_type_mismatch(
                            &unnamed_enum_type,
                            &resolved_enum.enum_sig,
                        )
                    }
                    None => false, // not compatible!
                }
            }
            (SemanticType::UnnamedUnion(unnamed_union1), SemanticType::UnnamedUnion(unnamed_union2)) => {
                unnamed_union1.fields == unnamed_union2.fields
            }
            (
                SemanticType::UnnamedUnion(unnamed_union_type),
                SemanticType::ResolvedSymbol(ResolvedSymbol::Union(union_symbol_id)),
            ) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, union_symbol_id)
                    .unwrap();
                let resolved_union = sym.as_union().unwrap();

                self.check_unnamed_union_and_named_union_type_mismatch(&unnamed_union_type, &resolved_union.union_sig)
            }
            (
                SemanticType::ResolvedSymbol(ResolvedSymbol::Union(union_symbol_id)),
                SemanticType::UnnamedUnion(unnamed_union_type),
            ) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, union_symbol_id)
                    .unwrap();
                let resolved_union = sym.as_union().unwrap();

                self.check_unnamed_union_and_named_union_type_mismatch(&unnamed_union_type, &resolved_union.union_sig)
            }
            (SemanticType::UnnamedUnion(unnamed_union_type), SemanticType::GenericType(generic_type)) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, generic_type.base)
                    .unwrap();

                match sym.as_union().cloned() {
                    Some(mut resolved_union) => {
                        resolved_union.union_sig = substitute_union_sig(
                            self.mapping_ctx_arena.clone(),
                            &resolved_union.union_sig,
                            generic_type.mapping_ctx.clone(),
                        )
                        .unwrap();

                        self.check_unnamed_union_and_named_union_type_mismatch(
                            &unnamed_union_type,
                            &resolved_union.union_sig,
                        )
                    }
                    None => false, // not compatible!
                }
            }
            (SemanticType::GenericType(generic_type), SemanticType::UnnamedUnion(unnamed_union_type)) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, generic_type.base)
                    .unwrap();

                match sym.as_union().cloned() {
                    Some(mut resolved_union) => {
                        resolved_union.union_sig = substitute_union_sig(
                            self.mapping_ctx_arena.clone(),
                            &resolved_union.union_sig,
                            generic_type.mapping_ctx.clone(),
                        )
                        .unwrap();

                        self.check_unnamed_union_and_named_union_type_mismatch(
                            &unnamed_union_type,
                            &resolved_union.union_sig,
                        )
                    }
                    None => false, // not compatible!
                }
            }
            (SemanticType::UnnamedStruct(unnamed_struct1), SemanticType::UnnamedStruct(unnamed_struct2)) => {
                let is_packed = unnamed_struct1.repr_attr == unnamed_struct2.repr_attr;
                let is_aligned = unnamed_struct1.align == unnamed_struct2.align;

                let mut fields = true;
                for (field1, field2) in unnamed_struct1.fields.iter().zip(unnamed_struct2.fields) {
                    if !self.check_type_mismatch(scope_id_opt, *field1.ty.clone(), *field2.ty.clone(), loc.clone()) {
                        fields = false;
                        break;
                    }
                }
                is_packed && is_aligned && fields
            }
            (
                SemanticType::UnnamedStruct(unnamed_struct_type),
                SemanticType::ResolvedSymbol(ResolvedSymbol::Struct(struct_symbol_id)),
            ) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, struct_symbol_id)
                    .unwrap();
                let resolved_struct = sym.as_struct().unwrap();

                self.check_unnamed_struct_and_named_struct_type_mismatch(
                    &unnamed_struct_type,
                    &resolved_struct.struct_sig,
                )
            }
            (
                SemanticType::ResolvedSymbol(ResolvedSymbol::Struct(struct_symbol_id)),
                SemanticType::UnnamedStruct(unnamed_struct_type),
            ) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, struct_symbol_id)
                    .unwrap();
                let resolved_struct = sym.as_struct().unwrap();

                self.check_unnamed_struct_and_named_struct_type_mismatch(
                    &unnamed_struct_type,
                    &resolved_struct.struct_sig,
                )
            }
            (SemanticType::UnnamedStruct(unnamed_struct_type), SemanticType::GenericType(generic_type)) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, generic_type.base)
                    .unwrap();

                match sym.as_struct().cloned() {
                    Some(mut resolved_struct) => {
                        resolved_struct.struct_sig = substitute_struct_sig(
                            self.mapping_ctx_arena.clone(),
                            &resolved_struct.struct_sig,
                            generic_type.mapping_ctx.clone(),
                        )
                        .unwrap();

                        self.check_unnamed_struct_and_named_struct_type_mismatch(
                            &unnamed_struct_type,
                            &resolved_struct.struct_sig,
                        )
                    }
                    None => false, // not compatible!
                }
            }
            (SemanticType::GenericType(generic_type), SemanticType::UnnamedStruct(unnamed_struct)) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, generic_type.base)
                    .unwrap();

                match sym.as_struct().cloned() {
                    Some(mut resolved_struct) => {
                        resolved_struct.struct_sig = substitute_struct_sig(
                            self.mapping_ctx_arena.clone(),
                            &resolved_struct.struct_sig,
                            generic_type.mapping_ctx.clone(),
                        )
                        .unwrap();

                        self.check_unnamed_struct_and_named_struct_type_mismatch(
                            &unnamed_struct,
                            &resolved_struct.struct_sig,
                        )
                    }
                    None => false, // not compatible!
                }
            }
            (SemanticType::GenericType(generic_type1), SemanticType::GenericType(generic_type2)) => {
                mapping_ctx_eq_refcell(
                    self.mapping_ctx_arena.clone(),
                    &generic_type1.generic_params,
                    &generic_type1.mapping_ctx,
                    &generic_type2.generic_params,
                    &generic_type2.mapping_ctx,
                )
            }
            (SemanticType::FuncType(func_type1), SemanticType::FuncType(func_type2)) => func_type1 == func_type2,
            (SemanticType::Tuple(tuple_type1), SemanticType::Tuple(tuple_type2)) => tuple_type1 == tuple_type2,
            (SemanticType::DynamicType(dynamic_type), SemanticType::Interface(interface_type)) => {
                dynamic_type.interface_symbol_id == interface_type.symbol_id
            }
            (SemanticType::Interface(interface_type1), SemanticType::Interface(interface_type2)) => {
                interface_type1.symbol_id == interface_type2.symbol_id
            }
            (SemanticType::PlainType(PlainType::Null), SemanticType::Pointer(..)) => true,
            _ => false,
        }
    }

    fn check_unnamed_union_and_named_union_type_mismatch(
        &self,
        unnamed_union_type: &TypedUnnamedUnionType,
        union_sig: &UnionSig,
    ) -> bool {
        let fields = unnamed_union_type
            .fields
            .iter()
            .zip(&union_sig.fields)
            .any(|(field1, field2)| *field1.ty == field2.ty);

        unnamed_union_type.repr_attr == union_sig.modifiers.repr_attr
            && unnamed_union_type.align == union_sig.align
            && fields
    }

    fn check_unnamed_enum_and_named_enum_type_mismatch(
        &self,
        unnamed_enum_type: &TypedUnnamedEnumType,
        enum_sig: &EnumSig,
    ) -> bool {
        enum_sig
            .variants
            .iter()
            .zip(&unnamed_enum_type.variants)
            .any(
                |(named_variant, unnamed_variant)| match (named_variant, unnamed_variant) {
                    (TypedEnumVariant::Ident(ident1), TypedUnnamedEnumVariant::Ident(ident2)) => ident1 == ident2,
                    (TypedEnumVariant::Valued(ident1, expr1), TypedUnnamedEnumVariant::Valued(ident2, expr2)) => {
                        ident1 == ident2 && expr1 == expr2
                    }
                    (
                        TypedEnumVariant::Variant(ident1, valued_fields1),
                        TypedUnnamedEnumVariant::Variant(ident2, valued_fields2),
                    ) => {
                        let valued_fields = valued_fields1
                            .iter()
                            .zip(valued_fields2)
                            .any(|(field1, field2)| field1.ty == field2.ty);

                        ident1 == ident2 && valued_fields
                    }
                    _ => false,
                },
            )
    }

    fn check_unnamed_struct_and_named_struct_type_mismatch(
        &self,
        unnamed_struct: &TypedUnnamedStructType,
        struct_sig: &StructSig,
    ) -> bool {
        let unnamed_struct_fields = unnamed_struct
            .fields
            .iter()
            .map(|field| *field.ty.clone())
            .collect::<Vec<_>>();

        let named_struct_fields = struct_sig
            .fields
            .iter()
            .map(|field| field.ty.clone())
            .collect::<Vec<_>>();

        unnamed_struct.repr_attr == struct_sig.modifiers.repr_attr
            && unnamed_struct.align == struct_sig.align
            && unnamed_struct_fields == named_struct_fields
    }

    pub(crate) fn check_plain_type_mismatch(&self, value_type: PlainType, target_type: PlainType) -> bool {
        use PlainType::*;

        match (value_type, target_type) {
            // Same plain type is always compatible
            (a, b) if a == b => true,

            // Lower rank integer value is always compatible if both are signed/unsigned
            (a, b) if a.is_integer() && b.is_integer() => {
                let signedness = (a.is_signed() && b.is_signed()) || (!a.is_signed() && !b.is_signed());

                if let (Some(rank1), Some(rank2)) = (PlainType::plain_type_rank(&a), PlainType::plain_type_rank(&b)) {
                    // target value must have lower rank and signedness be valid
                    rank1 <= rank2 && signedness
                } else {
                    false
                }
            }

            // Lower rank float value is always compatible
            (a, b) if a.is_float() && b.is_float() => {
                if let (Some(rank1), Some(rank2)) = (PlainType::plain_type_rank(&a), PlainType::plain_type_rank(&b)) {
                    rank1 <= rank2
                } else {
                    false
                }
            }

            // Char To Integer
            (value_type @ Char, target_type) => {
                let is_integer = target_type.is_integer();

                if let (Some(rank1), Some(rank2)) = (
                    PlainType::plain_type_rank(&value_type),
                    PlainType::plain_type_rank(&target_type),
                ) {
                    rank1 <= rank2 && is_integer
                } else {
                    false
                }
            }

            // Integer to Char
            (value_type, target_type @ Char) => {
                if let (Some(rank1), Some(rank2)) = (
                    PlainType::plain_type_rank(&value_type),
                    PlainType::plain_type_rank(&target_type),
                ) {
                    rank1 <= rank2
                } else {
                    false
                }
            }

            (Bool, Int8 | UInt8) | (Int8 | UInt8, Bool) => true,

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

    pub(crate) fn check_explicit_typecast(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        value_type: SemanticType,
        target_type: SemanticType,
    ) -> bool {
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

            // NOTE
            //
            // At the semantic/typecheck layer we intentionally allow casts between
            // enums and integer/bool scalar types without validating the enum’s
            // underlying representation yet.
            //
            // The reason is architectural: the semantic layer only answers the
            // question “is this cast conceptually legal?”, not "how exactly should it
            // be lowered?”. Determining the actual integer representation of an enum
            // (its tag type) is a lowering concern that is handled
            // later in the CIR stage.
            //
            //
            // Therefore here we accept the following conversions:
            // enum -> integer/bool ^ integer/bool -> enum
            //
            //
            // as long as the non‑enum side is a scalar integer or bool. This keeps the
            // typechecker simple and avoids pulling enum layout/lowering knowledge
            // into this phase.
            //
            // During CIR lowering the expression:
            //
            // @cast(IntType, enumValue)
            //
            // is resolved using the enum's tag type. At that
            // point we perform the precise lowering (and any required compile‑time validation)
            // through the cast builtin implementation.
            //
            (
                SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(enum_symbol_id)),
                SemanticType::PlainType(plain_type),
            ) => {
                let scope_opt =
                    scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, enum_symbol_id)
                    .unwrap();

                sym.as_enum().is_some() && plain_type.is_integer_or_bool()
            }
            (SemanticType::UnnamedEnum(_), SemanticType::PlainType(plain_type)) => plain_type.is_integer_or_bool(),
            (
                SemanticType::PlainType(plain_type),
                SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(enum_symbol_id)),
            ) => {
                let scope_opt =
                    scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, enum_symbol_id)
                    .unwrap();

                sym.as_enum().is_some() && plain_type.is_integer_or_bool()
            }
            (SemanticType::PlainType(plain_type), SemanticType::UnnamedEnum(_)) => plain_type.is_integer_or_bool(),
            // END
            _ => false,
        }
    }
}
