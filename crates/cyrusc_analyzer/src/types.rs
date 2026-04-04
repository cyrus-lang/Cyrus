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

use crate::context::AnalysisContext;
use cyrusc_const_eval::fold::ConstFolder;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::StructDeclID,
    types::{PlainType, SemanticType, TypeDeclID, TypedArrayCapacity, TypedArrayType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn struct_field_contains_self_by_value(
        &self,
        field_type: &SemanticType,
        struct_decl_id: StructDeclID,
    ) -> bool {
        match field_type {
            SemanticType::Unresolved(_) => unreachable!(),

            SemanticType::Pointer(_) => {
                false // indirect
            }
            SemanticType::FuncType(_) => {
                // func type lowered as pointer-size value in codegen,
                // hence it's never harmful for self-recursion situations.
                false
            }
            SemanticType::Named(named_type) => named_type.decl_id == TypeDeclID::Struct(struct_decl_id),
            SemanticType::Const(inner) => self.struct_field_contains_self_by_value(inner, struct_decl_id),
            SemanticType::Array(array_type) => {
                self.struct_field_contains_self_by_value(&array_type.element_type, struct_decl_id)
            }
            SemanticType::Tuple(tuple_type) => tuple_type
                .elements
                .iter()
                .any(|ty| self.struct_field_contains_self_by_value(ty, struct_decl_id)),

            SemanticType::InterfaceType(_) => false,
            SemanticType::SelfType(_) => false,
            SemanticType::Plain(_) => false,
            SemanticType::GenericParam(_) => false,
        }
    }

    pub(crate) fn is_assignable_to(&mut self, rhs: SemanticType, lhs: SemanticType, loc: Loc) -> bool {
        match (rhs.const_inner().clone(), lhs.const_inner().clone()) {
            (SemanticType::Named(named_type1), SemanticType::Named(named_type2)) => named_type1 == named_type2,
            (SemanticType::Plain(basic_concrete_type1), SemanticType::Plain(basic_concrete_type2)) => {
                self.is_plain_type_assignable_to(basic_concrete_type1, basic_concrete_type2)
            }
            (SemanticType::Array(array_type1), SemanticType::Array(array_type2)) => {
                let valid_capacity = self.is_const_str_assignable_to_array(array_type1.clone(), array_type2.clone());

                valid_capacity && self.is_assignable_to(*array_type1.element_type, *array_type2.element_type, loc)
            }
            (SemanticType::Array(array_type), SemanticType::Pointer(inner)) => {
                // REVIEW: Maybe we don't need this really?
                // let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);
                // if array_type.element_type.is_const() && !inner.is_const() {
                //     self.reporter.report(Diag {
                //         level: DiagLevel::Warning,
                //         kind: Box::new(AnalyzerDiagKind::CannotDiscardConst {
                //             from: format_sema_type(value_type, fmt_symbol),
                //             to: format_sema_type(target_type, fmt_symbol),
                //         }),
                //         loc: Some(loc),
                //         hint: None,
                //     });
                // }

                // array-to-pointer decay
                self.is_assignable_to(*array_type.element_type, *inner, loc)
            }
            (SemanticType::Pointer(inner1), SemanticType::Pointer(inner2)) => {
                (inner1.is_void() || inner2.is_void()) || self.is_assignable_to(*inner1, *inner2, loc)
            }
            (SemanticType::FuncType(func_type1), SemanticType::FuncType(func_type2)) => func_type1 == func_type2,
            (SemanticType::Tuple(tuple_type1), SemanticType::Tuple(tuple_type2)) => tuple_type1 == tuple_type2,
            (SemanticType::InterfaceType(interface_type1), SemanticType::InterfaceType(interface_type2)) => {
                interface_type1.interface_decl_id == interface_type2.interface_decl_id
            }

            // allowed: null -> T*
            (SemanticType::Plain(PlainType::Null), SemanticType::Pointer(..)) => true,

            _ => false,
        }
    }

    pub(crate) fn is_plain_type_assignable_to(&self, value_type: PlainType, target_type: PlainType) -> bool {
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

    fn is_const_str_assignable_to_array(&mut self, value_type: TypedArrayType, target_type: TypedArrayType) -> bool {
        match (value_type.capacity, target_type.capacity) {
            (TypedArrayCapacity::Fixed(value_capacity_expr), TypedArrayCapacity::Fixed(target_capacity_expr)) => {
                let mut folder = ConstFolder::new(self);

                let value_capacity = folder.expr_as_const_int(&value_capacity_expr).unwrap();
                let target_capacity = folder.expr_as_const_int(&target_capacity_expr).unwrap();

                value_capacity == target_capacity
            }
            _ => false, // not valid
        }
    }

    // TODO: Would be used after implementing @cast.
    // pub(crate) fn is_explicit_cast_allowed(&mut self, value_type: SemanticType, target_type: SemanticType) -> bool {
    //     match (value_type, target_type) {
    //         // Any integer to any integer
    //         (SemanticType::PlainType(value), SemanticType::PlainType(target))
    //             if value.is_integer() && target.is_integer() =>
    //         {
    //             true
    //         }

    //         // Any float to any float
    //         (SemanticType::PlainType(value), SemanticType::PlainType(target))
    //             if value.is_float() && target.is_float() =>
    //         {
    //             true
    //         }

    //         // Any integer <-> float
    //         (SemanticType::PlainType(v), SemanticType::PlainType(t))
    //             if (v.is_integer() && t.is_float()) || (v.is_float() && t.is_integer()) =>
    //         {
    //             true
    //         }

    //         // Bool to anything integer-ish (common in C-style languages)
    //         (SemanticType::PlainType(PlainType::Bool), SemanticType::PlainType(target)) if target.is_integer() => true,

    //         // Char to integer and back
    //         (SemanticType::PlainType(PlainType::Char), SemanticType::PlainType(target)) if target.is_integer() => true,
    //         (SemanticType::PlainType(value), SemanticType::PlainType(PlainType::Char)) if value.is_integer() => true,

    //         // void* <-> intptr/uintptr
    //         (SemanticType::Pointer(..), SemanticType::PlainType(PlainType::IntPtr))
    //         | (SemanticType::Pointer(..), SemanticType::PlainType(PlainType::UIntPtr))
    //         | (SemanticType::PlainType(PlainType::IntPtr), SemanticType::Pointer(..))
    //         | (SemanticType::PlainType(PlainType::UIntPtr), SemanticType::Pointer(..)) => true,

    //         // NOTE
    //         //
    //         // At the semantic/typecheck layer we intentionally allow casts between
    //         // enums and integer/bool scalar types without validating the enum’s
    //         // underlying representation yet.
    //         //
    //         // The reason is architectural: the semantic layer only answers the
    //         // question “is this cast conceptually legal?”, not "how exactly should it
    //         // be lowered?”. Determining the actual integer representation of an enum
    //         // (its tag type) is a lowering concern that is handled
    //         // later in the CIR stage.
    //         //
    //         //
    //         // Therefore here we accept the following conversions:
    //         // enum -> integer/bool ^ integer/bool -> enum
    //         //
    //         //
    //         // as long as the non‑enum side is a scalar integer or bool. This keeps the
    //         // typechecker simple and avoids pulling enum layout/lowering knowledge
    //         // into this phase.
    //         //
    //         // During CIR lowering the expression:
    //         //
    //         // @cast(IntType, enumValue)
    //         //
    //         // is resolved using the enum's tag type. At that
    //         // point we perform the precise lowering (and any required compile‑time validation)
    //         // through the cast builtin implementation.
    //         //
    //         (SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(enum_id)), SemanticType::PlainType(plain_type)) => {
    //             let symbol_entry = self.query.lookup_symbol_entry(enum_id).unwrap();

    //             symbol_entry.as_enum().is_some() && plain_type.is_integer_or_bool()
    //         }
    //         (SemanticType::UnnamedEnum(_), SemanticType::PlainType(plain_type)) => plain_type.is_integer_or_bool(),
    //         (SemanticType::PlainType(plain_type), SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(enum_id))) => {
    //             let symbol_entry = self.query.lookup_symbol_entry(enum_id).unwrap();

    //             symbol_entry.as_enum().is_some() && plain_type.is_integer_or_bool()
    //         }
    //         (SemanticType::PlainType(plain_type), SemanticType::UnnamedEnum(_)) => plain_type.is_integer_or_bool(),
    //         // END
    //         _ => false,
    //     }
    // }
}
