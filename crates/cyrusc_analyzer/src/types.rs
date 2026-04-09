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

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, env::generic_env::GenericEnv};
use cyrusc_const_eval::fold::ConstFolder;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::{EnumDecl, StructDecl, UnionDecl},
    format::format_sema_type,
    stmts::{TypedEnumVariant, TypedTypeArg},
    types::{NamedType, PlainType, SemanticType, TypeDeclID, TypedArrayCapacity, TypedArrayType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn sema_type_contains_self_by_value(&self, field_type: &SemanticType, named_type: NamedType) -> bool {
        match field_type {
            SemanticType::Unresolved(_) => unreachable!(),
            SemanticType::Named(_named_type) => *_named_type == named_type,
            SemanticType::Pointer(_) => {
                false // indirect
            }
            SemanticType::FuncType(_) => {
                // func type lowered as pointer-size value in codegen,
                // hence it's never harmful for self-recursion situations.
                false
            }
            SemanticType::Const(inner) => self.sema_type_contains_self_by_value(inner, named_type),
            SemanticType::Array(array_type) => {
                self.sema_type_contains_self_by_value(&array_type.element_type, named_type)
            }
            SemanticType::Tuple(tuple_type) => tuple_type
                .elements
                .iter()
                .any(|ty| self.sema_type_contains_self_by_value(ty, named_type.clone())),

            SemanticType::InterfaceType(_)
            | SemanticType::SelfType(_)
            | SemanticType::Plain(_)
            | SemanticType::GenericParam(_)
            | SemanticType::InferVar(_)
            | SemanticType::Placeholder => false,
        }
    }

    pub(crate) fn is_assignable_to(
        &mut self,
        mut rhs_type: SemanticType,
        mut lhs_type: SemanticType,
        loc: Loc,
    ) -> bool {
        lhs_type = self.expand_semantic_type(lhs_type, loc);
        rhs_type = self.expand_semantic_type(rhs_type, loc);

        match (rhs_type.const_inner().clone(), lhs_type.const_inner().clone()) {
            (SemanticType::Named(named_type1), SemanticType::Named(named_type2)) => {
                self.is_named_type_assignable_to(named_type1, named_type2, loc)
            }
            (SemanticType::Plain(plain_type1), SemanticType::Plain(plain_type2)) => {
                self.is_plain_type_assignable_to(plain_type1, plain_type2)
            }
            (SemanticType::Array(array_type1), SemanticType::Array(array_type2)) => {
                let valid_capacity = self.is_const_str_assignable_to_array(array_type1.clone(), array_type2.clone());

                valid_capacity && self.is_assignable_to(*array_type1.element_type, *array_type2.element_type, loc)
            }
            (SemanticType::Array(array_type), SemanticType::Pointer(inner)) => {
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

    fn is_named_type_assignable_to(&mut self, named_type1: NamedType, named_type2: NamedType, loc: Loc) -> bool {
        // check type args count
        if named_type1.type_args.len() != named_type2.type_args.len() {
            return false;
        }

        // check type args compatibility
        if !named_type1
            .type_args
            .0
            .iter()
            .zip(&named_type2.type_args.0)
            .all(|(type_arg1, type_arg2)| {
                let (TypedTypeArg::Type(sema_type1, _), TypedTypeArg::Type(sema_type2, _)) = (type_arg1, type_arg2)
                else {
                    return false;
                };

                self.is_assignable_to(sema_type1.clone(), sema_type2.clone(), loc)
            })
        {
            return false;
        }

        match (named_type1.decl_id, named_type2.decl_id) {
            (TypeDeclID::Struct(id1), TypeDeclID::Struct(id2)) => {
                let decl1 = self.decl_tables.struct_decl(id1);
                let decl2 = self.decl_tables.struct_decl(id2);

                let env1 = GenericEnv::from_type_args(decl1.generic_params.clone(), &named_type1.type_args);
                let env2 = GenericEnv::from_type_args(decl2.generic_params.clone(), &named_type2.type_args);

                self.is_struct_decl_assignable_to(&decl1, &decl2, env1, env2, loc)
            }
            (TypeDeclID::Union(id1), TypeDeclID::Union(id2)) => {
                let decl1 = self.decl_tables.union_decl(id1);
                let decl2 = self.decl_tables.union_decl(id2);

                let env1 = GenericEnv::from_type_args(decl1.generic_params.clone(), &named_type1.type_args);
                let env2 = GenericEnv::from_type_args(decl2.generic_params.clone(), &named_type2.type_args);

                self.is_union_decl_assignable_to(&decl1, &decl2, env1, env2, loc)
            }
            (TypeDeclID::Enum(id1), TypeDeclID::Enum(id2)) => {
                let decl1 = self.decl_tables.enum_decl(id1);
                let decl2 = self.decl_tables.enum_decl(id2);

                let env1 = GenericEnv::from_type_args(decl1.generic_params.clone(), &named_type1.type_args);
                let env2 = GenericEnv::from_type_args(decl2.generic_params.clone(), &named_type2.type_args);

                self.is_enum_decl_assignable_to(&decl1, &decl2, env1, env2, loc)
            }

            (TypeDeclID::Interface(id1), TypeDeclID::Interface(id2)) => id1 == id2,

            _ => false,
        }
    }

    fn is_struct_decl_assignable_to(
        &mut self,
        struct_decl1: &StructDecl,
        struct_decl2: &StructDecl,
        env1: GenericEnv,
        env2: GenericEnv,
        loc: Loc,
    ) -> bool {
        for field2 in &struct_decl2.fields {
            let Some(field1) = struct_decl1.lookup_field(&field2.name) else {
                return false;
            };

            let ty1 = env1.substitute_sema_type(&field1.ty);
            let ty2 = env2.substitute_sema_type(&field2.ty);

            if !self.is_assignable_to(ty1, ty2, loc) {
                return false;
            }
        }

        true
    }

    fn is_union_decl_assignable_to(
        &mut self,
        union_decl1: &UnionDecl,
        union_decl2: &UnionDecl,
        env1: GenericEnv,
        env2: GenericEnv,
        loc: Loc,
    ) -> bool {
        for field1 in &union_decl1.fields {
            let Some(field2) = union_decl2.lookup_field(&field1.name) else {
                return false;
            };

            let ty1 = env1.substitute_sema_type(&field1.ty);
            let ty2 = env2.substitute_sema_type(&field2.ty);

            if !self.is_assignable_to(ty1, ty2, loc) {
                return false;
            }
        }

        true
    }

    fn is_enum_decl_assignable_to(
        &mut self,
        enum_decl1: &EnumDecl,
        enum_decl2: &EnumDecl,
        env1: GenericEnv,
        env2: GenericEnv,
        loc: Loc,
    ) -> bool {
        if let (Some(tag1), Some(tag2)) = (&enum_decl1.tag_type, &enum_decl2.tag_type) {
            let tag1 = env1.substitute_sema_type(tag1);
            let tag2 = env2.substitute_sema_type(tag2);

            if !self.is_assignable_to(tag1, tag2, loc) {
                return false;
            }
        }

        if let (Some(a1), Some(a2)) = (enum_decl1.align, enum_decl2.align) {
            if a1 != a2 {
                return false;
            }
        }

        for variant1 in &enum_decl1.variants {
            let Some(variant2) = enum_decl2.lookup_variant(&variant1.ident().value) else {
                return false;
            };

            match (variant1, variant2) {
                (TypedEnumVariant::Unit(_), TypedEnumVariant::Unit(_)) => {}

                (TypedEnumVariant::Valued { value: v1, .. }, TypedEnumVariant::Valued { value: v2, .. }) => {
                    let (Some(sema_type1), Some(sema_type2)) = (v1.sema_type.clone(), v2.sema_type.clone()) else {
                        return false;
                    };

                    let ty1 = env1.substitute_sema_type(&sema_type1);
                    let ty2 = env2.substitute_sema_type(&sema_type2);

                    if !self.is_assignable_to(ty1, ty2, loc) {
                        return false;
                    }
                }

                (TypedEnumVariant::Tuple { fields: f1, .. }, TypedEnumVariant::Tuple { fields: f2, .. }) => {
                    if f1.len() != f2.len() {
                        return false;
                    }

                    for (t1, t2) in f1.iter().zip(f2) {
                        let ty1 = env1.substitute_sema_type(&t1.ty);
                        let ty2 = env2.substitute_sema_type(&t2.ty);

                        if !self.is_assignable_to(ty1, ty2, loc) {
                            return false;
                        }
                    }
                }

                (TypedEnumVariant::Struct { fields: f1, .. }, TypedEnumVariant::Struct { fields: f2, .. }) => {
                    for field2 in f2 {
                        let Some(field1) = f1.iter().find(|f| f.name == field2.name) else {
                            return false;
                        };

                        let ty1 = env1.substitute_sema_type(&field1.ty);
                        let ty2 = env2.substitute_sema_type(&field2.ty);

                        if !self.is_assignable_to(ty1, ty2, loc) {
                            return false;
                        }
                    }
                }

                _ => {
                    return false;
                }
            }
        }

        true
    }

    fn is_plain_type_assignable_to(&self, value_type: PlainType, target_type: PlainType) -> bool {
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

    // NOTE: Would be used after implementing @cast.
    pub(crate) fn is_explicit_cast_allowed(&mut self, value_type: SemanticType, target_type: SemanticType) -> bool {
        match (value_type, target_type) {
            // Any integer to any integer
            (SemanticType::Plain(value), SemanticType::Plain(target)) if value.is_integer() && target.is_integer() => {
                true
            }

            // Any float to any float
            (SemanticType::Plain(value), SemanticType::Plain(target)) if value.is_float() && target.is_float() => true,

            // Any integer <-> float
            (SemanticType::Plain(value), SemanticType::Plain(target))
                if (value.is_integer() && target.is_float()) || (value.is_float() && target.is_integer()) =>
            {
                true
            }

            // Bool to anything integer-ish (common in C-style languages)
            (SemanticType::Plain(PlainType::Bool), SemanticType::Plain(target)) if target.is_integer() => true,

            // Char to integer and back
            (SemanticType::Plain(PlainType::Char), SemanticType::Plain(target)) if target.is_integer() => true,
            (SemanticType::Plain(value), SemanticType::Plain(PlainType::Char)) if value.is_integer() => true,

            // void* <-> intptr/uintptr
            (SemanticType::Pointer(..), SemanticType::Plain(PlainType::IntPtr))
            | (SemanticType::Pointer(..), SemanticType::Plain(PlainType::UIntPtr))
            | (SemanticType::Plain(PlainType::IntPtr), SemanticType::Pointer(..))
            | (SemanticType::Plain(PlainType::UIntPtr), SemanticType::Pointer(..)) => true,

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
            (SemanticType::Named(named_type), SemanticType::Plain(plain_type)) => {
                let Some(enum_decl_id) = named_type.decl_id.as_enum() else {
                    return false;
                };

                let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

                // FIXME: Checking repr isn't enough, we must validate tag type.
                enum_decl.is_repr_c() && plain_type.is_integer_or_bool()
            }

            _ => false,
        }
    }
}

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_type_formation(&mut self, sema_type: SemanticType, loc: Loc) -> Option<SemanticType> {
        if sema_type.count_const_layers() > 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::RedundantConstQualifier),
                loc: Some(loc),
                hint: None,
            });
        }

        if sema_type.is_self_type() && self.func_env.current_object.is_none() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::SelfTypeOutsideOfAnObject),
                loc: Some(loc),
                hint: None,
            });
        }

        if let Some(named_type) = sema_type.as_named_type() {
            self.check_unexpected_type_args(named_type, loc);
        }

        Some(sema_type)
    }

    pub(crate) fn check_type_arity(&mut self, sema_type: SemanticType, loc: Loc) -> Option<SemanticType> {
        if let Some(named_type) = sema_type.as_named_type() {
            if self.check_missing_type_args(named_type, loc) {
                return None;
            }

            if self.check_unresolved_infer_vars(named_type, loc) {
                return None;
            }
        }

        Some(sema_type)
    }

    fn check_unexpected_type_args(&self, named_type: &NamedType, loc: Loc) -> bool {
        let generic_params = match named_type.decl_id {
            TypeDeclID::Struct(id) => self.decl_tables.struct_decl(id).generic_params,
            TypeDeclID::Enum(id) => self.decl_tables.enum_decl(id).generic_params,
            TypeDeclID::Union(id) => self.decl_tables.union_decl(id).generic_params,
            TypeDeclID::Interface(id) => self.decl_tables.interface_decl(id).generic_params,
            TypeDeclID::Typedef(id) => self.decl_tables.typedef_decl(id).generic_params,
        };

        if generic_params.is_empty() && !named_type.type_args.is_empty() {
            let type_name = format_sema_type(SemanticType::Named(named_type.clone()), self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs { type_name }),
                loc: Some(loc),
                hint: None,
            });
            return true;
        }

        false
    }

    fn check_missing_type_args(&self, named_type: &NamedType, loc: Loc) -> bool {
        let mut has_error = false;

        let generic_params = match named_type.decl_id {
            TypeDeclID::Struct(id) => self.decl_tables.struct_decl(id).generic_params,
            TypeDeclID::Enum(id) => self.decl_tables.enum_decl(id).generic_params,
            TypeDeclID::Union(id) => self.decl_tables.union_decl(id).generic_params,
            TypeDeclID::Interface(id) => self.decl_tables.interface_decl(id).generic_params,
            TypeDeclID::Typedef(id) => self.decl_tables.typedef_decl(id).generic_params,
        };

        for (i, param_id) in generic_params.iter().enumerate() {
            let Some(type_arg) = named_type.type_args.get(i) else {
                has_error = true;

                let param = self.decl_tables.generic_param(*param_id);
                let type_name = format_sema_type(SemanticType::Named(named_type.clone()), self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::MissingGenericArgument {
                        type_name,
                        param_name: param.name.as_string(),
                    }),
                    loc: Some(loc),
                    hint: None,
                });

                continue;
            };

            let generic_param = self.decl_tables.generic_param(*param_id);

            match type_arg {
                TypedTypeArg::Type(sema_ty, _) => {
                    if sema_ty.contains_generic_param() {
                        has_error = true;

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::UnresolvedGenericParameter {
                                param_name: generic_param.name.as_string(),
                            }),
                            loc: Some(loc),
                            hint: None,
                        });
                    }
                }
                TypedTypeArg::Infer => {
                    has_error = true;

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::UnresolvedGenericParameter {
                            param_name: generic_param.name.as_string(),
                        }),
                        loc: Some(loc),
                        hint: None,
                    });
                }
            }
        }

        has_error
    }

    fn check_unresolved_infer_vars(&self, named_type: &NamedType, loc: Loc) -> bool {
        let mut has_error = false;

        let generic_params = match named_type.decl_id {
            TypeDeclID::Struct(id) => self.decl_tables.struct_decl(id).generic_params,
            TypeDeclID::Enum(id) => self.decl_tables.enum_decl(id).generic_params,
            TypeDeclID::Union(id) => self.decl_tables.union_decl(id).generic_params,
            TypeDeclID::Interface(id) => self.decl_tables.interface_decl(id).generic_params,
            TypeDeclID::Typedef(id) => self.decl_tables.typedef_decl(id).generic_params,
        };

        for (i, param_id) in generic_params.iter().enumerate() {
            let Some(type_arg) = named_type.type_args.get(i) else {
                continue;
            };

            let generic_param = self.decl_tables.generic_param(*param_id);

            match type_arg {
                TypedTypeArg::Type(sema_ty, _) => {
                    if sema_ty.contains_infer_var() {
                        has_error = true;
                        let type_name = format_sema_type(SemanticType::Named(named_type.clone()), self.formatter);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::CannotInferGenericArgument {
                                type_name,
                                param_name: generic_param.name.as_string(),
                            }),
                            loc: Some(loc),
                            hint: None,
                        });
                    }
                }
                TypedTypeArg::Infer => {
                    has_error = true;
                    let type_name = format_sema_type(SemanticType::Named(named_type.clone()), self.formatter);

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::CannotInferGenericArgument {
                            type_name,
                            param_name: generic_param.name.as_string(),
                        }),
                        loc: Some(loc),
                        hint: None,
                    });
                }
            }
        }

        has_error
    }
}
