// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, env::generic_env::GenericEnv};
use cyrusc_const_eval::fold::ConstFolder;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::cir::lower::lower_enum_type;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::{EnumDecl, StructDecl, TypedefDeclID, UnionDecl},
    exprs::TypedExpr,
    format::format_sema_type,
    stmts::{TypedEnumVariant, TypedFuncTypeParams, TypedFuncTypeVariadicParam, TypedTypeArg, TypedTypeArgs},
    types::{
        InterfaceObjectType, NamedType, PlainType, SemaType, TypeDeclID, TypedArrayCapacity, TypedArrayType,
        TypedFuncType, TypedTupleType,
    },
};

macro_rules! with_typedef_expansion {
    ($ctx:expr, $id:expr, $body:block) => {{
        match $ctx.push_typedef_expansion($id) {
            Ok(_) => {
                let result = (|| $body)();
                $ctx.pop_typedef_expansion();
                result
            }
            Err(cycle) => Err(cycle),
        }
    }};
}

impl<'a> AnalysisContext<'a> {
    pub(crate) fn sema_type_contains_self_by_value(&self, field_type: &SemaType, named_type: NamedType) -> bool {
        match field_type {
            SemaType::Unresolved(_) => unreachable!(),

            SemaType::Named(_named_type) => *_named_type == named_type,

            SemaType::Pointer(_) => {
                false // indirect
            }

            SemaType::FuncType(_) => {
                // func type lowered as pointer-size value in codegen,
                // hence it's never harmful for self-recursion situations.
                false
            }

            SemaType::Const(inner) => self.sema_type_contains_self_by_value(inner, named_type),

            SemaType::Array(array_type) => self.sema_type_contains_self_by_value(&array_type.element_type, named_type),

            SemaType::Tuple(tuple_type) => tuple_type
                .elements
                .iter()
                .any(|(ty, _)| self.sema_type_contains_self_by_value(ty, named_type.clone())),

            SemaType::InterfaceObject(_)
            | SemaType::SelfType(_)
            | SemaType::Plain(_)
            | SemaType::GenericParam(_)
            | SemaType::InferVar(_)
            | SemaType::Placeholder
            | SemaType::Err(_) => false,
        }
    }

    pub(crate) fn is_assignable_to(&mut self, mut rhs_type: SemaType, mut lhs_type: SemaType, loc: Loc) -> bool {
        lhs_type = self.expand_sema_type(lhs_type, loc);
        rhs_type = self.expand_sema_type(rhs_type, loc);

        match (rhs_type.const_inner().clone(), lhs_type.const_inner().clone()) {
            (SemaType::Err(_), _) | (_, SemaType::Err(_)) => {
                return true;
            }

            (SemaType::Plain(plain_type1), SemaType::Plain(plain_type2)) => {
                self.is_plain_type_assignable_to(plain_type1, plain_type2)
            }
            (SemaType::InterfaceObject(interface_object1), SemaType::InterfaceObject(interface_object2)) => self
                .is_named_type_assignable_to(interface_object1.interface_type, interface_object2.interface_type, loc),
            (SemaType::InterfaceObject(interface_object), SemaType::Named(named_type2)) => {
                self.is_named_type_assignable_to(interface_object.interface_type, named_type2, loc)
            }
            (SemaType::Named(named_type1), SemaType::Named(named_type2)) => {
                self.is_named_type_assignable_to(named_type1, named_type2, loc)
            }

            (SemaType::Array(array_type1), SemaType::Array(array_type2)) => {
                let valid_capacity = self.is_const_str_assignable_to_array(array_type1.clone(), array_type2.clone());

                valid_capacity && self.is_assignable_to(*array_type1.element_type, *array_type2.element_type, loc)
            }
            // array-to-pointer decay
            (SemaType::Array(array_type), SemaType::Pointer(inner)) => {
                self.is_assignable_to(*array_type.element_type, *inner, loc)
            }
            (SemaType::Pointer(inner1), SemaType::Pointer(inner2)) => {
                (inner1.is_void() || inner2.is_void()) || self.is_assignable_to(*inner1, *inner2, loc)
            }
            (SemaType::Tuple(tuple_type1), SemaType::Tuple(tuple_type2)) => tuple_type1 == tuple_type2,
            (SemaType::FuncType(func_type1), SemaType::FuncType(func_type2)) => func_type1 == func_type2,

            // allowed: null -> T*
            (SemaType::Plain(PlainType::Null), SemaType::Pointer(..)) => true,

            _ => false,
        }
    }

    fn is_named_type_assignable_to(&mut self, named_type1: NamedType, named_type2: NamedType, loc: Loc) -> bool {
        match (named_type1.type_decl_id, named_type2.type_decl_id) {
            (TypeDeclID::Struct(id1), TypeDeclID::Struct(id2)) => {
                if id1 == id2 {
                    return true;
                }

                let decl1 = self.decl_tables.struct_decl(id1);
                let decl2 = self.decl_tables.struct_decl(id2);

                let env1 = GenericEnv::from_type_args(decl1.generic_params.clone(), &named_type1.type_args);
                let env2 = GenericEnv::from_type_args(decl2.generic_params.clone(), &named_type2.type_args);

                self.is_struct_decl_assignable_to(&decl1, &decl2, env1, env2, loc)
            }
            (TypeDeclID::Union(id1), TypeDeclID::Union(id2)) => {
                if id1 == id2 {
                    return true;
                }

                let decl1 = self.decl_tables.union_decl(id1);
                let decl2 = self.decl_tables.union_decl(id2);

                let env1 = GenericEnv::from_type_args(decl1.generic_params.clone(), &named_type1.type_args);
                let env2 = GenericEnv::from_type_args(decl2.generic_params.clone(), &named_type2.type_args);

                self.is_union_decl_assignable_to(&decl1, &decl2, env1, env2, loc)
            }
            (TypeDeclID::Enum(id1), TypeDeclID::Enum(id2)) => {
                if id1 == id2 {
                    return true;
                }

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

            let Some(mut ty1) = self.normalize_sema_type(field1.ty.clone(), loc, 0) else {
                return false;
            };

            let Some(mut ty2) = self.normalize_sema_type(field2.ty.clone(), loc, 0) else {
                return false;
            };

            ty1 = env1.substitute_sema_type(&ty1);
            ty2 = env2.substitute_sema_type(&ty2);

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

            let Some(mut ty1) = self.normalize_sema_type(field1.ty.clone(), loc, 0) else {
                return false;
            };

            let Some(mut ty2) = self.normalize_sema_type(field2.ty.clone(), loc, 0) else {
                return false;
            };

            ty1 = env1.substitute_sema_type(&ty1);
            ty2 = env2.substitute_sema_type(&ty2);

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
                    let (Some(ty1), Some(ty2)) = (v1.ty.clone(), v2.ty.clone()) else {
                        return false;
                    };

                    let Some(mut ty1) = self.normalize_sema_type(ty1.clone(), loc, 0) else {
                        return false;
                    };

                    let Some(mut ty2) = self.normalize_sema_type(ty2.clone(), loc, 0) else {
                        return false;
                    };

                    ty1 = env1.substitute_sema_type(&ty1);
                    ty2 = env2.substitute_sema_type(&ty2);

                    if !self.is_assignable_to(ty1, ty2, loc) {
                        return false;
                    }
                }

                (TypedEnumVariant::Tuple { fields: f1, .. }, TypedEnumVariant::Tuple { fields: f2, .. }) => {
                    if f1.len() != f2.len() {
                        return false;
                    }

                    for (t1, t2) in f1.iter().zip(f2) {
                        let Some(mut ty1) = self.normalize_sema_type(t1.ty.clone(), loc, 0) else {
                            return false;
                        };

                        let Some(mut ty2) = self.normalize_sema_type(t2.ty.clone(), loc, 0) else {
                            return false;
                        };

                        ty1 = env1.substitute_sema_type(&ty1);
                        ty2 = env2.substitute_sema_type(&ty2);

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

                        let Some(mut ty1) = self.normalize_sema_type(field1.ty.clone(), loc, 0) else {
                            return false;
                        };

                        let Some(mut ty2) = self.normalize_sema_type(field2.ty.clone(), loc, 0) else {
                            return false;
                        };

                        ty1 = env1.substitute_sema_type(&ty1);
                        ty2 = env2.substitute_sema_type(&ty2);

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
                let mut folder = ConstFolder::new(self, &self.decl_tables, self.target, self.tctx.clone(), self);

                let value_capacity = folder.expr_as_const_int(&value_capacity_expr, self).unwrap();
                let target_capacity = folder.expr_as_const_int(&target_capacity_expr, self).unwrap();

                value_capacity == target_capacity
            }
            _ => false, // not valid
        }
    }

    pub(crate) fn is_explicit_cast_allowed(&mut self, value_type: SemaType, target_type: SemaType, loc: Loc) -> bool {
        if self.is_assignable_to(value_type.clone(), target_type.clone(), loc) {
            // it's castable, if it's assignable normally
            return true;
        }

        match (value_type, target_type) {
            // Any integer to any integer
            (SemaType::Plain(value), SemaType::Plain(target)) if value.is_integer() && target.is_integer() => true,

            // Any float to any float
            (SemaType::Plain(value), SemaType::Plain(target)) if value.is_float() && target.is_float() => true,

            // Any integer <-> float
            (SemaType::Plain(value), SemaType::Plain(target))
                if (value.is_integer() && target.is_float()) || (value.is_float() && target.is_integer()) =>
            {
                true
            }

            // Bool to anything integer-ish (common in C-style languages)
            (SemaType::Plain(PlainType::Bool), SemaType::Plain(target)) if target.is_integer() => true,

            // Char to integer and back
            (SemaType::Plain(PlainType::Char), SemaType::Plain(target)) if target.is_integer() => true,
            (SemaType::Plain(value), SemaType::Plain(PlainType::Char)) if value.is_integer() => true,

            // void* <-> intptr/uintptr
            (SemaType::Pointer(..), SemaType::Plain(PlainType::IntPtr))
            | (SemaType::Pointer(..), SemaType::Plain(PlainType::UIntPtr))
            | (SemaType::Plain(PlainType::IntPtr), SemaType::Pointer(..))
            | (SemaType::Plain(PlainType::UIntPtr), SemaType::Pointer(..)) => true,

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
            (SemaType::Named(named_type), SemaType::Plain(plain_type))
            | (SemaType::Plain(plain_type), SemaType::Named(named_type)) => {
                let Some(enum_decl_id) = named_type.type_decl_id.as_enum() else {
                    return false;
                };

                let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

                let ty = lower_enum_type(
                    &self.decl_tables,
                    self.target,
                    self.tctx.clone(),
                    enum_decl_id,
                    &enum_decl,
                    named_type.type_args.clone(),
                );

                let cir_enum_type = ty.as_enum(&self.tctx).unwrap();

                let tag_type = cir_enum_type.tag_type_or_infer_or_default();

                tag_type.is_integer_or_bool() && plain_type.is_integer_or_bool()
            }

            _ => false,
        }
    }
}

impl<'a> AnalysisContext<'a> {
    fn expand_typedef(&mut self, typedef_decl_id: TypedefDeclID, args: &TypedTypeArgs, loc: Loc) -> SemaType {
        let expansion_result: Result<SemaType, Vec<TypedefDeclID>> = with_typedef_expansion!(self, typedef_decl_id, {
            let typedef_decl = self.decl_tables.typedef_decl(typedef_decl_id);

            let generic_params = &typedef_decl.generic_params;
            let mut final_args = Vec::with_capacity(generic_params.len());

            for i in 0..generic_params.len() {
                if let Some(arg) = args.get(i) {
                    final_args.push(arg.clone());
                } else {
                    let infer = self.func_env.infer.as_mut().unwrap().new_var();
                    final_args.push(TypedTypeArg::Type(infer, loc));
                }
            }

            if args.len() > final_args.len() {
                let type_name = format_sema_type(
                    SemaType::Named(NamedType {
                        type_decl_id: TypeDeclID::Typedef(typedef_decl_id),
                        type_args: args.clone(),
                    }),
                    self.formatter,
                );

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::WrongNumberOfTypeArgs {
                        type_name,
                        expected: final_args.len(),
                        provided: args.len(),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
            }

            let generic_env = GenericEnv::from_type_args(generic_params.clone(), &TypedTypeArgs(final_args));

            let typedef_type = match self.normalize_and_check_type_formation(*typedef_decl.ty, loc, 0) {
                Some(ty) => ty,
                None => return Ok(SemaType::Err(loc)),
            };

            let substituted = generic_env.substitute_sema_type(&typedef_type);

            Ok(self.expand_sema_type(substituted, loc))
        });

        match expansion_result {
            Ok(ty) => ty,
            Err(path) => {
                let canonical = canonicalize_typedef_cycle(&path);

                if self.reported_typedef_cycles.insert(canonical.clone()) {
                    let mut ids = canonical.clone();
                    ids.push(canonical[0]);

                    let cycle_str = ids
                        .iter()
                        .map(|&id| self.formatter.format_type_decl(TypeDeclID::Typedef(id)))
                        .collect::<Vec<_>>()
                        .join(" -> ");

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::CyclicTypeDefinition { symbol_name: cycle_str }),
                        loc: Some(loc),
                        hint: Some(
                            "Cycles in type definitions are not allowed as they result in infinite recursion."
                                .to_string(),
                        ),
                    });
                }

                SemaType::Err(loc)
            }
        }
    }

    pub(crate) fn expand_sema_type(&mut self, ty: SemaType, loc: Loc) -> SemaType {
        match &ty {
            SemaType::InferVar(_) => ty,
            SemaType::Placeholder => ty,
            SemaType::Named(named_type) => match &named_type.type_decl_id {
                TypeDeclID::Typedef(typedef_decl_id) => {
                    self.expand_typedef(*typedef_decl_id, &named_type.type_args, loc)
                }
                _ => {
                    let type_args = named_type
                        .type_args
                        .iter()
                        .map(|type_arg| match type_arg {
                            TypedTypeArg::Type(ty, loc) => {
                                TypedTypeArg::Type(self.expand_sema_type(ty.clone(), *loc), *loc)
                            }
                            TypedTypeArg::Infer => TypedTypeArg::Infer,
                        })
                        .collect();

                    SemaType::Named(NamedType {
                        type_decl_id: named_type.type_decl_id,
                        type_args,
                    })
                }
            },
            SemaType::Pointer(inner) => SemaType::Pointer(Box::new(self.expand_sema_type(*inner.clone(), loc))),
            SemaType::Const(inner) => SemaType::Const(Box::new(self.expand_sema_type(*inner.clone(), loc))),
            SemaType::Array(array) => SemaType::Array(TypedArrayType {
                element_type: Box::new(self.expand_sema_type(*array.element_type.clone(), loc)),
                capacity: array.capacity.clone(),
                loc: array.loc,
            }),
            SemaType::Tuple(tuple) => {
                let elements = tuple
                    .elements
                    .clone()
                    .into_iter()
                    .map(|(ty, loc)| (self.expand_sema_type(ty, loc), loc))
                    .collect();

                SemaType::Tuple(TypedTupleType {
                    elements,
                    loc: tuple.loc,
                })
            }
            SemaType::FuncType(func) => {
                let params = TypedFuncTypeParams {
                    list: func
                        .params
                        .list
                        .clone()
                        .into_iter()
                        .map(|ty| self.expand_sema_type(ty, loc))
                        .collect(),

                    variadic: func.params.variadic.clone().map(|variadic| {
                        Box::new(match *variadic {
                            TypedFuncTypeVariadicParam::UntypedCStyle => TypedFuncTypeVariadicParam::UntypedCStyle,
                            TypedFuncTypeVariadicParam::Typed(ty) => {
                                TypedFuncTypeVariadicParam::Typed(self.expand_sema_type(ty, loc))
                            }
                        })
                    }),
                };
                let ret_type = Box::new(self.expand_sema_type(*func.ret_type.clone(), loc));

                SemaType::FuncType(TypedFuncType {
                    params,
                    ret_type,
                    is_public: func.is_public,
                    loc: func.loc,
                })
            }
            SemaType::Plain(_)
            | SemaType::GenericParam(_)
            | SemaType::SelfType(_)
            | SemaType::Unresolved(_)
            | SemaType::InterfaceObject(_) => ty,

            SemaType::Err(_) => ty,
        }
    }
}

impl<'a> AnalysisContext<'a> {
    /// Pushes a typedef to the expansion stack.
    /// Returns `Err(Vec<TypedefDeclID>)` containing the detected cycle path if a cycle occurs.
    pub(crate) fn push_typedef_expansion(&mut self, id: TypedefDeclID) -> Result<(), Vec<TypedefDeclID>> {
        if let Some(index) = self.typedef_expansion_stack.iter().position(|&x| x == id) {
            let mut cycle_path = self.typedef_expansion_stack[index..].to_vec();
            cycle_path.push(id);
            return Err(cycle_path);
        }

        self.typedef_expansion_stack.push(id);
        Ok(())
    }

    #[inline]
    pub(crate) fn pop_typedef_expansion(&mut self) {
        self.typedef_expansion_stack.pop();
    }
}

fn canonicalize_typedef_cycle(path: &[TypedefDeclID]) -> Vec<TypedefDeclID> {
    // remove duplicated last element
    let mut cycle = path[..path.len() - 1].to_vec();

    // find smallest element to normalize rotation
    let min_index = cycle
        .iter()
        .enumerate()
        .min_by_key(|(_, id)| *id)
        .map(|(i, _)| i)
        .unwrap();

    cycle.rotate_left(min_index);

    cycle
}

impl<'a> AnalysisContext<'a> {
    pub(crate) fn coerce_interface_as_interface_object_if_possible(
        &mut self,
        ty: &SemaType,
        expr: &TypedExpr,
    ) -> SemaType {
        let Some(expr_type) = &expr.ty else {
            return ty.clone();
        };

        let Some(interface_object) = expr_type.as_interface_object() else {
            return ty.clone();
        };

        // recursively substitute interface types that are compatible with interface-object
        fn coerce_recursively(
            this: &mut AnalysisContext,
            ty: &SemaType,
            interface_object: &InterfaceObjectType,
            loc: Loc,
        ) -> SemaType {
            match ty {
                SemaType::Named(named_type) => {
                    if this.is_named_type_assignable_to(
                        named_type.clone(),
                        interface_object.interface_type.clone(),
                        loc,
                    ) {
                        SemaType::InterfaceObject(interface_object.clone())
                    } else {
                        ty.clone()
                    }
                }

                SemaType::Const(inner) => coerce_recursively(this, inner, interface_object, loc),
                SemaType::Pointer(inner) => coerce_recursively(this, inner, interface_object, loc),
                SemaType::Array(array_type) => {
                    let element_type = Box::new(coerce_recursively(
                        this,
                        &array_type.element_type,
                        interface_object,
                        loc,
                    ));

                    SemaType::Array(TypedArrayType {
                        element_type,
                        capacity: array_type.capacity.clone(),
                        loc: array_type.loc,
                    })
                }
                SemaType::Tuple(tuple_type) => {
                    let elements = tuple_type
                        .elements
                        .iter()
                        .map(|(ty, loc)| (coerce_recursively(this, ty, interface_object, *loc), *loc))
                        .collect();

                    SemaType::Tuple(TypedTupleType {
                        elements,
                        loc: tuple_type.loc,
                    })
                }
                SemaType::FuncType(func_type) => {
                    let params = TypedFuncTypeParams {
                        list: func_type
                            .params
                            .list
                            .iter()
                            .map(|ty| coerce_recursively(this, ty, interface_object, loc))
                            .collect(),
                        variadic: func_type.params.variadic.clone().and_then(|variadic| {
                            Some(match *variadic {
                                TypedFuncTypeVariadicParam::UntypedCStyle => {
                                    Box::new(TypedFuncTypeVariadicParam::UntypedCStyle)
                                }
                                TypedFuncTypeVariadicParam::Typed(ty) => Box::new(TypedFuncTypeVariadicParam::Typed(
                                    coerce_recursively(this, &ty, interface_object, loc),
                                )),
                            })
                        }),
                    };

                    let ret_type = coerce_recursively(this, &func_type.ret_type, interface_object, loc);

                    SemaType::FuncType(TypedFuncType {
                        params,
                        ret_type: Box::new(ret_type),
                        is_public: func_type.is_public,
                        loc: func_type.loc,
                    })
                }

                // existing interface-object remains same (don't touch it)
                SemaType::InterfaceObject(_) => ty.clone(),

                SemaType::Plain(_) => ty.clone(),

                SemaType::SelfType(_)
                | SemaType::GenericParam(_)
                | SemaType::InferVar(_)
                | SemaType::Unresolved(_)
                | SemaType::Err(_)
                | SemaType::Placeholder => ty.clone(),
            }
        }

        coerce_recursively(self, ty, interface_object, expr.loc)
    }

    pub(crate) fn check_type_formation(&mut self, ty: SemaType, loc: Loc) -> Option<SemaType> {
        fn check_recursively(this: &mut AnalysisContext, ty: &SemaType, loc: Loc, has_error: &mut bool) {
            if ty.count_const_layers() > 1 {
                this.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::RedundantConstQualifier),
                    loc: Some(loc),
                    hint: None,
                });
                *has_error = true;
            }

            match ty {
                SemaType::Named(named_type) => {
                    this.check_unexpected_type_args(named_type, loc);
                    for type_arg in named_type.type_args.iter() {
                        if let TypedTypeArg::Type(inner, _) = type_arg {
                            check_recursively(this, inner, loc, has_error);
                        }
                    }

                    if let Some(union_decl_id) = named_type.type_decl_id.as_union() {
                        let union_decl = this.decl_tables.union_decl(union_decl_id);

                        if union_decl.fields.is_empty() {
                            this.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::UnionTypeMustContainAtLeastOneField),
                                loc: Some(loc),
                                hint: None,
                            });
                            *has_error = true;
                        }
                    }
                }
                SemaType::Array(array_type) => {
                    if array_type.element_type.is_void() {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::VoidElementTypeNotAllowed),
                            loc: Some(array_type.loc),
                            hint: None,
                        });
                        *has_error = true;
                    }
                    check_recursively(this, &array_type.element_type, loc, has_error);
                }
                SemaType::FuncType(func_type) => {
                    for param in &func_type.params.list {
                        if param.is_void() {
                            this.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::VoidParameterType),
                                loc: Some(func_type.loc),
                                hint: None,
                            });
                            *has_error = true;
                        }
                        check_recursively(this, param, loc, has_error);
                    }
                    check_recursively(this, &func_type.ret_type, loc, has_error);
                }
                SemaType::Tuple(tuple_type) => {
                    for (element, _) in &tuple_type.elements {
                        if element.is_void() {
                            this.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::VoidTupleElementNotAllowed),
                                loc: Some(tuple_type.loc),
                                hint: None,
                            });
                            *has_error = true;
                        }
                        check_recursively(this, element, loc, has_error);
                    }
                }
                SemaType::SelfType(_) => {
                    if this.func_env.current_object.is_none() {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::SelfTypeOutsideOfAnObject),
                            loc: Some(loc),
                            hint: None,
                        });
                        *has_error = true;
                    }
                }
                SemaType::Const(inner) => check_recursively(this, inner, loc, has_error),
                SemaType::Pointer(inner) => check_recursively(this, inner, loc, has_error),

                SemaType::InterfaceObject(_) => {}
                SemaType::Plain(_) => {}

                SemaType::GenericParam(_)
                | SemaType::InferVar(_)
                | SemaType::Unresolved(_)
                | SemaType::Err(_)
                | SemaType::Placeholder => {}
            }
        }

        if ty.is_err() {
            return None;
        }

        let mut has_error = false;
        check_recursively(self, &ty, loc, &mut has_error);

        if has_error { None } else { Some(ty) }
    }

    pub(crate) fn check_type_arity(&mut self, ty: SemaType, loc: Loc) -> Option<()> {
        fn check_recursively(this: &mut AnalysisContext, ty: &SemaType, loc: Loc) -> bool {
            match ty {
                SemaType::Named(named_type) => {
                    // wrong number of args?
                    if this.check_missing_type_args(named_type, loc) {
                        return false;
                    }

                    // infer vars remaining?
                    if this.check_unresolved_infer_vars(named_type, loc) {
                        return false;
                    }

                    for arg in named_type.type_args.iter() {
                        match arg {
                            TypedTypeArg::Type(inner, inner_loc) => {
                                if !check_recursively(this, inner, *inner_loc) {
                                    return false;
                                }
                            }
                            TypedTypeArg::Infer => { /* infer is allowed, but NOT after monomorph */ }
                        }
                    }
                    true
                }

                SemaType::Const(inner) | SemaType::Pointer(inner) => check_recursively(this, inner, loc),
                SemaType::Array(array_type) => check_recursively(this, &array_type.element_type, array_type.loc),
                SemaType::FuncType(func_type) => {
                    for param_ty in &func_type.params.list {
                        if !check_recursively(this, param_ty, func_type.loc) {
                            return false;
                        }
                    }
                    check_recursively(this, &func_type.ret_type, func_type.loc)
                }
                SemaType::Tuple(tuple_type) => {
                    for (element, _) in &tuple_type.elements {
                        if !check_recursively(this, element, tuple_type.loc) {
                            return false;
                        }
                    }
                    true
                }

                SemaType::InterfaceObject(_) => true,
                SemaType::Plain(_) => true,

                SemaType::SelfType(_)
                | SemaType::GenericParam(_)
                | SemaType::InferVar(_)
                | SemaType::Unresolved(_)
                | SemaType::Err(_)
                | SemaType::Placeholder => true,
            }
        }

        if ty.is_err() {
            return None;
        }

        if check_recursively(self, &ty, loc) {
            Some(())
        } else {
            None
        }
    }

    fn check_unexpected_type_args(&self, named_type: &NamedType, loc: Loc) -> bool {
        let generic_params = match named_type.type_decl_id {
            TypeDeclID::Struct(id) => self.decl_tables.struct_decl(id).generic_params,
            TypeDeclID::Enum(id) => self.decl_tables.enum_decl(id).generic_params,
            TypeDeclID::Union(id) => self.decl_tables.union_decl(id).generic_params,
            TypeDeclID::Interface(id) => self.decl_tables.interface_decl(id).generic_params,
            TypeDeclID::Typedef(id) => self.decl_tables.typedef_decl(id).generic_params,
        };

        if generic_params.is_empty() && !named_type.type_args.is_empty() {
            let type_name = format_sema_type(SemaType::Named(named_type.clone()), self.formatter);

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

        let generic_params = match named_type.type_decl_id {
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
                let type_name = format_sema_type(SemaType::Named(named_type.clone()), self.formatter);

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

        let generic_params = match named_type.type_decl_id {
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
                        let type_name = format_sema_type(SemaType::Named(named_type.clone()), self.formatter);

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
                    let type_name = format_sema_type(SemaType::Named(named_type.clone()), self.formatter);

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
