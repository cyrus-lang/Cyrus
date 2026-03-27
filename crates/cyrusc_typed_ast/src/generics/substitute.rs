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
    generics::{mapping_ctx::GenericMappingCtx, mapping_ctx_arena::GenericMappingCtxArena},
    sigs::{EnumSig, FuncSig, StructSig, UnionSig},
    stmts::{
        TypedEnumValuedField, TypedEnumVariant, TypedFuncParamKind, TypedFuncParams, TypedFuncTypeParams,
        TypedFuncVariadicParams,
    },
    types::{
        SemanticType, TypedArrayType, TypedFuncType, TypedTupleType, TypedUnnamedStructType,
        TypedUnnamedStructTypeField,
    },
};
use std::rc::Rc;
use std::{
    cell::RefCell,
    sync::{Arc, Mutex},
};

/// Recursively substitute generic type parameters within a semantic type.
/// Performs substitution for every nested type form (array, pointer, tuple, etc.)
/// using the provided generic mapping context.
pub fn substitute_type(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    sema_type: SemanticType,
    mapping_ctx: Rc<RefCell<GenericMappingCtx>>,
) -> Option<SemanticType> {
    /// Small helper to apply a substitution callback on an inner type.
    fn sub<F>(inner: SemanticType, f: &F) -> Option<SemanticType>
    where
        F: Fn(SemanticType) -> Option<SemanticType>,
    {
        f(inner)
    }

    match sema_type {
        SemanticType::GenericParam(generic_param) => {
            let mut sema_type = {
                let mapping_ctx_ref = mapping_ctx.borrow();
                mapping_ctx_ref.resolve_with_name(mapping_ctx_arena, &generic_param.name.value)
            };

            if let Some(SemanticType::GenericParam(generic_param)) = sema_type {
                sema_type = generic_param.default.map(|sema_type| *sema_type);
            }

            sema_type
        }
        SemanticType::GenericType(mut generic_type) => {
            if let Some(type_args) = &mut generic_type.type_args {
                for type_arg in type_args {
                    *type_arg.ty_mut() =
                        substitute_type(mapping_ctx_arena.clone(), type_arg.ty().clone(), mapping_ctx.clone())?;
                }
            }
            Some(SemanticType::GenericType(generic_type))
        }
        SemanticType::Pointer(inner) => sub(*inner, &|t| {
            substitute_type(mapping_ctx_arena.clone(), t, mapping_ctx.clone())
        })
        .map(|t| SemanticType::Pointer(Box::new(t))),
        SemanticType::Array(array_type) => sub(*array_type.element_type, &|t| {
            substitute_type(mapping_ctx_arena.clone(), t, mapping_ctx.clone())
        })
        .map(|elem_ty| {
            SemanticType::Array(TypedArrayType {
                element_type: Box::new(elem_ty),
                capacity: array_type.capacity,
                loc: array_type.loc,
            })
        }),
        SemanticType::Const(inner) => sub(*inner, &|sema_type| {
            substitute_type(mapping_ctx_arena.clone(), sema_type, mapping_ctx.clone())
        })
        .map(|sema_type| sema_type.as_const()),
        SemanticType::Tuple(tuple_type) => {
            let list = tuple_type
                .elements
                .into_iter()
                .map(|t| substitute_type(mapping_ctx_arena.clone(), t, mapping_ctx.clone()))
                .collect::<Option<Vec<_>>>()?;
            Some(SemanticType::Tuple(TypedTupleType {
                elements: list,
                loc: tuple_type.loc,
            }))
        }
        SemanticType::FuncType(func_type) => {
            let params = func_type
                .params
                .list
                .into_iter()
                .map(|p| substitute_type(mapping_ctx_arena.clone(), p, mapping_ctx.clone()))
                .collect::<Option<Vec<_>>>()?;
            let ret_ty = Box::new(substitute_type(
                mapping_ctx_arena,
                *func_type.ret_type,
                mapping_ctx.clone(),
            )?);
            Some(SemanticType::FuncType(TypedFuncType {
                symbol_id: func_type.symbol_id,
                params: TypedFuncTypeParams {
                    list: params,
                    variadic: func_type.params.variadic,
                },
                is_public: func_type.is_public,
                ret_type: ret_ty,
                loc: func_type.loc,
            }))
        }
        SemanticType::UnnamedStruct(unnamed_struct_type) => {
            let fields = unnamed_struct_type
                .fields
                .iter()
                .map(|f| {
                    let inner = substitute_type(mapping_ctx_arena.clone(), *f.ty.clone(), mapping_ctx.clone())?;
                    Some(TypedUnnamedStructTypeField {
                        name: f.name.clone(),
                        ty: Box::new(inner),
                        loc: f.loc,
                    })
                })
                .collect::<Option<Vec<_>>>()?;

            Some(SemanticType::UnnamedStruct(TypedUnnamedStructType {
                fields,
                repr_attr: unnamed_struct_type.repr_attr.clone(),
                align: unnamed_struct_type.align.clone(),
                loc: unnamed_struct_type.loc,
            }))
        }
        other => Some(other),
    }
}

fn substitute_func_params(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    func_params: &TypedFuncParams,
    ctx: Rc<RefCell<GenericMappingCtx>>,
) -> Option<TypedFuncParams> {
    let list: Vec<TypedFuncParamKind> = func_params
        .list
        .iter()
        .map(|func_param_kind| match func_param_kind.clone() {
            TypedFuncParamKind::FuncParam(mut func_param) => {
                if let Some(sema_type) = substitute_type(mapping_ctx_arena.clone(), func_param.ty.clone(), ctx.clone())
                {
                    func_param.ty = sema_type;
                }
                TypedFuncParamKind::FuncParam(func_param)
            }
            TypedFuncParamKind::SelfModifier(mut self_modifier) => {
                self_modifier.ty = self_modifier
                    .ty
                    .and_then(|sema_type| substitute_type(mapping_ctx_arena.clone(), sema_type, ctx.clone()));
                TypedFuncParamKind::SelfModifier(self_modifier)
            }
        })
        .collect();

    let variadic = func_params.variadic.clone().and_then(|variadic| match &variadic {
        unsubstituted_variadic_param @ TypedFuncVariadicParams::Typed(ident, sema_type) => {
            if let Some(sema_type) = substitute_type(mapping_ctx_arena, sema_type.clone(), ctx.clone()) {
                Some(TypedFuncVariadicParams::Typed(ident.clone(), sema_type))
            } else {
                Some(unsubstituted_variadic_param.clone())
            }
        }
        c_style_variadic @ TypedFuncVariadicParams::UntypedCStyle => Some(c_style_variadic.clone()),
    });

    Some(TypedFuncParams { list, variadic })
}

pub fn substitute_func_sig(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    func_sig: &FuncSig,
    ctx: Rc<RefCell<GenericMappingCtx>>,
) -> Option<FuncSig> {
    let params = substitute_func_params(mapping_ctx_arena.clone(), &func_sig.params, ctx.clone())?;
    let ret_type = substitute_type(mapping_ctx_arena.clone(), func_sig.ret_type.clone(), ctx)?;

    Some(FuncSig {
        symbol_id: func_sig.symbol_id,
        name: func_sig.name.clone(),
        params,
        ret_type,
        is_func_decl: func_sig.is_func_decl,
        generic_params: func_sig.generic_params.clone(),
        modifiers: func_sig.modifiers.clone(),
        loc: func_sig.loc,
    })
}

pub fn substitute_struct_sig(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    struct_sig: &StructSig,
    ctx: Rc<RefCell<GenericMappingCtx>>,
) -> Option<StructSig> {
    let new_fields = struct_sig
        .fields
        .iter()
        .map(|f| {
            let substituted = substitute_type(mapping_ctx_arena.clone(), f.ty.clone(), ctx.clone())?;
            let mut f2 = f.clone();
            f2.ty = substituted;
            Some(f2)
        })
        .collect::<Option<Vec<_>>>()?;

    Some(StructSig {
        symbol_id: struct_sig.symbol_id,
        name: struct_sig.name.clone(),
        fields: new_fields,
        impls: struct_sig.impls.clone(),
        methods: struct_sig.methods.clone(),
        generic_params: struct_sig.generic_params.clone(),
        modifiers: struct_sig.modifiers.clone(),
        align: struct_sig.align.clone(),
        loc: struct_sig.loc,
    })
}

pub fn substitute_union_sig(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    union_sig: &UnionSig,
    ctx: Rc<RefCell<GenericMappingCtx>>,
) -> Option<UnionSig> {
    let fields = union_sig
        .fields
        .iter()
        .map(|f| {
            let substituted = substitute_type(mapping_ctx_arena.clone(), f.ty.clone(), ctx.clone())?;
            let mut f2 = f.clone();
            f2.ty = substituted;
            Some(f2)
        })
        .collect::<Option<Vec<_>>>()?;

    Some(UnionSig {
        symbol_id: union_sig.symbol_id,
        name: union_sig.name.clone(),
        fields,
        methods: union_sig.methods.clone(),
        generic_params: union_sig.generic_params.clone(),
        modifiers: union_sig.modifiers.clone(),
        align: union_sig.align.clone(),
        loc: union_sig.loc,
    })
}

pub fn substitute_enum_sig(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    enum_sig: &EnumSig,
    ctx: Rc<RefCell<GenericMappingCtx>>,
) -> Option<EnumSig> {
    let variants = enum_sig
        .variants
        .iter()
        .map(|variant| match variant {
            TypedEnumVariant::Ident(ident) => Some(TypedEnumVariant::Ident(ident.clone())),
            TypedEnumVariant::Valued(ident, expr) => {
                let substituted =
                    substitute_type(mapping_ctx_arena.clone(), expr.sema_type.clone().unwrap(), ctx.clone())?;

                let mut expr_clone = *expr.clone();
                expr_clone.sema_type = Some(substituted);

                Some(TypedEnumVariant::Valued(ident.clone(), Box::new(expr_clone)))
            }
            TypedEnumVariant::Variant(ident, fields) => {
                let new_fields = fields
                    .iter()
                    .map(|field| {
                        let substituted = substitute_type(mapping_ctx_arena.clone(), field.ty.clone(), ctx.clone())?;

                        Some(TypedEnumValuedField {
                            ty: substituted,
                            loc: field.loc,
                        })
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(TypedEnumVariant::Variant(ident.clone(), new_fields))
            }
        })
        .collect::<Option<Vec<_>>>()?;

    Some(EnumSig {
        symbol_id: enum_sig.symbol_id,
        name: enum_sig.name.clone(),
        methods: enum_sig.methods.clone(),
        variants,
        generic_params: enum_sig.generic_params.clone(),
        tag_type: enum_sig.tag_type.clone(),
        modifiers: enum_sig.modifiers.clone(),
        align: enum_sig.align.clone(),
        loc: enum_sig.loc,
    })
}
