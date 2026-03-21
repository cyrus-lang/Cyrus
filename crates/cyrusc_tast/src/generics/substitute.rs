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

pub fn substitute_type(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    sema_ty: SemanticType,
    mapping_ctx: Rc<RefCell<GenericMappingCtx>>,
) -> Option<SemanticType> {
    fn sub<F>(inner: SemanticType, f: &F) -> Option<SemanticType>
    where
        F: Fn(SemanticType) -> Option<SemanticType>,
    {
        f(inner)
    }

    match sema_ty {
        SemanticType::GenericParam(generic_param) => {
            let mut sema_ty = {
                let mapping_ctx_ref = mapping_ctx.borrow();
                mapping_ctx_ref.resolve_with_name(mapping_ctx_arena, &generic_param.param_name.name)
            };

            if let Some(SemanticType::GenericParam(generic_param)) = sema_ty {
                sema_ty = generic_param.default.map(|sema_ty| *sema_ty);
            }

            sema_ty
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
        SemanticType::Const(inner) => sub(*inner, &|sema_ty| {
            substitute_type(mapping_ctx_arena.clone(), sema_ty, mapping_ctx.clone())
        })
        .map(|sema_ty| sema_ty.as_const()),
        SemanticType::Tuple(tuple_type) => {
            let list = tuple_type
                .type_list
                .into_iter()
                .map(|t| substitute_type(mapping_ctx_arena.clone(), t, mapping_ctx.clone()))
                .collect::<Option<Vec<_>>>()?;
            Some(SemanticType::Tuple(TypedTupleType {
                type_list: list,
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
                *func_type.return_type,
                mapping_ctx.clone(),
            )?);
            Some(SemanticType::FuncType(TypedFuncType {
                symbol_id: func_type.symbol_id,
                def_module_id: func_type.def_module_id,
                params: TypedFuncTypeParams {
                    list: params,
                    variadic: func_type.params.variadic,
                },
                is_public: func_type.is_public,
                return_type: ret_ty,
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
                if let Some(sema_ty) = substitute_type(mapping_ctx_arena.clone(), func_param.ty.clone(), ctx.clone()) {
                    func_param.ty = sema_ty;
                }
                TypedFuncParamKind::FuncParam(func_param)
            }
            TypedFuncParamKind::SelfModifier(mut self_modifier) => {
                self_modifier.ty = self_modifier
                    .ty
                    .and_then(|sema_ty| substitute_type(mapping_ctx_arena.clone(), sema_ty, ctx.clone()));
                TypedFuncParamKind::SelfModifier(self_modifier)
            }
        })
        .collect();

    let variadic = func_params.variadic.clone().and_then(|variadic| match &variadic {
        unsubstituted_variadic_param @ TypedFuncVariadicParams::Typed(ident, sema_ty) => {
            if let Some(sema_ty) = substitute_type(mapping_ctx_arena, sema_ty.clone(), ctx.clone()) {
                Some(TypedFuncVariadicParams::Typed(ident.clone(), sema_ty))
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
    sig: &FuncSig,
    ctx: Rc<RefCell<GenericMappingCtx>>,
) -> Option<FuncSig> {
    let params = substitute_func_params(mapping_ctx_arena.clone(), &sig.params, ctx.clone())?;
    let return_type = substitute_type(mapping_ctx_arena.clone(), sig.return_type.clone(), ctx)?;

    Some(FuncSig {
        name: sig.name.clone(),
        module_id: sig.module_id,
        symbol_id: sig.symbol_id,
        params,
        return_type,
        is_func_decl: sig.is_func_decl,
        generic_params: sig.generic_params.clone(),
        modifiers: sig.modifiers.clone(),
        loc: sig.loc,
    })
}

pub fn substitute_struct_sig(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    sig: &StructSig,
    ctx: Rc<RefCell<GenericMappingCtx>>,
) -> Option<StructSig> {
    let new_fields = sig
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
        name: sig.name.clone(),
        fields: new_fields,
        impls: sig.impls.clone(),
        methods: sig.methods.clone(),
        generic_params: sig.generic_params.clone(),
        modifiers: sig.modifiers.clone(),
        align: sig.align.clone(),
        loc: sig.loc,
    })
}

pub fn substitute_union_sig(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    sig: &UnionSig,
    ctx: Rc<RefCell<GenericMappingCtx>>,
) -> Option<UnionSig> {
    let new_fields = sig
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
        symbol_id: sig.symbol_id,
        name: sig.name.clone(),
        fields: new_fields,
        methods: sig.methods.clone(),
        generic_params: sig.generic_params.clone(),
        modifiers: sig.modifiers.clone(),
        align: sig.align.clone(),
        loc: sig.loc,
    })
}

pub fn substitute_enum_sig(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    sig: &EnumSig,
    ctx: Rc<RefCell<GenericMappingCtx>>,
) -> Option<EnumSig> {
    let new_variants = sig
        .variants
        .iter()
        .map(|v| match v {
            TypedEnumVariant::Ident(ident) => Some(TypedEnumVariant::Ident(ident.clone())),
            TypedEnumVariant::Valued(ident, expr) => {
                let substituted =
                    substitute_type(mapping_ctx_arena.clone(), expr.sema_ty.clone().unwrap(), ctx.clone())?;
                let mut expr_v2 = *expr.clone();
                expr_v2.sema_ty = Some(substituted);
                Some(TypedEnumVariant::Valued(ident.clone(), Box::new(expr_v2)))
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
        symbol_id: sig.symbol_id,
        name: sig.name.clone(),
        methods: sig.methods.clone(),
        variants: new_variants,
        generic_params: sig.generic_params.clone(),
        tag_type: sig.tag_type.clone(),
        modifiers: sig.modifiers.clone(),
        align: sig.align.clone(),
        loc: sig.loc,
    })
}
