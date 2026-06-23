// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    decls::{EnumDecl, FuncDecl, MethodDecl, StructDecl, UnionDecl},
    stmts::{
        TypedEnumVariant, TypedEnumVariantStructField, TypedEnumVariantTupleField, TypedFuncParam, TypedFuncParamKind,
        TypedFuncParams, TypedFuncTypeParams, TypedFuncTypeVariadicParam, TypedFuncVariadicParam, TypedGenericParams,
        TypedSelfModifier, TypedStructField, TypedTypeArg, TypedTypeArgs, TypedUnionField,
    },
    types::{InterfaceObjectType, NamedType, SemaType, TypedArrayType, TypedFuncType, TypedTupleType},
};

pub fn substitute_sema_type_with_type_args(
    ty: &SemaType,
    generic_params: &TypedGenericParams,
    type_args: &TypedTypeArgs,
) -> SemaType {
    match ty {
        SemaType::GenericParam(generic_param_id) => {
            let pos = generic_params
                .iter()
                .position(|_generic_param_id| _generic_param_id == generic_param_id);

            match pos {
                Some(idx) => match &type_args.0[idx] {
                    TypedTypeArg::Type(inner, _) => {
                        substitute_sema_type_with_type_args(inner, generic_params, type_args)
                    }
                    TypedTypeArg::Infer => ty.clone(),
                },
                None => ty.clone(),
            }
        }

        SemaType::Named(named_type) => {
            let mut new_args = Vec::new();

            for arg in &named_type.type_args.0 {
                match arg {
                    TypedTypeArg::Type(inner, loc) => {
                        new_args.push(TypedTypeArg::Type(
                            substitute_sema_type_with_type_args(inner, generic_params, type_args),
                            *loc,
                        ));
                    }
                    TypedTypeArg::Infer => new_args.push(TypedTypeArg::Infer),
                }
            }

            SemaType::Named(NamedType {
                type_decl_id: named_type.type_decl_id,
                type_args: TypedTypeArgs(new_args),
            })
        }
        SemaType::InterfaceObject(interface_object) => {
            let concrete_type =
                substitute_sema_type_with_type_args(&interface_object.concrete_type, generic_params, type_args);

            SemaType::InterfaceObject(InterfaceObjectType {
                interface_type: interface_object.interface_type.clone(),
                concrete_type: Box::new(concrete_type),
                vtable_id: interface_object.vtable_id,
                loc: interface_object.loc,
            })
        }
        SemaType::Array(array) => {
            let element_type = substitute_sema_type_with_type_args(&array.element_type, generic_params, type_args);

            SemaType::Array(TypedArrayType {
                element_type: Box::new(element_type),
                capacity: array.capacity.clone(),
                loc: array.loc,
            })
        }
        SemaType::Const(inner) => SemaType::Const(Box::new(substitute_sema_type_with_type_args(
            inner,
            generic_params,
            type_args,
        ))),
        SemaType::Pointer(inner) => SemaType::Pointer(Box::new(substitute_sema_type_with_type_args(
            inner,
            generic_params,
            type_args,
        ))),
        SemaType::FuncType(func_type) => {
            let list = func_type
                .params
                .list
                .iter()
                .map(|ty| substitute_sema_type_with_type_args(ty, generic_params, type_args))
                .collect();

            let variadic = func_type.params.variadic.clone().map(|variadic| match *variadic {
                v @ TypedFuncTypeVariadicParam::UntypedCStyle => Box::new(v),
                TypedFuncTypeVariadicParam::Typed(ty) => Box::new(TypedFuncTypeVariadicParam::Typed(
                    substitute_sema_type_with_type_args(&ty, generic_params, type_args),
                )),
            });

            let ret_type = substitute_sema_type_with_type_args(&func_type.ret_type, generic_params, type_args);

            SemaType::FuncType(TypedFuncType {
                params: TypedFuncTypeParams { list, variadic },
                ret_type: Box::new(ret_type),
                is_public: func_type.is_public,
                loc: func_type.loc,
            })
        }

        SemaType::Tuple(tuple) => {
            let elements = tuple
                .elements
                .iter()
                .map(|(ty, loc)| (substitute_sema_type_with_type_args(ty, generic_params, type_args), *loc))
                .collect();

            SemaType::Tuple(TypedTupleType {
                elements,
                loc: tuple.loc,
            })
        }

        SemaType::SelfType(_)
        | SemaType::Plain(_)
        | SemaType::Unresolved(_)
        | SemaType::InferVar(_)
        | SemaType::Placeholder
        | SemaType::Err(_) => ty.clone(),
    }
}

pub fn instantiate_struct_decl_with_type_args(struct_decl: &StructDecl, type_args: &TypedTypeArgs) -> StructDecl {
    let mut fields = Vec::new();

    for field in &struct_decl.fields {
        let ty = substitute_sema_type_with_type_args(&field.ty, &struct_decl.generic_params, type_args);

        fields.push(TypedStructField {
            name: field.name.clone(),
            ty,
            vis: field.vis,
            loc: field.loc,
        });
    }

    StructDecl {
        name: struct_decl.name.clone(),
        fields,
        impls: struct_decl.impls.clone(),
        methods: struct_decl.methods.clone(),
        generic_params: struct_decl.generic_params.clone(),
        modifiers: struct_decl.modifiers.clone(),
        align: struct_decl.align,
        loc: struct_decl.loc,

        is_normalized: true,
    }
}

pub fn instantiate_union_decl_with_type_args(union_decl: &UnionDecl, type_args: &TypedTypeArgs) -> UnionDecl {
    let mut fields = Vec::new();

    for field in &union_decl.fields {
        let ty = substitute_sema_type_with_type_args(&field.ty, &union_decl.generic_params, type_args);

        fields.push(TypedUnionField {
            name: field.name.clone(),
            ty,
            loc: field.loc,
        });
    }

    UnionDecl {
        name: union_decl.name.clone(),
        fields,
        impls: union_decl.impls.clone(),
        methods: union_decl.methods.clone(),
        generic_params: union_decl.generic_params.clone(),
        modifiers: union_decl.modifiers.clone(),
        align: union_decl.align,
        loc: union_decl.loc,

        is_normalized: true,
    }
}

pub fn instantiate_enum_decl_with_type_args(enum_decl: &EnumDecl, type_args: &TypedTypeArgs) -> EnumDecl {
    let mut variants = Vec::new();

    for variant in &enum_decl.variants {
        match variant {
            v @ TypedEnumVariant::Unit(_) | v @ TypedEnumVariant::Valued { .. } => {
                variants.push(v.clone());
            }

            TypedEnumVariant::Tuple { ident, fields } => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        let ty = substitute_sema_type_with_type_args(&field.ty, &enum_decl.generic_params, type_args);

                        TypedEnumVariantTupleField { ty, loc: field.loc }
                    })
                    .collect();

                variants.push(TypedEnumVariant::Tuple {
                    ident: ident.clone(),
                    fields,
                });
            }
            TypedEnumVariant::Struct { ident, fields } => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        let ty = substitute_sema_type_with_type_args(&field.ty, &enum_decl.generic_params, type_args);

                        TypedEnumVariantStructField {
                            name: field.name.clone(),
                            ty,
                            loc: field.loc,
                        }
                    })
                    .collect();

                variants.push(TypedEnumVariant::Struct {
                    ident: ident.clone(),
                    fields,
                });
            }
        }
    }

    EnumDecl {
        name: enum_decl.name.clone(),
        methods: enum_decl.methods.clone(),
        variants,
        impls: enum_decl.impls.clone(),
        generic_params: enum_decl.generic_params.clone(),
        modifiers: enum_decl.modifiers.clone(),
        tag_type: enum_decl.tag_type.clone(),
        align: enum_decl.align,
        loc: enum_decl.loc,

        is_normalized: true,
    }
}

#[inline]
pub fn instantiate_method_decl(
    method_decl: &MethodDecl,
    interface_generic_params: &TypedGenericParams,
    type_args: &TypedTypeArgs,
) -> MethodDecl {
    MethodDecl {
        func_decl: instantiate_func_decl(&method_decl.func_decl, interface_generic_params, type_args),
        body: method_decl.body,
        object_name: method_decl.object_name.clone(),
    }
}

pub fn instantiate_func_decl(
    func_decl: &FuncDecl,
    generic_params: &TypedGenericParams,
    type_args: &TypedTypeArgs,
) -> FuncDecl {
    let params = substitute_func_params_with_type_args(&func_decl.params, generic_params, type_args);

    let ret_type = substitute_sema_type_with_type_args(&func_decl.ret_type, generic_params, type_args);

    FuncDecl {
        file_id: func_decl.file_id,
        name: func_decl.name.clone(),
        params,
        generic_params: func_decl.generic_params.clone(),
        ret_type,
        body: func_decl.body,
        is_func_decl: func_decl.is_func_decl,
        modifiers: func_decl.modifiers.clone(),
        loc: func_decl.loc,
    }
}

pub fn substitute_func_params_with_type_args(
    params: &TypedFuncParams,
    generic_params: &TypedGenericParams,
    type_args: &TypedTypeArgs,
) -> TypedFuncParams {
    let mut new_list = Vec::with_capacity(params.list.len());

    for param_kind in &params.list {
        let new_p = match param_kind {
            TypedFuncParamKind::FuncParam(func_param) => {
                let ty = substitute_sema_type_with_type_args(&func_param.ty, generic_params, type_args);

                TypedFuncParamKind::FuncParam(TypedFuncParam {
                    var_decl_id: func_param.var_decl_id,
                    ident: func_param.ident.clone(),
                    ty,
                    mutability: func_param.mutability,
                    loc: func_param.loc,
                })
            }
            TypedFuncParamKind::SelfModifier(self_modifier) => {
                let ty = substitute_sema_type_with_type_args(&self_modifier.ty, generic_params, type_args);

                TypedFuncParamKind::SelfModifier(TypedSelfModifier {
                    var_decl_id: self_modifier.var_decl_id,
                    ty,
                    kind: self_modifier.kind.clone(),
                    mutability: self_modifier.mutability,
                    loc: self_modifier.loc,
                })
            }
        };

        new_list.push(new_p);
    }
    let new_variadic = match &params.variadic {
        None => None,
        Some(TypedFuncVariadicParam::UntypedCStyle) => Some(TypedFuncVariadicParam::UntypedCStyle),
        Some(TypedFuncVariadicParam::Typed { var_decl_id, ty, loc }) => {
            let ty = substitute_sema_type_with_type_args(ty, generic_params, type_args);

            Some(TypedFuncVariadicParam::Typed {
                var_decl_id: *var_decl_id,
                ty,
                loc: *loc,
            })
        }
    };

    TypedFuncParams {
        list: new_list,
        variadic: new_variadic,
    }
}
