// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    abi::target::ABITarget,
    cir::{
        cir::CIREnumVariant,
        typectx::CIRTypeContext,
        types::{
            CIRArrayType, CIREnumType, CIRFuncType, CIRStructType, CIRTupleType, CIRType, CIRUnionType,
            cir_fat_ptr_type,
        },
    },
};
use cyrusc_ast::abi::CallConv;
use cyrusc_typed_ast::{
    decls::{EnumDecl, StructDecl, UnionDecl, table::DeclTablesRegistry},
    stmts::{TypedEnumVariant, TypedFuncTypeParams},
    substitute::{
        instantiate_enum_decl_with_type_args, instantiate_struct_decl_with_type_args,
        instantiate_union_decl_with_type_args,
    },
    types::{NamedType, SemaType, TypeDeclID, TypedArrayCapacity, TypedFuncType},
};
use fx_hash::FxHashSet;
use std::sync::Arc;

pub fn lower_sema_type(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    ty: &SemaType,
) -> CIRType {
    let cir_type = match ty {
        SemaType::Plain(plain_type) => {
            let cir = CIRType::Plain(plain_type.clone());
            tctx.register(cir.clone());
            cir
        }
        SemaType::Named(named_type) => lower_named_type(decl_tables, target, tctx, named_type),
        SemaType::Tuple(tuple_type) => {
            let elements: Vec<CIRType> = tuple_type
                .elements
                .iter()
                .map(|sema_type| lower_sema_type(decl_tables, target, tctx.clone(), sema_type))
                .collect();

            let cir = CIRType::Tuple(CIRTupleType {
                elements,
                loc: tuple_type.loc,
            });
            tctx.register(cir.clone());
            cir
        }
        SemaType::Array(array_type) => {
            let element_type = lower_sema_type(decl_tables, target, tctx.clone(), &array_type.element_type);

            let len = match &array_type.capacity {
                TypedArrayCapacity::Fixed(expr) => expr.literal_const_int_value().unwrap(),
                TypedArrayCapacity::Dynamic => todo!(),
            };

            let cir = CIRType::Array(CIRArrayType {
                element_type: Box::new(element_type),
                len: len.as_int(),
            });
            tctx.register(cir.clone());
            cir
        }
        SemaType::Const(sema_type) => {
            let inner = lower_sema_type(decl_tables, target, tctx.clone(), sema_type);
            let cir = CIRType::Const(Box::new(inner));
            tctx.register(cir.clone());
            cir
        }
        SemaType::Pointer(sema_type) => {
            let inner = lower_sema_type(decl_tables, target, tctx.clone(), sema_type);
            let cir = CIRType::Pointer(Box::new(inner));
            tctx.register(cir.clone());
            cir
        }
        SemaType::FuncType(func_type) => {
            let cir = CIRType::FuncType(lower_func_type(decl_tables, target, tctx.clone(), func_type));
            tctx.register(cir.clone());
            cir
        }
        SemaType::InterfaceObject(interface_object) => {
            let cir = cir_fat_ptr_type(interface_object.loc);
            tctx.register(cir.clone());
            cir
        }

        SemaType::Unresolved(_)
        | SemaType::GenericParam(_)
        | SemaType::SelfType(_)
        | SemaType::InferVar(_)
        | SemaType::Placeholder
        | SemaType::Err(_) => unreachable!(),
    };

    cir_type.const_inner().clone()
}

pub fn lower_named_type(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    named_type: &NamedType,
) -> CIRType {
    match named_type.type_decl_id {
        TypeDeclID::Struct(struct_decl_id) => {
            let struct_decl = decl_tables.struct_decl(struct_decl_id);
            let inst_struct_decl = instantiate_struct_decl_with_type_args(&struct_decl, &named_type.type_args);

            let placeholder = tctx.insert_type_placeholder();
            let struct_ty = lower_struct_decl(decl_tables, target, tctx.clone(), &inst_struct_decl);
            let cir = CIRType::Struct(struct_ty);
            tctx.resolve_placeholder(placeholder, cir.clone());
            tctx.register(cir.clone());
            cir
        }
        TypeDeclID::Union(union_decl_id) => {
            let union_decl = decl_tables.union_decl(union_decl_id);
            let inst_union_decl = instantiate_union_decl_with_type_args(&union_decl, &named_type.type_args);

            let placeholder = tctx.insert_type_placeholder();
            let union_ty = lower_union_decl(decl_tables, target, tctx.clone(), &inst_union_decl);
            let cir = CIRType::Union(union_ty);
            tctx.resolve_placeholder(placeholder, cir.clone());
            tctx.register(cir.clone());
            cir
        }
        TypeDeclID::Enum(enum_decl_id) => {
            let enum_decl = decl_tables.enum_decl(enum_decl_id);
            let inst_enum_decl = instantiate_enum_decl_with_type_args(&enum_decl, &named_type.type_args);

            let placeholder = tctx.insert_type_placeholder();
            let enum_ty = lower_enum_decl(decl_tables, target, tctx.clone(), &inst_enum_decl);
            let cir = CIRType::Enum(enum_ty);
            tctx.resolve_placeholder(placeholder, cir.clone());
            tctx.register(cir.clone());
            cir
        }
        TypeDeclID::Interface(interface_decl_id) => {
            let interface_decl = decl_tables.interface_decl(interface_decl_id);
            let cir = cir_fat_ptr_type(interface_decl.loc);
            tctx.register(cir.clone());
            cir
        }
        TypeDeclID::Typedef(_) => unreachable!("unexpected unexpanded typedef"),
    }
}

pub fn lower_struct_decl(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    struct_decl: &StructDecl,
) -> CIRStructType {
    let fields = struct_decl
        .fields
        .iter()
        .map(|field| lower_sema_type(decl_tables, target, tctx.clone(), &field.ty))
        .collect();

    let fields_info = struct_decl
        .fields
        .iter()
        .map(|field| (field.name.clone(), field.loc))
        .collect();

    CIRStructType {
        name: struct_decl.name.clone(),
        fields,
        fields_info,
        repr_attr: struct_decl.modifiers.repr_attr.clone(),
        align: struct_decl.align.clone(),
        loc: struct_decl.loc,
    }
}

/// Lowers a semantic `EnumDecl` into a `CIREnumType`.
///
/// This converts each semantic variant into its CIR representation and
/// computes the enum tag layout. Explicit discriminants from `Valued`
/// variants are preserved when the enum is scalar‑optimizable. Remaining
/// variants receive automatically assigned tags, ensuring all tags are
/// unique.
pub fn lower_enum_decl(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    enum_decl: &EnumDecl,
) -> CIREnumType {
    let mut variants: Vec<CIREnumVariant> = enum_decl
        .variants
        .iter()
        .map(|variant| lower_enum_variant(decl_tables, target, tctx.clone(), enum_decl, variant))
        .collect();

    let tag_type = enum_decl
        .tag_type
        .clone()
        .map(|sema_type| Box::new(lower_sema_type(decl_tables, target, tctx.clone(), &sema_type)));

    let mut cir_enum_type = CIREnumType {
        name: enum_decl.name.clone(),
        variants: variants.clone(),
        tag_type,
        repr_attr: enum_decl.modifiers.repr_attr.clone(),
        align: enum_decl.align.clone(),
        loc: enum_decl.loc,
    };

    let mut used_tags = FxHashSet::<u32>::default();
    let mut next_tag: u32 = 0;

    if cir_enum_type.is_scalar_optimizable() {
        for variant in &mut variants {
            if let CIREnumVariant::Valued(ident, _, tag) = variant {
                let value = match enum_decl.lookup_variant(ident).unwrap() {
                    TypedEnumVariant::Valued { value, .. } => value,
                    _ => unreachable!(),
                };

                if let Some(int_tag) = value.literal_const_int_value() {
                    let tag_value: u32 = int_tag.as_int();
                    *tag = tag_value;
                    used_tags.insert(tag_value);
                }
            }
        }
    }

    for variant in &mut variants {
        match variant {
            CIREnumVariant::Unit(_, tag) | CIREnumVariant::Payload(_, _, tag) | CIREnumVariant::Valued(_, _, tag) => {
                if used_tags.contains(tag) {
                    continue;
                }

                while used_tags.contains(&next_tag) {
                    next_tag += 1;
                }

                *tag = next_tag;
                used_tags.insert(next_tag);
                next_tag += 1;
            }
        }
    }

    cir_enum_type.variants = variants;

    cir_enum_type
}

pub fn lower_union_decl(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    union_decl: &UnionDecl,
) -> CIRUnionType {
    let fields = union_decl
        .fields
        .iter()
        .map(|field| lower_sema_type(decl_tables, target, tctx.clone(), &field.ty))
        .collect();

    let fields_info = union_decl
        .fields
        .iter()
        .map(|field| (field.name.clone(), field.loc))
        .collect();

    CIRUnionType {
        name: union_decl.name.clone(),
        fields,
        fields_info,
        repr_attr: union_decl.modifiers.repr_attr.clone(),
        align: union_decl.align.clone(),
        loc: union_decl.loc,
    }
}

pub fn lower_func_type(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    func_type: &TypedFuncType,
) -> CIRFuncType {
    let ret_type = Box::new(lower_sema_type(decl_tables, target, tctx.clone(), &func_type.ret_type));
    let params = lower_func_type_params(decl_tables, target, tctx.clone(), &func_type.params);

    let mut cir_type = CIRFuncType {
        params,
        ret_type,
        is_var: func_type.params.variadic.is_some(),
        callconv: CallConv::default(),
        abi_func_info: None,
    };

    cir_type.abi_func_info = Some(target.target_abi.classify_func(&cir_type).unwrap());
    cir_type
}

fn lower_func_type_params(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    func_type_params: &TypedFuncTypeParams,
) -> Vec<CIRType> {
    func_type_params
        .list
        .iter()
        .map(|sema_type| lower_sema_type(decl_tables, target, tctx.clone(), sema_type))
        .collect()
}

/// Lowers a single `TypedEnumVariant` into its CIR representation.
///
/// This performs structural lowering of the variant (payload types, etc.)
/// and assigns a provisional tag equal to the variant index.
///
/// For `Valued` variants the returned tag is **temporary**. The final
/// discriminant value is resolved later in [`lower_enum_decl`] based on
/// explicit discriminants and enum layout rules.
fn lower_enum_variant(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    enum_decl: &EnumDecl,
    variant: &TypedEnumVariant,
) -> CIREnumVariant {
    let variant_idx: u32 = enum_decl
        .variants
        .iter()
        .position(|_variant| _variant.ident() == variant.ident())
        .unwrap()
        .try_into()
        .unwrap();

    match variant {
        TypedEnumVariant::Unit(ident) => CIREnumVariant::Unit(ident.as_string(), variant_idx),
        TypedEnumVariant::Valued { ident, value } => {
            let cir_value_type = lower_sema_type(decl_tables, target, tctx.clone(), value.ty.as_ref().unwrap());

            // IMPORTANT
            // default=variant_idx
            // changed if enum-type is_scalar_optimizable.
            CIREnumVariant::Valued(ident.as_string(), cir_value_type, variant_idx)
        }
        TypedEnumVariant::Tuple { ident, fields } => {
            let fields = fields
                .iter()
                .map(|field| lower_sema_type(decl_tables, target, tctx.clone(), &field.ty))
                .collect();

            CIREnumVariant::Payload(ident.as_string(), fields, variant_idx)
        }
        TypedEnumVariant::Struct { ident, fields } => {
            let fields = fields
                .iter()
                .map(|struct_variant_field| {
                    lower_sema_type(decl_tables, target, tctx.clone(), &struct_variant_field.ty)
                })
                .collect();

            CIREnumVariant::Payload(ident.as_string(), fields, variant_idx)
        }
    }
}
