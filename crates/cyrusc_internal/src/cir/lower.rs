// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    abi::target::ABITarget,
    cir::{
        cir::CIREnumVariant,
        typectx::{CIRTypeContext, CIRTypeDef},
        types::{CIRArrayType, CIREnumType, CIRFuncType, CIRStructType, CIRType, CIRUnionType},
    },
};
use cyrusc_ast::abi::CallConv;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::{EnumDecl, EnumDeclID, StructDecl, StructDeclID, UnionDecl, UnionDeclID, table::DeclTablesRegistry},
    stmts::{TypedEnumVariant, TypedFuncTypeParams, TypedTypeArgs},
    substitute::{
        instantiate_enum_decl_with_type_args, instantiate_struct_decl_with_type_args,
        instantiate_union_decl_with_type_args,
    },
    types::{NamedType, PlainType, SemaType, TypeDeclID, TypedArrayCapacity, TypedFuncType},
};
use fx_hash::FxHashSet;
use std::sync::Arc;

pub fn lower_sema_type(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    ty: &SemaType,
) -> CIRType {
    match ty {
        SemaType::Named(named_type) => lower_named_type(decl_tables, target, tctx, named_type),

        SemaType::Plain(plain_type) => CIRType::Plain(plain_type.clone()),

        SemaType::Tuple(tuple_type) => {
            let fields: Vec<CIRType> = tuple_type
                .elements
                .iter()
                .map(|(ty, _)| lower_sema_type(decl_tables, target, tctx.clone(), ty))
                .collect();

            let fields_info = tuple_type
                .elements
                .iter()
                .enumerate()
                .map(|(i, &(_, loc))| (i.to_string(), loc))
                .collect();

            let type_id = tctx.insert_struct(CIRStructType {
                decl_key: None,
                name: None,
                fields,
                fields_info,
                repr_attr: None,
                align: None,
                loc: tuple_type.loc,
            });

            CIRType::Struct(type_id)
        }
        SemaType::Array(array_type) => {
            let element_type = lower_sema_type(decl_tables, target, tctx.clone(), &array_type.element_type);

            let len = match &array_type.capacity {
                TypedArrayCapacity::Fixed(expr) => expr.literal_const_int_value().unwrap(),
                TypedArrayCapacity::Dynamic => todo!(),
            };

            CIRType::Array(CIRArrayType {
                element_type: Box::new(element_type),
                len: len.as_int(),
            })
        }
        SemaType::Const(sema_type) => {
            let inner = lower_sema_type(decl_tables, target, tctx.clone(), sema_type);
            CIRType::Const(Box::new(inner))
        }
        SemaType::Pointer(sema_type) => {
            let inner = lower_sema_type(decl_tables, target, tctx.clone(), sema_type);
            CIRType::Pointer(Box::new(inner))
        }
        SemaType::FuncType(func_type) => {
            CIRType::FuncType(lower_func_type(decl_tables, target, tctx.clone(), func_type))
        }

        SemaType::InterfaceObject(interface_object) => cir_fat_ptr_type(&tctx, None, interface_object.loc),

        SemaType::Unresolved(_)
        | SemaType::GenericParam(_)
        | SemaType::SelfType(_)
        | SemaType::InferVar(_)
        | SemaType::Placeholder
        | SemaType::Err(_) => unreachable!(),
    }
    .const_inner()
    .clone()
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
            lower_struct_type(
                decl_tables,
                target,
                tctx,
                struct_decl_id,
                &inst_struct_decl,
                named_type.type_args.clone(),
            )
        }
        TypeDeclID::Union(union_decl_id) => {
            let union_decl = decl_tables.union_decl(union_decl_id);
            let inst_union_decl = instantiate_union_decl_with_type_args(&union_decl, &named_type.type_args);
            lower_union_type(
                decl_tables,
                target,
                tctx,
                union_decl_id,
                &inst_union_decl,
                named_type.type_args.clone(),
            )
        }
        TypeDeclID::Enum(enum_decl_id) => {
            let enum_decl = decl_tables.enum_decl(enum_decl_id);
            let inst_enum_decl = instantiate_enum_decl_with_type_args(&enum_decl, &named_type.type_args);
            lower_enum_type(
                decl_tables,
                target,
                tctx,
                enum_decl_id,
                &inst_enum_decl,
                named_type.type_args.clone(),
            )
        }
        TypeDeclID::Interface(interface_decl_id) => {
            let interface_decl = decl_tables.interface_decl(interface_decl_id);
            cir_fat_ptr_type(&tctx, None, interface_decl.loc)
        }
        TypeDeclID::Typedef(_) => unreachable!("unexpected unexpanded typedef"),
    }
}

pub fn lower_struct_type(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    struct_decl_id: StructDeclID,
    struct_decl: &StructDecl,
    type_args: TypedTypeArgs,
) -> CIRType {
    if tctx.is_lowering(TypeDeclID::Struct(struct_decl_id)) {
        let placeholder = tctx.insert_type_placeholder();
        return CIRType::Struct(placeholder);
    }
    tctx.start_lowering(TypeDeclID::Struct(struct_decl_id));
    let struct_type = lower_struct_decl(
        decl_tables,
        target,
        tctx.clone(),
        struct_decl_id,
        struct_decl,
        type_args,
    );
    let type_id = tctx.insert_struct(struct_type.clone());
    tctx.finish_lowering(TypeDeclID::Struct(struct_decl_id));
    tctx.resolve_placeholder(type_id, CIRTypeDef::Struct(struct_type));
    CIRType::Struct(type_id)
}

pub fn lower_union_type(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    union_decl_id: UnionDeclID,
    union_decl: &UnionDecl,
    type_args: TypedTypeArgs,
) -> CIRType {
    if tctx.is_lowering(TypeDeclID::Union(union_decl_id)) {
        let placeholder = tctx.insert_type_placeholder();
        return CIRType::Union(placeholder);
    }
    tctx.start_lowering(TypeDeclID::Union(union_decl_id));
    let union_type = lower_union_decl(decl_tables, target, tctx.clone(), union_decl_id, union_decl, type_args);
    let type_id = tctx.insert_union(union_type);
    tctx.finish_lowering(TypeDeclID::Union(union_decl_id));
    CIRType::Union(type_id)
}

pub fn lower_enum_type(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    enum_decl_id: EnumDeclID,
    enum_decl: &EnumDecl,
    type_args: TypedTypeArgs,
) -> CIRType {
    if tctx.is_lowering(TypeDeclID::Enum(enum_decl_id)) {
        let placeholder = tctx.insert_type_placeholder();
        return CIRType::Enum(placeholder);
    }
    tctx.start_lowering(TypeDeclID::Enum(enum_decl_id));
    let enum_type = lower_enum_decl(decl_tables, target, tctx.clone(), enum_decl_id, enum_decl, type_args);
    let type_id = tctx.insert_enum(enum_type);
    tctx.finish_lowering(TypeDeclID::Enum(enum_decl_id));
    CIRType::Enum(type_id)
}

fn lower_struct_decl(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    struct_decl_id: StructDeclID,
    struct_decl: &StructDecl,
    type_args: TypedTypeArgs,
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
        decl_key: Some((TypeDeclID::Struct(struct_decl_id), type_args)),
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
fn lower_enum_decl(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    enum_decl_id: EnumDeclID,
    enum_decl: &EnumDecl,
    type_args: TypedTypeArgs,
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
        decl_key: Some((TypeDeclID::Enum(enum_decl_id), type_args)),
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

fn lower_union_decl(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    tctx: Arc<CIRTypeContext>,
    union_decl_id: UnionDeclID,
    union_decl: &UnionDecl,
    type_args: TypedTypeArgs,
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
        decl_key: Some((TypeDeclID::Union(union_decl_id), type_args)),
        name: union_decl.name.clone(),
        fields,
        fields_info,
        repr_attr: union_decl.modifiers.repr_attr.clone(),
        align: union_decl.align.clone(),
        loc: union_decl.loc,
    }
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
            let fields_types = fields
                .iter()
                .map(|field| lower_sema_type(decl_tables, target, tctx.clone(), &field.ty))
                .collect();

            let fields_info = fields
                .iter()
                .enumerate()
                .map(|(i, field)| (i.to_string(), field.loc))
                .collect();

            let struct_type = CIRStructType {
                decl_key: None,
                name: None,
                fields: fields_types,
                fields_info,
                repr_attr: None,
                align: None,
                loc: ident.loc,
            };

            CIREnumVariant::Payload(ident.as_string(), struct_type, variant_idx)
        }
        TypedEnumVariant::Struct { ident, fields } => {
            let fields_types = fields
                .iter()
                .map(|field| lower_sema_type(decl_tables, target, tctx.clone(), &field.ty))
                .collect();

            let fields_info = fields
                .iter()
                .enumerate()
                .map(|(i, field)| (i.to_string(), field.loc))
                .collect();

            let struct_type = CIRStructType {
                decl_key: None,
                name: None,
                fields: fields_types,
                fields_info,
                repr_attr: None,
                align: None,
                loc: ident.loc,
            };

            CIREnumVariant::Payload(ident.as_string(), struct_type, variant_idx)
        }
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

pub fn cir_fat_ptr_type(tctx: &CIRTypeContext, data_type: Option<CIRType>, loc: Loc) -> CIRType {
    let struct_type = CIRStructType {
        decl_key: None,
        name: None,
        fields: vec![
            CIRType::Pointer(Box::new(data_type.unwrap_or(CIRType::Plain(PlainType::Void)))), // T* or void*
            CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void))),
        ],
        fields_info: vec![("data_ptr".to_string(), loc), ("vtable_ptr".to_string(), loc)],
        repr_attr: None,
        align: None,
        loc,
    };

    let type_id = tctx.insert_struct(struct_type);
    CIRType::Struct(type_id)
}
