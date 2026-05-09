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
    abi::target::ABITarget,
    cir::{
        cir::CIREnumVariant,
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

pub fn lower_sema_type(decl_tables: &DeclTablesRegistry, target: &ABITarget, ty: &SemaType) -> CIRType {
    match ty {
        SemaType::Plain(plain_type) => CIRType::Plain(plain_type.clone()),
        SemaType::Named(named_type) => lower_named_type(decl_tables, target, named_type),
        SemaType::InterfaceObject(interface_object) => cir_fat_ptr_type(interface_object.loc),
        SemaType::Array(array_type) => {
            let element_type = lower_sema_type(decl_tables, target, &array_type.element_type);
            let len = match &array_type.capacity {
                TypedArrayCapacity::Fixed(expr) => expr.literal_const_int_value().unwrap(),
                TypedArrayCapacity::Dynamic => todo!(),
            };

            CIRType::Array(CIRArrayType {
                element_type: Box::new(element_type),
                len: len.try_into().unwrap(),
            })
        }
        SemaType::Const(sema_type) => CIRType::Const(Box::new(lower_sema_type(decl_tables, target, &*sema_type))),
        SemaType::Pointer(sema_type) => CIRType::Pointer(Box::new(lower_sema_type(decl_tables, target, &*sema_type))),
        SemaType::FuncType(func_type) => CIRType::FuncType(lower_func_type(decl_tables, target, func_type)),
        SemaType::Tuple(tuple_type) => {
            let elements: Vec<CIRType> = tuple_type
                .elements
                .iter()
                .map(|sema_type| lower_sema_type(decl_tables, target, sema_type))
                .collect();

            CIRType::Tuple(CIRTupleType {
                elements,
                loc: tuple_type.loc,
            })
        }

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

pub fn lower_named_type(decl_tables: &DeclTablesRegistry, target: &ABITarget, named_type: &NamedType) -> CIRType {
    match named_type.type_decl_id {
        TypeDeclID::Struct(struct_decl_id) => {
            let struct_decl = decl_tables.struct_decl(struct_decl_id);

            let inst_struct_decl = instantiate_struct_decl_with_type_args(&struct_decl, &named_type.type_args);

            CIRType::Struct(lower_struct_decl(decl_tables, target, &inst_struct_decl))
        }
        TypeDeclID::Union(union_decl_id) => {
            let union_decl = decl_tables.union_decl(union_decl_id);

            let inst_union_decl = instantiate_union_decl_with_type_args(&union_decl, &named_type.type_args);

            CIRType::Union(lower_union_decl(decl_tables, target, &inst_union_decl))
        }
        TypeDeclID::Enum(enum_decl_id) => {
            let enum_decl = decl_tables.enum_decl(enum_decl_id);

            let inst_enum_decl = instantiate_enum_decl_with_type_args(&enum_decl, &named_type.type_args);

            CIRType::Enum(lower_enum_decl(decl_tables, target, &inst_enum_decl))
        }
        TypeDeclID::Interface(interface_decl_id) => {
            let interface_decl = decl_tables.interface_decl(interface_decl_id);

            cir_fat_ptr_type(interface_decl.loc)
        }
        TypeDeclID::Typedef(_) => unreachable!("unexpected unexpanded typedef"),
    }
}

pub fn lower_struct_decl(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    struct_decl: &StructDecl,
) -> CIRStructType {
    let fields = struct_decl
        .fields
        .iter()
        .map(|field| lower_sema_type(decl_tables, target, &field.ty))
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

pub fn lower_enum_decl(decl_tables: &DeclTablesRegistry, target: &ABITarget, enum_decl: &EnumDecl) -> CIREnumType {
    let variants: Vec<CIREnumVariant> = enum_decl
        .variants
        .iter()
        .map(|variant| lower_enum_type_variant(decl_tables, target, variant))
        .collect();

    let tag_type = enum_decl
        .tag_type
        .clone()
        .map(|sema_type| Box::new(lower_sema_type(decl_tables, target, &sema_type)));

    CIREnumType {
        name: enum_decl.name.clone(),
        variants,
        tag_type,
        repr_attr: enum_decl.modifiers.repr_attr.clone(),
        align: enum_decl.align.clone(),
        loc: enum_decl.loc,
    }
}

pub fn lower_union_decl(decl_tables: &DeclTablesRegistry, target: &ABITarget, union_decl: &UnionDecl) -> CIRUnionType {
    let fields = union_decl
        .fields
        .iter()
        .map(|field| lower_sema_type(decl_tables, target, &field.ty))
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

pub fn lower_func_type(decl_tables: &DeclTablesRegistry, target: &ABITarget, func_type: &TypedFuncType) -> CIRFuncType {
    let ret = Box::new(lower_sema_type(decl_tables, target, &func_type.ret_type));
    let params = lower_func_type_params(decl_tables, target, &func_type.params);

    let mut cir_type = CIRFuncType {
        params: params,
        is_var: func_type.params.variadic.is_some(),
        ret_type: ret,
        callconv: CallConv::default(),
        abi_func_info: None,
    };

    cir_type.abi_func_info = Some(target.target_abi.classify_func(&cir_type).unwrap());
    cir_type
}

fn lower_func_type_params(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    func_type_params: &TypedFuncTypeParams,
) -> Vec<CIRType> {
    func_type_params
        .list
        .iter()
        .map(|sema_type| lower_sema_type(decl_tables, target, sema_type))
        .collect()
}

fn lower_enum_type_variant(
    decl_tables: &DeclTablesRegistry,
    target: &ABITarget,
    variant: &TypedEnumVariant,
) -> CIREnumVariant {
    match variant {
        TypedEnumVariant::Unit(ident) => CIREnumVariant::Unit(ident.as_string()),
        TypedEnumVariant::Valued { ident, value } => {
            let cir_value_type = lower_sema_type(decl_tables, target, value.ty.as_ref().unwrap());

            CIREnumVariant::Valued(ident.as_string(), cir_value_type)
        }
        TypedEnumVariant::Tuple { ident, fields } => {
            let fields = fields
                .iter()
                .map(|field| lower_sema_type(decl_tables, target, &field.ty))
                .collect();

            CIREnumVariant::Payload(ident.as_string(), fields)
        }
        TypedEnumVariant::Struct { ident, fields } => {
            let fields = fields
                .iter()
                .map(|struct_variant_field| lower_sema_type(decl_tables, target, &struct_variant_field.ty))
                .collect();

            CIREnumVariant::Payload(ident.as_string(), fields)
        }
    }
}
