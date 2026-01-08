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
    ModuleID, SymbolID,
    exprs::{TypedExprStmt, TypedIdentifier},
    stmts::{
        TypedEnumStmt, TypedEnumVariant, TypedFuncDeclStmt, TypedFuncDefStmt, TypedFuncParamKind, TypedFuncParams,
        TypedFuncTypeParams, TypedFuncTypeVariadicParams, TypedFuncVariadicParams, TypedGenericParamsList,
        TypedStructField, TypedStructStmt, TypedUnionField, TypedUnionStmt,
    },
    types::{SemanticType, TypedFuncType},
};
use cyrusc_abi::{
    modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers},
    visibility::Visibility,
};
use cyrusc_ast::{SelfModifierKind, source_loc::SourceLoc};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct StructSig {
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub impls: Vec<TypedIdentifier>,
    pub methods: HashMap<String, SymbolID>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub is_packed: bool,
    pub modifiers: StructModifiers,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct UnionSig {
    pub symbol_id: SymbolID,
    pub name: String,
    pub fields: Vec<TypedUnionField>,
    pub methods: HashMap<String, SymbolID>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub modifiers: UnionModifiers,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, Eq)]
pub struct FuncSig {
    pub module_id: ModuleID,
    pub symbol_id: Option<SymbolID>,
    pub name: String,
    pub params: TypedFuncParams,
    pub generic_params: Option<TypedGenericParamsList>,
    pub return_type: SemanticType,
    pub is_func_decl: bool,
    pub modifiers: FuncModifiers,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct EnumSig {
    pub symbol_id: SymbolID,
    pub name: String,
    pub methods: HashMap<String, SymbolID>,
    pub variants: Vec<TypedEnumVariant>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub modifiers: EnumModifiers,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedefSig {
    pub name: String,
    pub ty: SemanticType,
    pub generic_params: Option<TypedGenericParamsList>,
    pub vis: Visibility,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct InterfaceSig {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub methods: Vec<TypedFuncDeclStmt>,
    pub vis: Visibility,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct GlobalVarSig {
    pub module_id: ModuleID,
    pub name: String,
    pub ty: Option<SemanticType>,
    pub rhs: Option<TypedExprStmt>,
    pub analyzed: bool,
    pub modifiers: GlobalVarModifiers,
    pub loc: SourceLoc,
}

impl PartialEq for FuncSig {
    fn eq(&self, other: &Self) -> bool {
        let self_params = self.params.list.iter().collect::<Vec<_>>();
        let other_params = other.params.list.iter().collect::<Vec<_>>();

        self.name == other.name && self_params == other_params && self.return_type == other.return_type
    }
}

impl FuncSig {
    pub fn is_instance_method(&self) -> bool {
        match self.params.list.first() {
            Some(typed_func_param_kind) => match typed_func_param_kind {
                TypedFuncParamKind::FuncParam(..) => false,
                TypedFuncParamKind::SelfModifier(..) => true,
            },
            None => false,
        }
    }
}

pub fn set_self_modifier_type_in_func_sig(func_sig: &mut FuncSig, sema_ty: &SemanticType) {
    let first_param = func_sig.params.list.first_mut();

    if let Some(func_param_kind) = first_param {
        if let Some(self_modifier) = func_param_kind.as_self_modifier_mut() {
            match self_modifier.kind {
                SelfModifierKind::Copied => {
                    self_modifier.ty = Some(sema_ty.clone());
                }
                SelfModifierKind::Referenced => {
                    self_modifier.ty = Some(SemanticType::Pointer(Box::new(sema_ty.clone())));
                }
            }
        }
    }
}

pub fn typed_func_decl_as_func_sig(func_decl: &TypedFuncDeclStmt) -> FuncSig {
    FuncSig {
        symbol_id: Some(func_decl.symbol_id),
        module_id: func_decl.module_id,
        name: func_decl.name.clone(),
        generic_params: func_decl.generic_params.clone(),
        params: func_decl.params.clone(),
        return_type: func_decl.return_type.clone(),
        is_func_decl: true,
        modifiers: func_decl.modifiers.clone(),
        loc: func_decl.loc.clone(),
    }
}

pub fn typed_func_decl_from_func_sig(sig: &FuncSig) -> TypedFuncDeclStmt {
    TypedFuncDeclStmt {
        symbol_id: sig.symbol_id.unwrap(),
        module_id: sig.module_id,
        name: sig.name.clone(),
        generic_params: sig.generic_params.clone(),
        params: sig.params.clone(),
        return_type: sig.return_type.clone(),
        modifiers: sig.modifiers.clone(),
        loc: sig.loc.clone(),
        renamed_as: None,
    }
}

pub fn typed_func_def_as_func_sig(func_def: &TypedFuncDefStmt) -> FuncSig {
    FuncSig {
        symbol_id: Some(func_def.symbol_id),
        module_id: func_def.module_id,
        name: func_def.name.clone(),
        generic_params: func_def.generic_params.clone(),
        params: func_def.params.clone(),
        return_type: func_def.return_type.clone(),
        is_func_decl: false,
        modifiers: func_def.modifiers.clone(),
        loc: func_def.loc.clone(),
    }
}

pub fn typed_func_type_from_func_sig(func_sig: &FuncSig) -> TypedFuncType {
    TypedFuncType {
        symbol_id: func_sig.symbol_id,
        def_module_id: Some(func_sig.module_id),
        params: typed_func_params_as_func_type_params(&func_sig.params),
        return_type: Box::new(func_sig.return_type.clone()),
        is_public: func_sig.modifiers.vis.is_public(),
        loc: func_sig.loc.clone(),
    }
}

pub fn typed_struct_as_struct_sig(typed_struct: &TypedStructStmt) -> StructSig {
    StructSig {
        name: typed_struct.name.clone(),
        fields: typed_struct.fields.clone(),
        impls: typed_struct.impls.clone(),
        methods: typed_struct.methods.clone(),
        generic_params: typed_struct.generic_params.clone(),
        is_packed: typed_struct.is_packed,
        modifiers: typed_struct.modifiers.clone(),
        loc: typed_struct.loc.clone(),
    }
}

pub fn typed_enum_as_enum_sig(typed_enum: &TypedEnumStmt) -> EnumSig {
    EnumSig {
        symbol_id: typed_enum.symbol_id,
        name: typed_enum.name.clone(),
        methods: typed_enum.methods.clone(),
        variants: typed_enum.variants.clone(),
        generic_params: typed_enum.generic_params.clone(),
        modifiers: typed_enum.modifiers.clone(),
        loc: typed_enum.loc.clone(),
    }
}

pub fn typed_union_as_union_sig(typed_union: &TypedUnionStmt) -> UnionSig {
    UnionSig {
        symbol_id: typed_union.symbol_id,
        name: typed_union.name.clone(),
        fields: typed_union.fields.clone(),
        methods: typed_union.methods.clone(),
        generic_params: typed_union.generic_params.clone(),
        modifiers: typed_union.modifiers.clone(),
        loc: typed_union.loc.clone(),
    }
}

pub fn typed_func_params_as_func_type_params(params: &TypedFuncParams) -> TypedFuncTypeParams {
    TypedFuncTypeParams {
        list: params
            .list
            .iter()
            .map(|param| match param {
                TypedFuncParamKind::FuncParam(typed_func_param) => typed_func_param.ty.clone(),
                TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
                    let ty = SemanticType::UnresolvedSymbol(typed_self_modifier.symbol_id.unwrap());
                    match typed_self_modifier.kind {
                        SelfModifierKind::Copied => ty,
                        SelfModifierKind::Referenced => SemanticType::Pointer(Box::new(ty)),
                    }
                }
            })
            .collect(),
        variadic: match &params.variadic {
            Some(variadic) => match variadic {
                TypedFuncVariadicParams::UntypedCStyle => Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle)),
                TypedFuncVariadicParams::Typed(_, sema_ty) => {
                    Some(Box::new(TypedFuncTypeVariadicParams::Typed(sema_ty.clone())))
                }
            },
            None => None,
        },
    }
}
