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
    SymbolID,
    exprs::TypedExprStmt,
    stmts::{
        TypedBlockStmt, TypedEnumVariant, TypedFuncParams, TypedGenericParams, TypedImplementInterface,
        TypedStructField, TypedUnionField,
    },
    types::{SemanticType, TypedFuncType},
};
use cyrusc_ast::{
    abi::{ReprKind, Visibility},
    modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers},
};
use cyrusc_source_loc::Loc;
use std::{collections::HashMap, hash::Hash};

pub mod table;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct StructDeclID(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct EnumDeclID(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct UnionDeclID(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FuncDeclID(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct MethodDeclID(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct InterfaceDeclID(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct GlobalVarDeclID(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct VarDeclID(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypedefDeclID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MonomorphID(pub usize);

#[derive(Debug, Clone)]
pub struct MethodDecls(pub HashMap<String, MethodDeclID>);

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub symbol_id: Option<SymbolID>,
    pub name: Option<String>,
    pub fields: Vec<TypedStructField>,
    pub impls: Vec<TypedImplementInterface>,
    pub methods: MethodDecls,
    pub generic_params: TypedGenericParams,
    pub modifiers: StructModifiers,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnionDecl {
    pub symbol_id: Option<SymbolID>,
    pub name: Option<String>,
    pub fields: Vec<TypedUnionField>,
    pub impls: Vec<TypedImplementInterface>,
    pub methods: MethodDecls,
    pub generic_params: TypedGenericParams,
    pub modifiers: UnionModifiers,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub symbol_id: Option<SymbolID>,
    pub name: Option<String>,
    pub methods: MethodDecls,
    pub variants: Vec<TypedEnumVariant>,
    pub impls: Vec<TypedImplementInterface>,
    pub generic_params: TypedGenericParams,
    pub modifiers: EnumModifiers,
    pub tag_type: Option<SemanticType>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub symbol_id: Option<SymbolID>,
    pub name: String,
    pub params: TypedFuncParams,
    pub generic_params: TypedGenericParams,
    pub ret_type: SemanticType,
    pub is_func_decl: bool,
    pub modifiers: FuncModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct MethodDecl {
    pub func_decl: FuncDecl,
    pub body: Option<Box<TypedBlockStmt>>,
}

#[derive(Debug, Clone)]
pub struct TypedefDecl {
    pub name: String,
    pub ty: Box<SemanticType>,
    pub generic_params: TypedGenericParams,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct InterfaceDecl {
    pub symbol_id: SymbolID,
    pub name: String,
    pub methods: Vec<FuncDeclID>,
    pub generic_params: TypedGenericParams,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct GlobalVarDecl {
    pub symbol_id: SymbolID,
    pub name: String,
    pub ty: Option<SemanticType>,
    pub rhs: Option<TypedExprStmt>,
    pub is_const: bool,
    pub analyzed: bool,
    pub modifiers: GlobalVarModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub ty: Option<SemanticType>,
    pub rhs: Option<TypedExprStmt>,
    pub is_const: bool,
    pub analyzed: bool,
    pub loc: Loc,
}

impl EnumDecl {
    #[inline]
    pub fn lookup_variant(&self, name: &str) -> Option<&TypedEnumVariant> {
        self.variants.iter().find(|variant| variant.ident().value == name)
    }

    #[inline]
    pub fn is_repr_c(&self) -> bool {
        if let Some(repr_attr) = &self.modifiers.repr_attr {
            if let Some(kind) = repr_attr.kind() {
                return match kind {
                    ReprKind::C => true,
                    ReprKind::Cyrus => false,
                    ReprKind::Transparent => false,
                };
            }
        }
        false
    }
}

impl MethodDecls {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn new_with_capacity(capacity: usize) -> Self {
        Self(HashMap::with_capacity(capacity))
    }

    pub fn insert(&mut self, name: String, method_decl_id: MethodDeclID) -> bool {
        let name = name.into();
        self.0.insert(name, method_decl_id).is_none()
    }

    pub fn get(&self, name: &str) -> Option<MethodDeclID> {
        self.0.get(name).cloned()
    }

    pub fn contains(&self, name: &str) -> bool {
        self.0.contains_key(name)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &MethodDeclID)> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&String, &mut MethodDeclID)> {
        self.0.iter_mut()
    }
}

impl Hash for FuncDecl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbol_id.hash(state);
    }
}

impl PartialEq for FuncDecl {
    fn eq(&self, other: &Self) -> bool {
        let self_params = self.params.list.iter().collect::<Vec<_>>();
        let other_params = other.params.list.iter().collect::<Vec<_>>();

        self.name == other.name && self_params == other_params && self.ret_type == other.ret_type
    }
}

impl UnionDecl {
    #[inline]
    pub fn lookup_field(&self, name: &str) -> Option<&TypedUnionField> {
        self.fields.iter().find(|field| field.name == name)
    }

    #[inline]
    pub fn lookup_field_mut(&mut self, name: &str) -> Option<&mut TypedUnionField> {
        self.fields.iter_mut().find(|field| field.name == name)
    }
}

impl StructDecl {
    #[inline]
    pub fn lookup_field(&self, name: &str) -> Option<&TypedStructField> {
        self.fields.iter().find(|field| field.name == name)
    }

    #[inline]
    pub fn lookup_field_mut(&mut self, name: &str) -> Option<&mut TypedStructField> {
        self.fields.iter_mut().find(|field| field.name == name)
    }

    #[inline]
    pub fn is_packed(&self) -> bool {
        match &self.modifiers.repr_attr {
            Some(repr_attr) => repr_attr.is_packed(),
            None => false,
        }
    }
}

impl FuncDecl {
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }

    pub fn as_func_type(&self) -> TypedFuncType {
        let params = self.params.as_func_type_params();
        let is_public = self.modifiers.vis.is_public();

        TypedFuncType {
            symbol_id: self.symbol_id,
            params,
            ret_type: Box::new(self.ret_type.clone()),
            is_public,
            loc: self.loc,
        }
    }
}
