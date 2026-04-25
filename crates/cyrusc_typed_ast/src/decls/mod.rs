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
    BodyID,
    exprs::TypedExpr,
    stmts::{
        TypedBlockStmt, TypedEnumVariant, TypedFuncParams, TypedGenericParams, TypedImplementInterface,
        TypedStructField, TypedUnionField,
    },
    types::{SemaType, TypeDeclID, TypedFuncType},
};
use cyrusc_ast::{
    abi::{ReprKind, Visibility},
    modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers},
};
use cyrusc_source_loc::Loc;
use indexmap::IndexMap;
use std::hash::Hash;

pub mod table;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum DeclID {
    Struct(StructDeclID),
    Enum(EnumDeclID),
    Union(UnionDeclID),
    Func(FuncDeclID),
    Method(MethodDeclID),
    Interface(InterfaceDeclID),
    GlobalVar(GlobalVarDeclID),
    Var(VarDeclID),
    Typedef(TypedefDeclID),
}

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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypedefDeclID(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MonomorphID(pub usize);

#[derive(Debug, Clone)]
pub struct MethodDecls(pub IndexMap<String, MethodDeclID>);

#[derive(Debug, Clone)]
pub struct StructDecl {
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
    pub name: Option<String>,
    pub methods: MethodDecls,
    pub variants: Vec<TypedEnumVariant>,
    pub impls: Vec<TypedImplementInterface>,
    pub generic_params: TypedGenericParams,
    pub modifiers: EnumModifiers,
    pub tag_type: Option<SemaType>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub params: TypedFuncParams,
    pub generic_params: TypedGenericParams,
    pub ret_type: SemaType,

    /// Body of the function stored in the global BodyTable.
    ///
    /// - None for pure function declarations (no definition).
    /// - For generic functions, this refers to the *template body* that will
    ///   be cloned and instantiated during monomorphization.
    /// - For non-generic functions, this is the concrete body analyzed normally.
    pub body: Option<BodyID>,

    pub is_func_decl: bool,
    pub modifiers: FuncModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct MethodDecl {
    pub func_decl: FuncDecl,

    /// Concrete body of the method, used for both normal methods and
    /// monomorphized generic instances.
    pub body: Option<BodyID>,
}

#[derive(Debug, Clone)]
pub struct TypedefDecl {
    pub name: String,
    pub ty: Box<SemaType>,
    pub generic_params: TypedGenericParams,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct InterfaceDecl {
    pub name: String,
    pub methods: MethodDecls,
    pub generic_params: TypedGenericParams,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct GlobalVarDecl {
    pub name: String,
    pub ty: Option<SemaType>,
    pub rhs: Option<TypedExpr>,
    pub is_const: bool,
    pub analyzed: bool,
    pub modifiers: GlobalVarModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub ty: Option<SemaType>,
    pub rhs: Option<TypedExpr>,
    pub is_const: bool,
    pub analyzed: bool,
    pub loc: Loc,
}

impl EnumDecl {
    #[inline]
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }

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

impl MethodDecl {
    #[inline]
    pub fn is_instance_method(&self) -> bool {
        self.func_decl.params.is_instance_method()
    }
}

impl MethodDecls {
    #[inline]
    pub fn new() -> Self {
        Self(IndexMap::new())
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn insert(&mut self, name: String, method_decl_id: MethodDeclID) -> bool {
        let name = name.into();
        self.0.insert(name, method_decl_id).is_none()
    }

    #[inline]
    pub fn get(&self, name: &str) -> Option<MethodDeclID> {
        self.0.get(name).cloned()
    }

    #[inline]
    pub fn contains(&self, name: &str) -> bool {
        self.0.contains_key(name)
    }

    #[inline]
    pub fn contains_method_id(&self, method_decl_id: MethodDeclID) -> bool {
        self.0.values().any(|&id| id == method_decl_id)
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&String, &MethodDeclID)> {
        self.0.iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&String, &mut MethodDeclID)> {
        self.0.iter_mut()
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
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }

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
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }

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
            params,
            ret_type: Box::new(self.ret_type.clone()),
            is_public,
            loc: self.loc,
        }
    }
}

impl DeclID {
    #[inline]
    pub fn as_type_decl_id(&self) -> Option<TypeDeclID> {
        match self {
            DeclID::Struct(id) => Some(TypeDeclID::Struct(*id)),
            DeclID::Enum(id) => Some(TypeDeclID::Enum(*id)),
            DeclID::Union(id) => Some(TypeDeclID::Union(*id)),
            DeclID::Interface(id) => Some(TypeDeclID::Interface(*id)),
            DeclID::Typedef(id) => Some(TypeDeclID::Typedef(*id)),

            DeclID::Func(_) | DeclID::Method(_) | DeclID::GlobalVar(_) | DeclID::Var(_) => None,
        }
    }
}

impl DeclID {
    #[inline(always)]
    pub fn is_var(&self) -> bool {
        matches!(self, DeclID::Var(_))
    }

    #[inline(always)]
    pub fn is_global_var(&self) -> bool {
        matches!(self, DeclID::GlobalVar(_))
    }

    #[inline(always)]
    pub fn is_var_or_global_var(&self) -> bool {
        matches!(self, DeclID::Var(_) | DeclID::GlobalVar(_))
    }

    #[inline(always)]
    pub fn is_func(&self) -> bool {
        matches!(self, DeclID::Func(_))
    }
}

impl DeclID {
    #[inline(always)]
    pub fn as_struct(&self) -> Option<StructDeclID> {
        match *self {
            DeclID::Struct(id) => Some(id),
            _ => None,
        }
    }

    #[inline(always)]
    pub fn as_enum(&self) -> Option<EnumDeclID> {
        match *self {
            DeclID::Enum(id) => Some(id),
            _ => None,
        }
    }

    #[inline(always)]
    pub fn as_union(&self) -> Option<UnionDeclID> {
        match *self {
            DeclID::Union(id) => Some(id),
            _ => None,
        }
    }

    #[inline(always)]
    pub fn as_func(&self) -> Option<FuncDeclID> {
        match *self {
            DeclID::Func(id) => Some(id),
            _ => None,
        }
    }

    #[inline(always)]
    pub fn as_method(&self) -> Option<MethodDeclID> {
        match *self {
            DeclID::Method(id) => Some(id),
            _ => None,
        }
    }

    #[inline(always)]
    pub fn as_interface(&self) -> Option<InterfaceDeclID> {
        match *self {
            DeclID::Interface(id) => Some(id),
            _ => None,
        }
    }

    #[inline(always)]
    pub fn as_global_var(&self) -> Option<GlobalVarDeclID> {
        match *self {
            DeclID::GlobalVar(id) => Some(id),
            _ => None,
        }
    }

    #[inline(always)]
    pub fn as_var(&self) -> Option<VarDeclID> {
        match *self {
            DeclID::Var(id) => Some(id),
            _ => None,
        }
    }

    #[inline(always)]
    pub fn as_typedef(&self) -> Option<TypedefDeclID> {
        match *self {
            DeclID::Typedef(id) => Some(id),
            _ => None,
        }
    }
}

impl InterfaceDecl {
    #[inline]
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }

    #[inline]
    pub fn method_index(&self, name: &str) -> Option<usize> {
        self.methods.0.iter().position(|(method_name, _)| method_name == name)
    }

    #[inline]
    pub fn get_method(&self, name: &str) -> Option<MethodDeclID> {
        self.methods.0.get(name).copied()
    }
}
