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
        TypedBlockStmt, TypedEnumVariant, TypedFuncDeclStmt, TypedFuncParams, TypedGenericParamsList,
        TypedImplementInterface, TypedStructField, TypedUnionField,
    },
    types::SemanticType,
};
use cyrusc_ast::{
    abi::Visibility,
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
    pub generic_params: Option<TypedGenericParamsList>,
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
    pub generic_params: Option<TypedGenericParamsList>,
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
    pub generic_params: Option<TypedGenericParamsList>,
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
    pub generic_params: Option<TypedGenericParamsList>,
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
    pub generic_params: Option<TypedGenericParamsList>,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct InterfaceDecl {
    pub symbol_id: SymbolID,
    pub name: String,
    pub methods: Vec<TypedFuncDeclStmt>,
    pub generic_params: Option<TypedGenericParamsList>,
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
    pub is_const: bool,
    pub analyzed: bool,
    pub loc: Loc,
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

// // FIXME: Make this method for FuncDecl
// pub fn set_self_modifier_type_in_func_sig(func_decl: &mut FuncDecl, sema_type: &SemanticType) {
//     let first_param = func_decl.params.list.first_mut();

//     if let Some(func_param_kind) = first_param {
//         if let Some(self_modifier) = func_param_kind.as_self_modifier_mut() {
//             match self_modifier.kind {
//                 SelfModifierKind::Copied => {
//                     self_modifier.ty = Some(sema_type.clone());
//                 }
//                 SelfModifierKind::Referenced => {
//                     self_modifier.ty = Some(sema_type.clone());
//                 }
//             }
//         }
//     }
// }

// // FIXME: Make this method for FuncDecl
// pub fn set_self_modifier_symbol_id_in_func_sig(func_decl: &mut FuncDecl, symbol_id: SymbolID) {
//     let first_param = func_decl.params.list.first_mut();

//     if let Some(func_param_kind) = first_param {
//         if let Some(self_modifier) = func_param_kind.as_self_modifier_mut() {
//             self_modifier.self_id = Some(symbol_id);
//         }
//     }
// }

// pub fn typed_func_decl_as_func_sig(func_decl: &TypedFuncDeclStmt) -> FuncDecl {
//     FuncDecl {
//         symbol_id: Some(func_decl.symbol_id),
//         name: func_decl.name.clone(),
//         generic_params: func_decl.generic_params.clone(),
//         params: func_decl.params.clone(),
//         ret_type: func_decl.ret_type.clone(),
//         is_func_decl: true,
//         modifiers: func_decl.modifiers.clone(),
//         loc: func_decl.loc,
//     }
// }

// pub fn typed_func_decl_from_func_sig(sig: &FuncDecl) -> TypedFuncDeclStmt {
//     TypedFuncDeclStmt {
//         symbol_id: sig.symbol_id.unwrap(),
//         name: sig.name.clone(),
//         generic_params: sig.generic_params.clone(),
//         params: sig.params.clone(),
//         ret_type: sig.ret_type.clone(),
//         modifiers: sig.modifiers.clone(),
//         loc: sig.loc,
//         renamed_as: None,
//     }
// }

// pub fn typed_func_def_as_func_sig(func_def: &TypedFuncDefStmt) -> FuncDecl {
//     FuncDecl {
//         symbol_id: Some(func_def.symbol_id),
//         name: func_def.name.clone(),
//         generic_params: func_def.generic_params.clone(),
//         params: func_def.params.clone(),
//         ret_type: func_def.ret_type.clone(),
//         is_func_decl: false,
//         modifiers: func_def.modifiers.clone(),
//         loc: func_def.loc,
//     }
// }

// pub fn typed_func_type_from_func_sig(func_decl: &FuncDecl) -> TypedFuncType {
//     TypedFuncType {
//         symbol_id: func_decl.symbol_id,
//         params: typed_func_params_as_func_type_params(&func_decl.params),
//         ret_type: Box::new(func_decl.ret_type.clone()),
//         is_public: func_decl.modifiers.vis.is_public(),
//         loc: func_decl.loc,
//     }
// }

// pub fn typed_struct_as_struct_sig(struct_stmt: &TypedStructStmt) -> StructDecl {
//     StructDecl {
//         symbol_id: struct_stmt.symbol_id,
//         name: struct_stmt.name.clone(),
//         fields: struct_stmt.fields.clone(),
//         impls: struct_stmt.impls.clone(),
//         methods: struct_stmt.methods.clone(),
//         generic_params: struct_stmt.generic_params.clone(),
//         modifiers: struct_stmt.modifiers.clone(),
//         align: struct_stmt.align.clone(),
//         loc: struct_stmt.loc,
//     }
// }

// pub fn typed_enum_as_enum_sig(typed_enum: &TypedEnumStmt) -> EnumDecl {
//     EnumDecl {
//         symbol_id: typed_enum.symbol_id,
//         name: typed_enum.name.clone(),
//         methods: typed_enum.methods.clone(),
//         variants: typed_enum.variants.clone(),
//         generic_params: typed_enum.generic_params.clone(),
//         modifiers: typed_enum.modifiers.clone(),
//         tag_type: typed_enum.tag_type.clone(),
//         align: typed_enum.align.clone(),
//         loc: typed_enum.loc,
//     }
// }

// pub fn typed_union_as_union_sig(union_stmt: &TypedUnionStmt) -> UnionDecl {
//     UnionDecl {
//         symbol_id: union_stmt.symbol_id,
//         name: union_stmt.name.clone(),
//         fields: union_stmt.fields.clone(),
//         methods: union_stmt.methods.clone(),
//         generic_params: union_stmt.generic_params.clone(),
//         modifiers: union_stmt.modifiers.clone(),
//         align: union_stmt.align.clone(),
//         loc: union_stmt.loc,
//     }
// }

// pub fn typed_func_params_as_func_type_params(params: &TypedFuncParams) -> TypedFuncTypeParams {
//     let list = params
//         .list
//         .iter()
//         .map(|param| match param {
//             TypedFuncParamKind::FuncParam(typed_func_param) => typed_func_param.ty.clone(),
//             TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
//                 let ty = SemanticType::UnresolvedSymbol(typed_self_modifier.symbol_id.unwrap());
//                 match typed_self_modifier.kind {
//                     SelfModifierKind::Copied => ty,
//                     SelfModifierKind::Referenced => SemanticType::Pointer(Box::new(ty)),
//                 }
//             }
//         })
//         .collect();

//     let variadic = match &params.variadic {
//         Some(variadic) => match variadic {
//             TypedFuncVariadicParams::UntypedCStyle => Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle)),
//             TypedFuncVariadicParams::Typed(_, sema_type) => {
//                 Some(Box::new(TypedFuncTypeVariadicParams::Typed(sema_type.clone())))
//             }
//         },
//         None => None,
//     };

//     TypedFuncTypeParams { list, variadic }
// }

impl StructDecl {
    pub fn is_packed(&self) -> bool {
        match &self.modifiers.repr_attr {
            Some(repr_attr) => repr_attr.is_packed(),
            None => false,
        }
    }
}

impl FuncDecl {
    // pub fn is_instance_method(&self) -> bool {
    //     match self.params.list.first() {
    //         Some(typed_func_param_kind) => match typed_func_param_kind {
    //             TypedFuncParamKind::FuncParam(..) => false,
    //             TypedFuncParamKind::SelfModifier(..) => true,
    //         },
    //         None => false,
    //     }
    // }

    pub fn is_generic(&self) -> bool {
        self.generic_params.is_some()
    }
}
