// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::abi::layout::{ABITypeLayout, type_layout};
use crate::abi::target::ABITargetInfo;
use crate::cir::cir::CIREnumVariant;
use crate::cir::types::*;
use cyrusc_source_loc::{FileID, Loc};
use cyrusc_typed_ast::stmts::TypedTypeArgs;
use cyrusc_typed_ast::types::PlainType;
use cyrusc_typed_ast::types::TypeDeclID;
use fx_hash::{FxHashMap, FxHashSet};
use std::sync::RwLock;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CIRTypeContextID(usize);

pub type CIRTypeContextDeclKey = (TypeDeclID, TypedTypeArgs);

pub struct CIRTypeContext {
    pub target_info: ABITargetInfo,

    defs: RwLock<Vec<CIRTypeDef>>,
    key_to_id: RwLock<FxHashMap<CIRTypeKey, CIRTypeContextID>>,
    layouts: RwLock<FxHashMap<CIRTypeContextID, ABITypeLayout>>,
    in_progress: RwLock<FxHashSet<TypeDeclID>>,
}

#[derive(Debug, Clone)]
pub enum CIRTypeDef {
    Struct(CIRStructType),
    Union(CIRUnionType),
    Enum(CIREnumType),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum CIRTypeKey {
    Plain(PlainType),
    Pointer(Box<CIRTypeKey>),
    Array(Box<CIRTypeKey>, usize),
    Tuple(Vec<CIRTypeKey>),
    Struct(Option<CIRTypeContextDeclKey>, Vec<CIRTypeKey>),
    Union(Option<CIRTypeContextDeclKey>, Vec<CIRTypeKey>),
    Enum(Option<CIRTypeContextDeclKey>, Vec<EnumVariantKey>, Box<CIRTypeKey>),
    FuncType(Vec<CIRTypeKey>, Box<CIRTypeKey>, bool),
    Dynamic,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum EnumVariantKey {
    Unit,
    Valued(Box<CIRTypeKey>),
    Payload(Vec<CIRTypeKey>),
}

impl CIRTypeContext {
    #[inline]
    pub fn new(target_info: ABITargetInfo) -> Self {
        Self {
            defs: RwLock::new(Vec::new()),
            key_to_id: RwLock::new(FxHashMap::default()),
            layouts: RwLock::new(FxHashMap::default()),
            target_info,
            in_progress: RwLock::new(FxHashSet::default()),
        }
    }

    pub fn insert_type_placeholder(&self) -> CIRTypeContextID {
        let mut defs = self.defs.write().unwrap();
        let id = CIRTypeContextID(defs.len());
        defs.push(CIRTypeDef::Struct(CIRStructType {
            decl_key: None,
            name: None,
            fields: vec![],
            fields_info: vec![],
            repr_attr: None,
            align: None,
            loc: Loc::default(FileID(0)),
        }));
        id
    }

    #[inline]
    pub fn resolve_placeholder(&self, id: CIRTypeContextID, def: CIRTypeDef) {
        let mut defs = self.defs.write().unwrap();
        defs[id.0] = def;
    }

    pub fn insert_struct(&self, struct_type: CIRStructType) -> CIRTypeContextID {
        let key = self.struct_key(&struct_type);
        {
            let key_to_id = self.key_to_id.read().unwrap();
            if let Some(&id) = key_to_id.get(&key) {
                return id;
            }
        }
        let mut defs = self.defs.write().unwrap();
        let mut key_to_id = self.key_to_id.write().unwrap();
        if let Some(&id) = key_to_id.get(&key) {
            return id;
        }
        let id = CIRTypeContextID(defs.len());
        defs.push(CIRTypeDef::Struct(struct_type));
        key_to_id.insert(key, id);
        id
    }

    pub fn insert_union(&self, union_type: CIRUnionType) -> CIRTypeContextID {
        let key = self.union_key(&union_type);
        {
            let key_to_id = self.key_to_id.read().unwrap();
            if let Some(&id) = key_to_id.get(&key) {
                return id;
            }
        }
        let mut defs = self.defs.write().unwrap();
        let mut key_to_id = self.key_to_id.write().unwrap();
        if let Some(&id) = key_to_id.get(&key) {
            return id;
        }
        let id = CIRTypeContextID(defs.len());
        defs.push(CIRTypeDef::Union(union_type));
        key_to_id.insert(key, id);
        id
    }

    pub fn insert_enum(&self, enum_type: CIREnumType) -> CIRTypeContextID {
        let key = self.enum_key(&enum_type);
        {
            let key_to_id = self.key_to_id.read().unwrap();
            if let Some(&id) = key_to_id.get(&key) {
                return id;
            }
        }
        let mut defs = self.defs.write().unwrap();
        let mut key_to_id = self.key_to_id.write().unwrap();
        if let Some(&id) = key_to_id.get(&key) {
            return id;
        }
        let id = CIRTypeContextID(defs.len());
        defs.push(CIRTypeDef::Enum(enum_type));
        key_to_id.insert(key, id);
        id
    }

    #[inline]
    pub fn get_struct(&self, id: CIRTypeContextID) -> CIRStructType {
        let defs = self.defs.read().unwrap();

        match &defs[id.0] {
            CIRTypeDef::Struct(struct_type) => struct_type.clone(),
            _ => panic!("not a struct"),
        }
    }

    #[inline]
    pub fn get_union(&self, id: CIRTypeContextID) -> CIRUnionType {
        let defs = self.defs.read().unwrap();

        match &defs[id.0] {
            CIRTypeDef::Union(union_type) => union_type.clone(),
            _ => panic!("not a union"),
        }
    }

    #[inline]
    pub fn get_enum(&self, id: CIRTypeContextID) -> CIREnumType {
        let defs = self.defs.read().unwrap();

        match &defs[id.0] {
            CIRTypeDef::Enum(enum_type) => enum_type.clone(),
            _ => panic!("not an enum"),
        }
    }

    pub fn get_or_compute_layout(&self, id: CIRTypeContextID) -> ABITypeLayout {
        {
            let cache = self.layouts.read().unwrap();
            if let Some(layout) = cache.get(&id) {
                return layout.clone();
            }
        }
        let layout = self.compute_layout_for_def(id);
        self.layouts.write().unwrap().insert(id, layout.clone());
        layout
    }

    fn compute_layout_for_def(&self, id: CIRTypeContextID) -> ABITypeLayout {
        let defs = self.defs.read().unwrap();
        let def = &defs[id.0];
        let cir_type = match def {
            CIRTypeDef::Struct(_) => CIRType::Struct(id),
            CIRTypeDef::Union(_) => CIRType::Union(id),
            CIRTypeDef::Enum(_) => CIRType::Enum(id),
        };
        type_layout(&self.target_info, &cir_type)
    }

    pub fn layout_of(&self, ty: &CIRType) -> ABITypeLayout {
        match ty {
            CIRType::Struct(type_id) | CIRType::Union(type_id) | CIRType::Enum(type_id) => {
                self.get_or_compute_layout(*type_id)
            }
            CIRType::Plain(plain_type) => type_layout(&self.target_info, &CIRType::Plain(plain_type.clone())),
            CIRType::Const(inner) => self.layout_of(inner),
            CIRType::Pointer(_) => type_layout(&self.target_info, ty),
            CIRType::Array(_) => type_layout(&self.target_info, ty),
            CIRType::FuncType(_) => type_layout(&self.target_info, ty),
            CIRType::Dynamic(_) => type_layout(&self.target_info, ty),
        }
    }

    #[inline]
    pub fn start_lowering(&self, decl_id: TypeDeclID) {
        self.in_progress.write().unwrap().insert(decl_id);
    }

    #[inline]
    pub fn finish_lowering(&self, decl_id: TypeDeclID) {
        self.in_progress.write().unwrap().remove(&decl_id);
    }

    #[inline]
    pub fn is_lowering(&self, decl_id: TypeDeclID) -> bool {
        self.in_progress.read().unwrap().contains(&decl_id)
    }

    #[inline]
    fn struct_key(&self, struct_type: &CIRStructType) -> CIRTypeKey {
        let fields: Vec<CIRTypeKey> = struct_type.fields.iter().map(|f| self.type_to_key(f)).collect();

        CIRTypeKey::Struct(struct_type.decl_key.clone(), fields)
    }

    #[inline]
    fn union_key(&self, union_type: &CIRUnionType) -> CIRTypeKey {
        let fields: Vec<CIRTypeKey> = union_type.fields.iter().map(|f| self.type_to_key(f)).collect();

        CIRTypeKey::Union(union_type.decl_key.clone(), fields)
    }

    fn enum_key(&self, enum_type: &CIREnumType) -> CIRTypeKey {
        let variants: Vec<EnumVariantKey> = enum_type
            .variants
            .iter()
            .map(|v| match v {
                CIREnumVariant::Unit(..) => EnumVariantKey::Unit,
                CIREnumVariant::Valued(_, ty, _) => EnumVariantKey::Valued(Box::new(self.type_to_key(ty))),
                CIREnumVariant::Payload(_, fields, _) => {
                    let keys: Vec<CIRTypeKey> = fields.iter().map(|f| self.type_to_key(f)).collect();
                    EnumVariantKey::Payload(keys)
                }
            })
            .collect();

        let tag_key = self.type_to_key(&enum_type.tag_type_or_infer_or_default());

        CIRTypeKey::Enum(enum_type.decl_key.clone(), variants, Box::new(tag_key))
    }

    fn type_to_key(&self, ty: &CIRType) -> CIRTypeKey {
        match ty {
            CIRType::Plain(p) => CIRTypeKey::Plain(p.clone()),
            CIRType::Const(inner) => CIRTypeKey::Pointer(Box::new(self.type_to_key(inner))), // Const not stored, flatten
            CIRType::Pointer(inner) => CIRTypeKey::Pointer(Box::new(self.type_to_key(inner))),
            CIRType::Array(arr) => CIRTypeKey::Array(Box::new(self.type_to_key(&arr.element_type)), arr.len),
            CIRType::Struct(id) => {
                let defs = self.defs.read().unwrap();
                match &defs[id.0] {
                    CIRTypeDef::Struct(s) => self.struct_key(s),
                    _ => unreachable!(),
                }
            }
            CIRType::Union(id) => {
                let defs = self.defs.read().unwrap();
                match &defs[id.0] {
                    CIRTypeDef::Union(u) => self.union_key(u),
                    _ => unreachable!(),
                }
            }
            CIRType::Enum(id) => {
                let defs = self.defs.read().unwrap();
                match &defs[id.0] {
                    CIRTypeDef::Enum(e) => self.enum_key(e),
                    _ => unreachable!(),
                }
            }
            CIRType::FuncType(f) => {
                let params: Vec<CIRTypeKey> = f.params.iter().map(|p| self.type_to_key(p)).collect();
                CIRTypeKey::FuncType(params, Box::new(self.type_to_key(&f.ret_type)), f.is_var)
            }
            CIRType::Dynamic(_) => CIRTypeKey::Dynamic,
        }
    }
}
