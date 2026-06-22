// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::abi::layout::{ABITypeLayout, type_layout};
use crate::abi::target::ABITargetInfo;
use crate::cir::cir::CIREnumVariant;
use crate::cir::types::*;
use cyrusc_typed_ast::types::PlainType;
use fx_hash::FxHashMap;
use std::sync::RwLock;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CIRTypeContextID(usize);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum CIRTypeKey {
    Plain(PlainType),
    Const(Box<CIRTypeKey>),
    Pointer(Box<CIRTypeKey>),
    Array(Box<CIRTypeKey>, usize),
    Tuple(Vec<CIRTypeKey>),
    Struct(Vec<CIRTypeKey>),
    Union(Vec<CIRTypeKey>),
    Enum(Vec<EnumVariantKey>, Box<CIRTypeKey>),
    FuncType(Vec<CIRTypeKey>, Box<CIRTypeKey>, bool),
    Dynamic,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum EnumVariantKey {
    Unit,
    Valued(Box<CIRTypeKey>),
    Payload(Vec<CIRTypeKey>),
}

struct CIRTypeEntry {
    /// The canonical type (None for forward-declared placeholders).
    ty: Option<CIRType>,

    /// Cached layout, computed on first access.
    layout: RwLock<Option<ABITypeLayout>>,
}

impl CIRTypeEntry {
    fn placeholder() -> Self {
        Self {
            ty: None,
            layout: RwLock::new(None),
        }
    }

    fn new(ty: CIRType) -> Self {
        Self {
            ty: Some(ty),
            layout: RwLock::new(None),
        }
    }
}

pub struct CIRTypeContext {
    /// All types in the context, indexed by CIRTypeContextID
    types: RwLock<Vec<CIRTypeEntry>>,

    /// Map from type key to handle for deduplication.
    key_to_id: RwLock<FxHashMap<CIRTypeKey, CIRTypeContextID>>,

    /// Target info for layout computation.
    target_info: ABITargetInfo,
}

impl CIRTypeContext {
    pub fn new(target_info: ABITargetInfo) -> Self {
        Self {
            types: RwLock::new(Vec::new()),
            key_to_id: RwLock::new(FxHashMap::default()),
            target_info,
        }
    }

    pub fn insert_type_placeholder(&self) -> CIRTypeContextID {
        let mut types = self.types.write().unwrap();
        let id = CIRTypeContextID(types.len());
        types.push(CIRTypeEntry::placeholder());
        id
    }

    pub fn insert_type(&self, ty: CIRType) -> CIRTypeContextID {
        let mut types = self.types.write().unwrap();
        let id = CIRTypeContextID(types.len());
        types.push(CIRTypeEntry::new(ty));
        id
    }

    pub fn resolve_placeholder(&self, id: CIRTypeContextID, ty: CIRType) {
        let mut types = self.types.write().unwrap();

        if let Some(entry) = types.get_mut(id.0) {
            entry.ty = Some(ty);
        }
    }

    /// Returns existing handle if the type already exists,
    /// otherwise registers and returns a new handle.
    ///
    /// This is the MAIN method for type registration (it deduplicates types).
    pub fn register(&self, ty: CIRType) -> CIRTypeContextID {
        let key = self.type_to_key(&ty);

        {
            let key_to_id = self.key_to_id.read().unwrap();
            if let Some(&id) = key_to_id.get(&key) {
                return id;
            }
        }

        let mut types = self.types.write().unwrap();
        let mut key_to_id = self.key_to_id.write().unwrap();

        if let Some(&id) = key_to_id.get(&key) {
            return id;
        }

        let id = CIRTypeContextID(types.len());
        types.push(CIRTypeEntry::new(ty));
        key_to_id.insert(key, id);

        id
    }

    /// Register a type and immediately compute its layout.
    pub fn register_with_layout(&self, ty: CIRType) -> (CIRTypeContextID, ABITypeLayout) {
        let id = self.register(ty);
        let layout = self.get_or_compute_layout(id);
        (id, layout)
    }

    pub fn get_type(&self, id: CIRTypeContextID) -> Option<CIRType> {
        let types = self.types.read().unwrap();
        types.get(id.0).and_then(|entry| entry.ty.clone())
    }

    /// Check if a handle refers to a placeholder.
    pub fn is_placeholder(&self, id: CIRTypeContextID) -> bool {
        let types = self.types.read().unwrap();
        types.get(id.0).map(|entry| entry.ty.is_none()).unwrap_or(false)
    }

    /// Get the layout for a type, computing and caching it if necessary.
    pub fn get_or_compute_layout(&self, id: CIRTypeContextID) -> ABITypeLayout {
        {
            let types = self.types.read().unwrap();
            if let Some(entry) = types.get(id.0) {
                let layout_guard = entry.layout.read().unwrap();
                if let Some(layout) = layout_guard.as_ref() {
                    return layout.clone();
                }
            }
        }

        let types = self.types.read().unwrap();
        let entry = types.get(id.0).expect("invalid type ID");

        let ty = entry
            .ty
            .as_ref()
            .expect("cannot compute layout for unresolved placeholder");

        let layout = type_layout(&self.target_info, ty);

        let mut layout_guard = entry.layout.write().unwrap();
        *layout_guard = Some(layout.clone());

        layout
    }

    fn type_to_key(&self, ty: &CIRType) -> CIRTypeKey {
        match ty {
            CIRType::Plain(plain_type) => CIRTypeKey::Plain(plain_type.clone()),
            CIRType::Const(inner) => CIRTypeKey::Const(Box::new(self.type_to_key(inner))),
            CIRType::Pointer(inner) => CIRTypeKey::Pointer(Box::new(self.type_to_key(inner))),
            CIRType::Array(array_type) => {
                CIRTypeKey::Array(Box::new(self.type_to_key(&array_type.element_type)), array_type.len)
            }
            CIRType::Tuple(tuple_type) => {
                let elements: Vec<CIRTypeKey> = tuple_type.elements.iter().map(|f| self.type_to_key(f)).collect();

                CIRTypeKey::Tuple(elements)
            }
            CIRType::Struct(struct_type) => {
                let fields: Vec<CIRTypeKey> = struct_type.fields.iter().map(|f| self.type_to_key(f)).collect();

                CIRTypeKey::Struct(fields)
            }
            CIRType::Union(union_type) => {
                let fields: Vec<CIRTypeKey> = union_type.fields.iter().map(|f| self.type_to_key(f)).collect();

                CIRTypeKey::Union(fields)
            }
            CIRType::Enum(enum_type) => {
                let variants: Vec<EnumVariantKey> = enum_type
                    .variants
                    .iter()
                    .map(|v| match v {
                        &CIREnumVariant::Unit(..) => EnumVariantKey::Unit,
                        CIREnumVariant::Valued(_, ty, _) => EnumVariantKey::Valued(Box::new(self.type_to_key(ty))),
                        CIREnumVariant::Payload(_, fields, _) => {
                            let keys: Vec<CIRTypeKey> = fields.iter().map(|f| self.type_to_key(f)).collect();

                            EnumVariantKey::Payload(keys)
                        }
                    })
                    .collect();

                let tag_key = enum_type
                    .tag_type
                    .as_ref()
                    .map(|t| Box::new(self.type_to_key(t)))
                    .unwrap_or_else(|| Box::new(CIRTypeKey::Plain(PlainType::Int32)));

                CIRTypeKey::Enum(variants, tag_key)
            }
            CIRType::FuncType(func_type) => {
                let params: Vec<CIRTypeKey> = func_type.params.iter().map(|p| self.type_to_key(p)).collect();

                CIRTypeKey::FuncType(
                    params,
                    Box::new(self.type_to_key(&func_type.ret_type)),
                    func_type.is_var,
                )
            }
            CIRType::Dynamic(_) => CIRTypeKey::Dynamic,
        }
    }
}
