// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::abi::helpers::align_offset;
use crate::abi::layout::{ABIFieldOffsetInfo, ABITypeLayout};
use crate::abi::target::{ABITargetArch, ABITargetInfo};
use crate::cir::cir::CIREnumVariant;
use crate::cir::types::*;
use cyrusc_source_loc::{FileID, Loc};
use cyrusc_typed_ast::stmts::TypedTypeArgs;
use cyrusc_typed_ast::types::PlainType;
use cyrusc_typed_ast::types::TypeDeclID;
use fx_hash::{FxHashMap, FxHashMapExt};
use std::fmt;
use std::sync::{Arc, Mutex, OnceLock, RwLock};

/// A unique handle to a canonical type stored in the context.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CIRTypeContextID(usize);

/// Uniquely identifies a named type declaration combined with its type arguments.
pub type CIRTypeContextDeclKey = (TypeDeclID, TypedTypeArgs);

/// The central type registry which owns all canonical type definitions.
///
/// Provides:
/// - deduplication
/// - interning
/// - layout caching
/// - recursion prevention
///
/// during type lowering.
pub struct CIRTypeContext {
    pub target_info: ABITargetInfo,
    defs: RwLock<Vec<CIRTypeDef>>,
    key_to_id: RwLock<FxHashMap<CIRTypeKey, CIRTypeContextID>>,
    layouts: RwLock<FxHashMap<CIRTypeContextID, ABITypeLayout>>,
    layout_locks: RwLock<FxHashMap<CIRTypeContextID, Arc<Mutex<()>>>>,
    in_progress: RwLock<FxHashMap<CIRTypeContextDeclKey, CIRTypeContextID>>,
    fat_ptr_type_id: OnceLock<CIRTypeContextID>,
}

/// A canonical type definition stored in the context arena.
#[derive(Debug, Clone)]
pub enum CIRTypeDef {
    Struct(CIRStructType),
    Union(CIRUnionType),
    Enum(CIREnumType),
}

/// Hash key for anonymous types.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum CIRTypeKey {
    Named(CIRTypeContextDeclKey),

    AnonStruct(Vec<CIRTypeKey>),
    AnonUnion(Vec<CIRTypeKey>),
    AnonEnum {
        variants: Vec<EnumVariantKey>,
        tag: Box<CIRTypeKey>,
    },

    Plain(PlainType),
    Pointer(Box<CIRTypeKey>),
    Array(Box<CIRTypeKey>, usize),
    FuncType(Vec<CIRTypeKey>, Box<CIRTypeKey>, bool),

    // Pre-registered type.
    Dynamic,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum EnumVariantKey {
    Unit,
    Valued(Box<CIRTypeKey>),
    Payload(Vec<CIRTypeKey>),
}

impl CIRTypeContext {
    #[inline]
    pub fn new(target_info: ABITargetInfo) -> Self {
        let ctx = Self {
            target_info,
            defs: RwLock::new(Vec::new()),
            key_to_id: RwLock::new(FxHashMap::default()),
            layouts: RwLock::new(FxHashMap::default()),
            layout_locks: RwLock::new(FxHashMap::new()),
            in_progress: RwLock::new(FxHashMap::default()),
            fat_ptr_type_id: OnceLock::new(),
        };

        let fat_ptr_type_id = ctx.create_fat_ptr_type();
        ctx.fat_ptr_type_id.set(fat_ptr_type_id).unwrap();

        ctx
    }

    pub fn get_named_type(&self, decl_key: &CIRTypeContextDeclKey) -> Option<CIRTypeContextID> {
        let key = CIRTypeKey::Named(decl_key.clone());
        self.key_to_id.read().unwrap().get(&key).copied()
    }

    pub fn update_def(&self, id: CIRTypeContextID, def: CIRTypeDef) {
        self.defs.write().unwrap()[id.0] = def;
        self.layouts.write().unwrap().remove(&id);
    }

    pub fn insert_struct(&self, struct_type: CIRStructType) -> CIRTypeContextID {
        let key = self.struct_key(&struct_type);
        {
            let key_to_id = self.key_to_id.read().unwrap();
            if let Some(&type_id) = key_to_id.get(&key) {
                return type_id;
            }
        }
        let mut defs = self.defs.write().unwrap();
        let mut key_to_id = self.key_to_id.write().unwrap();
        if let Some(&type_id) = key_to_id.get(&key) {
            return type_id;
        }
        let type_id = CIRTypeContextID(defs.len());
        defs.push(CIRTypeDef::Struct(struct_type));
        key_to_id.insert(key, type_id);
        type_id
    }

    pub fn insert_union(&self, union_type: CIRUnionType) -> CIRTypeContextID {
        let key = self.union_key(&union_type);
        {
            let key_to_id = self.key_to_id.read().unwrap();
            if let Some(&type_id) = key_to_id.get(&key) {
                return type_id;
            }
        }
        let mut defs = self.defs.write().unwrap();
        let mut key_to_id = self.key_to_id.write().unwrap();
        if let Some(&type_id) = key_to_id.get(&key) {
            return type_id;
        }
        let type_id = CIRTypeContextID(defs.len());
        defs.push(CIRTypeDef::Union(union_type));
        key_to_id.insert(key, type_id);
        type_id
    }

    pub fn insert_enum(&self, enum_type: CIREnumType) -> CIRTypeContextID {
        let key = self.enum_key(&enum_type);
        {
            let key_to_id = self.key_to_id.read().unwrap();
            if let Some(&type_id) = key_to_id.get(&key) {
                return type_id;
            }
        }
        let mut defs = self.defs.write().unwrap();
        let mut key_to_id = self.key_to_id.write().unwrap();
        if let Some(&type_id) = key_to_id.get(&key) {
            return type_id;
        }
        let type_id = CIRTypeContextID(defs.len());
        defs.push(CIRTypeDef::Enum(enum_type));
        key_to_id.insert(key, type_id);
        type_id
    }

    #[inline]
    pub fn get_struct(&self, type_id: CIRTypeContextID) -> CIRStructType {
        let defs = self.defs.read().unwrap();

        match &defs[type_id.0] {
            CIRTypeDef::Struct(struct_type) => struct_type.clone(),
            _ => panic!("not a struct"),
        }
    }

    #[inline]
    pub fn get_union(&self, type_id: CIRTypeContextID) -> CIRUnionType {
        let defs = self.defs.read().unwrap();

        match &defs[type_id.0] {
            CIRTypeDef::Union(union_type) => union_type.clone(),
            _ => panic!("not a union"),
        }
    }

    #[inline]
    pub fn get_enum(&self, type_id: CIRTypeContextID) -> CIREnumType {
        let defs = self.defs.read().unwrap();

        match &defs[type_id.0] {
            CIRTypeDef::Enum(enum_type) => enum_type.clone(),
            _ => panic!("not an enum"),
        }
    }

    pub fn get_or_compute_layout(&self, type_id: CIRTypeContextID) -> ABITypeLayout {
        // check if already computed
        {
            let cache = self.layouts.read().unwrap();
            if let Some(layout) = cache.get(&type_id) {
                return layout.clone();
            }
        }

        // IMPORTANT(to prevent data race):
        // get or create a per-type mutex
        let lock = {
            let mut locks = self.layout_locks.write().unwrap();
            locks.entry(type_id).or_insert_with(|| Arc::new(Mutex::new(()))).clone()
        };

        // thread lock this type
        let _guard = lock.lock().unwrap();

        // IMPORTANT(to prevent data race):
        // DOUBLE-CHECK after acquiring the lock
        {
            let cache = self.layouts.read().unwrap();
            if let Some(layout) = cache.get(&type_id) {
                return layout.clone();
            }
        }

        let layout = self.compute_layout_for_def(type_id);
        self.layouts.write().unwrap().insert(type_id, layout.clone());
        layout
    }

    fn compute_layout_for_def(&self, id: CIRTypeContextID) -> ABITypeLayout {
        let defs = self.defs.read().unwrap();
        let def = &defs[id.0];
        match def {
            CIRTypeDef::Struct(struct_type) => self.compute_struct_layout(struct_type),
            CIRTypeDef::Union(union_type) => self.compute_union_layout(union_type),
            CIRTypeDef::Enum(enum_type) => self.compute_enum_layout(enum_type),
        }
    }

    #[inline]
    pub fn start_lowering(&self, decl_key: CIRTypeContextDeclKey, handle: CIRTypeContextID) {
        self.in_progress.write().unwrap().insert(decl_key, handle);
    }

    #[inline]
    pub fn finish_lowering(&self, decl_key: &CIRTypeContextDeclKey) {
        self.in_progress.write().unwrap().remove(&decl_key);
    }

    #[inline]
    pub fn is_lowering(&self, decl_key: &CIRTypeContextDeclKey) -> bool {
        self.in_progress.read().unwrap().contains_key(decl_key)
    }

    pub fn get_in_progress_handle(&self, decl_key: &CIRTypeContextDeclKey) -> CIRTypeContextID {
        self.in_progress
            .read()
            .unwrap()
            .get(decl_key)
            .copied()
            .expect("in-progress handle not found")
    }

    fn struct_key(&self, struct_type: &CIRStructType) -> CIRTypeKey {
        match &struct_type.decl_key {
            Some(decl_key) => CIRTypeKey::Named(decl_key.clone()),
            None => {
                let fields: Vec<CIRTypeKey> = struct_type.fields.iter().map(|f| self.type_to_key(f)).collect();
                CIRTypeKey::AnonStruct(fields)
            }
        }
    }

    fn union_key(&self, union_type: &CIRUnionType) -> CIRTypeKey {
        match &union_type.decl_key {
            Some(decl_key) => CIRTypeKey::Named(decl_key.clone()),
            None => {
                let fields: Vec<CIRTypeKey> = union_type.fields.iter().map(|ty| self.type_to_key(ty)).collect();

                CIRTypeKey::AnonUnion(fields)
            }
        }
    }

    fn enum_key(&self, enum_type: &CIREnumType) -> CIRTypeKey {
        match &enum_type.decl_key {
            Some(decl_key) => CIRTypeKey::Named(decl_key.clone()),
            None => {
                let variants: Vec<EnumVariantKey> = enum_type
                    .variants
                    .iter()
                    .map(|variant| match variant {
                        CIREnumVariant::Unit(..) => EnumVariantKey::Unit,
                        CIREnumVariant::Valued(_, ty, _) => EnumVariantKey::Valued(Box::new(self.type_to_key(ty))),
                        CIREnumVariant::Payload(_, struct_type, _) => {
                            let keys: Vec<CIRTypeKey> =
                                struct_type.fields.iter().map(|ty| self.type_to_key(ty)).collect();

                            EnumVariantKey::Payload(keys)
                        }
                    })
                    .collect();

                let tag_key = self.type_to_key(&enum_type.tag_type_or_infer_or_default());

                CIRTypeKey::AnonEnum {
                    variants,
                    tag: Box::new(tag_key),
                }
            }
        }
    }

    fn type_to_key(&self, ty: &CIRType) -> CIRTypeKey {
        match ty.const_inner() {
            CIRType::Const(_) => unreachable!(),

            CIRType::Plain(plain_type) => CIRTypeKey::Plain(plain_type.clone()),

            CIRType::Pointer(inner) => CIRTypeKey::Pointer(Box::new(self.type_to_key(inner))),

            CIRType::Array(array_type) => {
                CIRTypeKey::Array(Box::new(self.type_to_key(&array_type.element_type)), array_type.len)
            }

            CIRType::Struct(type_id) => {
                let defs = self.defs.read().unwrap();

                match &defs[type_id.0] {
                    CIRTypeDef::Struct(struct_type) => match &struct_type.decl_key {
                        Some(decl_key) => CIRTypeKey::Named(decl_key.clone()),
                        None => self.struct_key(struct_type),
                    },
                    _ => unreachable!(),
                }
            }

            CIRType::Union(type_id) => {
                let defs = self.defs.read().unwrap();

                match &defs[type_id.0] {
                    CIRTypeDef::Union(union_type) => match &union_type.decl_key {
                        Some(decl_key) => CIRTypeKey::Named(decl_key.clone()),
                        None => self.union_key(union_type),
                    },
                    _ => unreachable!(),
                }
            }

            CIRType::Enum(type_id) => {
                let defs = self.defs.read().unwrap();

                match &defs[type_id.0] {
                    CIRTypeDef::Enum(enum_type) => match &enum_type.decl_key {
                        Some(decl_key) => CIRTypeKey::Named(decl_key.clone()),
                        None => self.enum_key(enum_type),
                    },
                    _ => unreachable!(),
                }
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

// Pre-registered types.
impl CIRTypeContext {
    #[inline]
    pub fn fat_ptr_type(&self) -> CIRType {
        CIRType::Struct(*self.fat_ptr_type_id.get().expect("fat_ptr_type not initialized"))
    }

    fn create_fat_ptr_type(&self) -> CIRTypeContextID {
        let struct_type = CIRStructType {
            decl_key: None,
            name: Some("__fat_ptr".into()),
            fields: vec![
                CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void))),
                CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void))),
            ],
            fields_info: vec![
                ("data_ptr".into(), Loc::default(FileID(0))),
                ("vtable_ptr".into(), Loc::default(FileID(0))),
            ],
            repr_attr: None,
            align: None,
            loc: Loc::default(FileID(0)),
        };
        self.insert_struct(struct_type)
    }
}

// Compute layouts.
impl CIRTypeContext {
    pub fn layout_of(&self, ty: &CIRType) -> ABITypeLayout {
        match ty.const_inner() {
            CIRType::Struct(type_id) | CIRType::Union(type_id) | CIRType::Enum(type_id) => {
                self.get_or_compute_layout(*type_id)
            }

            CIRType::Plain(plain_type) => plain_type_layout(&self.target_info, plain_type),

            CIRType::Const(ty) => self.layout_of(ty.const_inner()),

            CIRType::Pointer(_) => {
                let size = self.target_info.pointer_size();
                ABITypeLayout::normal(size, size, Vec::new())
            }

            CIRType::Array(array_type) => {
                let element_layout = self.layout_of(&array_type.element_type);
                let total_size = element_layout.size * array_type.len as u32;

                let mut field_offsets = Vec::new();
                for i in 0..array_type.len {
                    field_offsets.push(ABIFieldOffsetInfo::Normal {
                        index: i as u32,
                        offset: element_layout.align * i as u32,
                        original_index: i,
                    });
                }

                ABITypeLayout::aggregate(total_size, element_layout.align, field_offsets)
            }

            CIRType::FuncType(_) => {
                let size = self.target_info.pointer_size();

                ABITypeLayout::normal(size, size, Vec::new())
            }

            CIRType::Dynamic(_) => {
                let size = self.target_info.pointer_size() * 2;
                ABITypeLayout::normal(size, self.target_info.pointer_size(), Vec::new())
            }
        }
    }

    pub fn compute_struct_layout(&self, struct_type: &CIRStructType) -> ABITypeLayout {
        let mut offset = 0u32;
        let mut max_align = 1u32;
        let mut field_offsets = Vec::new();
        let is_packed = struct_type.is_packed();
        let mut field_offset_index = 0u32;

        for (field_original_index, ty) in struct_type.fields.iter().enumerate() {
            let field_layout = self.layout_of(ty);

            let effective_field_align = if is_packed { 1 } else { field_layout.align };

            if !is_packed {
                let padding = (effective_field_align - (offset % effective_field_align)) % effective_field_align;

                if padding > 0 {
                    field_offsets.push(ABIFieldOffsetInfo::padding(field_offset_index, offset, padding));
                    field_offset_index += 1;
                    offset += padding;
                }
            }

            field_offsets.push(ABIFieldOffsetInfo::normal(
                field_offset_index,
                offset,
                field_original_index,
            ));
            field_offset_index += 1;

            offset += field_layout.size;
            max_align = max_align.max(field_layout.align);
        }

        if let Some(explicit_align) = struct_type.align {
            max_align = max_align.max(explicit_align as u32);
        }

        if is_packed {
            max_align = 1;
        }

        let total_size = align_offset(offset, max_align);

        if total_size > offset {
            field_offsets.push(ABIFieldOffsetInfo::padding(
                field_offsets.len() as u32,
                offset,
                total_size - offset,
            ));
        }

        ABITypeLayout::aggregate(total_size, max_align, field_offsets)
    }

    pub fn compute_union_layout(&self, union_type: &CIRUnionType) -> ABITypeLayout {
        let mut max_size = 0u32;
        let mut max_align = 1u32;
        let mut field_offsets = Vec::new();

        for (original_index, ty) in union_type.fields.iter().enumerate() {
            let field_layout = self.layout_of(ty);

            max_size = max_size.max(field_layout.size);
            max_align = max_align.max(field_layout.align);

            field_offsets.push(ABIFieldOffsetInfo::normal(original_index as u32, 0, original_index));
        }

        let total_size = align_offset(max_size, max_align);
        ABITypeLayout::aggregate(total_size, max_align, field_offsets)
    }

    pub fn compute_enum_layout(&self, enum_type: &CIREnumType) -> ABITypeLayout {
        let tag_type = enum_type.tag_type_or_infer_or_default();

        if enum_type.is_scalar_optimizable() {
            return self.layout_of(&tag_type);
        }

        let tag_layout = self.layout_of(&tag_type);
        let tag_size = tag_layout.size;
        let tag_align = tag_layout.align;

        let mut max_payload_size = 0u32;
        let mut max_payload_align = 1u32;

        for variant in &enum_type.variants {
            let (variant_size, variant_align) = match variant {
                CIREnumVariant::Unit(_, _) => (0, 1),
                CIREnumVariant::Valued(_, value_type, _) => {
                    let layout = self.layout_of(value_type);
                    (layout.size, layout.align)
                }
                CIREnumVariant::Payload(_, struct_type, _) => {
                    let layout = self.compute_struct_layout(struct_type);

                    (layout.size, layout.align)
                }
            };

            max_payload_size = max_payload_size.max(variant_size);
            max_payload_align = max_payload_align.max(variant_align);
        }

        if max_payload_size == 0 && enum_type.includes_payload() {
            max_payload_size = 1;
            max_payload_align = 1;
        }

        if let Some(align) = enum_type.align {
            max_payload_align = max_payload_align.max(align as u32);
        }

        let payload_offset = ((tag_size + (max_payload_align - 1)) / max_payload_align) * max_payload_align;

        let mut total_align = tag_align.max(max_payload_align);
        if let Some(align) = enum_type.align {
            total_align = total_align.max(align as u32);
        }

        let mut total_size = payload_offset + max_payload_size;
        total_size = ((total_size + (total_align - 1)) / total_align) * total_align;

        ABITypeLayout::aggregate(total_size, total_align, Vec::new())
    }
}

fn plain_type_layout(info: &ABITargetInfo, plain_type: &PlainType) -> ABITypeLayout {
    use PlainType::*;

    match plain_type {
        UIntPtr | IntPtr | ISize | USize => {
            let size = info.pointer_size();
            ABITypeLayout::normal(size, size, Vec::new())
        }

        Int8 | UInt8 | Bool => ABITypeLayout::normal(1, 1, Vec::new()),
        Int16 | UInt16 => ABITypeLayout::normal(2, 2, Vec::new()),
        Int32 | UInt32 => ABITypeLayout::normal(4, 4, Vec::new()),
        Int64 | UInt64 => ABITypeLayout::normal(8, 8, Vec::new()),
        Int128 | UInt128 => {
            let align = match info.arch {
                ABITargetArch::X86_64 | ABITargetArch::Aarch64 => 16,
                ABITargetArch::RiscV64 => 16,
                ABITargetArch::Wasm32 => 8,
            };
            ABITypeLayout::normal(16, align, Vec::new())
        }

        Float16 => ABITypeLayout::normal(2, 2, Vec::new()),
        Float32 => ABITypeLayout::normal(4, 4, Vec::new()),
        Float64 => ABITypeLayout::normal(8, 8, Vec::new()),
        Float128 => {
            let align = match info.arch {
                ABITargetArch::X86_64 | ABITargetArch::Aarch64 => 16,
                ABITargetArch::RiscV64 => 16,
                ABITargetArch::Wasm32 => 8,
            };
            ABITypeLayout::normal(16, align, Vec::new())
        }

        Void => ABITypeLayout::normal(0, 1, Vec::new()),
        Null => {
            let size = info.pointer_size();
            ABITypeLayout::normal(size, size, Vec::new())
        }
    }
}

impl fmt::Display for CIRTypeContextID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
