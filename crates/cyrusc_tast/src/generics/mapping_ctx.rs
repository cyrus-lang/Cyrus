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
    exprs::TypedIdentifier,
    format::format_sema_ty,
    generics::mapping_ctx_arena::{GenericMappingCtxArena, ParentGenericMappingCtxID},
    types::SemanticType,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    hash::Hash,
    rc::Rc,
    sync::{Arc, Mutex},
};

pub type ChildGenericParamSymbolID = SymbolID;

#[derive(Debug, Clone, Default)]
pub struct GenericMappingCtx {
    named: HashMap<GenericMappingEntry, SemanticType>,
    links: HashMap<GenericMappingEntry, GenericMappingEntry>,
    parent: Option<ParentGenericMappingCtxID>,
}

#[derive(Debug, Clone, Eq)]
pub struct GenericMappingEntry {
    pub name: String,
}

pub fn mapping_ctx_eq(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    a: &GenericMappingCtx,
    b: &GenericMappingCtx,
) -> bool {
    let (a, b) = {
        let mapping_ctx_arena = mapping_ctx_arena.lock().unwrap();
        (
            normalize_generic_mapping_ctx(&*mapping_ctx_arena, a),
            normalize_generic_mapping_ctx(&*mapping_ctx_arena, b),
        )
    };

    if a.named.len() != b.named.len() || a.links != b.links {
        return false;
    }

    for (k, v1) in &a.named {
        let sema_ty = {
            let mapping_ctx_arena = mapping_ctx_arena.lock().unwrap();
            b.resolve_with_name(&*mapping_ctx_arena, &k.name)
        };

        match sema_ty {
            Some(v2) if *v1 == v2 => {}
            _ => return false,
        }
    }

    match (
        a.parent.and_then(|parent_id| {
            let mapping_ctx_arena = mapping_ctx_arena.lock().unwrap();
            Some(mapping_ctx_arena.get(parent_id).unwrap().clone())
        }),
        b.parent.and_then(|parent_id| {
            let mapping_ctx_arena = mapping_ctx_arena.lock().unwrap();
            Some(mapping_ctx_arena.get(parent_id).unwrap().clone())
        }),
    ) {
        (Some(pa), Some(pb)) => mapping_ctx_eq(mapping_ctx_arena, &pa, &pb),
        (None, None) => true,
        _ => false,
    }
}

pub fn mapping_ctx_eq_refcell(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    a: &Rc<RefCell<GenericMappingCtx>>,
    b: &Rc<RefCell<GenericMappingCtx>>,
) -> bool {
    let a_ref = a.borrow();
    let b_ref = b.borrow();

    mapping_ctx_eq(mapping_ctx_arena, &a_ref, &b_ref)
}

pub fn normalize_generic_mapping_ctx(
    mapping_ctx_arena: &dyn GenericMappingCtxArena,
    generic_mapping_ctx: &GenericMappingCtx,
) -> GenericMappingCtx {
    let mut named = HashMap::new();
    let mut links = HashMap::new();

    let mut current_generic_mapping_ctx: Option<GenericMappingCtx> = Some(generic_mapping_ctx.clone());

    while let Some(generic_mapping_ctx) = current_generic_mapping_ctx {
        for (k, v) in &generic_mapping_ctx.named {
            named.entry(k.clone()).or_insert(v.clone());
        }
        for (k, v) in &generic_mapping_ctx.links {
            links.entry(k.clone()).or_insert(v.clone());
        }

        current_generic_mapping_ctx = generic_mapping_ctx
            .parent
            .and_then(|parent_id| Some(mapping_ctx_arena.get(parent_id).unwrap()))
            .cloned();
    }

    GenericMappingCtx {
        named,
        links,
        parent: None,
    }
}

impl GenericMappingCtx {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.named.is_empty() && self.links.is_empty() && self.parent.is_none()
    }

    #[inline]
    pub fn parent_id(&self) -> Option<ParentGenericMappingCtxID> {
        self.parent
    }

    #[inline]
    pub fn set_parent_id(&mut self, new_parent_id: ParentGenericMappingCtxID) {
        self.parent = Some(new_parent_id);
    }

    #[inline]
    pub fn named_mapping(&self) -> &HashMap<GenericMappingEntry, SemanticType> {
        &self.named
    }

    #[inline]
    pub fn links(&self) -> &HashMap<GenericMappingEntry, GenericMappingEntry> {
        &self.links
    }

    pub fn resolve_with_name(&self, mapping_ctx_arena: &dyn GenericMappingCtxArena, name: &str) -> Option<SemanticType> {
        if let Some((_, ty)) = self.named.iter().find(|(id, _)| id.name == name) {
            return Some(ty.clone());
        }

        if let Some(parent_id) = self.parent {
            let parent_mapping_ctx = mapping_ctx_arena.get(parent_id).unwrap();
            return parent_mapping_ctx.resolve_with_name(mapping_ctx_arena, name);
        }

        None
    }

    pub fn insert_named(&mut self, entry: GenericMappingEntry, ty: SemanticType) {
        if !self.named.contains_key(&entry) {
            self.named.insert(entry, ty);
        }
    }

    pub fn resolve_linked_by_name(
        &self,
        mapping_ctx_arena: &dyn GenericMappingCtxArena,
        child_name: &str,
    ) -> Option<GenericMappingEntry> {
        if let Some(parent) = self
            .links
            .iter()
            .find(|(child, _)| child.name == child_name)
            .map(|(_, parent)| parent.clone())
        {
            return Some(parent);
        }

        if let Some(parent_id) = self.parent {
            let parent_mapping_ctx = mapping_ctx_arena.get(parent_id).unwrap();
            return parent_mapping_ctx.resolve_linked_by_name(mapping_ctx_arena, child_name);
        }

        None
    }

    pub fn insert_linked(&mut self, child: GenericMappingEntry, parent: GenericMappingEntry) {
        if self.named.contains_key(&child) || self.links.contains_key(&child) {
            return;
        }
        if child.name == parent.name {
            return;
        }
        self.links.insert(child, parent);
    }

    pub fn new_root() -> Self {
        Self {
            named: HashMap::new(),
            links: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_child(parent_id: ParentGenericMappingCtxID) -> Self {
        Self {
            named: HashMap::new(),
            links: HashMap::new(),
            parent: Some(parent_id),
        }
    }

    pub fn new_manual(
        named: HashMap<GenericMappingEntry, SemanticType>,
        links: HashMap<GenericMappingEntry, GenericMappingEntry>,
        parent: Option<ParentGenericMappingCtxID>,
    ) -> Self {
        Self { named, links, parent }
    }

    pub fn format(&self, format_symbol: &impl Fn(SymbolID) -> String) -> String {
        let mut parts = Vec::new();

        for (entry, sema_ty) in &self.named {
            let type_str = format_sema_ty(sema_ty.clone(), format_symbol);
            parts.push(format!("{} = {}", entry, type_str));
        }

        for (child, parent) in &self.links {
            parts.push(format!("{} = {}", child.name, parent.name));
        }

        parts.join(", ")
    }
}

impl PartialEq for GenericMappingEntry {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Hash for GenericMappingEntry {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Display for GenericMappingEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl From<TypedIdentifier> for GenericMappingEntry {
    fn from(value: TypedIdentifier) -> Self {
        Self {
            name: value.name.clone(),
        }
    }
}

#[macro_export]
macro_rules! mapping_ctx_arena {
    ($self:expr, $export:ident, $body:stmt) => {{
        #[allow(unused_mut)]
        let mut $export = $self.mapping_ctx_arena.lock().unwrap();
        $body
    }};
}
