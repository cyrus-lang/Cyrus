// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                                                         │
// │  Cyrus Programming Language                                             │
// │  https://github.com/cyrus-lang/Cyrus                                    │
// │                                                                         │
// │  A general-purpose, statically-typed, manually memory-managed           │
// │  programming language designed for performance-critical applications.   │
// │                                                                         │
// │  Copyright (c) 2026 The Cyrus Programming Language Project              │
// │                                                                         │
// │  This program is free software: you can redistribute it and/or modify   │
// │  it under the terms of the GNU General Public License as published by   │
// │  the Free Software Foundation, either version 3 of the License, or      │
// │  (at your option) any later version.                                    │
// │                                                                         │
// │  This program is distributed in the hope that it will be useful,        │
// │  but WITHOUT ANY WARRANTY; without even the implied warranty of         │
// │  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the           │
// │  GNU General Public License for more details.                           │
// │                                                                         │
// │  You should have received a copy of the GNU General Public License      │
// │  along with this program. If not, see <https://www.gnu.org/licenses/>.  │
// │                                                                         │
// └─────────────────────────────────────────────────────────────────────────┘

use crate::{SymbolID, exprs::TypedIdentifier, format::format_sema_ty, types::SemanticType};
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    hash::Hash,
    rc::{Rc, Weak},
};

pub type ChildGenericParamSymbolID = SymbolID;

#[derive(Debug, Clone, Default)]
pub struct GenericMappingCtx {
    pub named: HashMap<GenericMappingEntry, SemanticType>,
    pub linked_gps: HashMap<GenericMappingEntry, GenericMappingEntry>,
    pub parent: Option<Weak<GenericMappingCtx>>,
}

#[derive(Debug, Clone, Eq)]
pub struct GenericMappingEntry {
    pub name: String,
}

impl PartialEq for GenericMappingCtx {
    fn eq(&self, other: &Self) -> bool {
        mapping_ctx_eq(self, other)
    }
}

pub fn mapping_ctx_eq(a: &GenericMappingCtx, b: &GenericMappingCtx) -> bool {
    let a = a.normalized();
    let b = b.normalized();

    if a.named.len() != b.named.len() || a.linked_gps != b.linked_gps {
        return false;
    }

    for (k, v1) in &a.named {
        match b.get_with_name(&k.name) {
            Some(v2) if *v1 == v2 => {}
            _ => return false,
        }
    }

    match (
        a.parent.as_ref().and_then(|p| p.upgrade()),
        b.parent.as_ref().and_then(|p| p.upgrade()),
    ) {
        (Some(pa), Some(pb)) => mapping_ctx_eq(&pa, &pb),
        (None, None) => true,
        _ => false,
    }
}

pub fn mapping_ctx_eq_refcell(a: &Rc<RefCell<GenericMappingCtx>>, b: &Rc<RefCell<GenericMappingCtx>>) -> bool {
    let a_ref = a.borrow();
    let b_ref = b.borrow();

    mapping_ctx_eq(&a_ref, &b_ref)
}

impl GenericMappingCtx {
    pub fn normalized(&self) -> GenericMappingCtx {
        let mut named = HashMap::new();
        let mut linked_gps = HashMap::new();

        let mut cur: Option<Rc<GenericMappingCtx>> = Some(Rc::new(self.clone()));

        while let Some(ctx_rc) = cur {
            let ctx = &*ctx_rc;

            for (k, v) in &ctx.named {
                named.entry(k.clone()).or_insert(v.clone());
            }
            for (k, v) in &ctx.linked_gps {
                linked_gps.entry(k.clone()).or_insert(v.clone());
            }

            cur = ctx.parent.as_ref().and_then(|p| p.upgrade());
        }

        GenericMappingCtx {
            named,
            linked_gps,
            parent: None,
        }
    }

    pub fn get_with_name(&self, name: &str) -> Option<SemanticType> {
        if let Some((_, ty)) = self.named.iter().find(|(id, _)| id.name == name) {
            return Some(ty.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.upgrade()?.get_with_name(name);
        }

        None
    }

    pub fn insert_named(&mut self, entry: GenericMappingEntry, ty: SemanticType) {
        if !self.named.contains_key(&entry) {
            self.named.insert(entry, ty);
        }
    }

    pub fn get_linked_by_name(&self, child_name: &str) -> Option<GenericMappingEntry> {
        if let Some(parent) = self
            .linked_gps
            .iter()
            .find(|(child, _)| child.name == child_name)
            .map(|(_, parent)| parent.clone())
        {
            return Some(parent);
        }

        if let Some(parent_ctx) = &self.parent {
            return parent_ctx.upgrade()?.get_linked_by_name(child_name);
        }

        None
    }

    pub fn insert_linked(&mut self, child: GenericMappingEntry, parent: GenericMappingEntry) {
        if self.named.contains_key(&child) || self.linked_gps.contains_key(&child) {
            return;
        }
        if child.name == parent.name {
            return;
        }
        self.linked_gps.insert(child, parent);
    }

    pub fn new_root() -> Self {
        Self {
            named: HashMap::new(),
            linked_gps: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_child(parent: Weak<GenericMappingCtx>) -> Self {
        Self {
            named: HashMap::new(),
            linked_gps: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn format(&self, format_symbol: &impl Fn(SymbolID) -> String) -> String {
        let mut parts = Vec::new();

        for (entry, sema_ty) in &self.named {
            let type_str = format_sema_ty(sema_ty.clone(), format_symbol);
            parts.push(format!("{} = {}", entry, type_str));
        }

        for (child, parent) in &self.linked_gps {
            parts.push(format!("{} = {}", child.name, parent.name));
        }

        parts.join(", ")
    }
}

impl Hash for GenericMappingCtx {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let named_entries: Vec<_> = self.named.iter().collect();
        for (k, v) in named_entries {
            k.hash(state);
            v.hash(state);
        }

        let linked_entries: Vec<_> = self.linked_gps.iter().collect();
        for (k, v) in linked_entries {
            k.hash(state);
            v.hash(state);
        }

        // hash parent recursively
        if let Some(parent) = &self.parent {
            parent.upgrade().inspect(|ctx| ctx.hash(state));
        } else {
            None::<()>.hash(state);
        }
    }
}

impl Eq for GenericMappingCtx {}

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
