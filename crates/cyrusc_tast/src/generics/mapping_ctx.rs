use crate::{SymbolID, exprs::TypedIdentifier, format::format_sema_ty, types::SemanticType};
use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

pub type ChildGenericParamSymbolID = SymbolID;

#[derive(Debug, Clone, Default)]
pub struct GenericMappingCtx {
    pub named: HashMap<TypedIdentifier, SemanticType>,
    pub linked_gps: HashMap<TypedIdentifier, TypedIdentifier>,
    pub parent: Option<Rc<GenericMappingCtx>>,
}

impl PartialEq for GenericMappingCtx {
    fn eq(&self, other: &Self) -> bool {
        self.named == other.named && self.linked_gps == other.linked_gps && self.parent == other.parent
    }
}

pub fn mapping_ctx_eq(a: &GenericMappingCtx, b: &GenericMappingCtx) -> bool {
    let a = a.normalized();
    let b = b.normalized();

    if a.named.len() != b.named.len() || a.linked_gps != b.linked_gps {
        return false;
    }

    for (k, v) in &a.named {
        match b.named.get(k) {
            Some(v2) if v == v2 => {}
            _ => return false,
        }
    }

    match (&a.parent, &b.parent) {
        (Some(pa), Some(pb)) => mapping_ctx_eq(pa, pb),
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

        let mut cur = Some(self);
        while let Some(ctx) = cur {
            // child overrides parent
            for (k, v) in &ctx.named {
                named.entry(k.clone()).or_insert(v.clone());
            }
            for (k, v) in &ctx.linked_gps {
                linked_gps.entry(k.clone()).or_insert(v.clone());
            }
            cur = ctx.parent.as_deref();
        }

        GenericMappingCtx {
            named,
            linked_gps,
            parent: None,
        }
    }

    pub fn get_local_with_symbol_id(&self, symbol_id: SymbolID) -> Option<SemanticType> {
        self.named
            .iter()
            .find_map(|(k, v)| (k.symbol_id == symbol_id).then(|| v.clone()))
    }

    pub fn get_with_name(&self, name: &str) -> Option<SemanticType> {
        if let Some((_, ty)) = self.named.iter().find(|(id, _)| id.name == name) {
            return Some(ty.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.get_with_name(name);
        }

        None
    }

    pub fn insert_named(&mut self, id: TypedIdentifier, ty: SemanticType) {
        if !self.named.contains_key(&id) {
            self.named.insert(id, ty);
        }
    }

    pub fn get_linked_by_name(&self, child_name: &str) -> Option<TypedIdentifier> {
        if let Some(parent) = self
            .linked_gps
            .iter()
            .find(|(child, _)| child.name == child_name)
            .map(|(_, parent)| parent.clone())
        {
            return Some(parent);
        }

        if let Some(parent_ctx) = &self.parent {
            return parent_ctx.get_linked_by_name(child_name);
        }

        None
    }

    pub fn insert_linked(&mut self, child: TypedIdentifier, parent: TypedIdentifier) {
        if self.named.contains_key(&child) || self.linked_gps.contains_key(&child) {
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

    pub fn new_child(parent: Rc<GenericMappingCtx>) -> Self {
        Self {
            named: HashMap::new(),
            linked_gps: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn format(&self, format_symbol: &impl Fn(SymbolID) -> String) -> String {
        let mut parts = Vec::new();

        for (param_id, sema_ty) in &self.named {
            let param_name = format_symbol(param_id.symbol_id);
            let type_str = format_sema_ty(sema_ty.clone(), format_symbol);
            parts.push(format!("{} = {}", param_name, type_str));
        }

        for (child, parent) in &self.linked_gps {
            parts.push(format!("{} = {}", child.name, parent.name));
        }

        parts.join(", ")
    }
}

impl Hash for GenericMappingCtx {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // sort named entries by symbol_id for deterministic order
        let mut named_entries: Vec<_> = self.named.iter().collect();
        named_entries.sort_by_key(|(k, _)| k.symbol_id);
        for (k, v) in named_entries {
            k.hash(state);
            v.hash(state);
        }

        // sort linked_gps entries by child symbol_id
        let mut linked_entries: Vec<_> = self.linked_gps.iter().collect();
        linked_entries.sort_by_key(|(k, _)| k.symbol_id); // sort by child symbol_id
        for (k, v) in linked_entries {
            k.hash(state);
            v.hash(state);
        }

        // hash parent recursively
        if let Some(parent) = &self.parent {
            parent.hash(state);
        } else {
            None::<()>.hash(state);
        }
    }
}

impl Eq for GenericMappingCtx {}
