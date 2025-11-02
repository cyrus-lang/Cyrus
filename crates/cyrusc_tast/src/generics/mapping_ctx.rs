use crate::{SymbolID, exprs::TypedIdentifier, format::format_concrete_type, types::SemanticType};
use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

pub type ChildGenericParamSymbolID = SymbolID;

#[derive(Debug, Clone, Default)]
pub struct GenericMappingCtx {
    pub named: HashMap<TypedIdentifier, SemanticType>,
    pub linked_gps: HashMap<ChildGenericParamSymbolID, SymbolID>,
    pub parent: Option<Rc<GenericMappingCtx>>,
}

impl PartialEq for GenericMappingCtx {
    fn eq(&self, other: &Self) -> bool {
        self.named == other.named && self.linked_gps == other.linked_gps && self.parent == other.parent
    }
}

pub fn mapping_ctx_eq(a: &GenericMappingCtx, b: &GenericMappingCtx) -> bool {
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
    pub fn get_with_symbol_id(&self, symbol_id: SymbolID) -> Option<SemanticType> {
        if let Some(ty) = self
            .named
            .iter()
            .find_map(|(key, val)| (key.symbol_id == symbol_id).then(|| val.clone()))
        {
            return Some(ty);
        }

        if let Some(&mapped_id) = self.linked_gps.get(&symbol_id) {
            // Try to resolve the mapped ID recursively
            if let Some(ty) = self.get_with_symbol_id(mapped_id) {
                return Some(ty);
            }
        }

        if let Some(parent) = &self.parent {
            return parent.get_with_symbol_id(symbol_id);
        }

        None
    }

    pub fn insert_named(&mut self, id: TypedIdentifier, ty: SemanticType) {
        if !self.named.contains_key(&id) {
            self.named.insert(id, ty);
        }
    }

    pub fn insert_linked(&mut self, child_id: SymbolID, parent_id: SymbolID) {
        self.linked_gps.insert(child_id, parent_id);
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
            let type_str = format_concrete_type(sema_ty.clone(), format_symbol);
            parts.push(format!("{} = {}", param_name, type_str));
        }

        for (child_id, parent_id) in &self.linked_gps {
            let child_name = format_symbol(*child_id);
            let parent_name = format_symbol(*parent_id);
            parts.push(format!("{} = {}", child_name, parent_name));
        }

        parts.join(", ")
    }
}

impl Hash for GenericMappingCtx {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let mut named_entries: Vec<_> = self.named.iter().collect();
        named_entries.sort_by_key(|(k, _)| k.symbol_id);
        for (k, v) in named_entries {
            k.hash(state);
            v.hash(state);
        }

        let mut linked_entries: Vec<_> = self.linked_gps.iter().collect();
        linked_entries.sort_by_key(|(k, _)| *k);
        for (k, v) in linked_entries {
            k.hash(state);
            v.hash(state);
        }

        if let Some(parent) = &self.parent {
            parent.hash(state);
        } else {
            None::<()>.hash(state);
        }
    }
}

impl Eq for GenericMappingCtx {}
