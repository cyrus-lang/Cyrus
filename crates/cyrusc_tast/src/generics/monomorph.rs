use crate::{SymbolID, generics::mapping_ctx::GenericMappingCtx, stmts::TypedBlockStmt};
use rand::Rng;
use std::collections::HashMap;

pub type MonomorphID = u32;

#[derive(Debug, Clone)]
pub struct GenericTemplateRegistry {
    map: HashMap<SymbolID, TypedBlockStmt>,
}

impl GenericTemplateRegistry {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn register_template(&mut self, base: SymbolID, body: TypedBlockStmt) {
        self.map.insert(base, body);
    }

    pub fn get_template(&self, base: SymbolID) -> Option<&TypedBlockStmt> {
        self.map.get(&base)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonomorphKey(pub u32);

impl MonomorphKey {
    pub fn new_instance_key() -> Self {
        let mut rng = rand::rng();
        let key = rng.random::<u32>();
        MonomorphKey(key)
    }
}

#[derive(Debug, Clone)]
pub struct MonomorphFuncEntry {
    pub id: MonomorphID,
    pub base_symbol: SymbolID,
    pub mapping_ctx: GenericMappingCtx,
    pub analyzed: bool,
}

#[derive(Debug, Clone)]
pub enum MonomorphEntry {
    Func(MonomorphFuncEntry),
}

impl MonomorphEntry {
    pub fn id(&self) -> MonomorphID {
        match self {
            MonomorphEntry::Func(e) => e.id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MonomorphRegistry {
    next_id: MonomorphID,
    map: HashMap<MonomorphKey, MonomorphEntry>,
    pub templates: GenericTemplateRegistry,
}

impl MonomorphRegistry {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            map: HashMap::new(),
            templates: GenericTemplateRegistry::new(),
        }
    }

    pub fn register_template(&mut self, base: SymbolID, body: TypedBlockStmt) {
        self.templates.register_template(base, body);
    }

    pub fn get_template(&self, base: SymbolID) -> Option<&TypedBlockStmt> {
        self.templates.get_template(base)
    }

    pub fn get(&self, key: &MonomorphKey) -> Option<&MonomorphEntry> {
        self.map.get(key)
    }

    pub fn register_func(
        &mut self,
        base_symbol: SymbolID,
        mapping_ctx: GenericMappingCtx,
    ) -> (MonomorphKey, MonomorphID) {
        let key = MonomorphKey::new_instance_key();

        let id = self.next_id;
        self.next_id += 1;

        let entry = MonomorphEntry::Func(MonomorphFuncEntry {
            id,
            base_symbol,
            mapping_ctx,
            analyzed: false,
        });

        self.map.insert(key.clone(), entry);

        (key, id)
    }

    pub fn mark_analyzed(&mut self, key: &MonomorphKey) {
        if let Some(MonomorphEntry::Func(e)) = self.map.get_mut(key) {
            e.analyzed = true;
        }
    }

    pub fn needs_analysis(&self, key: &MonomorphKey) -> bool {
        match self.map.get(key) {
            Some(MonomorphEntry::Func(e)) => !e.analyzed,
            _ => false,
        }
    }
}

#[macro_export]
macro_rules! with_monomorph_registry {
    ($self:ident, $ctx:ident, $body:block) => {{
        let mut $ctx = $self.monomorph_registry.lock().unwrap();
        $body
    }};
}
