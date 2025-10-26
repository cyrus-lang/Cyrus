use std::collections::HashMap;
use cyrusc_tast::{SymbolID, types::SemanticType};

pub type MonomorphID = u32;
pub type NormalizedTypeArgs = Vec<SemanticType>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonomorphKey {
    pub base_symbol: SymbolID,
    pub normalized_args: NormalizedTypeArgs, // canonical order
}

#[derive(Debug, Clone)]
pub struct MonomorphEntry {
    pub id: MonomorphID,
    pub instantiated_symbol: SymbolID,
}

#[derive(Debug, Clone)]
pub struct MonomorphRegistry {
    next_id: MonomorphID,
    pub map: HashMap<MonomorphKey, MonomorphEntry>, // lookup & storage
    pub ordered: Vec<MonomorphEntry>,               // preserve insertion order for codegen_llvm
}

impl MonomorphRegistry {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            map: HashMap::new(),
            ordered: Vec::new(),
        }
    }

    pub fn get_with_key(&mut self, key: MonomorphKey) -> Option<MonomorphID> {
        self.map.get(&key).map(|entry| entry.id)
    }

    pub fn register(&mut self, base_symbol: SymbolID, normalized_args: NormalizedTypeArgs) -> MonomorphID {
        let key = MonomorphKey {
            base_symbol,
            normalized_args,
        };

        if let Some(entry) = self.map.get(&key) {
            return entry.id;
        }

        let id = self.next_id;
        self.next_id += 1;

        let instantiated_symbol = base_symbol;

        let entry = MonomorphEntry {
            id,
            instantiated_symbol,
        };

        self.map.insert(key, entry.clone());
        self.ordered.push(entry.clone());

        id
    }

    pub fn entries(&self) -> &[MonomorphEntry] {
        &self.ordered
    }
}

impl MonomorphKey {
    pub fn new(base_symbol: SymbolID, normalized_args: NormalizedTypeArgs) -> Self {
        MonomorphKey {
            base_symbol,
            normalized_args, // canonicalized
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
