use std::collections::HashMap;

use typed_ast::{SymbolID, types::ConcreteType};

pub type MonomorphID = u32;
pub type NormalizedTypeArgs = Vec<ConcreteType>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MonomorphKey {
    base_symbol: SymbolID,
    normalized_args: NormalizedTypeArgs, // canonical order
}

#[derive(Debug, Clone)]
struct MonomorphEntry {
    id: MonomorphID,
    instantiated_symbol: SymbolID,
    concrete_type: ConcreteType,
}

#[derive(Debug, Clone)]
pub struct MonomorphRegistry {
    next_id: MonomorphID,
    map: HashMap<MonomorphKey, MonomorphEntry>, // lookup & storage
    ordered: Vec<MonomorphEntry>,               // preserve insertion order for codegen
}

impl MonomorphRegistry {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            map: HashMap::new(),
            ordered: Vec::new(),
        }
    }

    pub fn register(
        &mut self,
        base_symbol: SymbolID,
        normalized_args: NormalizedTypeArgs,
        concrete_type: ConcreteType,
    ) -> MonomorphID {
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
            concrete_type,
        };

        self.map.insert(key, entry.clone());
        self.ordered.push(entry.clone());

        id
    }

    pub fn entries(&self) -> &[MonomorphEntry] {
        &self.ordered
    }
}
