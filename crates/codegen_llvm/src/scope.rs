use crate::AnyType;
use inkwell::values::PointerValue;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type ScopeRef<'a> = Rc<RefCell<Scope<'a>>>;

#[derive(Debug, Clone)]
pub struct ScopeRecord<'a> {
    pub ptr: PointerValue<'a>,
    pub ty: AnyType<'a>,
}

pub struct Scope<'a> {
    pub records: HashMap<String, Rc<RefCell<ScopeRecord<'a>>>>,
    pub parent: Option<Box<Scope<'a>>>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Self {
            records: HashMap::new(),
            parent: None,
        }
    }

    pub fn get(&self, key: String) -> Option<Rc<RefCell<ScopeRecord<'a>>>> {
        match self.records.get(&key) {
            Some(value) => Some(Rc::clone(value)),
            None => {
                if let Some(parent) = &self.parent {
                    return parent.get(key);
                } else {
                    None
                }
            }
        }
    }

    pub fn insert(&mut self, key: String, record: ScopeRecord<'a>) {
        self.records.insert(key, Rc::new(RefCell::new(record)));
    }
}

// TODO 
// Write unit test for this section
