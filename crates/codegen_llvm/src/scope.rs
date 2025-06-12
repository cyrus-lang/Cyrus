use crate::InternalType;
use inkwell::values::PointerValue;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type ScopeRef<'a> = Rc<RefCell<Scope<'a>>>;

#[derive(Debug, Clone)]
pub struct ScopeRecord<'a> {
    pub ptr: PointerValue<'a>,
    pub ty: InternalType<'a>,
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

    pub fn deep_clone_detached(&self) -> Scope<'a> {
        let mut new_records = HashMap::new();

        for (key, val) in &self.records {
            let record = val.borrow();
            let cloned_record = ScopeRecord {
                ptr: record.ptr,       // `PointerValue` is `Copy`, so no issue
                ty: record.ty.clone(), // assuming `InternalType` is `Clone`
            };
            new_records.insert(key.clone(), Rc::new(RefCell::new(cloned_record)));
        }

        let cloned_parent = self.parent.as_ref().map(|p| Box::new(p.deep_clone_detached()));

        Scope {
            records: new_records,
            parent: cloned_parent,
        }
    }
}
