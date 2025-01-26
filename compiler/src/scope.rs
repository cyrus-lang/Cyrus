use gccjit_sys::gcc_jit_lvalue;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub struct Scope {
    pub table: HashMap<String, Rc<RefCell<*mut gcc_jit_lvalue>>>,
    pub parent: Option<Box<ScopeRef>>,
}

pub type ScopeRef = Rc<RefCell<Scope>>;

impl Scope {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
            parent: None,
        }
    }

    pub fn get(&self, key: String) -> Option<Rc<RefCell<*mut gcc_jit_lvalue>>> {
        match self.table.get(&key) {
            Some(value) => Some(Rc::clone(&value)),
            None => {
                if let Some(parent) = &self.parent {
                    return parent.borrow().get(key);
                } else {
                    None
                }
            }
        }
    }

    pub fn insert(&mut self, key: String, value: *mut gcc_jit_lvalue) {
        self.table.insert(key, Rc::new(RefCell::new(value)));
    }

    pub fn clone_immutable(&self) -> Self {
        Scope {
            table: self.table.clone(),
            parent: self.parent.clone()
        }
    }
}
