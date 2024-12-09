use crate::object::Object;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type Env = Rc<RefCell<Environment>>;

#[derive(Debug, Clone, Default)]
pub struct Environment {
    store: HashMap<String, Rc<Object>>,
    outer: Option<Env>,
}

impl Environment {
    pub fn new_enclosed_environment(outer: &Env) -> Self {
        Environment {
            store: Default::default(),
            outer: Some(outer.clone()),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        match self.store.get(name) {
            Some(obj) => Some(Rc::clone(obj)),
            None => { 
                // trying to get the object of outer environment
                if let Some(outer) = &self.outer {
                    return outer.borrow().get(name);
                } 

                return None;
            }
        }
    }

    pub fn set(&mut self, name: String, val: Rc<Object>) {
        self.store.insert(name, val);
    }
}
