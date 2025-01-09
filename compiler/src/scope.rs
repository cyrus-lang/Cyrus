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
            parent: self.parent.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, ptr::null_mut, rc::Rc};

    use crate::scope::ScopeRef;

    use super::Scope;

    #[test]
    fn test_scope() {
        let func_scope_ref: ScopeRef = Rc::new(RefCell::new(Scope::new()));
        let child_scope_ref: ScopeRef = Rc::new(RefCell::new(Scope::new()));

        let mut func_scope = func_scope_ref.borrow_mut();
        let mut child_scope = child_scope_ref.borrow_mut();

        func_scope.insert(String::from("my_var"), null_mut());
        child_scope.parent = Some(Box::new(Rc::new(RefCell::new(func_scope.to_owned()))));


        drop(func_scope);

        let my_var = child_scope.get(String::from("my_var"));
        dbg!(my_var);
    }
}
