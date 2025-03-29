use inkwell::llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type ScopeRef = Rc<RefCell<Scope>>;

#[derive(Debug, Clone)]
pub struct ScopeRecord {
    // FIXME
    // Consider to change this to use safe PointerValue<'_>
    pub ptr: LLVMValueRef,
    pub ty: LLVMTypeRef,
}

pub struct Scope {
    pub records: HashMap<String, Rc<RefCell<ScopeRecord>>>,
    pub parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            records: HashMap::new(),
            parent: None,
        }
    }

    pub fn get(&self, key: String) -> Option<Rc<RefCell<ScopeRecord>>> {
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

    pub fn insert(&mut self, key: String, record: ScopeRecord) {
        self.records.insert(key, Rc::new(RefCell::new(record)));
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_scope_new() {
//         let scope: Scope<String> = Scope::new();
//         assert!(scope.records.is_empty());
//         assert!(scope.parent.is_none());
//     }

//     #[test]
//     fn test_scope_insert_and_get() {
//         let mut scope: Scope<String> = Scope::new();
//         let record = ScopeRecord {
//             ptr: "test_value".to_string(),
//         };
//         scope.insert("test_key".to_string(), record.clone());

//         let retrieved_record = scope.get("test_key".to_string()).unwrap();
//         assert_eq!(retrieved_record.borrow().ptr, "test_value".to_string());
//     }

//     #[test]
//     fn test_scope_get_nonexistent() {
//         let scope: Scope<String> = Scope::new();
//         assert!(scope.get("nonexistent_key".to_string()).is_none());
//     }

//     #[test]
//     fn test_scope_parent_get() {
//         let mut parent_scope: Scope<String> = Scope::new();
//         let record = ScopeRecord {
//             ptr: "parent_value".to_string(),
//         };
//         parent_scope.insert("parent_key".to_string(), record.clone());

//         let mut child_scope: Scope<String> = Scope::new();
//         child_scope.parent = Some(Box::new(parent_scope));

//         let retrieved_record = child_scope.get("parent_key".to_string()).unwrap();
//         assert_eq!(retrieved_record.borrow().ptr, "parent_value".to_string());

//         assert!(child_scope.get("child_key".to_string()).is_none());
//     }

//     #[test]
//     fn test_scope_parent_get_shadowing() {
//         let mut parent_scope: Scope<String> = Scope::new();
//         let record_parent = ScopeRecord {
//             ptr: "parent_value".to_string(),
//         };
//         parent_scope.insert("shared_key".to_string(), record_parent.clone());

//         let mut child_scope: Scope<String> = Scope::new();
//         child_scope.parent = Some(Box::new(parent_scope));

//         let record_child = ScopeRecord {
//             ptr: "child_value".to_string(),
//         };
//         child_scope.insert("shared_key".to_string(), record_child.clone());

//         let retrieved_child_record = child_scope.get("shared_key".to_string()).unwrap();
//         assert_eq!(retrieved_child_record.borrow().ptr, "child_value".to_string());
//     }
// }
