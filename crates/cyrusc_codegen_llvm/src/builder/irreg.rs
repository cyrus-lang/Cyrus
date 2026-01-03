// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use cyrusc_cir::types::CIRTy;
use inkwell::values::{FunctionValue, GlobalValue, PointerValue};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

/// Represents a unique symbol ID in the current IR scope.
pub type IRValueID = u32;

/// Shared reference to the registry.
pub type LocalIRValueRegistryRef<'a> = Rc<RefCell<LocalIRValueRegistry<'a>>>;

/// Registry that maps symbol IDs to LLVM IR values.
pub struct LocalIRValueRegistry<'a> {
    map: HashMap<IRValueID, LocalIRValue<'a>>,
}

/// Represents a local LLVM IR value.
#[derive(Debug, Clone)]
pub enum LocalIRValue<'a> {
    Func(FunctionValue<'a>, CIRTy),
    Global(GlobalValue<'a>, CIRTy),
    LValue(PointerValue<'a>, CIRTy),
}

impl<'a> LocalIRValueRegistry<'a> {
    /// Creates a new, empty registry.
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    /// Inserts a new IR value associated with a id.
    pub fn insert(&mut self, irv_id: IRValueID, local_value: LocalIRValue<'a>) {
        self.map.insert(irv_id, local_value);
    }

    /// Retrieves a cloned IR value, if present.
    pub fn get(&self, irv_id: IRValueID) -> Option<LocalIRValue<'a>> {
        self.map.get(&irv_id).cloned()
    }
}

impl<'a> LocalIRValue<'a> {
    #[allow(unused)]
    pub fn as_func(&self) -> Option<&FunctionValue<'a>> {
        match self {
            LocalIRValue::Func(func, _) => Some(func),
            _ => None,
        }
    }

    #[allow(unused)]
    pub fn as_global(&self) -> Option<&GlobalValue<'a>> {
        match self {
            LocalIRValue::Global(global, _) => Some(global),
            _ => None,
        }
    }

    #[allow(unused)]
    pub fn as_lvalue(&self) -> Option<&PointerValue<'a>> {
        match self {
            LocalIRValue::LValue(ptr, _) => Some(ptr),
            _ => None,
        }
    }
}
