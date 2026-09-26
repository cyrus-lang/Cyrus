// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_internal::cir::{cir::IRValueID, types::CIRType};
use inkwell::values::{BasicValueEnum, FunctionValue, GlobalValue, PointerValue};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::builder::builder::CodeGenIRBuilder;

/// Shared reference to the registry.
pub type LocalIRValueRegistryRef<'a> = Rc<RefCell<LocalIRValueRegistry<'a>>>;

/// Registry that maps symbol IDs to LLVM IR values.
pub struct LocalIRValueRegistry<'a> {
    map: HashMap<IRValueID, LocalIRValue<'a>>,
}

/// Represents a local LLVM IR value.
#[derive(Debug, Clone)]
pub enum LocalIRValue<'a> {
    Func(FunctionValue<'a>, CIRType),
    Global(GlobalValue<'a>, CIRType),
    LValue(PointerValue<'a>, CIRType),
    RValue(BasicValueEnum<'a>, CIRType),
}

impl<'a> LocalIRValueRegistry<'a> {
    /// Creates a new, empty registry.
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    /// Inserts a new IR value associated with a id.
    fn insert(&mut self, irv_id: IRValueID, local_value: LocalIRValue<'a>) {
        self.map.insert(irv_id, local_value);
    }

    /// Retrieves a cloned IR value, if present.
    fn get(&self, irv_id: IRValueID) -> Option<LocalIRValue<'a>> {
        self.map.get(&irv_id).cloned()
    }
}

impl<'a> LocalIRValue<'a> {
    pub fn as_global(&self) -> Option<&GlobalValue<'a>> {
        match self {
            LocalIRValue::Global(global, _) => Some(global),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<&FunctionValue<'a>> {
        match self {
            LocalIRValue::Func(func_value, _) => Some(func_value),
            _ => None,
        }
    }
}

impl<'ll> CodeGenIRBuilder<'ll> {
    #[inline]
    pub(crate) fn insert_local_ir_value(&self, irv_id: IRValueID, value: LocalIRValue<'ll>) {
        let mut irreg = self.irreg.borrow_mut();
        irreg.insert(irv_id, value);
    }

    #[inline]
    pub(crate) fn lookup_local_ir_value(&self, irv_id: IRValueID) -> Option<LocalIRValue<'ll>> {
        let irreg = self.irreg.borrow();
        irreg.get(irv_id).clone()
    }
}
