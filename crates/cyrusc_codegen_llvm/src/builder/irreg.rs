/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

use cyrusc_internal::cir::{cir::IRValueID, types::CIRType};
use inkwell::values::{FunctionValue, GlobalValue, PointerValue};
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
