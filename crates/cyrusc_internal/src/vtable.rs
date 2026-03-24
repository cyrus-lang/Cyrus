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

use cyrusc_typed_ast::{SymbolID, VTableID, sigs::FuncSig, types::SemanticType};
use std::collections::HashMap;

pub type GlobalVarID = u32;

/// Uniquely identifies a vtable by the pair:
///   (concrete type, interface).
///
/// This key is created **during type checking**, after it has been
/// proven that `sema_type` implements `interface_id`.
///
/// IMPORTANT INVARIANT:
/// - Two identical keys must always map to the same vtable.
/// - A key must never be constructed during codegen.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VTableKey {
    /// The fully resolved concrete type (after generic substitution).
    ///
    /// This must be a *concrete* semantic type, never an interface,
    /// never `Self`, never an unresolved generic.
    pub sema_type: SemanticType,

    /// The symbol ID of the interface being implemented.
    pub interface_id: SymbolID,
}

/// Compile-time registry of all vtables required by the program.
///
/// This registry is populated **only during semantic analysis**
/// (typically while validating `impl Interface for Type` blocks).
///
/// Codegen and CIR lowering must treat this registry as immutable.
pub struct VTableRegistry {
    /// Maps (ConcreteType, Interface) to VTableID
    map: HashMap<VTableKey, VTableID>,

    /// Dense storage of all registered vtables.
    tables: Vec<VTableInfo>,
}

/// Compile-time description of a single vtable.
///
/// This structure contains *no runtime data*.
/// It exists solely to allow later phases to:
///   - emit the vtable as a global
///   - reference it by symbol
///   - compute stable slot indices
#[derive(Debug, Clone)]
pub struct VTableInfo {
    /// The concrete type implementing the interface.
    pub sema_type: SemanticType,

    /// The interface being implemented.
    pub interface_id: SymbolID,
    pub interface_name: String,

    /// Ordered list of method symbols.
    ///
    /// The index into this vector is the **vtable slot index**.
    /// This order must exactly match the interface method order.
    pub methods: Vec<FuncSig>,

    /// Global variable symbol representing the emitted vtable.
    ///
    /// Codegen will emit a single global definition for this symbol
    /// and all dynamic dispatch sites will reference it by address.
    pub global_var_id: SymbolID,
}

impl VTableRegistry {
    /// Creates an empty vtable registry.
    ///
    /// Typically constructed once per module or compilation unit.
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            tables: Vec::new(),
        }
    }

    /// Registers a vtable for a (concrete type, interface) pair.
    ///
    /// This function must be called **only during type checking**,
    /// after interface conformance has been fully validated.
    ///
    /// If the vtable was already registered, the existing `VTableID`
    /// is returned.
    ///
    /// # Panics
    ///
    /// Panics if `methods` is empty or if a duplicate registration
    /// attempts to change the method layout.
    pub fn register(
        &mut self,
        sema_type: SemanticType,
        interface_id: SymbolID,
        interface_name: String,
        methods: Vec<FuncSig>,
    ) -> VTableID {
        assert!(!methods.is_empty(), "vtable must contain at least one method");

        let key = VTableKey {
            sema_type: sema_type.clone(),
            interface_id,
        };

        if let Some(&existing_id) = self.map.get(&key) {
            let existing = &self.tables[existing_id.value() as usize];

            // Hard invariant: layout must be identical
            assert_eq!(
                existing.methods, methods,
                "attempted to re-register vtable with different method layout"
            );

            return existing_id;
        }

        let vtable_id = VTableID::new(self.tables.len() as u32);
        let global_var_id = generate_global_var_id();

        self.tables.push(VTableInfo {
            sema_type,
            interface_id,
            interface_name,
            methods,
            global_var_id,
        });

        self.map.insert(key, vtable_id);
        vtable_id
    }

    /// Retrieves the VTableID for a concrete type implementing an interface.
    ///
    /// This is intended for use during CIR lowering and codegen.
    ///
    /// # Panics
    ///
    /// Panics if the vtable was not registered during type checking.
    pub fn get(&self, sema_type: &SemanticType, interface_id: SymbolID) -> VTableID {
        let key = VTableKey {
            sema_type: sema_type.clone(),
            interface_id,
        };

        *self
            .map
            .get(&key)
            .expect("missing vtable: interface implementation was not registered during type checking")
    }

    /// Returns metadata for a previously registered vtable.
    pub fn info(&self, vtable_id: VTableID) -> &VTableInfo {
        &self.tables[vtable_id.value() as usize]
    }

    /// Returns an iterator over all registered vtables.
    ///
    /// This is typically used during global emission to emit
    /// all vtable globals.
    pub fn iter(&self) -> impl Iterator<Item = &VTableInfo> {
        self.tables.iter()
    }
}

unsafe impl Sync for VTableRegistry {}
unsafe impl Send for VTableRegistry {}

fn generate_global_var_id() -> SymbolID {
    SymbolID::new()
}
