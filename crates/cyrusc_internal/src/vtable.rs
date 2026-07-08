// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::cir::cir::IRValueID;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    VTableID,
    decls::{InterfaceDeclID, MethodDecls, MonomorphID},
    stmts::TypedTypeArgs,
    types::SemaType,
};
use fx_hash::{FxHashMap, FxHashMapExt};
use std::sync::{Arc, RwLock};

/// Key identifying a vtable: (concrete type, instantiated interface).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VTableKey {
    pub ty: SemaType,
    pub interface: (InterfaceDeclID, TypedTypeArgs),
}

#[derive(Debug, Clone)]
pub struct VTableInfo {
    pub concrete_type: SemaType,
    pub interface_decl_id: InterfaceDeclID,
    pub interface_type_args: TypedTypeArgs,
    pub is_interface_generic: bool,
    pub method_decls: MethodDecls,
    pub vtable_id: VTableID,

    /// Populated during CIR lowering.
    /// Codegen will rely on this for function declarations.
    pub cir_method_decls: Option<Vec<Option<IRValueID>>>,

    pub monomorphized_methods: Vec<Option<MonomorphID>>,

    pub vtable_irv_id: Option<IRValueID>,

    pub abi_name: Option<String>,

    pub loc: Loc,
}

/// Registry of all vtables created during semantic analysis.
/// CIR lowering + codegen read this registry but never mutate it.
///
/// Internally uses interior mutability to allow safe shared access everywhere.
#[derive(Debug, Clone)]
pub struct VTableRegistry {
    inner: Arc<RwLock<VTableRegistryInner>>,
}

#[derive(Debug)]
struct VTableRegistryInner {
    map: FxHashMap<VTableKey, VTableID>,
    tables: Vec<VTableInfo>,
}

impl VTableRegistry {
    /// Create an empty registry.
    pub fn new() -> Self {
        Self {
            inner: Arc::new(RwLock::new(VTableRegistryInner {
                map: FxHashMap::new(),
                tables: Vec::new(),
            })),
        }
    }

    /// Get existing vtable ID or create a new one if not present.
    /// Validates method layout consistency for existing vtables.
    pub fn get_or_create_vtable(
        &self,
        sema_type: SemaType,
        interface_decl_id: InterfaceDeclID,
        interface_type_args: TypedTypeArgs,
        methods: MethodDecls,
        is_interface_generic: bool,
        loc: Loc,
    ) -> VTableID {
        assert!(!methods.0.is_empty(), "vtable must contain at least one method");

        let key = VTableKey {
            ty: sema_type.clone(),
            interface: (interface_decl_id, interface_type_args.clone()),
        };

        let mut inner = self.inner.write().unwrap();

        // Check for existing vtable
        if let Some(&existing_id) = inner.map.get(&key) {
            let existing = &inner.tables[existing_id.0 as usize];

            // Validate method layout consistency
            assert_eq!(
                existing.method_decls.0, methods.0,
                "duplicate vtable registration with mismatched method layout"
            );

            // Validate interface generic flag consistency
            assert_eq!(
                existing.is_interface_generic, is_interface_generic,
                "duplicate vtable registration with mismatched interface generic flag"
            );

            return existing_id;
        }

        // Create new vtable
        let vtable_id = VTableID(inner.tables.len() as u32);
        let monomorphized_methods = (0..methods.len()).map(|_| None).collect();

        inner.tables.push(VTableInfo {
            concrete_type: sema_type,
            interface_decl_id,
            interface_type_args,
            method_decls: methods,
            vtable_id,
            cir_method_decls: None,
            vtable_irv_id: None,
            monomorphized_methods,
            is_interface_generic,
            abi_name: None,
            loc,
        });

        inner.map.insert(key, vtable_id);
        vtable_id
    }

    /// Lookup an existing vtable. Used by CIR lowering and codegen.
    /// Panics if no vtable was registered during semantic analysis.
    pub fn get(&self, sema_type: &SemaType, interface: (InterfaceDeclID, TypedTypeArgs)) -> VTableID {
        let key = VTableKey {
            ty: sema_type.clone(),
            interface,
        };

        let inner = self.inner.read().unwrap();

        *inner
            .map
            .get(&key)
            .expect("missing vtable: semantic analysis did not register expected implementation")
    }

    /// Get metadata for a vtable ID.
    pub fn info(&self, vtable_id: VTableID) -> VTableInfo {
        let inner = self.inner.read().unwrap();
        inner.tables[vtable_id.0 as usize].clone()
    }

    /// Iterate over all vtables (used by global codegen).
    pub fn iter(&self) -> Vec<VTableInfo> {
        let inner = self.inner.read().unwrap();
        inner.tables.clone()
    }

    /// Mutably access a specific VTableInfo using a closure.
    /// Only CIR lowering should call this.
    pub fn with_vtable_info_mut<F, R>(&self, vtable_id: VTableID, f: F) -> R
    where
        F: FnOnce(&mut VTableInfo) -> R,
    {
        let mut inner = self.inner.write().unwrap();
        let info = &mut inner.tables[vtable_id.0 as usize];
        f(info)
    }
}

unsafe impl Sync for VTableRegistry {}
unsafe impl Send for VTableRegistry {}
