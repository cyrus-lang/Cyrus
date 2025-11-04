use cyrusc_cir::types::CIRTy;
use inkwell::{
    types::{ArrayType, StructType},
    values::{BasicValueEnum, FunctionValue, GlobalValue, PointerValue},
};
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
    RValue(BasicValueEnum<'a>, CIRTy),
    Enum(StructType<'a>, ArrayType<'a>),
    Struct(StructType<'a>),
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

    /// Removes an IR value from the registry.
    pub fn remove(&mut self, irv_id: IRValueID) -> Option<LocalIRValue<'a>> {
        self.map.remove(&irv_id)
    }

    /// Checks whether the registry contains the given symbol ID.
    pub fn contains(&self, irv_id: IRValueID) -> bool {
        self.map.contains_key(&irv_id)
    }

    /// Clears all registered IR values.
    pub fn clear(&mut self) {
        self.map.clear();
    }
}

impl<'a> LocalIRValue<'a> {
    pub fn as_func(&self) -> Option<&FunctionValue<'a>> {
        match self {
            LocalIRValue::Func(func, _) => Some(func),
            _ => None,
        }
    }

    pub fn as_global(&self) -> Option<&GlobalValue<'a>> {
        match self {
            LocalIRValue::Global(global, _) => Some(global),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<&StructType<'a>> {
        match self {
            LocalIRValue::Struct(st) => Some(st),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<(&StructType<'a>, &ArrayType<'a>)> {
        match self {
            LocalIRValue::Enum(st, arr) => Some((st, arr)),
            _ => None,
        }
    }

    pub fn as_lvalue(&self) -> Option<&PointerValue<'a>> {
        match self {
            LocalIRValue::LValue(ptr, _) => Some(ptr),
            _ => None,
        }
    }

    pub fn as_rvalue(&self) -> Option<&BasicValueEnum<'a>> {
        match self {
            LocalIRValue::RValue(val, _) => Some(val),
            _ => None,
        }
    }
}
