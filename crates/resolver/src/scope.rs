use crate::declsign::FuncSig;
use std::collections::HashMap;
use typed_ast::{ModuleID, TypedBlockStatement};

#[derive(Debug)]
pub struct ModuleSymbolTable {
    pub symbols: HashMap<String, SymbolEntry>,
    pub parent: Option<Box<ModuleSymbolTable>>,
}

#[derive(Debug)]
pub enum SymbolEntry {
    Func(ResolvedFunction),
    // Variable(ResolvedVariable),
    // Struct(ResolvedStruct),
    // Enum(ResolvedEnum),
    // Module(ModuleId),
}

#[derive(Debug)]
pub struct ResolvedFunction {
    pub module_id: ModuleID,
    pub func_sig: FuncSig,
    pub body: Option<TypedBlockStatement>,
}
