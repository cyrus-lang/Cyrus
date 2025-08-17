use crate::declsign::{EnumSig, FuncSig, GlobalVarSig, InterfaceSig, StructSig, TypedefSig};
use ast::token::Location;
use rand::Rng;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use typed_ast::{ModuleID, ScopeID, SymbolID, TypedBlockStatement, TypedVariable};

// Symbol Table (Per Module)

#[derive(Debug)]
pub struct SymbolTable {
    pub entries: HashMap<SymbolID, SymbolEntry>,
    pub names: HashMap<String, SymbolID>,
    pub scopes: HashMap<ScopeID, LocalScopeRef>,
    pub locs: HashMap<SymbolID, (String, Location, usize)>,
}

#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub used: bool,
    pub kind: SymbolEntryKind,
}

#[derive(Debug, Clone)]
pub enum SymbolEntryKind {
    Method(ResolvedMethod),
    Func(ResolvedFunction),
    Typedef(ResolvedTypedef),
    GlobalVar(ResolvedGlobalVar),
    Struct(ResolvedStruct),
    Enum(ResolvedEnum),
    Interface(ResolvedInterface),
}

#[derive(Debug, Clone)]
pub struct ResolvedEnum {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub enum_sig: EnumSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedStruct {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub struct_sig: StructSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedGlobalVar {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub global_var_sig: GlobalVarSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedMethod {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub func_sig: FuncSig,
    pub func_body: Option<Box<TypedBlockStatement>>,
}

#[derive(Debug, Clone)]
pub struct ResolvedFunction {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub func_sig: FuncSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedTypedef {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub typedef_sig: TypedefSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedInterface {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub interface_sig: InterfaceSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedVariable {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub typed_variable: TypedVariable,
}

// Local Scope

pub type LocalScopeRef = Rc<RefCell<LocalScope>>;

#[derive(Debug, Clone)]
pub struct LocalScope {
    pub symbols: HashMap<String, LocalSymbol>,
    pub parent: Option<Box<LocalScope>>,
}

#[derive(Debug, Clone)]
pub struct LocalSymbol {
    pub used: bool,
    pub kind: LocalSymbolKind,
}

#[derive(Debug, Clone)]
pub enum LocalSymbolKind {
    Variable(ResolvedVariable),
    Struct(ResolvedStruct),
    Enum(ResolvedEnum),
    Typedef(ResolvedTypedef),
    Interface(ResolvedInterface),
}

#[derive(Debug, Clone)]
pub enum LocalOrGlobalSymbol {
    LocalSymbol(LocalSymbol),
    GlobalSymbol(SymbolEntry),
}

impl LocalSymbol {
    pub fn new(kind: LocalSymbolKind) -> Self {
        Self { used: false, kind }
    }

    pub fn get_symbol_id(&self) -> SymbolID {
        match &self.kind {
            LocalSymbolKind::Variable(resolved) => resolved.symbol_id,
            LocalSymbolKind::Struct(resolved) => resolved.symbol_id,
            LocalSymbolKind::Enum(resolved) => resolved.symbol_id,
            LocalSymbolKind::Typedef(resolved) => resolved.symbol_id,
            LocalSymbolKind::Interface(resolved) => resolved.symbol_id,
        }
    }

    pub fn as_struct(&self) -> Option<&ResolvedStruct> {
        match &self.kind {
            LocalSymbolKind::Struct(resolved_struct) => Some(resolved_struct),
            _ => None,
        }
    }
}

impl LocalScope {
    pub fn new(parent: Option<Box<LocalScope>>) -> Self {
        Self {
            symbols: HashMap::new(),
            parent,
        }
    }

    pub fn insert(&mut self, name: String, symbol: LocalSymbol) -> Option<LocalSymbol> {
        self.symbols.insert(name, symbol)
    }

    pub fn resolve(&self, name: &str) -> Option<&LocalSymbol> {
        self.symbols.get(name).or_else(|| self.parent.as_ref()?.resolve(name))
    }

    pub fn deep_clone(scope_ref: &LocalScopeRef) -> LocalScopeRef {
        let borrowed = scope_ref.borrow();
        let copy = borrowed.clone();
        drop(borrowed);
        Rc::new(RefCell::new(copy))
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            names: HashMap::new(),
            scopes: HashMap::new(),
            locs: HashMap::new(),
        }
    }
}

impl SymbolEntry {
    pub fn new(kind: SymbolEntryKind) -> Self {
        Self { used: false, kind }
    }

    pub fn get_symbol_id(&self) -> SymbolID {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.symbol_id,
            SymbolEntryKind::Func(resolved_func) => resolved_func.symbol_id,
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.symbol_id,
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.symbol_id,
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.symbol_id,
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.symbol_id,
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.symbol_id,
        }
    }

    pub fn as_struct(&self) -> Option<&ResolvedStruct> {
        match &self.kind {
            SymbolEntryKind::Struct(struct_) => Some(struct_),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&ResolvedEnum> {
        match &self.kind {
            SymbolEntryKind::Enum(enum_) => Some(enum_),
            _ => None,
        }
    }

    pub fn as_typedef(&self) -> Option<&ResolvedTypedef> {
        match &self.kind {
            SymbolEntryKind::Typedef(typedef) => Some(typedef),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<&ResolvedFunction> {
        match &self.kind {
            SymbolEntryKind::Func(func) => Some(func),
            _ => None,
        }
    }

    pub fn as_global_var(&self) -> Option<&ResolvedGlobalVar> {
        match &self.kind {
            SymbolEntryKind::GlobalVar(global_var) => Some(global_var),
            _ => None,
        }
    }

    pub fn as_interface(&self) -> Option<&ResolvedInterface> {
        match &self.kind {
            SymbolEntryKind::Interface(interface) => Some(interface),
            _ => None,
        }
    }

    pub fn as_method(&self) -> Option<&ResolvedMethod> {
        match &self.kind {
            SymbolEntryKind::Method(method) => Some(method),
            _ => None,
        }
    }
}

pub fn generate_symbol_id() -> SymbolID {
    let mut rng = rand::rng();
    rng.random::<u32>()
}

pub fn generate_scope_id() -> ScopeID {
    let mut rng = rand::rng();
    rng.random::<u32>()
}
