use crate::declsign::{EnumSig, FuncSig, GlobalVarSig, StructSig, TypedefSig};
use ast::token::Location;
use rand::Rng;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use typed_ast::{ModuleID, SymbolID, TypedBlockStatement, TypedVariable};

// Symbol Table (Per Module)

#[derive(Debug)]
pub struct SymbolTable {
    pub entries: HashMap<SymbolID, SymbolEntry>,
    pub names: HashMap<String, SymbolID>,
    pub locs: HashMap<SymbolID, (String, Location, usize)>,
}

#[derive(Debug, Clone)]
pub enum SymbolEntry {
    Method(ResolvedMethod),
    Func(ResolvedFunction),
    Typedef(ResolvedTypedef),
    GlobalVar(ResolvedGlobalVar),
    Struct(ResolvedStruct),
    Enum(ResolvedEnum),
    // Module(ModuleId),
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
pub enum LocalSymbol {
    Variable(ResolvedVariable),
    Struct(ResolvedStruct),
    Enum(ResolvedEnum),
    Typedef(ResolvedTypedef),
}

#[derive(Debug, Clone)]
pub enum LocalOrGlobalSymbol {
    LocalSymbol(LocalSymbol),
    GlobalSymbol(SymbolEntry),
}

impl LocalSymbol {
    pub fn get_symbol_id(&self) -> SymbolID {
        match self {
            LocalSymbol::Variable(resolved) => resolved.symbol_id,
            LocalSymbol::Struct(resolved) => resolved.symbol_id,
            LocalSymbol::Enum(resolved) => resolved.symbol_id,
            LocalSymbol::Typedef(resolved) => resolved.symbol_id,
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
            locs: HashMap::new(),
        }
    }
}

impl SymbolEntry {
    pub fn as_struct(&self) -> Option<&ResolvedStruct> {
        match self {
            SymbolEntry::Struct(struct_) => Some(struct_),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&ResolvedEnum> {
        match self {
            SymbolEntry::Enum(enum_) => Some(enum_),
            _ => None,
        }
    }

    pub fn as_typedef(&self) -> Option<&ResolvedTypedef> {
        match self {
            SymbolEntry::Typedef(typedef) => Some(typedef),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<&ResolvedFunction> {
        match self {
            SymbolEntry::Func(func) => Some(func),
            _ => None,
        }
    }

    pub fn as_global_var(&self) -> Option<&ResolvedGlobalVar> {
        match self {
            SymbolEntry::GlobalVar(global_var) => Some(global_var),
            _ => None,
        }
    }
}

pub fn generate_symbol_id() -> SymbolID {
    let mut rng = rand::rng();
    rng.random::<u32>()
}
