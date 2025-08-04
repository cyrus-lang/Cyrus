use crate::declsign::{EnumSig, FuncSig, GlobalVarSig, StructSig, TypedefSig};
use ast::{BlockStatement, Variable};
use rand::Rng;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use typed_ast::ModuleID;

// Symbol Table (Per Module)

pub type SymbolID = u32;

#[derive(Debug)]
pub struct SymbolTable {
    pub entries: HashMap<SymbolID, SymbolEntry>,
    pub names: HashMap<String, SymbolID>,
}

#[derive(Debug, Clone)]
pub enum SymbolEntry {
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
    pub enum_sig: EnumSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedStruct {
    pub module_id: ModuleID,
    pub struct_sig: StructSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedGlobalVar {
    pub module_id: ModuleID,
    pub global_var_sig: GlobalVarSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedFunction {
    pub module_id: ModuleID,
    pub func_sig: FuncSig,
    pub body: Option<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct ResolvedTypedef {
    pub module_id: ModuleID,
    pub typedef_sig: TypedefSig,
}

// Local Scope

pub type LocalScopeRef = Rc<RefCell<LocalScope>>;

pub struct LocalScope {
    pub symbols: HashMap<String, LocalSymbol>,
    pub parent: Option<Box<LocalScope>>,
}

#[derive(Debug, Clone)]
pub enum LocalSymbol {
    Variable(Variable),
    Struct(ResolvedStruct),
    Enum(ResolvedEnum),
    TypeAlias(ResolvedTypedef),
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
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            names: HashMap::new(),
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
