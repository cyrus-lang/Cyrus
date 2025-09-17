use crate::declsign::{EnumSig, FuncSig, GlobalVarSig, InterfaceSig, StructSig, TypedefSig, UnionSig};
use ast::{AccessSpecifier, source_loc::SourceLoc};
use rand::Rng;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use typed_ast::{ModuleID, ScopeID, SymbolID, TypedBlockStatement, TypedFuncParamKind, TypedVariable};

// Symbol Table (Per Module)

#[derive(Debug)]
pub struct SymbolTable {
    pub entries: HashMap<SymbolID, SymbolEntry>,
    pub names: HashMap<String, SymbolID>,
    pub scopes: HashMap<ScopeID, LocalScopeRef>,
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
    Union(ResolvedUnion),
    Interface(ResolvedInterface),
}

#[derive(Debug, Clone)]
pub struct ResolvedUnion {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub union_sig: UnionSig,
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
    Union(ResolvedUnion),
}

#[derive(Debug, Clone)]
pub enum LocalOrGlobalSymbol {
    LocalSymbol(LocalSymbol),
    GlobalSymbol(SymbolEntry),
}

impl LocalOrGlobalSymbol {
    pub fn get_symbol_id(&self) -> SymbolID {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => local_symbol.get_symbol_id(),
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => symbol_entry.get_symbol_id(),
        }
    }

    pub fn as_struct(&self) -> Option<&ResolvedStruct> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                LocalSymbolKind::Struct(resolved_struct) => Some(resolved_struct),
                _ => None,
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Struct(resolved_struct) => Some(resolved_struct),
                _ => None,
            },
        }
    }

    pub fn as_enum(&self) -> Option<&ResolvedEnum> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                LocalSymbolKind::Enum(resolved_enum) => Some(resolved_enum),
                _ => None,
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Enum(resolved_enum) => Some(resolved_enum),
                _ => None,
            },
        }
    }

    pub fn as_union(&self) -> Option<&ResolvedUnion> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                LocalSymbolKind::Union(resolved_union) => Some(resolved_union),
                _ => None,
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Union(resolved_union) => Some(resolved_union),
                _ => None,
            },
        }
    }

    pub fn as_global_var(&self) -> Option<ResolvedGlobalVar> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(..) => None,
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => symbol_entry.as_global_var().cloned(),
        }
    }

    pub fn as_variable(&self) -> Option<ResolvedVariable> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => local_symbol.as_variable().cloned(),
            LocalOrGlobalSymbol::GlobalSymbol(..) => None,
        }
    }
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
            LocalSymbolKind::Union(resolved) => resolved.symbol_id,
        }
    }

    pub fn as_struct(&self) -> Option<&ResolvedStruct> {
        match &self.kind {
            LocalSymbolKind::Struct(resolved_struct) => Some(resolved_struct),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&ResolvedEnum> {
        match &self.kind {
            LocalSymbolKind::Enum(resolved_enum) => Some(resolved_enum),
            _ => None,
        }
    }

    pub fn as_typedef(&self) -> Option<&ResolvedTypedef> {
        match &self.kind {
            LocalSymbolKind::Typedef(resolved_typedef) => Some(resolved_typedef),
            _ => None,
        }
    }

    pub fn as_variable(&self) -> Option<&ResolvedVariable> {
        match &self.kind {
            LocalSymbolKind::Variable(resolved_variable) => Some(resolved_variable),
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

    pub fn resolve_with_symbol_id(&self, symbol_id: SymbolID) -> Option<&LocalSymbol> {
        match self
            .symbols
            .iter()
            .find(|(_, local_symbol)| local_symbol.get_symbol_id() == symbol_id)
        {
            Some((_, local_symbol)) => Some(local_symbol),
            None => None,
        }
    }

    pub fn resolve_with_symbol_id_mut(&mut self, symbol_id: SymbolID) -> Option<&mut LocalSymbol> {
        match self
            .symbols
            .iter_mut()
            .find(|(_, local_symbol)| local_symbol.get_symbol_id() == symbol_id)
        {
            Some((_, local_symbol)) => Some(local_symbol),
            None => None,
        }
    }

    pub fn resolve_mut(&mut self, name: &str) -> Option<&mut LocalSymbol> {
        match self.symbols.iter_mut().find(|(_name, ..)| **_name == name) {
            Some((_, local_symbol)) => Some(local_symbol),
            None => None,
        }
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
        }
    }
}

impl SymbolEntry {
    pub fn new(kind: SymbolEntryKind) -> Self {
        Self { used: false, kind }
    }

    pub fn get_vis(&self) -> AccessSpecifier {
        match &self.kind {
            SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.vis.clone(),
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.vis.clone(),
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.vis.clone(),
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.vis.clone(),
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.vis.clone(),
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.vis.clone(),
            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.vis.clone(),
            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.vis.clone(),
        }
    }

    pub fn get_loc(&self) -> SourceLoc {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.loc.clone(),
            SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.loc.clone(),
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.loc.clone(),
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.loc.clone(),
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.loc.clone(),
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.loc.clone(),
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.loc.clone(),
            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.loc.clone(),
        }
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
            SymbolEntryKind::Union(resolved_union) => resolved_union.symbol_id,
        }
    }

    pub fn get_module_id(&self) -> ModuleID {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.module_id,
            SymbolEntryKind::Func(resolved_func) => resolved_func.module_id,
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.module_id,
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.module_id,
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.module_id,
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.module_id,
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.module_id,
            SymbolEntryKind::Union(resolved_union) => resolved_union.module_id,
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

impl ResolvedMethod {
    pub fn is_instance_method(&self) -> bool {
        match self.func_sig.params.list.first() {
            Some(typed_func_param_kind) => match typed_func_param_kind {
                TypedFuncParamKind::FuncParam(..) => false,
                TypedFuncParamKind::SelfModifier(..) => true,
            },
            None => false,
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
