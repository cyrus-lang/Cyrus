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
use crate::sigs::{EnumSig, FuncSig, GlobalVarSig, InterfaceSig, StructSig, TypedefSig, UnionSig};
use cyrusc_abi::abi_ast_defs::Visibility;
use cyrusc_diagcentral::source_loc::SourceLoc;
use cyrusc_tast::{
    LabelID, ModuleID, ScopeID, SymbolID,
    stmts::{TypedBlockStmt, TypedGenericParamsList, TypedVarStmt},
};
use rand::Rng;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

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
    ProxiedSymbol(ModuleID, SymbolID),
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
    pub func_body: Option<Box<TypedBlockStmt>>,
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
    pub typed_variable: TypedVarStmt,
}

// Local Scope

pub type LocalScopeRef = Rc<RefCell<LocalScope>>;

#[derive(Debug, Clone)]
pub struct LocalScope {
    pub labels: HashMap<String, LabelID>,
    pub symbols: HashMap<String, LocalSymbol>,
    pub parent: Option<LocalScopeRef>,
}

#[derive(Debug, Clone)]
pub struct LocalSymbol {
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
    pub fn symbol_name(&self) -> Option<String> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                LocalSymbolKind::Variable(resolved_variable) => Some(resolved_variable.typed_variable.name.clone()),
                LocalSymbolKind::Struct(resolved_struct) => Some(resolved_struct.struct_sig.name.clone()),
                LocalSymbolKind::Enum(resolved_enum) => Some(resolved_enum.enum_sig.name.clone()),
                LocalSymbolKind::Typedef(resolved_typedef) => Some(resolved_typedef.typedef_sig.name.clone()),
                LocalSymbolKind::Interface(resolved_interface) => Some(resolved_interface.interface_sig.name.clone()),
                LocalSymbolKind::Union(resolved_union) => Some(resolved_union.union_sig.name.clone()),
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Method(resolved_method) => Some(resolved_method.func_sig.name.clone()),
                SymbolEntryKind::Func(resolved_function) => Some(resolved_function.func_sig.name.clone()),
                SymbolEntryKind::Typedef(resolved_typedef) => Some(resolved_typedef.typedef_sig.name.clone()),
                SymbolEntryKind::GlobalVar(resolved_global_var) => {
                    Some(resolved_global_var.global_var_sig.name.clone())
                }
                SymbolEntryKind::Struct(resolved_struct) => Some(resolved_struct.struct_sig.name.clone()),
                SymbolEntryKind::Enum(resolved_enum) => Some(resolved_enum.enum_sig.name.clone()),
                SymbolEntryKind::Union(resolved_union) => Some(resolved_union.union_sig.name.clone()),
                SymbolEntryKind::Interface(resolved_interface) => Some(resolved_interface.interface_sig.name.clone()),
                SymbolEntryKind::ProxiedSymbol(_, _) => None,
            },
        }
    }

    pub fn symbol_methods(&self) -> Option<HashMap<String, SymbolID>> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                LocalSymbolKind::Variable(_) => None,
                LocalSymbolKind::Typedef(_) => None,
                LocalSymbolKind::Interface(_) => None,
                LocalSymbolKind::Struct(resolved_struct) => Some(resolved_struct.struct_sig.methods.clone()),
                LocalSymbolKind::Enum(resolved_enum) => Some(resolved_enum.enum_sig.methods.clone()),
                LocalSymbolKind::Union(resolved_union) => Some(resolved_union.union_sig.methods.clone()),
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Method(_) => None,
                SymbolEntryKind::Func(_) => None,
                SymbolEntryKind::Typedef(_) => None,
                SymbolEntryKind::GlobalVar(_) => None,
                SymbolEntryKind::Interface(_) => None,
                SymbolEntryKind::Struct(resolved_struct) => Some(resolved_struct.struct_sig.methods.clone()),
                SymbolEntryKind::Enum(resolved_enum) => Some(resolved_enum.enum_sig.methods.clone()),
                SymbolEntryKind::Union(resolved_union) => Some(resolved_union.union_sig.methods.clone()),
                SymbolEntryKind::ProxiedSymbol(_, _) => unreachable!(),
            },
        }
    }

    pub fn method_symbol_id_by_name(&self, name: &str) -> Option<SymbolID> {
        let method_map = self.symbol_methods();
        method_map.and_then(|map| map.get(name).cloned())
    }

    pub fn symbol_method_generic_params(&self) -> Option<&TypedGenericParamsList> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(_) => None,
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.generic_params.as_ref(),
                _ => None,
            },
        }
    }

    pub fn symbol_generic_params(&self) -> Option<TypedGenericParamsList> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                LocalSymbolKind::Variable(_) => None,
                LocalSymbolKind::Struct(resolved_struct) => resolved_struct.struct_sig.generic_params.clone(),
                LocalSymbolKind::Enum(resolved_enum) => resolved_enum.enum_sig.generic_params.clone(),
                LocalSymbolKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.generic_params.clone(),
                LocalSymbolKind::Union(resolved_union) => resolved_union.union_sig.generic_params.clone(),
                LocalSymbolKind::Interface(resolved_interface) => {
                    resolved_interface.interface_sig.generic_params.clone()
                }
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.generic_params.clone(),
                SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.generic_params.clone(),
                SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.generic_params.clone(),
                SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.generic_params.clone(),
                SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.generic_params.clone(),
                SymbolEntryKind::Interface(resolved_interface) => {
                    resolved_interface.interface_sig.generic_params.clone()
                }
                SymbolEntryKind::GlobalVar(..) => None,
                SymbolEntryKind::Method(_) => None,
                SymbolEntryKind::ProxiedSymbol(..) => unreachable!(),
            },
        }
    }

    pub fn is_kind_of_variable(&self) -> bool {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match local_symbol.kind {
                LocalSymbolKind::Variable(..) => true,
                _ => false,
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match symbol_entry.kind {
                SymbolEntryKind::GlobalVar(..) => true,
                _ => false,
            },
        }
    }

    pub fn symbol_id(&self) -> SymbolID {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => local_symbol.symbol_id(),
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => symbol_entry.symbol_id(),
        }
    }

    pub fn as_func(&self) -> Option<&ResolvedFunction> {
        match self {
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Func(resolved_func) => Some(resolved_func),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_method(&self) -> Option<&ResolvedMethod> {
        match self {
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Method(resolved_method) => Some(resolved_method),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_method_mut(&mut self) -> Option<&mut ResolvedMethod> {
        match self {
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &mut symbol_entry.kind {
                SymbolEntryKind::Method(resolved_method) => Some(resolved_method),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_interface(&self) -> Option<&ResolvedInterface> {
        match self {
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Interface(resolved_interface) => Some(resolved_interface),
                _ => None,
            },
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                LocalSymbolKind::Interface(resolved_interface) => Some(resolved_interface),
                _ => None,
            },
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

    pub fn as_typedef(&self) -> Option<&ResolvedTypedef> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                LocalSymbolKind::Typedef(resolved_typedef) => Some(resolved_typedef),
                _ => None,
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Typedef(resolved_typedef) => Some(resolved_typedef),
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

    pub fn as_global_var(&self) -> Option<&ResolvedGlobalVar> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(..) => None,
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => symbol_entry.as_global_var(),
        }
    }

    pub fn as_variable(&self) -> Option<&ResolvedVariable> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => local_symbol.as_variable(),
            LocalOrGlobalSymbol::GlobalSymbol(..) => None,
        }
    }
}

impl LocalSymbol {
    pub fn new(kind: LocalSymbolKind) -> Self {
        Self { kind }
    }

    pub fn symbol_id(&self) -> SymbolID {
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

    pub fn as_variable_mut(&mut self) -> Option<&mut ResolvedVariable> {
        match &mut self.kind {
            LocalSymbolKind::Variable(resolved_variable) => Some(resolved_variable),
            _ => None,
        }
    }
}

impl LocalScope {
    pub fn new(parent: Option<LocalScopeRef>) -> LocalScopeRef {
        Rc::new(RefCell::new(LocalScope {
            labels: HashMap::new(),
            symbols: HashMap::new(),
            parent,
        }))
    }

    pub fn deep_clone(&self) -> LocalScopeRef {
        Rc::new(RefCell::new(self.clone()))
    }

    pub fn insert(&mut self, name: String, symbol: LocalSymbol) -> Option<LocalSymbol> {
        self.symbols.insert(name, symbol)
    }

    pub fn resolve(&self, name: &str) -> Option<LocalSymbol> {
        self.symbols
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.borrow().resolve(name)))
    }

    pub fn resolve_label(&self, name: &str) -> Option<LabelID> {
        self.labels
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.borrow().resolve_label(name)))
    }

    pub fn insert_label(&mut self, name: String, label_id: LabelID) {
        self.labels.insert(name, label_id);
    }

    pub fn with_symbol<F, R>(&self, name: &str, f: F) -> Option<R>
    where
        F: FnOnce(&LocalSymbol) -> R,
    {
        if let Some(sym) = self.symbols.get(name) {
            return Some(f(sym));
        }

        if let Some(parent) = &self.parent {
            return parent.borrow().with_symbol(name, f);
        }

        None
    }

    pub fn with_symbol_mut<F, R>(&mut self, name: &str, f: F) -> Option<R>
    where
        F: FnOnce(&mut LocalSymbol) -> R,
    {
        if let Some(sym) = self.symbols.get_mut(name) {
            return Some(f(sym));
        }

        if let Some(parent) = &self.parent {
            return parent.borrow_mut().with_symbol_mut(name, f);
        }

        None
    }

    pub fn with_symbol_id<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&LocalSymbol) -> R,
    {
        if let Some((_, sym)) = self.symbols.iter().find(|(_, s)| s.symbol_id() == symbol_id) {
            return Some(f(sym));
        }

        if let Some(parent) = &self.parent {
            return parent.borrow().with_symbol_id(symbol_id, f);
        }

        None
    }

    /// Mutable version by SymbolID
    pub fn with_symbol_id_mut<F, R>(&mut self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut LocalSymbol) -> R,
    {
        if let Some((_, sym)) = self.symbols.iter_mut().find(|(_, s)| s.symbol_id() == symbol_id) {
            return Some(f(sym));
        }

        if let Some(parent) = &self.parent {
            return parent.borrow_mut().with_symbol_id_mut(symbol_id, f);
        }

        None
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

    pub fn vis(&self) -> Visibility {
        match &self.kind {
            SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.modifiers.vis.clone(),
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.vis.clone(),
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.modifiers.vis.clone(),
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.modifiers.vis.clone(),
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.modifiers.vis.clone(),
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.vis.clone(),
            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.modifiers.vis.clone(),
            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.modifiers.vis.clone(),
            SymbolEntryKind::ProxiedSymbol(..) => unreachable!(),
        }
    }

    pub fn loc(&self) -> SourceLoc {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.loc.clone(),
            SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.loc.clone(),
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.loc.clone(),
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.loc.clone(),
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.loc.clone(),
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.loc.clone(),
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.loc.clone(),
            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.loc.clone(),
            SymbolEntryKind::ProxiedSymbol(..) => unreachable!(),
        }
    }

    pub fn symbol_id(&self) -> SymbolID {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.symbol_id,
            SymbolEntryKind::Func(resolved_func) => resolved_func.symbol_id,
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.symbol_id,
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.symbol_id,
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.symbol_id,
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.symbol_id,
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.symbol_id,
            SymbolEntryKind::Union(resolved_union) => resolved_union.symbol_id,
            SymbolEntryKind::ProxiedSymbol(_, symbol_id) => *symbol_id,
        }
    }

    pub fn module_id(&self) -> ModuleID {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.module_id,
            SymbolEntryKind::Func(resolved_func) => resolved_func.module_id,
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.module_id,
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.module_id,
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.module_id,
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.module_id,
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.module_id,
            SymbolEntryKind::Union(resolved_union) => resolved_union.module_id,
            SymbolEntryKind::ProxiedSymbol(module_id, _) => *module_id,
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

    pub fn as_proxied_symbol(&self) -> Option<(ModuleID, SymbolID)> {
        match self.kind {
            SymbolEntryKind::ProxiedSymbol(module_id, symbol_id) => Some((module_id, symbol_id)),
            _ => None,
        }
    }
}

pub fn generate_symbol_id() -> SymbolID {
    let mut rng = rand::rng();
    rng.random::<u32>()
}

pub fn generate_label_id() -> LabelID {
    let mut rng = rand::rng();
    rng.random::<u32>()
}

pub fn generate_scope_id() -> ScopeID {
    let mut rng = rand::rng();
    rng.random::<u32>()
}
