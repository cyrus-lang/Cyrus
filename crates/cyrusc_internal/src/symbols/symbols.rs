// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::symbols::table::ScopeTable;
use cyrusc_ast::abi::Visibility;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    SymbolID,
    decls::{
        EnumDeclID, FuncDeclID, GlobalVarDeclID, InterfaceDeclID, MethodDeclID, StructDeclID, TypedefDeclID,
        UnionDeclID, VarDeclID,
    },
};

#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub parent_scope_id: Option<SymbolID>,
    pub kind: SymbolEntryKind,
    pub vis_opt: Option<Visibility>,
    pub used: bool,
    pub loc: Option<Loc>,
}

#[derive(Debug, Clone)]
pub enum SymbolEntryKind {
    Unresolved,

    Module(Module),
    Namespace(Namespace),

    Func(FuncDeclID),
    Method(MethodDeclID),
    Struct(StructDeclID),
    Enum(EnumDeclID),
    Union(UnionDeclID),
    Interface(InterfaceDeclID),
    Var(VarDeclID),
    GlobalVar(GlobalVarDeclID),
    Typedef(TypedefDeclID),

    // utility symbols
    ProxiedSymbol { scope_id: SymbolID, symbol_id: SymbolID },
    ProxiedModule { symbol_id: SymbolID },
}

#[derive(Debug, Clone)]
pub struct Module {
    pub scope_id: SymbolID,
    pub name: String,
    pub scope: ScopeTable,
}

#[derive(Debug, Clone)]
pub struct Namespace {
    pub name: String,
    pub scope: ScopeTable,
    pub loc: Loc,
}

impl SymbolEntry {
    pub fn new(
        kind: SymbolEntryKind,
        vis: Option<Visibility>,
        parent_scope_id: Option<SymbolID>,
        loc: Option<Loc>,
    ) -> Self {
        Self {
            parent_scope_id,
            kind,
            vis_opt: vis,
            used: false,
            loc,
        }
    }

    #[inline]
    pub fn unresolved(vis: Option<Visibility>, parent_scope_id: Option<SymbolID>, loc: Option<Loc>) -> Self {
        Self {
            parent_scope_id,
            kind: SymbolEntryKind::Unresolved,
            vis_opt: vis,
            used: false,
            loc,
        }
    }

    /// Return a reference to the scope table for this symbol entry.
    ///
    /// Only `Module` and `Namespace` variants own scope tables.
    /// Calling this on any non-scope symbol will return `None`.
    #[inline]
    pub fn get_scope_table(&self) -> Option<&ScopeTable> {
        match &self.kind {
            SymbolEntryKind::Module(module) => Some(&module.scope),
            SymbolEntryKind::Namespace(namespace) => Some(&namespace.scope),
            _ => None,
        }
    }

    /// Return a mutable reference to the scope table for this symbol entry.
    ///
    /// Only `Module` and `Namespace` variants own scope tables.
    /// Returns `None` if the symbol is not a scope.
    #[inline]
    pub fn get_scope_table_mut(&mut self) -> Option<&mut ScopeTable> {
        match &mut self.kind {
            SymbolEntryKind::Module(module) => Some(&mut module.scope),
            SymbolEntryKind::Namespace(namespace) => Some(&mut namespace.scope),
            _ => None,
        }
    }

    #[inline]
    pub fn as_namespace(&self) -> Option<&Namespace> {
        match &self.kind {
            SymbolEntryKind::Namespace(namespace) => Some(namespace),
            _ => None,
        }
    }

    #[inline]
    pub fn as_proxied_symbol(&self) -> Option<(SymbolID, SymbolID)> {
        match self.kind {
            SymbolEntryKind::ProxiedSymbol { scope_id, symbol_id } => Some((scope_id, symbol_id)),
            _ => None,
        }
    }

    #[inline]
    pub fn as_var(&self) -> Option<VarDeclID> {
        match &self.kind {
            SymbolEntryKind::Var(var_decl_id) => Some(*var_decl_id),
            _ => None,
        }
    }

    #[inline]
    pub fn as_global_var(&self) -> Option<GlobalVarDeclID> {
        match &self.kind {
            SymbolEntryKind::GlobalVar(global_var_decl_id) => Some(*global_var_decl_id),
            _ => None,
        }
    }

    #[inline]
    pub fn is_var_or_global_var(&self) -> bool {
        matches!(self.kind, SymbolEntryKind::Var(_) | SymbolEntryKind::GlobalVar(_))
    }

    #[inline]
    pub fn as_func(&self) -> Option<FuncDeclID> {
        match &self.kind {
            SymbolEntryKind::Func(func_decl_id) => Some(*func_decl_id),
            _ => None,
        }
    }

    #[inline]
    pub fn is_func(&self) -> bool {
        matches!(self.kind, SymbolEntryKind::Func(_))
    }

    #[inline]
    pub fn as_interface(&self) -> Option<InterfaceDeclID> {
        match &self.kind {
            SymbolEntryKind::Interface(interface_decl_id) => Some(*interface_decl_id),
            _ => None,
        }
    }
}
