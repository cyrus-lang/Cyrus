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

use std::collections::HashMap;

use cyrusc_ast::abi::Visibility;
use cyrusc_source_loc::{FileID, Loc};
use cyrusc_typed_ast::{
    SymbolID,
    sigs::{EnumSig, FuncSig, GlobalVarSig, InterfaceSig, StructSig, TypedefSig, UnionSig},
    stmts::{TypedBlockStmt, TypedGenericParamsList, TypedVarStmt},
};

use crate::symbols::table::ScopeTable;

#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub parent_scope_id: Option<SymbolID>,
    pub kind: SymbolEntryKind,
    pub vis_opt: Option<Visibility>,
    pub used: bool,
}

#[derive(Debug, Clone)]
pub enum SymbolEntryKind {
    Unresolved,

    Module(Module),
    Namespace(Namespace),

    Method(ResolvedMethod),
    Func(ResolvedFunc),
    Typedef(ResolvedTypedef),
    Var(ResolvedVar),
    GlobalVar(ResolvedGlobalVar),
    Struct(ResolvedStruct),
    Enum(ResolvedEnum),
    Union(ResolvedUnion),
    Interface(ResolvedInterface),

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

#[derive(Debug, Clone)]
pub struct ResolvedUnion {
    pub symbol_id: SymbolID,
    pub union_sig: UnionSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedEnum {
    pub symbol_id: SymbolID,
    pub enum_sig: EnumSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedStruct {
    pub symbol_id: SymbolID,
    pub struct_sig: StructSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedGlobalVar {
    pub symbol_id: SymbolID,
    pub file_id: FileID,
    pub global_var_sig: GlobalVarSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedMethod {
    pub symbol_id: SymbolID,
    pub func_sig: FuncSig,
    pub func_body: Option<Box<TypedBlockStmt>>,
}

#[derive(Debug, Clone)]
pub struct ResolvedFunc {
    pub symbol_id: SymbolID,
    pub file_id: FileID,
    pub func_sig: FuncSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedTypedef {
    pub symbol_id: SymbolID,
    pub typedef_sig: TypedefSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedInterface {
    pub symbol_id: SymbolID,
    pub interface_sig: InterfaceSig,
}

#[derive(Debug, Clone)]
pub struct ResolvedVar {
    pub symbol_id: SymbolID,
    pub variable: TypedVarStmt,
}

impl SymbolEntry {
    pub fn new(kind: SymbolEntryKind, vis: Option<Visibility>, parent_scope_id: Option<SymbolID>) -> Self {
        Self {
            parent_scope_id,
            kind,
            vis_opt: vis,
            used: false,
        }
    }

    #[inline]
    pub fn unresolved(vis: Option<Visibility>, parent_scope_id: Option<SymbolID>) -> Self {
        Self {
            parent_scope_id,
            kind: SymbolEntryKind::Unresolved,
            vis_opt: vis,
            used: false,
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

    pub fn symbol_id(&self) -> SymbolID {
        match &self.kind {
            SymbolEntryKind::Unresolved => unreachable!(),
            SymbolEntryKind::Method(resolved_method) => resolved_method.symbol_id,
            SymbolEntryKind::Func(resolved_func) => resolved_func.symbol_id,
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.symbol_id,
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.symbol_id,
            SymbolEntryKind::Var(resolved_var) => resolved_var.symbol_id,
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.symbol_id,
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.symbol_id,
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.symbol_id,
            SymbolEntryKind::Union(resolved_union) => resolved_union.symbol_id,
            SymbolEntryKind::Module(module) => module.scope_id,
            SymbolEntryKind::ProxiedSymbol { symbol_id, .. } => *symbol_id,
            SymbolEntryKind::ProxiedModule { symbol_id } => *symbol_id,
            SymbolEntryKind::Namespace(_) => unreachable!(),
        }
    }

    pub fn decl_name(&self) -> String {
        match &self.kind {
            SymbolEntryKind::Unresolved => "<UNRESOLVED_SYMBOL_ENTRY_KIND>".to_string(),
            SymbolEntryKind::ProxiedSymbol { .. } => "<PROXIED_SYMBOL>".to_string(),
            SymbolEntryKind::ProxiedModule { .. } => "<PROXIED_MODULE>".to_string(),

            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.name.clone(),
            SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.name.clone(),
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.name.clone(),
            SymbolEntryKind::Var(resolved_var) => resolved_var.variable.name.clone(),
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.name.clone(),
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.name.clone(),
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.name.clone(),
            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.name.clone(),
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.name.clone(),
            SymbolEntryKind::Module(module) => module.name.clone(),
            SymbolEntryKind::Namespace(namespace) => namespace.name.clone(),
        }
    }

    pub fn vis(&self) -> Visibility {
        match &self.kind {
            SymbolEntryKind::Unresolved => unreachable!(),
            SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.modifiers.vis.clone(),
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.vis.clone(),
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.modifiers.vis.clone(),
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.modifiers.vis.clone(),
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.modifiers.vis.clone(),
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.vis.clone(),
            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.modifiers.vis.clone(),
            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.modifiers.vis.clone(),
            SymbolEntryKind::Var(_) => unreachable!(),
            SymbolEntryKind::Module(_) => unreachable!(),
            SymbolEntryKind::Namespace(_) => unreachable!(),
            SymbolEntryKind::ProxiedSymbol { .. } => unreachable!(),
            SymbolEntryKind::ProxiedModule { .. } => unreachable!(),
        }
    }

    pub fn loc(&self) -> Loc {
        match &self.kind {
            SymbolEntryKind::Unresolved => unreachable!(),
            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.loc,
            SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.loc,
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.loc,
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.loc,
            SymbolEntryKind::Var(resolved_var) => resolved_var.variable.loc,
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.loc,
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.loc,
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.loc,
            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.loc,
            SymbolEntryKind::Namespace(namespace) => namespace.loc,
            SymbolEntryKind::Module(_) => unreachable!(),
            SymbolEntryKind::ProxiedSymbol { .. } => unreachable!(),
            SymbolEntryKind::ProxiedModule { .. } => unreachable!(),
        }
    }

    pub fn symbol_generic_params(&self) -> Option<TypedGenericParamsList> {
        match &self.kind {
            SymbolEntryKind::Unresolved => unreachable!(),
            SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.generic_params.clone(),
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.generic_params.clone(),
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.generic_params.clone(),
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.generic_params.clone(),
            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.generic_params.clone(),
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.generic_params.clone(),
            SymbolEntryKind::Var(_) => None,
            SymbolEntryKind::GlobalVar(_) => None,
            SymbolEntryKind::Method(_) => None,
            SymbolEntryKind::Module(_) => unreachable!(),
            SymbolEntryKind::Namespace(_) => unreachable!(),
            SymbolEntryKind::ProxiedSymbol { .. } => unreachable!(),
            SymbolEntryKind::ProxiedModule { .. } => unreachable!(),
        }
    }

    pub fn symbol_methods(&self) -> Option<HashMap<String, SymbolID>> {
        match &self.kind {
            SymbolEntryKind::Struct(resolved_struct) => Some(resolved_struct.struct_sig.methods.clone()),
            SymbolEntryKind::Enum(resolved_enum) => Some(resolved_enum.enum_sig.methods.clone()),
            SymbolEntryKind::Union(resolved_union) => Some(resolved_union.union_sig.methods.clone()),
            _ => None,
        }
    }

    pub fn get_method(&self, name: &str) -> Option<SymbolID> {
        self.symbol_methods()?.get(name).cloned()
    }

    pub fn method_generic_params(&self) -> Option<&TypedGenericParamsList> {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.generic_params.as_ref(),
            _ => None,
        }
    }

    pub fn is_kind_of_variable(&self) -> bool {
        self.as_var().is_some() || self.as_global_var().is_some()
    }

    pub fn is_const_qualified(&self) -> bool {
        match &self.kind {
            SymbolEntryKind::Var(resolved_var) => resolved_var.variable.is_const,
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.is_const,
            _ => false,
        }
    }

    pub fn as_struct(&self) -> Option<&ResolvedStruct> {
        match &self.kind {
            SymbolEntryKind::Struct(resolved_struct) => Some(resolved_struct),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&ResolvedEnum> {
        match &self.kind {
            SymbolEntryKind::Enum(resolved_enum) => Some(resolved_enum),
            _ => None,
        }
    }

    pub fn as_union(&self) -> Option<&ResolvedUnion> {
        match &self.kind {
            SymbolEntryKind::Union(resolved_union) => Some(resolved_union),
            _ => None,
        }
    }

    pub fn as_typedef(&self) -> Option<&ResolvedTypedef> {
        match &self.kind {
            SymbolEntryKind::Typedef(resolved_typedef) => Some(resolved_typedef),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<&ResolvedFunc> {
        match &self.kind {
            SymbolEntryKind::Func(resolved_func) => Some(resolved_func),
            _ => None,
        }
    }

    pub fn as_global_var(&self) -> Option<&ResolvedGlobalVar> {
        match &self.kind {
            SymbolEntryKind::GlobalVar(resolved_global_var) => Some(resolved_global_var),
            _ => None,
        }
    }

    pub fn as_var(&self) -> Option<&ResolvedVar> {
        match &self.kind {
            SymbolEntryKind::Var(resolved_var) => Some(resolved_var),
            _ => None,
        }
    }

    pub fn as_interface(&self) -> Option<&ResolvedInterface> {
        match &self.kind {
            SymbolEntryKind::Interface(resolved_interface) => Some(resolved_interface),
            _ => None,
        }
    }

    pub fn as_method(&self) -> Option<&ResolvedMethod> {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => Some(resolved_method),
            _ => None,
        }
    }

    pub fn as_proxied_symbol(&self) -> Option<(SymbolID, SymbolID)> {
        match self.kind {
            SymbolEntryKind::ProxiedSymbol { scope_id, symbol_id } => Some((scope_id, symbol_id)),
            _ => None,
        }
    }
}
