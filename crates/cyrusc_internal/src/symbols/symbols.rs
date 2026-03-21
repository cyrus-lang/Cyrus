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

use cyrusc_ast::abi::Visibility;
use cyrusc_diagcentral::source_loc::SourceLoc;
use cyrusc_tast::{
    ModuleID, SymbolID,
    sigs::{EnumSig, FuncSig, GlobalVarSig, InterfaceSig, StructSig, TypedefSig, UnionSig},
    stmts::{TypedBlockStmt, TypedVarStmt},
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub kind: SymbolEntryKind,
    pub used: bool,
}

#[derive(Debug, Clone)]
pub enum SymbolEntryKind {
    Method(ResolvedMethod),
    Func(ResolvedFunc),
    Typedef(ResolvedTypedef),
    GlobalVar(ResolvedGlobalVar),
    Struct(ResolvedStruct),
    Enum(ResolvedEnum),
    Union(ResolvedUnion),
    Interface(ResolvedInterface),
    ProxiedSymbol(ModuleID, SymbolID),
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
pub struct ResolvedFunc {
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
    pub variable: TypedVarStmt,
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

    pub fn as_func(&self) -> Option<&ResolvedFunc> {
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

impl LocalOrGlobalSymbol {
    pub fn symbol_id(&self) -> SymbolID {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => local_symbol.symbol_id(),
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => symbol_entry.symbol_id(),
        }
    }

    pub fn symbol_name(&self) -> Option<String> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                LocalSymbolKind::Variable(resolved_variable) => Some(resolved_variable.variable.name.clone()),
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

    /// Returns the set of associated methods for this symbol, if any.
    pub fn associated_methods(&self) -> Option<HashMap<String, SymbolID>> {
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

    /// Looks up a method by name on the underlying type represented by this symbol.
    pub fn lookup_method(&self, name: &str) -> Option<SymbolID> {
        let method_map = self.associated_methods();
        method_map.and_then(|map| map.get(name).cloned())
    }

    /// Returns the generic parameters of this symbol **if it represents a method**.
    pub fn method_generic_params(&self) -> Option<&TypedGenericParamsList> {
        match self {
            LocalOrGlobalSymbol::LocalSymbol(_) => None,
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.generic_params.as_ref(),
                _ => None,
            },
        }
    }

    /// Returns the generic parameter list associated with this symbol,
    /// if the underlying entity supports generics.
    pub fn generic_params(&self) -> Option<TypedGenericParamsList> {
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
