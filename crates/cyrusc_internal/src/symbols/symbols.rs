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
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    ModuleID, SymbolID,
    sigs::{EnumSig, FuncSig, GlobalVarSig, InterfaceSig, StructSig, TypedefSig, UnionSig},
    stmts::{TypedBlockStmt, TypedGenericParamsList, TypedVarStmt},
};

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
    Var(ResolvedVar),
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
pub struct ResolvedVar {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub variable: TypedVarStmt,
}

impl SymbolEntry {
    pub fn new(kind: SymbolEntryKind) -> Self {
        Self { used: false, kind }
    }

    pub fn module_id(&self) -> ModuleID {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.module_id,
            SymbolEntryKind::Func(resolved_func) => resolved_func.module_id,
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.module_id,
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.module_id,
            SymbolEntryKind::Var(resolved_var) => resolved_var.module_id,
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.module_id,
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.module_id,
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.module_id,
            SymbolEntryKind::Union(resolved_union) => resolved_union.module_id,
            SymbolEntryKind::ProxiedSymbol(module_id, _) => *module_id,
        }
    }

    pub fn symbol_id(&self) -> SymbolID {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.symbol_id,
            SymbolEntryKind::Func(resolved_func) => resolved_func.symbol_id,
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.symbol_id,
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.symbol_id,
            SymbolEntryKind::Var(resolved_var) => resolved_var.symbol_id,
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.symbol_id,
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.symbol_id,
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.symbol_id,
            SymbolEntryKind::Union(resolved_union) => resolved_union.symbol_id,
            SymbolEntryKind::ProxiedSymbol(_, symbol_id) => *symbol_id,
        }
    }

    pub fn decl_name(&self) -> String {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.name.clone(),
            SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.name.clone(),
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.name.clone(),
            SymbolEntryKind::Var(resolved_var) => resolved_var.variable.name.clone(),
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.name.clone(),
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.name.clone(),
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.name.clone(),
            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.name.clone(),
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.name.clone(),
            SymbolEntryKind::ProxiedSymbol(_, _) => "<PROXIED_SYMBOL>".to_string(),
        }
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
            SymbolEntryKind::Var(_) => unreachable!(),
            SymbolEntryKind::ProxiedSymbol(..) => unreachable!(),
        }
    }

    pub fn loc(&self) -> Loc {
        match &self.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.loc,
            SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.loc,
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.loc,
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.loc,
            SymbolEntryKind::Var(resolved_var) => resolved_var.variable.loc,
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.loc,
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.loc,
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.loc,
            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.loc,
            SymbolEntryKind::ProxiedSymbol(..) => unreachable!(),
        }
    }

    pub fn symbol_generic_params(&self) -> Option<TypedGenericParamsList> {
        match &self.kind {
            SymbolEntryKind::Func(resolved_func) => resolved_func.func_sig.generic_params.clone(),
            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.generic_params.clone(),
            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.generic_params.clone(),
            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.generic_params.clone(),
            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.generic_params.clone(),
            SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.generic_params.clone(),
            SymbolEntryKind::Var(_) => None,
            SymbolEntryKind::GlobalVar(_) => None,
            SymbolEntryKind::Method(_) => None,
            SymbolEntryKind::ProxiedSymbol(..) => unreachable!("proxied symbol should be resolved first"),
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

    pub fn lookup_method(&self, name: &str) -> Option<SymbolID> {
        self.symbol_methods()?.get(name).cloned()
    }

    pub fn method_generic_params(&self) -> Option<&TypedGenericParamsList> {
        match &self.kind {
            SymbolEntryKind::Method(resolved_methods) => resolved_methods.func_sig.generic_params.as_ref(),
            _ => None,
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
