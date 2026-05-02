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

use crate::symbols::symbols::SymbolEntry;
use cyrusc_source_loc::FileID;
use cyrusc_typed_ast::{
    SymbolID,
    decls::{
        DeclID, EnumDeclID, FuncDeclID, GlobalVarDeclID, InterfaceDeclID, MethodDeclID, StructDeclID, TypedefDeclID,
        UnionDeclID, VarDeclID,
    },
};

pub mod symbols;
pub mod table;

pub trait SymbolQuery: Sync + Send {
    fn get_var(&self, symbol_id: SymbolID) -> Option<VarDeclID>;
    fn get_global_var(&self, symbol_id: SymbolID) -> Option<GlobalVarDeclID>;
    fn get_func(&self, symbol_id: SymbolID) -> Option<FuncDeclID>;
    fn get_method(&self, symbol_id: SymbolID) -> Option<MethodDeclID>;
    fn get_typedef(&self, symbol_id: SymbolID) -> Option<TypedefDeclID>;
    fn get_union(&self, symbol_id: SymbolID) -> Option<UnionDeclID>;
    fn get_enum(&self, symbol_id: SymbolID) -> Option<EnumDeclID>;
    fn get_struct(&self, symbol_id: SymbolID) -> Option<StructDeclID>;
    fn get_interface(&self, symbol_id: SymbolID) -> Option<InterfaceDeclID>;

    fn lookup_symbol_id(&self, scope_id: SymbolID, name: &str) -> Option<SymbolID>;
    fn lookup_symbol_id_in_scope(&self, scope_id: SymbolID, name: &str) -> Option<SymbolID>;
    fn lookup_symbol_entry(&self, symbol_id: SymbolID) -> Option<SymbolEntry>;

    fn lookup_module_name(&self, file_id: FileID) -> Option<String>;

    fn lookup_symbol_as_decl_id(&self, symbol_id: SymbolID) -> Option<DeclID>;
}
