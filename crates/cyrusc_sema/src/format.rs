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
use crate::analyze::AnalysisContext;
use cyrusc_resolver::{
    Resolver,
    symbols::{LocalOrGlobalSymbol, LocalSymbolKind, SymbolEntryKind},
};
use cyrusc_tast::{ModuleID, ScopeID, SymbolID};

type SymbolFormatterFn<'a> = Box<dyn Fn(SymbolID) -> String + 'a>;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn build_symbol_formatter(
        resolver: &'a Resolver,
        module_id: ModuleID,
    ) -> Box<dyn Fn(Option<ScopeID>) -> SymbolFormatterFn<'a> + 'a> {
        Box::new(move |scope_id_opt: Option<ScopeID>| {
            Box::new(move |symbol_id: SymbolID| -> String {
                Self::format_symbol_name(resolver, module_id, scope_id_opt, symbol_id)
            }) as Box<dyn Fn(SymbolID) -> String + 'a>
        }) as Box<dyn Fn(Option<ScopeID>) -> SymbolFormatterFn<'a> + 'a>
    }

    pub(crate) fn format_symbol_name(
        resolver: &Resolver,
        module_id: ModuleID,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
    ) -> String {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| resolver.resolve_local_scope(module_id, scope_id).clone());

        let Some(sym) = resolver.resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id) else {
            return "SYMBOL".to_string();
        };

        match sym {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                LocalSymbolKind::Variable(resolved_variable) => resolved_variable.typed_variable.name.clone(),
                LocalSymbolKind::Struct(resolved_struct) => resolved_struct.struct_sig.name.clone(),
                LocalSymbolKind::Enum(resolved_enum) => resolved_enum.enum_sig.name.clone(),
                LocalSymbolKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.name.clone(),
                LocalSymbolKind::Interface(resolved_interface) => resolved_interface.interface_sig.name.clone(),
                LocalSymbolKind::Union(resolved_union) => resolved_union.union_sig.name.clone(),
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.name.clone(),
                SymbolEntryKind::Func(resolved_function) => resolved_function.func_sig.name.clone(),
                SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.name.clone(),
                SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.name.clone(),
                SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.name.clone(),
                SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.name.clone(),
                SymbolEntryKind::Interface(resolved_interface) => resolved_interface.interface_sig.name.clone(),
                SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.name.clone(),
                SymbolEntryKind::ProxiedSymbol(..) => unreachable!(),
            },
        }
    }
}

pub fn format_missing_fields(list: &Vec<String>) -> String {
    list.iter()
        .map(|str| format!("'{str}'"))
        .collect::<Vec<String>>()
        .join(", ")
}
