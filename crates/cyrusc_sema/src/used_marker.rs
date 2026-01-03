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
use cyrusc_resolver::symbols::LocalScopeRef;
use cyrusc_tast::{ModuleID, SymbolID};

// FIXME Symbol marking not finished yet.
impl<'a> AnalysisContext<'a> {
    pub(crate) fn mark_symbol_used_once(&mut self, _module_id: ModuleID, _symbol_id: SymbolID) {
        // let mut global_symbols = self.resolver.global_symbols.lock().unwrap();
        // let symbol_table = global_symbols.get_mut(&module_id).unwrap();
        // let symbol_entry = symbol_table
        //     .entries
        //     .iter_mut()
        //     .find(|(entry_symbol_id, _)| **entry_symbol_id == symbol_id)
        //     .unwrap()
        //     .1;
        // symbol_entry.used = true;
        // drop(global_symbols);
    }

    pub(crate) fn mark_local_symbol_used_once(
        &mut self,
        local_scope_rc: LocalScopeRef,
        module_id: ModuleID,
        symbol_id: SymbolID,
    ) {
        let mut local_scope = local_scope_rc.borrow_mut();
        let local_symbol = match local_scope
            .symbols
            .iter_mut()
            .find(|local_symbol| local_symbol.1.get_symbol_id() == symbol_id)
        {
            Some((_, local_symbol)) => local_symbol,
            None => {
                return self.mark_symbol_used_once(module_id, symbol_id);
            }
        };
        local_symbol.used = true;
        drop(local_scope);
    }

    // pub(crate) fn mark_func_used(
    //     &mut self,
    //     local_scope_opt: Option<LocalScopeRef>,
    //     module_id: ModuleID,
    //     symbol_id: SymbolID,
    // ) {
    //     if let Some(local_scope_rc) = local_scope_opt {
    //         if self
    //             .resolver
    //             .resolve_symbol_from_local_scope(local_scope_rc.clone(), symbol_id)
    //             .is_some()
    //         {
    //             self.mark_local_symbol_used_once(local_scope_rc, module_id, symbol_id);
    //             return;
    //         }
    //     }

    //     self.mark_symbol_used_once(module_id, symbol_id);
    // }
}
