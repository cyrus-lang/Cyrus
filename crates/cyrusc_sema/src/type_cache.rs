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
use cyrusc_tast::{SymbolID, types::SemanticType};
use fx_hash::FxHashMap;
use smallvec::SmallVec;

#[derive(Default)]
pub struct TypeResolverCaches {
    // Canonical, fully normalized result for a symbol (no UnresolvedSymbol, no Typedef)
    pub cache: FxHashMap<SymbolID, SemanticType>,
    // Guard against cycles
    pub in_progress: SmallVec<[SymbolID; 16]>,
}

impl TypeResolverCaches {
    pub fn push(&mut self, sym: SymbolID) -> Result<(), ()> {
        if self.in_progress.contains(&sym) {
            return Err(());
        }
        self.in_progress.push(sym);
        Ok(())
    }

    pub fn pop(&mut self, sym: SymbolID) {
        debug_assert!(self.in_progress.last() == Some(&sym));
        self.in_progress.pop();
    }
}
