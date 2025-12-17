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
    pub fn push(&mut self, sym: SymbolID) -> bool {
        if self.in_progress.contains(&sym) {
            return false;
        }
        self.in_progress.push(sym);
        true
    }

    pub fn pop(&mut self, sym: SymbolID) {
        debug_assert!(self.in_progress.last() == Some(&sym));
        self.in_progress.pop();
    }
}
