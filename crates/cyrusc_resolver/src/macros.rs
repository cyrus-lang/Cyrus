// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

/// Executes a block within the given local scope.
///
/// The macro:
/// - Takes a pre‑constructed `LocalScope`
/// - Pushes it onto the resolver's scope stack
/// - Executes the provided block
/// - Pops the scope before returning the block's result
///
/// Guarantees balanced scope entry/exit even if the block performs early returns.
#[macro_export]
macro_rules! with_local_scope {
    ($resolver:expr, $scope:expr, $body:block) => {{
        $resolver.enter_local_scope($scope);

        let __result = { $body };

        $resolver.exit_local_scope();

        __result
    }};
}

#[macro_export]
macro_rules! impl_helper_method__get_kind {
    ($name:ident, $variant:ident, $ty:ty) => {
        fn $name(&self, id: SymbolID) -> Option<$ty> {
            match &self.lookup_symbol_entry(id)?.kind {
                SymbolEntryKind::$variant(v) => Some(v.clone()),
                _ => None,
            }
        }
    };
}
