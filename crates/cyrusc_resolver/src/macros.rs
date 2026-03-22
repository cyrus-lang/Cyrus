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
        $resolver.enter_scope($scope);

        let __result = { $body };

        $resolver.exit_scope();

        __result
    }};
}
