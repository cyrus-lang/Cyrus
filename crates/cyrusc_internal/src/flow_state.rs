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

/// Represents the current control-flow region during analysis.
///
/// Used to validate context-sensitive control statements such as
/// `break` or `continue`, ensuring they appear within a valid construct
/// (e.g., loop, switch).
#[derive(Debug)]
pub enum ControlRegion {
    Loop,
    Switch,
}

impl ControlRegion {
    pub fn allows_break(&self) -> bool {
        matches!(self, Self::Loop)
    }

    pub fn allows_continue(&self) -> bool {
        matches!(self, Self::Loop)
    }
}

/// Describes the control-flow state after evaluating a statement or block.
///
/// This helps the analyzer reason about reachability and determine whether
/// execution can continue, has become unreachable, or the function has
/// definitely returned.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FlowState {
    // Execution can continue normally
    Reachable,
    // Execution cannot reach further statements (after return/break/continue)
    Unreachable,
    // This path definitely returned from the function
    Returns,
}

impl FlowState {
    /// Merges two flow states coming from different control-flow paths.
    ///
    /// Used when analyzing branching constructs (e.g., `if`, `switch`) to
    /// compute the resulting reachability state after the branches join.
    pub fn merge(&self, other: FlowState) -> FlowState {
        match (self, other) {
            (FlowState::Returns, FlowState::Returns) => FlowState::Returns,
            (FlowState::Unreachable, FlowState::Unreachable) => FlowState::Unreachable,
            _ => FlowState::Reachable,
        }
    }
}
