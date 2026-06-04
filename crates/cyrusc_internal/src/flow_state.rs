// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

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
