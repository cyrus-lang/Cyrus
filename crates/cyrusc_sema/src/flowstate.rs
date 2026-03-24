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

#[derive(Debug)]
pub(crate) enum ControlContext {
    Loop,
    Switch,
    While,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum FlowState {
    // Execution can continue normally
    Reachable,
    // Execution cannot reach further statements (after return/break/continue)
    Unreachable,
    // This path definitely returned from the function
    Returns,
}

impl FlowState {
    pub fn merge(&self, other: FlowState) -> FlowState {
        match (self, other) {
            (FlowState::Returns, FlowState::Returns) => FlowState::Returns,
            (FlowState::Unreachable, FlowState::Unreachable) => FlowState::Unreachable,
            _ => FlowState::Reachable,
        }
    }
}
