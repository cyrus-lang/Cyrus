use crate::analyze::AnalysisContext;

#[allow(unused)]
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

impl<'a> AnalysisContext<'a> {
    pub(crate) fn merge_flow_state(&self, a: FlowState, b: FlowState) -> FlowState {
        match (a, b) {
            (FlowState::Returns, FlowState::Returns) => FlowState::Returns,
            (FlowState::Unreachable, FlowState::Unreachable) => FlowState::Unreachable,
            _ => FlowState::Reachable,
        }
    }
}
