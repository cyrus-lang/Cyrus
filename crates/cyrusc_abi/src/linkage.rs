use crate::callconv::CallConv;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Linkage {
    Extern(Option<CallConv>),
    Weak,
    LinkOnce,
}

impl Linkage {
    pub fn is_exclusive(&self) -> bool {
        true
    }
}
