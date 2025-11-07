#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Linkage {
    Extern,
    Weak,
    LinkOnce,
}

impl Linkage {
    pub fn is_exclusive(&self) -> bool { true }
}