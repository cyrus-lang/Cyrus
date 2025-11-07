#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prologue {
    Naked,
}

impl Prologue {
    pub fn conflicts_with_inline(&self) -> bool {
        matches!(self, Prologue::Naked)
    }
}