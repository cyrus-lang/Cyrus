#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Inlining {
    Inline,
    NoInline,
    AlwaysInline,
}
