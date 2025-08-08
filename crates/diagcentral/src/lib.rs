use ast::token::Location;

pub mod reporter;
mod tests;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagLevel {
    Error,
    Warning,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagLoc {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diag<K> {
    pub level: DiagLevel,
    pub kind: K,
    pub location: Option<DiagLoc>,
    pub hint: Option<String>,
}

impl DiagLoc {
    pub fn new(file: String, loc: Location, span_end: usize) -> Self {
        Self {
            file,
            line: loc.line,
            column: loc.column,
            length: span_end,
        }
    }
}
