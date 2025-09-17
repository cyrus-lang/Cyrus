use ast::source_loc::SourceLoc;

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diag<K> {
    pub level: DiagLevel,
    pub kind: K,
    pub location: Option<DiagLoc>,
    pub hint: Option<String>,
}

impl DiagLoc {
    pub fn new(loc: SourceLoc) -> Self {
        Self {
            file: loc.file_path,
            line: loc.line,
            column: loc.column,
        }
    }
}
