use std::fmt::Display;

use cyrusc_ast::source_loc::SourceLoc;

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

pub struct Diag {
    pub level: DiagLevel,
    pub kind: Box<dyn DiagKind>,
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

pub trait DiagKind: Display {
    fn clone_box(&self) -> Box<dyn DiagKind>;
}

impl<T> DiagKind for T
where
    T: Display + Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn DiagKind> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn DiagKind> {
    fn clone(&self) -> Box<dyn DiagKind> {
        self.clone_box()
    }
}

impl Clone for Diag {
    fn clone(&self) -> Self {
        Self {
            level: self.level.clone(),
            kind: self.kind.clone(),
            location: self.location.clone(),
            hint: self.hint.clone(),
        }
    }
}
