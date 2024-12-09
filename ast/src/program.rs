use std::fmt;
use token::Span;
use crate::statement::{format_statements, Statement};

#[derive(Debug)]
pub struct Program {
    pub body: Vec<Statement>,
    pub span: Span,
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl Program {
    pub fn new() -> Self {
        Self { body: vec![], span: Span::new_empty_span() }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_statements(&self.body))
    }
}