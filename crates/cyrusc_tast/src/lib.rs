use crate::stmts::TypedStmt;
use ast::source_loc::SourceLoc;

pub mod exprs;
pub mod format;
pub mod stmts;
pub mod types;

pub type ScopeID = u32;
pub type SymbolID = u32;
pub type ModuleID = u64;

#[derive(Debug)]
pub struct TypedProgramTree {
    pub body: Vec<TypedStmt>,
}
