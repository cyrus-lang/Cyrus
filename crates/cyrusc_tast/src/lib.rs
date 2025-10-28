use crate::stmts::TypedStmt;
use cyrusc_ast::source_loc::SourceLoc;

pub mod exprs;
pub mod format;
pub mod stmts;
pub mod types;

pub type ScopeID = u32;
pub type SymbolID = u32;
pub type ModuleID = u64;

#[derive(Debug, Clone)]
pub struct TypedProgramTree {
    pub body: Vec<TypedStmt>,
    pub file_path: String,
}
