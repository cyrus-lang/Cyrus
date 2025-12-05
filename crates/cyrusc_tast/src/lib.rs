use crate::stmts::TypedStmt;
use cyrusc_ast::source_loc::SourceLoc;

pub mod exprs;
pub mod format;
pub mod generics;
pub mod sigs;
pub mod stmts;
mod tests;
pub mod types;

pub type ScopeID = u32;
pub type SymbolID = u32;
pub type LabelID = u32;
pub type ModuleID = u64;

#[derive(Debug, Clone)]
pub struct TypedProgramTree {
    pub body: Vec<TypedStmt>,
    pub file_path: String,
    pub module_id: ModuleID,
    pub module_name: String,
}

unsafe impl Send for TypedProgramTree {}
unsafe impl Sync for TypedProgramTree {}
