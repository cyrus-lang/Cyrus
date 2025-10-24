use tast::{TypedGlobalVarStmt, TypedProgramTree, TypedStmt};

use crate::{CIRProgramTree, CIRStmt};

#[derive(Debug)]
pub struct CIRWalk {
    program: TypedProgramTree,
}

impl CIRWalk {
    pub fn run_pass(&self) -> CIRProgramTree {
        let stmts = self.lower_stmts(&self.program.body);
        CIRProgramTree { body: stmts }
    }

    fn lower_stmts(&self, stmts: &Vec<TypedStmt>) -> Vec<CIRStmt> {
        todo!();
    }

    fn lower_global_var(&self, global_var: &TypedGlobalVarStmt) {
        todo!();
    }
}
