use tast::{
    TypedBlockStmt, TypedExprStmt, TypedFuncDeclStmt, TypedFuncDefStmt, TypedFuncParamKind, TypedFuncParams,
    TypedFuncVariadicParams, TypedGlobalVarStmt, TypedProgramTree, TypedStmt, TypedVarStmt, types::SemanticType,
};

use crate::{
    CIRBlockStmt, CIRExpr, CIRFuncDeclStmt, CIRFuncDefStmt, CIRGlobalVarStmt, CIRProgramTree, CIRStmt, CIRVarStmt,
    concrete_type::CIRTy,
};

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
        let mut lowered_stmts: Vec<CIRStmt> = Vec::new();

        for stmt in stmts {
            let lowered_stmt = match stmt {
                TypedStmt::Variable(var_stmt) => self.lower_var(var_stmt),
                TypedStmt::GlobalVariable(global_var_stmt) => self.lower_global_var(global_var_stmt),
                TypedStmt::FuncDef(func_def_stmt) => self.lower_func_def(func_def_stmt),
                TypedStmt::FuncDecl(func_decl_stmt) => self.lower_func_decl(func_decl_stmt),
                TypedStmt::BlockStatement(block_stmt) => CIRStmt::Block(self.lower_block(block_stmt)),
                TypedStmt::If(if_stmt) => todo!(),
                TypedStmt::Return(return_stmt) => todo!(),
                TypedStmt::Break(break_stmt) => todo!(),
                TypedStmt::Continue(continue_stmt) => todo!(),
                TypedStmt::For(for_stmt) => todo!(),
                TypedStmt::While(while_stmt) => todo!(),
                TypedStmt::Switch(switch_stmt) => todo!(),
                TypedStmt::Struct(struct_stmt) => todo!(),
                TypedStmt::Enum(enum_stmt) => todo!(),
                TypedStmt::Union(union_stmt) => todo!(),
                TypedStmt::Interface(interface_stmt) => todo!(),
                TypedStmt::ExportTuple(export_tuple_stmt) => todo!(),
                // skipped
                TypedStmt::Expression(..) | TypedStmt::Defer(..) | TypedStmt::Typedef(..) => continue,
            };

            lowered_stmts.push(lowered_stmt);
        }

        lowered_stmts
    }

    fn lower_global_var(&self, global_var: &TypedGlobalVarStmt) -> CIRStmt {
        let ty = self.lower_sema_type(
            &global_var
                .ty
                .clone()
                .unwrap_or(global_var.expr.clone().unwrap().sema_ty.unwrap()),
        );

        let expr = global_var.expr.clone().and_then(|expr| Some(self.lower_expr(&expr)));

        CIRStmt::GlobalVar(CIRGlobalVarStmt {
            name: global_var.name.clone(),
            ty,
            expr,
            vis: global_var.vis.clone(),
        })
    }

    fn lower_var(&self, var: &TypedVarStmt) -> CIRStmt {
        let ty = self.lower_sema_type(&var.ty.clone().unwrap_or(var.rhs.clone().unwrap().sema_ty.unwrap()));

        let expr = var.rhs.clone().and_then(|expr| Some(self.lower_expr(&expr)));

        CIRStmt::Variable(CIRVarStmt {
            name: var.name.clone(),
            ty,
            expr,
        })
    }

    fn lower_func_params(&self, func_params: &TypedFuncParams) -> (Vec<CIRTy>, bool) {
        let mut params: Vec<CIRTy> = Vec::new();

        func_params
            .list
            .iter()
            .for_each(|func_param_kind| match func_param_kind {
                TypedFuncParamKind::FuncParam(func_param) => {
                    params.push(self.lower_sema_type(&func_param.ty));
                }
                TypedFuncParamKind::SelfModifier(self_modifier) => {
                    params.push(self.lower_sema_type(&self_modifier.ty.clone().unwrap()));
                }
            });

        let mut is_var = false;
        if let Some(variadic) = &func_params.variadic {
            is_var = true;

            match variadic {
                TypedFuncVariadicParams::Typed(..) => todo!(), // TODO
                TypedFuncVariadicParams::UntypedCStyle => {}
            }
        }

        (params, is_var)
    }

    fn lower_func_def(&self, func_def: &TypedFuncDefStmt) -> CIRStmt {
        let (params, is_var) = self.lower_func_params(&func_def.params);

        let body = self.lower_block(&func_def.body);
        let ret = self.lower_sema_type(&func_def.return_type);

        CIRStmt::FuncDef(CIRFuncDefStmt {
            name: func_def.name.clone(),
            params,
            is_var,
            body: Box::new(body),
            ret,
            vis: func_def.vis.clone(),
        })
    }

    fn lower_func_decl(&self, func_decl: &TypedFuncDeclStmt) -> CIRStmt {
        let (params, is_var) = self.lower_func_params(&func_decl.params);
        let ret = self.lower_sema_type(&func_decl.return_type);

        CIRStmt::FuncDecl(CIRFuncDeclStmt {
            name: func_decl.name.clone(),
            params,
            is_var,
            ret,
            vis: func_decl.vis.clone(),
        })
    }

    fn lower_block(&self, block: &TypedBlockStmt) -> CIRBlockStmt {
        let mut stmts = self.lower_stmts(&block.stmts);
        let defer_stmts: Vec<TypedStmt> = block.defers.iter().map(|defer| *defer.operand.clone()).collect();
        stmts.extend(self.lower_stmts(&defer_stmts));
        CIRBlockStmt { stmts }
    }

    fn lower_expr(&self, expr: &TypedExprStmt) -> CIRExpr {
        todo!();
    }

    fn lower_sema_type(&self, sema_type: &SemanticType) -> CIRTy {
        todo!();
    }
}
