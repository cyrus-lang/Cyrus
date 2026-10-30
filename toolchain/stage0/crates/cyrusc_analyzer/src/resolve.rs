// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::context::AnalysisContext;
use cyrusc_ast::Mutability;
use cyrusc_const_eval::{fold::ConstFolder, resolver::ConstResolver};
use cyrusc_typed_ast::{
    SymbolID,
    decls::DeclID,
    exprs::{TypedExpr, ValueCategory},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn is_const_qualified_lvalue(&self, expr: &TypedExpr) -> bool {
        // declaration-level constness
        if matches!(expr.val_cat, ValueCategory::LValue(Mutability::Const)) {
            return true;
        }

        // type-level constness
        if expr.ty.as_ref().map(|ty| ty.is_const()).unwrap_or(false) {
            return true;
        }

        false
    }

    #[inline]
    pub(crate) fn fold_const_expr(&mut self, expr: &mut TypedExpr) {
        let mut folder = ConstFolder::new(self, &self.decl_tables, self.target, self.tctx.clone(), self);

        folder.fold_expr(expr, self);
    }

    fn get_var_rhs_expr(&self, decl_id: DeclID) -> Option<TypedExpr> {
        if let Some(var_decl_id) = decl_id.as_var() {
            let var_decl = self.decl_tables.var_decl(var_decl_id);

            var_decl.rhs.clone()
        } else if let Some(global_var_decl_id) = decl_id.as_global_var() {
            let global_var_decl = self.decl_tables.global_var_decl(global_var_decl_id);

            global_var_decl.rhs.clone()
        } else {
            None
        }
    }
}

impl<'a> ConstResolver for AnalysisContext<'a> {
    fn get_var_rhs_expr(&self, decl_id: DeclID) -> Option<TypedExpr> {
        self.get_var_rhs_expr(decl_id)
    }

    fn is_decl_const(&self, decl_id: DeclID) -> bool {
        match decl_id {
            DeclID::GlobalVar(global_var_decl_id) => {
                let global_var_decl = self.decl_tables.global_var_decl(global_var_decl_id);

                global_var_decl.is_const
            }
            DeclID::Var(var_decl_id) => {
                let var_decl = self.decl_tables.var_decl(var_decl_id);

                var_decl.is_const
            }
            _ => false,
        }
    }

    fn lookup_symbol_as_decl_id(&self, symbol_id: SymbolID) -> Option<DeclID> {
        self.query.lookup_symbol_as_decl_id(symbol_id)
    }
}
