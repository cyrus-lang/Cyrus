/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

use crate::context::AnalysisContext;
use cyrusc_ast::Mutability;
use cyrusc_const_eval::{fold::ConstFolder, resolver::ConstResolver};
use cyrusc_typed_ast::{
    SymbolID,
    decls::DeclID,
    exprs::{TypedExpr, ValueCategory},
    types::SemaType,
};

impl<'a> AnalysisContext<'a> {
    /// Returns `true` when a const‑qualified type is assigned to a mutable variable.
    ///
    /// Example:
    ///     var x: const int = 10;
    pub(crate) fn is_const_qualified_type_assigned_to_non_const_variable(
        &mut self,
        ty: &SemaType,
        is_variable_const: bool,
    ) -> bool {
        !is_variable_const && ty.is_const()
    }

    pub(crate) fn is_const_qualified_lvalue(&self, expr: &TypedExpr) -> bool {
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
        let mut folder = ConstFolder::new(self, &self.decl_tables, self.target);

        folder.fold_expr(expr);
    }

    fn resolve_variable_rhs_expr(&self, decl_id: DeclID) -> Option<TypedExpr> {
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
    fn resolve_symbol_expr(&self, decl_id: DeclID) -> Option<TypedExpr> {
        self.resolve_variable_rhs_expr(decl_id)
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
