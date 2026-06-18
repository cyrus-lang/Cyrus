// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::context::AnalysisContext;
use cyrusc_typed_ast::stmts::TypedTypedefStmt;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_typedef(&mut self, typedef: &mut TypedTypedefStmt) {
        typedef.ty = match self.normalize_and_check_type_formation(typedef.ty.clone(), typedef.loc, 0) {
            Some(ty) => ty,
            None => return,
        };

        self.analyze_generic_bounds(&typedef.generic_params);

        self.decl_tables
            .with_typedef_decl_mut(typedef.typedef_decl_id, |typedef_decl| {
                typedef_decl.ty = Box::new(typedef.ty.clone());
            });
    }
}
