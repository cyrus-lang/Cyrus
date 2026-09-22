// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_typed_ast::{SymbolID, decls::DeclID, exprs::TypedExpr};

pub trait ConstResolver {
    fn is_decl_const(&self, decl_id: DeclID) -> bool;
    fn get_var_rhs_expr(&self, decl_id: DeclID) -> Option<TypedExpr>;
    fn lookup_symbol_as_decl_id(&self, symbol_id: SymbolID) -> Option<DeclID>;
}
