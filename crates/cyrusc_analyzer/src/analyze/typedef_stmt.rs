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
use cyrusc_typed_ast::stmts::TypedTypedefStmt;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_typedef(&mut self, typedef: &mut TypedTypedefStmt) {
        typedef.ty = match self.normalize_sema_type(typedef.ty.clone(), typedef.loc) {
            Some(sema_type) => sema_type,
            None => return,
        };

        let typedef_decl_id = self.query.get_typedef(typedef.symbol_id).unwrap();
        self.decl_tables.with_typedef_decl_mut(typedef_decl_id, |typedef_decl| {
            typedef_decl.ty = Box::new(typedef.ty.clone());
        });
    }
}
