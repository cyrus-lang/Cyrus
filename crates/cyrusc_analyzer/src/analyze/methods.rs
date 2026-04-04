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
use cyrusc_typed_ast::decls::{MethodDecl, MethodDecls};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_object_methods(&mut self, object_name: &String, method_decls: &MethodDecls) {
        for (_, method_decl_id) in method_decls.iter() {
            let method_decl = self.decl_tables.method_decl(*method_decl_id);

            self.analyze_method_generic_params(object_name, method_decls, &method_decl.func_decl.generic_params);

            self.analyze_method(&method_decl);
        }
    }

    pub(crate) fn analyze_method(&self, method_decl: &MethodDecl) {
        // self.analyze_func_decl(method_decl.func_decl);
        todo!();
    }
}
