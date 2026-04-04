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
use cyrusc_typed_ast::decls::{MethodDecl, MethodDeclID, MethodDecls};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_object_methods(&mut self, object_name: &String, method_decls: &MethodDecls) {
        // pass 1: declarations
        for (_, method_decl_id) in method_decls.iter() {
            let mut method_decl = self.decl_tables.method_decl(*method_decl_id);

            self.analyze_method_generic_params(object_name, method_decls, &method_decl.func_decl.generic_params);
            self.analyze_method_decl(&mut method_decl);

            self.decl_tables.with_method_decl_mut(*method_decl_id, |_method_decl| {
                *_method_decl = method_decl;
            });
        }

        // pass 2: bodies
        for (_, method_decl_id) in method_decls.iter() {
            let mut method_decl = self.decl_tables.method_decl(*method_decl_id);

            if !method_decl.func_decl.is_generic() {
                self.analyze_method_body(*method_decl_id, &mut method_decl);
            } else {
                // generic methods are being analyzed in call-site
            }

            self.decl_tables.with_method_decl_mut(*method_decl_id, |_method_decl| {
                *_method_decl = method_decl.clone();
            });
        }
    }

    pub(crate) fn analyze_method_decl(&mut self, method_decl: &mut MethodDecl) {
        self.analyze_func_decl(&mut method_decl.func_decl);

        self.nameconv_check_method_name(&method_decl.func_decl.name, method_decl.func_decl.loc);
    }

    pub(crate) fn analyze_method_body(&mut self, method_decl_id: MethodDeclID, method_decl: &mut MethodDecl) {
        let parent_method = self.func_env.current_method;
        let parent_func = self.func_env.current_func.clone();

        self.func_env.current_method = Some(method_decl_id);
        self.func_env.current_func = Some(method_decl.func_decl.as_func_type());
        self.analyze_func_body(&mut method_decl.body.as_mut().unwrap(), &method_decl.func_decl.ret_type);
        self.func_env.current_method = parent_method;
        self.func_env.current_func = parent_func;
    }
}
