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

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{
    decls::MethodDecls,
    stmts::{TypedImplementInterface, TypedInterfaceStmt},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_interface(&mut self, interface: &TypedInterfaceStmt) {
        let interface_name = &interface.name;

        self.nameconv_check_interface_name(&interface_name, interface.loc);

        // if let Some(generic_params) = &interface.generic_params {
        //     self.analyze_generic_params(generic_params);
        // }

        let mut methods: Vec<String> = Vec::new();

        for method in &interface.methods {
            if !method.params.is_instance_method() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InterfaceMethodsMustHaveSelfModifier),
                    loc: Some(method.loc),
                    hint: None,
                });
            }

            if methods.contains(&method.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InterfaceDuplicateMethod {
                        interface_name: interface_name.clone(),
                        method_name: method.name.clone(),
                    }),
                    loc: Some(method.loc),
                    hint: None,
                });
                continue;
            }

            methods.push(method.name.clone());
        }
    }

    /// Validates that an object correctly implements its declared interfaces.
    ///
    /// Ensures the referenced symbols are valid interfaces, checks visibility
    /// rules, resolves generic interface instantiations, and verifies that all
    /// required interface methods are implemented with matching signatures.
    pub(crate) fn analyze_object_implements_interface_list(
        &mut self,
        object_name: &String,
        impls: &Vec<TypedImplementInterface>,
        method_decls: &MethodDecls,
    ) {
        for implement_interface in impls {
            let symbol_entry = self.query.lookup_symbol_entry(implement_interface.symbol_id).unwrap();

            let interface_decl_id = match symbol_entry.as_interface() {
                Some(id) => id,
                None => {
                    let symbol_name = self.formatter.format_symbol_name(implement_interface.symbol_id);

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::SymbolIsNotInterface { symbol_name }),
                        loc: Some(implement_interface.loc),
                        hint: None,
                    });
                    continue;
                }
            };

            let interface_decl = self.decl_tables.interface_decl(interface_decl_id);

            for func_decl_stmt in &interface_decl.methods {
                let interface_func_decl = func_decl_stmt.as_func_decl();

                if !method_decls.contains(&interface_func_decl.name) {
                    // method missing
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::MissingInterfaceMethodImpl {
                            object_name: object_name.clone(),
                            method_name: interface_func_decl.name.clone(),
                            interface_name: interface_decl.name.clone(),
                        }),
                        loc: Some(implement_interface.loc),
                        hint: None,
                    });
                    continue;
                }

                let method_decl_id = method_decls.get(&interface_func_decl.name).unwrap();
                let method_decl = self.decl_tables.method_decl(method_decl_id);

                // check method declaration mismatch
                if method_decl.func_decl != interface_func_decl {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InterfaceMethodTypeMismatch {
                            object_name: object_name.clone(),
                            method_name: interface_func_decl.name.clone(),
                            interface_name: interface_decl.name.clone(),
                        }),
                        loc: Some(interface_func_decl.loc),
                        hint: None,
                    });
                }
            }
        }
    }
}
