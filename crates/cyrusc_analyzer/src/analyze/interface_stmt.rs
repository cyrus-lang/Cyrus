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
    format::format_sema_type,
    stmts::{TypedImplementInterface, TypedInterfaceStmt},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_interface(&mut self, interface: &TypedInterfaceStmt) {
        let interface_name = &interface.name;

        self.nameconv_check_interface_name(&interface_name, interface.loc);

        let mut methods: Vec<String> = Vec::new();

        for (_, method_decl_id) in interface.methods.iter() {
            let method_decl = self.decl_tables.method_decl(*method_decl_id);
            let func_decl = method_decl.func_decl;

            if let Some(self_modifier) = func_decl.params.get_self_modifier() {
                if !self_modifier.kind.is_referenced() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InterfaceMethodsMustUseReferencedSelf),
                        loc: Some(self_modifier.loc),
                        hint: None,
                    });
                }
            } else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InterfaceMethodsMustHaveSelfModifier),
                    loc: Some(func_decl.loc),
                    hint: None,
                });
            }

            // skip the self param (index 0)
            let params = &func_decl.params.list[1..];

            for param_kind in params {
                let ty = param_kind.param_type();

                if ty.contains_self_type() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InterfaceMethodParamContainsSelf {
                            interface_name: interface_name.clone(),
                            method_name: func_decl.name.clone(),
                        }),
                        loc: Some(param_kind.loc()),
                        hint: None,
                    });
                }
            }

            if func_decl.ret_type.contains_self_type() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InterfaceMethodReturnTypeContainsSelf {
                        interface_name: interface_name.clone(),
                        method_name: func_decl.name.clone(),
                    }),
                    loc: Some(func_decl.loc),
                    hint: None,
                });
            }

            if methods.contains(&func_decl.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InterfaceDuplicateMethod {
                        interface_name: interface_name.clone(),
                        method_name: func_decl.name.clone(),
                    }),
                    loc: Some(func_decl.loc),
                    hint: None,
                });
                continue;
            }

            methods.push(func_decl.name.clone());
        }
    }

    /// Validates that an object correctly implements its declared interfaces.
    ///
    /// Ensures the referenced symbols are valid interfaces, checks visibility
    /// rules, resolves generic interface instantiations, and verifies that all
    /// required interface methods are implemented with matching signatures.
    pub(crate) fn analyze_object_implements_interfaces(
        &mut self,
        object_name: &String,
        impls: &Vec<TypedImplementInterface>,
        method_decls: &MethodDecls,
    ) {
        for implement_interface in impls {
            let Some(normalized_type) =
                self.normalize_and_check_type_formation(implement_interface.ty.clone(), implement_interface.loc)
            else {
                continue;
            };

            let sema_type = self.expand_sema_type(normalized_type, implement_interface.loc);

            if self
                .check_type_arity(sema_type.clone(), implement_interface.loc)
                .is_none()
            {
                continue;
            }

            let interface_decl_id = sema_type.as_interface();

            let interface_decl = match interface_decl_id {
                Some(id) => self.decl_tables.interface_decl(id),
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::NonInterfaceSymbol {
                            symbol_name: format_sema_type(sema_type, self.formatter),
                        }),
                        loc: Some(implement_interface.loc),
                        hint: None,
                    });
                    continue;
                }
            };

            for (_, method_decl_id) in interface_decl.methods.iter() {
                let interface_method_decl = self.decl_tables.method_decl(*method_decl_id);
                let func_decl = interface_method_decl.func_decl;

                if !method_decls.contains(&func_decl.name) {
                    // method missing
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::MissingInterfaceMethodImpl {
                            object_name: object_name.clone(),
                            method_name: func_decl.name.clone(),
                            interface_name: interface_decl.name.clone(),
                        }),
                        loc: Some(implement_interface.loc),
                        hint: None,
                    });
                    continue;
                }

                let method_decl_id = method_decls.get(&func_decl.name).unwrap();
                let method_decl = self.decl_tables.method_decl(method_decl_id);

                // check method declaration mismatch
                if method_decl.func_decl != func_decl {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InterfaceMethodTypeMismatch {
                            object_name: object_name.clone(),
                            method_name: func_decl.name.clone(),
                            interface_name: interface_decl.name.clone(),
                        }),
                        loc: Some(func_decl.loc),
                        hint: None,
                    });
                }
            }
        }
    }
}
