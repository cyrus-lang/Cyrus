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
use cyrusc_typed_ast::stmts::TypedInterfaceStmt;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_interface(&mut self, interface: &TypedInterfaceStmt) {
        let interface_name = &interface.name;

        self.nameconv_check_interface_name(interface_name.clone(), interface.loc);

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
}
