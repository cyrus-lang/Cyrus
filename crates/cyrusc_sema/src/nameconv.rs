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

use crate::{analyze::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;

enum NamingConvDeclKind {
    Struct,
    Enum,
    Interface,
    Union,
}

impl<'a, M: SymbolEntryMut> AnalysisContext<'a, M> {
    fn check_name(&mut self, decl_kind: NamingConvDeclKind, name: &str, loc: Loc) {
        let valid = is_pascal_case(name);

        if !valid {
            let kind_str = match decl_kind {
                NamingConvDeclKind::Struct => "Struct",
                NamingConvDeclKind::Enum => "Enum",
                NamingConvDeclKind::Interface => "Interface",
                NamingConvDeclKind::Union => "Union",
            };

            self.report_nameconv_diag(kind_str.to_string(), name.to_string(), loc);
        }
    }

    pub(crate) fn check_struct_name(&mut self, name: String, loc: Loc) {
        self.check_name(NamingConvDeclKind::Struct, &name, loc);
    }

    pub(crate) fn check_enum_name(&mut self, name: String, loc: Loc) {
        self.check_name(NamingConvDeclKind::Enum, &name, loc);
    }

    pub(crate) fn check_union_name(&mut self, name: String, loc: Loc) {
        self.check_name(NamingConvDeclKind::Union, &name, loc);
    }

    pub(crate) fn check_interface_name(&mut self, name: String, loc: Loc) {
        self.check_name(NamingConvDeclKind::Interface, &name, loc);
    }

    pub(crate) fn check_method_name(&mut self, name: String, loc: Loc) {
        if self.config.warnings.enabled {
            if !is_snake_case(&name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Warning,
                    kind: Box::new(AnalyzerDiagKind::NamingConv {
                        name,
                        kind: "Method".to_string(),
                        expected: "snake_case".to_string(),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
            }
        }
    }

    fn report_nameconv_diag(&mut self, kind: String, name: String, loc: Loc) {
        if self.config.warnings.enabled {
            self.reporter.report(Diag {
                level: DiagLevel::Warning,
                kind: Box::new(AnalyzerDiagKind::NamingConv {
                    kind,
                    name,
                    expected: "PascalCase".to_string(),
                }),
                loc: Some(loc),
                hint: None,
            });
        }
    }
}

fn is_pascal_case(s: &str) -> bool {
    !s.is_empty() && s.chars().next().unwrap().is_uppercase() && !s.contains('_')
}

fn is_snake_case(s: &str) -> bool {
    !s.is_empty()
        && s.chars().next().unwrap().is_lowercase()
        && s.chars().all(|c| c.is_lowercase() || c.is_ascii_digit() || c == '_')
        && !s.starts_with('_')
        && !s.ends_with('_')
        && !s.contains("__")
}
