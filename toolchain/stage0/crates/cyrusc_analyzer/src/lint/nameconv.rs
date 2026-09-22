// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;

enum NamingConvDeclKind {
    Struct,
    Enum,
    Interface,
    Union,
}

impl<'a> AnalysisContext<'a> {
    pub(crate) fn nameconv_check_struct_name(&mut self, name: &str, loc: Loc) {
        self.nameconv_check_name(NamingConvDeclKind::Struct, name, loc);
    }

    pub(crate) fn nameconv_check_enum_name(&mut self, name: &str, loc: Loc) {
        self.nameconv_check_name(NamingConvDeclKind::Enum, name, loc);
    }

    pub(crate) fn nameconv_check_union_name(&mut self, name: &str, loc: Loc) {
        self.nameconv_check_name(NamingConvDeclKind::Union, name, loc);
    }

    pub(crate) fn nameconv_check_interface_name(&mut self, name: &str, loc: Loc) {
        self.nameconv_check_name(NamingConvDeclKind::Interface, name, loc);
    }

    pub(crate) fn nameconv_check_method_name(&mut self, name: &str, loc: Loc) {
        if self.config.warnings.enabled {
            if !is_snake_case(&name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Warning,
                    kind: Box::new(AnalyzerDiagKind::NamingConv {
                        name: name.to_string(),
                        kind: "Method".to_string(),
                        expected: "snake_case".to_string(),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
            }
        }
    }

    fn nameconv_check_name(&mut self, decl_kind: NamingConvDeclKind, name: &str, loc: Loc) {
        let valid = is_pascal_case(name);

        if !valid {
            let kind_str = match decl_kind {
                NamingConvDeclKind::Struct => "Struct",
                NamingConvDeclKind::Enum => "Enum",
                NamingConvDeclKind::Interface => "Interface",
                NamingConvDeclKind::Union => "Union",
            };

            self.report_nameconv_diag(kind_str.to_string(), name, loc);
        }
    }

    fn report_nameconv_diag(&mut self, kind: String, name: &str, loc: Loc) {
        if self.config.warnings.enabled {
            self.reporter.report(Diag {
                level: DiagLevel::Warning,
                kind: Box::new(AnalyzerDiagKind::NamingConv {
                    kind,
                    name: name.to_string(),
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
