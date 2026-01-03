// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
use crate::{analyze::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc};

enum NamingConvDeclKind {
    Struct,
    Enum,
    Interface,
    Union,
}

impl<'a> AnalysisContext<'a> {
    fn check_name(&mut self, decl_kind: NamingConvDeclKind, name: &str, loc: SourceLoc, is_local: bool) {
        let valid = if is_local {
            is_camel_case(name)
        } else {
            is_pascal_case(name)
        };

        if !valid {
            let kind_str = match decl_kind {
                NamingConvDeclKind::Struct => "Struct",
                NamingConvDeclKind::Enum => "Enum",
                NamingConvDeclKind::Interface => "Interface",
                NamingConvDeclKind::Union => "Union",
            };

            self.report_nameconv_diag(kind_str.to_string(), name.to_string(), loc, is_local);
        }
    }

    pub(crate) fn check_struct_name(&mut self, name: String, loc: SourceLoc, is_local: bool) {
        self.check_name(NamingConvDeclKind::Struct, &name, loc, is_local);
    }

    pub(crate) fn check_enum_name(&mut self, name: String, loc: SourceLoc, is_local: bool) {
        self.check_name(NamingConvDeclKind::Enum, &name, loc, is_local);
    }

    pub(crate) fn check_union_name(&mut self, name: String, loc: SourceLoc, is_local: bool) {
        self.check_name(NamingConvDeclKind::Union, &name, loc, is_local);
    }

    pub(crate) fn check_interface_name(&mut self, name: String, loc: SourceLoc, is_local: bool) {
        self.check_name(NamingConvDeclKind::Interface, &name, loc, is_local);
    }

    pub(crate) fn check_method_name(&mut self, name: String, loc: SourceLoc) {
        if !self.disable_warnings {
            if !is_snake_case(&name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Warning,
                    kind: Box::new(AnalyzerDiagKind::NamingConv {
                        name,
                        kind: "Method".to_string(),
                        expected: "snake_case".to_string(),
                    }),
                    location: Some(DiagLoc::new(loc)),
                    hint: None,
                });
            }
        }
    }

    fn report_nameconv_diag(&mut self, kind: String, name: String, loc: SourceLoc, is_local: bool) {
        if !self.disable_warnings {
            let expected = if is_local { "camelCase" } else { "PascalCase" };

            self.reporter.report(Diag {
                level: DiagLevel::Warning,
                kind: Box::new(AnalyzerDiagKind::NamingConv {
                    kind,
                    name,
                    expected: expected.to_string(),
                }),
                location: Some(DiagLoc::new(loc)),
                hint: None,
            });
        }
    }
}

fn is_pascal_case(s: &str) -> bool {
    !s.is_empty() && s.chars().next().unwrap().is_uppercase() && !s.contains('_')
}

fn is_camel_case(s: &str) -> bool {
    !s.is_empty() && s.chars().next().unwrap().is_lowercase() && !s.contains('_')
}

fn is_snake_case(s: &str) -> bool {
    !s.is_empty()
        && s.chars().next().unwrap().is_lowercase()
        && s.chars().all(|c| c.is_lowercase() || c.is_ascii_digit() || c == '_')
        && !s.starts_with('_')
        && !s.ends_with('_')
        && !s.contains("__")
}
