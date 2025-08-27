use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use ast::token::Location;
use diagcentral::{Diag, DiagLevel, DiagLoc};

enum NamingConvDeclKind {
    Struct,
    Enum,
    Typedef,
    Interface,
}

impl<'a> AnalysisContext<'a> {
    fn check_name(&mut self, decl_kind: NamingConvDeclKind, name: &str, loc: Location, is_local: bool) {
        let valid = if is_local {
            is_camel_case(name)
        } else {
            is_pascal_case(name)
        };

        if !valid {
            let kind_str = match decl_kind {
                NamingConvDeclKind::Struct => "Struct",
                NamingConvDeclKind::Enum => "Enum",
                NamingConvDeclKind::Typedef => "Typedef",
                NamingConvDeclKind::Interface => "Interface",
            };

            self.report_nameconv_diag(kind_str.to_string(), name.to_string(), loc, is_local);
        }
    }

    pub(crate) fn check_struct_name(&mut self, name: String, loc: Location, is_local: bool) {
        self.check_name(NamingConvDeclKind::Struct, &name, loc, is_local);
    }

    pub(crate) fn check_enum_name(&mut self, name: String, loc: Location, is_local: bool) {
        self.check_name(NamingConvDeclKind::Enum, &name, loc, is_local);
    }

    pub(crate) fn check_typedef_name(&mut self, name: String, loc: Location, is_local: bool) {
        self.check_name(NamingConvDeclKind::Typedef, &name, loc, is_local);
    }

    pub(crate) fn check_interface_name(&mut self, name: String, loc: Location, is_local: bool) {
        self.check_name(NamingConvDeclKind::Interface, &name, loc, is_local);
    }

    pub(crate) fn check_method_name(&mut self, name: String, loc: Location) {
        if !self.disable_warnings {
            if !is_snake_case(&name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Warning,
                    kind: AnalyzerDiagKind::NamingConv {
                        name,
                        kind: "Method".to_string(),
                        expected: "snake_case".to_string(),
                    },
                    location: Some(DiagLoc::new(self.resolver.get_current_module_file_path(), loc, 0)),
                    hint: None,
                });
            }
        }
    }

    fn report_nameconv_diag(&mut self, kind: String, name: String, loc: Location, is_local: bool) {
        if !self.disable_warnings {
            let expected = if is_local { "camelCase" } else { "PascalCase" };

            self.reporter.report(Diag {
                level: DiagLevel::Warning,
                kind: AnalyzerDiagKind::NamingConv {
                    kind,
                    name,
                    expected: expected.to_string(),
                },
                location: Some(DiagLoc::new(self.resolver.get_current_module_file_path(), loc, 0)),
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
