use colorized::*;
use core::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagKind {
    NoEntryPointDetected,
    InvalidTypeToken,
    DerefNonPointerType,
    InfixNonBasic,
    NonInternalEntryPoint,
    UnimplementedFeature,
    InvalidTokenAsArrayCapacity,
    TypeAnnotationRequired(String, String),
    Custom(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagLevel {
    Error,
    Warning,
    Note,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagLoc {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diag {
    pub level: DiagLevel,
    pub kind: DiagKind,
    pub location: Option<DiagLoc>,
}

impl fmt::Display for DiagKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = match self {
            DiagKind::Custom(str) => str,
            DiagKind::InvalidTypeToken => "Invalid type token.",
            DiagKind::UnimplementedFeature => "Unimplemented.",
            DiagKind::DerefNonPointerType => "Cannot dereference a non-pointer type.",
            DiagKind::NoEntryPointDetected => "No entry point detected.",
            DiagKind::NonInternalEntryPoint => "Entry pont must be defined internally.",
            DiagKind::TypeAnnotationRequired(param, func) => &format!(
                "Type annotation required for parameter '{}' in function '{}'.",
                param, func
            ),
            DiagKind::InfixNonBasic => "Cannot build infix expression for non-basic value.",
            DiagKind::InvalidTokenAsArrayCapacity => "Invalid token given as array capacity.",
        };
        write!(f, "{}", msg)
    }
}

pub struct DiagReporter {
    diags: Vec<Diag>,
}

pub fn display_single_diag(diag: Diag) {
    let mut reporter = DiagReporter::new();
    reporter.report(diag);
    reporter.display_diags();
}

impl DiagReporter {
    pub fn new() -> Self {
        DiagReporter { diags: Vec::new() }
    }

    pub fn report(&mut self, diag: Diag) -> &mut Self {
        self.diags.push(diag);
        self
    }

    pub fn display_diags(&self) {
        for diag in &self.diags {
            match diag.level {
                DiagLevel::Error => eprintln!("{}: {}", "Error".color(Colors::RedFg), self.fmt_diag(diag)),
                DiagLevel::Warning => eprintln!("{}: {}", "Warning".color(Colors::YellowFg), self.fmt_diag(diag)),
                DiagLevel::Note => eprintln!("{}: {}", "Note".color(Colors::BlueFg), self.fmt_diag(diag)),
            }
        }
    }

    pub fn has_errors(&self) -> bool {
        self.diags.len() > 0
    }

    fn fmt_diag(&self, diag: &Diag) -> String {
        let mut formatted = String::new();
        if let Some(loc) = &diag.location {
            formatted.push_str(&format!("{}:{}:{}: ", loc.file, loc.line, loc.column));
        }
        formatted.push_str(&format!("{}", diag.kind.to_string()));
        formatted
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diag_reporter() {
        let mut reporter = DiagReporter::new();
        let diag = Diag {
            level: DiagLevel::Error,
            kind: DiagKind::InvalidTypeToken,
            location: Some(DiagLoc {
                file: "test.rs".to_string(),
                line: 10,
                column: 5,
                length: 1,
            }),
        };

        reporter.report(diag.clone());
        assert_eq!(reporter.diags.len(), 1);
        assert_eq!(reporter.diags[0], diag);
    }

    #[test]
    fn test_diag_reporter_display() {
        let mut reporter = DiagReporter::new();
        let diag = Diag {
            level: DiagLevel::Warning,
            kind: DiagKind::Custom("Test warning".to_string()),
            location: Some(DiagLoc {
                file: "test.rs".to_string(),
                line: 15,
                column: 10,
                length: 5,
            }),
        };

        reporter.report(diag);
        reporter.display_diags();
    }

    #[test]
    fn test_diag_loc() {
        let loc = DiagLoc {
            file: "example.rs".to_string(),
            line: 20,
            column: 15,
            length: 10,
        };

        assert_eq!(loc.file, "example.rs");
        assert_eq!(loc.line, 20);
        assert_eq!(loc.column, 15);
        assert_eq!(loc.length, 10);
    }

    #[test]
    fn test_diag_struct() {
        let diag = Diag {
            level: DiagLevel::Note,
            kind: DiagKind::UnimplementedFeature,
            location: None,
        };

        assert_eq!(diag.level, DiagLevel::Note);
        assert_eq!(diag.kind, DiagKind::UnimplementedFeature);
        assert!(diag.location.is_none());
    }
}
