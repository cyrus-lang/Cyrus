use core::fmt;
use utils::tui::{tui_error, tui_warning};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagKind {
    NoEntryPointDetected,
    InvalidTypeToken,
    DerefNonPointerType,
    InfixNonBasic,
    NonInternalEntryPoint,
    UnimplementedFeature,
    InvalidTokenAsArrayCapacity,
    IdentifierNotDefined(String),
    TypeAnnotationRequired,
    UndefinedDataType(String),
    FuncNotFound(String),
    InvalidWildcard,
    ModuleNotFound(String),
    FuncCallArgumentCountMismatch(String, i32, i32),
    TypeAnnotationRequiredForParam(String, String),
    LenCalledWithInvalidInput,
    Custom(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagLevel {
    Error,
    Warning,
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
            DiagKind::TypeAnnotationRequiredForParam(param, func) => &format!(
                "Type annotation required for parameter '{}' in function '{}'.",
                param, func
            ),
            DiagKind::TypeAnnotationRequired => &format!("Type annotation required.",),
            DiagKind::InfixNonBasic => "Cannot build infix expression for non-basic value.",
            DiagKind::InvalidTokenAsArrayCapacity => "Invalid token given as array capacity.",
            DiagKind::IdentifierNotDefined(value) => &format!("The '{}' not found anywhere.", value),
            DiagKind::FuncNotFound(func_name) => &format!("Function '{}' not found in this module.", func_name),
            DiagKind::InvalidWildcard => "Wildcard cannot be used in the begging or middle of a module path.",
            DiagKind::ModuleNotFound(name) => &format!(
                "The module '{}' could not be found in any of the specified source directories.",
                name
            ),
            DiagKind::FuncCallArgumentCountMismatch(func_name, current, expected) => &format!(
                "Expected {} arguments for function '{}', but got {}.",
                expected, func_name, current
            ),
            DiagKind::LenCalledWithInvalidInput => "Cannot get length of non-string or non-array value.",
            DiagKind::UndefinedDataType(type_name) => {
                &format!("The data type '{}' is not defined in this module.", type_name)
            }
        };
        write!(f, "{}", msg)
    }
}

#[derive(Debug, Clone)]
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
                DiagLevel::Error => tui_error(self.fmt_diag(diag)),
                DiagLevel::Warning => tui_warning(self.fmt_diag(diag)),
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
}
