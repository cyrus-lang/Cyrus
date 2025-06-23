use colorized::{Color, Colors};
use console::user_attended;
use core::fmt;
use std::fs;
use utils::purify_string::{saturating_sub, spaces};

const PANEL_LENGTH: usize = 4;

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
    SizeOfOperatorOnUnsizedObject,
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
            DiagKind::SizeOfOperatorOnUnsizedObject => {
                "Cannot determine complete sizeof with flexible member at compile time."
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
                DiagLevel::Error => eprintln!("{}", self.format_panel(diag)),
                DiagLevel::Warning => println!("{}", self.format_panel(diag)),
            }
        }
    }

    pub fn has_errors(&self) -> bool {
        self.diags.len() > 0
    }

    fn format_panel(&self, diag: &Diag) -> String {
        let mut formatted = String::new();

        let diag_level_text = match diag.level {
            DiagLevel::Error => "error".color(Colors::RedFg),
            DiagLevel::Warning => "warning".color(Colors::YellowFg),
        };

        formatted.push_str(&format!("{}: {}\n", diag_level_text, diag.kind.to_string()));

        if let Some(loc) = &diag.location {
            formatted.push_str(&format!(
                "       --> {}:{}:{}\n\n",
                loc.file.clone(),
                loc.line,
                loc.column
            ));

            let mut starting_line = saturating_sub(loc.line, PANEL_LENGTH);
            let source_content = fs::read_to_string(loc.file.clone()).unwrap();
            let sources_lines: Vec<&str> = source_content.split("\n").collect();

            while starting_line < loc.line + PANEL_LENGTH {
                if let Some(line_str) = sources_lines.get(starting_line) {
                    if starting_line + 1 == loc.line && user_attended() {
                        formatted.push_str(
                            &format!("{}{}  |  {}", spaces(2), starting_line + 1, line_str).color(Colors::RedFg),
                        );
                    } else {
                        formatted.push_str(&format!("{}{}  |  {}", spaces(2), starting_line + 1, line_str));
                    }
                } else {
                    break;
                }

                starting_line += 1;
                formatted.push_str("\n");
            }
        }

        // formatted.push_str(&format!("{}", diag.kind.to_string()));

        formatted
    }
}
