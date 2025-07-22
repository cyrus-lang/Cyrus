use colorized::{Color, Colors};
use console::user_attended;
use core::fmt;
use std::fs;
use utils::escaping::{saturating_sub, spaces};

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
    ModuleNotFound(String),
    ModuleImportNotFound(String),
    FuncCallArgumentCountMismatch(String, i32, i32),
    MethodCallArgumentCountMismatch(String, i32, i32),
    TypeAnnotationRequiredForParam(String, String),
    MethodNotDefinedForStruct(String, String),
    SizeOfOperatorOnUnsizedObject,
    CannotUseModuleImportIfImportsSingles,
    FuncCallInvalidOperand,
    DuplicateNaming(String),
    SymbolNotFoundInModule(String, String),
    ImportingPrivateFunc(String),
    ImportingPrivateStruct(String),
    ImportingPrivateTypedef(String),
    InvalidStructAccessSpecifier,
    MethodCallOnNonStructValue,
    MethodIsStatic(String),
    MethodIsAnInstance(String),
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
            DiagKind::FuncNotFound(func_name) => &format!("Unable to resolve function '{}'.", func_name),
            DiagKind::ModuleNotFound(name) => &format!(
                "The module '{}' could not be found in any of the specified source directories.",
                name
            ),
            DiagKind::FuncCallArgumentCountMismatch(func_name, current, expected) => &format!(
                "Expected {} arguments for function '{}', but got {}.",
                expected, func_name, current
            ),
            DiagKind::MethodCallArgumentCountMismatch(func_name, current, expected) => &format!(
                "Expected {} arguments for method '{}', but got {}.",
                expected, func_name, current
            ),
            DiagKind::UndefinedDataType(type_name) => {
                &format!("The data type '{}' is not defined in this module.", type_name)
            }
            DiagKind::SizeOfOperatorOnUnsizedObject => {
                "Cannot determine complete sizeof with flexible member at compile time."
            }
            DiagKind::CannotUseModuleImportIfImportsSingles => "Cannot use module import if it imports singles.",
            DiagKind::FuncCallInvalidOperand => "Invalid operand for function call.",
            DiagKind::DuplicateNaming(name) => {
                &format!("Another object already declared with name '{}' in this module.", name)
            }
            DiagKind::SymbolNotFoundInModule(symbol, module_name) => {
                &format!("Symbol '{}' not found in module '{}'.", symbol, module_name)
            }
            DiagKind::ImportingPrivateFunc(func_name) => &format!("Cannot import private function '{}'.", func_name),
            DiagKind::ImportingPrivateStruct(struct_name) => {
                &format!("Cannot import private struct '{}'.", struct_name)
            }
            DiagKind::ImportingPrivateTypedef(typedef_name) => {
                &format!("Cannot import private typedef '{}'.", typedef_name)
            }
            DiagKind::InvalidStructAccessSpecifier => {
                "Structs must be declared with public or internal access specifier."
            }
            DiagKind::ModuleImportNotFound(module_name) => &format!("Module '{}' not found.", module_name),
            DiagKind::MethodCallOnNonStructValue => "Cannot build method call for non-struct values.",
            DiagKind::MethodNotDefinedForStruct(method_name, struct_name) => {
                &format!("Method '{}' not defined for struct '{}'.", method_name, struct_name)
            }
            DiagKind::MethodIsStatic(method_name) => {
                &format!("Method '{}' is static, cannot be called on an instance.", method_name)
            }
            DiagKind::MethodIsAnInstance(method_name) => {
                &format!("Method '{}' belongs to an instance, not the type itself.", method_name)
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

        formatted
    }
}
