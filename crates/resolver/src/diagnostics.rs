use ast::token::Location;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ResolverDiagKind {
    SymbolAlreadyDefined {
        name: String,
        original: Location,
    },
    SymbolNotFound {
        name: String,
    },
    TypeNotFound {
        name: String,
    },
    FuncSignatureMismatch {
        name: String,
        expected: usize,
        found: usize,
    },
    InvalidSelfUsage,
    InvalidSelfModifier,
    InvalidReturnType {
        expected: String,
        found: String,
    },
    UnexpectedSymbolType {
        name: String,
        expected: String,
        found: String,
    },
    InvalidTopLevelStatement,
    InvalidArrayCapacity,
    InvalidUntypedFuncParam,
    RequiresLocalScope,
    InvalidOperandForFuncCall,
    InvalidOperandForMethodCall,
    SymbolIsNotAFunction {
        name: String,
    },
    UselessTypeSpecifier,
    ImportCycle {
        module_names: Vec<String>,
    },
    StdlibNotFound,
    InvalidRenameWhenImportingModule,
    CannotImportDirectoryAsModule,
    ModuleNotFound {
        module_name: String,
    },
    ModuleImportNotFound {
        module_name: String,
    },
    SymbolIsNotDefinedInModule {
        symbol_name: String,
        module_name: String,
    },
    ImportTwice {
        module_name: String,
    },
    DuplicateSymbol {
        symbol_name: String,
    },
    MethodNotDefined {
        struct_name: String,
        method_name: String,
    },
}

impl fmt::Display for ResolverDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolverDiagKind::SymbolAlreadyDefined { name, original } => {
                write!(
                    f,
                    "Symbol '{}' is already defined (first defined at {}).",
                    name, original
                )
            }
            ResolverDiagKind::SymbolNotFound { name } => {
                write!(f, "Symbol '{}' not found anywhere.", name)
            }
            ResolverDiagKind::FuncSignatureMismatch { name, expected, found } => {
                write!(
                    f,
                    "Function '{}' called with {} arguments, but {} expected.",
                    name, found, expected
                )
            }
            ResolverDiagKind::InvalidSelfUsage => {
                write!(f, "Self used outside of a instance method context.")
            }
            ResolverDiagKind::InvalidReturnType { expected, found } => {
                write!(
                    f,
                    "Invalid return type: expected '{}', but found '{}'.",
                    expected, found
                )
            }
            ResolverDiagKind::TypeNotFound { name } => {
                write!(f, "Type '{}' not found.", name)
            }
            ResolverDiagKind::UnexpectedSymbolType { name, expected, found } => {
                write!(
                    f,
                    "Unexpected symbol type for '{}': expected '{}', but found '{}'.",
                    name, expected, found
                )
            }
            ResolverDiagKind::InvalidTopLevelStatement => {
                write!(f, "Invalid top-level statement.")
            }
            ResolverDiagKind::InvalidArrayCapacity => {
                write!(f, "Array capacity must be a positive constant integer.")
            }
            ResolverDiagKind::InvalidUntypedFuncParam => {
                write!(f, "Function parameter must have a type.")
            }
            ResolverDiagKind::InvalidSelfModifier => {
                write!(f, "Self modifier must be the beginning parameter of a method.")
            }
            ResolverDiagKind::RequiresLocalScope => {
                write!(f, "This expression requires a local scope.")
            }
            ResolverDiagKind::InvalidOperandForFuncCall => {
                write!(f, "Invalid operand for function call.")
            }
            ResolverDiagKind::InvalidOperandForMethodCall => {
                write!(f, "Invalid operand for method call.")
            }
            ResolverDiagKind::SymbolIsNotAFunction { name } => {
                write!(f, "Symbol '{}' is not a function.", name)
            }
            ResolverDiagKind::UselessTypeSpecifier => {
                write!(f, "What you wanna do with this type specifier?")
            }
            ResolverDiagKind::ImportCycle { module_names } => {
                write!(
                    f,
                    "An import cycle was found, indicating a circular dependency between modules.\n\n"
                )?;
                write!(f, "Cycle Path: \n")?;
                for (i, module_name) in module_names.iter().enumerate() {
                    write!(f, "   {} {}\n", i + 1, module_name)?;
                }
                write!(f, "\n")?;
                write!(f, "Consider resolving the cycle by refactoring the modules.")
            }
            ResolverDiagKind::StdlibNotFound => {
                write!(
                    f,
                    "Could'nt find stdlib anywhere. You can set it with 'CYRUS_STDLIB_PATH' environment variable or '--stdlib' command line argument."
                )
            }
            ResolverDiagKind::InvalidRenameWhenImportingModule => {
                write!(
                    f,
                    "Cannot rename imported module when you considered to import singles."
                )
            }
            ResolverDiagKind::CannotImportDirectoryAsModule => {
                write!(f, "Cannot import directory as module.")
            }
            ResolverDiagKind::ModuleNotFound { module_name } => {
                write!(
                    f,
                    "Module '{}' couldn't be found in any of the specified source directories.",
                    module_name
                )
            }
            ResolverDiagKind::ModuleImportNotFound { module_name } => {
                write!(f, "Module '{}' not found.", module_name)
            }
            ResolverDiagKind::ImportTwice { module_name } => {
                write!(f, "Cannot import module '{}' twice.", module_name)
            }
            ResolverDiagKind::SymbolIsNotDefinedInModule {
                symbol_name,
                module_name,
            } => {
                write!(
                    f,
                    "Symbol '{}' is not defined in module '{}'.",
                    symbol_name, module_name
                )
            }
            ResolverDiagKind::DuplicateSymbol { symbol_name } => {
                write!(f, "Symbol '{}' has already been declared in this module.", symbol_name)
            }
            ResolverDiagKind::MethodNotDefined {
                struct_name,
                method_name,
            } => {
                write!(
                    f,
                    "Method '{}' is not defined for struct '{}'.",
                    method_name, struct_name
                )
            }
        }
    }
}
