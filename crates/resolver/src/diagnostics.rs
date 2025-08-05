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
    SymbolIsNotAFunction {
        name: String,
    },
    UselessTypeSpecifier,
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
            ResolverDiagKind::SymbolIsNotAFunction { name } => {
                write!(f, "Symbol '{}' is not a function.", name)
            }
            ResolverDiagKind::UselessTypeSpecifier => {
                write!(f, "What you wanna do with this type specifier?")
            }
        }
    }
}
