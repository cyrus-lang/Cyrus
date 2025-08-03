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
    InvalidReturnType {
        expected: String,
        found: String,
    },
    UnexpectedSymbolType {
        name: String,
        expected: String,
        found: String,
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
                write!(f, "Symbol '{}' not found in current scope.", name)
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
        }
    }
}
