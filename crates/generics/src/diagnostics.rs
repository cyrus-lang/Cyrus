use std::fmt;

#[derive(Debug, Clone)]
pub enum GenericsDiagKind {
    UnexpectedTypeArgs,
    ExplicitTypeArgsRequired {
        type_name: String
    },
}

impl fmt::Display for GenericsDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenericsDiagKind::UnexpectedTypeArgs => {
                write!(f, "Type arguments supplied to a non-generic type.")
            }
            GenericsDiagKind::ExplicitTypeArgsRequired { type_name } => {
                write!(f, "Cannot infer all generic parameters for type '{}'.", type_name)
            }
        }
    }
}
