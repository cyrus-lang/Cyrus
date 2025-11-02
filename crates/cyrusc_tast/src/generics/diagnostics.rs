use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum GenericTypesDiagKind {
    #[error("Undefined generic param '{name}'.")]
    UndefinedGenericParam { name: String },

    #[error("No matching positional type argument found with index {idx}.")]
    UndefinedPositionalGenericParam { idx: usize },

    #[error("Generic type '{ty}' requires explicit type arguments.")]
    RequiresExplicitTypeArgs { ty: String },
}
