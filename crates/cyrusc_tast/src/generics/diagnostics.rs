// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
use cyrusc_diagcentral::DiagKind;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum GenericTypesDiagKind {
    #[error("Undefined generic param '{name}'.")]
    UndefinedGenericParam { name: String },

    #[error("No matching positional type argument found with index {idx}.")]
    UndefinedPositionalGenericParam { idx: usize },

    #[error("Generic type '{ty}' requires explicit type arguments.")]
    RequiresExplicitTypeArgs { ty: String },

    #[error("Cannot override generic param '{generic_param}'. It's already inferred as '{already_inferred_as}'.")]
    CannotOverrideParentInferredGenericParam {
        generic_param: String,
        already_inferred_as: String,
    },
}

impl DiagKind for GenericTypesDiagKind {}
