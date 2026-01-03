// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                                                         │
// │  Cyrus Programming Language                                             │
// │  https://github.com/cyrus-lang/Cyrus                                    │
// │                                                                         │
// │  A general-purpose, statically-typed, manually memory-managed           │
// │  programming language designed for performance-critical applications.   │
// │                                                                         │
// │  Copyright (c) 2026 The Cyrus Programming Language Project              │
// │                                                                         │
// │  This program is free software: you can redistribute it and/or modify   │
// │  it under the terms of the GNU General Public License as published by   │
// │  the Free Software Foundation, either version 3 of the License, or      │
// │  (at your option) any later version.                                    │
// │                                                                         │
// │  This program is distributed in the hope that it will be useful,        │
// │  but WITHOUT ANY WARRANTY; without even the implied warranty of         │
// │  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the           │
// │  GNU General Public License for more details.                           │
// │                                                                         │
// │  You should have received a copy of the GNU General Public License      │
// │  along with this program. If not, see <https://www.gnu.org/licenses/>.  │
// │                                                                         │
// └─────────────────────────────────────────────────────────────────────────┘

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
