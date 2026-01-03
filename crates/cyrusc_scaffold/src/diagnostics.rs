// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use cyrusc_diagcentral::DiagKind;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ProjectLayoutDiagKind {
    DuplicateProjectName { name: String },
}

impl fmt::Display for ProjectLayoutDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProjectLayoutDiagKind::DuplicateProjectName { name } => {
                write!(f, "A project named '{}' already exists in this location.", name)
            }
        }
    }
}

impl DiagKind for ProjectLayoutDiagKind {}
