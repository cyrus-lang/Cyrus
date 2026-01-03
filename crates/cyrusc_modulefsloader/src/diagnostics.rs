// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use cyrusc_diagcentral::DiagKind;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ModuleFSLoaderDiagKind {
    #[error("Module '{module_name}' couldn't be found in any of the specified source directories.")]
    ModuleNotFound { module_name: String },

    #[error("Module '{module_name}' not found.")]
    ModuleImportNotFound { module_name: String },

    #[error(
        "Couldn't find stdlib anywhere. You can set it with 'CYRUS_STDLIB_PATH' environment variable or '--stdlib' command line argument."
    )]
    StdlibNotFound,

    #[error("Module directory '{module_name}' must contain an 'index.cyr' file for it to be importable.")]
    ModuleIndexNotFound { module_name: String },

    #[error("Module '{module_name}' cannot exist as both a file and a directory.")]
    DuplicateModule { module_name: String },
}

impl DiagKind for ModuleFSLoaderDiagKind {}
