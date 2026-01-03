/* 
 * Copyright (c) 2026 The Cyrus Programming Language Project
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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
