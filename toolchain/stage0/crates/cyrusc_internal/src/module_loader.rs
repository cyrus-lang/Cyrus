// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_ast::{ASTImportStmt, Ident, ModulePath, ModuleSegmentSingle, ProgramTree};
use cyrusc_diagcentral::DiagKindClone;
use cyrusc_source_loc::FileID;
use std::{
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct LoadedModule {
    pub segment: Ident,
    pub alias: ModuleAlias,
    pub path: ModulePath,
    pub file_id: FileID,
    pub program_tree: Rc<ProgramTree>,
    pub resolved_module_file: ResolvedModuleFile,
}

#[derive(Debug, Clone)]
pub struct ResolvedModuleFile {
    pub file_path: PathBuf,

    // directories leading to the module file
    // foo/bar/baz.cyrus -> ["foo", "bar"]
    pub directory_modules: Vec<Ident>,

    // FIXME: Remove because unused
    // actual file module name
    // foo/bar/baz.cyrus -> "baz"
    pub file_module_name: String,

    // Used to compute symbol table for imported namespaces,
    // after module-path reaches a concrete cyrus file like:
    //
    // import foo::bar::baz;
    //
    // It resolves `foo/bar.cyrus`;
    // But then for the remaining `baz` which is a namespace,
    // resolve_import takes the responsibility to emit a symbol_table for it.
    pub consumed_segments: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleAlias {
    Group(String),
    Single(Vec<ModuleSegmentSingle>),
}

pub trait ModuleLoader {
    fn load_module(
        &mut self,
        import: &ASTImportStmt,
        current_module_file_id: FileID,
    ) -> Vec<Result<LoadedModule, Option<Box<dyn DiagKindClone>>>>;

    /// Forms a stable module name from a filesystem path.
    /// Strips extensions, normalizes separators, and prefixes stdlib modules.
    fn module_name_from_file_path(&mut self, path: &Path) -> String;
}

impl Hash for ModuleAlias {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ModuleAlias::Group(name) => {
                0u8.hash(state);
                name.hash(state);
            }
            ModuleAlias::Single(singles) => {
                1u8.hash(state);
                singles.hash(state);
            }
        }
    }
}
