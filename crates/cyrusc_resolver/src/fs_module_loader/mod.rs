/*
 * Copyright (c) 2026 The Cyrus Language
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

use crate::fs_module_loader::diagnostics::ModuleFSLoaderDiagKind;
use cyrusc_ast::format::format_module_segments;
use cyrusc_ast::{ASTImportStmt, ModulePath, ModuleSegment, ProgramTree};
use cyrusc_diagcentral::{Diag, DiagKindClone, DiagLevel, exit_with_single_diag};
use cyrusc_fs_utils::find_file_from_sources;
use cyrusc_internal::module_loader::{LoadedModule, ModuleAlias, ModuleLoader};
use cyrusc_parser::SourceParser;
use cyrusc_source_loc::{FileID, SourceMap};
use std::path::{Component, Path, PathBuf};
use std::{env, rc::Rc, sync::Arc};

mod diagnostics;

/// Options controlling how modules are located on disk.
/// Defines search roots for user code and optional standard library.
#[derive(Debug)]
pub struct FsModuleLoaderOptions {
    pub base_path: String,
    pub stdlib_path: Option<String>,
    pub source_dirs: Vec<String>,
}

/// Filesystem-backed module loader.
/// Resolves module paths, loads files, and delegates parsing to a SourceParser.
#[derive(Debug)]
pub struct FsModuleLoader {
    pub source_map: Arc<SourceMap>,
    source_parser: Arc<SourceParser>,
    opts: FsModuleLoaderOptions,
}

impl FsModuleLoader {
    pub fn new(source_map: Arc<SourceMap>, source_parser: Arc<SourceParser>, opts: FsModuleLoaderOptions) -> Self {
        Self {
            source_map,
            source_parser,
            opts,
        }
    }

    /// Computes the filesystem path for an imported module.
    /// Handles stdlib redirection and resolves full module paths.
    fn get_imported_module_file_path(&self, segments: Vec<ModuleSegment>) -> Result<String, ModuleFSLoaderDiagKind> {
        let stdlib_path = self.opts.stdlib_path.clone().map(|str| Path::new(&str).to_path_buf());
        let stdlib_modules_path = || {
            get_stdlib_modules_path(stdlib_path.as_ref())
                .to_str()
                .unwrap()
                .to_string()
        };

        let mut sources = self.opts.source_dirs.clone();

        let mut segments = segments;
        if matches!(segments.first(), Some(ModuleSegment::SubModule(id)) if id.value == "std") {
            segments.remove(0);
            sources = vec![stdlib_modules_path()];
        }

        // starting point
        let module_file_path = String::new();
        self.load_module_segments(&segments, sources, module_file_path)
    }

    /// Walks through module segments to locate either a `.cyrus` file or
    /// a directory containing `index.cyrus`. Reports missing or ambiguous modules.
    fn load_module_segments(
        &self,
        segments: &[ModuleSegment],
        sources: Vec<String>,
        initial_base_path: String,
    ) -> Result<String, ModuleFSLoaderDiagKind> {
        let mut current_path = PathBuf::from(initial_base_path);
        let total_segments = segments.len();

        for (i, segment) in segments.iter().enumerate() {
            let is_last = i == total_segments - 1;

            match segment {
                ModuleSegment::SubModule(ident) => {
                    let name = &ident.value;

                    // check file
                    let file_path_buf = current_path.join(format!("{}.cyrus", name));
                    let file_exists = if file_path_buf.exists() {
                        Some(file_path_buf.clone())
                    } else {
                        // fallback: search in additional source dirs
                        find_file_from_sources(file_path_buf.to_string_lossy().as_ref(), &sources)
                    };

                    // check dir
                    let dir_path_buf = current_path.join(name);
                    let dir_exists = if dir_path_buf.is_dir() {
                        Some(dir_path_buf.clone())
                    } else {
                        // fallback: search in additional source dirs
                        find_file_from_sources(dir_path_buf.to_string_lossy().as_ref(), &sources)
                    };

                    match (file_exists, dir_exists) {
                        // ambiguity check
                        (Some(_), Some(_)) => {
                            return Err(ModuleFSLoaderDiagKind::DuplicateModule {
                                module_name: name.clone(),
                            });
                        }
                        // it's a file
                        (Some(_), None) => {
                            if !is_last {
                                // can't have `foo::bar` if 'foo' is a file!
                                return Err(ModuleFSLoaderDiagKind::ModuleNotFound {
                                    module_name: name.clone(),
                                });
                            }
                            return Ok(file_path_buf.to_str().unwrap().to_string());
                        }
                        // it's a directory
                        (None, Some(dir_buf)) => {
                            // we must use `dir_buf` because `dir_path_buf`
                            // might not exist if the directory was found in the 'sources' folders!

                            if is_last {
                                let index_path = dir_buf.join("index.cyrus");

                                if !index_path.exists() {
                                    return Err(ModuleFSLoaderDiagKind::ModuleIndexNotFound {
                                        module_name: name.clone(),
                                    });
                                }

                                return Ok(index_path.to_string_lossy().to_string());
                            } else {
                                // descend into the directory
                                current_path = dir_buf;
                            }
                        }
                        (None, None) => {
                            return Err(ModuleFSLoaderDiagKind::ModuleNotFound {
                                module_name: format_module_segments(segments),
                            });
                        }
                    }
                }
                ModuleSegment::Single(_) => return Ok(current_path.to_str().unwrap().to_string()),
            }
        }

        Ok(current_path.to_str().unwrap().to_string())
    }
}

impl ModuleLoader for FsModuleLoader {
    /// Loads all modules referenced in an import statement.
    /// Phase 1: locate and parse each module.  
    /// Phase 2: if all succeeded, construct LoadedModule entries.
    fn load_module(&mut self, import: &ASTImportStmt) -> Vec<Result<LoadedModule, Box<dyn DiagKindClone>>> {
        type ImpliedParents = Vec<String>;

        // phase 1: collect and parse all modules
        let mut parsed_program_trees: Vec<(FileID, Rc<ProgramTree>, &ModulePath, ImpliedParents)> = Vec::new();
        let mut loaded_modules_list: Vec<Result<LoadedModule, Box<dyn DiagKindClone>>> = Vec::new();

        for sub_import in &import.paths {
            let mut implied_parents = Vec::new();

            let file_path = match self.get_imported_module_file_path(sub_import.segments.clone()) {
                Ok(path) => path,
                Err(diag) => {
                    loaded_modules_list.push(Err(Box::new(diag)));
                    continue;
                }
            };

            dbg!(file_path.clone());

            // handle directory imports -> index.cyrus
            let mut module_file_path = Path::new(&file_path).to_path_buf();

            dbg!(module_file_path.clone());
            if module_file_path.is_dir() {
                // extract implied parent segments (directory ancestors)
                implied_parents.extend(collect_directory_segments(&sub_import.segments));

                let index_path = module_file_path.join("index.cyrus");

                if !index_path.exists() {
                    loaded_modules_list.push(Err(Box::new(ModuleFSLoaderDiagKind::ModuleIndexNotFound {
                        module_name: format_module_segments(&sub_import.segments),
                    })));
                    continue;
                }

                module_file_path = index_path;
            }

            // verify file exists
            if std::fs::read_to_string(&module_file_path).is_err() {
                loaded_modules_list.push(Err(Box::new(ModuleFSLoaderDiagKind::ModuleNotFound {
                    module_name: format_module_segments(&sub_import.segments),
                })));
                continue;
            }

            // register file in SourceMap
            let file_id = self.source_map.add_file_by_loading(module_file_path.clone());

            let source_file = { self.source_map.get_file(file_id).unwrap().clone() };

            let Ok(program_tree) = self.source_parser.parse_program(&source_file) else {
                self.source_parser.display_errors();
                continue;
            };

            let program_tree_rc = Rc::new(ProgramTree {
                body: Rc::clone(&program_tree.body),
            });

            parsed_program_trees.push((file_id, program_tree_rc, sub_import, implied_parents));
        }

        // if any module failed parsing/path resolution, stop immediately
        if loaded_modules_list.iter().any(|result| result.is_err()) {
            return loaded_modules_list;
        }

        // phase 2: construct LoadedModule objects
        for (file_id, program_tree_rc, sub_import, implied_parents) in parsed_program_trees {
            let module_alias = match sub_import.segments.last().unwrap() {
                ModuleSegment::SubModule(ident) => {
                    ModuleAlias::Group(sub_import.alias.clone().unwrap_or(ident.value.clone()))
                }
                ModuleSegment::Single(module_segment_singles) => ModuleAlias::Single(module_segment_singles.clone()),
            };

            let loaded_module = LoadedModule {
                alias: module_alias,
                path: sub_import.clone(),
                program_tree: program_tree_rc,
                file_id,
                implied_parent_modules: implied_parents,
            };

            loaded_modules_list.push(Ok(loaded_module));
        }

        loaded_modules_list
    }

    fn module_name_from_file_path(&mut self, path: &Path) -> String {
        // canonicalize for consistent behavior
        let canonical_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());

        // canonical base path
        let base_path = Path::new(&self.opts.base_path)
            .canonicalize()
            .unwrap_or_else(|_| PathBuf::from(&self.opts.base_path));

        // compute relative path (fallback to canonical_path if unrelated)
        let relative = canonical_path
            .strip_prefix(&base_path)
            .unwrap_or(&canonical_path)
            .to_path_buf();

        // extract usable parts from the relative path
        let mut parts: Vec<String> = relative
            .components()
            .filter_map(|c| match c {
                Component::Normal(os) => os.to_str().map(|s| s.to_string()),
                _ => None,
            })
            .collect();

        // remove file extension from the last component
        if let Some(last) = parts.last_mut() {
            if let Some(stripped) = last.strip_suffix(".cyrus") {
                *last = stripped.to_string();
            } else if let Some((name, _ext)) = last.rsplit_once('.') {
                *last = name.to_string();
            }
        }

        // detect if this file is in stdlib
        let is_stdlib = self.opts.stdlib_path.as_ref().map_or(false, |path_str| {
            if let Ok(path_buf) = Path::new(path_str).canonicalize() {
                canonical_path.starts_with(path_buf)
            } else {
                false
            }
        });

        // construct module name
        let mut module_name = parts.join("_");

        // prefix stdlib modules
        if is_stdlib && !module_name.starts_with("stdlib_") {
            module_name = format!("stdlib_{}", module_name);
        }

        // remove leading underscores
        module_name = module_name.trim_start_matches('_').to_string();

        // sanitize to valid identifier
        module_name
            .chars()
            .map(|ch| {
                if ch.is_ascii_alphanumeric() || ch == '_' {
                    ch
                } else {
                    '_'
                }
            })
            .collect()
    }
}

fn collect_directory_segments(segments: &[ModuleSegment]) -> Vec<String> {
    // drop the last segment if it's a file (submodule)
    if segments.len() <= 1 {
        Vec::new()
    } else {
        segments[..segments.len() - 1]
            .iter()
            .map(|seg| seg.as_ident().unwrap().as_string())
            .collect()
    }
}

/// Resolves the active stdlib directory.
/// Uses explicit configuration first, then falls back to environment-variable(`CYRUS_STDLIB_PATH`).
fn get_stdlib_modules_path(stdlib_path: Option<&PathBuf>) -> PathBuf {
    match stdlib_path {
        Some(stdlib_path) => stdlib_path.to_path_buf(),
        None => match env::var("CYRUS_STDLIB_PATH") {
            Ok(stdlib_path) => Path::new(&stdlib_path).to_path_buf(),
            Err(_) => {
                exit_with_single_diag!(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ModuleFSLoaderDiagKind::StdlibNotFound),
                    loc: None,
                    hint: None
                });
            }
        },
    }
}
