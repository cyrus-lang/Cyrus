// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
use crate::diagnostics::ModuleFSLoaderDiagKind;
use cyrusc_ast::{
    Import, ModulePath, ModuleSegment, ModuleSegmentSingle, ProgramTree, format::module_segments_as_string,
};
use cyrusc_diagcentral::{Diag, DiagLevel, display_single_diag};
use cyrusc_fs_utils::find_file_from_sources;
use cyrusc_lexer::Lexer;
use cyrusc_parser::Parser;
use std::{
    env,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    rc::Rc,
};

mod diagnostics;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleAlias {
    Group(String),
    Single(Vec<ModuleSegmentSingle>),
}

#[derive(Debug)]
pub struct ModuleLoaderOptions {
    pub base_path: String,
    pub stdlib_path: Option<String>,
    pub source_dirs: Vec<String>,
}

#[derive(Debug)]
pub struct ModuleLoader {
    pub opts: ModuleLoaderOptions,
}

#[derive(Debug, Clone)]
pub struct LoadedModule {
    pub alias: ModuleAlias,
    pub path: ModulePath,
    pub file_path: String,
    pub program: Rc<ProgramTree>,
}

impl ModuleLoader {
    pub fn new(opts: ModuleLoaderOptions) -> Self {
        ModuleLoader { opts }
    }

    pub fn load_module(&mut self, import: &Import) -> Vec<Result<LoadedModule, ModuleFSLoaderDiagKind>> {
        let mut loaded_modules_list: Vec<Result<LoadedModule, ModuleFSLoaderDiagKind>> = Vec::new();

        for sub_import in &import.paths {
            let mut module_file_path = match self.get_imported_module_path(sub_import.segments.clone()) {
                Ok(path) => path,
                Err(diag_kind) => {
                    loaded_modules_list.push(Err(diag_kind));
                    continue;
                }
            };

            let path_buf = Path::new(&module_file_path);
            if path_buf.is_dir() {
                let index_path = path_buf.join("index.cyrus");
                if !index_path.exists() {
                    loaded_modules_list.push(Err(ModuleFSLoaderDiagKind::ModuleIndexNotFound {
                        module_name: module_segments_as_string(sub_import.segments.clone()),
                    }));
                    continue;
                }
                module_file_path = index_path.to_str().unwrap().to_string();
            }

            let file_content = match std::fs::read_to_string(&module_file_path) {
                Ok(content) => content,
                Err(_) => {
                    loaded_modules_list.push(Err(ModuleFSLoaderDiagKind::ModuleNotFound {
                        module_name: module_segments_as_string(sub_import.segments.clone()),
                    }));
                    continue;
                }
            };

            let mut lexer = Lexer::new(file_content, module_file_path.clone());
            let mut parser = Parser::new(lexer.tokenize(), module_file_path.clone());

            match parser.parse() {
                Ok(program_tree) => {
                    let program_tree_rc = Rc::new(ProgramTree {
                        body: Rc::clone(&program_tree.body),
                    });

                    let module_alias = match sub_import.segments.last().unwrap() {
                        ModuleSegment::SubModule(identifier) => {
                            ModuleAlias::Group(sub_import.alias.clone().unwrap_or(identifier.name.clone()))
                        }
                        ModuleSegment::Single(module_segment_singles) => {
                            ModuleAlias::Single(module_segment_singles.clone())
                        }
                    };

                    let loaded_module = LoadedModule {
                        alias: module_alias,
                        path: sub_import.clone(),
                        file_path: module_file_path.clone(),
                        program: program_tree_rc,
                    };

                    loaded_modules_list.push(Ok(loaded_module));
                }
                Err(errors) => {
                    parser.display_parser_errors(errors.clone());
                }
            }
        }

        loaded_modules_list
    }

    fn get_imported_module_path(&self, segments: Vec<ModuleSegment>) -> Result<String, ModuleFSLoaderDiagKind> {
        let mut sources = self.opts.source_dirs.clone();

        let mut segments = segments;
        if matches!(segments.first(), Some(ModuleSegment::SubModule(id)) if id.name == "std") {
            segments.remove(0);
            sources = vec![self.get_stdlib_modules_path()];
        }

        // starting point
        let module_file_path = String::new();
        self.load_module_segments(&segments, sources, module_file_path)
    }

    fn load_module_segments(
        &self,
        segments: &[ModuleSegment],
        sources: Vec<String>,
        mut module_file_path: String,
    ) -> Result<String, ModuleFSLoaderDiagKind> {
        let module_name = module_segments_as_string(segments.to_vec());

        for (idx, segment) in segments.iter().enumerate() {
            match segment {
                ModuleSegment::SubModule(identifier) => {
                    let file_path = format!("{}{}.cyrus", module_file_path, identifier.name);
                    let dir_path = format!("{}{}/", module_file_path, identifier.name);

                    let file_exists = if Path::new(&file_path).exists() {
                        Some(PathBuf::from(&file_path))
                    } else {
                        find_file_from_sources(&file_path, &sources)
                    };

                    let dir_exists = if Path::new(&dir_path).exists() {
                        Some(PathBuf::from(&dir_path))
                    } else {
                        find_file_from_sources(&dir_path, &sources)
                    };

                    match (file_exists, dir_exists) {
                        (Some(_), Some(_)) => {
                            return Err(ModuleFSLoaderDiagKind::DuplicateModule {
                                module_name: identifier.name.clone(),
                            });
                        }
                        (Some(file_buf), None) => {
                            module_file_path = file_buf.to_str().unwrap().to_string();
                            if idx == segments.len() - 1 {
                                return Ok(module_file_path);
                            }
                            // continue to next segment
                        }
                        (None, Some(dir_buf)) => {
                            module_file_path = dir_buf.to_str().unwrap().to_string();
                            if idx == segments.len() - 1 {
                                // last segment (require index.cyr)
                                let index_path = dir_buf.join("index.cyrus");
                                if !index_path.exists() {
                                    return Err(ModuleFSLoaderDiagKind::ModuleIndexNotFound {
                                        module_name: identifier.name.clone(),
                                    });
                                }
                                return Ok(index_path.to_str().unwrap().to_string());
                            } else {
                                // not last segment (descend into directory)
                                if !module_file_path.ends_with('/') {
                                    module_file_path.push('/');
                                }
                            }
                        }
                        (None, None) => return Err(ModuleFSLoaderDiagKind::ModuleNotFound { module_name }),
                    }
                }
                ModuleSegment::Single(_) => {
                    // single segment (return current path)
                    return Ok(module_file_path);
                }
            }
        }

        Ok(module_file_path)
    }

    fn get_stdlib_modules_path(&self) -> String {
        match self.opts.stdlib_path.clone() {
            Some(stdlib_path) => stdlib_path,
            None => match env::var("CYRUS_STDLIB_PATH") {
                Ok(stdlib_path) => stdlib_path,
                Err(_) => {
                    display_single_diag!(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ModuleFSLoaderDiagKind::StdlibNotFound),
                        location: None,
                        hint: None
                    });
                }
            },
        }
    }
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
