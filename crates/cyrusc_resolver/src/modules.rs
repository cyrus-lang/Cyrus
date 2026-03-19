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

use cyrusc_ast::ProgramTree;
use cyrusc_modulefsloader::ModuleAlias;
use cyrusc_tast::{ModuleID, TypedProgramTree};
use std::{cell::RefCell, collections::HashSet, path::PathBuf, rc::Rc};

use crate::Resolver;

// Track imported module + alias
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ImportedModuleEntry {
    module_file_path: PathBuf,
    alias: ModuleAlias,
}

// Used to check import cycles.
pub(crate) struct VisitingModule {
    // stack of modules currently being resolved
    pub active: HashSet<PathBuf>,
    // modules fully resolved
    pub done: HashSet<PathBuf>,
}

impl VisitingModule {
    pub fn active_paths_str(&self) -> String {
        self.active
            .iter()
            .map(|path_buf| path_buf.to_string_lossy().to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl Resolver {
    /// Entry point for resolving a module.
    ///
    /// This function performs a full two-phase resolution of a module:
    ///   1. **Declaration collection pass** – Collects symbol names (function/struct declarations, etc.)
    ///      and registers them in the global symbol table for this module.
    ///   2. **Definition resolution pass** – Resolves the actual definitions and types of the previously
    ///      declared symbols, producing a fully typed program tree.
    ///
    /// It also ensures that:
    ///   - Each module is only analyzed once (`analyzed_modules` set).
    ///   - Imports are recursively resolved and linked.
    ///   - The master (root) module is tracked and its typed tree stored in `program_trees`.
    ///   - Active/done sets in `Visiting` prevent circular imports and track resolution state.
    ///
    pub fn resolve_module(
        &mut self,
        module_id: ModuleID,
        ast: &ProgramTree,
        mut visiting: &mut VisitingModule,
        is_master: bool,
        module_file_path: PathBuf,
    ) -> Option<Rc<RefCell<TypedProgramTree>>> {
        self.current_module = Some(module_id);
        self.insert_imported_aliases_for_module();

        if is_master {
            self.insert_module_file_path(module_id, self.master_module_file_path.clone());
            visiting.active.insert(self.master_module_file_path.clone());
        }

        let mut analyzed = self.analyzed_modules.lock().unwrap();
        if analyzed.contains(&module_id) {
            return None;
        }
        analyzed.insert(module_id);
        drop(analyzed);

        // Initialize symbol table for this module
        let mut global_symbols = self.global_symbols.lock().unwrap();
        global_symbols.insert(module_id, SymbolTable::new());
        drop(global_symbols);

        // Collect symbol names (first pass).
        self.resolve_decl_names(module_id, &ast);

        let parent_module_id = module_id;

        // Analyze imports of this module
        for import in ast.import_stmts() {
            self.resolve_import(parent_module_id, import, &mut visiting);
        }

        self.current_module = Some(parent_module_id);

        // Collect exact definitions and details of the symbols (second pass).
        let typed_body = self.resolve_decl(module_id, &ast);

        let base_path = Path::new(&self.module_loader.opts.base_path);
        let stdlib_path = self.module_loader.opts.stdlib_path.clone().map(PathBuf::from);
        let module_name =
            make_module_name_from_filepath(module_file_path.clone(), Some(base_path), stdlib_path.as_deref());

        let typed_program_tree = Rc::new(RefCell::new(TypedProgramTree {
            module_name: module_name.clone(),
            body: typed_body,
            file_path: module_file_path.clone(),
            module_id,
        }));

        if is_master {
            let mut program_trees = self.program_trees.lock().unwrap();
            program_trees.push(Rc::new(ProgramTreeEntry {
                module_name,
                module_path: module_file_path.clone(),
                module_id: self.current_module.unwrap(),
                program: typed_program_tree.clone(),
            }));
            drop(program_trees);
        }

        visiting.active.remove(&module_file_path);
        visiting.done.insert(module_file_path);

        Some(typed_program_tree.clone())
    }

    fn skip_module_if_loaded_once(&self, file_path: PathBuf) -> bool {
        let file_paths = self.file_paths.lock().unwrap();
        let exists = file_paths.iter().find(|(_, fp)| **fp == file_path).is_some();
        drop(file_paths);
        exists
    }

    /// Resolves a module import with duplicate and cycle detection.
    fn resolve_import(&mut self, parent_module_id: ModuleID, import: Import, visiting: &mut VisitingModule) {
        let current_module_file_path = self.resolve_module_file_path(parent_module_id).unwrap();
        let loaded_modules_list = self.module_loader.load_module(&import);

        for loaded_module in loaded_modules_list {
            let loaded_module = match loaded_module {
                Ok(m) => m,
                Err((diag, loc)) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(diag),
                        location: Some(DiagLoc::new(SourceLoc::from_loc(loc, current_module_file_path.clone()))),
                        hint: None,
                    });
                    continue;
                }
            };

            // check for self-import
            if loaded_module.file_path == current_module_file_path {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::ModuleCannotImportItself),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        import.loc.clone(),
                        current_module_file_path.clone(),
                    ))),
                    hint: None,
                });
                continue;
            }

            let module_id = self
                .resolve_module_id_by_file_path(loaded_module.file_path.clone())
                .unwrap_or_else(generate_module_id);

            {
                let mut global_symbols = self.global_symbols.lock().unwrap();
                global_symbols.entry(module_id).or_insert_with(|| SymbolTable::new());
            }

            // check duplicates using module file + alias
            let import_key = ImportedModuleEntry {
                module_file_path: loaded_module.file_path.clone(),
                alias: loaded_module.alias.clone(),
            };
            let already_directly_imported = self.imported_modules.contains(&import_key);
            self.imported_modules.insert(import_key);

            // cycle detection
            if visiting.active.contains(&loaded_module.file_path) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::ImportCycle {
                        module_names: visiting.active_paths_str(),
                    }),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        import.loc.clone(),
                        current_module_file_path.clone(),
                    ))),
                    hint: Some("Break the cycle by removing one import.".to_string()),
                });
                continue;
            }

            visiting.active.insert(loaded_module.file_path.clone());

            // load symbols even if module was processed
            if visiting.done.contains(&loaded_module.file_path) {
                match loaded_module.alias {
                    ModuleAlias::Group(group_name) => {
                        self.insert_module_alias(parent_module_id, group_name, module_id);
                    }
                    ModuleAlias::Single(ref module_segment_singles) => {
                        self.load_module_import_singles(
                            parent_module_id,
                            module_id,
                            module_segment_singles,
                            import.loc.clone(),
                        );
                    }
                }
                visiting.active.remove(&loaded_module.file_path);
                continue;
            }

            if self.skip_module_if_loaded_once(loaded_module.file_path.clone()) {
                match loaded_module.alias {
                    ModuleAlias::Group(group_name) => {
                        self.insert_module_alias(parent_module_id, group_name, module_id);
                    }
                    ModuleAlias::Single(ref module_segment_singles) => {
                        self.load_module_import_singles(
                            parent_module_id,
                            module_id,
                            module_segment_singles,
                            import.loc.clone(),
                        );
                    }
                }
            } else {
                self.insert_module_file_path(module_id, loaded_module.file_path.clone());

                if let Some(typed_program_tree) = self.resolve_module(
                    module_id,
                    &loaded_module.program.as_ref(),
                    visiting,
                    false,
                    loaded_module.file_path.clone(),
                ) {
                    let module_file_path = self.current_file_path();
                    let mut program_trees = self.program_trees.lock().unwrap();

                    let base_path = Path::new(&self.module_loader.opts.base_path);
                    let stdlib_path = self.module_loader.opts.stdlib_path.clone().map(PathBuf::from);
                    let module_name = make_module_name_from_filepath(
                        module_file_path.clone(),
                        Some(base_path),
                        stdlib_path.as_deref(),
                    );

                    program_trees.push(Rc::new(ProgramTreeEntry {
                        module_name,
                        module_id,
                        program: typed_program_tree,
                        module_path: Path::new(&module_file_path).to_path_buf(),
                    }));
                    drop(program_trees);

                    match loaded_module.alias {
                        ModuleAlias::Group(group_name) => {
                            self.insert_module_alias(parent_module_id, group_name, module_id);
                        }
                        ModuleAlias::Single(ref module_segment_singles) => {
                            self.load_module_import_singles(
                                parent_module_id,
                                module_id,
                                module_segment_singles,
                                import.loc.clone(),
                            );
                        }
                    }
                }
            }

            visiting.active.remove(&loaded_module.file_path);
            visiting.done.insert(loaded_module.file_path);

            // warn only for exact duplicate (same module + same import type)
            if already_directly_imported {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::ImportTwice {
                        module_name: module_segments_as_string(loaded_module.path.segments.clone()),
                    }),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        import.loc.clone(),
                        current_module_file_path.clone(),
                    ))),
                    hint: Some("Consider removing the previous declaration.".to_string()),
                });
            }
        }
    }

    fn load_module_import_singles(
        &mut self,
        parent_module_id: ModuleID,
        imported_module_id: ModuleID,
        singles: &[ModuleSegmentSingle],
        loc: Location,
    ) {
        let mut imported_symbol_ids = Vec::new();

        for single in singles {
            let actual_name = single.ident.as_string();
            let renamed_name = single.renamed.as_ref().unwrap_or(&single.ident).as_string();

            let Some(symbol_id) = self.lookup_symbol_id(imported_module_id, &actual_name) else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::SymbolNotFound {
                        name: renamed_name.clone(),
                    }),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        loc.clone(),
                        self.resolve_module_file_path(parent_module_id).unwrap(),
                    ))),
                    hint: None,
                });
                continue;
            };

            imported_symbol_ids.push(symbol_id);

            {
                // check symbol visibility
                let symbol_entry = self.resolve_global_symbol(symbol_id).unwrap();
                let vis = symbol_entry.vis();
                self.check_import_single_vis(actual_name.clone(), vis, loc.clone());
            }

            {
                let mut global_symbols = self.global_symbols.lock().unwrap();
                let symbol_table = global_symbols
                    .get_mut(&parent_module_id)
                    .expect("Parent module should exist in global symbols.");

                if symbol_table.names.contains_key(&actual_name) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::DuplicateSymbol {
                            symbol_name: renamed_name.clone(),
                        }),
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            loc.clone(),
                            self.resolve_module_file_path(parent_module_id).unwrap(),
                        ))),
                        hint: None,
                    });
                    continue;
                }

                let proxy_symbol_id = generate_symbol_id();
                symbol_table.names.insert(renamed_name.clone(), proxy_symbol_id);
                symbol_table.entries.insert(
                    proxy_symbol_id,
                    SymbolEntry {
                        kind: SymbolEntryKind::ProxiedSymbol(parent_module_id, symbol_id),
                        used: false,
                    },
                );

                drop(global_symbols);
            }
        }
    }

    fn check_import_single_vis(&mut self, single_name: String, vis: Visibility, loc: Location) {
        if vis.is_private() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::ImportSinglePrivateSymbol {
                    symbol_name: single_name,
                }),
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.current_file_path()))),
                hint: None,
            });
        }
    }

    pub(crate) fn resolve_module_alias(&self, group_name: &ModuleGroupName) -> Option<ModuleID> {
        let module_aliases = self.module_aliases.lock().unwrap();
        let imported_modules = module_aliases.get(&self.current_module.unwrap()).unwrap();
        let option = imported_modules.get(group_name).cloned();
        drop(module_aliases);
        option
    }

    pub(crate) fn resolve_module_id_by_file_path(&self, module_file_path: PathBuf) -> Option<ModuleID> {
        let file_paths = self.file_paths.lock().unwrap();
        let module_id_opt = match file_paths.iter().find(|(_, fp)| **fp == module_file_path) {
            Some((module_id, _)) => Some(*module_id),
            None => None,
        };
        drop(file_paths);
        module_id_opt
    }

    pub(crate) fn insert_module_file_path(&self, module_id: ModuleID, module_file_path: PathBuf) {
        let mut file_paths = self.file_paths.lock().unwrap();
        file_paths.insert(module_id, module_file_path);
        drop(file_paths);
    }

    fn insert_imported_aliases_for_module(&mut self) {
        let mut module_aliases = self.module_aliases.lock().unwrap();
        module_aliases.insert(self.current_module.unwrap(), HashMap::new());
        drop(module_aliases);
    }

    pub(crate) fn resolve_module_file_path(&self, module_id: ModuleID) -> Option<String> {
        let file_paths = self.file_paths.lock().unwrap();
        let file_path = match file_paths.get(&module_id) {
            Some(module_file_path) => Some(module_file_path.clone()),
            None => None,
        };
        drop(file_paths);
        file_path.map(|path_buf| path_buf.to_string_lossy().to_string())
    }
}

impl VisitingModule {
    pub fn new() -> Self {
        Self {
            active: HashSet::new(),
            done: HashSet::new(),
        }
    }
}
