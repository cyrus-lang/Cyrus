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

use crate::{ResolvedProgramTree, Resolver, diagnostics::ResolverDiagKind};
use cyrusc_ast::{Import, ModuleSegmentSingle, ProgramTree, abi::Visibility, format::format_module_segments};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::{
    module_loader::ModuleAlias,
    symbols::{
        symbols::{SymbolEntry, SymbolEntryKind},
        table::GlobalSymbolQuery,
    },
};
use cyrusc_source_loc::Loc;
use cyrusc_tast::{ModuleID, SymbolID, TypedProgramTree};
use std::{
    cell::RefCell,
    collections::HashSet,
    path::{Path, PathBuf},
    rc::Rc,
};

// Track imported module + alias
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ImportedModuleEntry {
    module_file_path: PathBuf,
    alias: ModuleAlias,
}

// Used to check import cycles.
pub struct VisitingModule {
    // Stack of modules currently being resolved.
    pub active: HashSet<PathBuf>,
    // Modules fully resolved.
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
    /// Resolve a module and produce its typed program tree.
    ///
    /// Performs a two‑phase semantic resolution:
    ///
    /// - Declaration pass: Collects symbol names (functions, structs, etc.)
    ///   and registers them in the module's symbol table.
    /// - Definition pass:  Resolves symbol definitions and types,
    ///   producing the final typed representation of the module.
    ///
    /// Responsibilities:
    ///
    /// - Ensures each module is analyzed only once via `analyzed_modules`.
    /// - Recursively resolves and links imported modules.
    /// - Initializes the module's symbol table in the global registry.
    /// - Tracks module resolution state through `VisitingModule` to prevent circular imports.
    /// - Records the typed program tree of the master module in `program_trees`.
    pub fn resolve_module(
        &mut self,
        module_id: ModuleID,
        ast: &ProgramTree,
        mut visiting: &mut VisitingModule,
        is_master: bool,
        module_file_path: PathBuf,
    ) -> Option<Rc<RefCell<TypedProgramTree>>> {
        self.module_id = Some(module_id);

        if is_master {
            visiting.active.insert(self.master_module_file_path.clone());
            self.module_file_map
                .insert(module_id, self.master_module_file_path.clone());
        }

        let mut analyzed = self.analyzed_modules.lock().unwrap();
        if analyzed.contains(&module_id) {
            return None;
        }
        analyzed.insert(module_id);
        drop(analyzed);

        // collect symbol names (first pass)
        self.resolve_decl_names(&ast);

        let parent_module_id = module_id;

        // analyze `import statements` of this module
        for import in ast.import_stmts() {
            self.resolve_import(parent_module_id, import, &mut visiting);
        }

        self.module_id = Some(parent_module_id);

        // collect full definitions and details of the symbols (second pass)
        let typed_body = self.resolve_decl_full(&ast);

        let module_name = self
            .module_loader
            .module_name_from_file_path(Path::new(&module_file_path));

        let typed_program_tree = Rc::new(RefCell::new(TypedProgramTree {
            module_name: module_name.clone(),
            body: typed_body,
            file_path: module_file_path.clone(),
            module_id,
        }));

        if is_master {
            let mut program_trees = self.program_trees.lock().unwrap();
            program_trees.push(Rc::new(ResolvedProgramTree {
                module_name,
                module_path: module_file_path.clone(),
                module_id: self.module_id.unwrap(),
                program: typed_program_tree.clone(),
            }));
            drop(program_trees);
        }

        visiting.active.remove(&module_file_path);
        visiting.done.insert(module_file_path);

        Some(typed_program_tree.clone())
    }

    /// Resolves a module import with duplicate and cycle detection.
    fn resolve_import(&mut self, parent_module_id: ModuleID, import: Import, visiting: &mut VisitingModule) {
        let current_module_file_path = self.module_file_map.get(parent_module_id).unwrap();
        let loaded_modules_list = self.module_loader.load_module(&import);

        for loaded_module in loaded_modules_list {
            let Ok(loaded_module) = loaded_module else { continue };

            // check for self-import
            if loaded_module.file_path == current_module_file_path {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::ModuleCannotImportItself),
                    loc: Some(import.loc),
                    hint: None,
                });
                continue;
            }

            let module_id = self
                .module_file_map
                .get_file_path(&loaded_module.file_path)
                .unwrap_or(ModuleID::new());

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
                    loc: Some(import.loc),
                    hint: Some("Break the cycle by removing one import.".to_string()),
                });
                continue;
            }

            visiting.active.insert(loaded_module.file_path.clone());

            // load symbols even if module was processed
            if visiting.done.contains(&loaded_module.file_path) {
                match loaded_module.alias {
                    ModuleAlias::Group(group_name) => {
                        self.module_aliases
                            .insert_module_alias(parent_module_id, group_name, module_id);
                    }
                    ModuleAlias::Single(ref module_segment_singles) => {
                        self.load_module_import_singles(
                            parent_module_id,
                            module_id,
                            module_segment_singles,
                            import.loc,
                        );
                    }
                }
                visiting.active.remove(&loaded_module.file_path);
                continue;
            }

            if self.skip_module_if_loaded_once(loaded_module.file_path.clone()) {
                match loaded_module.alias {
                    ModuleAlias::Group(group_name) => {
                        self.module_aliases
                            .insert_module_alias(parent_module_id, group_name, module_id);
                    }
                    ModuleAlias::Single(ref module_segment_singles) => {
                        self.load_module_import_singles(
                            parent_module_id,
                            module_id,
                            module_segment_singles,
                            import.loc,
                        );
                    }
                }
            } else {
                self.module_file_map.insert(module_id, loaded_module.file_path.clone());

                if let Some(typed_program_tree) = self.resolve_module(
                    module_id,
                    &loaded_module.program.as_ref(),
                    visiting,
                    false,
                    loaded_module.file_path.clone(),
                ) {
                    let module_file_path_buf = self.current_module_file_path();
                    let module_file_path = Path::new(&module_file_path_buf);

                    let mut program_trees = self.program_trees.lock().unwrap();

                    let module_name = self.module_loader.module_name_from_file_path(module_file_path);

                    program_trees.push(Rc::new(ResolvedProgramTree {
                        module_name,
                        module_id,
                        program: typed_program_tree,
                        module_path: Path::new(&module_file_path).to_path_buf(),
                    }));
                    drop(program_trees);

                    match loaded_module.alias {
                        ModuleAlias::Group(group_name) => {
                            self.module_aliases
                                .insert_module_alias(parent_module_id, group_name, module_id);
                        }
                        ModuleAlias::Single(ref module_segment_singles) => {
                            self.load_module_import_singles(
                                parent_module_id,
                                module_id,
                                module_segment_singles,
                                import.loc,
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
                        module_name: format_module_segments(loaded_module.path.segments.clone()),
                    }),
                    loc: Some(import.loc),
                    hint: Some("Consider removing the previous declaration.".to_string()),
                });
            }
        }
    }

    fn skip_module_if_loaded_once(&self, file_path: PathBuf) -> bool {
        self.module_file_map.get_file_path(&file_path).is_some()
    }

    fn load_module_import_singles(
        &mut self,
        parent_module_id: ModuleID,
        imported_module_id: ModuleID,
        singles: &[ModuleSegmentSingle],
        loc: Loc,
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
                    loc: Some(loc),
                    hint: None,
                });
                continue;
            };

            imported_symbol_ids.push(symbol_id);

            {
                // check symbol visibility
                let symbol_entry = self.resolve_global_symbol(symbol_id).unwrap();
                let vis = symbol_entry.vis();
                self.report_if_imported_private_symbol(actual_name.clone(), vis, loc);
            }

            {
                // let mut global_symbols = self.global_symbols_registry.lock().unwrap();
                // let symbol_table = global_symbols
                //     .get_mut(&parent_module_id)
                //     .expect("Parent module should exist in global symbols.");

                let exists = self.lookup_symbol_id(parent_module_id, &actual_name).is_some();

                if exists {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::DuplicateSymbol {
                            symbol_name: renamed_name.clone(),
                        }),
                        loc: Some(loc),
                        hint: None,
                    });
                    continue;
                }

                let proxy_symbol_id = SymbolID::new();

                self.global_symbols_registry
                    .insert_symbol_name(parent_module_id, &renamed_name);

                self.global_symbols_registry.insert_symbol_entry(
                    parent_module_id,
                    proxy_symbol_id,
                    SymbolEntry {
                        kind: SymbolEntryKind::ProxiedSymbol(parent_module_id, symbol_id),
                        used: false,
                    },
                );

                // symbol_table.names.insert(renamed_name.clone(), proxy_symbol_id);
                // symbol_table.entries.insert(
                //     proxy_symbol_id,
                //     SymbolEntry {
                //         kind: SymbolEntryKind::ProxiedSymbol(parent_module_id, symbol_id),
                //         used: false,
                //     },
                // );

                // drop(global_symbols);
            }
        }
    }

    fn report_if_imported_private_symbol(&mut self, single_name: String, vis: Visibility, loc: Loc) {
        if vis.is_private() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::ImportSinglePrivateSymbol {
                    symbol_name: single_name,
                }),
                loc: Some(loc),
                hint: None,
            });
        }
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
