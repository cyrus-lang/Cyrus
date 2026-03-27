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
use cyrusc_ast::{ASTImportStmt, ModuleSegmentSingle, ProgramTree, abi::Visibility};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::{
    module_loader::ModuleAlias,
    symbols::{
        symbols::{SymbolEntry, SymbolEntryKind},
        table::Query,
    },
};
use cyrusc_source_loc::{FileID, Loc, SourceMap};
use cyrusc_typed_ast::{SymbolID, TypedProgramTree};
use std::{
    cell::RefCell,
    collections::HashSet,
    path::{Path, PathBuf},
    rc::Rc,
    sync::Arc,
};

// Track imported module + alias
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ImportedModuleEntry {
    file_id: FileID,
    alias: ModuleAlias,
}

// Used to check import cycles.
pub struct VisitingModule {
    // Stack of modules currently being resolved.
    pub active: HashSet<FileID>,
    // Modules fully resolved.
    pub done: HashSet<FileID>,
}

impl VisitingModule {
    pub fn active_paths_str(&self, source_map: Arc<SourceMap>) -> String {
        self.active
            .iter()
            .map(|file_id| {
                let source_file = source_map.get_file(*file_id).unwrap();
                source_file.file_path.to_str().unwrap().to_string()
            })
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
        module_symbol_id: SymbolID,
        ast: &ProgramTree,
        mut visiting: &mut VisitingModule,
        file_id: FileID,
        is_master: bool,
    ) -> Option<Rc<RefCell<TypedProgramTree>>> {
        if is_master {
            visiting.active.insert(file_id);
        }

        {
            let mut analyzed = self.analyzed_files.lock().unwrap();
            if analyzed.contains(&file_id) {
                return None;
            }
            analyzed.insert(file_id);
        }

        // module scope becomes current scope
        let previous_scope = self.current_scope;
        self.current_scope = Some(module_symbol_id);

        // current module file
        let prev_file = self.current_module_file_id;
        self.current_module_file_id = Some(file_id);

        // collect symbol names (first pass)
        self.resolve_decl_names(&ast);

        // analyze `import statements` of this module
        for import in ast.import_stmts() {
            self.resolve_import(self.current_scope.unwrap(), import, &mut visiting);
        }

        // collect full definitions and details of the symbols (second pass)
        let body = self.resolve_decl_full(&ast);

        let program_tree = Rc::new(RefCell::new(TypedProgramTree { file_id, body }));

        if is_master {
            let mut program_trees = self.program_trees.lock().unwrap();
            program_trees.push(Rc::new(ResolvedProgramTree {
                file_id,
                program_tree: program_tree.clone(),
            }));
            drop(program_trees);
        }

        // restore scope
        self.current_scope = previous_scope;

        // restore module file id
        self.current_module_file_id = prev_file;

        visiting.active.remove(&file_id);
        visiting.done.insert(file_id);

        Some(program_tree)
    }

    /// Resolves a module import with duplicate and cycle detection.
    fn resolve_import(&mut self, parent_scope_id: SymbolID, import: ASTImportStmt, visiting: &mut VisitingModule) {
        let current_file = self.current_module_file_id.unwrap();
        let loaded_modules_list = self.module_loader.load_module(&import);

        for loaded_module_result in loaded_modules_list {
            let loaded_module = match loaded_module_result {
                Ok(loaded_module) => loaded_module,
                Err(diag_kind) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: diag_kind,
                        loc: Some(import.loc),
                        hint: None,
                    });

                    continue;
                }
            };

            // store module name and generate symbol id in global_symbols for it
            let module_symbol_id = {
                let module_file_path = self
                    .source_map
                    .get_file(loaded_module.file_id)
                    .unwrap()
                    .file_path
                    .clone();

                let module_name = self
                    .module_loader
                    .module_name_from_file_path(Path::new(&module_file_path));

                self.insert_module_name(loaded_module.file_id, module_name.to_string());

                self.get_or_create_module_symbol_id(loaded_module.file_id, &module_name, import.loc)
            };

            // check for self-import
            if current_file == loaded_module.file_id {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::ModuleCannotImportItself),
                    loc: Some(import.loc),
                    hint: None,
                });
                continue;
            }

            // cycle detection
            if visiting.active.contains(&loaded_module.file_id) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::ImportCycle {
                        module_names: visiting.active_paths_str(self.source_map.clone()),
                    }),
                    loc: Some(import.loc),
                    hint: Some("Break the cycle by removing one import.".to_string()),
                });
                continue;
            }

            // resolve module if needed
            if !visiting.done.contains(&loaded_module.file_id) {
                visiting.active.insert(loaded_module.file_id);

                self.resolve_module(
                    module_symbol_id,
                    &loaded_module.program_tree,
                    visiting,
                    loaded_module.file_id,
                    false,
                );

                visiting.active.remove(&loaded_module.file_id);
                visiting.done.insert(loaded_module.file_id);
            }

            match loaded_module.alias {
                ModuleAlias::Group(alias) => {
                    let already_directly_imported =
                        self.with_scope_table(parent_scope_id, |scope_table| scope_table.lookup(&alias_name).is_some());

                    if already_directly_imported {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(ResolverDiagKind::ImportTwice {
                                module_name: alias_name.clone(),
                            }),
                            loc: Some(import.loc),
                            hint: Some("Consider removing the previous declaration.".to_string()),
                        });
                        continue;
                    }

                    let proxy_symbol_id = self.global_symbols.alloc_symbol_entry(SymbolEntry::new(
                        SymbolEntryKind::ProxiedModule {
                            symbol_id: module_symbol_id,
                        },
                        None,
                    ));

                    self.global_symbols
                        .bind_symbol_name(parent_scope_id, proxy_symbol_id, &alias_name);
                }
                ModuleAlias::Single(module_segment_singles) => {
                    for single in module_segment_singles {
                        let visible_name = single.visible_name();

                        let already_directly_imported =
                            self.with_scope_table(parent_scope_id, |scope| scope.lookup(&visible_name).is_some());

                        if already_directly_imported {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(ResolverDiagKind::ImportTwice {
                                    module_name: visible_name.clone(),
                                }),
                                loc: Some(import.loc),
                                hint: Some("Consider removing the previous declaration.".to_string()),
                            });
                            continue;
                        }

                        // lookup symbol inside module scope
                        let target_symbol_id = self.with_scope_table(module_symbol_id, |scope| scope.lookup(&single.name));

                        let target_symbol_id = match target_symbol_id {
                            Some(id) => id,
                            None => {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(ResolverDiagKind::SymbolNotFound {
                                        name: single.name.clone(),
                                    }),
                                    loc: Some(import.loc),
                                    hint: None,
                                });
                                continue;
                            }
                        };

                        let proxy_symbol_id = self.global_symbols.alloc_symbol_entry(SymbolEntry::new(
                            SymbolEntryKind::ProxiedSymbol {
                                scope_id: module_symbol_id,
                                symbol_id: target_symbol_id,
                            },
                            Some(parent_scope_id),
                        ));

                        self.global_symbols
                            .bind_symbol_name(parent_scope_id, proxy_symbol_id, &visible_name);
                    }
                }
            }
        }
    }

    fn proxy_imported_scope_symbols_to_current_scope(
        &mut self,
        parent_scope_id: SymbolID,
        imported_scope_id: SymbolID,
        singles: &[ModuleSegmentSingle],
        loc: Loc,
    ) {
        let mut imported_symbol_ids = Vec::new();

        for single in singles {
            let actual_name = single.ident.as_string();
            let renamed_name = single.renamed.as_ref().unwrap_or(&single.ident).as_string();

            let Some(symbol_id) = self.lookup_symbol_id_in_scope(imported_scope_id, &actual_name) else {
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
                let symbol_entry = self.lookup_global_symbol(symbol_id).unwrap();

                if let Some(vis) = symbol_entry.vis_opt {
                    self.report_if_imported_private_symbol(actual_name.clone(), vis, loc);
                }
            }

            {
                let exists = self.lookup_symbol_id_in_scope(parent_scope_id, &actual_name).is_some();

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

                self.global_symbols.bind_symbol_proxy(
                    self.current_scope.unwrap(),
                    &renamed_name,
                    imported_scope_id,
                    symbol_id,
                );
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
