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
    module_loader::{LoadedModule, ModuleAlias},
    symbols::table::Query,
};
use cyrusc_source_loc::{FileID, Loc, SourceMap};
use cyrusc_typed_ast::{SymbolID, TypedProgramTree};
use std::{cell::RefCell, collections::HashSet, rc::Rc, sync::Arc};

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
        let prev_scope = self.current_scope;
        let concrete_scope = self.global_symbols.resolve_concrete_scope_id(module_symbol_id);
        self.current_scope = Some(concrete_scope);

        // current module file
        let prev_file = self.current_module_file_id;
        self.current_module_file_id = Some(file_id);

        // collect symbol names (first pass)
        self.resolve_decl_names(&ast);

        // analyze `import statements` of this module
        for import in ast.import_stmts() {
            self.resolve_import(module_symbol_id, import, &mut visiting);
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
        self.current_scope = prev_scope;

        // restore module file id
        self.current_module_file_id = prev_file;

        visiting.active.remove(&file_id);
        visiting.done.insert(file_id);

        Some(program_tree)
    }

    /// Resolves a module import with duplicate and cycle detection.
    fn resolve_import(&mut self, parent_scope_id: SymbolID, import: ASTImportStmt, visiting: &mut VisitingModule) {
        let parent_scope_id = self.global_symbols.resolve_concrete_scope_id(parent_scope_id);

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

            let module_symbol_id =
                self.create_module_symbol_id_for_loaded_module(parent_scope_id, &loaded_module, import.loc);

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
                    // check if alias name is already taken in current scope
                    if self.lookup_symbol_id_in_scope(parent_scope_id, &alias).is_some() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(ResolverDiagKind::ImportTwice {
                                module_name: alias.clone(),
                            }),
                            loc: Some(import.loc),
                            hint: Some("Consider removing the previous declaration.".to_string()),
                        });
                        continue;
                    }

                    // insert proxied module symbol
                    self.global_symbols
                        .insert_proxied_module(parent_scope_id, &alias, module_symbol_id);
                }
                ModuleAlias::Single(singles) => {
                    self.resolve_import_module_segments(parent_scope_id, module_symbol_id, &singles);
                }
            }
        }
    }

    fn resolve_import_module_segments(
        &mut self,
        // where the proxy goes
        target_scope_id: SymbolID,
        // the module we are looking inside
        source_module_id: SymbolID,
        singles: &[ModuleSegmentSingle],
    ) {
        for single in singles {
            let loc = single.ident.loc;
            let actual_name = single.ident.as_string();
            let visible_name = single.visible_name();

            // look up the actual symbol inside the source module's scope
            let Some(target_symbol_id) = self.lookup_symbol_id_in_scope(source_module_id, &actual_name) else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::SymbolNotFound {
                        name: actual_name.clone(),
                    }),
                    loc: Some(loc),
                    hint: Some(format!("Module does not export '{}'", actual_name)),
                });
                continue;
            };

            if let Some(symbol_entry) = self.get_symbol_entry(target_symbol_id) {
                if let Some(vis) = symbol_entry.vis_opt {
                    self.report_if_imported_private_symbol(actual_name.clone(), vis, loc);
                }
            }

            // check if the visible name exists in the local scope
            if self.lookup_symbol_id_in_scope(target_scope_id, &visible_name).is_some() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::DuplicateSymbol {
                        symbol_name: visible_name.clone(),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
                continue;
            }

            // insert proxied symbol
            self.global_symbols.insert_proxied_symbol(
                target_scope_id,
                &visible_name,
                source_module_id,
                target_symbol_id,
            );
        }
    }

    fn create_module_symbol_id_for_loaded_module(
        &mut self,
        parent_scope_id: SymbolID,
        loaded_module: &LoadedModule,
        loc: Loc,
    ) -> SymbolID {
        dbg!(self.global_symbols.inner.read().unwrap().entries.clone());

        let module_name = loaded_module.segment.as_string();

        let mut current_scope_id = parent_scope_id;
        let mut root_symbol_id = None;

        for implied_parent in &loaded_module.implied_parent_modules {
            let parent_name = &implied_parent.ident.value;

            let synthetic_id =
                self.get_or_create_synthetic_module_symbol(current_scope_id, parent_name, implied_parent.ident.loc);

            // 🩶 make a *real* nested module, not a proxy.
            self.global_symbols
                .insert_symbol_name(current_scope_id, synthetic_id, parent_name);

            // descend into its real internal scope
            current_scope_id = self.global_symbols.resolve_concrete_scope_id(synthetic_id);
        }

        // // build the implied parent chain (directories)
        // for implied_parent in &loaded_module.implied_parent_modules {
        //     let parent_name = &implied_parent.ident.value;

        //     // create or get the synthetic module for this directory level
        //     let synthetic_id =
        //         self.get_or_create_synthetic_module_symbol(current_scope_id, parent_name, implied_parent.ident.loc);

        //     self.global_symbols
        //         .insert_symbol_name(current_scope_id, synthetic_id, parent_name);

        //     if root_symbol_id.is_none() {
        //         root_symbol_id = Some(synthetic_id);
        //     }

        //     // move the cursor: the next segment belongs inside this parent's scope.
        //     current_scope_id = synthetic_id;
        // }

        let real_module_id = self.get_or_create_module_symbol_id_for_file(loaded_module.file_id, &module_name, loc);

        self.module_symbols.insert(loaded_module.file_id, real_module_id);

        self.global_symbols
            .insert_symbol_name(current_scope_id, real_module_id, &module_name);

        if root_symbol_id.is_none() {
            root_symbol_id = Some(real_module_id);
        }

        self.insert_module_name(loaded_module.file_id, module_name.to_string());

        root_symbol_id.unwrap()
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
