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
use cyrusc_ast::{ASTImportStmt, ModuleSegment, ModuleSegmentSingle, ProgramTree, abi::Visibility};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::{
    module_loader::{LoadedModule, ModuleAlias},
    symbols::table::SymbolQuery,
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
        let mut paths: Vec<String> = self
            .active
            .iter()
            .map(|file_id| {
                source_map
                    .get_file(*file_id)
                    .unwrap()
                    .file_path
                    .to_string_lossy()
                    .to_string()
            })
            .collect();

        // show a cycle visually: A -> B -> A
        if let Some(first) = paths.first().cloned() {
            paths.push(first);
        }

        // build pretty chain formatting
        let mut out = String::new();
        for (i, p) in paths.iter().enumerate() {
            if i == 0 {
                out.push_str(p);
            } else {
                out.push_str(&format!("\n  - {}", p));
            }
        }

        out
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
        program_tree: &ProgramTree,
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
        self.resolve_decl_names(&program_tree.body);

        // analyze `import statements` of this module
        for import in program_tree.import_stmts() {
            self.resolve_import(module_symbol_id, import, &mut visiting);
        }

        // collect full definitions and details of the symbols (second pass)
        let body = self.resolve_decl_full(&program_tree);

        let typed_program_tree = Rc::new(RefCell::new(TypedProgramTree { file_id, body }));

        {
            let mut program_trees = self.program_trees.lock().unwrap();
            program_trees.push(Rc::new(ResolvedProgramTree {
                file_id,
                program_tree: typed_program_tree.clone(),
            }));
        }

        // restore scope
        self.current_scope = prev_scope;

        // restore module file id
        self.current_module_file_id = prev_file;

        visiting.active.remove(&file_id);
        visiting.done.insert(file_id);

        Some(typed_program_tree)
    }

    /// Resolves a module import with duplicate and cycle detection.
    fn resolve_import(&mut self, parent_scope_id: SymbolID, import: ASTImportStmt, visiting: &mut VisitingModule) {
        let parent_scope_id = self.global_symbols.resolve_concrete_scope_id(parent_scope_id);

        let current_file = self.current_module_file_id.unwrap();
        let loaded_modules_list = self.module_loader.load_module(&import, current_file);

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

            if let ModuleAlias::Group(alias) = &loaded_module.alias {
                if let Some(_) = self.lookup_symbol_id_in_scope(parent_scope_id, &alias) {
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
                visiting.done.insert(loaded_module.file_id);
            }

            // insert file module

            let mut module_symbol_id =
                self.get_or_create_module_symbol_id_for_loaded_module(parent_scope_id, &loaded_module);

            module_symbol_id = self.global_symbols.resolve_concrete_scope_id(module_symbol_id);

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

            // resolve namespace segments

            let segments = &loaded_module.path.segments;
            let consumed_segments = loaded_module.resolved_module_file.consumed_segments;

            let namespace_segments: Vec<&ModuleSegment> = segments[consumed_segments..]
                .iter()
                .filter(|seg| matches!(seg, ModuleSegment::SubModule(_)))
                .collect();

            if !namespace_segments.is_empty() {
                module_symbol_id =
                    self.resolve_namespace_segments_after_file(module_symbol_id, &namespace_segments, import.loc);
            }

            // insert alias

            match loaded_module.alias {
                ModuleAlias::Group(alias) => {
                    // insert proxied module symbol
                    self.global_symbols
                        .insert_proxied_module(parent_scope_id, &alias, module_symbol_id);
                }
                ModuleAlias::Single(singles) => {
                    self.resolve_import_single_symbols_from_module(parent_scope_id, module_symbol_id, &singles);
                }
            }
        }
    }

    /// Walks remaining path segments *after* filesystem resolution.
    /// These represent namespaces (inline modules) inside the loaded module file.
    ///
    /// Example:
    ///   import foo::bar::baz
    ///   filesystem resolved: foo/bar.cyrus
    ///   remaining segments = ["baz"]
    ///
    /// This will find the symbol `baz` *inside bar.cyrus* and return its module symbol ID.
    fn resolve_namespace_segments_after_file(
        &mut self,
        mut module_symbol_id: SymbolID,
        namespace_segments: &[&ModuleSegment],
        loc: Loc,
    ) -> SymbolID {
        for segment in namespace_segments {
            let name = segment.as_ident().unwrap().value;

            let Some(symbol_id) = self.lookup_symbol_id_in_scope(module_symbol_id, &name) else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::SymbolNotFound { name: name.clone() }),
                    loc: Some(loc),
                    hint: Some(format!(
                        "Declare it with `mod {} {{ ... }}` inside that module or import the correct path.",
                        name
                    )),
                });
                return module_symbol_id;
            };

            // ensure the symbol actually represents a namespace/module
            let scope_id = self.global_symbols.resolve_concrete_scope_id(symbol_id);

            module_symbol_id = scope_id;
        }

        module_symbol_id
    }

    fn resolve_import_single_symbols_from_module(
        &mut self,
        target_scope_id: SymbolID,         // where proxies are inserted
        source_module_symbol_id: SymbolID, // module we import from
        singles: &[ModuleSegmentSingle],
    ) {
        let source_scope_id = self.global_symbols.resolve_concrete_scope_id(source_module_symbol_id);

        for single in singles {
            let loc = single.ident.loc;

            let actual_name = single.ident.as_string();
            let visible_name = single.visible_name();

            // lookup symbol inside module scope
            let Some(target_symbol_id) = self.lookup_symbol_id_in_scope(source_scope_id, &actual_name) else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::SymbolNotFound {
                        name: actual_name.clone(),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
                continue;
            };

            // visibility check
            if let Some(symbol_entry) = self.lookup_symbol_entry(target_symbol_id) {
                if let Some(vis) = symbol_entry.vis_opt {
                    self.report_if_imported_private_symbol(actual_name.clone(), vis, loc);
                }
            }

            // duplicate check in target scope
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

            // insert proxy symbol
            self.global_symbols.insert_proxied_symbol(
                target_scope_id,
                &visible_name,
                source_module_symbol_id,
                target_symbol_id,
            );
        }
    }

    fn get_or_create_module_symbol_id_for_loaded_module(
        &mut self,
        parent_scope_id: SymbolID,
        loaded_module: &LoadedModule,
    ) -> SymbolID {
        let resolved_module_file = &loaded_module.resolved_module_file;

        // lookup in root_scope that this module has ever been registered or not
        let real_module_symbol_id;
        let root_scope_id = self.global_symbols.root_scope_id();
        let fs_module_name = self
            .module_loader
            .module_name_from_file_path(&loaded_module.resolved_module_file.file_path);

        if let Some(module_symbol_id) = self.lookup_symbol_id_in_scope(root_scope_id, &fs_module_name) {
            real_module_symbol_id = module_symbol_id; // use it
        } else {
            self.insert_module_name(loaded_module.file_id, &fs_module_name);

            // real modules always inserted to ROOT SCOPE!
            real_module_symbol_id = self.global_symbols.insert_module_symbol(root_scope_id, &fs_module_name);
        }

        // build directory modules as visual-modules that contains
        // descendent child, which proxies us to REAL imported modules.
        let directory_modules = &resolved_module_file.directory_modules;

        let mut current_scope_id = parent_scope_id;

        for dir in directory_modules {
            let dir_name = &dir.value;

            let dir_symbol_id = {
                if let Some(module_symbol_id) = self.lookup_symbol_id_in_scope(current_scope_id, dir_name) {
                    module_symbol_id
                } else {
                    // create virtual module
                    let virtual_module_symbol_id = self.get_or_create_virtual_module_symbol(current_scope_id, dir_name);

                    self.global_symbols
                        .insert_symbol_name(current_scope_id, virtual_module_symbol_id, dir_name);

                    virtual_module_symbol_id
                }
            };

            current_scope_id = self.global_symbols.resolve_concrete_scope_id(dir_symbol_id);
        }

        // insert virtual-module proxy to current scope to navigate into real_module_symbol_id
        let real_module_name = &loaded_module.resolved_module_file.file_module_name;
        self.global_symbols
            .insert_proxied_module(current_scope_id, real_module_name, real_module_symbol_id);

        return real_module_symbol_id;
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
