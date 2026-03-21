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

use crate::{diagnostics::ResolverDiagKind, modules::ImportedModuleEntry};
use cyrusc_ast::abi::Visibility;
use cyrusc_ast::format::module_segments_as_string;
use cyrusc_ast::*;
use cyrusc_diagcentral::source_loc::SourceLoc;
use cyrusc_diagcentral::{reporter::DiagReporter, *};
use cyrusc_internal::module_loader::ModuleLoader;
use cyrusc_internal::symbols::table::{GlobalSymbolRegistry, SymbolTable};
use cyrusc_tast::generics::mapping_ctx_arena::GenericMappingCtxArena;
use cyrusc_tast::generics::monomorph::MonomorphRegistry;
use cyrusc_tast::stmts::*;
use cyrusc_tast::*;
use cyrusc_tokens::loc::{Location, Span};
use cyrusc_tokens::{Token, TokenKind};
use rand::Rng;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

mod diagnostics;
pub mod fs_module_loader;
pub mod macros;
pub mod modules;
pub(crate) mod query;
pub mod traverse;

type ModuleGroupName = String;
type ImportedModules = HashMap<ModuleGroupName, ModuleID>;

/// Thread-safe registry mapping modules to their symbol tables.
///
/// GlobalSymbolRegistry wraps an Arc<Mutex<_>> to allow concurrent access
/// across compilation units.
pub struct GlobalSymbolRegistry {
    inner: Arc<Mutex<HashMap<ModuleID, SymbolTable>>>,
}

/// Registry for module aliases within the current scope.
///
/// Manages mappings from `ModuleGroupName` to `ModuleID` for imported modules,
/// enabling alias resolution during semantic analysis.
pub struct ModuleAliasRegistry {
    inner: Arc<Mutex<HashMap<ModuleID, ImportedModules>>>,
}

/// Registry mapping modules to their source file paths.
///
/// This is used to associate a `ModuleID` with the file it originated from.
pub struct ModuleFileMap {
    inner: Arc<Mutex<HashMap<ModuleID, PathBuf>>>,
}

/// A guard that manages the lifetime and context of a LocalScope.
pub struct LocalScopeGuard<'a> {
    scope: LocalScope,
    resolver: &'a mut Resolver, // Assume Resolver manages the global context in which scopes reside.
}

/// Semantic resolver responsible for symbol binding, module loading,
/// and building the typed representation of the program.
pub struct Resolver<'diag> {
    /// Global symbol table shared across all modules.
    /// Stores all declared symbols and their associated metadata.
    pub global_symbols_registry: GlobalSymbolRegistry,

    /// Tracks modules that have already been analyzed.
    /// Prevents resolving the same module multiple times.
    pub analyzed_modules: Arc<Mutex<HashSet<ModuleID>>>,

    /// Program trees of successfully analyzed modules.
    /// Acts as the semantic output collected during resolution.
    pub program_trees: Arc<Mutex<Vec<Rc<ProgramTreeEntry>>>>,

    /// Maps module identifiers to their originating file paths.
    /// Used for diagnostics and module identity tracking.
    pub module_file_map: ModuleFileMap,

    /// Diagnostic reporter used to emit compiler errors and warnings.
    pub reporter: &'diag DiagReporter<'diag>,

    /// Module loader responsible for locating and parsing modules.
    /// Typically backed by a filesystem implementation.
    pub module_loader: Box<dyn ModuleLoader>,

    /// Registry storing generic monomorphization templates.
    ///
    /// The resolver registers untyped templates for generic entities here.
    /// Actual type checking and specialization are performed later by the analyzer.
    pub monomorph_registry: Arc<Mutex<MonomorphRegistry>>,

    /// Maps module aliases to their resolved imported modules.
    /// Used for resolving `import` references within a module.
    pub module_aliases: ModuleAliasRegistry,

    /// Arena managing generic mapping contexts.
    ///
    /// This is primarily required for constructing `GenericType` instances,
    /// which indirectly depend on a shared mapping context arena.
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,

    /// File path of the entry module driving the compilation.
    master_module_file_path: PathBuf,

    /// List of modules imported by the current module.
    /// Used during resolution to track dependency relationships.
    imported_modules: HashSet<ImportedModuleEntry>,

    /// Identifier of the module currently being resolved.
    module_id: Option<ModuleID>,

    /// Symbol representing the current object context (struct/trait/impl).
    /// Used to resolve `Self` and member references.
    current_object: Option<SymbolID>,

    /// Generic parameters currently in scope for the active object.
    /// Used when resolving generic type references.
    current_object_generic_params: Option<TypedGenericParamsList>,

    /// Stack of active `LocalScope`s representing the current nesting of lexical scopes
    ///
    /// This stack is manipulated internally by `LocalScopeGuard`, ensuring
    /// automatic scope entry and exit based on lexical lifetime.
    scopes: Vec<LocalScope>,
}

pub(crate) struct ProgramTreeEntry {
    pub module_id: ModuleID,
    pub module_name: String,
    pub module_path: PathBuf,
    pub program: Rc<RefCell<TypedProgramTree>>,
}

impl Resolver {
    pub fn new(
        opts: ModuleLoaderOptions,
        reporter: &'diag DiagReporter<'diag>,
        monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
        mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
        master_module_file_path: PathBuf,
    ) -> Self {
        let file_paths = Arc::new(Mutex::new(HashMap::new()));

        Self {
            global_symbols_registry: GlobalSymbolRegistry::new(),
            module_aliases: ModuleAliasRegistry::new(),
            analyzed_modules: Arc::new(Mutex::new(HashSet::new())),
            program_trees: Arc::new(Mutex::new(Vec::new())),
            module_loader: ModuleLoader::new(opts),
            module_file_map: ModuleFileMap::new(),
            imported_modules: HashSet::new(),
            scopes: Vec::new(),
            current_object_generic_params: None,
            module_id: None,
            current_object: None,
            master_module_file_path,
            monomorph_registry,
            mapping_ctx_arena,
            reporter,
        }
    }

    /// Returns a reference to the current active scope, if any.
    #[inline(always)]
    pub fn current_scope(&self) -> Option<&LocalScope> {
        self.scopes.last()
    }

    // Method to enter a new scope.
    pub fn enter_scope(&mut self, scope: LocalScope) {
        self.scopes.push(scope);
    }

    // Method to exit the current scope.
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    #[inline]
    pub fn current_file_path(&self) -> String {
        let current_module_id = self.module_id.unwrap();
        let file_paths = self.file_paths.lock().unwrap();
        let file_path = match file_paths.get(&current_module_id) {
            Some(child_module_file_path) => child_module_file_path.clone(),
            None => self.master_module_file_path.clone(),
        };
        drop(file_paths);
        file_path.to_string_lossy().to_string()
    }
}

impl GlobalSymbolRegistry {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Insert a symbol entry into the symbol table of the given module.
    ///
    /// Associates `symbol_id` with its corresponding `SymbolEntry`.
    /// The module must already have an initialized symbol table.
    pub fn insert_symbol_entry(&self, module_id: ModuleID, symbol_id: SymbolID, entry: SymbolEntry) {
        let mut registry = self.inner.lock().unwrap();

        let symbol_table = registry
            .get_mut(&module_id)
            .expect("symbol table not initialized for module");

        symbol_table.entries.insert(symbol_id, entry);
    }

    /// Insert a symbol name into the module's symbol table and allocate a new `SymbolID`.
    ///
    /// Returns the generated symbol identifier associated with the name.
    /// The module must already have an initialized symbol table.
    pub fn insert_symbol_name(&self, module_id: ModuleID, name: &str) -> SymbolID {
        let symbol_id = generate_symbol_id();

        let mut registry = self.inner.lock().unwrap();
        let symbol_table = registry
            .get_mut(&module_id)
            .expect("symbol table not initialized for module");

        symbol_table.names.insert(name.to_owned(), symbol_id);
        symbol_id
    }
}

impl ModuleAliasRegistry {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Insert an alias for an imported module into the parent module's registry.
    ///
    /// Associates `imported_module_id` with `group_name` for the specified `parent_module_id`.
    /// The parent module's entry must already exist.
    pub fn insert_module_alias(
        &self,
        parent_module_id: ModuleID,
        group_name: ModuleGroupName,
        imported_module_id: ModuleID,
    ) {
        let mut registry = self.inner.lock().unwrap();

        let imported_modules = registry
            .get_mut(&parent_module_id)
            .expect("parent module id not found in alias registry");

        imported_modules.insert(group_name, imported_module_id);
    }

    /// Resolve a module alias to its corresponding `ModuleID`.
    ///
    /// Looks up the `group_name` within the aliases of the `current_module`.
    /// Returns `None` if the alias is not found or if the current module
    /// has no associated aliases.
    pub fn resolve_module_alias(
        &self,
        current_module: Option<ModuleID>,
        group_name: &ModuleGroupName,
    ) -> Option<ModuleID> {
        let registry = self.inner.lock().unwrap();

        if let Some(current_module_id) = current_module {
            if let Some(imported_modules) = registry.get(&current_module_id) {
                return imported_modules.get(group_name).cloned();
            }
        }
        None
    }
}

impl ModuleFileMap {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Associate a module with its file path.
    ///
    /// If the module already exists in the map, the path will be replaced.
    pub fn insert(&self, module_id: ModuleID, path: PathBuf) {
        let mut map = self.inner.lock().unwrap();
        map.insert(module_id, path);
    }

    /// Retrieve the file path associated with a module.
    ///
    /// Returns `None` if the module is not registered.
    pub fn get(&self, module_id: ModuleID) -> Option<PathBuf> {
        let map = self.inner.lock().unwrap();
        map.get(&module_id).cloned()
    }

    /// Check whether a module already has a registered file path.
    pub fn contains(&self, module_id: ModuleID) -> bool {
        let map = self.inner.lock().unwrap();
        map.contains_key(&module_id)
    }

    /// Remove the file path entry for a module.
    pub fn remove(&self, module_id: ModuleID) {
        let mut map = self.inner.lock().unwrap();
        map.remove(&module_id);
    }
}

impl<'a> LocalScopeGuard<'a> {
    /// Constructs a new LocalScopeGuard, entering the new scope.
    pub fn new(resolver: &'a mut Resolver<'a>, parent: Option<ScopeID>) -> Self {
        let scope = LocalScope::new(parent);
        // Entering the new scope in the resolver.
        resolver.enter_scope(scope);
        Self { scope, resolver }
    }
}

impl<'a> Drop for LocalScopeGuard<'a> {
    fn drop(&mut self) {
        // exiting the scope when the guard goes out of scope
        self.resolver.exit_scope();
    }
}

unsafe impl Send for Resolver {}
unsafe impl Sync for Resolver {}
