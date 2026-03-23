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

use crate::modules::ImportedModuleEntry;
use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_internal::local_scope::LocalScope;
use cyrusc_internal::module_loader::ModuleLoader;
use cyrusc_internal::symbols::symbols::{
    ResolvedEnum, ResolvedFunc, ResolvedGlobalVar, ResolvedInterface, ResolvedMethod, ResolvedStruct, ResolvedTypedef,
    ResolvedUnion, ResolvedVar, SymbolEntry, SymbolEntryKind,
};
use cyrusc_internal::symbols::table::{SymbolEntryMut, SymbolQuery, SymbolTable};
use cyrusc_typed_ast::generics::mapping_ctx_arena::GenericMappingCtxArena;
use cyrusc_typed_ast::generics::monomorph::MonomorphRegistry;
use cyrusc_typed_ast::stmts::*;
use cyrusc_typed_ast::*;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

mod diagnostics;
pub mod fs_module_loader;
pub mod macros;
pub mod modules;
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

/// Semantic resolver responsible for symbol binding, module loading,
/// and building the typed representation of the program.
pub struct Resolver {
    /// Global symbol table shared across all modules.
    /// Stores all declared symbols and their associated metadata.
    pub global_symbols_registry: GlobalSymbolRegistry,

    /// Tracks modules that have already been analyzed.
    /// Prevents resolving the same module multiple times.
    pub analyzed_modules: Arc<Mutex<HashSet<ModuleID>>>,

    /// Program trees of successfully analyzed modules.
    /// Acts as the semantic output collected during resolution.
    pub program_trees: Arc<Mutex<Vec<Rc<ResolvedProgramTree>>>>,

    /// Maps module identifiers to their originating file paths.
    /// Used for diagnostics and module identity tracking.
    pub module_file_map: ModuleFileMap,

    /// Diagnostic reporter used to emit compiler errors and warnings.
    pub reporter: Arc<DiagReporter>,

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

pub struct ResolvedProgramTree {
    pub module_id: ModuleID,
    pub module_name: String,
    pub module_path: PathBuf,
    pub program: Rc<RefCell<TypedProgramTree>>,
}

impl Resolver {
    pub fn new(
        module_loader: Box<dyn ModuleLoader>,
        reporter: Arc<DiagReporter>,
        monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
        mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
        master_module_file_path: PathBuf,
    ) -> Self {
        Self {
            global_symbols_registry: GlobalSymbolRegistry::new(),
            module_aliases: ModuleAliasRegistry::new(),
            analyzed_modules: Arc::new(Mutex::new(HashSet::new())),
            program_trees: Arc::new(Mutex::new(Vec::new())),
            module_file_map: ModuleFileMap::new(),
            imported_modules: HashSet::new(),
            scopes: Vec::new(),
            module_loader,
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
    #[inline]
    pub fn current_scope(&self) -> Option<&LocalScope> {
        self.scopes.last()
    }

    /// Returns a mutable reference to the current active scope, if any.
    #[inline]
    pub fn current_scope_mut(&mut self) -> Option<&mut LocalScope> {
        self.scopes.last_mut()
    }

    // Method to enter a new scope.
    #[inline]
    pub fn enter_scope(&mut self, scope: LocalScope) {
        self.scopes.push(scope);
    }

    // Method to exit the current scope.
    #[inline]
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    /// Returns an iterator over the active scope stack.
    ///
    /// The iterator yields scopes from the innermost scope outward toward
    /// the outermost scope.
    pub fn scopes_into_iter(&self) -> impl Iterator<Item = &LocalScope> {
        self.scopes.iter().rev()
    }

    #[inline]
    pub fn current_module_file_path(&self) -> PathBuf {
        let module_id = self.module_id.unwrap();
        match self.module_file_map.get(module_id) {
            Some(path_buf) => path_buf,
            None => self.master_module_file_path.clone(),
        }
    }

    fn with_global_symbol_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut SymbolEntry) -> R,
    {
        let mut registry = self.global_symbols_registry.inner.lock().unwrap();

        for table in registry.values_mut() {
            if let Some(entry) = table.entries.get_mut(&symbol_id) {
                return Some(f(entry));
            }
        }

        None
    }

    /// Resolves a symbol by searching the active scope stack.
    ///
    /// Lookup proceeds from the innermost scope outward until a matching
    /// symbol binding is found.
    pub(crate) fn resolve_scope_symbol(&self, name: &str) -> Option<SymbolID> {
        for scope in self.scopes_into_iter() {
            if let Some(symbol_id) = scope.resolve(name) {
                return Some(symbol_id);
            }
        }

        None
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
    pub fn insert_symbol_entry(&self, module_id: ModuleID, symbol_id: SymbolID, symbol_entry: SymbolEntry) {
        let mut registry = self.inner.lock().unwrap();

        let symbol_table = registry.entry(module_id).or_insert_with(SymbolTable::new);

        symbol_table.entries.insert(symbol_id, symbol_entry);
    }

    /// Insert a symbol name into the module's symbol table and allocate a new `SymbolID`.
    ///
    /// Returns the generated symbol identifier associated with the name.
    pub fn insert_symbol_name(&self, module_id: ModuleID, name: &str) -> SymbolID {
        let symbol_id = SymbolID::new();

        let mut registry = self.inner.lock().unwrap();
        let symbol_table = registry.entry(module_id).or_insert_with(SymbolTable::new);

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

        let imported_modules = registry.entry(parent_module_id).or_insert_with(HashMap::new);

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
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Associate a module with its file path.
    ///
    /// If the module already exists in the map, the path will be replaced.
    #[inline]
    pub fn insert(&self, module_id: ModuleID, path: PathBuf) {
        let mut map = self.inner.lock().unwrap();
        map.insert(module_id, path);
    }

    /// Retrieve the file path associated with a module.
    ///
    /// Returns `None` if the module is not registered.
    #[inline]
    pub fn get(&self, module_id: ModuleID) -> Option<PathBuf> {
        let map = self.inner.lock().unwrap();
        map.get(&module_id).cloned()
    }

    /// Check and returns if file path already has a registered.
    #[inline]
    pub fn get_file_path(&self, file_path: &PathBuf) -> Option<ModuleID> {
        let map = self.inner.lock().unwrap();
        map.iter()
            .find(|(_, path_buf)| **path_buf == *file_path)
            .map(|(module_id, _)| *module_id)
    }

    /// Check whether a module already has a registered module id.
    #[inline]
    pub fn contains_module_id(&self, module_id: ModuleID) -> bool {
        self.get(module_id).is_some()
    }

    /// Check whether a file path already has a registered.
    #[inline]
    pub fn contains_file_path(&self, file_path: &PathBuf) -> bool {
        self.get_file_path(file_path).is_some()
    }

    /// Remove the file path entry for a module.
    #[inline]
    pub fn remove(&self, module_id: ModuleID) {
        let mut map = self.inner.lock().unwrap();
        map.remove(&module_id);
    }
}

impl SymbolQuery for Resolver {
    /// Resolve a symbol id to a variable within the given scope.
    fn lookup_var(&self, symbol_id: SymbolID) -> Option<ResolvedVar> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Var(resolved_var) => Some(resolved_var),
            _ => None,
        }
    }

    /// Resolve a symbol id to a method definition.
    fn lookup_method(&self, symbol_id: SymbolID) -> Option<ResolvedMethod> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Method(resolved_method) => Some(resolved_method),
            _ => None,
        }
    }

    /// Resolve a symbol id to a global function.
    fn lookup_func(&self, symbol_di: SymbolID) -> Option<ResolvedFunc> {
        match self.lookup_global_symbol(symbol_di)?.kind {
            SymbolEntryKind::Func(resolved_func) => Some(resolved_func),
            _ => None,
        }
    }

    /// Resolve a symbol id to a type definition (typedef).
    fn lookup_typedef(&self, symbol_id: SymbolID) -> Option<ResolvedTypedef> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Typedef(resolved_typedef) => Some(resolved_typedef),
            _ => None,
        }
    }

    /// Resolve a symbol id to a global variable.
    fn lookup_global_var(&self, symbol_id: SymbolID) -> Option<ResolvedGlobalVar> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::GlobalVar(resolved_global_var) => Some(resolved_global_var),
            _ => None,
        }
    }

    /// Resolve a symbol id to an interface/trait definition.
    fn lookup_interface(&self, symbol_id: SymbolID) -> Option<ResolvedInterface> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Interface(resolved_interface) => Some(resolved_interface),
            _ => None,
        }
    }

    /// Resolve a symbol id to a union definition.
    fn lookup_union(&self, symbol_id: SymbolID) -> Option<ResolvedUnion> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Union(resolved_union) => Some(resolved_union),
            _ => None,
        }
    }

    /// Resolve a symbol id to an enum definition.
    fn lookup_enum(&self, symbol_id: SymbolID) -> Option<ResolvedEnum> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Enum(resolved_enum) => Some(resolved_enum),
            _ => None,
        }
    }

    /// Resolve a symbol id to a struct definition.
    fn lookup_struct(&self, symbol_id: SymbolID) -> Option<ResolvedStruct> {
        match self.lookup_global_symbol(symbol_id)?.kind {
            SymbolEntryKind::Struct(resolved_struct) => Some(resolved_struct),
            _ => None,
        }
    }

    /// Look up a symbol identifier by name.
    fn lookup_symbol_id(&self, name: &str) -> Option<SymbolID> {
        let registry = self.global_symbols_registry.inner.lock().unwrap();

        for table in registry.values() {
            if let Some(symbol_id) = table.names.get(name) {
                return Some(*symbol_id);
            }
        }

        None
    }

    /// Look up a symbol identifier by name within a specific module.
    fn lookup_symbol_id_in_module(&self, module_id: ModuleID, name: &str) -> Option<SymbolID> {
        let registry = self.global_symbols_registry.inner.lock().unwrap();
        let table = registry.get(&module_id)?;
        table.names.get(name).copied()
    }

    /// Retrieve the full semantic entry for a symbol by its name.
    fn lookup_symbol_entry(&self, name: &str) -> Option<SymbolEntry> {
        let symbol_id = self.lookup_symbol_id(name)?;
        self.lookup_global_symbol(symbol_id)
    }

    /// Retrieve the semantic entry for a specific symbol identifier.
    fn lookup_global_symbol(&self, symbol_id: SymbolID) -> Option<SymbolEntry> {
        let registry = self.global_symbols_registry.inner.lock().unwrap();

        for table in registry.values() {
            if let Some(entry) = table.entries.get(&symbol_id) {
                return Some(entry.clone());
            }
        }

        None
    }

    /// Perform a deep resolution of a symbol, following aliases or redirects.
    fn lookup_global_symbol_deep(&self, symbol_id: SymbolID) -> Option<SymbolEntry> {
        let mut current_entry = self.lookup_global_symbol(symbol_id)?;

        // if the entry is an alias, resolve what it points to
        while let SymbolEntryKind::ProxiedSymbol(_, symbol_id) = &current_entry.kind {
            current_entry = self.lookup_global_symbol(*symbol_id)?;
        }

        Some(current_entry)
    }

    fn format_symbol_name(&self, symbol_id: SymbolID) -> String {
        match self.lookup_global_symbol(symbol_id) {
            Some(symbol_entry) => symbol_entry.decl_name(),
            None => "<UNRESOLVED_SYMBOL>".to_string(),
        }
    }
}

impl SymbolEntryMut for Resolver {
    fn with_var_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedVar) -> R,
    {
        self.with_global_symbol_mut(symbol_id, |entry| match &mut entry.kind {
            SymbolEntryKind::Var(resolved_var) => Some(f(resolved_var)),
            _ => None,
        })?
    }

    fn with_global_var_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedGlobalVar) -> R,
    {
        self.with_global_symbol_mut(symbol_id, |entry| match &mut entry.kind {
            SymbolEntryKind::GlobalVar(resolved_global_var) => Some(f(resolved_global_var)),
            _ => None,
        })?
    }

    fn with_method_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedMethod) -> R,
    {
        self.with_global_symbol_mut(symbol_id, |entry| match &mut entry.kind {
            SymbolEntryKind::Method(resolved_method) => Some(f(resolved_method)),
            _ => None,
        })?
    }

    fn with_func_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedFunc) -> R,
    {
        self.with_global_symbol_mut(symbol_id, |entry| match &mut entry.kind {
            SymbolEntryKind::Func(resolved_func) => Some(f(resolved_func)),
            _ => None,
        })?
    }

    fn with_typedef_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedTypedef) -> R,
    {
        self.with_global_symbol_mut(symbol_id, |entry| match &mut entry.kind {
            SymbolEntryKind::Typedef(resolved_typedef) => Some(f(resolved_typedef)),
            _ => None,
        })?
    }

    fn with_union_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedUnion) -> R,
    {
        self.with_global_symbol_mut(symbol_id, |entry| match &mut entry.kind {
            SymbolEntryKind::Union(resolved_union) => Some(f(resolved_union)),
            _ => None,
        })?
    }

    fn with_enum_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedEnum) -> R,
    {
        self.with_global_symbol_mut(symbol_id, |entry| match &mut entry.kind {
            SymbolEntryKind::Enum(resolved_enum) => Some(f(resolved_enum)),
            _ => None,
        })?
    }

    fn with_struct_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedStruct) -> R,
    {
        self.with_global_symbol_mut(symbol_id, |entry| match &mut entry.kind {
            SymbolEntryKind::Struct(resolved_struct) => Some(f(resolved_struct)),
            _ => None,
        })?
    }

    fn with_interface_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut ResolvedInterface) -> R,
    {
        self.with_global_symbol_mut(symbol_id, |entry| match &mut entry.kind {
            SymbolEntryKind::Interface(resolved_interface) => Some(f(resolved_interface)),
            _ => None,
        })?
    }
}

unsafe impl Send for Resolver {}
unsafe impl Sync for Resolver {}
