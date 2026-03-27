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
use cyrusc_ast::abi::Visibility;
use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_internal::local_scope::LocalScope;
use cyrusc_internal::module_loader::ModuleLoader;
use cyrusc_internal::symbols::symbols::{
    Module, Namespace, ResolvedEnum, ResolvedFunc, ResolvedGlobalVar, ResolvedInterface, ResolvedMethod,
    ResolvedStruct, ResolvedTypedef, ResolvedUnion, ResolvedVar, SymbolEntry, SymbolEntryKind,
};
use cyrusc_internal::symbols::table::{Query, ScopeTable, SymbolEntryMut};
use cyrusc_source_loc::{FileID, Loc, SourceMap};
use cyrusc_typed_ast::generics::mapping_ctx_arena::GenericMappingCtxArena;
use cyrusc_typed_ast::generics::monomorph::{
    MonomorphEntry, MonomorphFuncEntry, MonomorphID, MonomorphRegistry, SpecializedFuncEntry,
};
use cyrusc_typed_ast::stmts::*;
use cyrusc_typed_ast::*;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex, RwLock};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

mod diagnostics;
pub mod fs_module_loader;
pub mod macros;
pub mod modules;
pub mod traverse;

type ModuleGroupName = String;

/// Thread-safe registry mapping modules to their symbol tables.
///
/// GlobalSymbolRegistry wraps an Arc<Mutex<_>> to allow concurrent access
/// across compilation units.
pub struct GlobalSymbolRegistry {
    inner: Arc<RwLock<GlobalSymbolRegistryInner>>,
    root_scope: Option<SymbolID>,
}

pub struct GlobalSymbolRegistryInner {
    pub entries: Vec<SymbolEntry>,
}

/// Semantic resolver responsible for symbol binding, module loading,
/// and building the typed representation of the program.
pub struct Resolver {
    /// Global symbol table shared across all modules.
    /// Stores all declared symbols and their associated metadata.
    pub global_symbols: GlobalSymbolRegistry,

    /// Program trees of successfully analyzed modules.
    /// Acts as the semantic output collected during resolution.
    pub program_trees: Arc<Mutex<Vec<Rc<ResolvedProgramTree>>>>,

    module_names: Arc<Mutex<HashMap<FileID, String>>>,

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

    /// Tracks files that have already been analyzed.
    /// Prevents resolving the same file multiple times.
    analyzed_files: Arc<Mutex<HashSet<FileID>>>,

    module_symbols: HashMap<FileID, SymbolID>,

    source_map: Arc<SourceMap>,

    /// Arena managing generic mapping contexts.
    ///
    /// This is primarily required for constructing `GenericType` instances,
    /// which indirectly depend on a shared mapping context arena.
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,

    master_module_file_id: FileID,

    current_module_file_id: Option<FileID>,

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
    local_scopes: Vec<LocalScope>,

    scope_table_stack: Vec<SymbolID>,
    current_scope: Option<SymbolID>,

    // ID allocator for all compiler entities
    pub(crate) id_gen: IDGen,
}

pub struct ResolvedProgramTree {
    pub file_id: FileID,
    pub program_tree: Rc<RefCell<TypedProgramTree>>,
}

#[derive(Debug)]
pub struct IDGen {
    next_label_id: AtomicU32,
}

impl Resolver {
    pub fn new(
        module_loader: Box<dyn ModuleLoader>,
        source_map: Arc<SourceMap>,
        reporter: Arc<DiagReporter>,
        monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
        mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
        master_module_file_id: FileID,
    ) -> Self {
        Self {
            global_symbols: GlobalSymbolRegistry::new(),
            analyzed_files: Arc::new(Mutex::new(HashSet::new())),
            program_trees: Arc::new(Mutex::new(Vec::new())),
            module_names: Arc::new(Mutex::new(HashMap::new())),
            local_scopes: Vec::new(),
            module_loader,
            current_object_generic_params: None,
            current_object: None,
            master_module_file_id,
            monomorph_registry,
            mapping_ctx_arena,
            reporter,
            source_map,
            id_gen: IDGen::new(),
            current_scope: None,
            scope_table_stack: Vec::new(),
            module_symbols: HashMap::new(),
            current_module_file_id: None,
        }
    }

    /// Returns a reference to the current active scope, if any.
    #[inline]
    pub fn current_local_scope(&self) -> Option<&LocalScope> {
        self.local_scopes.last()
    }

    /// Returns a mutable reference to the current active scope, if any.
    #[inline]
    pub fn current_scope_mut(&mut self) -> Option<&mut LocalScope> {
        self.local_scopes.last_mut()
    }

    // Method to enter a new scope.
    #[inline]
    pub fn enter_local_scope(&mut self, scope: LocalScope) {
        self.local_scopes.push(scope);
    }

    // Method to exit the current scope.
    #[inline]
    pub fn exit_local_scope(&mut self) {
        self.local_scopes.pop();
    }

    /// Returns an iterator over the active scope stack.
    ///
    /// The iterator yields scopes from the innermost scope outward toward
    /// the outermost scope.
    pub fn local_scopes_into_iter(&self) -> impl Iterator<Item = &LocalScope> {
        self.local_scopes.iter().rev()
    }

    #[inline]
    pub fn current_module_file_path(&self) -> PathBuf {
        let file_id = self
            .current_module_file_id
            .expect("resolver current module file not set");

        self.source_map.get_file(file_id).unwrap().file_path.clone()
    }

    fn insert_module_name(&self, file_id: FileID, name: String) {
        let mut map = self.module_names.lock().unwrap();
        map.insert(file_id, name);
    }

    fn with_global_symbol_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut SymbolEntry) -> R,
    {
        let mut registry = self.global_symbols.inner.write().unwrap();
        let entry = registry.entries.get_mut(symbol_id.0 as usize)?;
        Some(f(entry))
    }

    /// Resolves a symbol by searching the active scope stack.
    ///
    /// Lookup proceeds from the innermost scope outward until a matching
    /// symbol binding is found.
    pub(crate) fn resolve_scope_symbol(&self, name: &str) -> Option<SymbolID> {
        for scope in self.local_scopes_into_iter() {
            if let Some(symbol_id) = scope.resolve(name) {
                return Some(symbol_id);
            }
        }

        None
    }

    /// Return a mutable reference to the scope table for a given symbol entry.
    ///
    /// This is a utility used during symbol insertion to avoid repeating
    /// pattern matching on `SymbolEntryKind`. Only `Module` and `Namespace`
    /// symbols own a scope table.
    pub fn scope_table_mut(entry: &mut SymbolEntry) -> &mut ScopeTable {
        match &mut entry.kind {
            SymbolEntryKind::Module(module) => &mut module.scope,
            SymbolEntryKind::Namespace(namespace) => &mut namespace.scope,
            _ => panic!("symbol is not a scope"),
        }
    }

    pub fn get_or_create_module_symbol_id(&mut self, file_id: FileID, module_name: &str, loc: Loc) -> SymbolID {
        if let Some(symbol_id) = self.module_symbols.get(&file_id) {
            return *symbol_id;
        }

        let module_symbol_id =
            self.global_symbols
                .insert_module_symbol(self.global_symbols.root_scope(), module_name, file_id);

        self.module_symbols.insert(file_id, module_symbol_id);
        module_symbol_id
    }
}

impl GlobalSymbolRegistry {
    pub fn new() -> Self {
        let mut global_symbols = Self {
            inner: Arc::new(RwLock::new(GlobalSymbolRegistryInner { entries: Vec::new() })),
            root_scope: None,
        };

        global_symbols.root_scope = Some(global_symbols.insert_symbol_entry(SymbolEntry::new(
            SymbolEntryKind::Namespace(Namespace {
                name: "<global>".to_string(),
                scope: ScopeTable::new(),
                loc: Loc::default(FileID(0)),
            }),
            None,
        )));

        global_symbols
    }

    pub fn root_scope(&self) -> SymbolID {
        self.root_scope.unwrap()
    }

    /// Insert a symbol into a scope by name.
    ///
    /// Looks up the `SymbolEntry` for `scope_id`, extracts its
    /// `ScopeTable`, and inserts `(name -> symbol_id)` into the table.
    /// Only `Module` and `Namespace` entries may be used as scopes.
    pub fn insert_symbol_name(&self, scope_id: SymbolID, symbol_id: SymbolID, name: &str) {
        let mut registry = self.inner.write().unwrap();
        let symbol_entry = &mut registry.entries[scope_id.0 as usize];
        let scope_table = symbol_entry.get_scope_table_mut().unwrap();

        scope_table.names.insert(name.to_string(), symbol_id);
    }

    /// Lookup a symbol entry by `SymbolID` in global symbols.
    pub fn lookup_symbol_entry(&self, id: SymbolID) -> Option<SymbolEntry> {
        let registry = self.inner.read().unwrap();
        registry.entries.get(id.0 as usize).cloned()
    }

    /// Lookup a symbol by name within a given scope.
    pub fn lookup_symbol_id(&self, scope_id: SymbolID, name: &str) -> Option<SymbolID> {
        let registry = self.inner.read().unwrap();
        let symbol_entry = &registry.entries[scope_id.0 as usize];
        let scope_table = symbol_entry.get_scope_table().unwrap();

        scope_table.names.get(name).copied()
    }

    /// Inserts a new symbol entry in the global symbols and returns the associated `SymbolID`.
    pub fn insert_symbol_entry(&self, entry: SymbolEntry) -> SymbolID {
        let mut registry = self.inner.write().unwrap();

        let symbol_id = SymbolID(registry.entries.len() as u32);
        registry.entries.push(entry);
        symbol_id
    }

    /// Create a `ProxiedSymbol` inside the given scope.
    ///
    /// A proxy symbol represents an imported symbol. The proxy inherits
    /// visibility from the target symbol and binds into the caller's
    /// current scope under the provided `name`.
    pub fn insert_proxy_symbol(
        &self,
        current_scope: SymbolID,
        name: &str,
        imported_scope_id: SymbolID,
        target_symbol_id: SymbolID,
    ) -> SymbolID {
        // proxy inherits visibility from target symbol
        let target_vis = self.lookup_symbol_entry(target_symbol_id).unwrap().vis_opt.clone();

        let proxy_id = self.insert_symbol_entry(SymbolEntry {
            kind: SymbolEntryKind::ProxiedSymbol {
                scope_id: imported_scope_id,
                symbol_id: target_symbol_id,
            },
            vis_opt: target_vis,
            used: false,
        });

        self.insert_symbol_name(current_scope, proxy_id, name);
        proxy_id
    }

    /// Insert a new `Module` symbol into the given parent scope.
    ///
    /// A module symbol owns a `ScopeTable` which becomes the root scope
    /// for resolving declarations inside that module.
    ///
    /// The symbol is:
    /// 1. Allocated in the global symbol arena
    /// 2. Bound into the parent scope under `name`
    ///
    /// Returns the created `SymbolID`.
    pub fn insert_module_symbol(&self, parent_scope: SymbolID, name: &str, file_id: FileID) -> SymbolID {
        let module_symbol = self.insert_symbol_entry(SymbolEntry::new(
            SymbolEntryKind::Module(Module {
                name: name.to_string(),
                scope: ScopeTable::new(),
                loc: Loc::new(file_id, 0, 0, 0, 0),
            }),
            None,
        ));

        self.insert_symbol_name(parent_scope, module_symbol, name);
        module_symbol
    }

    /// Insert a new lexical `Namespace` symbol into the given scope.
    ///
    /// Namespaces behave similarly to modules but exist purely inside
    /// source files (`mod foo { ... }`). They introduce a new scope
    /// table for name resolution.
    ///
    /// The symbol is:
    /// 1. Allocated in the global symbol arena
    /// 2. Bound into the parent scope under `name`
    ///
    /// Returns the created `SymbolID`.
    pub fn insert_namespace_symbol(
        &self,
        parent_scope: SymbolID,
        name: &str,
        loc: Loc,
        vis_opt: Option<Visibility>,
    ) -> SymbolID {
        let namespace_symbol = self.insert_symbol_entry(SymbolEntry::new(
            SymbolEntryKind::Namespace(Namespace {
                name: name.to_string(),
                scope: ScopeTable::new(),
                loc,
            }),
            vis_opt,
        ));

        self.insert_symbol_name(parent_scope, namespace_symbol, name);
        namespace_symbol
    }

    pub fn insert_module_alias(&self, parent_scope_id: SymbolID, name: &str, target_symbol_id: SymbolID) -> SymbolID {
        let symbol_id = self.insert_symbol_entry(SymbolEntry::new(
            SymbolEntryKind::ProxiedModule {
                symbol_id: target_symbol_id,
            },
            None,
        ));

        self.insert_symbol_name(parent_scope_id, symbol_id, name);
        symbol_id
    }
}

impl Query for Resolver {
    lookup_kind!(lookup_var, Var, ResolvedVar);

    lookup_kind!(lookup_global_var, GlobalVar, ResolvedGlobalVar);

    lookup_kind!(lookup_method, Method, ResolvedMethod);

    lookup_kind!(lookup_func, Func, ResolvedFunc);

    lookup_kind!(lookup_typedef, Typedef, ResolvedTypedef);

    lookup_kind!(lookup_union, Union, ResolvedUnion);

    lookup_kind!(lookup_enum, Enum, ResolvedEnum);

    lookup_kind!(lookup_struct, Struct, ResolvedStruct);

    lookup_kind!(lookup_interface, Interface, ResolvedInterface);

    /// Look up a symbol identifier by name.
    fn lookup_symbol_id(&self, scope_id: SymbolID, name: &str) -> Option<SymbolID> {
        let registry = self.global_symbols.inner.read().unwrap();
        let symbol_entry = registry.entries.get(scope_id.0 as usize)?;
        let scope_table = symbol_entry.get_scope_table()?;

        scope_table.names.get(name).copied()
    }

    /// Look up a symbol identifier by name within a specific module.
    fn lookup_symbol_id_in_scope(&self, scope_id: SymbolID, name: &str) -> Option<SymbolID> {
        let registry = self.global_symbols.inner.read().unwrap();
        let symbol_entry = registry.entries.get(scope_id.0 as usize)?;
        let scope_table = symbol_entry.get_scope_table()?;

        scope_table.names.get(name).copied()
    }

    /// Retrieve the full semantic entry for a symbol by its name.
    fn lookup_symbol_entry(&self, scope_id: SymbolID, name: &str) -> Option<SymbolEntry> {
        let symbol_id = self.lookup_symbol_id(scope_id, name)?;
        self.global_symbols.lookup_symbol_entry(symbol_id)
    }

    /// Retrieve the semantic entry for a symbol, resolving proxy chains.
    fn lookup_global_symbol(&self, mut symbol_id: SymbolID) -> Option<SymbolEntry> {
        let registry = self.global_symbols.inner.read().unwrap();

        loop {
            let symbol_entry = registry.entries.get(symbol_id.0 as usize)?;

            match symbol_entry.kind {
                SymbolEntryKind::ProxiedSymbol {
                    symbol_id: target_symbol_id,
                    ..
                } => {
                    symbol_id = target_symbol_id;
                }
                _ => return Some(symbol_entry.clone()),
            }
        }
    }

    fn format_symbol_name(&self, symbol_id: SymbolID) -> String {
        match self.lookup_global_symbol(symbol_id) {
            Some(symbol_entry) => symbol_entry.decl_name(),
            None => "<UNRESOLVED_SYMBOL>".to_string(),
        }
    }

    fn lookup_monomorph_func(&self, monomorph_id: MonomorphID) -> Option<MonomorphFuncEntry> {
        {
            let monomorph_registry = self.monomorph_registry.lock().unwrap();
            let monomorph_entry = monomorph_registry.resolve_by_monomorph_id(monomorph_id).unwrap();
            let monomorph_func_entry = match monomorph_entry.clone() {
                MonomorphEntry::Func(monomorph_func_entry) => monomorph_func_entry,
            };
            Some(monomorph_func_entry)
        }
    }

    fn lookup_specialized_func_instance(&self, monomorph_id: MonomorphID) -> Option<SpecializedFuncEntry> {
        {
            let monomorph_registry = self.monomorph_registry.lock().unwrap();
            monomorph_registry
                .resolve_specialized_func_instance(monomorph_id)
                .cloned()
        }
    }

    fn lookup_module_name(&self, file_id: FileID) -> Option<String> {
        let map = self.module_names.lock().unwrap();
        map.get(&file_id).cloned()
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

impl IDGen {
    pub fn new() -> Self {
        Self {
            // 0 = invalid, 1 = master module.
            // Allocation starts at 2.
            next_module_id: AtomicU32::new(2),
            next_label_id: AtomicU32::new(1),
        }
    }

    #[inline(always)]
    pub fn alloc_label(&self) -> LabelID {
        LabelID(self.next_label_id.fetch_add(1, Ordering::Relaxed))
    }
}

unsafe impl Send for Resolver {}
unsafe impl Sync for Resolver {}
