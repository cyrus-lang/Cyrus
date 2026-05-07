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

use cyrusc_ast::abi::Visibility;
use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_internal::generic_scope::GenericScope;
use cyrusc_internal::local_scope::LocalScope;
use cyrusc_internal::module_loader::ModuleLoader;
use cyrusc_internal::symbols::SymbolQuery;
use cyrusc_internal::symbols::symbols::{Module, Namespace, SymbolEntry, SymbolEntryKind};
use cyrusc_internal::symbols::table::ScopeTable;
use cyrusc_source_loc::{FileID, Loc, SourceMap};
use cyrusc_typed_ast::decls::table::DeclTablesRegistry;
use cyrusc_typed_ast::decls::{
    DeclID, EnumDeclID, FuncDeclID, GlobalVarDeclID, InterfaceDeclID, MethodDeclID, StructDeclID, TypedefDeclID,
    UnionDeclID, VarDeclID,
};
use cyrusc_typed_ast::format::{Formatter, format_enum_decl, format_struct_decl, format_union_decl};
use cyrusc_typed_ast::stmts::*;
use cyrusc_typed_ast::types::TypeDeclID;
use cyrusc_typed_ast::*;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex, RwLock};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

mod diagnostics;
pub mod fs_module_loader;
pub mod macros;
pub mod modules;
pub mod traverse;

/// Thread-safe registry mapping modules to their symbol tables.
///
/// GlobalSymbolRegistry wraps an Arc<Mutex<_>> to allow concurrent access
/// across compilation units.
pub struct GlobalSymbolRegistry {
    pub inner: Arc<RwLock<GlobalSymbolRegistryInner>>,
    pub root_scope: Option<SymbolID>,
}

pub struct GlobalSymbolRegistryInner {
    pub entries: Vec<SymbolEntry>,
}

/// Semantic resolver responsible for symbol binding, module loading,
/// and building the typed representation of the program.
pub struct Resolver {
    /// Global symbol table shared across all modules.
    /// Stores all declared symbols and their associated metadata.
    pub global_symbols: Arc<GlobalSymbolRegistry>,

    /// Program trees of successfully analyzed modules.
    /// Acts as the semantic output collected during resolution.
    pub program_trees: Arc<Mutex<Vec<Rc<ResolvedProgramTree>>>>,

    /// Diagnostic reporter used to emit compiler errors and warnings.
    pub reporter: Arc<DiagReporter>,

    /// Module loader responsible for locating and parsing modules.
    /// Typically backed by a filesystem implementation.
    pub module_loader: Box<dyn ModuleLoader>,

    /// Tracks files that have already been analyzed.
    /// Prevents resolving the same file multiple times.
    analyzed_files: Arc<Mutex<HashSet<FileID>>>,

    module_names: Arc<Mutex<HashMap<FileID, String>>>,

    module_symbols: HashMap<FileID, SymbolID>,

    decl_tables: Arc<DeclTablesRegistry>,

    source_map: Arc<SourceMap>,

    current_module_file_id: Option<FileID>,

    generic_scopes: Vec<GenericScope>,

    /// Stack of active `LocalScope`s representing the current nesting of lexical scopes
    ///
    /// This stack is manipulated internally by `LocalScopeGuard`, ensuring
    /// automatic scope entry and exit based on lexical lifetime.
    local_scopes: Vec<LocalScope>,

    scope_table_stack: Vec<SymbolID>,
    current_scope: Option<SymbolID>,

    current_object_symbol_id: Option<SymbolID>,

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
        decl_tables: Arc<DeclTablesRegistry>,
    ) -> Self {
        Self {
            global_symbols: Arc::new(GlobalSymbolRegistry::new()),
            analyzed_files: Arc::new(Mutex::new(HashSet::new())),
            program_trees: Arc::new(Mutex::new(Vec::new())),
            module_names: Arc::new(Mutex::new(HashMap::new())),
            local_scopes: Vec::new(),
            module_loader,
            generic_scopes: Vec::new(),
            reporter,
            decl_tables,
            source_map,
            id_gen: IDGen::new(),
            current_scope: None,
            scope_table_stack: Vec::new(),
            module_symbols: HashMap::new(),
            current_module_file_id: None,
            current_object_symbol_id: None,
        }
    }

    /// Returns a reference to the current active scope, if any.
    #[inline]
    pub fn current_local_scope(&self) -> Option<&LocalScope> {
        self.local_scopes.last()
    }

    /// Returns a mutable reference to the current active scope, if any.
    #[inline]
    pub fn current_local_scope_mut(&mut self) -> Option<&mut LocalScope> {
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

    /// Insert a local variable into the current local scope.
    #[inline]
    pub fn insert_local(&mut self, name: String, symbol_id: SymbolID) {
        self.current_local_scope_mut().unwrap().insert(name, symbol_id);
    }

    /// Look up a name in all lexical scopes (innermost-first).
    pub fn lookup_local(&self, name: &str) -> Option<SymbolID> {
        for scope in self.local_scopes_into_iter() {
            if let Some(symbol_id) = scope.resolve(name) {
                return Some(symbol_id);
            }
        }
        None
    }

    #[inline]
    pub fn lookup_module_file_id(&self, module_symbol_id: SymbolID) -> Option<FileID> {
        self.module_symbols
            .iter()
            .find(|(_, symbol_id)| **symbol_id == module_symbol_id)
            .map(|(file_id, _)| *file_id)
    }

    #[inline]
    pub fn current_module_file_path(&self) -> PathBuf {
        let file_id = self
            .current_module_file_id
            .expect("resolver current module file not set");

        self.source_map.get_file(file_id).unwrap().file_path.clone()
    }

    fn insert_module_name(&self, file_id: FileID, name: &str) {
        let mut module_names = self.module_names.lock().unwrap();
        module_names.insert(file_id, name.to_string());
    }

    pub(crate) fn resolve_local_scope_label(&self, name: &str) -> Option<LabelID> {
        for scope in self.local_scopes_into_iter() {
            if let Some(label_id) = scope.resolve_label(name) {
                return Some(label_id);
            }
        }

        None
    }

    /// Resolves a symbol by searching the active local scope stack.
    ///
    /// Lookup proceeds from the innermost scope outward until a matching
    /// symbol binding is found.
    pub(crate) fn resolve_local_scope_symbol(&self, name: &str) -> Option<SymbolID> {
        for scope in self.local_scopes_into_iter() {
            if let Some(symbol_id) = scope.resolve(name) {
                return Some(symbol_id);
            }
        }

        None
    }

    pub fn with_scope_table<R, F>(&self, scope_id: SymbolID, f: F) -> R
    where
        F: FnOnce(&ScopeTable) -> R,
    {
        let scope_id = self.global_symbols.resolve_concrete_scope_id(scope_id);

        let registry = self.global_symbols.inner.read().unwrap();
        let symbol_entry = &registry.entries[scope_id.0 as usize];
        let scope_table = symbol_entry
            .get_scope_table()
            .expect("scope does not exist for given scope id");

        f(scope_table)
    }

    pub fn with_scope_table_mut<R, F>(&self, scope_id: SymbolID, f: F) -> R
    where
        F: FnOnce(&mut ScopeTable) -> R,
    {
        let scope_id = self.global_symbols.resolve_concrete_scope_id(scope_id);

        let mut registry = self.global_symbols.inner.write().unwrap();
        let symbol_entry = &mut registry.entries[scope_id.0 as usize];
        let scope_table = symbol_entry
            .get_scope_table_mut()
            .expect("scope does not exist for given scope id");

        f(scope_table)
    }

    /// Enter a new lexical scope.
    ///
    /// This pushes the current scope onto `scope_table_stack` and
    /// updates `current_scope` to the provided `scope_id`.
    pub fn enter_scope_table(&mut self, scope_id: SymbolID) {
        let scope_id = self.global_symbols.resolve_concrete_scope_id(scope_id);

        if let Some(curr) = self.current_scope {
            self.scope_table_stack.push(curr);
        }
        self.current_scope = Some(scope_id);
    }

    /// Exit the current lexical scope.
    ///
    /// Pops the last scope ID from `scope_table_stack` and restores it as `current_scope`.
    pub fn exit_scope_table(&mut self) {
        self.current_scope = self.scope_table_stack.pop();
    }

    pub fn create_entry_module_symbol_id(&mut self, module_file_path: &Path, file_id: FileID) -> SymbolID {
        let root_scope_id = self.global_symbols.root_scope_id();
        let module_name = self.module_loader.module_name_from_file_path(module_file_path);
        self.insert_module_name(file_id, &module_name);
        self.get_or_create_module_symbol_id_for_file(root_scope_id, file_id, &module_name)
    }

    pub fn get_or_create_module_symbol_id_for_file(
        &mut self,
        parent_scope: SymbolID,
        file_id: FileID,
        module_name: &str,
    ) -> SymbolID {
        if let Some(symbol_id) = self.module_symbols.get(&file_id) {
            return *symbol_id;
        }

        let module_symbol_id = self.global_symbols.insert_module_symbol(parent_scope, module_name);

        self.global_symbols
            .insert_symbol_name(parent_scope, module_symbol_id, module_name);

        self.module_symbols.insert(file_id, module_symbol_id);

        module_symbol_id
    }

    pub fn get_or_create_virtual_module_symbol(&mut self, parent_scope_id: SymbolID, module_name: &str) -> SymbolID {
        let parent_scope_id = self.global_symbols.resolve_concrete_scope_id(parent_scope_id);

        if let Some(symbol_id) = self.lookup_symbol_id(parent_scope_id, module_name) {
            let entry = self.global_symbols.get_symbol_entry(symbol_id).unwrap();
            return match &entry.kind {
                SymbolEntryKind::ProxiedModule { symbol_id: target } => *target,
                _ => symbol_id,
            };
        }

        self.global_symbols.insert_module_symbol(parent_scope_id, module_name)
    }

    fn with_global_symbol_mut<F, R>(&self, symbol_id: SymbolID, f: F) -> Option<R>
    where
        F: FnOnce(&mut SymbolEntry) -> R,
    {
        let mut registry = self.global_symbols.inner.write().unwrap();
        let entry = registry.entries.get_mut(symbol_id.0 as usize)?;
        Some(f(entry))
    }

    pub(crate) fn with_generic_scope<F, R>(&mut self, generic_params: &TypedGenericParams, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let scope = self.new_generic_scope(generic_params);
        self.push_generic_scope(scope);

        let result = f(self);

        self.pop_generic_scope();

        result
    }

    pub(crate) fn new_generic_scope(&self, generic_params: &TypedGenericParams) -> GenericScope {
        let mut generic_scope = GenericScope::new();
        for generic_param_id in generic_params.iter() {
            let generic_param = self.decl_tables.generic_param(*generic_param_id);
            generic_scope.insert(generic_param.name.as_string(), generic_param_id.clone());
        }
        generic_scope
    }

    pub(crate) fn push_generic_scope(&mut self, generic_scope: GenericScope) {
        self.generic_scopes.push(generic_scope);
    }

    pub(crate) fn pop_generic_scope(&mut self) {
        self.generic_scopes.pop();
    }

    pub(crate) fn generic_scopes_iter(&self) -> impl Iterator<Item = &GenericScope> {
        self.generic_scopes.iter().rev()
    }
}

// Insertions.
impl GlobalSymbolRegistry {
    /// Create a new global symbol registry and allocate the root scope.
    ///
    /// The root scope is a global namespace symbol named `<global>`.
    pub fn new() -> Self {
        let mut global_symbols = Self {
            inner: Arc::new(RwLock::new(GlobalSymbolRegistryInner { entries: Vec::new() })),
            root_scope: None,
        };

        global_symbols.root_scope = Some(global_symbols.insert_symbol_entry(SymbolEntry::new(
            SymbolEntryKind::Namespace(Namespace {
                name: "<GLOBAL_NAMESPACE>".to_string(),
                scope: ScopeTable::new(),
                loc: Loc::default(FileID(0)),
            }),
            None,
            None,
            None,
        )));

        global_symbols
    }

    /// Get the root scope symbol for the global namespace.
    #[inline]
    pub fn root_scope_id(&self) -> SymbolID {
        self.root_scope.unwrap()
    }

    /// Insert symbol by name into the specified scope.
    pub fn insert_symbol_name(&self, scope_id: SymbolID, symbol_id: SymbolID, name: &str) {
        let scope_id = self.resolve_concrete_scope_id(scope_id);

        let mut registry = self.inner.write().unwrap();
        let symbol_entry = &mut registry.entries[scope_id.0 as usize];
        let scope_table = symbol_entry.get_scope_table_mut().unwrap();

        scope_table.bind(name.to_string(), symbol_id);
    }

    /// Create and bind a proxied imported symbol in the current scope.
    ///
    /// The proxied symbol inherits visibility. Used for single imports.
    pub fn insert_proxied_symbol(
        &self,
        current_scope_id: SymbolID,
        name: &str,
        imported_scope_id: SymbolID,
        target_symbol_id: SymbolID,
    ) -> SymbolID {
        // proxy inherits visibility from target symbol
        let target_vis = self.get_symbol_entry(target_symbol_id).unwrap().vis_opt.clone();

        let proxy_id = self.insert_symbol_entry(SymbolEntry::new(
            SymbolEntryKind::ProxiedSymbol {
                scope_id: imported_scope_id,
                symbol_id: target_symbol_id,
            },
            target_vis,
            Some(current_scope_id),
            None,
        ));

        self.insert_symbol_name(current_scope_id, proxy_id, name);
        proxy_id
    }

    /// Create and bind a proxied module symbol in the parent scope.
    ///
    /// Used for module group imports or aliasing entire modules.
    pub fn insert_proxied_module(&self, parent_scope_id: SymbolID, name: &str, target_symbol_id: SymbolID) -> SymbolID {
        let symbol_id = self.insert_symbol_entry(SymbolEntry::new(
            SymbolEntryKind::ProxiedModule {
                symbol_id: target_symbol_id,
            },
            None,
            Some(parent_scope_id),
            None,
        ));

        self.insert_symbol_name(parent_scope_id, symbol_id, name);
        symbol_id
    }

    /// Create and bind a new lexical namespace symbol.
    ///
    /// Namespaces introduce new scope tables within source files or modules.
    pub fn insert_namespace_symbol(
        &self,
        parent_scope_id: SymbolID,
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
            Some(parent_scope_id),
            Some(loc),
        ));

        self.insert_symbol_name(parent_scope_id, namespace_symbol, name);
        namespace_symbol
    }

    /// Allocate a new module symbol and bind it in the specified parent scope.
    pub fn insert_module_symbol(&self, parent_scope_id: SymbolID, name: &str) -> SymbolID {
        let module_symbol_id = self.insert_symbol_entry(SymbolEntry::new(
            SymbolEntryKind::Module(Module {
                scope_id: SymbolID::placeholder(), // temporarily placeholder
                name: name.to_string(),
                scope: ScopeTable::new(),
            }),
            None,
            Some(parent_scope_id),
            None,
        ));

        self.insert_symbol_name(parent_scope_id, module_symbol_id, name);

        let mut inner = self.inner.write().unwrap();

        match &mut inner.entries[module_symbol_id.0 as usize].kind {
            SymbolEntryKind::Module(module) => {
                // module's own SymbolID *is* the scope_id
                module.scope_id = module_symbol_id;
            }
            _ => unreachable!(),
        }

        module_symbol_id
    }

    /// Inserts a new symbol entry in the global symbols and returns the associated `SymbolID`.
    pub fn insert_symbol_entry(&self, symbol_entry: SymbolEntry) -> SymbolID {
        let mut registry = self.inner.write().unwrap();

        let symbol_id = SymbolID(registry.entries.len() as u32);
        registry.entries.push(symbol_entry);
        symbol_id
    }
}

// Lookups.
impl GlobalSymbolRegistry {
    /// Get the symbol entry for a symbol.
    pub fn get_symbol_entry(&self, symbol_id: SymbolID) -> Option<SymbolEntry> {
        let registry = self.inner.read().unwrap();
        registry.entries.get(symbol_id.0 as usize).cloned()
    }

    /// Lookup a symbol by name within a specific scope.
    pub fn lookup_symbol_id_in_scope(&self, scope_id: SymbolID, name: &str) -> Option<SymbolID> {
        let scope_id = self.resolve_concrete_scope_id(scope_id);

        let registry = self.inner.read().unwrap();
        let symbol_entry = &registry.entries[scope_id.0 as usize];
        let scope_table = symbol_entry.get_scope_table()?;

        scope_table.lookup(name)
    }

    /// Recursively search up the scope chain for a symbol by name.
    pub fn lookup_in_scope_chain(&self, scope_id: SymbolID, name: &str) -> Option<SymbolID> {
        let mut scope_id = self.resolve_concrete_scope_id(scope_id);

        loop {
            if let Some(symbol_id) = self.lookup_symbol_id_in_scope(scope_id, name) {
                return Some(symbol_id);
            }

            let symbol_entry = self.get_symbol_entry(scope_id)?;

            match symbol_entry.parent_scope_id {
                Some(parent_id) => scope_id = parent_id,
                None => return None,
            }
        }
    }

    /// Resolve a symbol ID to the concrete scope that actually owns
    /// a `ScopeTable`.
    ///
    /// In the symbol model, some entries such as `ProxiedModule` and
    /// `ProxiedSymbol` do *not* contain their own scope tables. They
    /// are indirections that refer to another symbol whose scope should
    /// be used for name lookup. Attempting to read a scope table from
    /// a proxy would therefore panic.
    ///
    /// This function follows proxy chains recursively until it reaches
    /// a symbol entry that is guaranteed to own a scope table.
    ///
    /// If the given symbol ID is already a concrete scope owner, it is
    /// returned unchanged. If the symbol is not found (should not
    /// happen in a consistent registry), the original ID is returned as
    /// a defensive fallback.
    fn resolve_concrete_scope_id(&self, scope_id: SymbolID) -> SymbolID {
        let symbol_entry = self.get_symbol_entry(scope_id).expect("Symbol entry not found");

        match &symbol_entry.kind {
            SymbolEntryKind::ProxiedModule { symbol_id } => {
                // Recurse to handle nested proxies (e.g. aliases of aliases)
                self.resolve_concrete_scope_id(*symbol_id)
            }
            SymbolEntryKind::Module(module) => {
                // return the scope id of the internal scope,
                // not the id of the module symbol itself!
                module.scope_id
            }
            _ => scope_id,
        }
    }

    /// Resolve a symbol ID to the underlying concrete symbol by
    /// eliminating proxy indirections.
    ///
    /// In the symbol model, some entries such as `ProxiedSymbol`
    /// represent aliases or re-exports of another symbol. These
    /// entries do not define a new semantic entity; instead they
    /// forward all semantic meaning to a target symbol.
    pub fn resolve_concrete_symbol_id(&self, symbol_id: SymbolID) -> SymbolID {
        let Some(symbol_entry) = self.get_symbol_entry(symbol_id) else {
            return symbol_id;
        };

        match symbol_entry.kind {
            SymbolEntryKind::ProxiedSymbol {
                symbol_id: target_symbol_id,
                ..
            } => self.resolve_concrete_symbol_id(target_symbol_id),
            _ => symbol_id,
        }
    }
}

impl Formatter for Resolver {
    fn format_decl(&self, decl_id: DeclID) -> String {
        match decl_id {
            DeclID::Struct(struct_decl_id) => {
                let decl = self.decl_tables.struct_decl(struct_decl_id);

                if let Some(name) = &decl.name {
                    name.clone()
                } else {
                    format_struct_decl(&decl, self)
                }
            }
            DeclID::Enum(enum_decl_id) => {
                let decl = self.decl_tables.enum_decl(enum_decl_id);

                if let Some(name) = &decl.name {
                    name.clone()
                } else {
                    format_enum_decl(&decl, self)
                }
            }
            DeclID::Union(union_decl_id) => {
                let decl = self.decl_tables.union_decl(union_decl_id);

                if let Some(name) = &decl.name {
                    name.clone()
                } else {
                    format_union_decl(&decl, self)
                }
            }
            DeclID::Func(func_decl_id) => {
                let decl = self.decl_tables.func_decl(func_decl_id);
                decl.name.clone()
            }
            DeclID::Method(method_decl_id) => {
                let method_decl = self.decl_tables.method_decl(method_decl_id);
                let func_decl = method_decl.func_decl;

                func_decl.name.clone()
            }
            DeclID::Interface(interface_decl_id) => {
                let decl = self.decl_tables.interface_decl(interface_decl_id);
                decl.name.clone()
            }
            DeclID::GlobalVar(global_var_decl_id) => {
                let decl = self.decl_tables.global_var_decl(global_var_decl_id);
                decl.name.clone()
            }
            DeclID::Var(var_decl_id) => {
                let decl = self.decl_tables.var_decl(var_decl_id);
                decl.name.clone()
            }
            DeclID::Typedef(typedef_decl_id) => {
                let decl = self.decl_tables.typedef_decl(typedef_decl_id);
                decl.name.clone()
            }
        }
    }

    fn format_type_decl(&self, id: TypeDeclID) -> String {
        match id {
            TypeDeclID::Struct(struct_decl_id) => {
                let decl = self.decl_tables.struct_decl(struct_decl_id);

                if let Some(name) = decl.name {
                    name
                } else {
                    format_struct_decl(&decl, self)
                }
            }
            TypeDeclID::Enum(enum_decl_id) => {
                let decl = self.decl_tables.enum_decl(enum_decl_id);

                if let Some(name) = decl.name {
                    name
                } else {
                    format_enum_decl(&decl, self)
                }
            }
            TypeDeclID::Union(union_decl_id) => {
                let decl = self.decl_tables.union_decl(union_decl_id);

                if let Some(name) = decl.name {
                    name
                } else {
                    format_union_decl(&decl, self)
                }
            }
            TypeDeclID::Interface(interface_decl_id) => self.decl_tables.interface_decl(interface_decl_id).name,
            TypeDeclID::Typedef(typedef_decl_id) => {
                let decl = self.decl_tables.typedef_decl(typedef_decl_id);

                decl.name
            }
        }
    }

    fn format_symbol_name(&self, symbol_id: SymbolID) -> String {
        const UNRESOLVED_SYMBOL: &str = "<UNRESOLVED_SYMBOL>";
        const PROXIED_SYMBOL: &str = "<PROXIED_SYMBOL>";
        const PROXIED_MODULE: &str = "<PROXIED_MODULE>";

        match self.lookup_symbol_entry(symbol_id) {
            Some(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Unresolved => String::from(UNRESOLVED_SYMBOL),
                SymbolEntryKind::Module(module) => module.name.clone(),
                SymbolEntryKind::Namespace(namespace) => namespace.name.clone(),
                SymbolEntryKind::Func(func_decl_id) => self.decl_tables.func_decl(*func_decl_id).name.clone(),
                SymbolEntryKind::Method(method_decl_id) => {
                    let method_decl = self.decl_tables.method_decl(*method_decl_id);
                    let func_decl = method_decl.func_decl;
                    func_decl.name.clone()
                }
                SymbolEntryKind::Struct(struct_decl_id) => {
                    let struct_decl = self.decl_tables.struct_decl(*struct_decl_id);

                    if let Some(name) = &struct_decl.name {
                        name.clone()
                    } else {
                        format_struct_decl(&struct_decl, self)
                    }
                }
                SymbolEntryKind::Enum(enum_decl_id) => {
                    let enum_decl = self.decl_tables.enum_decl(*enum_decl_id);

                    if let Some(name) = &enum_decl.name {
                        name.clone()
                    } else {
                        format_enum_decl(&enum_decl, self)
                    }
                }
                SymbolEntryKind::Union(union_decl_id) => {
                    let union_decl = self.decl_tables.union_decl(*union_decl_id);

                    if let Some(name) = &union_decl.name {
                        name.clone()
                    } else {
                        format_union_decl(&union_decl, self)
                    }
                }
                SymbolEntryKind::Interface(interface_decl_id) => {
                    self.decl_tables.interface_decl(*interface_decl_id).name.clone()
                }
                SymbolEntryKind::Var(var_decl_id) => self.decl_tables.var_decl(*var_decl_id).name.clone(),
                SymbolEntryKind::GlobalVar(global_var_decl_id) => {
                    self.decl_tables.global_var_decl(*global_var_decl_id).name.clone()
                }
                SymbolEntryKind::Typedef(typedef_decl_id) => {
                    self.decl_tables.typedef_decl(*typedef_decl_id).name.clone()
                }
                SymbolEntryKind::ProxiedSymbol { symbol_id, .. } => self
                    .lookup_symbol_entry(*symbol_id)
                    .map(|_| self.format_symbol_name(*symbol_id))
                    .unwrap_or_else(|| String::from(PROXIED_SYMBOL)),
                SymbolEntryKind::ProxiedModule { symbol_id } => self
                    .lookup_symbol_entry(*symbol_id)
                    .map(|_| self.format_symbol_name(*symbol_id))
                    .unwrap_or_else(|| String::from(PROXIED_MODULE)),
            },
            None => String::from(UNRESOLVED_SYMBOL),
        }
    }

    fn format_generic_param(&self, generic_param_id: GenericParamID) -> String {
        let generic_param = self.decl_tables.generic_param(generic_param_id);
        generic_param.name.as_string()
    }
}

impl SymbolQuery for Resolver {
    impl_helper_method__get_kind!(get_var, Var, VarDeclID);

    impl_helper_method__get_kind!(get_global_var, GlobalVar, GlobalVarDeclID);

    impl_helper_method__get_kind!(get_method, Method, MethodDeclID);

    impl_helper_method__get_kind!(get_func, Func, FuncDeclID);

    impl_helper_method__get_kind!(get_typedef, Typedef, TypedefDeclID);

    impl_helper_method__get_kind!(get_union, Union, UnionDeclID);

    impl_helper_method__get_kind!(get_enum, Enum, EnumDeclID);

    impl_helper_method__get_kind!(get_struct, Struct, StructDeclID);

    impl_helper_method__get_kind!(get_interface, Interface, InterfaceDeclID);

    /// Get the symbol entry for a symbol.
    fn lookup_symbol_entry(&self, symbol_id: SymbolID) -> Option<SymbolEntry> {
        let concrete_symbol_id = self.global_symbols.resolve_concrete_symbol_id(symbol_id);

        self.global_symbols.get_symbol_entry(concrete_symbol_id)
    }

    /// Resolve a symbol ID by name.
    ///
    /// Resolution order:
    /// 1. Local lexical scopes (innermost -> outer)
    /// 2. Global semantic scope (modules, namespaces)
    fn lookup_symbol_id(&self, scope_id: SymbolID, name: &str) -> Option<SymbolID> {
        let scope_id = self.global_symbols.resolve_concrete_scope_id(scope_id);

        if let Some(local_id) = self.lookup_local(name) {
            return Some(local_id);
        }

        self.global_symbols.lookup_in_scope_chain(scope_id, name)
    }

    fn lookup_symbol_id_in_scope(&self, scope_id: SymbolID, name: &str) -> Option<SymbolID> {
        let scope_id = self.global_symbols.resolve_concrete_scope_id(scope_id);

        self.global_symbols.lookup_symbol_id_in_scope(scope_id, name)
    }

    fn lookup_module_name(&self, file_id: FileID) -> Option<String> {
        let map = self.module_names.lock().unwrap();
        map.get(&file_id).cloned()
    }

    fn lookup_symbol_as_decl_id(&self, symbol_id: SymbolID) -> Option<DeclID> {
        let symbol_entry = self.lookup_symbol_entry(symbol_id)?;

        match symbol_entry.kind {
            SymbolEntryKind::Var(var_decl_id) => Some(DeclID::Var(var_decl_id)),
            SymbolEntryKind::GlobalVar(global_var_decl_id) => Some(DeclID::GlobalVar(global_var_decl_id)),
            SymbolEntryKind::Func(func_decl_id) => Some(DeclID::Func(func_decl_id)),
            SymbolEntryKind::Method(method_decl_id) => Some(DeclID::Method(method_decl_id)),
            SymbolEntryKind::Struct(struct_decl_id) => Some(DeclID::Struct(struct_decl_id)),
            SymbolEntryKind::Enum(enum_decl_id) => Some(DeclID::Enum(enum_decl_id)),
            SymbolEntryKind::Union(union_decl_id) => Some(DeclID::Union(union_decl_id)),
            SymbolEntryKind::Interface(interface_decl_id) => Some(DeclID::Interface(interface_decl_id)),
            SymbolEntryKind::Typedef(typedef_decl_id) => Some(DeclID::Typedef(typedef_decl_id)),

            SymbolEntryKind::Namespace(_)
            | SymbolEntryKind::Module(_)
            | SymbolEntryKind::Unresolved
            | SymbolEntryKind::ProxiedSymbol { .. }
            | SymbolEntryKind::ProxiedModule { .. } => None,
        }
    }
}

impl IDGen {
    pub fn new() -> Self {
        Self {
            next_label_id: AtomicU32::new(1),
        }
    }

    #[inline(always)]
    pub fn label_id(&self) -> LabelID {
        LabelID(self.next_label_id.fetch_add(1, Ordering::Relaxed))
    }
}

unsafe impl Send for Resolver {}
unsafe impl Sync for Resolver {}
