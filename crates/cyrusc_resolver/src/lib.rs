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
use crate::sigs::{EnumSig, GlobalVarSig, InterfaceSig, StructSig, UnionSig};
use crate::symbols::*;
use crate::{
    diagnostics::ResolverDiagKind,
    sigs::{FuncSig, TypedefSig},
};
use cyrusc_abi::modulename::make_module_name_from_filepath;
use cyrusc_abi::visibility::Visibility;
use cyrusc_ast::format::module_segments_as_string;
use cyrusc_ast::*;
use cyrusc_diagcentral::source_loc::SourceLoc;
use cyrusc_diagcentral::{reporter::DiagReporter, *};
use cyrusc_modulefsloader::{ModuleAlias, ModuleLoader, ModuleLoaderOptions};
use cyrusc_tast::exprs::*;
use cyrusc_tast::generics::generic_type::GenericType;
use cyrusc_tast::generics::mapping_ctx::GenericMappingCtx;
use cyrusc_tast::generics::mapping_ctx_arena::GenericMappingCtxArena;
use cyrusc_tast::generics::monomorph::MonomorphRegistry;
use cyrusc_tast::stmts::*;
use cyrusc_tast::types::*;
use cyrusc_tast::*;
use cyrusc_tokens::literals::{Literal, LiteralKind, StringPrefix};
use cyrusc_tokens::loc::{Location, Span};
use cyrusc_tokens::{Token, TokenKind};
use rand::Rng;
use std::collections::HashSet;
use std::hash::Hash;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

mod diagnostics;
pub mod macros;
pub mod symbols;
pub mod utility;

pub type GlobalSymbolsMutex = Mutex<HashMap<ModuleID, SymbolTable>>;
type ModuleGroupName = String;
type ImportedModules = HashMap<ModuleGroupName, ModuleID>;

pub struct Resolver {
    // symbol table that holds
    pub global_symbols: Arc<GlobalSymbolsMutex>,

    // Holds the analyzed modules which is used to prevent analyzing a module multiple times.
    pub analyzed_modules: Arc<Mutex<HashSet<ModuleID>>>,

    // Holds the program trees of the modules that are analyzed successfully.
    pub program_trees: Arc<Mutex<Vec<Rc<ProgramTreeEntry>>>>,

    // Holds file path related to the module.
    pub file_paths: Arc<Mutex<HashMap<ModuleID, String>>>,

    // Diagnostic Reporter Instance.
    pub reporter: DiagReporter,

    // TODO: Consider to inject ModuleLoader via an implementation of a trait instead of direct injection.
    pub module_loader: ModuleLoader,

    // TODO: Explain why these two are necessary in resolver layer and what we are doing with them here.
    pub monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,

    // Holds the file path of entry module.
    master_module_file_path: String,

    // Holds the imported modules by current module by their alias.
    module_aliases: Arc<Mutex<HashMap<ModuleID, ImportedModules>>>,

    // Holds a list of imported modules by current module.
    imported_modules: HashSet<ImportedModuleEntry>,

    // Holds reference to current module.
    current_module: Option<ModuleID>,

    // Used to resolve self type.
    current_object: Option<SymbolID>,

    // Used to resolve generic params.
    current_object_generic_params: Option<TypedGenericParamsList>,
}

pub struct ProgramTreeEntry {
    pub module_name: String,
    pub module_path: String,
    pub module_id: ModuleID,
    pub program: Rc<RefCell<TypedProgramTree>>,
}

// Track previously imported module + alias
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ImportedModuleEntry {
    module_file_path: String,
    alias: ModuleAlias,
}

// Used to check import cycles.
pub struct Visiting {
    pub active: HashSet<String>, // stack of modules currently being resolved
    pub done: HashSet<String>,   // modules fully resolved
}

macro_rules! is_unscoped_expr {
    ($self:expr, $local_scope_opt:expr, $loc:expr, $span_end:expr) => {{
        if $local_scope_opt.is_none() {
            $self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::RequiresLocalScope),
                location: Some(DiagLoc::new(SourceLoc::from_loc($loc, $self.current_file_path()))),
                hint: None,
            });
        }

        $local_scope_opt.is_none()
    }};
}

impl Resolver {
    pub fn new(
        opts: ModuleLoaderOptions,
        monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
        mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
        master_module_file_path: String,
    ) -> Self {
        let file_paths = Arc::new(Mutex::new(HashMap::new()));

        Self {
            global_symbols: Arc::new(Mutex::new(HashMap::new())),
            analyzed_modules: Arc::new(Mutex::new(HashSet::new())),
            module_aliases: Arc::new(Mutex::new(HashMap::new())),
            file_paths: file_paths.clone(),
            program_trees: Arc::new(Mutex::new(Vec::new())),
            monomorph_registry,
            imported_modules: HashSet::new(),
            reporter: DiagReporter::new(),
            module_loader: ModuleLoader::new(opts),
            current_module: None,
            current_object: None,
            current_object_generic_params: None,
            master_module_file_path,
            mapping_ctx_arena,
        }
    }

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
        mut visiting: &mut Visiting,
        is_master: bool,
        module_file_path: String,
    ) -> Option<Rc<RefCell<TypedProgramTree>>> {
        self.current_module = Some(module_id);
        self.init_imported_modules_for_module();

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
        let typed_body = self.resolve_definitions(module_id, &ast);

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

    fn skip_module_if_loaded_once(&self, file_path: String) -> bool {
        let file_paths = self.file_paths.lock().unwrap();
        let exists = file_paths.iter().find(|(_, fp)| **fp == file_path).is_some();
        drop(file_paths);
        exists
    }

    /// Resolves a module import with duplicate and cycle detection.
    fn resolve_import(&mut self, parent_module_id: ModuleID, import: Import, visiting: &mut Visiting) {
        let current_module_file_path = self.resolve_module_file_path(parent_module_id).unwrap();
        let loaded_modules_list = self.module_loader.load_module(&import);

        for loaded_module in loaded_modules_list {
            let loaded_module = match loaded_module {
                Ok(m) => m,
                Err(diag_kind) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(diag_kind),
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            import.loc.clone(),
                            current_module_file_path.clone(),
                        ))),
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
                        module_names: visiting.active.iter().cloned().collect(),
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
                        module_path: module_file_path,
                        module_id,
                        program: typed_program_tree,
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

    fn init_imported_modules_for_module(&mut self) {
        let mut module_aliases = self.module_aliases.lock().unwrap();
        module_aliases.insert(self.current_module.unwrap(), HashMap::new());
        drop(module_aliases);
    }

    fn resolve_type_args(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        type_args: &TypeArgs,
        loc: Location,
        span_end: usize,
    ) -> Option<TypedTypeArgs> {
        type_args
            .iter()
            .enumerate()
            .map(|(idx, type_arg)| match type_arg {
                TypeArg::Positional(type_specifier) => {
                    let ty = self.resolve_type(
                        generic_params,
                        local_scope_opt.clone(),
                        module_id,
                        type_specifier.clone(),
                        loc.clone(),
                        span_end,
                    )?;

                    Some(TypedTypeArg::Positional {
                        idx,
                        ty,
                        loc: SourceLoc::from_loc(type_specifier.loc().0, self.current_file_path()),
                    })
                }
                TypeArg::Named { key, ty } => {
                    let ty = self.resolve_type(
                        generic_params,
                        local_scope_opt.clone(),
                        module_id,
                        ty.clone(),
                        loc.clone(),
                        span_end,
                    )?;
                    Some(TypedTypeArg::Named {
                        key: key.as_string(),
                        ty,
                        loc: SourceLoc::from_loc(key.loc.clone(), self.current_file_path()),
                    })
                }
            })
            .collect::<Option<_>>()
    }

    fn resolve_generic_params(&mut self, generic_params: &GenericParamsList) -> Option<TypedGenericParamsList> {
        let list = generic_params
            .iter()
            .map(|generic_param| {
                let bounds = if let Some(bounds_list) = &generic_param.bounds {
                    let resolved_bounds = bounds_list
                        .iter()
                        .map(|bound| {
                            Some(TypedBound {
                                symbol: bound.symbol.clone(),
                                type_args: self.resolve_type_args(
                                    &None,
                                    self.current_module?,
                                    None,
                                    &bound.type_args,
                                    generic_param.param_name.loc.clone(),
                                    generic_param.param_name.span.end,
                                )?,
                            })
                        })
                        .collect::<Option<Vec<_>>>()?;

                    Some(resolved_bounds)
                } else {
                    None
                };

                let default = if let Some(default_type_specifier) = &generic_param.default {
                    Some(Box::new(self.resolve_type(
                        &None,
                        None,
                        self.current_module?,
                        default_type_specifier.clone(),
                        generic_param.param_name.loc.clone(),
                        generic_param.param_name.span.end,
                    )?))
                } else {
                    None
                };

                Some(TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: generic_param.param_name.as_string(),
                        symbol_id: generate_symbol_id(),
                        loc: SourceLoc::from_loc(generic_param.param_name.loc.clone(), self.current_file_path()),
                    },
                    bounds,
                    default,
                })
            })
            .collect::<Option<Vec<_>>>()?;

        Some(TypedGenericParamsList { list })
    }

    fn resolve_generic_param_as_type(
        &mut self,
        generic_params_list_opt: &Option<TypedGenericParamsList>,
        ident: &Ident,
    ) -> Option<SemanticType> {
        generic_params_list_opt
            .clone()
            .and_then(|generic_params| {
                generic_params
                    .list
                    .iter()
                    .find(|param| param.param_name.name == ident.as_string())
                    .and_then(|generic_param| Some(SemanticType::GenericParam(generic_param.clone())))
            })
            .or({
                self.current_object_generic_params.as_ref().and_then(|generic_params| {
                    generic_params
                        .lookup_named(&ident.value)
                        .map(|generic_param| SemanticType::GenericParam(generic_param.clone()))
                })
            })
    }

    fn resolve_type(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        local_scope_opt: Option<LocalScopeRef>,
        module_id: ModuleID,
        type_specifier: TypeSpecifier,
        loc: Location,
        span_end: usize,
    ) -> Option<SemanticType> {
        let result = match &type_specifier {
            TypeSpecifier::GenericInst(generic_inst) => {
                let base = self.resolve_type(
                    generic_params,
                    local_scope_opt.clone(),
                    module_id,
                    *generic_inst.base.clone(),
                    loc.clone(),
                    span_end,
                )?;

                let is_const = base.is_const();
                if let Some(symbol_id) = base.const_inner().as_unresolved_symbol() {
                    let type_args = self.resolve_type_args(
                        generic_params,
                        module_id,
                        local_scope_opt,
                        &generic_inst.type_args,
                        loc.clone(),
                        span_end,
                    )?;

                    Ok(SemanticType::GenericType(GenericType {
                        base: symbol_id,
                        type_args: Some(type_args),
                        mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
                        mapping_ctx_arena: self.mapping_ctx_arena.clone(),
                        generic_params: TypedGenericParamsList::new(),
                        is_const,
                        loc: SourceLoc::from_loc(generic_inst.loc.clone(), self.current_file_path()),
                    }))
                } else {
                    Err(ResolverDiagKind::TypeDoesNotAcceptTypeArgs {
                        type_name: generic_inst.base.to_string(),
                    })
                }
            }
            TypeSpecifier::Tuple(tuple_type) => {
                let type_list: Vec<SemanticType> = tuple_type
                    .type_list
                    .iter()
                    .map(|elem| {
                        self.resolve_type(
                            generic_params,
                            local_scope_opt.clone(),
                            module_id,
                            elem.clone(),
                            loc.clone(),
                            span_end,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;

                Ok(SemanticType::Tuple(TypedTupleType {
                    type_list,
                    loc: SourceLoc::from_loc(tuple_type.loc.clone(), self.current_file_path()),
                }))
            }
            TypeSpecifier::FuncType(func_type) => {
                let params: Vec<SemanticType> = func_type
                    .params
                    .list
                    .iter()
                    .map(|param| {
                        self.resolve_type(
                            generic_params,
                            local_scope_opt.clone(),
                            module_id,
                            param.clone(),
                            loc.clone(),
                            span_end,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;

                let variadic = match &func_type.params.variadic {
                    Some(FuncTypeVariadicParams::UntypedCStyle) => {
                        Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle))
                    }
                    Some(FuncTypeVariadicParams::Typed(spec)) => {
                        let ct = self.resolve_type(
                            generic_params,
                            local_scope_opt.clone(),
                            module_id,
                            spec.clone(),
                            loc.clone(),
                            span_end,
                        )?;
                        Some(Box::new(TypedFuncTypeVariadicParams::Typed(ct)))
                    }
                    None => None,
                };

                let return_type = self.resolve_type(
                    generic_params,
                    local_scope_opt.clone(),
                    module_id,
                    *func_type.return_type.clone(),
                    loc.clone(),
                    span_end,
                )?;

                Ok(SemanticType::FuncType(TypedFuncType {
                    symbol_id: None,
                    def_module_id: None,
                    params: TypedFuncTypeParams { list: params, variadic },
                    return_type: Box::new(return_type),
                    is_public: true,
                    loc: SourceLoc::from_loc(loc.clone(), self.current_file_path()),
                }))
            }
            TypeSpecifier::TypeToken(token) => {
                SemanticType::try_from(token.kind.clone()).map_err(|_| ResolverDiagKind::TypeNotFound {
                    name: token.kind.to_string(),
                })
            }
            TypeSpecifier::Const(inner) => {
                let inner = self.resolve_type(
                    generic_params,
                    local_scope_opt,
                    module_id,
                    *inner.clone(),
                    loc.clone(),
                    span_end,
                )?;
                Ok(SemanticType::Const(Box::new(inner)))
            }
            TypeSpecifier::Deref(inner) => {
                let inner = self.resolve_type(
                    generic_params,
                    local_scope_opt,
                    module_id,
                    *inner.clone(),
                    loc.clone(),
                    span_end,
                )?;
                Ok(SemanticType::Pointer(Box::new(inner)))
            }
            TypeSpecifier::Array(array_type) => {
                let element_type = self.resolve_type(
                    generic_params,
                    local_scope_opt.clone(),
                    module_id,
                    *array_type.element_type.clone(),
                    loc.clone(),
                    span_end,
                )?;

                let capacity = match &array_type.size {
                    ArrayCapacity::Fixed(expr) => {
                        let typed_expr = self.resolve_expr(module_id, local_scope_opt.clone(), expr)?;
                        if let TypedExprKind::Literal(lit) = &typed_expr.kind {
                            if let LiteralKind::Integer(value, ..) = &lit.kind {
                                TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(
                                    (*value).try_into().unwrap(),
                                ))
                            } else {
                                TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Expr(Box::new(typed_expr)))
                            }
                        } else {
                            TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Expr(Box::new(typed_expr)))
                        }
                    }
                    ArrayCapacity::Dynamic => TypedArrayCapacity::Dynamic,
                };

                Ok(SemanticType::Array(TypedArrayType {
                    element_type: Box::new(element_type),
                    capacity,
                    loc: SourceLoc::from_loc(loc.clone(), self.current_file_path()),
                }))
            }
            TypeSpecifier::UnnamedStruct(struct_spec) => {
                let mut fields = Vec::new();

                for field in &struct_spec.fields {
                    if let Some(ty) = self.resolve_type(
                        generic_params,
                        local_scope_opt.clone(),
                        module_id,
                        field.field_ty.clone(),
                        field.loc.clone(),
                        field.span.end,
                    ) {
                        fields.push(TypedUnnamedStructTypeField {
                            name: field.field_name.value.clone(),
                            ty: Box::new(ty),
                            loc: SourceLoc::from_loc(field.loc.clone(), self.current_file_path()),
                        });
                    }
                }

                Ok(SemanticType::UnnamedStruct(TypedUnnamedStructType {
                    fields,
                    is_packed: struct_spec.is_packed,
                    loc: SourceLoc::from_loc(struct_spec.loc.clone(), self.current_file_path()),
                }))
            }
            TypeSpecifier::ModuleImport(module_import) => self
                .resolve_module_import(module_id, module_import.clone())
                .map(SemanticType::UnresolvedSymbol)
                .ok_or(ResolverDiagKind::TypeNotFound {
                    name: module_import.to_string(),
                }),
            TypeSpecifier::Ident(ident) => {
                let mut sema_ty_opt: Option<SemanticType> = self.resolve_generic_param_as_type(generic_params, ident);

                if sema_ty_opt.is_none() {
                    sema_ty_opt = self
                        .lookup_symbol_id(module_id, &ident.value)
                        .map(SemanticType::UnresolvedSymbol);
                }

                if sema_ty_opt.is_none() {
                    sema_ty_opt = local_scope_opt
                        .and_then(|scope_rc| self.resolve_symbol_id_from_local_scope(scope_rc, &ident.value))
                        .map(SemanticType::UnresolvedSymbol);
                }

                if let Some(sema_ty) = sema_ty_opt {
                    Ok(sema_ty)
                } else {
                    Err(ResolverDiagKind::TypeNotFound {
                        name: ident.value.clone(),
                    })
                }
            }
            TypeSpecifier::SelfType(self_type) => Ok(SemanticType::SelfType(TypedSelfType {
                loc: SourceLoc::from_loc(self_type.loc.clone(), self.current_file_path()),
            })),
        };

        match result {
            Ok(ct) => Some(ct),
            Err(kind) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(kind),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.current_file_path()))),
                    hint: None,
                });
                None
            }
        }
    }

    fn resolve_typedef_stmt(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        typedef: &Typedef,
    ) -> Option<TypedStmt> {
        let symbol_id = self
            .lookup_symbol_id(module_id, &typedef.ident.value)
            .unwrap_or_else(|| generate_symbol_id());

        let generic_params = typedef
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        let sema_ty = self.resolve_type(
            &generic_params,
            local_scope_opt.clone(),
            module_id,
            typedef.type_specifier.clone(),
            typedef.loc.clone(),
            typedef.span.end,
        )?;

        let typedef_sig = TypedefSig {
            name: typedef.ident.value.clone(),
            generic_params,
            ty: sema_ty.clone(),
            vis: typedef.vis.clone(),
            loc: SourceLoc::from_loc(typedef.loc.clone(), self.current_file_path()),
        };

        let resolved_typedef = ResolvedTypedef {
            module_id,
            symbol_id,
            typedef_sig: typedef_sig.clone(),
        };

        if let Some(scope_rc) = &local_scope_opt {
            scope_rc.borrow_mut().insert(
                typedef.ident.value.clone(),
                LocalSymbol::new(LocalSymbolKind::Typedef(resolved_typedef)),
            );
        } else {
            self.insert_symbol_entry(
                module_id,
                symbol_id,
                SymbolEntry::new(SymbolEntryKind::Typedef(resolved_typedef)),
            );
        }

        let generic_params = typedef
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        Some(TypedStmt::Typedef(TypedTypedefStmt {
            symbol_id,
            name: typedef.ident.value.clone(),
            ty: sema_ty,
            generic_params,
            vis: typedef.vis.clone(),
            loc: typedef_sig.loc,
        }))
    }

    fn insert_symbol_entry(&mut self, module_id: ModuleID, symbol_id: SymbolID, entry: SymbolEntry) {
        let mut global_symbols = self.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get_mut(&module_id).unwrap();
        symbol_table.entries.insert(symbol_id, entry);
        drop(global_symbols);
    }

    fn insert_symbol_name(&mut self, module_id: ModuleID, name: &String) -> SymbolID {
        let symbol_id = generate_symbol_id();
        let mut global_symbols = self.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get_mut(&module_id).unwrap();
        symbol_table.names.insert(name.clone(), symbol_id);
        drop(global_symbols);
        symbol_id
    }

    // Scans the top-level AST for declarations (typedefs, functions, structs, etc.)
    // And Registers each declared name into the current module’s symbol table. (first pass)
    fn resolve_decl_names(&mut self, module_id: ModuleID, ast: &ProgramTree) {
        for stmt in ast.body.as_ref() {
            match stmt {
                Stmt::Interface(interface) => {
                    if self.duplicate_symbol(
                        module_id,
                        interface.ident.value.clone(),
                        SourceLoc::from_loc(interface.loc.clone(), self.current_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &interface.ident.value.clone());
                }
                Stmt::Union(union_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        union_decl.ident.value.clone(),
                        SourceLoc::from_loc(union_decl.loc.clone(), self.current_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &union_decl.ident.value.clone());
                }
                Stmt::Typedef(typedef) => {
                    if self.duplicate_symbol(
                        module_id,
                        typedef.ident.value.clone(),
                        SourceLoc::from_loc(typedef.loc.clone(), self.current_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &typedef.ident.value.clone());
                }
                Stmt::FuncDef(func_def) => {
                    if self.duplicate_symbol(
                        module_id,
                        func_def.ident.value.clone(),
                        SourceLoc::from_loc(func_def.loc.clone(), self.current_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &func_def.ident.value.clone());
                }
                Stmt::FuncDecl(func_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        func_decl.ident.value.clone(),
                        SourceLoc::from_loc(func_decl.loc.clone(), self.current_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &func_decl.usable_name());
                }
                Stmt::GlobalVar(global_variable) => {
                    if self.duplicate_symbol(
                        module_id,
                        global_variable.ident.value.clone(),
                        SourceLoc::from_loc(global_variable.loc.clone(), self.current_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &global_variable.ident.value.clone());
                }
                Stmt::Struct(struct_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        struct_decl.ident.value.clone(),
                        SourceLoc::from_loc(struct_decl.loc.clone(), self.current_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &struct_decl.ident.value.clone());
                }
                Stmt::Enum(enum_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        enum_decl.ident.value.clone(),
                        SourceLoc::from_loc(enum_decl.loc.clone(), self.current_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &enum_decl.ident.value.clone());
                }
                _ => {}
            };
        }
    }

    // Resolves the full meaning of each top-level declaration in the AST (second pass)
    fn resolve_definitions(&mut self, module_id: ModuleID, ast: &ProgramTree) -> Vec<TypedStmt> {
        let mut typed_body: Vec<TypedStmt> = Vec::new();

        for stmt in ast.body.as_ref() {
            let valid_top_level_stmt: Result<TypedStmt, SourceLoc> = match stmt {
                Stmt::Import(..) => continue,
                Stmt::GlobalVar(global_var) => match self.resolve_global_var_stmt(module_id, global_var) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Stmt::Typedef(typedef) => match self.resolve_typedef_stmt(module_id, None, typedef) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Stmt::FuncDef(func_def) => match self.resolve_func_def_stmt(module_id, func_def) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Stmt::FuncDecl(func_decl) => match self.resolve_func_decl_stmt(module_id, func_decl) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Stmt::Struct(struct_decl) => match self.resolve_struct_stmt(module_id, None, struct_decl, None) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Stmt::Enum(enum_decl) => match self.resolve_enum_stmt(module_id, None, enum_decl, None) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Stmt::Interface(interface) => match self.resolve_interface_stmt(module_id, None, interface) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Stmt::Union(union_stmt) => match self.resolve_union_stmt(module_id, None, union_stmt, None) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Stmt::Variable(variable) => Err(SourceLoc::from_loc(variable.loc.clone(), self.current_file_path())),
                Stmt::If(if_stmt) => Err(SourceLoc::from_loc(if_stmt.loc.clone(), self.current_file_path())),
                Stmt::Return(return_stmt) => {
                    Err(SourceLoc::from_loc(return_stmt.loc.clone(), self.current_file_path()))
                }
                Stmt::For(for_stmt) => Err(SourceLoc::from_loc(for_stmt.loc.clone(), self.current_file_path())),
                Stmt::Foreach(foreach) => Err(SourceLoc::from_loc(foreach.loc.clone(), self.current_file_path())),
                Stmt::Switch(switch) => Err(SourceLoc::from_loc(switch.loc.clone(), self.current_file_path())),
                Stmt::BlockStmt(block_statement) => Err(SourceLoc::from_loc(
                    block_statement.loc.clone(),
                    self.current_file_path(),
                )),
                Stmt::Break(break_stmt) => Err(SourceLoc::from_loc(break_stmt.loc.clone(), self.current_file_path())),
                Stmt::Continue(continue_stmt) => {
                    Err(SourceLoc::from_loc(continue_stmt.loc.clone(), self.current_file_path()))
                }
                Stmt::Defer(defer) => Err(SourceLoc::from_loc(defer.loc.clone(), self.current_file_path())),
                Stmt::While(while_stmt) => Err(SourceLoc::from_loc(while_stmt.loc.clone(), self.current_file_path())),
                Stmt::ExportTuple(export_tuple_values) => Err(SourceLoc::from_loc(
                    export_tuple_values.loc.clone(),
                    self.current_file_path(),
                )),
                Stmt::Label(label) => Err(SourceLoc::from_loc(label.loc.clone(), self.current_file_path())),
                Stmt::Goto(goto) => Err(SourceLoc::from_loc(goto.loc.clone(), self.current_file_path())),
                Stmt::Expr(..) => continue,
            };

            match valid_top_level_stmt {
                Ok(typed_stmt) => {
                    typed_body.push(typed_stmt);
                }
                Err(loc) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::InvalidTopLevelStatement),
                        location: Some(DiagLoc::new(loc)),
                        hint: None,
                    });
                }
            };
        }

        typed_body
    }

    fn resolve_interface_stmt(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        interface: &Interface,
    ) -> Option<TypedStmt> {
        let interface_symbol_id = local_scope_opt
            .as_ref()
            .map(|_| generate_symbol_id())
            .unwrap_or_else(|| self.lookup_symbol_id(module_id, &interface.ident.value).unwrap());

        let typed_methods: Vec<TypedFuncDeclStmt> = interface
            .methods
            .iter()
            .filter_map(|func_decl| {
                if func_decl.renamed_as.is_some() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::RenameInterfaceMethod),
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            func_decl.loc.clone(),
                            self.current_file_path(),
                        ))),
                        hint: None,
                    });
                }

                let (return_type, typed_func_params, typed_variadic_param, generic_params) =
                    self.resolve_func(module_id, local_scope_opt.clone(), func_decl)?;

                Some(TypedFuncDeclStmt {
                    module_id: self.current_module.unwrap(),
                    symbol_id: interface_symbol_id,
                    name: func_decl.ident.value.clone(),
                    generic_params,
                    params: TypedFuncParams {
                        list: typed_func_params,
                        variadic: typed_variadic_param,
                    },
                    return_type,
                    modifiers: func_decl.modifiers.clone(),
                    renamed_as: None,
                    loc: SourceLoc::from_loc(func_decl.loc.clone(), self.current_file_path()),
                })
            })
            .collect();

        let resolved_interface = ResolvedInterface {
            module_id,
            symbol_id: interface_symbol_id,
            interface_sig: InterfaceSig {
                module_id,
                symbol_id: interface_symbol_id,
                name: interface.ident.value.clone(),
                methods: typed_methods.clone(),
                vis: interface.vis.clone(),
                loc: SourceLoc::from_loc(interface.loc.clone(), self.current_file_path()),
            },
        };

        match local_scope_opt {
            Some(scope_rc) => {
                scope_rc.borrow_mut().insert(
                    interface.ident.value.clone(),
                    LocalSymbol::new(LocalSymbolKind::Interface(resolved_interface)),
                );
            }
            None => {
                self.insert_symbol_entry(
                    module_id,
                    interface_symbol_id,
                    SymbolEntry::new(SymbolEntryKind::Interface(resolved_interface)),
                );
            }
        }

        Some(TypedStmt::Interface(TypedInterfaceStmt {
            name: interface.ident.value.clone(),
            symbol_id: interface_symbol_id,
            methods: typed_methods,
            vis: interface.vis.clone(),
            loc: SourceLoc::from_loc(interface.loc.clone(), self.current_file_path()),
        }))
    }

    fn resolve_union_stmt(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        union_decl: &Union,
        is_local: Option<ScopeID>,
    ) -> Option<TypedStmt> {
        let union_symbol_id = local_scope_opt
            .as_ref()
            .map(|_| generate_symbol_id())
            .unwrap_or_else(|| self.lookup_symbol_id(module_id, &union_decl.ident.value).unwrap());

        self.current_object = Some(union_symbol_id);

        let mut typed_union_fields: Vec<TypedUnionField> = Vec::new();

        let generic_params = union_decl
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        self.current_object_generic_params = generic_params.clone();

        for field in &union_decl.fields {
            match self.resolve_type(
                &generic_params,
                local_scope_opt.clone(),
                module_id,
                field.ty.clone(),
                field.loc.clone(),
                field.span.end,
            ) {
                Some(sema_ty) => {
                    typed_union_fields.push(TypedUnionField {
                        name: field.ident.value.clone(),
                        ty: sema_ty,
                        loc: SourceLoc::from_loc(field.loc.clone(), self.current_file_path()),
                    });
                }
                None => continue,
            }
        }

        self.check_duplicate_method_names(&union_decl.ident.value, union_decl.methods.clone());

        let methods = match self.resolve_methods(
            module_id,
            &union_decl.methods,
            union_symbol_id,
            generic_params.is_some(),
        ) {
            Some(methods) => methods,
            None => return None,
        };

        let resolved_union = ResolvedUnion {
            module_id,
            symbol_id: union_symbol_id,
            union_sig: UnionSig {
                symbol_id: union_symbol_id,
                name: union_decl.ident.value.clone(),
                fields: typed_union_fields.clone(),
                methods: methods.clone(),
                generic_params: generic_params.clone(),
                modifiers: union_decl.modifiers.clone(),
                loc: SourceLoc::from_loc(union_decl.loc.clone(), self.current_file_path()),
            },
        };

        if let Some(scope_rc) = &local_scope_opt {
            let mut scope = scope_rc.borrow_mut();
            scope.insert(
                union_decl.ident.value.clone(),
                LocalSymbol::new(LocalSymbolKind::Union(resolved_union)),
            );
            drop(scope);
        } else {
            self.insert_symbol_entry(
                module_id,
                union_symbol_id,
                SymbolEntry::new(SymbolEntryKind::Union(resolved_union)),
            );
        }

        let impls = self.resolve_object_impls(local_scope_opt.clone(), module_id, &union_decl.impls);

        Some(TypedStmt::Union(TypedUnionStmt {
            symbol_id: union_symbol_id,
            module_id,
            name: union_decl.ident.value.clone(),
            fields: typed_union_fields,
            methods,
            generic_params,
            modifiers: union_decl.modifiers.clone(),
            impls,
            loc: SourceLoc::from_loc(union_decl.ident.loc.clone(), self.current_file_path()),
            is_local: is_local,
        }))
    }

    fn resolve_enum_stmt(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        enum_decl: &Enum,
        is_local: Option<ScopeID>,
    ) -> Option<TypedStmt> {
        let enum_symbol_id = local_scope_opt
            .as_ref()
            .map(|_| generate_symbol_id())
            .unwrap_or_else(|| self.lookup_symbol_id(module_id, &enum_decl.ident.value).unwrap());

        self.current_object = Some(enum_symbol_id);

        let mut variants: Vec<TypedEnumVariant> = Vec::new();

        let generic_params = enum_decl
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        self.current_object_generic_params = generic_params.clone();

        for variant in &enum_decl.variants {
            let typed_variant = match variant {
                EnumVariant::Ident(ident) => TypedEnumVariant::Ident(ident.clone()),
                EnumVariant::Variant(ident, enum_valued_fields) => {
                    let mut fields: Vec<TypedEnumValuedField> = Vec::new();
                    for valued_field in enum_valued_fields {
                        let field_ty = match self.resolve_type(
                            &generic_params,
                            local_scope_opt.clone(),
                            module_id,
                            valued_field.field_ty.clone(),
                            valued_field.loc.clone(),
                            0,
                        ) {
                            Some(sema_ty) => sema_ty,
                            None => continue,
                        };

                        fields.push(TypedEnumValuedField {
                            ty: field_ty,
                            loc: SourceLoc::from_loc(valued_field.loc.clone(), self.current_file_path()),
                        });
                    }
                    TypedEnumVariant::Variant(ident.clone(), fields)
                }
                EnumVariant::Valued(ident, expr) => match self.resolve_expr(
                    module_id,
                    match &local_scope_opt {
                        Some(scope) => Some(Rc::clone(&scope)),
                        None => None,
                    },
                    expr,
                ) {
                    Some(typed_expr) => TypedEnumVariant::Valued(ident.clone(), Box::new(typed_expr)),
                    None => continue,
                },
            };

            variants.push(typed_variant);
        }

        self.check_duplicate_method_names(&enum_decl.ident.value, enum_decl.methods.clone());

        let methods =
            match self.resolve_methods(module_id, &enum_decl.methods, enum_symbol_id, generic_params.is_some()) {
                Some(methods) => methods,
                None => return None,
            };

        let resolved_enum = ResolvedEnum {
            module_id,
            symbol_id: enum_symbol_id,
            enum_sig: EnumSig {
                symbol_id: enum_symbol_id,
                name: enum_decl.ident.value.clone(),
                methods: methods.clone(),
                variants: variants.clone(),
                generic_params: generic_params.clone(),
                modifiers: enum_decl.modifiers.clone(),
                loc: SourceLoc::from_loc(enum_decl.loc.clone(), self.current_file_path()),
            },
        };

        if let Some(scope_rc) = &local_scope_opt {
            let mut scope = scope_rc.borrow_mut();
            scope.insert(
                enum_decl.ident.value.clone(),
                LocalSymbol::new(LocalSymbolKind::Enum(resolved_enum)),
            );
            drop(scope);
        } else {
            self.insert_symbol_entry(
                module_id,
                enum_symbol_id,
                SymbolEntry::new(SymbolEntryKind::Enum(resolved_enum)),
            );
        }

        let impls = self.resolve_object_impls(local_scope_opt.clone(), module_id, &enum_decl.impls);

        Some(TypedStmt::Enum(TypedEnumStmt {
            module_id,
            symbol_id: enum_symbol_id,
            name: enum_decl.ident.value.clone(),
            variants,
            methods,
            generic_params,
            impls,
            modifiers: enum_decl.modifiers.clone(),
            loc: SourceLoc::from_loc(enum_decl.ident.loc.clone(), self.current_file_path()),
            is_local,
        }))
    }

    fn resolve_global_var_stmt(&mut self, module_id: ModuleID, global_var: &GlobalVar) -> Option<TypedStmt> {
        let sema_ty = global_var
            .type_specifier
            .clone()
            .and_then(|ty| self.resolve_type(&None, None, module_id, ty, global_var.loc.clone(), global_var.span.end));

        let typed_expr = global_var
            .expr
            .as_ref()
            .and_then(|expr| self.resolve_expr(module_id, None, expr));

        let symbol_id = self.lookup_symbol_id(module_id, &global_var.ident.value).unwrap();

        let resolved_global_var = ResolvedGlobalVar {
            module_id,
            symbol_id,
            global_var_sig: GlobalVarSig {
                module_id,
                symbol_id,
                name: global_var.ident.value.clone(),
                ty: sema_ty.clone(),
                rhs: typed_expr.clone(),
                analyzed: true,
                is_const: global_var.is_const,
                modifiers: global_var.modifiers.clone(),
                loc: SourceLoc::from_loc(global_var.loc.clone(), self.current_file_path()),
            },
        };

        self.insert_symbol_entry(
            module_id,
            symbol_id,
            SymbolEntry::new(SymbolEntryKind::GlobalVar(resolved_global_var)),
        );

        Some(TypedStmt::GlobalVar(TypedGlobalVarStmt {
            module_id,
            symbol_id,
            name: global_var.ident.value.clone(),
            ty: sema_ty,
            expr: typed_expr,
            modifiers: global_var.modifiers.clone(),
            is_const: global_var.is_const,
            loc: SourceLoc::from_loc(global_var.loc.clone(), self.current_file_path()),
        }))
    }

    fn check_duplicate_method_names(&mut self, struct_name: &str, methods_list: Vec<FuncDef>) {
        let mut method_names: Vec<String> = Vec::new();

        for func_def in methods_list {
            let method_name = func_def.ident.value.clone();

            if method_names.contains(&method_name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::DuplicateMethodName {
                        struct_name: struct_name.to_string(),
                        method_name: method_name.clone(),
                    }),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        func_def.loc.clone(),
                        self.current_file_path(),
                    ))),
                    hint: Some("Consider to rename the method to a different name.".to_string()),
                });
                continue;
            }

            method_names.push(method_name);
        }
    }

    fn resolve_methods(
        &mut self,
        module_id: ModuleID,
        methods_list: &[FuncDef],
        struct_symbol_id: SymbolID,
        generic_object: bool,
    ) -> Option<HashMap<String, SymbolID>> {
        let mut methods: HashMap<String, SymbolID> = HashMap::new();
        let mut method_bodies: HashMap<SymbolID, (LocalScopeRef, Box<BlockStmt>, ScopeID, bool)> = HashMap::new();

        for func_def in methods_list {
            let method_scope_id = generate_scope_id();
            let scope_rc = LocalScope::new(None);
            self.insert_scope_ref(module_id, method_scope_id, Rc::clone(&scope_rc));

            if let Some((return_type, mut typed_func_params, typed_variadic_param, generic_params)) =
                self.resolve_func(module_id, Some(Rc::clone(&scope_rc)), &func_def.as_func_decl())
            {
                let original_method_name = func_def.ident.value.clone();
                let unique_method_name = method_symbol_name_for_struct(struct_symbol_id, original_method_name.clone());

                typed_func_params = typed_func_params
                    .into_iter()
                    .map(|param_kind| match param_kind {
                        TypedFuncParamKind::FuncParam(p) => TypedFuncParamKind::FuncParam(p),
                        TypedFuncParamKind::SelfModifier(mut s) => {
                            s.symbol_id = Some(struct_symbol_id);
                            TypedFuncParamKind::SelfModifier(s)
                        }
                    })
                    .collect();

                let symbol_id = self.insert_symbol_name(module_id, &unique_method_name);

                methods.insert(original_method_name.clone(), symbol_id);

                let func_sig = FuncSig {
                    symbol_id: Some(symbol_id),
                    module_id,
                    name: original_method_name.clone(),
                    is_func_decl: false,
                    generic_params,
                    params: TypedFuncParams {
                        list: typed_func_params,
                        variadic: typed_variadic_param,
                    },
                    return_type,
                    modifiers: func_def.modifiers.clone(),
                    loc: SourceLoc::from_loc(func_def.loc.clone(), self.current_file_path()),
                };

                let is_generic = func_sig.generic_params.is_some() || generic_object;

                let resolved_method = ResolvedMethod {
                    module_id,
                    symbol_id,
                    func_sig,
                    func_body: None,
                };

                self.insert_symbol_entry(
                    module_id,
                    symbol_id,
                    SymbolEntry::new(SymbolEntryKind::Method(resolved_method)),
                );

                method_bodies.insert(
                    symbol_id,
                    (Rc::clone(&scope_rc), func_def.body.clone(), method_scope_id, is_generic),
                );
            }
        }

        for (&symbol_id, (scope_rc, method_body, method_scope_id, is_generic)) in &method_bodies {
            let mut resolved_method = match self.lookup_symbol_entry_with_id(symbol_id).unwrap().kind {
                SymbolEntryKind::Method(m) => m,
                _ => unreachable!(),
            };

            for param in &mut resolved_method.func_sig.params.list {
                if let TypedFuncParamKind::SelfModifier(self_modifier) = param {
                    let self_symbol_id = generate_symbol_id();
                    self_modifier.self_symbol_id = Some(self_symbol_id);

                    let local_symbol = LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                        module_id,
                        symbol_id: self_symbol_id,
                        typed_variable: TypedVarStmt {
                            symbol_id: self_symbol_id,
                            name: "self".to_string(),
                            ty: match self_modifier.kind {
                                SelfModifierKind::Copied => Some(SemanticType::UnresolvedSymbol(struct_symbol_id)),
                                SelfModifierKind::Referenced => Some(SemanticType::Pointer(Box::new(
                                    SemanticType::UnresolvedSymbol(struct_symbol_id),
                                ))),
                            },
                            rhs: None,
                            is_const: false,
                            analyzed: true,
                            loc: resolved_method.func_sig.loc.clone(),
                        },
                    }));

                    scope_rc.borrow_mut().insert("self".to_string(), local_symbol);
                }
            }

            if let Some(typed_func_body) = self.resolve_block_stmt(*method_scope_id, scope_rc.clone(), method_body) {
                if *is_generic {
                    // generic method body is being analyzed when called
                    monomorph_registry!(self, ctx, {
                        ctx.register_template(symbol_id, typed_func_body);
                    });
                } else {
                    resolved_method.func_body = Some(Box::new(typed_func_body));
                }

                self.insert_symbol_entry(
                    module_id,
                    symbol_id,
                    SymbolEntry::new(SymbolEntryKind::Method(resolved_method)),
                );
            } else {
                return None;
            }
        }

        Some(methods)
    }

    fn resolve_object_impls(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        module_id: ModuleID,
        impls: &Vec<ModuleImport>,
    ) -> Vec<TypedIdentifier> {
        let mut symbol_ids: Vec<TypedIdentifier> = Vec::new();

        for module_import in impls {
            let Some(symbol_id) = self.resolve_local_module_import(local_scope_opt.clone(), module_id, module_import)
            else {
                continue;
            };

            symbol_ids.push(TypedIdentifier {
                name: module_import.to_string(),
                symbol_id,
                loc: SourceLoc::from_loc(module_import.loc, self.resolve_module_file_path(module_id).unwrap()),
            });
        }

        symbol_ids
    }

    fn resolve_struct_stmt(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        struct_decl: &Struct,
        is_local: Option<ScopeID>,
    ) -> Option<TypedStmt> {
        let struct_symbol_id = local_scope_opt
            .as_ref()
            .map(|_| generate_symbol_id())
            .unwrap_or_else(|| self.lookup_symbol_id(module_id, &struct_decl.ident.value).unwrap());

        self.current_object = Some(struct_symbol_id);

        let generic_params = struct_decl
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        self.current_object_generic_params = generic_params.clone();

        let typed_struct_fields: Vec<TypedStructField> = struct_decl
            .fields
            .iter()
            .filter_map(|field| {
                self.resolve_type(
                    &generic_params,
                    local_scope_opt.clone(),
                    module_id,
                    field.ty.clone(),
                    field.loc.clone(),
                    field.span.end,
                )
                .map(|ty| TypedStructField {
                    name: field.ident.value.clone(),
                    vis: field.vis.clone(),
                    ty,
                    loc: SourceLoc::from_loc(field.loc.clone(), self.current_file_path()),
                })
            })
            .collect();

        self.check_duplicate_method_names(&struct_decl.ident.value, struct_decl.methods.clone());

        let methods = self.resolve_methods(
            module_id,
            &struct_decl.methods,
            struct_symbol_id,
            generic_params.is_some(),
        )?;
        let impls = self.resolve_object_impls(local_scope_opt.clone(), module_id, &struct_decl.impls);

        let resolved_struct = ResolvedStruct {
            module_id,
            symbol_id: struct_symbol_id,
            struct_sig: StructSig {
                name: struct_decl.ident.value.clone(),
                fields: typed_struct_fields.clone(),
                generic_params: generic_params.clone(),
                impls: impls.clone(),
                is_packed: struct_decl.is_packed,
                methods: methods.clone(),
                modifiers: struct_decl.modifiers.clone(),
                loc: SourceLoc::from_loc(struct_decl.loc.clone(), self.current_file_path()),
            },
        };

        if let Some(scope_rc) = local_scope_opt {
            scope_rc.borrow_mut().insert(
                struct_decl.ident.value.clone(),
                LocalSymbol::new(LocalSymbolKind::Struct(resolved_struct)),
            );
        } else {
            self.insert_symbol_entry(
                module_id,
                struct_symbol_id,
                SymbolEntry::new(SymbolEntryKind::Struct(resolved_struct)),
            );
        }

        Some(TypedStmt::Struct(TypedStructStmt {
            module_id: self.current_module.unwrap(),
            symbol_id: struct_symbol_id,
            name: struct_decl.ident.value.clone(),
            fields: typed_struct_fields,
            methods,
            generic_params,
            modifiers: struct_decl.modifiers.clone(),
            impls,
            is_packed: struct_decl.is_packed,
            loc: SourceLoc::from_loc(struct_decl.loc.clone(), self.current_file_path()),
            is_local,
        }))
    }

    fn resolve_func(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        func_decl: &FuncDecl,
    ) -> Option<(
        SemanticType,
        Vec<TypedFuncParamKind>,
        Option<TypedFuncVariadicParams>,
        Option<TypedGenericParamsList>,
    )> {
        let return_type_specifier =
            return_type_or_void_as_default(func_decl.return_type.clone(), func_decl.loc.clone(), func_decl.span);

        let generic_params = func_decl
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        let return_type = self.resolve_type(
            &generic_params,
            local_scope_opt.clone(),
            module_id,
            return_type_specifier,
            func_decl.loc.clone(),
            func_decl.span.end,
        )?;

        let (typed_func_params, typed_variadic_param) =
            self.resolve_func_params(module_id, local_scope_opt.clone(), &generic_params, &func_decl.params)?;

        Some((return_type, typed_func_params, typed_variadic_param, generic_params))
    }

    fn resolve_func_params(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        generic_params: &Option<TypedGenericParamsList>,
        params: &FuncParams,
    ) -> Option<(Vec<TypedFuncParamKind>, Option<TypedFuncVariadicParams>)> {
        let mut typed_func_params = Vec::with_capacity(params.list.len());

        for param in &params.list {
            match param {
                FuncParamKind::FuncParam(func_param) => {
                    let param_type = match &func_param.ty {
                        Some(type_specifier) => self.resolve_type(
                            generic_params,
                            local_scope_opt.clone(),
                            module_id,
                            type_specifier.clone(),
                            func_param.loc.clone(),
                            func_param.span.end,
                        )?,
                        None => {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(ResolverDiagKind::InvalidUntypedFuncParam),
                                location: Some(DiagLoc::new(SourceLoc::from_loc(
                                    func_param.loc.clone(),
                                    self.current_file_path(),
                                ))),
                                hint: None,
                            });
                            continue;
                        }
                    };

                    let symbol_id = generate_symbol_id();
                    if let Some(scope_rc) = &local_scope_opt {
                        let mut scope = scope_rc.borrow_mut();
                        scope.insert(
                            func_param.ident.value.clone(),
                            LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                                module_id,
                                symbol_id,
                                typed_variable: TypedVarStmt {
                                    symbol_id,
                                    name: func_param.ident.value.clone(),
                                    ty: Some(param_type.clone()),
                                    rhs: None,
                                    is_const: false,
                                    analyzed: true,
                                    loc: SourceLoc::from_loc(func_param.loc.clone(), self.current_file_path()),
                                },
                            })),
                        );
                    }

                    typed_func_params.push(TypedFuncParamKind::FuncParam(TypedFuncParam {
                        symbol_id,
                        name: func_param.ident.value.clone(),
                        ty: param_type,
                        loc: SourceLoc::from_loc(func_param.loc.clone(), self.current_file_path()),
                    }));
                }
                FuncParamKind::SelfModifier(self_modifier) => {
                    typed_func_params.push(TypedFuncParamKind::SelfModifier(TypedSelfModifier {
                        symbol_id: None,
                        self_symbol_id: None,
                        ty: None,
                        kind: self_modifier.kind.clone(),
                        loc: SourceLoc::from_loc(self_modifier.loc.clone(), self.current_file_path()),
                    }));
                }
            }
        }

        let typed_variadic_param = params.variadic.as_ref().and_then(|variadic| match variadic {
            FuncVariadicParams::UntypedCStyle => Some(TypedFuncVariadicParams::UntypedCStyle),
            FuncVariadicParams::Typed(ident, type_specifier) => {
                let variadic_type = self.resolve_type(
                    &None, // FIXME Generic Params
                    local_scope_opt.clone(),
                    module_id,
                    type_specifier.clone(),
                    ident.loc.clone(),
                    ident.span.end,
                )?;

                let symbol_id = generate_symbol_id();

                if let Some(scope_rc) = &local_scope_opt {
                    let mut scope = scope_rc.borrow_mut();
                    scope.insert(
                        ident.value.clone(),
                        LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                            module_id,
                            symbol_id,
                            typed_variable: TypedVarStmt {
                                symbol_id,
                                name: ident.value.clone(),
                                ty: Some(variadic_type.clone()),
                                rhs: None,
                                is_const: false,
                                analyzed: true,
                                loc: SourceLoc::from_loc(ident.loc.clone(), self.current_file_path()),
                            },
                        })),
                    );
                }

                Some(TypedFuncVariadicParams::Typed(
                    TypedIdentifier {
                        name: ident.as_string(),
                        symbol_id,
                        loc: SourceLoc::from_loc(ident.loc.clone(), self.current_file_path()),
                    },
                    variadic_type,
                ))
            }
        });

        Some((typed_func_params, typed_variadic_param))
    }

    fn resolve_func_decl_stmt(&mut self, module_id: ModuleID, func_decl: &FuncDecl) -> Option<TypedStmt> {
        let symbol_id = self.lookup_symbol_id(module_id, &func_decl.usable_name()).unwrap();

        let (return_type, typed_func_params, typed_variadic_param, generic_params) =
            self.resolve_func(module_id, None, func_decl)?;

        let func_sig = FuncSig {
            symbol_id: Some(symbol_id),
            module_id,
            name: func_decl.ident.value.clone(),
            is_func_decl: true,
            generic_params: generic_params.clone(),
            params: TypedFuncParams {
                list: typed_func_params.clone(),
                variadic: typed_variadic_param.clone(),
            },
            return_type: return_type.clone(),
            modifiers: func_decl.modifiers.clone(),
            loc: SourceLoc::from_loc(func_decl.loc.clone(), self.current_file_path()),
        };

        self.insert_symbol_entry(
            module_id,
            symbol_id,
            SymbolEntry::new(SymbolEntryKind::Func(ResolvedFunction {
                module_id,
                symbol_id,
                func_sig,
            })),
        );

        Some(TypedStmt::FuncDecl(TypedFuncDeclStmt {
            module_id,
            symbol_id,
            name: func_decl.ident.value.clone(),
            generic_params,
            params: TypedFuncParams {
                list: typed_func_params,
                variadic: typed_variadic_param,
            },
            return_type,
            modifiers: func_decl.modifiers.clone(),
            renamed_as: func_decl.renamed_as.as_ref().map(|id| id.as_string()),
            loc: SourceLoc::from_loc(func_decl.loc.clone(), self.current_file_path()),
        }))
    }

    fn resolve_func_def_stmt(&mut self, module_id: ModuleID, func_def: &FuncDef) -> Option<TypedStmt> {
        let scope_id = generate_scope_id();
        let body_scope = LocalScope::new(None);
        self.insert_scope_ref(module_id, scope_id, body_scope.clone());

        let symbol_id = self.lookup_symbol_id(module_id, &func_def.ident.value)?;

        let (return_type, typed_func_params, typed_variadic_param, generic_params) =
            self.resolve_func(module_id, Some(body_scope.clone()), &func_def.as_func_decl())?;

        let func_sig = FuncSig {
            symbol_id: Some(symbol_id),
            module_id,
            name: func_def.ident.value.clone(),
            generic_params: generic_params.clone(),
            is_func_decl: false,
            params: TypedFuncParams {
                list: typed_func_params.clone(),
                variadic: typed_variadic_param.clone(),
            },
            return_type: return_type.clone(),
            modifiers: func_def.modifiers.clone(),
            loc: SourceLoc::from_loc(func_def.loc.clone(), self.current_file_path()),
        };

        self.insert_symbol_entry(
            module_id,
            symbol_id,
            SymbolEntry::new(SymbolEntryKind::Func(ResolvedFunction {
                module_id,
                symbol_id,
                func_sig,
            })),
        );

        let typed_func_body = self.resolve_block_stmt(scope_id, body_scope, &func_def.body)?;

        monomorph_registry!(self, ctx, {
            ctx.register_template(symbol_id, typed_func_body.clone());
        });

        Some(TypedStmt::FuncDef(TypedFuncDefStmt {
            symbol_id,
            module_id,
            name: func_def.ident.value.clone(),
            generic_params,
            params: TypedFuncParams {
                list: typed_func_params,
                variadic: typed_variadic_param,
            },
            return_type,
            modifiers: func_def.modifiers.clone(),
            loc: SourceLoc::from_loc(func_def.loc.clone(), self.current_file_path()),
            body: Box::new(typed_func_body),
        }))
    }

    fn declare_local_variable(
        &mut self,
        module_id: ModuleID,
        scope_rc: LocalScopeRef,
        variable: &Variable,
    ) -> Option<TypedVarStmt> {
        if scope_rc.borrow().resolve(&variable.ident.value).is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::DuplicateSymbolInThisScope {
                    symbol_name: variable.ident.value.clone(),
                }),
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    variable.loc.clone(),
                    self.current_file_path(),
                ))),
                hint: None,
            });
            return None;
        }

        let var_type = variable.ty.as_ref().and_then(|ty_spec| {
            self.resolve_type(
                &None,
                Some(scope_rc.clone()),
                module_id,
                ty_spec.clone(),
                variable.loc.clone(),
                variable.span.end,
            )
        });

        let typed_rhs = variable
            .rhs
            .as_ref()
            .and_then(|expr| self.resolve_expr(module_id, Some(scope_rc.clone()), expr));

        let symbol_id = generate_symbol_id();

        let typed_variable = TypedVarStmt {
            symbol_id,
            name: variable.ident.value.clone(),
            ty: var_type.clone(),
            rhs: typed_rhs.clone(),
            is_const: variable.is_const,
            analyzed: typed_rhs.is_some(),
            loc: SourceLoc::from_loc(variable.loc.clone(), self.current_file_path()),
        };

        let resolved_var = ResolvedVariable {
            module_id,
            symbol_id,
            typed_variable: typed_variable.clone(),
        };

        scope_rc.borrow_mut().insert(
            variable.ident.value.clone(),
            LocalSymbol::new(LocalSymbolKind::Variable(resolved_var)),
        );

        Some(typed_variable)
    }

    fn resolve_if_stmt(&mut self, module_id: ModuleID, scope: LocalScopeRef, if_stmt: &If) -> Option<TypedIfStmt> {
        let cond = match self.resolve_expr(module_id, Some(Rc::clone(&scope)), &if_stmt.condition) {
            Some(typed_expr) => typed_expr,
            None => return None,
        };

        let then_block_scope_id = generate_scope_id();
        let then_block_scope = LocalScope::new(Some(scope.clone()));
        self.insert_scope_ref(module_id, then_block_scope_id, then_block_scope.clone());

        let then_block = match self.resolve_block_stmt(then_block_scope_id, then_block_scope, &if_stmt.consequent) {
            Some(typed_block) => Box::new(typed_block),
            None => return None,
        };

        let else_block = {
            if let Some(alternate) = &if_stmt.alternate {
                let else_block_scope_id = generate_scope_id();
                let else_block_scope = LocalScope::new(Some(scope.clone()));
                self.insert_scope_ref(module_id, else_block_scope_id, else_block_scope.clone());

                match self.resolve_block_stmt(else_block_scope_id, else_block_scope.clone(), &*alternate) {
                    Some(typed_block) => Some(Box::new(typed_block)),
                    None => return None,
                }
            } else {
                None
            }
        };

        let mut branches: Vec<TypedIfStmt> = Vec::new();

        for else_if_stmt in &if_stmt.branches {
            let if_stmt = match self.resolve_if_stmt(module_id, scope.clone(), else_if_stmt) {
                Some(typed_if) => typed_if,
                None => continue,
            };

            branches.push(if_stmt);
        }

        Some(TypedIfStmt {
            cond,
            else_block,
            then_block,
            branches,
            loc: SourceLoc::from_loc(if_stmt.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_block_stmt(
        &mut self,
        scope_id: ScopeID,
        scope: LocalScopeRef,
        block_statement: &BlockStmt,
    ) -> Option<TypedBlockStmt> {
        let module_id = self.current_module.unwrap();
        let mut typed_body: Vec<TypedStmt> = Vec::new();
        let mut defers: Vec<TypedDeferStmt> = Vec::new();

        for stmt in &block_statement.exprs {
            match stmt {
                Stmt::Defer(defer) => {
                    if let Some(typed_stmt) = self.resolve_stmt(module_id, scope_id, scope.clone(), &defer.operand) {
                        defers.push(TypedDeferStmt {
                            operand: Box::new(typed_stmt),
                            loc: SourceLoc::from_loc(defer.loc.clone(), self.current_file_path()),
                        });
                    }
                }
                _ => {
                    if let Some(typed_stmt) = self.resolve_stmt(module_id, scope_id, scope.clone(), stmt) {
                        typed_body.push(typed_stmt);
                    }
                }
            }
        }

        Some(TypedBlockStmt {
            scope_id,
            stmts: typed_body,
            defers,
            loc: SourceLoc::from_loc(block_statement.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_export_tuple(
        &mut self,
        module_id: ModuleID,
        scope_id: ScopeID,
        scope: LocalScopeRef,
        export_tuple: &ExportTuple,
    ) -> Option<TypedStmt> {
        let var_type = export_tuple.ty.as_ref().and_then(|ty_spec| {
            self.resolve_type(
                &None,
                Some(scope.clone()),
                module_id,
                ty_spec.clone(),
                export_tuple.loc.clone(),
                export_tuple.span.end,
            )
        });

        let typed_rhs = export_tuple
            .rhs
            .as_ref()
            .and_then(|expr| self.resolve_expr(module_id, Some(scope.clone()), expr));

        let define_identifier = |this: &mut Resolver, ident: &Ident| -> Option<SymbolID> {
            let symbol_id = generate_symbol_id();

            let mut scope_ref = scope.borrow_mut();

            if scope_ref.resolve(&ident.value).is_some() {
                this.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::DuplicateSymbolInThisScope {
                        symbol_name: ident.value.clone(),
                    }),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        ident.loc.clone(),
                        this.current_file_path(),
                    ))),
                    hint: None,
                });
                return None;
            }

            let typed_variable = TypedVarStmt {
                symbol_id,
                name: ident.as_string(),
                ty: None,
                rhs: None,
                is_const: false,
                analyzed: typed_rhs.is_some(),
                loc: SourceLoc::from_loc(ident.loc.clone(), this.current_file_path()),
            };

            let resolved_var = ResolvedVariable {
                module_id,
                symbol_id,
                typed_variable: typed_variable.clone(),
            };

            scope_ref.insert(
                ident.as_string(),
                LocalSymbol::new(LocalSymbolKind::Variable(resolved_var)),
            );

            drop(scope_ref);
            Some(symbol_id)
        };

        let pattern = match &export_tuple.pattern {
            ExportPattern::Ident(ident) => {
                let symbol_id = define_identifier(self, ident)?;
                TypedExportPattern::Ident(symbol_id)
            }
            ExportPattern::Tuple(patterns) => {
                let mut typed_patterns = Vec::new();

                for sub_pattern in patterns {
                    match sub_pattern {
                        ExportPattern::Ident(ident) => {
                            let symbol_id = define_identifier(self, ident)?;
                            typed_patterns.push(TypedExportPattern::Ident(symbol_id));
                        }
                        ExportPattern::Tuple(inner) => {
                            let inner_export = ExportPattern::Tuple(inner.clone());
                            let inner_stmt = self.resolve_export_tuple(
                                module_id,
                                scope_id,
                                scope.clone(),
                                &ExportTuple {
                                    pattern: inner_export,
                                    ty: None,
                                    rhs: None,
                                    is_const: export_tuple.is_const,
                                    loc: export_tuple.loc.clone(),
                                    span: export_tuple.span,
                                },
                            )?;

                            if let TypedStmt::ExportTuple(inner_typed) = inner_stmt {
                                typed_patterns
                                    .push(TypedExportPattern::Tuple(inner_typed.pattern.into_tuple().to_vec()));
                            }
                        }
                    }
                }

                TypedExportPattern::Tuple(typed_patterns)
            }
        };

        Some(TypedStmt::ExportTuple(TypedExportTupleStmt {
            pattern,
            ty: var_type,
            rhs: typed_rhs,
            is_const: export_tuple.is_const,
            loc: SourceLoc::from_loc(export_tuple.loc.clone(), self.current_file_path()),
        }))
    }

    fn resolve_stmt(
        &mut self,
        module_id: ModuleID,
        scope_id: ScopeID,
        scope: LocalScopeRef,
        stmt: &Stmt,
    ) -> Option<TypedStmt> {
        let typed_stmt = match stmt {
            Stmt::ExportTuple(export_tuple) => self.resolve_export_tuple(module_id, scope_id, scope, export_tuple),
            Stmt::Variable(variable) => {
                let typed_var = self.declare_local_variable(module_id, scope.clone(), &variable)?;
                Some(TypedStmt::Variable(typed_var))
            }
            Stmt::Expr(expr) => {
                let typed_expr = self.resolve_expr(module_id, Some(Rc::clone(&scope)), expr)?;
                Some(TypedStmt::Expr(typed_expr))
            }
            Stmt::If(if_stmt) => {
                let typed_if = self.resolve_if_stmt(module_id, Rc::clone(&scope), if_stmt)?;
                Some(TypedStmt::If(typed_if))
            }
            Stmt::Return(return_stmt) => {
                let arg = if let Some(argument) = &return_stmt.argument {
                    Some(self.resolve_expr(module_id, Some(Rc::clone(&scope)), argument)?)
                } else {
                    None
                };
                Some(TypedStmt::Return(TypedReturnStmt {
                    arg,
                    loc: SourceLoc::from_loc(return_stmt.loc.clone(), self.current_file_path()),
                }))
            }
            Stmt::Foreach(..) => todo!(),
            Stmt::For(for_stmt) => {
                let body_scope_id = generate_scope_id();
                let body_scope = LocalScope::new(Some(scope.clone()));
                self.insert_scope_ref(module_id, body_scope_id, body_scope.clone());

                let initializer = if let Some(variable) = &for_stmt.initializer {
                    Some(self.declare_local_variable(module_id, Rc::clone(&body_scope), &variable)?)
                } else {
                    None
                };

                let cond = if let Some(expr) = &for_stmt.condition {
                    Some(self.resolve_expr(module_id, Some(Rc::clone(&body_scope)), expr)?)
                } else {
                    None
                };

                let increment = if let Some(expr) = &for_stmt.increment {
                    Some(self.resolve_expr(module_id, Some(Rc::clone(&body_scope)), expr)?)
                } else {
                    None
                };

                let body = Box::new(self.resolve_block_stmt(body_scope_id, Rc::clone(&body_scope), &*for_stmt.body)?);

                Some(TypedStmt::For(TypedForStmt {
                    initializer,
                    cond,
                    increment,
                    body,
                    loc: SourceLoc::from_loc(for_stmt.loc.clone(), self.current_file_path()),
                }))
            }
            Stmt::While(while_stmt) => {
                let body_scope_id = generate_scope_id();
                let body_scope = LocalScope::new(Some(scope.clone()));
                self.insert_scope_ref(module_id, body_scope_id, body_scope.clone());

                let cond = self.resolve_expr(module_id, Some(Rc::clone(&body_scope)), &while_stmt.condition)?;

                let while_typed_body =
                    Box::new(self.resolve_block_stmt(body_scope_id, Rc::clone(&body_scope), &*while_stmt.body)?);

                Some(TypedStmt::While(TypedWhileStmt {
                    cond,
                    body: while_typed_body,
                    loc: SourceLoc::from_loc(while_stmt.loc.clone(), self.current_file_path()),
                }))
            }
            Stmt::Switch(switch) => {
                let operand = self.resolve_expr(module_id, Some(Rc::clone(&scope)), &switch.operand)?;

                let mut cases: Vec<TypedSwitchCase> = Vec::new();
                for case in &switch.cases {
                    let case_scope_rc = LocalScope::new(Some(scope.clone()));
                    let case_scope_id = generate_scope_id();
                    self.insert_scope_ref(module_id, case_scope_id, case_scope_rc.clone());

                    let mut patterns = Vec::new();

                    for pattern in &case.patterns {
                        let typed_pattern = match pattern {
                            SwitchCasePattern::Expr(expr) => {
                                let typed_expr = self.resolve_expr(module_id, Some(Rc::clone(&scope)), &expr)?;
                                let loc = typed_expr.loc.clone();
                                TypedSwitchCasePattern::Expr(typed_expr, loc)
                            }
                            SwitchCasePattern::Ident(ident) => {
                                let symbol_id = generate_symbol_id();
                                let mut case_scope = case_scope_rc.borrow_mut();
                                case_scope.insert(
                                    ident.value.clone(),
                                    LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                                        module_id,
                                        symbol_id,
                                        typed_variable: TypedVarStmt {
                                            symbol_id,
                                            name: ident.value.clone(),
                                            ty: None,
                                            rhs: None,
                                            is_const: false,
                                            analyzed: true,
                                            loc: SourceLoc::from_loc(ident.loc.clone(), self.current_file_path()),
                                        },
                                    })),
                                );
                                drop(case_scope);

                                TypedSwitchCasePattern::Ident(
                                    ident.value.clone(),
                                    SourceLoc::from_loc(ident.loc.clone(), self.current_file_path()),
                                )
                            }
                            SwitchCasePattern::EnumVariant(ident, valued_fields) => {
                                TypedSwitchCasePattern::EnumVariant(
                                    ident.value.clone(),
                                    valued_fields
                                        .iter()
                                        .map(|ident| {
                                            let symbol_id = generate_symbol_id();
                                            let mut case_scope = case_scope_rc.borrow_mut();
                                            case_scope.insert(
                                                ident.value.clone(),
                                                LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                                                    module_id,
                                                    symbol_id,
                                                    typed_variable: TypedVarStmt {
                                                        symbol_id,
                                                        name: ident.value.clone(),
                                                        ty: None,
                                                        rhs: None,
                                                        is_const: false,
                                                        analyzed: true,
                                                        loc: SourceLoc::from_loc(
                                                            ident.loc.clone(),
                                                            self.current_file_path(),
                                                        ),
                                                    },
                                                })),
                                            );
                                            drop(case_scope);
                                            TypedIdentifier {
                                                name: ident.value.clone(),
                                                symbol_id,
                                                loc: SourceLoc::from_loc(ident.loc.clone(), self.current_file_path()),
                                            }
                                        })
                                        .collect(),
                                    SourceLoc::from_loc(ident.loc.clone(), self.current_file_path()),
                                )
                            }
                            SwitchCasePattern::Range(range) => {
                                let lower = self.resolve_expr(module_id, Some(scope.clone()), &range.lower);
                                let upper = self.resolve_expr(module_id, Some(scope.clone()), &range.upper);

                                TypedSwitchCasePattern::Range(TypedRange {
                                    lower: lower?,
                                    upper: upper?,
                                    inclusive_upper: range.inclusive_upper,
                                    loc: SourceLoc::from_loc(range.loc.clone(), self.current_file_path()),
                                })
                            }
                        };

                        patterns.push(typed_pattern);
                    }

                    let mut body = self.resolve_block_stmt(scope_id, case_scope_rc.clone(), &case.body)?;
                    body.scope_id = case_scope_id;

                    cases.push(TypedSwitchCase {
                        patterns,
                        body: Box::new(body),
                        loc: SourceLoc::from_loc(case.loc.clone(), self.current_file_path()),
                    });
                    drop(case_scope_rc);
                }

                let default_case = if let Some(default_case) = &switch.default_case {
                    let body_scope_id = generate_scope_id();
                    let body_scope = LocalScope::new(Some(scope.clone()));
                    self.insert_scope_ref(module_id, body_scope_id, body_scope.clone());

                    Some(self.resolve_block_stmt(body_scope_id, body_scope.clone(), &default_case)?)
                } else {
                    None
                };

                Some(TypedStmt::Switch(TypedSwitchStmt {
                    operand,
                    cases,
                    default_case,
                    loc: SourceLoc::from_loc(switch.loc.clone(), self.current_file_path()),
                }))
            }
            Stmt::Enum(enum_decl) => {
                let typed_stmt =
                    self.resolve_enum_stmt(module_id, Some(Rc::clone(&scope)), enum_decl, Some(scope_id))?;
                Some(typed_stmt)
            }
            Stmt::Union(union_decl) => {
                let typed_stmt =
                    self.resolve_union_stmt(module_id, Some(Rc::clone(&scope)), union_decl, Some(scope_id))?;
                Some(typed_stmt)
            }
            Stmt::Interface(interface) => {
                let typed_stmt = self.resolve_interface_stmt(module_id, Some(scope.clone()), interface)?;
                Some(typed_stmt)
            }
            Stmt::Struct(struct_decl) => {
                let typed_stmt =
                    self.resolve_struct_stmt(module_id, Some(scope.clone()), struct_decl, Some(scope_id))?;
                Some(typed_stmt)
            }
            Stmt::BlockStmt(block_statement) => {
                let scope_id = generate_scope_id();
                let new_local_scope = LocalScope::new(Some(scope.clone()));

                self.insert_scope_ref(module_id, scope_id, new_local_scope.clone());

                let typed_stmt = self.resolve_block_stmt(scope_id, new_local_scope, block_statement)?;
                Some(TypedStmt::BlockStmt(typed_stmt))
            }
            Stmt::Break(break_stmt) => Some(TypedStmt::Break(TypedBreakStmt {
                loc: SourceLoc::from_loc(break_stmt.loc.clone(), self.current_file_path()),
            })),
            Stmt::Continue(continue_stmt) => Some(TypedStmt::Continue(TypedContinueStmt {
                loc: SourceLoc::from_loc(continue_stmt.loc.clone(), self.current_file_path()),
            })),
            Stmt::Typedef(typedef) => {
                let typed_stmt = self.resolve_typedef_stmt(module_id, Some(scope.clone()), &typedef)?;
                Some(typed_stmt)
            }
            Stmt::GlobalVar(..) | Stmt::FuncDef(..) | Stmt::FuncDecl(..) | Stmt::Import(..) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::InvalidStatement),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(stmt.loc(), self.current_file_path()))),
                    hint: None,
                });
                None
            }
            Stmt::Label(label) => self.resolve_label(scope, label),
            Stmt::Goto(goto) => self.resolve_goto(goto),
            Stmt::Defer(_) => unreachable!(),
        };

        typed_stmt
    }

    fn resolve_goto(&self, goto: &Goto) -> Option<TypedStmt> {
        Some(TypedStmt::Goto(TypedGotoStmt {
            name: goto.name.as_string(),
            label_id: None,
            loc: SourceLoc::from_loc(goto.loc.clone(), self.current_file_path()),
        }))
    }

    fn resolve_label(&mut self, scope: LocalScopeRef, label: &Label) -> Option<TypedStmt> {
        {
            let scope_ref = scope.borrow();
            if scope_ref.resolve_label(&label.name.as_string()).is_some() {
                // label already declare for current scope
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::LabelAlreadyDefined {
                        label_name: label.name.to_string(),
                    }),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        label.loc.clone(),
                        self.current_file_path(),
                    ))),
                    hint: None,
                });
                return None;
            }
        }

        let label_id = generate_label_id();

        {
            let mut scope_ref = scope.borrow_mut();
            scope_ref.insert_label(label.name.as_string(), label_id);
        }

        Some(TypedStmt::Label(TypedLabelStmt {
            name: label.name.as_string(),
            label_id,
            loc: SourceLoc::from_loc(label.loc.clone(), self.current_file_path()),
        }))
    }

    fn resolve_ident(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        module_id: ModuleID,
        ident: &Ident,
    ) -> Option<SymbolID> {
        if let Some(scope_rc) = &local_scope_opt {
            let scope = scope_rc.borrow();
            if let Some(local_symbol) = scope.resolve(&ident.value) {
                return Some(local_symbol.symbol_id());
            }
        }

        if let Some(symbol_id) = self.lookup_symbol_id(module_id, &ident.value) {
            return Some(symbol_id);
        }

        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(ResolverDiagKind::SymbolNotFound {
                name: ident.value.clone(),
            }),
            location: Some(DiagLoc::new(SourceLoc::from_loc(
                ident.loc.clone(),
                self.current_file_path(),
            ))),
            hint: None,
        });

        None
    }

    fn resolve_module_import(&mut self, module_id: ModuleID, mut module_import: ModuleImport) -> Option<SymbolID> {
        if module_import.segments.len() == 1 {
            let maybe_ident = module_import.as_identifier();
            if let Some(ident) = maybe_ident {
                if let Some(sym) = self.lookup_symbol_id(module_id, &ident.value) {
                    return Some(sym);
                }
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::SymbolNotFound {
                        name: ident.value.clone(),
                    }),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        ident.loc.clone(),
                        self.current_file_path(),
                    ))),
                    hint: None,
                });
            } else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::ExpectedIdentifierInImport),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        module_import.loc.clone(),
                        self.current_file_path(),
                    ))),
                    hint: Some("Import path must include at least one symbol.".into()),
                });
            }
            return None;
        }

        let Some(last_segment) = module_import.segments.pop() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::ExpectedIdentifierInImport),
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    module_import.loc.clone(),
                    self.current_file_path(),
                ))),
                hint: Some("Import path must include at least one symbol.".into()),
            });
            return None;
        };

        let Some(symbol_ident) = last_segment.as_identifier_opt() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::ExpectedIdentifierInImport),
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    module_import.loc,
                    self.current_file_path(),
                ))),
                hint: Some("The last part of an import path must be a symbol name.".into()),
            });
            return None;
        };
        let symbol_name = symbol_ident.value;

        let module_alias = module_segments_as_string(module_import.segments);
        let Some(target_module_id) = self.resolve_module_alias(&module_alias) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::ModuleImportNotFound {
                    module_name: module_alias,
                }),
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    module_import.loc.clone(),
                    self.current_file_path(),
                ))),
                hint: None,
            });
            return None;
        };

        let Some(symbol_id) = self.lookup_symbol_id(target_module_id, &symbol_name) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::SymbolIsNotDefinedInModule {
                    symbol_name,
                    module_name: module_alias,
                }),
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    module_import.loc.clone(),
                    self.current_file_path(),
                ))),
                hint: None,
            });
            return None;
        };

        Some(symbol_id)
    }

    fn resolve_ident_expr(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        module_id: ModuleID,
        ident: &Ident,
    ) -> Option<TypedExprStmt> {
        let symbol_id = self.resolve_ident(local_scope_opt, module_id, ident)?;
        let loc = SourceLoc::from_loc(ident.loc.clone(), self.current_file_path());
        Some(TypedExprStmt {
            kind: TypedExprKind::Symbol(symbol_id, loc.clone()),
            sema_ty: None,
            mloc: MemoryLocation::LValue,
            loc,
        })
    }

    fn resolve_module_import_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        module_import: &ModuleImport,
    ) -> Option<TypedExprStmt> {
        if let Some(ident) = module_import.as_identifier() {
            self.resolve_ident_expr(local_scope_opt, module_id, &ident)
        } else {
            self.resolve_module_import(module_id, module_import.clone())
                .map(|symbol_id| TypedExprStmt {
                    kind: TypedExprKind::Symbol(
                        symbol_id,
                        SourceLoc::from_loc(module_import.loc.clone(), self.current_file_path()),
                    ),
                    sema_ty: None,
                    mloc: MemoryLocation::LValue,
                    loc: SourceLoc::from_loc(module_import.loc.clone(), self.current_file_path()),
                })
        }
    }

    fn resolve_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        expr: &Expr,
    ) -> Option<TypedExprStmt> {
        match expr {
            Expr::FieldAccess(field_access) => self.resolve_field_access(module_id, local_scope_opt, field_access),
            Expr::MethodCall(method_call) => self.resolve_method_call(module_id, local_scope_opt, method_call),
            Expr::StructInit(struct_init) => self.resolve_struct_init(module_id, local_scope_opt, struct_init),
            Expr::ModuleImport(module_import) => {
                self.resolve_module_import_expr(module_id, local_scope_opt, module_import)
            }
            Expr::Ident(ident) => self.resolve_ident_expr(local_scope_opt, module_id, ident),
            Expr::FuncCall(func_call) => self.resolve_func_call(module_id, local_scope_opt, func_call),
            Expr::Array(arr) => self.resolve_array_expr(module_id, local_scope_opt, arr),
            Expr::Infix(bin) => self.resolve_infix_expr(module_id, local_scope_opt, bin),
            Expr::Prefix(prefix) => self.resolve_prefix_expr(module_id, local_scope_opt, prefix),
            Expr::Cast(cast) => self.resolve_cast_expr(module_id, local_scope_opt, cast),
            Expr::TypeSpecifier(type_specifier) => {
                self.resolve_type_specifier_expr(module_id, local_scope_opt, type_specifier)
            }
            Expr::Assign(assignment) => self.resolve_assign_expr(module_id, local_scope_opt, assignment),
            Expr::Literal(literal) => self.resolve_literal_expr(literal),
            Expr::Unary(unary) => self.resolve_unary_expr(module_id, local_scope_opt, unary),
            Expr::ArrayIndex(array_index) => self.resolve_array_index_expr(module_id, local_scope_opt, array_index),
            Expr::AddrOf(address_of) => self.resolve_address_of_expr(module_id, local_scope_opt, address_of),
            Expr::Deref(dereference) => self.resolve_deref_expr(module_id, local_scope_opt, dereference),
            Expr::UnnamedStructValue(unnamed_struct_value) => {
                self.resolve_unnamed_struct_value(module_id, local_scope_opt, unnamed_struct_value)
            }
            Expr::SizeOf(size_of_expression) => {
                self.resolve_size_of_expr(module_id, local_scope_opt, size_of_expression)
            }
            Expr::Lambda(lambda) => self.resolve_lambda_expr(module_id, lambda),
            Expr::Tuple(tuple_value) => self.resolve_tuple_expr(module_id, local_scope_opt, tuple_value),
            Expr::TupleAccess(tuple_member_access) => {
                self.resolve_tuple_member_access(module_id, local_scope_opt, tuple_member_access)
            }
            Expr::Dynamic(dynamic_expr) => self.resolve_dynamic_expr(module_id, local_scope_opt, dynamic_expr),
            Expr::UntypedArray(untyped_array) => {
                self.resolve_untyped_array_expr(module_id, local_scope_opt, untyped_array)
            }
        }
    }

    fn resolve_dynamic_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        dynamic_expr: &Dynamic,
    ) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(module_id, local_scope_opt, &dynamic_expr.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Dynamic(TypedDynamicExpr {
                operand: Box::new(operand),
                loc: SourceLoc::from_loc(dynamic_expr.loc.clone(), self.current_file_path()),
            }),
            sema_ty: None,
            mloc: MemoryLocation::RValue,
            loc: SourceLoc::from_loc(dynamic_expr.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_tuple_member_access(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        tuple_member_access: &TupleAccess,
    ) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &tuple_member_access.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::TupleAccess(TypedTupleAccessExpr {
                operand: Box::new(operand),
                index: tuple_member_access.index,
                loc: SourceLoc::from_loc(tuple_member_access.loc.clone(), self.current_file_path()),
            }),
            sema_ty: None,
            mloc: MemoryLocation::LValue,
            loc: SourceLoc::from_loc(tuple_member_access.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_tuple_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        tuple_value: &TupleValue,
    ) -> Option<TypedExprStmt> {
        let mut expr_list: Vec<TypedExprStmt> = Vec::new();

        for expr in &tuple_value.expr_list {
            match self.resolve_expr(module_id, local_scope_opt.clone(), expr) {
                Some(typed_expr) => expr_list.push(typed_expr),
                None => continue,
            }
        }

        Some(TypedExprStmt {
            kind: TypedExprKind::Tuple(TypedTupleExpr {
                expr_list,
                loc: SourceLoc::from_loc(tuple_value.loc.clone(), self.current_file_path()),
            }),
            sema_ty: None,
            mloc: MemoryLocation::RValue,
            loc: SourceLoc::from_loc(tuple_value.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_lambda_expr(&mut self, module_id: ModuleID, lambda: &Lambda) -> Option<TypedExprStmt> {
        let scope_id = generate_scope_id();
        let scope_rc = LocalScope::new(None);
        self.insert_scope_ref(module_id, scope_id, scope_rc.clone());

        let (list, variadic) = self.resolve_func_params(module_id, Some(scope_rc.clone()), &None, &lambda.params)?;

        let body = match self.resolve_block_stmt(scope_id, scope_rc.clone(), &lambda.body) {
            Some(typed_block) => Box::new(typed_block),
            None => return None,
        };

        let return_type = self.resolve_type(
            &None,
            Some(scope_rc),
            module_id,
            lambda.return_type.clone(),
            lambda.loc.clone(),
            lambda.span.end,
        )?;

        let loc = SourceLoc::from_loc(lambda.loc.clone(), self.current_file_path());

        Some(TypedExprStmt {
            kind: TypedExprKind::Lambda(TypedLambdaExpr {
                params: TypedFuncParams { list, variadic },
                body,
                return_type,
                inline: lambda.inline,
                loc: loc.clone(),
            }),
            sema_ty: None,
            mloc: MemoryLocation::RValue,
            loc,
        })
    }

    fn resolve_field_access(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        field_access: &FieldAccess,
    ) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &field_access.operand)?;

        let type_args = field_access.type_args.clone().and_then(|type_args| {
            self.resolve_type_args(
                &None,
                module_id,
                local_scope_opt,
                &type_args,
                field_access.loc.clone(),
                field_access.span.end,
            )
        });

        Some(TypedExprStmt {
            kind: TypedExprKind::FieldAccess(TypedFieldAccess {
                operand: Box::new(operand),
                field_name: field_access.field_name.value.clone(),
                is_fat_arrow: field_access.is_fat_arrow,
                field_index: None,
                field_ty: None,
                object_symbol_id: None,
                type_args,
                loc: SourceLoc::from_loc(field_access.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::LValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(field_access.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_method_call(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        method_call: &MethodCall,
    ) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &method_call.operand)?;

        let args: Vec<TypedExprStmt> = method_call
            .args
            .iter()
            .filter_map(|arg| self.resolve_expr(module_id, local_scope_opt.clone(), arg))
            .collect();

        let type_args = method_call.type_args.clone().and_then(|type_args| {
            self.resolve_type_args(
                &None,
                module_id,
                local_scope_opt,
                &type_args,
                method_call.loc.clone(),
                method_call.span.end,
            )
        });

        Some(TypedExprStmt {
            kind: TypedExprKind::MethodCall(TypedMethodCall {
                operand: Box::new(operand),
                func_sig: None,
                type_args,
                method_name: method_call.method_name.value.clone(),
                is_fat_arrow: method_call.is_fat_arrow,
                monomorph_key: None,
                self_ty: None,
                is_enum_const: None,
                loc: SourceLoc::from_loc(method_call.loc.clone(), self.current_file_path()),
                args,
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(method_call.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_struct_init(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        struct_init: &StructInit,
    ) -> Option<TypedExprStmt> {
        let symbol_id =
            self.resolve_local_module_import(local_scope_opt.clone(), module_id, &struct_init.struct_name)?;

        let field_inits: Vec<TypedStructFieldInit> = struct_init
            .field_inits
            .iter()
            .filter_map(|field_init| {
                self.resolve_expr(module_id, local_scope_opt.clone(), &field_init.value)
                    .map(|value| TypedStructFieldInit {
                        name: field_init.ident.value.clone(),
                        value,
                        loc: SourceLoc::from_loc(field_init.loc.clone(), self.current_file_path()),
                    })
            })
            .collect();

        let type_args = struct_init.type_args.clone().and_then(|type_args| {
            self.resolve_type_args(
                &None,
                module_id,
                local_scope_opt,
                &type_args,
                struct_init.loc.clone(),
                struct_init.span.end,
            )
        });

        Some(TypedExprStmt {
            kind: TypedExprKind::StructInit(TypedStructInitExpr {
                symbol_id,
                fields: field_inits,
                type_args,
                is_const: struct_init.is_const,
                loc: SourceLoc::from_loc(struct_init.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(struct_init.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_func_call(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        func_call: &FuncCall,
    ) -> Option<TypedExprStmt> {
        if is_unscoped_expr!(self, local_scope_opt, func_call.loc.clone(), func_call.span.end) {
            return None;
        }

        let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &func_call.operand)?;

        let typed_args: Vec<TypedExprStmt> = func_call
            .args
            .iter()
            .filter_map(|arg| self.resolve_expr(module_id, local_scope_opt.clone(), arg))
            .collect();

        let type_args = func_call.type_args.clone().and_then(|type_args| {
            self.resolve_type_args(
                &None,
                module_id,
                local_scope_opt,
                &type_args,
                func_call.loc.clone(),
                func_call.span.end,
            )
        });

        Some(TypedExprStmt {
            kind: TypedExprKind::FuncCall(TypedFuncCall {
                operand: Box::new(operand),
                args: typed_args,
                type_args,
                return_type: None,
                monomorph_key: None,
                loc: SourceLoc::from_loc(func_call.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(func_call.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_untyped_array_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        untyped_array: &UntypedArray,
    ) -> Option<TypedExprStmt> {
        let typed_elements: Vec<TypedExprStmt> = untyped_array
            .elements
            .iter()
            .filter_map(|item| self.resolve_expr(module_id, local_scope_opt.clone(), item))
            .collect();

        Some(TypedExprStmt {
            kind: TypedExprKind::Array(TypedArrayExpr {
                array_type: None,
                elements: typed_elements,
                loc: SourceLoc::from_loc(untyped_array.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(untyped_array.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_array_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        arr: &Array,
    ) -> Option<TypedExprStmt> {
        let array_type = self.resolve_type(
            &None,
            local_scope_opt.clone(),
            module_id,
            arr.data_type.clone(),
            arr.loc.clone(),
            arr.span.end,
        )?;

        let typed_elements: Vec<TypedExprStmt> = arr
            .elements
            .iter()
            .filter_map(|item| self.resolve_expr(module_id, local_scope_opt.clone(), item))
            .collect();

        Some(TypedExprStmt {
            kind: TypedExprKind::Array(TypedArrayExpr {
                array_type: Some(array_type),
                elements: typed_elements,
                loc: SourceLoc::from_loc(arr.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(arr.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_infix_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        bin: &InfixExpr,
    ) -> Option<TypedExprStmt> {
        let lhs = self.resolve_expr(module_id, local_scope_opt.clone(), &*bin.lhs)?;
        let rhs = self.resolve_expr(module_id, local_scope_opt, &*bin.rhs)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Infix(TypedInfixExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: bin.op.clone(),
                loc: SourceLoc::from_loc(bin.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(bin.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_prefix_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        prefix: &PrefixExpr,
    ) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(module_id, local_scope_opt, &*prefix.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Prefix(TypedPrefixExpr {
                operand: Box::new(operand),
                op: prefix.op.clone(),
                loc: SourceLoc::from_loc(prefix.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(prefix.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_cast_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        cast: &Cast,
    ) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &*cast.expr)?;

        let target_type = self.resolve_type(
            &None,
            local_scope_opt,
            module_id,
            cast.target_type.clone(),
            cast.loc.clone(),
            cast.span.end,
        )?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Cast(TypedCastExpr {
                operand: Box::new(operand),
                target_type,
                loc: SourceLoc::from_loc(cast.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(cast.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_type_specifier_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        type_specifier: &TypeSpecifier,
    ) -> Option<TypedExprStmt> {
        let (loc, span_end) = type_specifier.loc();

        let symbol_id = match type_specifier {
            TypeSpecifier::Ident(ident) => self.resolve_ident(local_scope_opt, module_id, &ident)?,
            TypeSpecifier::ModuleImport(module_import) => {
                self.resolve_module_import(module_id, module_import.clone())?
            }
            _ => {
                let sema_ty = self.resolve_type(
                    &None,
                    local_scope_opt,
                    module_id,
                    type_specifier.clone(),
                    loc.clone(),
                    span_end,
                )?;
                return Some(TypedExprStmt {
                    kind: TypedExprKind::SemanticType(sema_ty.clone()),
                    mloc: MemoryLocation::RValue,
                    sema_ty: Some(sema_ty),
                    loc: SourceLoc::from_loc(loc, self.current_file_path()),
                });
            }
        };

        Some(TypedExprStmt {
            kind: TypedExprKind::Symbol(
                symbol_id,
                SourceLoc::from_loc(type_specifier.loc().0, self.current_file_path()),
            ),
            mloc: MemoryLocation::LValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(loc, self.current_file_path()),
        })
    }

    fn resolve_assign_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        assignment: &Assign,
    ) -> Option<TypedExprStmt> {
        let lhs = self.resolve_expr(module_id, local_scope_opt.clone(), &assignment.lhs)?;
        let rhs = self.resolve_expr(module_id, local_scope_opt, &assignment.rhs)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Assign(TypedAssignExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                kind: assignment.kind.clone(),
                loc: SourceLoc::from_loc(assignment.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(assignment.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_literal_expr(&mut self, literal: &Literal) -> Option<TypedExprStmt> {
        let literal_type: Option<SemanticType> = match &literal.kind {
            LiteralKind::Integer(_, suffix_opt) | LiteralKind::Float(_, suffix_opt) => {
                if let Some(token_kind) = suffix_opt {
                    match SemanticType::try_from(*token_kind.clone()) {
                        Ok(sema_ty) => Some(sema_ty),
                        Err(_) => {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(ResolverDiagKind::InvalidLiteralSuffix),
                                location: Some(DiagLoc::new(SourceLoc::from_loc(
                                    literal.loc.clone(),
                                    self.current_file_path(),
                                ))),
                                hint: None,
                            });
                            return None;
                        }
                    }
                } else {
                    None
                }
            }
            LiteralKind::String(string_value, string_prefix) => {
                if let Some(string_prefix) = string_prefix {
                    match string_prefix {
                        StringPrefix::B => {
                            let len = string_value.len() + 1;
                            Some(SemanticType::Array(TypedArrayType {
                                element_type: Box::new(SemanticType::PlainType(PlainType::Char)),
                                capacity: TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(
                                    len.try_into().unwrap(),
                                )),
                                loc: SourceLoc::from_loc(literal.loc.clone(), self.current_file_path()),
                            }))
                        }
                        StringPrefix::C => Some(SemanticType::Pointer(Box::new(SemanticType::PlainType(
                            PlainType::Char,
                        )))),
                    }
                } else {
                    Some(SemanticType::Pointer(Box::new(SemanticType::PlainType(
                        PlainType::Char,
                    ))))
                }
            }
            LiteralKind::Bool(_) => Some(SemanticType::PlainType(PlainType::Bool)),
            LiteralKind::Char(_) => Some(SemanticType::PlainType(PlainType::Char)),
            LiteralKind::Null => Some(SemanticType::Pointer(Box::new(SemanticType::PlainType(
                PlainType::Void,
            )))),
        };

        let typed_literal = TypedLiteralExpr {
            ty: literal_type,
            kind: literal.kind.clone(),
            loc: SourceLoc::from_loc(literal.loc.clone(), self.current_file_path()),
        };

        Some(TypedExprStmt {
            kind: TypedExprKind::Literal(typed_literal.clone()),
            sema_ty: None,
            mloc: MemoryLocation::RValue,
            loc: typed_literal.loc,
        })
    }

    fn resolve_unary_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        unary: &UnaryExpr,
    ) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(module_id, local_scope_opt, &*unary.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Unary(TypedUnaryExpr {
                op: unary.op.clone(),
                operand: Box::new(operand),
                loc: SourceLoc::from_loc(unary.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(unary.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_array_index_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        array_index: &ArrayIndex,
    ) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &array_index.operand)?;
        let index = self.resolve_expr(module_id, local_scope_opt, &array_index.index)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::ArrayIndex(TypedArrayIndexExpr {
                operand: Box::new(operand),
                index: Box::new(index),
                loc: SourceLoc::from_loc(array_index.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::LValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(array_index.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_address_of_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        address_of: &AddrOf,
    ) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(module_id, local_scope_opt, &address_of.expr)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::AddrOf(TypedAddrOfExpr {
                operand: Box::new(operand),
                loc: SourceLoc::from_loc(address_of.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::LValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(address_of.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_deref_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        dereference: &Deref,
    ) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(module_id, local_scope_opt, &dereference.expr)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Deref(TypedDerefExpr {
                operand: Box::new(operand),
                loc: SourceLoc::from_loc(dereference.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(dereference.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_unnamed_struct_value(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        unnamed_struct_value: &UnnamedStructValue,
    ) -> Option<TypedExprStmt> {
        let mut fields: Vec<TypedUnnamedStructValueField> = Vec::new();

        for field in &unnamed_struct_value.fields {
            let field_ty = if let Some(type_specifier) = &field.field_ty {
                match self.resolve_type(
                    &None,
                    local_scope_opt.clone(),
                    module_id,
                    type_specifier.clone(),
                    field.loc.clone(),
                    field.span.end,
                ) {
                    Some(sema_ty) => Some(sema_ty),
                    None => continue,
                }
            } else {
                None
            };

            let field_value = match self.resolve_expr(module_id, local_scope_opt.clone(), &field.field_value) {
                Some(typed_expr) => typed_expr,
                None => continue,
            };

            fields.push(TypedUnnamedStructValueField {
                field_name: field.field_name.value.clone(),
                field_ty,
                field_value: Box::new(field_value),
                loc: SourceLoc::from_loc(field.loc.clone(), self.current_file_path()),
            });
        }

        Some(TypedExprStmt {
            kind: TypedExprKind::UnnamedStructValue(TypedUnnamedStructValue {
                fields,
                unnamed_struct_type: None,
                is_packed: unnamed_struct_value.is_packed,
                is_const: unnamed_struct_value.is_const,
                loc: SourceLoc::from_loc(unnamed_struct_value.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(unnamed_struct_value.loc.clone(), self.current_file_path()),
        })
    }

    fn resolve_size_of_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        size_of_expression: &SizeOf,
    ) -> Option<TypedExprStmt> {
        let typed_expr = self.resolve_expr(module_id, local_scope_opt, &size_of_expression.expr)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::SizeOf(TypedSizeOfExpr {
                operand: Box::new(typed_expr),
                loc: SourceLoc::from_loc(size_of_expression.loc.clone(), self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: SourceLoc::from_loc(size_of_expression.loc.clone(), self.current_file_path()),
        })
    }

    fn duplicate_symbol(&mut self, module_id: ModuleID, symbol_name: String, loc: SourceLoc) -> bool {
        match self.lookup_symbol_id(module_id, &symbol_name) {
            Some(..) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::DuplicateSymbol { symbol_name }),
                    location: Some(DiagLoc::new(loc)),
                    hint: None,
                });

                true
            }
            None => false,
        }
    }

    fn insert_module_alias(
        &self,
        parent_module_id: ModuleID,
        group_name: ModuleGroupName,
        imported_module_id: ModuleID,
    ) {
        let mut module_aliases = self.module_aliases.lock().unwrap();
        let imported_modules = module_aliases.get_mut(&parent_module_id).unwrap();
        imported_modules.insert(group_name, imported_module_id);
        drop(module_aliases);
    }

    fn resolve_module_alias(&self, group_name: &ModuleGroupName) -> Option<ModuleID> {
        let module_aliases = self.module_aliases.lock().unwrap();
        let imported_modules = module_aliases.get(&self.current_module.unwrap()).unwrap();
        let option = imported_modules.get(group_name).cloned();
        drop(module_aliases);
        option
    }

    fn resolve_module_id_by_file_path(&self, module_file_path: String) -> Option<ModuleID> {
        let file_paths = self.file_paths.lock().unwrap();
        let module_id_opt = match file_paths.iter().find(|(_, fp)| **fp == module_file_path) {
            Some((module_id, _)) => Some(*module_id),
            None => None,
        };
        drop(file_paths);
        module_id_opt
    }

    fn insert_module_file_path(&self, module_id: ModuleID, module_file_path: String) {
        let mut file_paths = self.file_paths.lock().unwrap();
        file_paths.insert(module_id, module_file_path);
        drop(file_paths);
    }

    fn resolve_local_module_import(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        module_id: ModuleID,
        module_import: &ModuleImport,
    ) -> Option<SymbolID> {
        if let Some(ident) = module_import.as_identifier() {
            if let Some(scope_rc) = local_scope_opt {
                let scope = scope_rc.borrow();
                if let Some(local_symbol) = scope.resolve(&ident.value) {
                    return Some(local_symbol.symbol_id());
                }
            }

            if ident.as_string() == "Self" {
                return Some(self.current_object.unwrap());
            }

            if let Some(symbol_id) = self.lookup_symbol_id(module_id, &ident.value) {
                return Some(symbol_id);
            }

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::SymbolNotFound {
                    name: ident.value.clone(),
                }),
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    ident.loc.clone(),
                    self.current_file_path(),
                ))),
                hint: None,
            });
            return None;
        }

        self.resolve_module_import(module_id, module_import.clone())
            .map(|symbol_id| symbol_id)
    }

    pub fn insert_scope_ref(&self, module_id: ModuleID, scope_id: ScopeID, scope_ref: LocalScopeRef) {
        let mut global_symbols = self.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get_mut(&module_id).unwrap();
        symbol_table.scopes.insert(scope_id, scope_ref);
        drop(global_symbols);
    }

    pub fn current_file_path(&self) -> String {
        let current_module_id = self.current_module.unwrap();
        let file_paths = self.file_paths.lock().unwrap();
        let file_path = match file_paths.get(&current_module_id) {
            Some(child_module_file_path) => child_module_file_path.clone(),
            None => self.master_module_file_path.clone(),
        };
        drop(file_paths);
        file_path
    }

    pub fn resolve_module_file_path(&self, module_id: ModuleID) -> Option<String> {
        let file_paths = self.file_paths.lock().unwrap();
        let file_path = match file_paths.get(&module_id) {
            Some(module_file_path) => Some(module_file_path.clone()),
            None => None,
        };
        drop(file_paths);
        file_path
    }
}

/// Constructs a unique name for a struct method by prefixing with the struct's SymbolID.
/// This allows the resolver to differentiate between `Point::distance()` and `Circle::distance()`.
fn method_symbol_name_for_struct(struct_id: SymbolID, method_name: String) -> String {
    format!("{}{}", struct_id, method_name)
}

fn return_type_or_void_as_default(
    type_specifier_opt: Option<TypeSpecifier>,
    loc: Location,
    span: Span,
) -> TypeSpecifier {
    type_specifier_opt.unwrap_or(TypeSpecifier::TypeToken(Token {
        kind: TokenKind::Void,
        loc,
        span,
    }))
}

pub fn generate_module_id() -> ModuleID {
    let mut rng = rand::rng();
    rng.random::<u64>()
}

impl Visiting {
    pub fn new() -> Self {
        Self {
            active: HashSet::new(),
            done: HashSet::new(),
        }
    }
}

unsafe impl Send for Resolver {}
unsafe impl Sync for Resolver {}
