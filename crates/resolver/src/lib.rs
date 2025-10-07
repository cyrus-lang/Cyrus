use crate::modulefsloader::{ModuleAlias, ModuleFilePath, ModuleLoader, ModuleLoaderOptions};
use crate::scope::*;
use crate::signatures::{EnumSig, GlobalVarSig, InterfaceSig, StructSig, UnionSig};
use crate::{
    diagnostics::ResolverDiagKind,
    signatures::{FuncSig, TypedefSig},
};
use ast::format::module_segments_as_string;
use ast::source_loc::SourceLoc;
use ast::{
    AccessSpecifier, AddressOf, Array, ArrayIndex, Assignment, Cast, Dereference, FieldAccess, FuncCall, FuncParams,
    FuncTypeVariadicParams, GlobalVariable, If, Import, InfixExpression, Interface, Lambda, Literal, MethodCall,
    ModuleImport, ModulePath, ModuleSegment, ModuleSegmentSingle, PrefixExpression, SelfModifierKind, SizeOfExpression,
    StringPrefix, StructInit, SwitchCasePattern, UnaryExpression, Union, UnnamedStructValue,
};
use ast::{
    ArrayCapacity, BlockStatement, Enum, EnumVariant, Expression, FuncDecl, FuncDef, FuncParamKind, FuncVariadicParams,
    Identifier, LiteralKind, ProgramTree, Statement, Struct, TypeSpecifier, Typedef, Variable,
    token::{Location, Span, Token, TokenKind},
};
use diagcentral::{reporter::DiagReporter, *};
use rand::Rng;
use std::collections::HashSet;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use typed_ast::types::{
    BasicConcreteType, ConcreteType, TypedArrayCapacity, TypedArrayFixedCapacityValue, TypedArrayType, TypedFuncType,
    TypedUnnamedStructType, TypedUnnamedStructTypeField,
};
use typed_ast::{SymbolID, *};

mod diagnostics;
pub mod modulefsloader;
pub mod scope;
pub mod signatures;
pub mod utility;

pub type GlobalSymbolsMutex = Mutex<HashMap<ModuleID, SymbolTable>>;
type ModuleGroupName = String;
type ImportedModules = HashMap<ModuleGroupName, ModuleID>;

pub struct Resolver {
    pub global_symbols: Arc<GlobalSymbolsMutex>,
    // lists imported modules by current module by their alias
    pub module_aliases: Arc<Mutex<HashMap<ModuleID, ImportedModules>>>,
    pub analyzed_modules: Arc<Mutex<HashSet<ModuleID>>>,
    pub program_trees: Arc<Mutex<Vec<(String, ModuleFilePath, ModuleID, Rc<RefCell<TypedProgramTree>>)>>>,
    pub file_paths: Arc<Mutex<HashMap<ModuleID, ModuleFilePath>>>,
    pub reporter: DiagReporter<ResolverDiagKind>,
    pub module_loader: ModuleLoader,
    pub master_module_file_path: String,
    // loaded only for current module (not global)
    already_imported_modules: Vec<ModulePath>,
    current_module: Option<ModuleID>,
}

// Used to check import cycles.
pub struct Visiting {
    pub active: HashSet<ModuleFilePath>, // stack of modules currently being resolved
    pub done: HashSet<ModuleFilePath>,   // modules fully resolved
}

macro_rules! is_unscoped_expr {
    ($self:expr, $local_scope_opt:expr, $loc:expr, $span_end:expr) => {{
        if $local_scope_opt.is_none() {
            $self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: ResolverDiagKind::RequiresLocalScope,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    $loc,
                    $self.get_current_module_file_path(),
                ))),
                hint: None,
            });
        }

        $local_scope_opt.is_none()
    }};
}

macro_rules! resolve_local_identifier {
    ($self:expr, $local_scope_opt:expr, $module_id:expr, $identifier:expr) => {{
        let local_option = {
            if let Some(local_scope_rc) = &$local_scope_opt {
                let symbol_id_opt = {
                    let local_scope = local_scope_rc.borrow_mut();
                    match local_scope.resolve(&$identifier.name.clone()).cloned() {
                        Some(local_symbol) => Some(local_symbol),
                        None => None,
                    }
                };

                match symbol_id_opt {
                    Some(local_symbol) => Some(local_symbol.get_symbol_id()),
                    None => None,
                }
            } else {
                None
            }
        };

        match local_option {
            Some(symbol_id) => symbol_id,
            None => {
                match $self.resolve_module_import(
                    $module_id,
                    ModuleImport {
                        segments: vec![ModuleSegment::SubModule($identifier.clone())],
                        loc: $identifier.loc.clone(),
                        span: $identifier.span.clone(),
                    },
                ) {
                    Some(symbol_id) => symbol_id,
                    None => {
                        $self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: ResolverDiagKind::SymbolNotFound {
                                name: $identifier.name.clone(),
                            },
                            location: Some(DiagLoc::new(SourceLoc::from_loc(
                                $identifier.loc.clone(),
                                $self.get_current_module_file_path(),
                            ))),
                            hint: None,
                        });

                        return None;
                    }
                }
            }
        }
    }};
}

impl Resolver {
    pub fn new(opts: ModuleLoaderOptions, master_module_file_path: String) -> Self {
        let file_paths = Arc::new(Mutex::new(HashMap::new()));

        Self {
            global_symbols: Arc::new(Mutex::new(HashMap::new())),
            analyzed_modules: Arc::new(Mutex::new(HashSet::new())),
            module_aliases: Arc::new(Mutex::new(HashMap::new())),
            file_paths: file_paths.clone(),
            program_trees: Arc::new(Mutex::new(Vec::new())),
            already_imported_modules: Vec::new(),
            reporter: DiagReporter::new(),
            module_loader: ModuleLoader::new(opts),
            current_module: None,
            master_module_file_path,
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
        module_file_path: ModuleFilePath,
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
        for import in ast.get_imports() {
            self.resolve_import(parent_module_id, import, &mut visiting);
        }

        self.current_module = Some(parent_module_id);

        // Collect exact definitions and details of the symbols (second pass).
        let typed_body = self.resolve_definitions(module_id, &ast);
        let typed_program_tree = Rc::new(RefCell::new(TypedProgramTree { body: typed_body }));

        if is_master {
            let mut program_trees = self.program_trees.lock().unwrap();
            let module_name = get_module_name(module_file_path.clone());
            program_trees.push((
                module_name,
                module_file_path.clone(),
                self.current_module.unwrap(),
                typed_program_tree.clone(),
            ));
            drop(program_trees);
        }

        visiting.active.remove(&module_file_path);
        visiting.done.insert(module_file_path);

        Some(typed_program_tree.clone())
    }

    fn resolve_module_import(&mut self, module_id: ModuleID, mut module_import: ModuleImport) -> Option<SymbolID> {
        if let Some(identifier) = module_import.as_identifier() {
            return self.resolve_identifier(
                module_id,
                identifier,
                SourceLoc::from_loc(module_import.loc.clone(), self.get_current_module_file_path()),
            );
        }

        let Some(last_segment) = module_import.segments.pop() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: ResolverDiagKind::ExpectedIdentifierInImport,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    module_import.loc.clone(),
                    self.get_current_module_file_path(),
                ))),
                hint: Some("Import path must include at least one symbol.".into()),
            });
            return None;
        };

        let Some(symbol_ident) = last_segment.as_identifier_opt() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: ResolverDiagKind::ExpectedIdentifierInImport,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    module_import.loc,
                    self.get_current_module_file_path(),
                ))),
                hint: Some("The last part of an import path must be a symbol name.".into()),
            });
            return None;
        };
        let symbol_name = symbol_ident.name;

        let module_alias = module_segments_as_string(module_import.segments);
        let Some(target_module_id) = self.get_module_alias(&module_alias) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: ResolverDiagKind::ModuleImportNotFound {
                    module_name: module_alias,
                },
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    module_import.loc.clone(),
                    self.get_current_module_file_path(),
                ))),
                hint: None,
            });
            return None;
        };

        let Some(symbol_id) = self.lookup_symbol_id(target_module_id, &symbol_name) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: ResolverDiagKind::SymbolIsNotDefinedInModule {
                    symbol_name,
                    module_name: module_alias,
                },
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    module_import.loc.clone(),
                    self.get_current_module_file_path(),
                ))),
                hint: None,
            });
            return None;
        };

        Some(symbol_id)
    }

    fn skip_module_if_loaded_once(&self, file_path: String) -> bool {
        let file_paths = self.file_paths.lock().unwrap();
        let exists = file_paths.iter().find(|(_, fp)| **fp == file_path).is_some();
        drop(file_paths);
        exists
    }

    // FIXME   Fix cyclic-import problem here.
    fn resolve_import(&mut self, parent_module_id: ModuleID, import: Import, mut visiting: &mut Visiting) {
        let mut duplicate_import = false;

        for module_path in &import.paths {
            let already_loaded = self
                .already_imported_modules
                .iter()
                .find(|loaded_module_path| **loaded_module_path == *module_path);

            if already_loaded.is_some() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::ImportTwice {
                        module_name: module_segments_as_string(module_path.segments.clone()),
                    },
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        import.loc.clone(),
                        self.get_current_module_file_path(),
                    ))),
                    hint: Some("Consider to remove previous declaration.".to_string()),
                });
                duplicate_import = true;
            }

            self.already_imported_modules.push(module_path.clone());
        }

        if duplicate_import {
            return;
        }

        let loaded_modules_list = self
            .module_loader
            .load_module(import.clone(), self.get_current_module_file_path());

        for loaded_module in loaded_modules_list {
            self.already_imported_modules.clear();

            let (module_alias, module_file_path, program_tree) = match loaded_module {
                Ok(module_ast_and_file_path) => module_ast_and_file_path,
                Err(diag_kind) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: diag_kind,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            import.loc.clone(),
                            self.get_current_module_file_path(),
                        ))),
                        hint: None,
                    });
                    continue;
                }
            };

            let module_id = {
                self.get_module_id_by_file_path(module_file_path.clone())
                    .unwrap_or(generate_module_id())
            };

            if visiting.active.contains(&module_file_path) {
                // cycle import detected
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::ImportCycle {
                        module_names: visiting.active.iter().cloned().collect(),
                    },
                    location: None,
                    hint: None,
                });
                continue;
            }

            visiting.active.insert(module_file_path.clone());

            if self.skip_module_if_loaded_once(module_file_path.clone()) {
                match module_alias {
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
                self.insert_module_file_path(module_id, module_file_path.clone());

                match self.resolve_module(module_id, program_tree.as_ref(), &mut visiting, false, module_file_path) {
                    Some(typed_program_tree) => {
                        let module_file_path = self.get_current_module_file_path();

                        let mut program_trees = self.program_trees.lock().unwrap();
                        let module_name = get_module_name(module_file_path.clone());
                        program_trees.push((module_name, module_file_path, module_id, typed_program_tree));
                        drop(program_tree);
                        drop(program_trees);

                        match module_alias {
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
                    None => continue,
                };
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
            let actual_name = single.identifier.as_string();
            let renamed_name = single.renamed.as_ref().unwrap_or(&single.identifier).as_string();

            let Some(symbol_id) = self.lookup_symbol_id(imported_module_id, &actual_name) else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::SymbolNotFound {
                        name: renamed_name.clone(),
                    },
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        loc.clone(),
                        self.get_module_file_path(parent_module_id).unwrap(),
                    ))),
                    hint: None,
                });
                continue;
            };

            imported_symbol_ids.push(symbol_id);

            let symbol_entry = self.resolve_global_symbol(symbol_id).unwrap();

            {
                let mut global_symbols = self.global_symbols.lock().unwrap();
                let symbol_table = global_symbols
                    .get_mut(&parent_module_id)
                    .expect("parent module should exist in global symbols");

                if symbol_table.names.contains_key(&actual_name) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::DuplicateSymbol {
                            symbol_name: renamed_name.clone(),
                        },
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            loc.clone(),
                            self.get_module_file_path(parent_module_id).unwrap(),
                        ))),
                        hint: None,
                    });
                    continue;
                }

                symbol_table.names.insert(renamed_name.clone(), symbol_id);

                let vis = symbol_entry.get_vis();
                symbol_table.entries.insert(symbol_id, symbol_entry);

                drop(global_symbols);
                self.check_import_single_vis(actual_name, vis, loc.clone());
            }
        }
    }

    fn check_import_single_vis(&mut self, single_name: String, vis: AccessSpecifier, loc: Location) {
        if vis.is_private() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: ResolverDiagKind::ImportSinglePrivateSymbol {
                    symbol_name: single_name,
                },
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    loc,
                    self.get_current_module_file_path(),
                ))),
                hint: None,
            });
        }
    }

    fn init_imported_modules_for_module(&mut self) {
        let mut module_aliases = self.module_aliases.lock().unwrap();
        module_aliases.insert(self.current_module.unwrap(), HashMap::new());
        drop(module_aliases);
    }

    fn resolve_type(
        &mut self,
        local_scope: Option<LocalScopeRef>,
        module_id: ModuleID,
        type_specifier: TypeSpecifier,
        loc: Location,
        span_end: usize,
    ) -> Option<ConcreteType> {
        let result = match type_specifier {
            TypeSpecifier::FuncType(func_type) => {
                let params: Vec<ConcreteType> = func_type
                    .params
                    .list
                    .iter()
                    .filter_map(|param| {
                        self.resolve_type(local_scope.clone(), module_id, param.clone(), loc.clone(), span_end)
                    })
                    .collect();

                let variadic = match &func_type.params.variadic {
                    Some(FuncTypeVariadicParams::UntypedCStyle) => {
                        Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle))
                    }
                    Some(FuncTypeVariadicParams::Typed(spec)) => {
                        let ct =
                            self.resolve_type(local_scope.clone(), module_id, spec.clone(), loc.clone(), span_end)?;
                        Some(Box::new(TypedFuncTypeVariadicParams::Typed(ct)))
                    }
                    None => None,
                };

                let return_type = self.resolve_type(
                    local_scope.clone(),
                    module_id,
                    *func_type.return_type,
                    loc.clone(),
                    span_end,
                )?;

                Ok(ConcreteType::FuncType(TypedFuncType {
                    params: TypedFuncTypeParams { list: params, variadic },
                    return_type: Box::new(return_type),
                    vis_opt: func_type.vis_opt.clone(),
                    loc: SourceLoc::from_loc(loc.clone(), self.get_current_module_file_path()),
                }))
            }
            TypeSpecifier::TypeToken(token) => {
                ConcreteType::try_from(token.kind.clone()).map_err(|_| ResolverDiagKind::TypeNotFound {
                    name: token.kind.to_string(),
                })
            }
            TypeSpecifier::Const(inner) => {
                let ct = self.resolve_type(local_scope, module_id, *inner, loc.clone(), span_end)?;
                Ok(ConcreteType::Const(Box::new(ct)))
            }
            TypeSpecifier::Dereference(inner) => {
                let ct = self.resolve_type(local_scope, module_id, *inner, loc.clone(), span_end)?;
                Ok(ConcreteType::Pointer(Box::new(ct)))
            }
            TypeSpecifier::Array(array_spec) => {
                let elem_type = self.resolve_type(
                    local_scope.clone(),
                    module_id,
                    *array_spec.element_type,
                    loc.clone(),
                    span_end,
                )?;

                let capacity = match &array_spec.size {
                    ArrayCapacity::Fixed(expr) => {
                        let typed_expr = self.resolve_expr(module_id, local_scope.clone(), expr)?;
                        if let TypedExpressionKind::Literal(lit) = &typed_expr.kind {
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

                Ok(ConcreteType::Array(TypedArrayType {
                    element_type: Box::new(elem_type),
                    capacity,
                    loc: SourceLoc::from_loc(loc.clone(), self.get_current_module_file_path()),
                }))
            }
            TypeSpecifier::UnnamedStruct(struct_spec) => {
                let mut fields = Vec::new();
                for field in &struct_spec.fields {
                    if let Some(ft) = self.resolve_type(
                        local_scope.clone(),
                        module_id,
                        field.field_type.clone(),
                        field.loc.clone(),
                        field.span.end,
                    ) {
                        fields.push(TypedUnnamedStructTypeField {
                            field_name: field.field_name.name.clone(),
                            field_type: Box::new(ft),
                            loc: SourceLoc::from_loc(field.loc.clone(), self.get_current_module_file_path()),
                        });
                    }
                }

                Ok(ConcreteType::UnnamedStruct(TypedUnnamedStructType {
                    fields,
                    packed: struct_spec.packed,
                    loc: SourceLoc::from_loc(struct_spec.loc.clone(), self.get_current_module_file_path()),
                }))
            }
            TypeSpecifier::ModuleImport(import) => self
                .resolve_module_import(module_id, import)
                .map(ConcreteType::UnresolvedSymbol)
                .ok_or(ResolverDiagKind::TypeNotFound {
                    name: "import".to_string(),
                }),
            TypeSpecifier::Identifier(identifier) => self
                .lookup_symbol_id(module_id, &identifier.name)
                .map(ConcreteType::UnresolvedSymbol)
                .ok_or(ResolverDiagKind::TypeNotFound {
                    name: identifier.name.clone(),
                }),
        };

        match result {
            Ok(ct) => Some(ct),
            Err(kind) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        loc,
                        self.get_current_module_file_path(),
                    ))),
                    hint: None,
                });
                None
            }
        }
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
                Statement::Interface(interface) => {
                    if self.duplicate_symbol(
                        module_id,
                        interface.identifier.name.clone(),
                        SourceLoc::from_loc(interface.loc.clone(), self.get_current_module_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &interface.identifier.name.clone());
                }
                Statement::Union(union_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        union_decl.identifier.name.clone(),
                        SourceLoc::from_loc(union_decl.loc.clone(), self.get_current_module_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &union_decl.identifier.name.clone());
                }
                Statement::Typedef(typedef) => {
                    if self.duplicate_symbol(
                        module_id,
                        typedef.identifier.name.clone(),
                        SourceLoc::from_loc(typedef.loc.clone(), self.get_current_module_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &typedef.identifier.name.clone());
                }
                Statement::FuncDef(func_def) => {
                    if self.duplicate_symbol(
                        module_id,
                        func_def.identifier.name.clone(),
                        SourceLoc::from_loc(func_def.loc.clone(), self.get_current_module_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &func_def.identifier.name.clone());
                }
                Statement::FuncDecl(func_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        func_decl.identifier.name.clone(),
                        SourceLoc::from_loc(func_decl.loc.clone(), self.get_current_module_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &func_decl.get_usable_name());
                }
                Statement::GlobalVariable(global_variable) => {
                    if self.duplicate_symbol(
                        module_id,
                        global_variable.identifier.name.clone(),
                        SourceLoc::from_loc(global_variable.loc.clone(), self.get_current_module_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &global_variable.identifier.name.clone());
                }
                Statement::Struct(struct_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        struct_decl.identifier.name.clone(),
                        SourceLoc::from_loc(struct_decl.loc.clone(), self.get_current_module_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &struct_decl.identifier.name.clone());
                }
                Statement::Enum(enum_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        enum_decl.identifier.name.clone(),
                        SourceLoc::from_loc(enum_decl.loc.clone(), self.get_current_module_file_path()),
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(module_id, &enum_decl.identifier.name.clone());
                }
                _ => {}
            };
        }
    }

    // Resolves the full meaning of each top-level declaration in the AST (second pass)
    fn resolve_definitions(&mut self, module_id: ModuleID, ast: &ProgramTree) -> Vec<TypedStatement> {
        let mut typed_body: Vec<TypedStatement> = Vec::new();

        for stmt in ast.body.as_ref() {
            let valid_top_level_stmt: Result<TypedStatement, SourceLoc> = match stmt {
                Statement::Import(..) => continue,
                Statement::GlobalVariable(global_var) => match self.resolve_global_var(module_id, global_var) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Typedef(typedef) => match self.resolve_typedef(module_id, None, typedef) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::FuncDef(func_def) => match self.resolve_func_def(module_id, func_def) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::FuncDecl(func_decl) => match self.resolve_func_decl(module_id, func_decl) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Struct(struct_decl) => match self.resolve_struct(module_id, None, struct_decl, None) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Enum(enum_decl) => match self.resolve_enum(module_id, None, enum_decl, None) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Interface(interface) => match self.resolve_interface(module_id, None, interface) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Union(union_stmt) => match self.resolve_union(module_id, None, union_stmt, None) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Variable(variable) => Err(SourceLoc::from_loc(
                    variable.loc.clone(),
                    self.get_current_module_file_path(),
                )),
                Statement::If(if_stmt) => Err(SourceLoc::from_loc(
                    if_stmt.loc.clone(),
                    self.get_current_module_file_path(),
                )),
                Statement::Return(return_stmt) => Err(SourceLoc::from_loc(
                    return_stmt.loc.clone(),
                    self.get_current_module_file_path(),
                )),
                Statement::For(for_stmt) => Err(SourceLoc::from_loc(
                    for_stmt.loc.clone(),
                    self.get_current_module_file_path(),
                )),
                Statement::Foreach(foreach) => Err(SourceLoc::from_loc(
                    foreach.loc.clone(),
                    self.get_current_module_file_path(),
                )),
                Statement::Switch(switch) => Err(SourceLoc::from_loc(
                    switch.loc.clone(),
                    self.get_current_module_file_path(),
                )),
                Statement::BlockStatement(block_statement) => Err(SourceLoc::from_loc(
                    block_statement.loc.clone(),
                    self.get_current_module_file_path(),
                )),
                Statement::Break(break_stmt) => Err(SourceLoc::from_loc(
                    break_stmt.loc.clone(),
                    self.get_current_module_file_path(),
                )),
                Statement::Continue(continue_stmt) => Err(SourceLoc::from_loc(
                    continue_stmt.loc.clone(),
                    self.get_current_module_file_path(),
                )),
                Statement::Expression(..) => continue,
                Statement::While(..) => continue,
            };

            match valid_top_level_stmt {
                Ok(typed_stmt) => {
                    typed_body.push(typed_stmt);
                }
                Err(loc) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::InvalidTopLevelStatement,
                        location: Some(DiagLoc::new(loc)),
                        hint: None,
                    });
                }
            };
        }

        typed_body
    }

    fn resolve_interface(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        interface: &Interface,
    ) -> Option<TypedStatement> {
        let interface_symbol_id = local_scope_opt
            .as_ref()
            .map(|_| generate_symbol_id())
            .unwrap_or_else(|| self.lookup_symbol_id(module_id, &interface.identifier.name).unwrap());

        let typed_methods: Vec<TypedFuncDecl> = interface
            .methods
            .iter()
            .filter_map(|func_decl| {
                let resolved = self.resolve_func(module_id, local_scope_opt.clone(), func_decl)?;

                if func_decl.renamed_as.is_some() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::RenameInterfaceMethod,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            func_decl.loc.clone(),
                            self.get_current_module_file_path(),
                        ))),
                        hint: None,
                    });
                }

                let (return_type, typed_func_params, typed_variadic_param) = resolved;

                Some(TypedFuncDecl {
                    module_id: self.current_module.unwrap(),
                    symbol_id: interface_symbol_id,
                    name: func_decl.identifier.name.clone(),
                    params: TypedFuncParams {
                        list: typed_func_params,
                        variadic: typed_variadic_param,
                    },
                    return_type,
                    vis: func_decl.vis.clone(),
                    renamed_as: None,
                    loc: SourceLoc::from_loc(func_decl.loc.clone(), self.get_current_module_file_path()),
                })
            })
            .collect();

        let resolved_interface = ResolvedInterface {
            module_id,
            symbol_id: interface_symbol_id,
            interface_sig: InterfaceSig {
                name: interface.identifier.name.clone(),
                methods: typed_methods.clone(),
                vis: interface.vis.clone(),
                loc: SourceLoc::from_loc(interface.loc.clone(), self.get_current_module_file_path()),
            },
        };

        match local_scope_opt {
            Some(local_scope_rc) => {
                local_scope_rc.borrow_mut().insert(
                    interface.identifier.name.clone(),
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

        Some(TypedStatement::Interface(TypedInterface {
            name: interface.identifier.name.clone(),
            symbol_id: interface_symbol_id,
            methods: typed_methods,
            vis: interface.vis.clone(),
            loc: SourceLoc::from_loc(interface.loc.clone(), self.get_current_module_file_path()),
        }))
    }

    fn resolve_union(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        union_decl: &Union,
        is_local: Option<ScopeID>,
    ) -> Option<TypedStatement> {
        let union_symbol_id = if local_scope_opt.is_some() {
            generate_symbol_id()
        } else {
            self.lookup_symbol_id(module_id, &union_decl.identifier.name).unwrap()
        };

        let mut typed_union_fields: Vec<TypedUnionField> = Vec::new();

        for field in &union_decl.fields {
            match self.resolve_type(
                local_scope_opt.clone(),
                module_id,
                field.ty.clone(),
                field.loc.clone(),
                field.span.end,
            ) {
                Some(concrete_type) => {
                    typed_union_fields.push(TypedUnionField {
                        name: field.identifier.name.clone(),
                        ty: concrete_type,
                        loc: SourceLoc::from_loc(field.loc.clone(), self.get_current_module_file_path()),
                    });
                }
                None => continue,
            }
        }

        self.check_duplicate_method_names(&union_decl.identifier.name, union_decl.methods.clone());

        let methods = match self.resolve_methods(module_id, &union_decl.methods, union_symbol_id) {
            Some(methods) => methods,
            None => return None,
        };

        let resolved_union = ResolvedUnion {
            module_id,
            symbol_id: union_symbol_id,
            union_sig: UnionSig {
                symbol_id: union_symbol_id,
                name: union_decl.identifier.name.clone(),
                fields: typed_union_fields.clone(),
                methods: methods.clone(),
                vis: union_decl.vis.clone(),
                loc: SourceLoc::from_loc(union_decl.loc.clone(), self.get_current_module_file_path()),
            },
        };

        if let Some(local_scope_rc) = &local_scope_opt {
            let mut local_scope = local_scope_rc.borrow_mut();
            local_scope.insert(
                union_decl.identifier.name.clone(),
                LocalSymbol::new(LocalSymbolKind::Union(resolved_union)),
            );
            drop(local_scope);
        } else {
            self.insert_symbol_entry(
                module_id,
                union_symbol_id,
                SymbolEntry::new(SymbolEntryKind::Union(resolved_union)),
            );
        }

        Some(TypedStatement::Union(TypedUnion {
            symbol_id: union_symbol_id,
            module_id,
            name: union_decl.identifier.name.clone(),
            fields: typed_union_fields,
            methods,
            vis: union_decl.vis.clone(),
            loc: SourceLoc::from_loc(union_decl.identifier.loc.clone(), self.get_current_module_file_path()),
            is_local: is_local,
        }))
    }

    fn resolve_enum(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        enum_decl: &Enum,
        is_local: Option<ScopeID>,
    ) -> Option<TypedStatement> {
        let enum_symbol_id = if local_scope_opt.is_some() {
            generate_symbol_id()
        } else {
            self.lookup_symbol_id(module_id, &enum_decl.identifier.name).unwrap()
        };
        let mut variants: Vec<TypedEnumVariant> = Vec::new();

        for variant in &enum_decl.variants {
            let typed_variant = match variant {
                EnumVariant::Identifier(identifier) => TypedEnumVariant::Identifier(identifier.clone()),
                EnumVariant::Variant(identifier, enum_valued_fields) => {
                    let mut fields: Vec<TypedEnumValuedField> = Vec::new();
                    for valued_field in enum_valued_fields {
                        let field_type = match self.resolve_type(
                            local_scope_opt.clone(),
                            module_id,
                            valued_field.field_type.clone(),
                            valued_field.loc.clone(),
                            0,
                        ) {
                            Some(concrete_type) => concrete_type,
                            None => continue,
                        };

                        fields.push(TypedEnumValuedField {
                            field_type,
                            loc: SourceLoc::from_loc(valued_field.loc.clone(), self.get_current_module_file_path()),
                        });
                    }
                    TypedEnumVariant::Variant(identifier.clone(), fields)
                }
                EnumVariant::Valued(identifier, expr) => match self.resolve_expr(
                    module_id,
                    match &local_scope_opt {
                        Some(local_scope) => Some(Rc::clone(&local_scope)),
                        None => None,
                    },
                    expr,
                ) {
                    Some(typed_expr) => TypedEnumVariant::Valued(identifier.clone(), Box::new(typed_expr)),
                    None => continue,
                },
            };

            variants.push(typed_variant);
        }

        self.check_duplicate_method_names(&enum_decl.identifier.name, enum_decl.methods.clone());

        let methods = match self.resolve_methods(module_id, &enum_decl.methods, enum_symbol_id) {
            Some(methods) => methods,
            None => return None,
        };

        let resolved_enum = ResolvedEnum {
            module_id,
            symbol_id: enum_symbol_id,
            enum_sig: EnumSig {
                symbol_id: enum_symbol_id,
                name: enum_decl.identifier.name.clone(),
                methods: methods.clone(),
                variants: variants.clone(),
                vis: enum_decl.vis.clone(),
                loc: SourceLoc::from_loc(enum_decl.loc.clone(), self.get_current_module_file_path()),
            },
        };

        if let Some(local_scope_rc) = &local_scope_opt {
            let mut local_scope = local_scope_rc.borrow_mut();
            local_scope.insert(
                enum_decl.identifier.name.clone(),
                LocalSymbol::new(LocalSymbolKind::Enum(resolved_enum)),
            );
            drop(local_scope);
        } else {
            self.insert_symbol_entry(
                module_id,
                enum_symbol_id,
                SymbolEntry::new(SymbolEntryKind::Enum(resolved_enum)),
            );
        }

        Some(TypedStatement::Enum(TypedEnum {
            module_id,
            symbol_id: enum_symbol_id,
            name: enum_decl.identifier.name.clone(),
            variants,
            methods,
            vis: enum_decl.vis.clone(),
            loc: SourceLoc::from_loc(enum_decl.identifier.loc.clone(), self.get_current_module_file_path()),
            is_local,
        }))
    }

    fn resolve_global_var(&mut self, module_id: ModuleID, global_var: &GlobalVariable) -> Option<TypedStatement> {
        let concrete_type = global_var
            .type_specifier
            .clone()
            .and_then(|ty| self.resolve_type(None, module_id, ty, global_var.loc.clone(), global_var.span.end));

        let typed_expr = global_var
            .expr
            .as_ref()
            .and_then(|expr| self.resolve_expr(module_id, None, expr));

        let symbol_id = self.lookup_symbol_id(module_id, &global_var.identifier.name).unwrap();

        let resolved_global_var = ResolvedGlobalVar {
            module_id,
            symbol_id,
            global_var_sig: GlobalVarSig {
                module_id,
                name: global_var.identifier.name.clone(),
                ty: concrete_type.clone(),
                rhs: typed_expr.clone(),
                vis: global_var.vis.clone(),
                loc: SourceLoc::from_loc(global_var.loc.clone(), self.get_current_module_file_path()),
            },
        };

        self.insert_symbol_entry(
            module_id,
            symbol_id,
            SymbolEntry::new(SymbolEntryKind::GlobalVar(resolved_global_var)),
        );

        Some(TypedStatement::GlobalVariable(TypedGlobalVariable {
            module_id,
            symbol_id,
            name: global_var.identifier.name.clone(),
            ty: concrete_type,
            expr: typed_expr,
            vis: global_var.vis.clone(),
            loc: SourceLoc::from_loc(global_var.loc.clone(), self.get_current_module_file_path()),
        }))
    }

    fn check_duplicate_method_names(&mut self, struct_name: &str, methods_list: Vec<FuncDef>) {
        let mut method_names: Vec<String> = Vec::new();

        for func_def in methods_list {
            let method_name = func_def.identifier.name.clone();

            if method_names.contains(&method_name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::DuplicateMethodName {
                        struct_name: struct_name.to_string(),
                        method_name: method_name.clone(),
                    },
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        func_def.loc.clone(),
                        self.get_current_module_file_path(),
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
    ) -> Option<HashMap<String, SymbolID>> {
        let mut methods: HashMap<String, SymbolID> = HashMap::new();
        let mut method_bodies: HashMap<SymbolID, (LocalScopeRef, Box<BlockStatement>, ScopeID)> = HashMap::new();

        for func_def in methods_list {
            let method_scope_id = generate_scope_id();
            let local_scope_rc = Rc::new(RefCell::new(LocalScope::new(None)));
            self.insert_scope_ref(module_id, method_scope_id, Rc::clone(&local_scope_rc));

            if let Some((return_type, mut typed_func_params, typed_variadic_param)) =
                self.resolve_func(module_id, Some(Rc::clone(&local_scope_rc)), &func_def.as_func_decl())
            {
                let method_name = func_def.identifier.name.clone();
                let method_resolve_name = make_method_resolve_name(struct_symbol_id, method_name.clone());

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

                let symbol_id = self.insert_symbol_name(module_id, &method_resolve_name);

                methods.insert(method_name.clone(), symbol_id);

                let func_sig = FuncSig {
                    module_id,
                    name: method_name.clone(),
                    is_func_decl: false,
                    params: TypedFuncParams {
                        list: typed_func_params,
                        variadic: typed_variadic_param,
                    },
                    return_type,
                    vis: func_def.vis.clone(),
                    loc: SourceLoc::from_loc(func_def.loc.clone(), self.get_current_module_file_path()),
                };

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
                    (Rc::clone(&local_scope_rc), func_def.body.clone(), method_scope_id),
                );
            }
        }

        for (&symbol_id, (local_scope_rc, method_body, method_scope_id)) in &method_bodies {
            let mut resolved_method = match self.lookup_symbol_entry_with_id(module_id, symbol_id).unwrap().kind {
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
                        typed_variable: TypedVariable {
                            symbol_id: self_symbol_id,
                            name: "self".to_string(),
                            ty: match self_modifier.kind {
                                SelfModifierKind::Copied => Some(ConcreteType::UnresolvedSymbol(struct_symbol_id)),
                                SelfModifierKind::Referenced => Some(ConcreteType::Pointer(Box::new(
                                    ConcreteType::UnresolvedSymbol(struct_symbol_id),
                                ))),
                            },
                            rhs: None,
                            loc: resolved_method.func_sig.loc.clone(),
                        },
                    }));

                    local_scope_rc.borrow_mut().insert("self".to_string(), local_symbol);
                }
            }

            let method_scope = LocalScope::deep_clone(local_scope_rc);

            if let Some(typed_func_body) = self.resolve_block_statement(*method_scope_id, method_scope, method_body) {
                resolved_method.func_body = Some(Box::new(typed_func_body));
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

    fn resolve_struct(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        struct_decl: &Struct,
        is_local: Option<ScopeID>,
    ) -> Option<TypedStatement> {
        let struct_symbol_id = local_scope_opt
            .as_ref()
            .map(|_| generate_symbol_id())
            .unwrap_or_else(|| self.lookup_symbol_id(module_id, &struct_decl.identifier.name).unwrap());

        let typed_struct_fields: Vec<TypedStructField> = struct_decl
            .fields
            .iter()
            .filter_map(|field| {
                self.resolve_type(
                    local_scope_opt.clone(),
                    module_id,
                    field.ty.clone(),
                    field.loc.clone(),
                    field.span.end,
                )
                .map(|ty| TypedStructField {
                    name: field.identifier.name.clone(),
                    vis: field.vis.clone(),
                    ty,
                    loc: SourceLoc::from_loc(field.loc.clone(), self.get_current_module_file_path()),
                })
            })
            .collect();

        self.check_duplicate_method_names(&struct_decl.identifier.name, struct_decl.methods.clone());

        let methods = self.resolve_methods(module_id, &struct_decl.methods, struct_symbol_id)?;

        let impls: Vec<LocalOrGlobalSymbol> = struct_decl
            .impls
            .iter()
            .filter_map(|identifier| {
                let resolved = local_scope_opt
                    .as_ref()
                    .and_then(|local_scope| {
                        local_scope
                            .borrow()
                            .resolve(&identifier.as_string())
                            .map(|sym| LocalOrGlobalSymbol::LocalSymbol(sym.clone()))
                    })
                    .or_else(|| {
                        self.lookup_symbol(module_id, &identifier.name)
                            .map(LocalOrGlobalSymbol::GlobalSymbol)
                    });

                resolved.or_else(|| {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::SymbolNotFound {
                            name: identifier.as_string(),
                        },
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            identifier.loc.clone(),
                            self.get_current_module_file_path(),
                        ))),
                        hint: None,
                    });
                    None
                })
            })
            .collect();

        let resolved_struct = ResolvedStruct {
            module_id,
            symbol_id: struct_symbol_id,
            struct_sig: StructSig {
                name: struct_decl.identifier.name.clone(),
                fields: typed_struct_fields.clone(),
                impls,
                packed: struct_decl.packed,
                methods: methods.clone(),
                vis: struct_decl.vis.clone(),
                loc: SourceLoc::from_loc(struct_decl.loc.clone(), self.get_current_module_file_path()),
            },
        };

        if let Some(local_scope_rc) = local_scope_opt {
            local_scope_rc.borrow_mut().insert(
                struct_decl.identifier.name.clone(),
                LocalSymbol::new(LocalSymbolKind::Struct(resolved_struct)),
            );
        } else {
            self.insert_symbol_entry(
                module_id,
                struct_symbol_id,
                SymbolEntry::new(SymbolEntryKind::Struct(resolved_struct)),
            );
        }

        Some(TypedStatement::Struct(TypedStruct {
            module_id: self.current_module.unwrap(),
            symbol_id: struct_symbol_id,
            name: struct_decl.identifier.name.clone(),
            fields: typed_struct_fields,
            methods,
            vis: struct_decl.vis.clone(),
            packed: struct_decl.packed,
            loc: SourceLoc::from_loc(struct_decl.loc.clone(), self.get_current_module_file_path()),
            is_local,
        }))
    }

    fn resolve_func(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        func_decl: &FuncDecl,
    ) -> Option<(ConcreteType, Vec<TypedFuncParamKind>, Option<TypedFuncVariadicParams>)> {
        let return_type_specifier = func_decl.return_type.clone().unwrap_or(TypeSpecifier::TypeToken(Token {
            kind: TokenKind::Void,
            span: Span::default(),
            loc: Location::default(),
        }));

        let return_type = self.resolve_type(
            local_scope_opt.clone(),
            module_id,
            return_type_specifier,
            func_decl.loc.clone(),
            func_decl.span.end,
        )?;

        let (typed_func_params, typed_variadic_param) =
            self.resolve_func_params(module_id, local_scope_opt.clone(), &func_decl.params)?;

        Some((return_type, typed_func_params, typed_variadic_param))
    }

    fn resolve_func_params(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        params: &FuncParams,
    ) -> Option<(Vec<TypedFuncParamKind>, Option<TypedFuncVariadicParams>)> {
        let mut typed_func_params = Vec::with_capacity(params.list.len());

        for param in &params.list {
            match param {
                FuncParamKind::FuncParam(func_param) => {
                    let param_type = match &func_param.ty {
                        Some(type_specifier) => self.resolve_type(
                            local_scope_opt.clone(),
                            module_id,
                            type_specifier.clone(),
                            func_param.loc.clone(),
                            func_param.span.end,
                        )?,
                        None => {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: ResolverDiagKind::InvalidUntypedFuncParam,
                                location: Some(DiagLoc::new(SourceLoc::from_loc(
                                    func_param.loc.clone(),
                                    self.get_current_module_file_path(),
                                ))),
                                hint: None,
                            });
                            continue;
                        }
                    };

                    if let Some(local_scope_rc) = &local_scope_opt {
                        let mut local_scope = local_scope_rc.borrow_mut();
                        let symbol_id = generate_symbol_id();
                        local_scope.insert(
                            func_param.identifier.name.clone(),
                            LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                                module_id,
                                symbol_id,
                                typed_variable: TypedVariable {
                                    symbol_id,
                                    name: func_param.identifier.name.clone(),
                                    ty: Some(param_type.clone()),
                                    rhs: None,
                                    loc: SourceLoc::from_loc(
                                        func_param.loc.clone(),
                                        self.get_current_module_file_path(),
                                    ),
                                },
                            })),
                        );
                    }

                    typed_func_params.push(TypedFuncParamKind::FuncParam(TypedFuncParam {
                        name: func_param.identifier.name.clone(),
                        ty: param_type,
                        loc: SourceLoc::from_loc(func_param.loc.clone(), self.get_current_module_file_path()),
                    }));
                }
                FuncParamKind::SelfModifier(self_modifier) => {
                    typed_func_params.push(TypedFuncParamKind::SelfModifier(TypedSelfModifier {
                        symbol_id: None,
                        self_symbol_id: None,
                        ty: None,
                        kind: self_modifier.kind.clone(),
                        loc: SourceLoc::from_loc(self_modifier.loc.clone(), self.get_current_module_file_path()),
                    }));
                }
            }
        }

        let typed_variadic_param = params.variadic.as_ref().and_then(|variadic| match variadic {
            FuncVariadicParams::UntypedCStyle => Some(TypedFuncVariadicParams::UntypedCStyle),
            FuncVariadicParams::Typed(identifier, type_specifier) => {
                let variadic_type = self.resolve_type(
                    local_scope_opt.clone(),
                    module_id,
                    type_specifier.clone(),
                    identifier.loc.clone(),
                    identifier.span.end,
                )?;

                if let Some(local_scope_rc) = &local_scope_opt {
                    let mut local_scope = local_scope_rc.borrow_mut();
                    let symbol_id = generate_symbol_id();
                    local_scope.insert(
                        identifier.name.clone(),
                        LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                            module_id,
                            symbol_id,
                            typed_variable: TypedVariable {
                                symbol_id,
                                name: identifier.name.clone(),
                                ty: Some(variadic_type.clone()),
                                rhs: None,
                                loc: SourceLoc::from_loc(identifier.loc.clone(), self.get_current_module_file_path()),
                            },
                        })),
                    );
                }

                Some(TypedFuncVariadicParams::Typed(identifier.name.clone(), variadic_type))
            }
        });

        Some((typed_func_params, typed_variadic_param))
    }

    fn resolve_func_decl(&mut self, module_id: ModuleID, func_decl: &FuncDecl) -> Option<TypedStatement> {
        let symbol_id = self.lookup_symbol_id(module_id, &func_decl.get_usable_name()).unwrap();

        let (return_type, typed_func_params, typed_variadic_param) = self.resolve_func(module_id, None, func_decl)?;

        let func_sig = FuncSig {
            module_id,
            name: func_decl.identifier.name.clone(),
            is_func_decl: true,
            params: TypedFuncParams {
                list: typed_func_params.clone(),
                variadic: typed_variadic_param.clone(),
            },
            return_type: return_type.clone(),
            vis: func_decl.vis.clone(),
            loc: SourceLoc::from_loc(func_decl.loc.clone(), self.get_current_module_file_path()),
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

        Some(TypedStatement::FuncDecl(TypedFuncDecl {
            module_id,
            symbol_id,
            name: func_decl.identifier.name.clone(),
            params: TypedFuncParams {
                list: typed_func_params,
                variadic: typed_variadic_param,
            },
            return_type,
            vis: func_decl.vis.clone(),
            renamed_as: func_decl.renamed_as.as_ref().map(|id| id.as_string()),
            loc: SourceLoc::from_loc(func_decl.loc.clone(), self.get_current_module_file_path()),
        }))
    }

    fn resolve_func_def(&mut self, module_id: ModuleID, func_def: &FuncDef) -> Option<TypedStatement> {
        let scope_id = generate_scope_id();
        let body_scope = Rc::new(RefCell::new(LocalScope::new(None)));
        self.insert_scope_ref(module_id, scope_id, body_scope.clone());

        let symbol_id = self.lookup_symbol_id(module_id, &func_def.identifier.name)?;

        let (return_type, typed_func_params, typed_variadic_param) =
            self.resolve_func(module_id, Some(body_scope.clone()), &func_def.as_func_decl())?;

        let func_sig = FuncSig {
            module_id,
            name: func_def.identifier.name.clone(),
            is_func_decl: false,
            params: TypedFuncParams {
                list: typed_func_params.clone(),
                variadic: typed_variadic_param.clone(),
            },
            return_type: return_type.clone(),
            vis: func_def.vis.clone(),
            loc: SourceLoc::from_loc(func_def.loc.clone(), self.get_current_module_file_path()),
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

        let typed_func_body = self.resolve_block_statement(scope_id, body_scope, &func_def.body)?;

        Some(TypedStatement::FuncDef(TypedFuncDef {
            symbol_id,
            module_id,
            name: func_def.identifier.name.clone(),
            params: TypedFuncParams {
                list: typed_func_params,
                variadic: typed_variadic_param,
            },
            return_type,
            vis: func_def.vis.clone(),
            loc: SourceLoc::from_loc(func_def.loc.clone(), self.get_current_module_file_path()),
            body: Box::new(typed_func_body),
        }))
    }

    fn resolve_typedef(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        typedef: &Typedef,
    ) -> Option<TypedStatement> {
        let symbol_id = self.lookup_symbol_id(module_id, &typedef.identifier.name)?;

        let concrete_type = self.resolve_type(
            local_scope_opt.clone(),
            module_id,
            typedef.type_specifier.clone(),
            typedef.loc.clone(),
            typedef.span.end,
        )?;

        let typedef_sig = TypedefSig {
            name: typedef.identifier.name.clone(),
            ty: concrete_type.clone(),
            vis: typedef.vis.clone(),
            loc: SourceLoc::from_loc(typedef.loc.clone(), self.get_current_module_file_path()),
        };

        let resolved_typedef = ResolvedTypedef {
            module_id,
            symbol_id,
            typedef_sig: typedef_sig.clone(),
        };

        if let Some(local_scope_rc) = &local_scope_opt {
            local_scope_rc.borrow_mut().insert(
                typedef.identifier.name.clone(),
                LocalSymbol::new(LocalSymbolKind::Typedef(resolved_typedef)),
            );
        } else {
            self.insert_symbol_entry(
                module_id,
                symbol_id,
                SymbolEntry::new(SymbolEntryKind::Typedef(resolved_typedef)),
            );
        }

        Some(TypedStatement::Typedef(TypedTypedef {
            name: typedef.identifier.name.clone(),
            ty: concrete_type,
            vis: typedef.vis.clone(),
            loc: typedef_sig.loc,
        }))
    }

    fn declare_local_variable(
        &mut self,
        module_id: ModuleID,
        local_scope_rc: LocalScopeRef,
        variable: &Variable,
    ) -> Option<TypedVariable> {
        if local_scope_rc.borrow().resolve(&variable.identifier.name).is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: ResolverDiagKind::DuplicateSymbolInThisScope {
                    symbol_name: variable.identifier.name.clone(),
                },
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    variable.loc.clone(),
                    self.get_current_module_file_path(),
                ))),
                hint: None,
            });
            return None;
        }

        let var_type = variable.ty.as_ref().and_then(|ty_spec| {
            self.resolve_type(
                Some(local_scope_rc.clone()),
                module_id,
                ty_spec.clone(),
                variable.loc.clone(),
                variable.span.end,
            )
        });

        let typed_rhs = variable
            .rhs
            .as_ref()
            .and_then(|expr| self.resolve_expr(module_id, Some(local_scope_rc.clone()), expr));

        let symbol_id = generate_symbol_id();

        let typed_variable = TypedVariable {
            symbol_id,
            name: variable.identifier.name.clone(),
            ty: var_type.clone(),
            rhs: typed_rhs.clone(),
            loc: SourceLoc::from_loc(variable.loc.clone(), self.get_current_module_file_path()),
        };

        let resolved_var = ResolvedVariable {
            module_id,
            symbol_id,
            typed_variable: typed_variable.clone(),
        };

        local_scope_rc.borrow_mut().insert(
            variable.identifier.name.clone(),
            LocalSymbol::new(LocalSymbolKind::Variable(resolved_var)),
        );

        Some(typed_variable)
    }

    fn resolve_if_stmt(&mut self, module_id: ModuleID, local_scope: LocalScopeRef, if_stmt: &If) -> Option<TypedIf> {
        let typed_condition = match self.resolve_expr(module_id, Some(Rc::clone(&local_scope)), &if_stmt.condition) {
            Some(typed_expr) => typed_expr,
            None => return None,
        };

        let consequent_scope_id = generate_scope_id();
        let consequent_scope = LocalScope::deep_clone(&local_scope);
        self.insert_scope_ref(module_id, consequent_scope_id, consequent_scope.clone());

        let typed_consequent =
            match self.resolve_block_statement(consequent_scope_id, consequent_scope, &if_stmt.consequent) {
                Some(typed_block) => Box::new(typed_block),
                None => return None,
            };

        let typed_alternate = {
            if let Some(alternate) = &if_stmt.alternate {
                let alternate_scope_id = generate_scope_id();
                let alternate_scope = LocalScope::deep_clone(&local_scope);
                self.insert_scope_ref(module_id, alternate_scope_id, alternate_scope.clone());

                match self.resolve_block_statement(alternate_scope_id, Rc::clone(&local_scope), &*alternate) {
                    Some(typed_block) => Some(Box::new(typed_block)),
                    None => return None,
                }
            } else {
                None
            }
        };

        let mut branches: Vec<TypedIf> = Vec::new();

        for else_if_stmt in &if_stmt.branches {
            let typed_if = match self.resolve_if_stmt(module_id, local_scope.clone(), else_if_stmt) {
                Some(typed_if) => typed_if,
                None => continue,
            };

            branches.push(typed_if);
        }

        Some(TypedIf {
            condition: typed_condition,
            consequent: typed_consequent,
            alternate: typed_alternate,
            branches,
            loc: SourceLoc::from_loc(if_stmt.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_block_statement(
        &mut self,
        scope_id: ScopeID,
        local_scope: LocalScopeRef,
        block_statement: &BlockStatement,
    ) -> Option<TypedBlockStatement> {
        let module_id = self.current_module.unwrap();
        let mut typed_body: Vec<TypedStatement> = Vec::new();

        for stmt in &block_statement.exprs {
            match stmt {
                Statement::Variable(variable) => {
                    match self.declare_local_variable(module_id, local_scope.clone(), &variable) {
                        Some(typed_var) => {
                            typed_body.push(TypedStatement::Variable(typed_var));
                        }
                        None => continue,
                    }
                }
                Statement::Expression(expr) => {
                    match self.resolve_expr(module_id, Some(Rc::clone(&local_scope)), expr) {
                        Some(typed_expr) => {
                            typed_body.push(TypedStatement::Expression(typed_expr));
                        }
                        None => continue,
                    }
                }
                Statement::If(if_stmt) => match self.resolve_if_stmt(module_id, Rc::clone(&local_scope), if_stmt) {
                    Some(typed_if) => {
                        typed_body.push(TypedStatement::If(typed_if));
                    }
                    None => continue,
                },
                Statement::Return(return_stmt) => {
                    let argument = {
                        if let Some(argument) = &return_stmt.argument {
                            match self.resolve_expr(module_id, Some(Rc::clone(&local_scope)), argument) {
                                Some(typed_expr) => Some(typed_expr),
                                None => continue,
                            }
                        } else {
                            None
                        }
                    };

                    typed_body.push(TypedStatement::Return(TypedReturn {
                        argument,
                        loc: SourceLoc::from_loc(return_stmt.loc.clone(), self.get_current_module_file_path()),
                    }));
                }
                Statement::Foreach(..) => todo!(),
                Statement::For(for_stmt) => {
                    let body_scope_id = generate_scope_id();
                    let body_scope = LocalScope::deep_clone(&local_scope);
                    self.insert_scope_ref(module_id, body_scope_id, body_scope.clone());

                    let initializer = {
                        if let Some(variable) = &for_stmt.initializer {
                            match self.declare_local_variable(module_id, Rc::clone(&body_scope), &variable) {
                                Some(typed_var) => Some(typed_var),
                                None => continue,
                            }
                        } else {
                            None
                        }
                    };

                    let condition = {
                        if let Some(expr) = &for_stmt.condition {
                            match self.resolve_expr(module_id, Some(Rc::clone(&body_scope)), expr) {
                                Some(typed_expr) => Some(typed_expr),
                                None => continue,
                            }
                        } else {
                            None
                        }
                    };

                    let increment = {
                        if let Some(expr) = &for_stmt.increment {
                            match self.resolve_expr(module_id, Some(Rc::clone(&body_scope)), expr) {
                                Some(typed_expr) => Some(typed_expr),
                                None => continue,
                            }
                        } else {
                            None
                        }
                    };

                    let for_typed_body =
                        match self.resolve_block_statement(body_scope_id, Rc::clone(&body_scope), &*for_stmt.body) {
                            Some(typed_block) => Box::new(typed_block),
                            None => continue,
                        };

                    typed_body.push(TypedStatement::For(TypedFor {
                        initializer,
                        condition,
                        increment,
                        body: for_typed_body,
                        loc: SourceLoc::from_loc(for_stmt.loc.clone(), self.get_current_module_file_path()),
                    }));
                }
                Statement::While(while_stmt) => {
                    let body_scope_id = generate_scope_id();
                    let body_scope = LocalScope::deep_clone(&local_scope);
                    self.insert_scope_ref(module_id, body_scope_id, body_scope.clone());
                    let condition =
                        match self.resolve_expr(module_id, Some(Rc::clone(&body_scope)), &while_stmt.condition) {
                            Some(typed_expr) => Some(typed_expr),
                            None => continue,
                        }
                        .unwrap();

                    let while_typed_body =
                        match self.resolve_block_statement(body_scope_id, Rc::clone(&body_scope), &*while_stmt.body) {
                            Some(typed_block) => Box::new(typed_block),
                            None => continue,
                        };

                    typed_body.push(TypedStatement::While(TypedWhile {
                        condition,
                        body: while_typed_body,
                        loc: SourceLoc::from_loc(while_stmt.loc.clone(), self.get_current_module_file_path()),
                    }));
                }
                Statement::Switch(switch) => {
                    let operand = match self.resolve_expr(module_id, Some(Rc::clone(&local_scope)), &switch.operand) {
                        Some(typed_expr) => typed_expr,
                        None => continue,
                    };

                    let mut cases: Vec<TypedSwitchCase> = Vec::new();

                    for case in &switch.cases {
                        let case_scope_rc = LocalScope::deep_clone(&local_scope);

                        let case_scope_id = generate_scope_id();
                        self.insert_scope_ref(module_id, case_scope_id, case_scope_rc.clone());

                        let pattern = match &case.pattern {
                            SwitchCasePattern::Expression(expr) => {
                                let typed_expr =
                                    match self.resolve_expr(module_id, Some(Rc::clone(&local_scope)), &expr) {
                                        Some(typed_expr) => typed_expr,
                                        None => continue,
                                    };

                                let loc = typed_expr.loc.clone();
                                TypedSwitchCasePattern::Expression(typed_expr, loc)
                            }
                            SwitchCasePattern::Identifier(identifier) => {
                                let symbol_id = generate_symbol_id();

                                let mut case_scope = case_scope_rc.borrow_mut();
                                case_scope.insert(
                                    identifier.name.clone(),
                                    LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                                        module_id,
                                        symbol_id,
                                        typed_variable: TypedVariable {
                                            symbol_id,
                                            name: identifier.name.clone(),
                                            ty: None,
                                            rhs: None,
                                            loc: SourceLoc::from_loc(
                                                identifier.loc.clone(),
                                                self.get_current_module_file_path(),
                                            ),
                                        },
                                    })),
                                );
                                drop(case_scope);

                                TypedSwitchCasePattern::Identifier(
                                    identifier.name.clone(),
                                    SourceLoc::from_loc(identifier.loc.clone(), self.get_current_module_file_path()),
                                )
                            }
                            SwitchCasePattern::EnumVariant(identifier, valued_fields) => {
                                TypedSwitchCasePattern::EnumVariant(
                                    identifier.name.clone(),
                                    valued_fields
                                        .iter()
                                        .map(|identifier| {
                                            let symbol_id = generate_symbol_id();
                                            let mut case_scope = case_scope_rc.borrow_mut();

                                            case_scope.insert(
                                                identifier.name.clone(),
                                                LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                                                    module_id,
                                                    symbol_id,
                                                    typed_variable: TypedVariable {
                                                        symbol_id,
                                                        name: identifier.name.clone(),
                                                        ty: None,
                                                        rhs: None,
                                                        loc: SourceLoc::from_loc(
                                                            identifier.loc.clone(),
                                                            self.get_current_module_file_path(),
                                                        ),
                                                    },
                                                })),
                                            );
                                            drop(case_scope);
                                            identifier.clone()
                                        })
                                        .collect(),
                                    SourceLoc::from_loc(identifier.loc.clone(), self.get_current_module_file_path()),
                                )
                            }
                        };

                        let mut body = match self.resolve_block_statement(scope_id, case_scope_rc.clone(), &case.body) {
                            Some(typed_block) => typed_block,
                            None => continue,
                        };

                        body.scope_id = case_scope_id;

                        cases.push(TypedSwitchCase {
                            pattern,
                            body: Box::new(body),
                            loc: SourceLoc::from_loc(case.loc.clone(), self.get_current_module_file_path()),
                        });

                        drop(case_scope_rc);
                    }

                    let default_case = {
                        if let Some(default_case) = &switch.default_case {
                            let body_scope_id = generate_scope_id();
                            let body_scope = LocalScope::deep_clone(&local_scope);
                            self.insert_scope_ref(module_id, body_scope_id, body_scope.clone());

                            match self.resolve_block_statement(scope_id, body_scope.clone(), &default_case) {
                                Some(typed_block) => Some(typed_block),
                                None => continue,
                            }
                        } else {
                            None
                        }
                    };

                    typed_body.push(TypedStatement::Switch(TypedSwitch {
                        operand,
                        cases,
                        default_case,
                        loc: SourceLoc::from_loc(switch.loc.clone(), self.get_current_module_file_path()),
                    }));
                }
                Statement::Enum(enum_decl) => {
                    match self.resolve_enum(module_id, Some(Rc::clone(&local_scope)), enum_decl, Some(scope_id)) {
                        Some(typed_stmt) => {
                            typed_body.push(typed_stmt);
                        }
                        None => continue,
                    }
                }
                Statement::Union(union_decl) => {
                    match self.resolve_union(module_id, Some(Rc::clone(&local_scope)), union_decl, Some(scope_id)) {
                        Some(typed_stmt) => {
                            typed_body.push(typed_stmt);
                        }
                        None => continue,
                    }
                }
                Statement::Interface(interface) => {
                    match self.resolve_interface(module_id, Some(local_scope.clone()), interface) {
                        Some(typed_stmt) => {
                            typed_body.push(typed_stmt);
                        }
                        None => continue,
                    }
                }
                Statement::Struct(struct_decl) => {
                    match self.resolve_struct(module_id, Some(local_scope.clone()), struct_decl, Some(scope_id)) {
                        Some(typed_stmt) => {
                            typed_body.push(typed_stmt);
                        }
                        None => continue,
                    }
                }
                Statement::BlockStatement(block_statement) => {
                    let scope_id = generate_scope_id();
                    let local_scope_copy = LocalScope::deep_clone(&local_scope);
                    self.insert_scope_ref(module_id, scope_id, local_scope_copy.clone());

                    match self.resolve_block_statement(scope_id, local_scope_copy, block_statement) {
                        Some(typed_stmt) => {
                            typed_body.push(TypedStatement::BlockStatement(typed_stmt));
                        }
                        None => continue,
                    }
                }
                Statement::Break(break_stmt) => {
                    typed_body.push(TypedStatement::Break(TypedBreak {
                        loc: SourceLoc::from_loc(break_stmt.loc.clone(), self.get_current_module_file_path()),
                    }));
                }
                Statement::Continue(continue_stmt) => {
                    typed_body.push(TypedStatement::Continue(TypedContinue {
                        loc: SourceLoc::from_loc(continue_stmt.loc.clone(), self.get_current_module_file_path()),
                    }));
                }
                Statement::Typedef(typedef) => {
                    match self.resolve_typedef(module_id, Some(local_scope.clone()), &typedef) {
                        Some(typed_stmt) => typed_body.push(typed_stmt),
                        None => continue,
                    }
                }
                // Invalid statements.
                Statement::GlobalVariable(..)
                | Statement::FuncDef(..)
                | Statement::FuncDecl(..)
                | Statement::Import(..) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::InvalidStatement,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            stmt.get_loc(),
                            self.get_current_module_file_path(),
                        ))),
                        hint: None,
                    });
                }
            }
        }

        Some(TypedBlockStatement {
            scope_id,
            exprs: typed_body,
            loc: SourceLoc::from_loc(block_statement.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_identifier(&mut self, module_id: ModuleID, identifier: Identifier, loc: SourceLoc) -> Option<u32> {
        match self.lookup_symbol_id(module_id, &identifier.name) {
            Some(symbol_id) => Some(symbol_id),
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::SymbolNotFound {
                        name: identifier.name.clone(),
                    },
                    location: Some(DiagLoc::new(loc)),
                    hint: None,
                });
                return None;
            }
        }
    }

    fn resolve_local_module_import(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        module_id: ModuleID,
        module_import: &ModuleImport,
    ) -> Option<SymbolID> {
        let local_option = {
            if let Some(local_scope_rc) = local_scope_opt.clone() {
                let symbol_id_option = module_import.as_identifier().and_then(|identifier| {
                    let local_scope = local_scope_rc.borrow_mut();
                    local_scope
                        .resolve(&identifier.name)
                        .cloned()
                        .map(|local_symbol| local_symbol.get_symbol_id())
                });
                symbol_id_option
            } else {
                None
            }
        };

        let symbol_id = match local_option {
            Some(symbol_id) => symbol_id,
            None => match self.resolve_module_import(module_id, module_import.clone()) {
                Some(symbol_id) => symbol_id,
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::SymbolNotFound {
                            name: module_segments_as_string(module_import.segments.clone()),
                        },
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            module_import.loc.clone(),
                            self.get_current_module_file_path(),
                        ))),
                        hint: None,
                    });
                    return None;
                }
            },
        };

        Some(symbol_id)
    }

    fn resolve_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        expr: &Expression,
    ) -> Option<TypedExpression> {
        match expr {
            Expression::FieldAccess(field_access) => {
                self.resolve_field_access(module_id, local_scope_opt, field_access)
            }
            Expression::MethodCall(method_call) => self.resolve_method_call(module_id, local_scope_opt, method_call),
            Expression::StructInit(struct_init) => self.resolve_struct_init(module_id, local_scope_opt, struct_init),
            Expression::ModuleImport(module_import) => {
                self.resolve_module_import_expr(module_id, local_scope_opt, module_import)
            }
            Expression::Identifier(identifier) => self.resolve_identifier_expr(module_id, local_scope_opt, identifier),
            Expression::FuncCall(func_call) => self.resolve_func_call(module_id, local_scope_opt, func_call),
            Expression::Array(arr) => self.resolve_array_expr(module_id, local_scope_opt, arr),
            Expression::Infix(bin) => self.resolve_infix_expr(module_id, local_scope_opt, bin),
            Expression::Prefix(prefix) => self.resolve_prefix_expr(module_id, local_scope_opt, prefix),
            Expression::Cast(cast) => self.resolve_cast_expr(module_id, local_scope_opt, cast),
            Expression::TypeSpecifier(type_specifier) => {
                self.resolve_type_specifier_expr(module_id, local_scope_opt, type_specifier)
            }
            Expression::Assignment(assignment) => self.resolve_assign_expr(module_id, local_scope_opt, assignment),
            Expression::Literal(literal) => self.resolve_literal_expr(literal),
            Expression::Unary(unary) => self.resolve_unary_expr(module_id, local_scope_opt, unary),
            Expression::ArrayIndex(array_index) => {
                self.resolve_array_index_expr(module_id, local_scope_opt, array_index)
            }
            Expression::AddressOf(address_of) => self.resolve_address_of_expr(module_id, local_scope_opt, address_of),
            Expression::Dereference(dereference) => self.resolve_deref_expr(module_id, local_scope_opt, dereference),
            Expression::UnnamedStructValue(unnamed_struct_value) => {
                self.resolve_unnamed_struct_value(module_id, local_scope_opt, unnamed_struct_value)
            }
            Expression::SizeOfExpression(size_of_expression) => {
                self.resolve_size_of_expr(module_id, local_scope_opt, size_of_expression)
            }
            Expression::Lambda(lambda) => self.resolve_lambda_expr(module_id, lambda),
        }
    }

    fn resolve_lambda_expr(&mut self, module_id: ModuleID, lambda: &Lambda) -> Option<TypedExpression> {
        let scope_id = generate_scope_id();
        let body_scope = LocalScope::new(None);
        let local_scope_rc = Rc::new(RefCell::new(body_scope));
        self.insert_scope_ref(module_id, scope_id, local_scope_rc.clone());

        let (list, variadic) = self.resolve_func_params(module_id, Some(local_scope_rc.clone()), &lambda.params)?;

        let body = match self.resolve_block_statement(scope_id, local_scope_rc.clone(), &lambda.body) {
            Some(typed_block) => Box::new(typed_block),
            None => return None,
        };

        let return_type = self.resolve_type(
            Some(local_scope_rc),
            module_id,
            lambda.return_type.clone(),
            lambda.loc.clone(),
            lambda.span.end,
        )?;

        let loc = SourceLoc::from_loc(lambda.loc.clone(), self.get_current_module_file_path());

        Some(TypedExpression {
            kind: TypedExpressionKind::Lambda(TypedLambda {
                params: TypedFuncParams { list, variadic },
                body,
                return_type,
                loc: loc.clone(),
            }),
            concrete_type: None,
            value_category: ValueCategory::Rvalue,
            loc,
        })
    }

    fn resolve_field_access(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        field_access: &FieldAccess,
    ) -> Option<TypedExpression> {
        let operand = self.resolve_expr(module_id, local_scope_opt, &field_access.operand)?;

        Some(TypedExpression {
            kind: TypedExpressionKind::FieldAccess(TypedFieldAccess {
                operand: Box::new(operand),
                field_name: field_access.field_name.name.clone(),
                is_fat_arrow: field_access.is_fat_arrow,
                field_index: None,
                field_ty: None,
                object_symbol_id: None,
                loc: SourceLoc::from_loc(field_access.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Lvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(field_access.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_method_call(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        method_call: &MethodCall,
    ) -> Option<TypedExpression> {
        let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &method_call.operand)?;

        let args: Vec<TypedExpression> = method_call
            .args
            .iter()
            .filter_map(|arg| self.resolve_expr(module_id, local_scope_opt.clone(), arg))
            .collect();

        Some(TypedExpression {
            kind: TypedExpressionKind::MethodCall(TypedMethodCall {
                operand: Box::new(operand),
                object_symbol_id: None,
                method_name: method_call.method_name.name.clone(),
                is_fat_arrow: method_call.is_fat_arrow,
                loc: SourceLoc::from_loc(method_call.loc.clone(), self.get_current_module_file_path()),
                args,
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(method_call.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_struct_init(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        struct_init: &StructInit,
    ) -> Option<TypedExpression> {
        let symbol_id =
            self.resolve_local_module_import(local_scope_opt.clone(), module_id, &struct_init.struct_name)?;

        let field_inits: Vec<TypedStructFieldInit> = struct_init
            .field_inits
            .iter()
            .filter_map(|field_init| {
                self.resolve_expr(module_id, local_scope_opt.clone(), &field_init.value)
                    .map(|value| TypedStructFieldInit {
                        name: field_init.identifier.name.clone(),
                        value,
                        loc: SourceLoc::from_loc(field_init.loc.clone(), self.get_current_module_file_path()),
                    })
            })
            .collect();

        Some(TypedExpression {
            kind: TypedExpressionKind::StructInit(TypedStructInit {
                symbol_id,
                fields: field_inits,
                is_const: struct_init.is_const,
                loc: SourceLoc::from_loc(struct_init.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(struct_init.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_module_import_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        module_import: &ModuleImport,
    ) -> Option<TypedExpression> {
        if let Some(identifier) = module_import.as_identifier() {
            let symbol_id = resolve_local_identifier!(self, local_scope_opt, module_id, identifier);
            Some(TypedExpression {
                kind: TypedExpressionKind::Symbol(
                    symbol_id,
                    SourceLoc::from_loc(identifier.loc.clone(), self.get_current_module_file_path()),
                ),
                concrete_type: None,
                value_category: ValueCategory::Lvalue,
                loc: SourceLoc::from_loc(module_import.loc.clone(), self.get_current_module_file_path()),
            })
        } else {
            self.resolve_module_import(module_id, module_import.clone())
                .map(|symbol_id| TypedExpression {
                    kind: TypedExpressionKind::Symbol(
                        symbol_id,
                        SourceLoc::from_loc(module_import.loc.clone(), self.get_current_module_file_path()),
                    ),
                    concrete_type: None,
                    value_category: ValueCategory::Lvalue,
                    loc: SourceLoc::from_loc(module_import.loc.clone(), self.get_current_module_file_path()),
                })
        }
    }

    fn resolve_identifier_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        identifier: &Identifier,
    ) -> Option<TypedExpression> {
        let symbol_id = resolve_local_identifier!(self, local_scope_opt, module_id, identifier);
        Some(TypedExpression {
            kind: TypedExpressionKind::Symbol(
                symbol_id,
                SourceLoc::from_loc(identifier.loc.clone(), self.get_current_module_file_path()),
            ),
            concrete_type: None,
            value_category: ValueCategory::Lvalue,
            loc: SourceLoc::from_loc(identifier.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_func_call(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        func_call: &FuncCall,
    ) -> Option<TypedExpression> {
        if is_unscoped_expr!(self, local_scope_opt, func_call.loc.clone(), func_call.span.end) {
            return None;
        }

        let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &func_call.operand)?;

        let typed_args: Vec<TypedExpression> = func_call
            .args
            .iter()
            .filter_map(|arg| self.resolve_expr(module_id, local_scope_opt.clone(), arg))
            .collect();

        Some(TypedExpression {
            kind: TypedExpressionKind::FuncCall(TypedFuncCall {
                operand: Box::new(operand),
                args: typed_args,
                loc: SourceLoc::from_loc(func_call.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(func_call.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_array_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        arr: &Array,
    ) -> Option<TypedExpression> {
        let array_type = self.resolve_type(
            local_scope_opt.clone(),
            module_id,
            arr.data_type.clone(),
            arr.loc.clone(),
            arr.span.end,
        )?;

        let typed_elements: Vec<TypedExpression> = arr
            .elements
            .iter()
            .filter_map(|item| self.resolve_expr(module_id, local_scope_opt.clone(), item))
            .collect();

        Some(TypedExpression {
            kind: TypedExpressionKind::Array(TypedArray {
                array_type,
                elements: typed_elements,
                loc: SourceLoc::from_loc(arr.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(arr.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_infix_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        bin: &InfixExpression,
    ) -> Option<TypedExpression> {
        let lhs = self.resolve_expr(module_id, local_scope_opt.clone(), &*bin.lhs)?;
        let rhs = self.resolve_expr(module_id, local_scope_opt, &*bin.rhs)?;

        Some(TypedExpression {
            kind: TypedExpressionKind::Infix(TypedInfixExpression {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: bin.op.clone(),
                loc: SourceLoc::from_loc(bin.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(bin.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_prefix_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        prefix: &PrefixExpression,
    ) -> Option<TypedExpression> {
        let operand = self.resolve_expr(module_id, local_scope_opt, &*prefix.operand)?;

        Some(TypedExpression {
            kind: TypedExpressionKind::Prefix(TypedPrefixExpression {
                operand: Box::new(operand),
                op: prefix.op.clone(),
                loc: SourceLoc::from_loc(prefix.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(prefix.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_cast_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        cast: &Cast,
    ) -> Option<TypedExpression> {
        let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &*cast.expr)?;

        let target_type = self.resolve_type(
            local_scope_opt,
            module_id,
            cast.target_type.clone(),
            cast.loc.clone(),
            cast.span.end,
        )?;

        Some(TypedExpression {
            kind: TypedExpressionKind::Cast(TypedCast {
                operand: Box::new(operand),
                target_type,
                loc: SourceLoc::from_loc(cast.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(cast.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_type_specifier_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        type_specifier: &TypeSpecifier,
    ) -> Option<TypedExpression> {
        let (loc, span_end) = type_specifier.get_loc();

        let symbol_id = match type_specifier {
            TypeSpecifier::Identifier(identifier) => {
                resolve_local_identifier!(self, local_scope_opt, module_id, identifier)
            }
            TypeSpecifier::ModuleImport(module_import) => {
                self.resolve_module_import(module_id, module_import.clone())?
            }
            _ => {
                let concrete_type = self.resolve_type(
                    local_scope_opt,
                    module_id,
                    type_specifier.clone(),
                    loc.clone(),
                    span_end,
                )?;
                return Some(TypedExpression {
                    kind: TypedExpressionKind::ConcreteType(concrete_type.clone()),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: Some(concrete_type),
                    loc: SourceLoc::from_loc(loc, self.get_current_module_file_path()),
                });
            }
        };

        Some(TypedExpression {
            kind: TypedExpressionKind::Symbol(
                symbol_id,
                SourceLoc::from_loc(type_specifier.get_loc().0, self.get_current_module_file_path()),
            ),
            value_category: ValueCategory::Lvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(loc, self.get_current_module_file_path()),
        })
    }

    fn resolve_assign_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        assignment: &Assignment,
    ) -> Option<TypedExpression> {
        let lhs = self.resolve_expr(module_id, local_scope_opt.clone(), &assignment.lhs)?;
        let rhs = self.resolve_expr(module_id, local_scope_opt, &assignment.rhs)?;

        Some(TypedExpression {
            kind: TypedExpressionKind::Assignment(TypedAssignment {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                kind: assignment.kind.clone(),
                loc: SourceLoc::from_loc(assignment.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(assignment.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_literal_expr(&mut self, literal: &Literal) -> Option<TypedExpression> {
        let literal_type: Option<ConcreteType> = match &literal.kind {
            LiteralKind::Integer(_, suffix_opt) | LiteralKind::Float(_, suffix_opt) => {
                if let Some(token_kind) = suffix_opt {
                    match ConcreteType::try_from(*token_kind.clone()) {
                        Ok(concrete_type) => Some(concrete_type),
                        Err(_) => {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: ResolverDiagKind::InvalidLiteralSuffix,
                                location: Some(DiagLoc::new(SourceLoc::from_loc(
                                    literal.loc.clone(),
                                    self.get_current_module_file_path(),
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
                            Some(ConcreteType::Array(TypedArrayType {
                                element_type: Box::new(ConcreteType::BasicType(BasicConcreteType::Char)),
                                capacity: TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(len)),
                                loc: SourceLoc::from_loc(literal.loc.clone(), self.get_current_module_file_path()),
                            }))
                        }
                        StringPrefix::C => Some(ConcreteType::Pointer(Box::new(ConcreteType::BasicType(
                            BasicConcreteType::Char,
                        )))),
                    }
                } else {
                    Some(ConcreteType::Pointer(Box::new(ConcreteType::BasicType(
                        BasicConcreteType::Char,
                    ))))
                }
            }
            LiteralKind::Bool(_) => Some(ConcreteType::BasicType(BasicConcreteType::Bool)),
            LiteralKind::Char(_) => Some(ConcreteType::BasicType(BasicConcreteType::Char)),
            LiteralKind::Null => Some(ConcreteType::Pointer(Box::new(ConcreteType::BasicType(
                BasicConcreteType::Void,
            )))),
        };

        let typed_literal = TypedLiteral {
            ty: literal_type,
            kind: literal.kind.clone(),
            loc: SourceLoc::from_loc(literal.loc.clone(), self.get_current_module_file_path()),
        };

        Some(TypedExpression {
            kind: TypedExpressionKind::Literal(typed_literal.clone()),
            concrete_type: None,
            value_category: ValueCategory::Rvalue,
            loc: typed_literal.loc,
        })
    }

    fn resolve_unary_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        unary: &UnaryExpression,
    ) -> Option<TypedExpression> {
        let operand = self.resolve_expr(module_id, local_scope_opt, &*unary.operand)?;

        Some(TypedExpression {
            kind: TypedExpressionKind::Unary(TypedUnaryExpression {
                op: unary.op.clone(),
                operand: Box::new(operand),
                loc: SourceLoc::from_loc(unary.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(unary.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_array_index_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        array_index: &ArrayIndex,
    ) -> Option<TypedExpression> {
        let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &array_index.operand)?;
        let index = self.resolve_expr(module_id, local_scope_opt, &array_index.index)?;

        Some(TypedExpression {
            kind: TypedExpressionKind::ArrayIndex(TypedArrayIndex {
                operand: Box::new(operand),
                index: Box::new(index),
                loc: SourceLoc::from_loc(array_index.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Lvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(array_index.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_address_of_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        address_of: &AddressOf,
    ) -> Option<TypedExpression> {
        let operand = self.resolve_expr(module_id, local_scope_opt, &address_of.expr)?;

        Some(TypedExpression {
            kind: TypedExpressionKind::AddressOf(TypedAddressOf {
                operand: Box::new(operand),
                loc: SourceLoc::from_loc(address_of.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Lvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(address_of.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_deref_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        dereference: &Dereference,
    ) -> Option<TypedExpression> {
        let operand = self.resolve_expr(module_id, local_scope_opt, &dereference.expr)?;

        Some(TypedExpression {
            kind: TypedExpressionKind::Dereference(TypedDereference {
                operand: Box::new(operand),
                loc: SourceLoc::from_loc(dereference.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(dereference.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_unnamed_struct_value(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        unnamed_struct_value: &UnnamedStructValue,
    ) -> Option<TypedExpression> {
        let mut fields: Vec<TypedUnnamedStructValueField> = Vec::new();

        for field in &unnamed_struct_value.fields {
            let field_type = if let Some(type_specifier) = &field.field_type {
                match self.resolve_type(
                    local_scope_opt.clone(),
                    module_id,
                    type_specifier.clone(),
                    field.loc.clone(),
                    field.span.end,
                ) {
                    Some(concrete_type) => Some(concrete_type),
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
                field_name: field.field_name.name.clone(),
                field_type,
                field_value: Box::new(field_value),
                loc: SourceLoc::from_loc(field.loc.clone(), self.get_current_module_file_path()),
            });
        }

        Some(TypedExpression {
            kind: TypedExpressionKind::UnnamedStructValue(TypedUnnamedStructValue {
                fields,
                unnamed_struct_type: None,
                packed: unnamed_struct_value.packed,
                is_const: unnamed_struct_value.is_const,
                loc: SourceLoc::from_loc(unnamed_struct_value.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(unnamed_struct_value.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn resolve_size_of_expr(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        size_of_expression: &SizeOfExpression,
    ) -> Option<TypedExpression> {
        let typed_expr = self.resolve_expr(module_id, local_scope_opt, &size_of_expression.expr)?;

        Some(TypedExpression {
            kind: TypedExpressionKind::SizeOfExpression(TypedSizeOfExpression {
                expr: Box::new(typed_expr),
                loc: SourceLoc::from_loc(size_of_expression.loc.clone(), self.get_current_module_file_path()),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: SourceLoc::from_loc(size_of_expression.loc.clone(), self.get_current_module_file_path()),
        })
    }

    fn duplicate_symbol(&mut self, module_id: ModuleID, symbol_name: String, loc: SourceLoc) -> bool {
        match self.lookup_symbol_id(module_id, &symbol_name) {
            Some(..) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::DuplicateSymbol { symbol_name },
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

    fn get_module_alias(&self, group_name: &ModuleGroupName) -> Option<ModuleID> {
        let module_aliases = self.module_aliases.lock().unwrap();
        let imported_modules = module_aliases.get(&self.current_module.unwrap()).unwrap();
        let option = imported_modules.get(group_name).cloned();
        drop(module_aliases);
        option
    }

    fn get_module_id_by_file_path(&self, module_file_path: ModuleFilePath) -> Option<ModuleID> {
        let file_paths = self.file_paths.lock().unwrap();
        let module_id_opt = match file_paths.iter().find(|(_, fp)| **fp == module_file_path) {
            Some((module_id, _)) => Some(*module_id),
            None => None,
        };
        drop(file_paths);
        module_id_opt
    }

    fn insert_module_file_path(&self, module_id: ModuleID, module_file_path: ModuleFilePath) {
        let mut file_paths = self.file_paths.lock().unwrap();
        file_paths.insert(module_id, module_file_path);
        drop(file_paths);
    }

    fn insert_scope_ref(&self, module_id: ModuleID, scope_id: ScopeID, scope_ref: LocalScopeRef) {
        let mut global_symbols = self.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get_mut(&module_id).unwrap();
        symbol_table.scopes.insert(scope_id, scope_ref);
        drop(global_symbols);
    }

    pub fn get_current_module_file_path(&self) -> ModuleFilePath {
        let current_module_id = self.current_module.unwrap();
        let file_paths = self.file_paths.lock().unwrap();
        let file_path = match file_paths.get(&current_module_id) {
            Some(child_module_file_path) => child_module_file_path.clone(),
            None => self.master_module_file_path.clone(),
        };
        drop(file_paths);
        file_path
    }

    pub fn get_module_file_path(&self, module_id: ModuleID) -> Option<ModuleFilePath> {
        let file_paths = self.file_paths.lock().unwrap();
        let file_path = match file_paths.get(&module_id) {
            Some(module_file_path) => Some(module_file_path.clone()),
            None => None,
        };
        drop(file_paths);
        file_path
    }
}

fn get_module_name(module_file_path: String) -> String {
    let path = Path::new(&module_file_path);
    let file_stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("module");

    // hash the full path for uniqueness
    let mut hasher = DefaultHasher::new();
    path.to_string_lossy().hash(&mut hasher);
    let hash = hasher.finish();

    format!("{}_{}", file_stem, format!("{:x}", hash))
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

pub fn typed_func_decl_as_func_sig(func_decl: &TypedFuncDecl) -> FuncSig {
    FuncSig {
        module_id: func_decl.module_id,
        name: func_decl.name.clone(),
        params: func_decl.params.clone(),
        return_type: func_decl.return_type.clone(),
        is_func_decl: true,
        vis: func_decl.vis.clone(),
        loc: func_decl.loc.clone(),
    }
}

pub fn typed_func_def_as_func_sig(func_def: &TypedFuncDef) -> FuncSig {
    FuncSig {
        module_id: func_def.module_id,
        name: func_def.name.clone(),
        params: func_def.params.clone(),
        return_type: func_def.return_type.clone(),
        is_func_decl: false,
        vis: func_def.vis.clone(),
        loc: func_def.loc.clone(),
    }
}

pub fn typed_func_type_from_func_sig(func_sig: &FuncSig) -> TypedFuncType {
    TypedFuncType {
        params: typed_func_params_as_func_type_params(&func_sig.params),
        return_type: Box::new(func_sig.return_type.clone()),
        vis_opt: Some(func_sig.vis.clone()),
        loc: func_sig.loc.clone(),
    }
}

fn make_method_resolve_name(struct_symbol_id: SymbolID, method_name: String) -> String {
    format!("{}{}", struct_symbol_id, method_name)
}

pub fn typed_func_params_as_func_type_params(params: &TypedFuncParams) -> TypedFuncTypeParams {
    TypedFuncTypeParams {
        list: params
            .list
            .iter()
            .map(|param| match param {
                TypedFuncParamKind::FuncParam(typed_func_param) => typed_func_param.ty.clone(),
                TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
                    let ty = ConcreteType::UnresolvedSymbol(typed_self_modifier.symbol_id.unwrap());
                    match typed_self_modifier.kind {
                        SelfModifierKind::Copied => ty,
                        SelfModifierKind::Referenced => ConcreteType::Pointer(Box::new(ty)),
                    }
                }
            })
            .collect(),
        variadic: match &params.variadic {
            Some(variadic) => match variadic {
                TypedFuncVariadicParams::UntypedCStyle => Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle)),
                TypedFuncVariadicParams::Typed(_, concrete_type) => {
                    Some(Box::new(TypedFuncTypeVariadicParams::Typed(concrete_type.clone())))
                }
            },
            None => None,
        },
    }
}
