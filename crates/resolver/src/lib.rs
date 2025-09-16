use crate::declsign::{EnumSig, GlobalVarSig, InterfaceSig, StructSig, UnionSig};
use crate::moduleloader::{ModuleAlias, ModuleFilePath, ModuleLoader, ModuleLoaderOptions};
use crate::scope::*;
use crate::{
    declsign::{FuncSig, TypedefSig},
    diagnostics::ResolverDiagKind,
};
use ast::format::module_segments_as_string;
use ast::{
    AccessSpecifier, GlobalVariable, If, Import, Interface, ModuleImport, ModulePath, ModuleSegment,
    ModuleSegmentSingle, SelfModifierKind, StringPrefix, SwitchCasePattern, Union,
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
    BasicConcreteType, ConcreteType, TypedArrayCapacity, TypedArrayFixedCapacityValue, TypedArrayType,
    TypedUnnamedStructType, TypedUnnamedStructTypeField,
};
use typed_ast::{SymbolID, *};

pub mod declsign;
mod diagnostics;
pub mod moduleloader;
pub mod scope;

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
    pub file_paths: HashSet<ModuleFilePath>,
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

    fn resolve_module_import(&mut self, module_id: ModuleID, mut module_import: ModuleImport) -> Option<SymbolID> {
        if let Some(identifier) = module_import.as_identifier() {
            return self.resolve_identifier(module_id, identifier, module_import.loc.clone(), module_import.span.end);
        }

        assert!(module_import.segments.len() >= 2);

        let symbol_name = module_import.segments.pop().unwrap().as_identifier().name;

        let module_import_alias = module_segments_as_string(module_import.segments.clone());
        let module_id = match self.get_module_alias(module_import_alias.clone()) {
            Some(module_id) => module_id,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::ModuleImportNotFound {
                        module_name: module_import_alias,
                    },
                    location: Some(DiagLoc::new(
                        self.get_current_module_file_path(),
                        module_import.loc.clone(),
                        module_import.span.end,
                    )),
                    hint: None,
                });
                return None;
            }
        };

        let symbol_id = match self.lookup_symbol_id(module_id, &symbol_name) {
            Some(symbol_id) => symbol_id,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::SymbolIsNotDefinedInModule {
                        symbol_name,
                        module_name: module_import_alias,
                    },
                    location: Some(DiagLoc::new(
                        self.get_current_module_file_path(),
                        module_import.loc.clone(),
                        module_import.span.end,
                    )),
                    hint: None,
                });
                return None;
            }
        };

        Some(symbol_id)
    }

    fn skip_module_if_loaded_once(&self, file_path: String) -> bool {
        let file_paths = self.file_paths.lock().unwrap();
        let exists = file_paths.iter().find(|(_, fp)| **fp == file_path).is_some();
        drop(file_paths);
        exists
    }

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
                    location: Some(DiagLoc::new(
                        self.get_current_module_file_path(),
                        import.loc.clone(),
                        import.span.end,
                    )),
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
            let (module_alias, module_file_path, program_tree) = match loaded_module {
                Ok(module_ast_and_file_path) => module_ast_and_file_path,
                Err(diag_kind) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: diag_kind,
                        location: Some(DiagLoc::new(
                            self.get_current_module_file_path(),
                            import.loc.clone(),
                            import.span.end,
                        )),
                        hint: None,
                    });
                    continue;
                }
            };

            let module_id = {
                self.get_module_id_by_file_path(module_file_path.clone())
                    .unwrap_or(generate_module_id())
            };

            if visiting.contains(module_file_path.clone()) {
                // Cycle import detected.
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::ImportCycle {
                        module_names: {
                            let file_paths = self.file_paths.lock().unwrap();
                            let module_names = visiting.file_paths.clone().into_iter().collect();
                            drop(file_paths);
                            module_names
                        },
                    },
                    location: None,
                    hint: None,
                });
                continue;
            }

            visiting.insert(module_file_path.clone());

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
                self.insert_module_file_path(module_id, module_file_path);

                match self.resolve_module(module_id, program_tree.as_ref(), &mut visiting, false) {
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
        singles: &Vec<ModuleSegmentSingle>,
        loc: Location,
    ) {
        // move symbol and it's metadata from imported module to parent module

        for single in singles {
            let single_actual_name = single.identifier.as_string();
            let single_renamed_name = single.renamed.clone().unwrap_or(single.identifier.clone()).as_string();

            let symbol_id = match self.lookup_symbol_id(imported_module_id, &single_actual_name) {
                Some(symbol_id) => symbol_id,
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::SymbolNotFound {
                            name: single_renamed_name,
                        },
                        location: Some(DiagLoc::new(
                            self.get_module_file_path(parent_module_id).unwrap(),
                            loc.clone(),
                            0,
                        )),
                        hint: None,
                    });
                    continue;
                }
            };

            let loc_file = self.get_module_file_path(imported_module_id).unwrap();

            let symbol_entry = self.resolve_global_symbol(symbol_id).unwrap();
            {
                let mut global_symbols = self.global_symbols.lock().unwrap();
                let symbol_table = global_symbols.get_mut(&parent_module_id).unwrap();

                if symbol_table.names.contains_key(&single_actual_name) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::DuplicateSymbol {
                            symbol_name: single_renamed_name,
                        },
                        location: Some(DiagLoc::new(
                            self.get_module_file_path(parent_module_id).unwrap(),
                            loc.clone(),
                            0,
                        )),
                        hint: None,
                    });
                    continue;
                }

                symbol_table.names.insert(single_renamed_name.clone(), symbol_id);
                symbol_table
                    .locs
                    .insert(symbol_id, (loc_file, symbol_entry.get_loc(), 0));

                let vis = symbol_entry.get_vis();

                symbol_table.entries.insert(symbol_id, symbol_entry);
                drop(global_symbols);

                self.check_import_single_vis(single_actual_name, vis, loc.clone());
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
                location: Some(DiagLoc::new(self.get_current_module_file_path(), loc.clone(), 0)),
                hint: None,
            });
        }
    }

    fn init_imported_modules_for_module(&mut self) {
        let mut module_aliases = self.module_aliases.lock().unwrap();
        module_aliases.insert(self.current_module.unwrap(), HashMap::new());
        drop(module_aliases);
    }

    pub fn resolve_module(
        &mut self,
        module_id: ModuleID,
        ast: &ProgramTree,
        mut visiting: &mut Visiting,
        is_master: bool,
    ) -> Option<Rc<RefCell<TypedProgramTree>>> {
        self.current_module = Some(module_id);
        self.init_imported_modules_for_module();
        self.already_imported_modules.clear();
        visiting.file_paths.clear();

        if is_master {
            self.insert_module_file_path(module_id, self.master_module_file_path.clone());
            visiting.insert(self.master_module_file_path.clone());
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
            let module_file_path = self.get_current_module_file_path();

            let mut program_trees = self.program_trees.lock().unwrap();
            let module_name = get_module_name(module_file_path.clone());
            program_trees.push((
                module_name,
                module_file_path,
                self.current_module.unwrap(),
                typed_program_tree.clone(),
            ));
            drop(program_trees);
        }

        Some(typed_program_tree.clone())
    }

    fn resolve_type(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        module_id: ModuleID,
        type_specifier: TypeSpecifier,
        loc: Location,
        span_end: usize,
    ) -> Option<ConcreteType> {
        let resolving_result = match type_specifier {
            TypeSpecifier::TypeToken(token) => match ConcreteType::try_from(token.kind.clone()) {
                Ok(concrete_type) => Ok(concrete_type),
                Err(_) => Err(ResolverDiagKind::TypeNotFound {
                    name: token.kind.to_string(),
                }),
            },
            TypeSpecifier::Const(type_specifier) => Ok(ConcreteType::Const(Box::new(self.resolve_type(
                local_scope_opt,
                module_id,
                *type_specifier,
                loc.clone(),
                span_end,
            )?))),
            TypeSpecifier::Dereference(type_specifier) => Ok(ConcreteType::Pointer(Box::new(self.resolve_type(
                local_scope_opt,
                module_id,
                *type_specifier,
                loc.clone(),
                span_end,
            )?))),
            TypeSpecifier::Array(array_type_specifier) => Ok({
                let element_type = match self.resolve_type(
                    local_scope_opt.clone(),
                    module_id,
                    *array_type_specifier.element_type,
                    loc.clone(),
                    span_end,
                ) {
                    Some(concrete_type) => concrete_type,
                    None => return None,
                };

                let capacity = match &array_type_specifier.size {
                    ArrayCapacity::Fixed(expr) => {
                        let typed_expr = match self.resolve_expr(module_id, local_scope_opt, expr) {
                            Some(typed_expr) => typed_expr,
                            None => return None,
                        };

                        if let TypedExpressionKind::Literal(typed_literal) = &typed_expr.kind {
                            if let LiteralKind::Integer(value, ..) = &typed_literal.kind {
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

                ConcreteType::Array(TypedArrayType {
                    element_type: Box::new(element_type),
                    capacity,
                    loc: loc.clone(),
                })
            }),
            TypeSpecifier::UnnamedStruct(unnamed_struct_type) => {
                let mut fields: Vec<TypedUnnamedStructTypeField> = Vec::new();

                for field in &unnamed_struct_type.fields {
                    match self.resolve_type(
                        local_scope_opt.clone(),
                        module_id,
                        field.field_type.clone(),
                        field.loc.clone(),
                        field.span.end,
                    ) {
                        Some(concrete_type) => {
                            fields.push(TypedUnnamedStructTypeField {
                                field_name: field.field_name.name.clone(),
                                field_type: Box::new(concrete_type),
                                loc: field.loc.clone(),
                            });
                        }
                        None => continue,
                    };
                }

                Ok(ConcreteType::UnnamedStruct(TypedUnnamedStructType {
                    fields,
                    packed: unnamed_struct_type.packed,
                    loc: unnamed_struct_type.loc.clone(),
                }))
            }
            TypeSpecifier::ModuleImport(module_import) => match self.resolve_module_import(module_id, module_import) {
                Some(symbol_id) => Ok(ConcreteType::UnresolvedSymbol(symbol_id)),
                None => return None,
            },
            TypeSpecifier::Identifier(identifier) => {
                match match self.lookup_symbol_id(module_id, &identifier.name.clone()) {
                    Some(symbol_entry) => Some(symbol_entry),
                    None => None,
                } {
                    Some(symbol_id) => Ok(ConcreteType::UnresolvedSymbol(symbol_id)),
                    None => Err(ResolverDiagKind::TypeNotFound {
                        name: identifier.name.clone(),
                    }),
                }
            }
        };

        match resolving_result {
            Ok(concrete_typed) => Some(concrete_typed),
            Err(diag_kind) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: diag_kind,
                    location: Some(DiagLoc::new(self.get_current_module_file_path(), loc, span_end)),
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

    fn insert_symbol_name(
        &mut self,
        module_id: ModuleID,
        name: &String,
        loc_file: String,
        loc: Location,
        span_end: usize,
    ) -> SymbolID {
        let symbol_id = generate_symbol_id();
        let mut global_symbols = self.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get_mut(&module_id).unwrap();
        symbol_table.names.insert(name.clone(), symbol_id);
        symbol_table.locs.insert(symbol_id, (loc_file, loc, span_end));
        drop(global_symbols);
        symbol_id
    }

    fn get_symbol_loc(&self, module_id: ModuleID, symbol_id: SymbolID) -> Option<(String, Location, usize)> {
        let mut global_symbols = self.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get_mut(&module_id).unwrap();
        let option = symbol_table.locs.get(&symbol_id).cloned();
        drop(global_symbols);
        option
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
                        interface.loc.clone(),
                        interface.span.end,
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(
                        module_id,
                        &interface.identifier.name.clone(),
                        self.get_current_module_file_path(),
                        interface.loc.clone(),
                        interface.span.end,
                    );
                }
                Statement::Union(union_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        union_decl.identifier.name.clone(),
                        union_decl.loc.clone(),
                        union_decl.span.end,
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(
                        module_id,
                        &union_decl.identifier.name.clone(),
                        self.get_current_module_file_path(),
                        union_decl.loc.clone(),
                        union_decl.span.end,
                    );
                }
                Statement::Typedef(typedef) => {
                    if self.duplicate_symbol(
                        module_id,
                        typedef.identifier.name.clone(),
                        typedef.loc.clone(),
                        typedef.span.end,
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(
                        module_id,
                        &typedef.identifier.name.clone(),
                        self.get_current_module_file_path(),
                        typedef.loc.clone(),
                        typedef.span.end,
                    );
                }
                Statement::FuncDef(func_def) => {
                    if self.duplicate_symbol(
                        module_id,
                        func_def.identifier.name.clone(),
                        func_def.loc.clone(),
                        func_def.span.end,
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(
                        module_id,
                        &func_def.identifier.name.clone(),
                        self.get_current_module_file_path(),
                        func_def.loc.clone(),
                        func_def.span.end,
                    );
                }
                Statement::FuncDecl(func_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        func_decl.identifier.name.clone(),
                        func_decl.loc.clone(),
                        func_decl.span.end,
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(
                        module_id,
                        &func_decl.get_usable_name(),
                        self.get_current_module_file_path(),
                        func_decl.loc.clone(),
                        func_decl.span.end,
                    );
                }
                Statement::GlobalVariable(global_variable) => {
                    if self.duplicate_symbol(
                        module_id,
                        global_variable.identifier.name.clone(),
                        global_variable.loc.clone(),
                        global_variable.span.end,
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(
                        module_id,
                        &global_variable.identifier.name.clone(),
                        self.get_current_module_file_path(),
                        global_variable.loc.clone(),
                        global_variable.span.end,
                    );
                }
                Statement::Struct(struct_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        struct_decl.identifier.name.clone(),
                        struct_decl.loc.clone(),
                        struct_decl.span.end,
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(
                        module_id,
                        &struct_decl.identifier.name.clone(),
                        self.get_current_module_file_path(),
                        struct_decl.loc.clone(),
                        struct_decl.span.end,
                    );
                }
                Statement::Enum(enum_decl) => {
                    if self.duplicate_symbol(
                        module_id,
                        enum_decl.identifier.name.clone(),
                        enum_decl.loc.clone(),
                        enum_decl.span.end,
                    ) {
                        continue;
                    }

                    self.insert_symbol_name(
                        module_id,
                        &enum_decl.identifier.name.clone(),
                        self.get_current_module_file_path(),
                        enum_decl.loc.clone(),
                        enum_decl.span.end,
                    );
                }
                _ => {}
            };
        }
    }

    // Resolves the full meaning of each top-level declaration in the AST (second pass)
    fn resolve_definitions(&mut self, module_id: ModuleID, ast: &ProgramTree) -> Vec<TypedStatement> {
        let mut typed_body: Vec<TypedStatement> = Vec::new();

        for stmt in ast.body.as_ref() {
            let valid_top_level_stmt: Result<TypedStatement, (Location, usize)> = match stmt {
                Statement::Import(_) => continue,
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
                Statement::Struct(struct_decl) => match self.resolve_struct(module_id, None, struct_decl) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Enum(enum_decl) => match self.resolve_enum(module_id, None, enum_decl) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Interface(interface) => match self.resolve_interface(module_id, None, interface) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Union(union_stmt) => match self.resolve_union(module_id, None, union_stmt) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Variable(variable) => Err((variable.loc.clone(), variable.span.end)),
                Statement::If(if_stmt) => Err((if_stmt.loc.clone(), if_stmt.span.end)),
                Statement::Return(return_stmt) => Err((return_stmt.loc.clone(), return_stmt.span.end)),
                Statement::For(for_stmt) => Err((for_stmt.loc.clone(), for_stmt.span.end)),
                Statement::Foreach(foreach) => Err((foreach.loc.clone(), foreach.span.end)),
                Statement::Switch(switch) => Err((switch.loc.clone(), switch.span.end)),
                Statement::BlockStatement(block_statement) => {
                    Err((block_statement.loc.clone(), block_statement.span.end))
                }
                Statement::Break(break_stmt) => Err((break_stmt.loc.clone(), break_stmt.span.end)),
                Statement::Continue(continue_stmt) => Err((continue_stmt.loc.clone(), continue_stmt.span.end)),
                Statement::Expression(..) => continue,
                Statement::While(..) => continue,
            };

            match valid_top_level_stmt {
                Ok(typed_stmt) => {
                    typed_body.push(typed_stmt);
                }
                Err((loc, span_end)) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::InvalidTopLevelStatement,
                        location: Some(DiagLoc::new(self.get_current_module_file_path(), loc, span_end)),
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
        let interface_symbol_id = if local_scope_opt.is_some() {
            generate_symbol_id() // new symbol
        } else {
            self.lookup_symbol_id(module_id, &interface.identifier.name).unwrap()
        };

        let mut typed_methods: Vec<TypedFuncDecl> = Vec::new();

        for func_decl in &interface.methods {
            match self.resolve_func(module_id, local_scope_opt.clone(), &func_decl) {
                Some((return_type, typed_func_params, typed_variadic_param)) => {
                    if func_decl.renamed_as.is_some() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: ResolverDiagKind::RenameInterfaceMethod,
                            location: Some(DiagLoc::new(
                                self.get_current_module_file_path(),
                                interface.loc.clone(),
                                interface.span.end,
                            )),
                            hint: None,
                        });
                    }

                    let typed_func_decl = TypedFuncDecl {
                        symbol_id: interface_symbol_id,
                        name: func_decl.identifier.name.clone(),
                        params: TypedFuncParams {
                            list: typed_func_params,
                            variadic: typed_variadic_param,
                        },
                        return_type,
                        vis: func_decl.vis.clone(),
                        renamed_as: None,
                        loc: func_decl.loc.clone(),
                    };

                    typed_methods.push(typed_func_decl);
                }
                None => continue,
            }
        }

        let resolved_interface = ResolvedInterface {
            module_id,
            symbol_id: interface_symbol_id,
            interface_sig: InterfaceSig {
                name: interface.identifier.name.clone(),
                methods: typed_methods.clone(),
                vis: interface.vis.clone(),
                loc: interface.loc.clone(),
            },
        };

        if let Some(local_scope_rc) = &local_scope_opt {
            let mut local_scope = local_scope_rc.borrow_mut();
            local_scope.insert(
                interface.identifier.name.clone(),
                LocalSymbol::new(LocalSymbolKind::Interface(resolved_interface)),
            );
            drop(local_scope);
        } else {
            self.insert_symbol_entry(
                module_id,
                interface_symbol_id,
                SymbolEntry::new(SymbolEntryKind::Interface(resolved_interface)),
            );
        }

        Some(TypedStatement::Interface(TypedInterface {
            name: interface.identifier.name.clone(),
            symbol_id: interface_symbol_id,
            methods: typed_methods,
            vis: interface.vis.clone(),
            loc: interface.loc.clone(),
        }))
    }

    fn resolve_union(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        union_decl: &Union,
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
                        loc: field.loc.clone(),
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
                name: union_decl.identifier.name.clone(),
                fields: typed_union_fields.clone(),
                methods: methods.clone(),
                vis: union_decl.vis.clone(),
                loc: union_decl.loc.clone(),
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
            loc: union_decl.identifier.loc.clone(),
        }))
    }

    fn resolve_enum(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        enum_decl: &Enum,
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
                            loc: valued_field.loc.clone(),
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
                name: enum_decl.identifier.name.clone(),
                methods: methods.clone(),
                variants: variants.clone(),
                vis: enum_decl.vis.clone(),
                loc: enum_decl.loc.clone(),
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
            loc: enum_decl.identifier.loc.clone(),
        }))
    }

    fn resolve_global_var(&mut self, module_id: ModuleID, global_var: &GlobalVariable) -> Option<TypedStatement> {
        let concrete_type = {
            if let Some(type_specifier) = &global_var.type_specifier {
                match self.resolve_type(
                    None,
                    module_id,
                    type_specifier.clone(),
                    global_var.loc.clone(),
                    global_var.span.end,
                ) {
                    Some(concrete_type) => Some(concrete_type),
                    None => None,
                }
            } else {
                None
            }
        };

        let typed_expr = {
            if let Some(expr) = &global_var.expr {
                match self.resolve_expr(module_id, None, &expr) {
                    Some(typed_expr) => Some(typed_expr),
                    None => None,
                }
            } else {
                None
            }
        };

        let symbol_id = self.lookup_symbol_id(module_id, &global_var.identifier.name).unwrap();

        self.insert_symbol_entry(
            module_id,
            symbol_id,
            SymbolEntry::new(SymbolEntryKind::GlobalVar(ResolvedGlobalVar {
                module_id,
                symbol_id,
                global_var_sig: GlobalVarSig {
                    module_id,
                    name: global_var.identifier.name.clone(),
                    ty: concrete_type.clone(),
                    rhs: typed_expr.clone(),
                    vis: global_var.vis.clone(),
                    loc: global_var.loc.clone(),
                },
            })),
        );

        Some(TypedStatement::GlobalVariable(TypedGlobalVariable {
            module_id,
            symbol_id,
            name: global_var.identifier.name.clone(),
            ty: concrete_type,
            expr: typed_expr,
            vis: global_var.vis.clone(),
            loc: global_var.loc.clone(),
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
                    location: Some(DiagLoc::new(
                        self.get_current_module_file_path(),
                        func_def.loc.clone(),
                        0,
                    )),
                    hint: Some("Consider to rename the method to a different name.".to_string()),
                });
                continue;
            }

            method_names.push(method_name);
        }
    }

    pub fn make_method_resolve_name(struct_symbol_id: SymbolID, method_name: String) -> String {
        format!("{}{}", struct_symbol_id, method_name)
    }

    fn resolve_methods(
        &mut self,
        module_id: ModuleID,
        methods_list: &Vec<FuncDef>,
        struct_symbol_id: SymbolID,
    ) -> Option<HashMap<String, SymbolID>> {
        let mut methods: HashMap<String, SymbolID> = HashMap::new();
        let mut method_bodies: HashMap<SymbolID, (LocalScopeRef, Box<BlockStatement>, ScopeID)> = HashMap::new();

        for func_def in methods_list {
            let method_scope_id = generate_scope_id();
            let local_scope_rc = Rc::new(RefCell::new(LocalScope::new(None)));
            self.insert_scope_ref(module_id, method_scope_id, local_scope_rc.clone());

            match self.resolve_func(module_id, Some(local_scope_rc.clone()), &func_def.as_func_decl()) {
                Some((return_type, mut typed_func_params, typed_variadic_param)) => {
                    let method_name = func_def.identifier.name.clone();
                    let method_resolve_name = Resolver::make_method_resolve_name(struct_symbol_id, method_name.clone());

                    // resolve self modifier
                    typed_func_params = typed_func_params
                        .iter()
                        .map(|typed_func_param_kind| match typed_func_param_kind.clone() {
                            TypedFuncParamKind::FuncParam(typed_func_param) => {
                                TypedFuncParamKind::FuncParam(typed_func_param)
                            }
                            TypedFuncParamKind::SelfModifier(mut typed_self_modifier) => {
                                typed_self_modifier.symbol_id = Some(struct_symbol_id);
                                TypedFuncParamKind::SelfModifier(typed_self_modifier)
                            }
                        })
                        .collect();

                    let symbol_id = self.insert_symbol_name(
                        module_id,
                        &method_resolve_name,
                        self.get_current_module_file_path(),
                        func_def.loc.clone(),
                        func_def.span.end,
                    );
                    methods.insert(method_name.clone(), symbol_id);
                    self.insert_symbol_entry(
                        module_id,
                        symbol_id,
                        SymbolEntry::new(SymbolEntryKind::Method(ResolvedMethod {
                            module_id,
                            symbol_id,
                            func_sig: FuncSig {
                                module_id,
                                name: method_name,
                                is_func_decl: false,
                                params: TypedFuncParams {
                                    list: typed_func_params,
                                    variadic: typed_variadic_param,
                                },
                                return_type,
                                vis: func_def.vis.clone(),
                                loc: func_def.loc.clone(),
                            },
                            func_body: None,
                        })),
                    );

                    method_bodies.insert(
                        symbol_id,
                        (Rc::clone(&local_scope_rc), func_def.body.clone(), method_scope_id),
                    );
                }
                None => continue,
            }
        }

        for (.., symbol_id) in methods.iter() {
            let mut resolved_method = match self.lookup_symbol_entry_with_id(module_id, *symbol_id).unwrap().kind {
                SymbolEntryKind::Method(resolved_method) => resolved_method,
                _ => unreachable!(),
            };

            let (local_scope_rc, method_body, method_scope_id) = method_bodies.get(symbol_id).unwrap();

            for typed_param_kind in &mut resolved_method.func_sig.params.list {
                let self_symbol_name = "self".to_string();
                let self_symbol_id = generate_symbol_id();

                match typed_param_kind {
                    TypedFuncParamKind::SelfModifier(self_modifier) => {
                        self_modifier.self_symbol_id = Some(self_symbol_id);

                        let local_symbol = match self_modifier.kind {
                            SelfModifierKind::Copied => LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                                module_id,
                                symbol_id: self_symbol_id,
                                typed_variable: TypedVariable {
                                    symbol_id: self_symbol_id,
                                    name: self_symbol_name.clone(),
                                    ty: Some(ConcreteType::UnresolvedSymbol(struct_symbol_id)),
                                    rhs: None,
                                    loc: resolved_method.func_sig.loc.clone(),
                                },
                            })),
                            SelfModifierKind::Referenced => {
                                LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                                    module_id,
                                    symbol_id: self_symbol_id,
                                    typed_variable: TypedVariable {
                                        symbol_id: self_symbol_id,
                                        name: self_symbol_name.clone(),
                                        ty: Some(ConcreteType::Pointer(Box::new(ConcreteType::UnresolvedSymbol(
                                            struct_symbol_id,
                                        )))),
                                        rhs: None,
                                        loc: resolved_method.func_sig.loc.clone(),
                                    },
                                }))
                            }
                        };

                        let mut local_scope = local_scope_rc.borrow_mut();
                        local_scope.insert(self_symbol_name.clone(), local_symbol);
                        drop(local_scope);
                    }
                    TypedFuncParamKind::FuncParam(..) => continue,
                }
            }

            let method_scope = LocalScope::deep_clone(&local_scope_rc);

            let typed_func_body =
                match self.resolve_block_statement(*method_scope_id, method_scope.clone(), &method_body) {
                    Some(typed_block_statement) => typed_block_statement,
                    None => return None,
                };

            resolved_method.func_body = Some(Box::new(typed_func_body));
            self.insert_symbol_entry(
                module_id,
                *symbol_id,
                SymbolEntry::new(SymbolEntryKind::Method(resolved_method)),
            );
        }

        Some(methods)
    }

    fn resolve_struct(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        struct_decl: &Struct,
    ) -> Option<TypedStatement> {
        let struct_symbol_id = if local_scope_opt.is_some() {
            generate_symbol_id() // new symbol
        } else {
            self.lookup_symbol_id(module_id, &struct_decl.identifier.name).unwrap()
        };

        let mut typed_struct_fields: Vec<TypedStructField> = Vec::new();

        for field in &struct_decl.fields {
            match self.resolve_type(
                local_scope_opt.clone(),
                module_id,
                field.ty.clone(),
                field.loc.clone(),
                field.span.end,
            ) {
                Some(concrete_type) => {
                    typed_struct_fields.push(TypedStructField {
                        name: field.identifier.name.clone(),
                        vis: field.vis.clone(),
                        ty: concrete_type,
                        loc: field.loc.clone(),
                    });
                }
                None => continue,
            }
        }

        self.check_duplicate_method_names(&struct_decl.identifier.name, struct_decl.methods.clone());

        let methods = match self.resolve_methods(module_id, &struct_decl.methods, struct_symbol_id) {
            Some(methods) => methods,
            None => return None,
        };

        let mut impls: Vec<LocalOrGlobalSymbol> = Vec::new();

        for identifier in &struct_decl.impls {
            let interface_option = {
                let option = {
                    if let Some(local_scope) = &local_scope_opt {
                        let local_scope = local_scope.borrow();
                        let local_option = match local_scope.resolve(&identifier.as_string()) {
                            Some(local_symbol) => Some(LocalOrGlobalSymbol::LocalSymbol(local_symbol.clone())),
                            None => None,
                        };
                        drop(local_scope);
                        // option
                        local_option
                    } else {
                        None
                    }
                };

                match option {
                    Some(local_or_global_symbol) => Some(local_or_global_symbol),
                    None => match self.lookup_symbol(module_id, &identifier.name) {
                        Some(global_symbol) => Some(LocalOrGlobalSymbol::GlobalSymbol(global_symbol)),
                        None => None,
                    },
                }
            };

            let interface = match interface_option {
                Some(local_or_global_symbol) => local_or_global_symbol,
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::SymbolNotFound {
                            name: identifier.as_string(),
                        },
                        location: Some(DiagLoc::new(
                            self.get_current_module_file_path(),
                            identifier.loc.clone(),
                            identifier.span.end,
                        )),
                        hint: None,
                    });
                    continue;
                }
            };

            impls.push(interface);
        }

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
                loc: struct_decl.loc.clone(),
            },
        };

        if let Some(local_scope_rc) = &local_scope_opt {
            let mut local_scope = local_scope_rc.borrow_mut();
            local_scope.insert(
                struct_decl.identifier.name.clone(),
                LocalSymbol::new(LocalSymbolKind::Struct(resolved_struct)),
            );
            drop(local_scope);
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
            packed: struct_decl.packed.clone(),
            loc: struct_decl.loc.clone(),
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

        let return_type = match self.resolve_type(
            local_scope_opt.clone(),
            module_id,
            return_type_specifier,
            func_decl.loc.clone(),
            func_decl.span.end,
        ) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let mut typed_func_params: Vec<TypedFuncParamKind> = Vec::new();

        for param in &func_decl.params.list {
            match param {
                FuncParamKind::FuncParam(func_param) => {
                    let param_type = match &func_param.ty {
                        Some(type_specifier) => {
                            match self.resolve_type(
                                local_scope_opt.clone(),
                                module_id,
                                type_specifier.clone(),
                                func_param.loc.clone(),
                                func_param.span.end,
                            ) {
                                Some(concrete_type) => concrete_type,
                                None => continue,
                            }
                        }
                        None => {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: ResolverDiagKind::InvalidUntypedFuncParam,
                                location: Some(DiagLoc::new(
                                    self.get_current_module_file_path(),
                                    func_param.loc.clone(),
                                    func_param.span.end,
                                )),
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
                                    loc: func_param.loc.clone(),
                                },
                            })),
                        );
                        drop(local_scope);
                    }

                    typed_func_params.push(TypedFuncParamKind::FuncParam(TypedFuncParam {
                        name: func_param.identifier.name.clone(),
                        ty: param_type,
                        loc: func_param.loc.clone(),
                    }));
                }
                FuncParamKind::SelfModifier(self_modifier) => {
                    let typed_self_modifier = TypedSelfModifier {
                        symbol_id: None,
                        self_symbol_id: None,
                        ty: None,
                        kind: self_modifier.kind.clone(),
                        loc: self_modifier.loc.clone(),
                    };

                    typed_func_params.push(TypedFuncParamKind::SelfModifier(typed_self_modifier));
                }
            }
        }

        let typed_variadic_param = {
            if let Some(variadic_param) = &func_decl.params.variadic {
                match variadic_param {
                    FuncVariadicParams::UntypedCStyle => Some(TypedFuncVariadicParams::UntypedCStyle),
                    FuncVariadicParams::Typed(identifier, type_specifier) => {
                        let variadic_type = match self.resolve_type(
                            local_scope_opt.clone(),
                            module_id,
                            type_specifier.clone(),
                            identifier.loc.clone(),
                            identifier.span.end,
                        ) {
                            Some(concrete_type) => concrete_type,
                            None => return None,
                        };

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
                                        loc: identifier.loc.clone(),
                                    },
                                })),
                            );

                            drop(local_scope);
                        }

                        Some(TypedFuncVariadicParams::Typed(identifier.name.clone(), variadic_type))
                    }
                }
            } else {
                None
            }
        };

        Some((return_type, typed_func_params, typed_variadic_param))
    }

    fn resolve_func_decl(&mut self, module_id: ModuleID, func_decl: &FuncDecl) -> Option<TypedStatement> {
        let symbol_id = self.lookup_symbol_id(module_id, &func_decl.get_usable_name()).unwrap();

        match self.resolve_func(module_id, None, func_decl) {
            Some((return_type, typed_func_params, typed_variadic_param)) => {
                self.insert_symbol_entry(
                    module_id,
                    symbol_id,
                    SymbolEntry::new(SymbolEntryKind::Func(ResolvedFunction {
                        module_id,
                        symbol_id,
                        func_sig: FuncSig {
                            module_id,
                            name: func_decl.identifier.name.clone(),
                            is_func_decl: true,
                            params: TypedFuncParams {
                                list: typed_func_params.clone(),
                                variadic: typed_variadic_param.clone(),
                            },
                            return_type: return_type.clone(),
                            vis: func_decl.vis.clone(),
                            loc: func_decl.loc.clone(),
                        },
                    })),
                );

                Some(TypedStatement::FuncDecl(TypedFuncDecl {
                    symbol_id,
                    name: func_decl.identifier.name.clone(),
                    params: TypedFuncParams {
                        list: typed_func_params,
                        variadic: typed_variadic_param,
                    },
                    return_type,
                    vis: func_decl.vis.clone(),
                    renamed_as: match &func_decl.renamed_as {
                        Some(identifier) => Some(identifier.as_string()),
                        None => None,
                    },
                    loc: func_decl.loc.clone(),
                }))
            }
            None => None,
        }
    }

    fn resolve_func_def(&mut self, module_id: ModuleID, func_def: &FuncDef) -> Option<TypedStatement> {
        let scope_id = generate_scope_id();
        let body_scope = Rc::new(RefCell::new(LocalScope::new(None)));
        self.insert_scope_ref(module_id, scope_id, body_scope.clone());

        let symbol_id = self
            .lookup_symbol_id(module_id, &func_def.identifier.name.clone())
            .unwrap();

        match self.resolve_func(module_id, Some(body_scope.clone()), &func_def.as_func_decl()) {
            Some((return_type, typed_func_params, typed_variadic_param)) => {
                self.insert_symbol_entry(
                    module_id,
                    symbol_id,
                    SymbolEntry::new(SymbolEntryKind::Func(ResolvedFunction {
                        module_id,
                        symbol_id,
                        func_sig: FuncSig {
                            module_id,
                            name: func_def.identifier.name.clone(),
                            is_func_decl: false,
                            params: TypedFuncParams {
                                list: typed_func_params.clone(),
                                variadic: typed_variadic_param.clone(),
                            },
                            return_type: return_type.clone(),
                            vis: func_def.vis.clone(),
                            loc: func_def.loc.clone(),
                        },
                    })),
                );

                let typed_func_body = match self.resolve_block_statement(scope_id, body_scope.clone(), &func_def.body) {
                    Some(typed_block_statement) => typed_block_statement,
                    None => return None,
                };

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
                    loc: func_def.loc.clone(),
                    body: Box::new(typed_func_body),
                }))
            }
            None => None,
        }
    }

    fn resolve_typedef(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        typedef: &Typedef,
    ) -> Option<TypedStatement> {
        match self.lookup_symbol_id(module_id, &typedef.identifier.name.clone()) {
            Some(symbol_id) => {
                match self.resolve_type(
                    local_scope_opt.clone(),
                    module_id,
                    typedef.type_specifier.clone(),
                    typedef.loc.clone(),
                    typedef.span.end,
                ) {
                    Some(concrete_type) => {
                        let resolved_typedef = ResolvedTypedef {
                            module_id,
                            symbol_id,
                            typedef_sig: TypedefSig {
                                name: typedef.identifier.name.clone(),
                                ty: concrete_type.clone(),
                                vis: typedef.vis.clone(),
                                loc: typedef.loc.clone(),
                            },
                        };

                        if let Some(local_scope_rc) = &local_scope_opt {
                            let mut local_scope = local_scope_rc.borrow_mut();
                            local_scope.insert(
                                typedef.identifier.name.clone(),
                                LocalSymbol::new(LocalSymbolKind::Typedef(resolved_typedef)),
                            );
                            drop(local_scope);
                        } else {
                            self.insert_symbol_entry(
                                module_id,
                                symbol_id.clone(),
                                SymbolEntry::new(SymbolEntryKind::Typedef(resolved_typedef)),
                            );
                        }

                        Some(TypedStatement::Typedef(TypedTypedef {
                            name: typedef.identifier.name.clone(),
                            ty: concrete_type,
                            vis: typedef.vis.clone(),
                            loc: typedef.loc.clone(),
                        }))
                    }
                    None => None,
                }
            }
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::SymbolNotFound {
                        name: typedef.identifier.name.clone(),
                    },
                    location: Some(DiagLoc::new(
                        self.get_current_module_file_path(),
                        typedef.loc.clone(),
                        typedef.span.end,
                    )),
                    hint: None,
                });
                None
            }
        }
    }

    fn declare_local_variable(
        &mut self,
        module_id: ModuleID,
        local_scope_rc: LocalScopeRef,
        variable: &Variable,
    ) -> Option<TypedVariable> {
        let local_scope = local_scope_rc.borrow();
        if local_scope.resolve(&variable.identifier.name.clone()).is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: ResolverDiagKind::DuplicateSymbolInThisScope {
                    symbol_name: variable.identifier.name.clone(),
                },
                location: Some(DiagLoc::new(
                    self.get_current_module_file_path(),
                    variable.loc.clone(),
                    variable.span.end,
                )),
                hint: None,
            });
        }
        drop(local_scope);

        let var_type = {
            if let Some(var_type_specifier) = &variable.ty {
                match self.resolve_type(
                    Some(local_scope_rc.clone()),
                    module_id,
                    var_type_specifier.clone(),
                    variable.loc.clone(),
                    variable.span.end,
                ) {
                    Some(concrete_type) => Some(concrete_type),
                    None => return None,
                }
            } else {
                None
            }
        };

        let typed_rhs = {
            if let Some(rhs) = &variable.rhs {
                match self.resolve_expr(module_id, Some(local_scope_rc.clone()), rhs) {
                    Some(typed_expr) => Some(typed_expr),
                    None => {
                        return None;
                    }
                }
            } else {
                None
            }
        };

        let symbol_id = generate_symbol_id();

        let typed_variable = TypedVariable {
            symbol_id,
            name: variable.identifier.name.clone(),
            ty: var_type,
            rhs: typed_rhs,
            loc: variable.loc.clone(),
        };

        let resolved_var = ResolvedVariable {
            module_id,
            symbol_id,
            typed_variable: typed_variable.clone(),
        };

        let mut scope_borrowed = local_scope_rc.borrow_mut();
        scope_borrowed.insert(
            variable.identifier.name.clone(),
            LocalSymbol::new(LocalSymbolKind::Variable(resolved_var)),
        );
        drop(scope_borrowed);

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
            loc: if_stmt.loc.clone(),
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
                        loc: return_stmt.loc.clone(),
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
                        loc: for_stmt.loc.clone(),
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
                        loc: while_stmt.loc.clone(),
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
                                            loc: identifier.loc.clone(),
                                        },
                                    })),
                                );
                                drop(case_scope);

                                TypedSwitchCasePattern::Identifier(identifier.name.clone(), identifier.loc.clone())
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
                                                        loc: identifier.loc.clone(),
                                                    },
                                                })),
                                            );
                                            drop(case_scope);
                                            identifier.clone()
                                        })
                                        .collect(),
                                    identifier.loc.clone(),
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
                            loc: case.loc.clone(),
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
                        loc: switch.loc.clone(),
                    }));
                }
                Statement::Enum(enum_decl) => {
                    match self.resolve_enum(module_id, Some(Rc::clone(&local_scope)), enum_decl) {
                        Some(typed_stmt) => {
                            typed_body.push(typed_stmt);
                        }
                        None => continue,
                    }
                }
                Statement::Union(union_decl) => {
                    match self.resolve_union(module_id, Some(Rc::clone(&local_scope)), union_decl) {
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
                    match self.resolve_struct(module_id, Some(local_scope.clone()), struct_decl) {
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
                        loc: break_stmt.loc.clone(),
                    }));
                }
                Statement::Continue(continue_stmt) => {
                    typed_body.push(TypedStatement::Continue(TypedContinue {
                        loc: continue_stmt.loc.clone(),
                    }));
                }
                Statement::Typedef(typedef) => {
                    match self.resolve_typedef(module_id, Some(local_scope.clone()), &typedef) {
                        Some(typed_stmt) => typed_body.push(typed_stmt),
                        None => continue,
                    }
                }
                // Invalid statements.
                Statement::GlobalVariable(..) => unreachable!(),
                Statement::FuncDef(..) => unreachable!(),
                Statement::FuncDecl(..) => unreachable!(),
                Statement::Import(..) => unreachable!(),
            }
        }

        Some(TypedBlockStatement {
            scope_id,
            exprs: typed_body,
            loc: block_statement.loc.clone(),
        })
    }

    fn resolve_identifier(
        &mut self,
        module_id: ModuleID,
        identifier: Identifier,
        loc: Location,
        span_end: usize,
    ) -> Option<u32> {
        match self.lookup_symbol_id(module_id, &identifier.name) {
            Some(symbol_id) => Some(symbol_id),
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::SymbolNotFound {
                        name: identifier.name.clone(),
                    },
                    location: Some(DiagLoc::new(self.get_current_module_file_path(), loc.clone(), span_end)),
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
                        location: Some(DiagLoc::new(
                            self.get_current_module_file_path(),
                            module_import.loc.clone(),
                            module_import.span.end,
                        )),
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
        macro_rules! is_unscoped_expr {
            ($loc:expr, $span_end:expr) => {{
                if local_scope_opt.is_none() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::RequiresLocalScope,
                        location: Some(DiagLoc::new(self.get_current_module_file_path(), $loc, $span_end)),
                        hint: None,
                    });
                }

                local_scope_opt.is_none()
            }};
        }

        macro_rules! resolve_local_identifier {
            ($identifier:expr) => {{
                let local_option = {
                    if let Some(local_scope_rc) = &local_scope_opt {
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
                        match self.resolve_module_import(
                            module_id,
                            ModuleImport {
                                segments: vec![ModuleSegment::SubModule($identifier.clone())],
                                loc: $identifier.loc.clone(),
                                span: $identifier.span.clone(),
                            },
                        ) {
                            Some(symbol_id) => symbol_id,
                            None => {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: ResolverDiagKind::SymbolNotFound {
                                        name: $identifier.name.clone(),
                                    },
                                    location: Some(DiagLoc::new(
                                        self.get_current_module_file_path(),
                                        $identifier.loc.clone(),
                                        $identifier.span.end,
                                    )),
                                    hint: None,
                                });

                                return None;
                            }
                        }
                    }
                }
            }};
        }

        match expr {
            Expression::FieldAccess(field_access) => {
                let operand = match self.resolve_expr(module_id, local_scope_opt.clone(), &field_access.operand) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                Some(TypedExpression {
                    kind: TypedExpressionKind::FieldAccess(TypedFieldAccess {
                        operand: Box::new(operand),
                        field_name: field_access.field_name.name.clone(),
                        is_fat_arrow: field_access.is_fat_arrow,
                        field_index: None,
                        field_ty: None,
                        object_symbol_id: None,
                        loc: field_access.loc.clone(),
                    }),
                    value_category: ValueCategory::Lvalue,
                    concrete_type: None,
                    loc: field_access.loc.clone(),
                })
            }
            Expression::MethodCall(method_call) => {
                let operand = match self.resolve_expr(module_id, local_scope_opt.clone(), &method_call.operand) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                let symbol_id = match operand.kind {
                    TypedExpressionKind::Symbol(symbol_id, ..) => symbol_id,
                    _ => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: ResolverDiagKind::InvalidOperandForMethodCall,
                            location: Some(DiagLoc::new(
                                self.get_current_module_file_path(),
                                method_call.loc.clone(),
                                method_call.span.end,
                            )),
                            hint: None,
                        });
                        return None;
                    }
                };

                let mut args: Vec<TypedExpression> = Vec::new();

                for arg in &method_call.args {
                    let typed_expr = match self.resolve_expr(module_id, local_scope_opt.clone(), &arg) {
                        Some(typed_expr) => typed_expr,
                        None => continue,
                    };

                    args.push(typed_expr);
                }

                Some(TypedExpression {
                    kind: TypedExpressionKind::MethodCall(TypedMethodCall {
                        symbol_id,
                        operand: Box::new(operand),
                        method_name: method_call.method_name.name.clone(),
                        is_fat_arrow: method_call.is_fat_arrow,
                        loc: method_call.loc.clone(),
                        args,
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: method_call.loc.clone(),
                })
            }
            Expression::StructInit(struct_init) => {
                let symbol_id = match self.resolve_local_module_import(
                    local_scope_opt.clone(),
                    module_id,
                    &struct_init.struct_name.clone(),
                ) {
                    Some(symbol_id) => symbol_id,
                    None => {
                        return None;
                    }
                };

                let mut field_inits: Vec<TypedStructFieldInit> = Vec::new();

                for field_init in &struct_init.field_inits {
                    let value = match self.resolve_expr(module_id, local_scope_opt.clone(), &field_init.value) {
                        Some(typed_expr) => typed_expr,
                        None => continue,
                    };

                    field_inits.push(TypedStructFieldInit {
                        name: field_init.identifier.name.clone(),
                        value,
                        loc: field_init.loc.clone(),
                    });
                }

                Some(TypedExpression {
                    kind: TypedExpressionKind::StructInit(TypedStructInit {
                        symbol_id,
                        fields: field_inits,
                        loc: struct_init.loc.clone(),
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: struct_init.loc.clone(),
                })
            }
            Expression::ModuleImport(module_import) => {
                if let Some(identifier) = module_import.as_identifier() {
                    let symbol_id = resolve_local_identifier!(identifier);
                    Some(TypedExpression {
                        kind: TypedExpressionKind::Symbol(symbol_id, identifier.loc.clone()),
                        concrete_type: None,
                        value_category: ValueCategory::Lvalue,
                        loc: module_import.loc.clone(),
                    })
                } else {
                    match self.resolve_module_import(module_id, module_import.clone()) {
                        Some(symbol_id) => Some(TypedExpression {
                            kind: TypedExpressionKind::Symbol(symbol_id, module_import.loc.clone()),
                            concrete_type: None,
                            value_category: ValueCategory::Lvalue,
                            loc: module_import.loc.clone(),
                        }),
                        None => return None,
                    }
                }
            }
            Expression::Identifier(identifier) => {
                let symbol_id = resolve_local_identifier!(identifier);
                Some(TypedExpression {
                    kind: TypedExpressionKind::Symbol(symbol_id, identifier.loc.clone()),
                    concrete_type: None,
                    value_category: ValueCategory::Lvalue,
                    loc: identifier.loc.clone(),
                })
            }
            Expression::FuncCall(func_call) => {
                if is_unscoped_expr!(func_call.loc.clone(), func_call.span.end) {
                    return None;
                }

                let symbol_id = match &*func_call.operand.clone() {
                    Expression::Identifier(identifier) => match self.resolve_identifier(
                        module_id,
                        identifier.clone(),
                        func_call.loc.clone(),
                        func_call.span.end,
                    ) {
                        Some(resolved) => resolved,
                        None => return None,
                    },
                    Expression::ModuleImport(module_import) => {
                        match self.resolve_module_import(module_id, module_import.clone()) {
                            Some(resolved) => resolved,
                            None => return None,
                        }
                    }
                    _ => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: ResolverDiagKind::InvalidOperandForFuncCall,
                            location: Some(DiagLoc::new(
                                self.get_current_module_file_path(),
                                func_call.loc.clone(),
                                func_call.span.end,
                            )),
                            hint: None,
                        });
                        return None;
                    }
                };

                let mut typed_args: Vec<TypedExpression> = Vec::new();

                for arg in &func_call.args {
                    match self.resolve_expr(module_id, local_scope_opt.clone(), &arg.clone()) {
                        Some(typed_expr) => {
                            typed_args.push(typed_expr);
                        }
                        None => continue,
                    }
                }

                Some(TypedExpression {
                    kind: TypedExpressionKind::FuncCall(TypedFuncCall {
                        symbol_id,
                        args: typed_args,
                        loc: func_call.loc.clone(),
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: func_call.loc.clone(),
                })
            }
            Expression::Array(arr) => {
                let array_type = match self.resolve_type(
                    local_scope_opt.clone(),
                    module_id,
                    arr.data_type.clone(),
                    arr.loc.clone(),
                    arr.span.end,
                ) {
                    Some(concrete_type) => concrete_type,
                    None => return None,
                };

                let mut typed_elements: Vec<TypedExpression> = Vec::new();

                for item in &arr.elements {
                    match self.resolve_expr(module_id, local_scope_opt.clone(), &item.clone()) {
                        Some(typed_expr) => typed_elements.push(typed_expr),
                        None => continue,
                    };
                }

                Some(TypedExpression {
                    kind: TypedExpressionKind::Array(TypedArray {
                        array_type,
                        elements: typed_elements,
                        loc: arr.loc.clone(),
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: arr.loc.clone(),
                })
            }
            Expression::Infix(bin) => {
                let lhs = self.resolve_expr(module_id, local_scope_opt.clone(), &*bin.lhs.clone())?;
                let rhs = self.resolve_expr(module_id, local_scope_opt.clone(), &*bin.rhs.clone())?;

                Some(TypedExpression {
                    kind: TypedExpressionKind::Infix(TypedInfixExpression {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op: bin.op.clone(),
                        loc: bin.loc.clone(),
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: bin.loc.clone(),
                })
            }
            Expression::Prefix(prefix) => {
                let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &*prefix.operand.clone())?;

                Some(TypedExpression {
                    kind: TypedExpressionKind::Prefix(TypedPrefixExpression {
                        operand: Box::new(operand),
                        op: prefix.op.clone(),
                        loc: prefix.loc.clone(),
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: prefix.loc.clone(),
                })
            }
            Expression::Cast(cast) => {
                let operand = match self.resolve_expr(module_id, local_scope_opt.clone(), &*cast.expr.clone()) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                let target_type = match self.resolve_type(
                    local_scope_opt,
                    module_id,
                    cast.target_type.clone(),
                    cast.loc.clone(),
                    cast.span.end,
                ) {
                    Some(concrete_type) => concrete_type,
                    None => return None,
                };

                Some(TypedExpression {
                    kind: TypedExpressionKind::Cast(TypedCast {
                        operand: Box::new(operand),
                        target_type,
                        loc: cast.loc.clone(),
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: cast.loc.clone(),
                })
            }
            Expression::TypeSpecifier(type_specifier) => {
                let (loc, span_end) = type_specifier.get_loc();

                let symbol_id = match type_specifier {
                    TypeSpecifier::Identifier(identifier) => {
                        resolve_local_identifier!(identifier)
                    }
                    TypeSpecifier::ModuleImport(module_import) => {
                        self.resolve_module_import(module_id, module_import.clone())?
                    }
                    _ => match self.resolve_type(
                        local_scope_opt,
                        module_id,
                        type_specifier.clone(),
                        loc.clone(),
                        span_end,
                    ) {
                        Some(concrete_type) => {
                            return Some(TypedExpression {
                                kind: TypedExpressionKind::ConcreteType(concrete_type.clone()),
                                value_category: ValueCategory::Rvalue,
                                concrete_type: Some(concrete_type),
                                loc,
                            });
                        }
                        None => return None,
                    },
                };

                Some(TypedExpression {
                    kind: TypedExpressionKind::Symbol(symbol_id, type_specifier.get_loc().0.clone()),
                    value_category: ValueCategory::Lvalue,
                    concrete_type: None,
                    loc,
                })
            }
            Expression::Assignment(assignment) => {
                let lhs = match self.resolve_expr(module_id, local_scope_opt.clone(), &assignment.lhs) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                let rhs = match self.resolve_expr(module_id, local_scope_opt.clone(), &assignment.rhs) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                Some(TypedExpression {
                    kind: TypedExpressionKind::Assignment(TypedAssignment {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        kind: assignment.kind.clone(),
                        loc: assignment.loc.clone(),
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: assignment.loc.clone(),
                })
            }
            Expression::Literal(literal) => {
                let literal_type: Option<ConcreteType> = {
                    match &literal.kind {
                        LiteralKind::Integer(_, suffix_opt) => {
                            if let Some(token_kind) = suffix_opt {
                                match ConcreteType::try_from(*token_kind.clone()) {
                                    Ok(concrete_type) => Some(concrete_type),
                                    Err(_) => {
                                        self.reporter.report(Diag {
                                            level: DiagLevel::Error,
                                            kind: ResolverDiagKind::InvalidLiteralSuffix,
                                            location: Some(DiagLoc::new(
                                                self.get_current_module_file_path(),
                                                literal.loc.clone(),
                                                literal.span.end,
                                            )),
                                            hint: None,
                                        });
                                        return None;
                                    }
                                }
                            } else {
                                None
                            }
                        }
                        LiteralKind::Float(_, suffix_opt) => {
                            if let Some(token_kind) = suffix_opt {
                                match ConcreteType::try_from(*token_kind.clone()) {
                                    Ok(concrete_type) => Some(concrete_type),
                                    Err(_) => {
                                        self.reporter.report(Diag {
                                            level: DiagLevel::Error,
                                            kind: ResolverDiagKind::InvalidLiteralSuffix,
                                            location: Some(DiagLoc::new(
                                                self.get_current_module_file_path(),
                                                literal.loc.clone(),
                                                literal.span.end,
                                            )),
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
                                            capacity: TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(
                                                len,
                                            )),
                                            loc: literal.loc.clone(),
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
                    }
                };
                let typed_literal = TypedLiteral {
                    ty: literal_type,
                    kind: literal.kind.clone(),
                    loc: literal.loc.clone(),
                };

                Some(TypedExpression {
                    kind: TypedExpressionKind::Literal(typed_literal.clone()),
                    concrete_type: None,
                    value_category: ValueCategory::Rvalue,
                    loc: typed_literal.loc.clone(),
                })
            }
            Expression::Unary(unary) => {
                let operand = match self.resolve_expr(module_id, local_scope_opt.clone(), &*unary.operand) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                Some(TypedExpression {
                    kind: TypedExpressionKind::Unary(TypedUnaryExpression {
                        op: unary.op.clone(),
                        operand: Box::new(operand),
                        loc: unary.loc.clone(),
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: unary.loc.clone(),
                })
            }
            Expression::ArrayIndex(array_index) => {
                let operand = match self.resolve_expr(module_id, local_scope_opt.clone(), &array_index.operand) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                let index = match self.resolve_expr(module_id, local_scope_opt.clone(), &array_index.index) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                Some(TypedExpression {
                    kind: TypedExpressionKind::ArrayIndex(TypedArrayIndex {
                        operand: Box::new(operand),
                        index: Box::new(index),
                        loc: array_index.loc.clone(),
                    }),
                    value_category: ValueCategory::Lvalue,
                    concrete_type: None,
                    loc: array_index.loc.clone(),
                })
            }
            Expression::AddressOf(address_of) => {
                let operand = match self.resolve_expr(module_id, local_scope_opt.clone(), &address_of.expr) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                Some(TypedExpression {
                    kind: TypedExpressionKind::AddressOf(TypedAddressOf {
                        operand: Box::new(operand),
                        loc: address_of.loc.clone(),
                    }),
                    value_category: ValueCategory::Lvalue,
                    concrete_type: None,
                    loc: address_of.loc.clone(),
                })
            }
            Expression::Dereference(dereference) => {
                let operand = match self.resolve_expr(module_id, local_scope_opt.clone(), &dereference.expr) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                Some(TypedExpression {
                    kind: TypedExpressionKind::Dereference(TypedDereference {
                        operand: Box::new(operand),
                        loc: dereference.loc.clone(),
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: dereference.loc.clone(),
                })
            }
            Expression::UnnamedStructValue(unnamed_struct_value) => {
                let mut fields: Vec<TypedUnnamedStructValueField> = Vec::new();

                for field in &unnamed_struct_value.fields {
                    let field_type = {
                        if let Some(type_specifier) = &field.field_type {
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
                        }
                    };

                    let field_value = match self.resolve_expr(module_id, local_scope_opt.clone(), &*&field.field_value)
                    {
                        Some(typed_expr) => typed_expr,
                        None => continue,
                    };

                    fields.push(TypedUnnamedStructValueField {
                        field_name: field.field_name.name.clone(),
                        field_type,
                        field_value: Box::new(field_value),
                        loc: field.loc.clone(),
                    });
                }

                Some(TypedExpression {
                    kind: TypedExpressionKind::UnnamedStructValue(TypedUnnamedStructValue {
                        fields,
                        unnamed_struct_type: None,
                        packed: unnamed_struct_value.packed,
                        is_const: unnamed_struct_value.is_const,
                        loc: unnamed_struct_value.loc.clone(),
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: unnamed_struct_value.loc.clone(),
                })
            }
            Expression::SizeOfExpression(size_of_expression) => {
                let typed_expr = match self.resolve_expr(module_id, local_scope_opt, &size_of_expression.expr) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                Some(TypedExpression {
                    kind: TypedExpressionKind::SizeOfExpression(TypedSizeOfExpression {
                        expr: Box::new(typed_expr),
                        loc: size_of_expression.loc.clone(),
                    }),
                    value_category: ValueCategory::Rvalue,
                    concrete_type: None,
                    loc: size_of_expression.loc.clone(),
                })
            }
        }
    }

    fn duplicate_symbol(&mut self, module_id: ModuleID, symbol_name: String, loc: Location, span_end: usize) -> bool {
        match self.lookup_symbol_id(module_id, &symbol_name) {
            Some(symbol_id) => {
                let previous_decl = match self.get_symbol_loc(module_id, symbol_id) {
                    Some(previous_decl) => previous_decl,
                    None => return false,
                };

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::DuplicateSymbol { symbol_name },
                    location: Some(DiagLoc::new(self.get_current_module_file_path(), loc.clone(), span_end)),
                    hint: Some(format!(
                        "Previous declaration: {}:{}:{}.",
                        previous_decl.0, previous_decl.1.line, previous_decl.1.column
                    )),
                });

                true
            }
            None => false,
        }
    }

    pub fn lookup_symbol_id_in_modules(&self, symbol_id: SymbolID) -> Option<ModuleID> {
        let global_symbols = self.global_symbols.lock().unwrap();
        for (module_id, symbol_table) in global_symbols.iter() {
            match symbol_table
                .names
                .iter()
                .find(|(_, table_symbol_id)| **table_symbol_id == symbol_id)
            {
                Some(_) => return Some(*module_id),
                None => continue,
            }
        }
        drop(global_symbols);
        None
    }

    pub fn lookup_symbol_id(&self, module_id: ModuleID, name: &str) -> Option<SymbolID> {
        let global_symbols = self.global_symbols.lock().unwrap();
        let option = match global_symbols.get(&module_id) {
            Some(symbol_table) => match symbol_table.names.get(name) {
                Some(symbol_id) => Some(*symbol_id),
                None => None,
            },
            None => None,
        };
        drop(global_symbols);
        option
    }

    pub fn lookup_symbol(&self, module_id: ModuleID, name: &str) -> Option<SymbolEntry> {
        let global_symbols = self.global_symbols.lock().unwrap();
        let option = match global_symbols.get(&module_id) {
            Some(symbol_table) => match symbol_table.names.get(name) {
                Some(symbol_id) => match symbol_table.entries.get(symbol_id) {
                    Some(symbol_entry) => Some(symbol_entry.clone()),
                    None => None,
                },
                None => None,
            },
            None => None,
        };
        drop(global_symbols);
        option
    }

    pub fn resolve_global_symbol(&self, symbol_id: SymbolID) -> Option<SymbolEntry> {
        let module_id = self.lookup_symbol_id_in_modules(symbol_id)?;

        match self.lookup_symbol_entry_with_id(module_id, symbol_id) {
            Some(global_symbol) => Some(global_symbol),
            None => None,
        }
    }

    pub fn resolve_symbol_from_local_scope(
        &self,
        local_scope_rc: LocalScopeRef,
        symbol_id: SymbolID,
    ) -> Option<LocalSymbol> {
        let local_scope = local_scope_rc.borrow();
        let local_option = match local_scope
            .symbols
            .values()
            .find(|symbol| symbol.get_symbol_id() == symbol_id)
            .cloned()
        {
            Some(local_symbol) => Some(local_symbol),
            None => None,
        };
        drop(local_scope);
        local_option
    }

    pub fn resolve_local_or_global_symbol(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<LocalOrGlobalSymbol> {
        let option = {
            if let Some(local_scope_rc) = local_scope_opt {
                let local_scope = local_scope_rc.borrow();
                let local_option = match local_scope
                    .symbols
                    .values()
                    .find(|symbol| symbol.get_symbol_id() == symbol_id)
                    .cloned()
                {
                    Some(local_symbol) => Some(LocalOrGlobalSymbol::LocalSymbol(local_symbol)),
                    None => None,
                };
                drop(local_scope);
                local_option
            } else {
                None
            }
        };

        match option {
            Some(local_or_global_symbol) => Some(local_or_global_symbol),
            None => {
                let module_id = self.lookup_symbol_id_in_modules(symbol_id)?;

                match self.lookup_symbol_entry_with_id(module_id, symbol_id) {
                    Some(global_symbol) => Some(LocalOrGlobalSymbol::GlobalSymbol(global_symbol)),
                    None => None,
                }
            }
        }
    }

    pub fn lookup_symbol_entry_with_id(&self, module_id: ModuleID, symbol_id: SymbolID) -> Option<SymbolEntry> {
        let global_symbols = self.global_symbols.lock().unwrap();
        let option = match global_symbols.get(&module_id) {
            Some(symbol_table) => match symbol_table.entries.get(&symbol_id) {
                Some(symbol_entry) => Some(symbol_entry.clone()),
                None => None,
            },
            None => None,
        };
        drop(global_symbols);
        option
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

    fn get_module_alias(&self, group_name: ModuleGroupName) -> Option<ModuleID> {
        let module_aliases = self.module_aliases.lock().unwrap();
        let imported_modules = module_aliases.get(&self.current_module.unwrap()).unwrap();
        let option = imported_modules.get(&group_name).cloned();
        drop(module_aliases);
        option
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

    fn get_module_id_by_file_path(&self, module_file_path: ModuleFilePath) -> Option<ModuleID> {
        let file_paths = self.file_paths.lock().unwrap();
        let module_id_opt = match file_paths.iter().find(|(_, fp)| **fp == module_file_path) {
            Some((module_id, _)) => Some(*module_id),
            None => None,
        };
        drop(file_paths);
        module_id_opt
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

    pub fn get_scope_ref(&self, module_id: ModuleID, scope_id: ScopeID) -> Option<LocalScopeRef> {
        let global_symbols = self.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get(&module_id).unwrap();
        let option = symbol_table.scopes.get(&scope_id).cloned();
        drop(global_symbols);
        option
    }
}

fn get_module_name(module_file_path: String) -> String {
    let path = Path::new(&module_file_path);
    let file_stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("module");

    // Hash the full path for uniqueness
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
            file_paths: HashSet::new(),
        }
    }

    pub fn contains(&self, file_path: ModuleFilePath) -> bool {
        self.file_paths.contains(&file_path)
    }

    pub fn insert(&mut self, file_path: ModuleFilePath) {
        self.file_paths.insert(file_path);
    }

    pub fn remove(&mut self, file_path: ModuleFilePath) {
        self.file_paths.remove(&file_path);
    }
}
