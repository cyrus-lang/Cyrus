use crate::moduleloader::{ModuleAlias, ModuleFilePath, ModuleLoader, ModuleLoaderOptions};
use crate::scope::*;
use crate::{
    declsign::{FuncSig, TypedefSig},
    diagnostics::ResolverDiagKind,
};
use ast::format::module_segments_as_string;
use ast::{
    ArrayCapacity, BlockStatement, Enum, EnumVariant, Expression, FuncDecl, FuncDef, FuncParamKind, FuncVariadicParams,
    Identifier, LiteralKind, ProgramTree, Statement, Struct, TypeSpecifier, Typedef, Variable,
    token::{Location, Span, Token, TokenKind},
};
use ast::{Import, ModuleImport};
use diagcentral::{reporter::DiagReporter, *};
use rand::Rng;
use std::collections::HashSet;
use std::sync::{Arc, Mutex};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use typed_ast::types::{
    ConcreteType, TypedArrayCapacity, TypedArrayType, TypedUnnamedStructType, TypedUnnamedStructTypeField,
};
use typed_ast::{SymbolID, *};

pub mod declsign;
mod diagnostics;
pub mod moduleloader;
pub mod scope;

pub struct Resolver {
    pub global_symbols: Arc<Mutex<HashMap<ModuleID, SymbolTable>>>,
    pub module_aliases: Arc<Mutex<HashMap<ModuleAlias, ModuleID>>>,
    pub analyzed_modules: Arc<Mutex<HashSet<ModuleID>>>,
    pub file_paths: Arc<Mutex<HashMap<ModuleID, ModuleFilePath>>>,
    pub reporter: DiagReporter<ResolverDiagKind>,
    pub module_loader: ModuleLoader,
    pub master_module_file_path: String,
    current_module: Option<ModuleID>,
}

pub struct Visiting {
    pub file_paths: HashSet<ModuleFilePath>,
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

impl Resolver {
    pub fn new(opts: ModuleLoaderOptions, master_module_file_path: String) -> Self {
        Self {
            global_symbols: Arc::new(Mutex::new(HashMap::new())),
            analyzed_modules: Arc::new(Mutex::new(HashSet::new())),
            module_aliases: Arc::new(Mutex::new(HashMap::new())),
            file_paths: Arc::new(Mutex::new(HashMap::new())),
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

    fn resolve_import(&mut self, parent_module_id: ModuleID, import: Import, mut visiting: &mut Visiting) {
        let loaded_modules_list = self
            .module_loader
            .load_module(import.clone(), self.get_current_module_file_path());

        let report_if_imports_twice = |this: &mut Resolver, module_file_path: String| -> bool {
            let file_paths = this.file_paths.lock().unwrap();
            let imported_before = file_paths
                .values()
                .into_iter()
                .find(|file_path| **file_path == module_file_path)
                .is_some();
            drop(file_paths);

            if imported_before { true } else { false }
        };

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

            let module_id = generate_module_id();

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
            } else if report_if_imports_twice(self, module_file_path.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::ImportTwice {
                        module_name: module_alias,
                    },
                    location: Some(DiagLoc::new(
                        self.get_current_module_file_path(),
                        import.loc.clone(),
                        import.span.end,
                    )),
                    hint: Some("Consider to remove previous declaration.".to_string()),
                });

                continue;
            }

            visiting.insert(module_file_path.clone());
            self.insert_module_file_path(module_id, module_file_path);

            match self.resolve_module(module_id, program_tree.as_ref(), &mut visiting, false) {
                Some(..) => {}
                None => continue,
            };

            self.insert_module_alias(module_alias, module_id);
        }
    }

    pub fn resolve_module(
        &mut self,
        module_id: ModuleID,
        ast: &ProgramTree,
        mut visiting: &mut Visiting,
        is_master: bool,
    ) -> Option<TypedProgramTree> {
        self.current_module = Some(module_id);

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
        Some(TypedProgramTree { body: typed_body })
    }

    fn resolve_type(
        &mut self,
        module_id: ModuleID,
        type_specifier: TypeSpecifier,
        loc: Location,
        span_end: usize,
    ) -> Option<ConcreteType> {
        let resolving_result = match type_specifier {
            TypeSpecifier::TypeToken(token) => Ok(ConcreteType::from(token.kind.clone())),
            TypeSpecifier::Const(type_specifier) => Ok(ConcreteType::Const(Box::new(self.resolve_type(
                module_id,
                *type_specifier,
                loc.clone(),
                span_end,
            )?))),
            TypeSpecifier::Dereference(type_specifier) => Ok(ConcreteType::Pointer(Box::new(self.resolve_type(
                module_id,
                *type_specifier,
                loc.clone(),
                span_end,
            )?))),
            TypeSpecifier::Array(array_type_specifier) => Ok({
                let element_type =
                    match self.resolve_type(module_id, *array_type_specifier.element_type, loc.clone(), span_end) {
                        Some(concrete_type) => concrete_type,
                        None => return None,
                    };

                let capacity = match &array_type_specifier.size {
                    ArrayCapacity::Fixed(token_kind) => {
                        let fixed_capacity = match match token_kind {
                            TokenKind::Literal(literal) => match &literal.kind {
                                LiteralKind::Integer(integer_literal) => Some(integer_literal),
                                _ => None,
                            },
                            _ => None,
                        } {
                            Some(const_int) => const_int,
                            None => {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: ResolverDiagKind::InvalidArrayCapacity,
                                    location: Some(DiagLoc::new(
                                        self.get_current_module_file_path(),
                                        loc.clone(),
                                        span_end,
                                    )),
                                    hint: None,
                                });
                                return None;
                            }
                        };

                        TypedArrayCapacity::Fixed((*fixed_capacity).try_into().unwrap())
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
                    match self.resolve_type(module_id, field.field_type.clone(), field.loc.clone(), field.span.end) {
                        Some(concrete_type) => {
                            fields.push(TypedUnnamedStructTypeField {
                                field_name: field.field_name.clone(),
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
                Some(symbol_id) => {
                    todo!();
                }
                None => return None,
            },
            TypeSpecifier::Identifier(identifier) => {
                match match self.lookup_symbol_id(module_id, &identifier.name.clone()) {
                    Some(symbol_entry) => Some(symbol_entry),
                    None => None,
                } {
                    Some(symbol_id) => Ok(ConcreteType::Symbol(symbol_id)),
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

    fn insert_symbol_name(&mut self, module_id: ModuleID, name: &String, loc: Location, span_end: usize) {
        let symbol_id = generate_symbol_id();
        let mut global_symbols = self.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get_mut(&module_id).unwrap();
        symbol_table.names.insert(name.clone(), symbol_id);
        let module_file_path = self.get_module_file_path(module_id).unwrap();
        symbol_table.locs.insert(symbol_id, (module_file_path, loc, span_end));
        drop(global_symbols);
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
                        &func_decl.identifier.name.clone(),
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
                // Valid top-level statements.
                Statement::Import(_) => continue,
                Statement::GlobalVariable(global_variable) => todo!(),
                Statement::Typedef(typedef) => match self.resolve_typedef(module_id, typedef) {
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
                Statement::Struct(struct_decl) => match self.resolve_struct(module_id, struct_decl) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Enum(enum_decl) => match self.resolve_enum(module_id, None, enum_decl) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },

                // Invalid top-level statements.
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

    fn resolve_enum(
        &mut self,
        module_id: ModuleID,
        local_scope_opt: Option<LocalScopeRef>,
        enum_decl: &Enum,
    ) -> Option<TypedStatement> {
        let mut variants: Vec<TypedEnumVariant> = Vec::new();

        for variant in &enum_decl.variants {
            let typed_variant = match variant {
                EnumVariant::Identifier(identifier) => TypedEnumVariant::Identifier(identifier.name.clone()),
                EnumVariant::Variant(identifier, enum_valued_fields) => {
                    let mut fields: Vec<TypedEnumValuedField> = Vec::new();
                    for valued_field in enum_valued_fields {
                        let field_type = match self.resolve_type(
                            module_id,
                            valued_field.field_type.clone(),
                            valued_field.identifier.loc.clone(),
                            valued_field.identifier.span.end,
                        ) {
                            Some(concrete_type) => concrete_type,
                            None => continue,
                        };

                        fields.push(TypedEnumValuedField {
                            name: valued_field.identifier.name.clone(),
                            field_type,
                        });
                    }
                    TypedEnumVariant::Variant(identifier.name.clone(), fields)
                }
                EnumVariant::Valued(identifier, expr) => match self.resolve_expr(
                    module_id,
                    match &local_scope_opt {
                        Some(local_scope) => Some(Rc::clone(&local_scope)),
                        None => None,
                    },
                    expr,
                ) {
                    Some(typed_expr) => TypedEnumVariant::Valued(identifier.name.clone(), Box::new(typed_expr)),
                    None => continue,
                },
            };

            variants.push(typed_variant);
        }

        Some(TypedStatement::Enum(TypedEnum {
            name: enum_decl.identifier.name.clone(),
            variants,
            vis: enum_decl.vis.clone(),
            loc: enum_decl.identifier.loc.clone(),
        }))
    }

    fn resolve_struct(&mut self, module_id: ModuleID, struct_decl: &Struct) -> Option<TypedStatement> {
        let mut typed_struct_fields: Vec<TypedStructField> = Vec::new();
        let mut typed_methods: Vec<TypedFuncDef> = Vec::new();

        for field in &struct_decl.fields {
            match self.resolve_type(module_id, field.ty.clone(), field.loc.clone(), field.span.end) {
                Some(concrete_type) => {
                    typed_struct_fields.push(TypedStructField {
                        name: field.identifier.name.clone(),
                        ty: concrete_type,
                        loc: field.loc.clone(),
                    });
                }
                None => continue,
            }
        }

        for func_def in &struct_decl.methods {
            let local_scope: LocalScopeRef = Rc::new(RefCell::new(LocalScope::new(None)));

            let typed_func_body = match self.resolve_block_statement(Rc::clone(&local_scope), &func_def.body) {
                Some(typed_block_statement) => typed_block_statement,
                None => return None,
            };

            match self.resolve_func(module_id, &func_def.as_func_decl()) {
                Some((return_type, typed_func_params, typed_variadic_param)) => {
                    typed_methods.push(TypedFuncDef {
                        name: func_def.identifier.name.clone(),
                        params: TypedFuncParams {
                            list: typed_func_params,
                            variadic: typed_variadic_param,
                        },
                        return_type,
                        vis: func_def.vis.clone(),
                        loc: func_def.loc.clone(),
                        body: Box::new(typed_func_body),
                    });
                }
                None => continue,
            }
        }

        Some(TypedStatement::Struct(TypedStruct {
            name: struct_decl.identifier.name.clone(),
            fields: typed_struct_fields,
            methods: Vec::new(), // FIXME
            vis: struct_decl.vis.clone(),
            packed: struct_decl.packed.clone(),
            loc: struct_decl.loc.clone(),
        }))
    }

    fn resolve_func(
        &mut self,
        module_id: ModuleID,
        func_decl: &FuncDecl,
    ) -> Option<(ConcreteType, Vec<TypedFuncParamKind>, Option<TypedFuncVariadicParams>)> {
        let return_type_specifier = func_decl.return_type.clone().unwrap_or(TypeSpecifier::TypeToken(Token {
            kind: TokenKind::Void,
            span: Span::default(),
            loc: Location::default(),
        }));

        let return_type = match self.resolve_type(
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

                    typed_func_params.push(TypedFuncParamKind::FuncParam(TypedFuncParam {
                        name: func_param.identifier.name.clone(),
                        ty: param_type,
                        loc: func_param.loc.clone(),
                    }));
                }
                FuncParamKind::SelfModifier(self_modifier) => {
                    typed_func_params.push(TypedFuncParamKind::SelfModifier(self_modifier.clone()));
                }
            }
        }

        let typed_variadic_param = {
            if let Some(variadic_param) = &func_decl.params.variadic {
                match variadic_param {
                    FuncVariadicParams::UntypedCStyle => todo!(),
                    FuncVariadicParams::Typed(identifier, type_specifier) => {
                        let concrete_type = match self.resolve_type(
                            module_id,
                            type_specifier.clone(),
                            identifier.loc.clone(),
                            identifier.span.end,
                        ) {
                            Some(concrete_type) => concrete_type,
                            None => return None,
                        };

                        Some(TypedFuncVariadicParams::Typed(identifier.name.clone(), concrete_type))
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

        match self.resolve_func(module_id, func_decl) {
            Some((return_type, typed_func_params, typed_variadic_param)) => {
                self.insert_symbol_entry(
                    module_id,
                    symbol_id,
                    SymbolEntry::Func(ResolvedFunction {
                        module_id,
                        symbol_id,
                        func_sig: FuncSig {
                            name: func_decl.identifier.name.clone(),
                            params: TypedFuncParams {
                                list: typed_func_params.clone(),
                                variadic: typed_variadic_param.clone(),
                            },
                            return_type: return_type.clone(),
                            vis: func_decl.vis.clone(),
                            loc: func_decl.loc.clone(),
                        },
                    }),
                );

                Some(TypedStatement::FuncDecl(TypedFuncDecl {
                    name: func_decl.identifier.name.clone(),
                    params: TypedFuncParams {
                        list: typed_func_params,
                        variadic: typed_variadic_param,
                    },
                    return_type,
                    vis: func_decl.vis.clone(),
                    renamed_as: func_decl.renamed_as.clone(),
                    loc: func_decl.loc.clone(),
                }))
            }
            None => None,
        }
    }

    fn resolve_func_def(&mut self, module_id: ModuleID, func_def: &FuncDef) -> Option<TypedStatement> {
        let local_scope: LocalScopeRef = Rc::new(RefCell::new(LocalScope::new(None)));

        let typed_func_body = match self.resolve_block_statement(Rc::clone(&local_scope), &func_def.body) {
            Some(typed_block_statement) => typed_block_statement,
            None => return None,
        };

        let symbol_id = self
            .lookup_symbol_id(module_id, &func_def.identifier.name.clone())
            .unwrap();

        match self.resolve_func(module_id, &func_def.as_func_decl()) {
            Some((return_type, typed_func_params, typed_variadic_param)) => {
                self.insert_symbol_entry(
                    module_id,
                    symbol_id,
                    SymbolEntry::Func(ResolvedFunction {
                        module_id,
                        symbol_id,
                        func_sig: FuncSig {
                            name: func_def.identifier.name.clone(),
                            params: TypedFuncParams {
                                list: typed_func_params.clone(),
                                variadic: typed_variadic_param.clone(),
                            },
                            return_type: return_type.clone(),
                            vis: func_def.vis.clone(),
                            loc: func_def.loc.clone(),
                        },
                    }),
                );

                Some(TypedStatement::FuncDef(TypedFuncDef {
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

    fn resolve_typedef(&mut self, module_id: ModuleID, typedef: &Typedef) -> Option<TypedStatement> {
        if self.duplicate_symbol(
            module_id,
            typedef.identifier.name.clone(),
            typedef.loc.clone(),
            typedef.span.end,
        ) {
            return None;
        }

        match self.lookup_symbol_id(module_id, &typedef.identifier.name.clone()) {
            Some(symbol_id) => {
                match self.resolve_type(
                    module_id,
                    typedef.type_specifier.clone(),
                    typedef.loc.clone(),
                    typedef.span.end,
                ) {
                    Some(concrete_type) => {
                        let symbol_entry = SymbolEntry::Typedef(ResolvedTypedef {
                            module_id,
                            symbol_id,
                            typedef_sig: TypedefSig {
                                name: typedef.identifier.name.clone(),
                                ty: concrete_type.clone(),
                                vis: typedef.vis.clone(),
                                loc: typedef.loc.clone(),
                            },
                        });
                        self.insert_symbol_entry(module_id, symbol_id.clone(), symbol_entry);
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
        scope: LocalScopeRef,
        variable: &Variable,
    ) -> Option<TypedVariable> {
        let var_type = {
            if let Some(var_type_specifier) = &variable.ty {
                match self.resolve_type(
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
                match self.resolve_expr(module_id, Some(Rc::clone(&scope)), rhs) {
                    Some(typed_expr) => Some(typed_expr),
                    None => {
                        return None;
                    }
                }
            } else {
                None
            }
        };

        let typed_variable = TypedVariable {
            name: variable.identifier.name.clone(),
            ty: var_type,
            rhs: typed_rhs,
            loc: variable.loc.clone(),
        };

        let resolved_var = ResolvedVariable {
            module_id,
            symbol_id: generate_symbol_id(),
            typed_variable: typed_variable.clone(),
        };

        let mut scope_borrowed = scope.borrow_mut();
        scope_borrowed.insert(variable.identifier.name.clone(), LocalSymbol::Variable(resolved_var));
        drop(scope_borrowed);

        Some(typed_variable)
    }

    fn declare_local_typedef(
        &mut self,
        scope: LocalScopeRef,
        module_id: ModuleID,
        typedef: &Typedef,
    ) -> Option<TypedStatement> {
        let concrete_type = match self.resolve_type(
            module_id,
            typedef.type_specifier.clone(),
            typedef.loc.clone(),
            typedef.span.end,
        ) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let typed_typedef = TypedTypedef {
            name: typedef.identifier.name.clone(),
            ty: concrete_type.clone(),
            vis: typedef.vis.clone(),
            loc: typedef.loc.clone(),
        };

        let resolved_typedef = ResolvedTypedef {
            module_id,
            symbol_id: generate_symbol_id(),
            typedef_sig: TypedefSig {
                name: typedef.identifier.name.clone(),
                ty: concrete_type,
                vis: typedef.vis.clone(),
                loc: typedef.loc.clone(),
            },
        };

        let mut scope_borrowed = scope.borrow_mut();
        scope_borrowed.insert(typedef.identifier.name.clone(), LocalSymbol::Typedef(resolved_typedef));
        drop(scope_borrowed);

        Some(TypedStatement::Typedef(typed_typedef))
    }

    fn resolve_block_statement(
        &mut self,
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
                Statement::If(if_stmt) => {
                    let typed_condition =
                        match self.resolve_expr(module_id, Some(Rc::clone(&local_scope)), &if_stmt.condition) {
                            Some(typed_expr) => typed_expr,
                            None => continue,
                        };

                    let typed_consequent =
                        match self.resolve_block_statement(Rc::clone(&local_scope), &if_stmt.consequent) {
                            Some(typed_block) => Box::new(typed_block),
                            None => continue,
                        };

                    let typed_alternate = {
                        if let Some(alternate) = &if_stmt.alternate {
                            match self.resolve_block_statement(Rc::clone(&local_scope), &*alternate) {
                                Some(typed_block) => Some(Box::new(typed_block)),
                                None => continue,
                            }
                        } else {
                            None
                        }
                    };

                    typed_body.push(TypedStatement::If(TypedIf {
                        condition: typed_condition,
                        consequent: typed_consequent,
                        branches: Vec::new(),
                        alternate: typed_alternate,
                        loc: if_stmt.loc.clone(),
                    }));
                }
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
                Statement::For(for_stmt) => {
                    let initializer = {
                        if let Some(variable) = &for_stmt.initializer {
                            match self.declare_local_variable(module_id, Rc::clone(&local_scope), &variable) {
                                Some(typed_var) => Some(typed_var),
                                None => continue,
                            }
                        } else {
                            None
                        }
                    };

                    let condition = {
                        if let Some(expr) = &for_stmt.condition {
                            match self.resolve_expr(module_id, Some(Rc::clone(&local_scope)), expr) {
                                Some(typed_expr) => Some(typed_expr),
                                None => continue,
                            }
                        } else {
                            None
                        }
                    };

                    let increment = {
                        if let Some(expr) = &for_stmt.increment {
                            match self.resolve_expr(module_id, Some(Rc::clone(&local_scope)), expr) {
                                Some(typed_expr) => Some(typed_expr),
                                None => continue,
                            }
                        } else {
                            None
                        }
                    };

                    let for_typed_body = match self.resolve_block_statement(Rc::clone(&local_scope), &*for_stmt.body) {
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
                Statement::Foreach(foreach) => {
                    let local_scope_copy = LocalScope::deep_clone(&local_scope);

                    let typed_expr = self.resolve_expr(module_id, Some(local_scope.clone()), &foreach.expr)?;

                    let foreach_typed_body =
                        match self.resolve_block_statement(local_scope_copy.clone(), &*foreach.body) {
                            Some(typed_block) => Box::new(typed_block),
                            None => continue,
                        };

                    let typed_item = {
                        let typed_identifier = TypedIdentifier {
                            name: foreach.item.name.clone(),
                            symbol_id: generate_symbol_id(),
                            loc: foreach.item.loc.clone(),
                        };

                        let mut local_scope = local_scope_copy.borrow_mut();
                        local_scope.insert(
                            typed_identifier.name.clone(),
                            LocalSymbol::Identifier(ResolvedIdentifier {
                                module_id,
                                symbol_id: typed_identifier.symbol_id,
                                name: typed_identifier.name.clone(),
                            }),
                        );
                        drop(local_scope);
                        typed_identifier
                    };

                    let typed_index = {
                        if let Some(index) = &foreach.index {
                            let typed_identifier = TypedIdentifier {
                                name: index.name.clone(),
                                symbol_id: generate_symbol_id(),
                                loc: index.loc.clone(),
                            };

                            let mut local_scope = local_scope_copy.borrow_mut();
                            local_scope.insert(
                                index.name.clone(),
                                LocalSymbol::Identifier(ResolvedIdentifier {
                                    module_id,
                                    symbol_id: typed_identifier.symbol_id,
                                    name: index.name.clone(),
                                }),
                            );
                            drop(local_scope);

                            Some(typed_identifier)
                        } else {
                            None
                        }
                    };

                    typed_body.push(TypedStatement::Foreach(TypedForeach {
                        expr: typed_expr,
                        item: typed_item,
                        index: typed_index,
                        body: foreach_typed_body,
                        loc: foreach.loc.clone(),
                    }));
                }
                Statement::Switch(switch) => todo!(),
                Statement::Enum(enum_decl) => {
                    match self.resolve_enum(module_id, Some(Rc::clone(&local_scope)), enum_decl) {
                        Some(typed_stmt) => {
                            typed_body.push(typed_stmt);
                        }
                        None => continue,
                    }
                }
                Statement::Struct(_) => todo!(),
                Statement::BlockStatement(block_statement) => {
                    let local_scope_copy = LocalScope::deep_clone(&local_scope);

                    match self.resolve_block_statement(local_scope_copy, block_statement) {
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
                    match self.declare_local_typedef(Rc::clone(&local_scope), module_id, typedef) {
                        Some(typed_stmt) => {
                            typed_body.push(typed_stmt);
                        }
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

        macro_rules! get_local_scope {
            () => {{
                if let Some(local_scope) = local_scope_opt {
                    Rc::clone(&local_scope)
                } else {
                    return None;
                }
            }};
        }

        macro_rules! resolve_local_identifier {
            ($identifier:expr) => {{
                if is_unscoped_expr!($identifier.loc.clone(), $identifier.span.end) {
                    return None;
                }

                let local_scope = get_local_scope!();
                let scope_borrowed = local_scope.borrow_mut();

                let local_symbol_opt = scope_borrowed.resolve(&$identifier.name.clone()).cloned();
                if local_symbol_opt.is_none() {
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
                drop(scope_borrowed);

                local_symbol_opt.unwrap().get_symbol_id()
            }};
        }

        match expr {
            Expression::FieldAccess(field) => {
                // self.resolve_expr(scope.clone(), *field.operand.clone())?;
                // optional: check field existence here if possible
                todo!();
            }
            Expression::MethodCall(method_call) => todo!(),
            Expression::StructInit(init) => {
                // for (_, value) in &init.field_inits {
                //     self.resolve_expr(scope.clone(), value.clone())?;
                // }
                todo!();
            }
            Expression::ModuleImport(module_import) => {
                if let Some(identifier) = module_import.as_identifier() {
                    let symbol_id = resolve_local_identifier!(identifier);
                    Some(TypedExpression::Symbol(symbol_id))
                } else {
                    match self.resolve_module_import(module_id, module_import.clone()) {
                        Some(symbol_id) => Some(TypedExpression::Symbol(symbol_id)),
                        None => return None,
                    }
                }
            }
            Expression::Identifier(identifier) => {
                let symbol_id = resolve_local_identifier!(identifier);
                Some(TypedExpression::Symbol(symbol_id))
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

                Some(TypedExpression::FuncCall(TypedFuncCall {
                    args: typed_args,
                    symbol_id,
                    loc: func_call.loc.clone(),
                }))
            }
            Expression::Array(arr) => {
                let element_type =
                    match self.resolve_type(module_id, arr.data_type.clone(), arr.loc.clone(), arr.span.end) {
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

                Some(TypedExpression::Array(TypedArray {
                    element_type,
                    elements: typed_elements,
                    loc: arr.loc.clone(),
                }))
            }
            Expression::Infix(bin) => {
                let lhs = self.resolve_expr(module_id, local_scope_opt.clone(), &*bin.lhs.clone())?;
                let rhs = self.resolve_expr(module_id, local_scope_opt.clone(), &*bin.rhs.clone())?;

                Some(TypedExpression::Infix(TypedInfixExpression {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: bin.op.clone(),
                    loc: bin.loc.clone(),
                }))
            }
            Expression::Prefix(prefix) => {
                let operand = self.resolve_expr(module_id, local_scope_opt.clone(), &*prefix.operand.clone())?;

                Some(TypedExpression::Prefix(TypedPrefixExpression {
                    operand: Box::new(operand),
                    op: prefix.op.clone(),
                    loc: prefix.loc.clone(),
                }))
            }
            Expression::Cast(cast) => {
                let operand = match self.resolve_expr(module_id, local_scope_opt.clone(), &*cast.expr.clone()) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                let target_type =
                    match self.resolve_type(module_id, cast.target_type.clone(), cast.loc.clone(), cast.span.end) {
                        Some(concrete_type) => concrete_type,
                        None => return None,
                    };

                Some(TypedExpression::Cast(TypedCast {
                    operand: Box::new(operand),
                    target_type,
                    loc: cast.loc.clone(),
                }))
            }
            Expression::TypeSpecifier(type_specifier) => {
                let (loc, span_end) = type_specifier.get_loc();

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: ResolverDiagKind::UselessTypeSpecifier,
                    location: Some(DiagLoc::new(self.get_current_module_file_path(), loc, span_end)),
                    hint: None,
                });

                None
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

                Some(TypedExpression::Assignment(TypedAssignment {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    loc: assignment.loc.clone(),
                }))
            }
            Expression::Literal(literal) => Some(TypedExpression::Literal(literal.clone())),
            Expression::Unary(unary) => {
                let operand = match self.resolve_expr(module_id, local_scope_opt.clone(), &*unary.operand) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                Some(TypedExpression::Unary(TypedUnaryExpression {
                    op: unary.op.clone(),
                    operand: Box::new(operand),
                    loc: unary.loc.clone(),
                }))
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

                Some(TypedExpression::ArrayIndex(TypedArrayIndex {
                    operand: Box::new(operand),
                    index: Box::new(index),
                    loc: array_index.loc.clone(),
                }))
            }
            Expression::AddressOf(address_of) => {
                let operand = match self.resolve_expr(module_id, local_scope_opt.clone(), &address_of.expr) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                Some(TypedExpression::AddressOf(TypedAddressOf {
                    operand: Box::new(operand),
                    loc: address_of.loc.clone(),
                }))
            }
            Expression::Dereference(dereference) => {
                let operand = match self.resolve_expr(module_id, local_scope_opt.clone(), &dereference.expr) {
                    Some(typed_expr) => typed_expr,
                    None => return None,
                };

                Some(TypedExpression::Dereference(TypedDereference {
                    operand: Box::new(operand),
                    loc: dereference.loc.clone(),
                }))
            }
            Expression::UnnamedStructValue(unnamed_struct_value) => {
                let mut fields: Vec<TypedUnnamedStructValueField> = Vec::new();

                for field in &unnamed_struct_value.fields {
                    let field_type = {
                        if let Some(type_specifier) = &field.field_type {
                            match self.resolve_type(
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

                Some(TypedExpression::UnnamedStructValue(TypedUnnamedStructValue {
                    fields,
                    packed: unnamed_struct_value.packed,
                    loc: unnamed_struct_value.loc.clone(),
                }))
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

    fn lookup_symbol_id(&self, module_id: ModuleID, name: &str) -> Option<SymbolID> {
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

    fn lookup_symbol(&self, module_id: ModuleID, name: &str) -> Option<SymbolEntry> {
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

    fn insert_module_alias(&self, alias: ModuleAlias, module_id: ModuleID) {
        let mut module_aliases = self.module_aliases.lock().unwrap();
        module_aliases.insert(alias, module_id);
        drop(module_aliases);
    }

    fn get_module_alias(&self, alias: ModuleAlias) -> Option<ModuleID> {
        let module_aliases = self.module_aliases.lock().unwrap();
        let option = module_aliases.get(&alias).cloned();
        drop(module_aliases);
        option
    }

    fn get_current_module_file_path(&self) -> ModuleFilePath {
        let current_module_id = self.current_module.unwrap();
        let file_paths = self.file_paths.lock().unwrap();
        let file_path = match file_paths.get(&current_module_id) {
            Some(child_module_file_path) => child_module_file_path.clone(),
            None => self.master_module_file_path.clone(),
        };
        drop(file_paths);
        file_path
    }

    fn get_module_file_path(&self, module_id: ModuleID) -> Option<ModuleFilePath> {
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
}

pub fn generate_module_id() -> ModuleID {
    let mut rng = rand::rng();
    rng.random::<u64>()
}
