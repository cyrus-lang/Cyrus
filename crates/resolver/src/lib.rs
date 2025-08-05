use crate::{
    declsign::TypedefSig,
    diagnostics::ResolverDiagKind,
    scope::{LocalScope, LocalScopeRef, LocalSymbol, ResolvedTypedef, SymbolEntry, SymbolTable, generate_symbol_id},
};
use ast::{
    ArrayCapacity, BlockStatement, Enum, EnumVariant, Expression, FuncDecl, FuncDef, FuncParamKind, FuncVariadicParams,
    LiteralKind, ProgramTree, Statement, Struct, TypeSpecifier, Typedef, Variable,
    token::{Location, Span, Token, TokenKind},
};
use diagcentral::{Diag, DiagLevel, DiagLoc, reporter::DiagReporter};
use rand::Rng;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use typed_ast::{
    ModuleID, SymbolID, TypedBlockStatement, TypedBreak, TypedContinue, TypedEnum, TypedEnumValuedField,
    TypedEnumVariant, TypedExpression, TypedFor, TypedFuncCall, TypedFuncDecl, TypedFuncDef, TypedFuncParam,
    TypedFuncParamKind, TypedFuncParams, TypedFuncVariadicParams, TypedIf, TypedProgramTree, TypedReturn,
    TypedStatement, TypedStruct, TypedStructField, TypedTypedef, TypedVariable,
    types::{ConcreteType, TypedArrayCapacity, TypedArrayType, TypedUnnamedStructType, TypedUnnamedStructTypeField},
};

pub mod declsign;
mod diagnostics;
pub mod scope;
mod tests;

type MaybeResolved<T> = Option<T>;

pub struct Resolver {
    pub global_symbols: HashMap<ModuleID, SymbolTable>,
    pub file_path: String,
    pub reporter: DiagReporter<ResolverDiagKind>,
    current_module: Option<ModuleID>,
}

impl Resolver {
    pub fn new(file_path: String) -> Self {
        Resolver {
            global_symbols: HashMap::new(),
            file_path,
            reporter: DiagReporter::new(),
            current_module: None,
        }
    }

    fn resolve_type(
        &mut self,
        type_specifier: TypeSpecifier,
        loc: Location,
        span_end: usize,
    ) -> MaybeResolved<ConcreteType> {
        let resolving_result = match type_specifier {
            TypeSpecifier::TypeToken(token) => Ok(ConcreteType::from(token.kind.clone())),
            TypeSpecifier::Const(type_specifier) => Ok(ConcreteType::Const(Box::new(self.resolve_type(
                *type_specifier,
                loc.clone(),
                span_end,
            )?))),
            TypeSpecifier::Dereference(type_specifier) => Ok(ConcreteType::Pointer(Box::new(self.resolve_type(
                *type_specifier,
                loc.clone(),
                span_end,
            )?))),
            TypeSpecifier::Array(array_type_specifier) => Ok({
                let element_type = match self.resolve_type(*array_type_specifier.element_type, loc.clone(), span_end) {
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
                                    location: Some(DiagLoc::new(self.file_path.to_string(), loc.clone(), span_end)),
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
                    match self.resolve_type(field.field_type.clone(), field.loc.clone(), field.span.end) {
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
            TypeSpecifier::ModuleImport(module_import) => {
                // let module_symbols = self.modules.get(&module_import.module_id)
                //     .ok_or_else(|| ResolverDiagKind::ModuleNotFound(module_import.module_id.clone()))?;
                todo!();
            }
            TypeSpecifier::Identifier(identifier) => {
                match match self.lookup_symbol_id(&self.current_module.unwrap(), &identifier.name.clone()) {
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
                    location: Some(DiagLoc::new(self.file_path.to_string(), loc, span_end)),
                    hint: None,
                });
                None
            }
        }
    }

    pub fn insert_symbol_entry(&mut self, module_id: &ModuleID, symbol_id: SymbolID, entry: SymbolEntry) {
        let symbol_table = self.global_symbols.get_mut(module_id).unwrap();
        symbol_table.entries.insert(symbol_id, entry);
    }

    pub fn insert_symbol_name(&mut self, module_id: &ModuleID, name: &String) {
        let symbol_id = generate_symbol_id();
        let symbol_table = self.global_symbols.get_mut(module_id).unwrap();
        symbol_table.names.insert(name.clone(), symbol_id);
    }

    // Scans the top-level AST for declarations (typedefs, functions, structs, etc.)
    // And Registers each declared name into the current module’s symbol table. (first pass)
    fn resolve_decl_names(&mut self, module_id: ModuleID, ast: &ProgramTree) {
        for stmt in &ast.body {
            match stmt {
                Statement::Typedef(typedef) => {
                    self.insert_symbol_name(&module_id, &typedef.identifier.name.clone());
                }
                Statement::FuncDef(func_def) => {
                    self.insert_symbol_name(&module_id, &func_def.identifier.name.clone());
                }
                Statement::FuncDecl(func_decl) => {
                    self.insert_symbol_name(&module_id, &func_decl.identifier.name.clone());
                }
                Statement::GlobalVariable(global_variable) => {
                    self.insert_symbol_name(&module_id, &global_variable.identifier.name.clone());
                }
                Statement::Struct(struct_decl) => {
                    self.insert_symbol_name(&module_id, &struct_decl.identifier.name.clone());
                }
                Statement::Enum(enum_) => {
                    self.insert_symbol_name(&module_id, &enum_.identifier.name.clone());
                }
                _ => unreachable!(),
            };
        }
    }

    // Resolves the full meaning of each top-level declaration in the AST (second pass)
    fn resolve_definitions(&mut self, module_id: ModuleID, ast: &ProgramTree) -> Vec<TypedStatement> {
        let mut typed_body: Vec<TypedStatement> = Vec::new();

        for stmt in &ast.body {
            let valid_top_level_stmt: Result<TypedStatement, (Location, usize)> = match stmt {
                // Valid top-level statements.
                Statement::Typedef(typedef) => match self.resolve_typedef(&module_id, typedef) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::FuncDef(func_def) => match self.resoolve_func_def(func_def) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::FuncDecl(func_decl) => match self.resolve_func_decl(func_decl) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Struct(struct_) => match self.resolve_struct(struct_) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::Enum(enum_) => match self.resolve_enum(None, enum_) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::GlobalVariable(global_variable) => todo!(),
                Statement::Import(import) => todo!(),

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
                        location: Some(DiagLoc::new(self.file_path.clone(), loc, span_end)),
                        hint: None,
                    });
                }
            };
        }

        typed_body
    }

    fn resolve_enum(&mut self, local_scope_opt: Option<LocalScopeRef>, enum_: &Enum) -> Option<TypedStatement> {
        let mut variants: Vec<TypedEnumVariant> = Vec::new();

        for variant in &enum_.variants {
            let typed_variant = match variant {
                EnumVariant::Identifier(identifier) => TypedEnumVariant::Identifier(identifier.name.clone()),
                EnumVariant::Variant(identifier, enum_valued_fields) => {
                    let mut fields: Vec<TypedEnumValuedField> = Vec::new();
                    for valued_field in enum_valued_fields {
                        let field_type = match self.resolve_type(
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
            name: enum_.identifier.name.clone(),
            variants,
            vis: enum_.vis.clone(),
            loc: enum_.identifier.loc.clone(),
        }))
    }

    fn resolve_struct(&mut self, struct_: &Struct) -> Option<TypedStatement> {
        let mut typed_struct_fields: Vec<TypedStructField> = Vec::new();
        let mut typed_methods: Vec<TypedFuncDef> = Vec::new();

        for field in &struct_.fields {
            match self.resolve_type(field.ty.clone(), field.loc.clone(), field.span.end) {
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

        for func_def in &struct_.methods {
            let local_scope: LocalScopeRef = Rc::new(RefCell::new(LocalScope::new(None)));

            let typed_func_body = match self.resolve_block_statement(Rc::clone(&local_scope), &func_def.body) {
                Some(typed_block_statement) => typed_block_statement,
                None => return None,
            };

            match self.resolve_func(&func_def.as_func_decl()) {
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
            name: struct_.identifier.name.clone(),
            fields: typed_struct_fields,
            methods: Vec::new(), // FIXME
            vis: struct_.vis.clone(),
            packed: struct_.packed.clone(),
            loc: struct_.loc.clone(),
        }))
    }

    fn resolve_func(
        &mut self,
        func_decl: &FuncDecl,
    ) -> Option<(ConcreteType, Vec<TypedFuncParamKind>, Option<TypedFuncVariadicParams>)> {
        let return_type_specifier = func_decl.return_type.clone().unwrap_or(TypeSpecifier::TypeToken(Token {
            kind: TokenKind::Void,
            span: Span::default(),
            loc: Location::default(),
        }));

        let return_type = match self.resolve_type(return_type_specifier, func_decl.loc.clone(), func_decl.span.end) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let mut typed_func_params: Vec<TypedFuncParamKind> = Vec::new();

        for param in &func_decl.params.list {
            match param {
                FuncParamKind::FuncParam(func_param) => {
                    let param_type = match &func_param.ty {
                        Some(type_specifier) => {
                            match self.resolve_type(type_specifier.clone(), func_param.loc.clone(), func_param.span.end)
                            {
                                Some(concrete_type) => concrete_type,
                                None => continue,
                            }
                        }
                        None => {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: ResolverDiagKind::InvalidUntypedFuncParam,
                                location: Some(DiagLoc::new(
                                    self.file_path.to_string(),
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

    fn resolve_func_decl(&mut self, func_decl: &FuncDecl) -> Option<TypedStatement> {
        match self.resolve_func(func_decl) {
            Some((return_type, typed_func_params, typed_variadic_param)) => {
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

    fn resoolve_func_def(&mut self, func_def: &FuncDef) -> Option<TypedStatement> {
        let local_scope: LocalScopeRef = Rc::new(RefCell::new(LocalScope::new(None)));

        let typed_func_body = match self.resolve_block_statement(Rc::clone(&local_scope), &func_def.body) {
            Some(typed_block_statement) => typed_block_statement,
            None => return None,
        };

        match self.resolve_func(&func_def.as_func_decl()) {
            Some((return_type, typed_func_params, typed_variadic_param)) => {
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

    fn resolve_typedef(&mut self, module_id: &u64, typedef: &Typedef) -> Option<TypedStatement> {
        match self.lookup_symbol_id(module_id, &typedef.identifier.name.clone()) {
            Some(symbol_id) => {
                match self.resolve_type(typedef.type_specifier.clone(), typedef.loc.clone(), typedef.span.end) {
                    Some(concrete_type) => {
                        let symbol_entry = SymbolEntry::Typedef(ResolvedTypedef {
                            module_id: *module_id,
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
                        self.file_path.to_string(),
                        typedef.loc.clone(),
                        typedef.span.end,
                    )),
                    hint: None,
                });
                None
            }
        }
    }

    pub fn resolve_module(&mut self, module_id: ModuleID, ast: &ProgramTree) -> TypedProgramTree {
        self.current_module = Some(module_id);

        // Initialize symbol table for this module
        self.global_symbols.insert(module_id, SymbolTable::new());

        self.resolve_decl_names(module_id, &ast);
        let typed_body = self.resolve_definitions(module_id, &ast);
        TypedProgramTree { body: typed_body }
    }

    fn declare_local_variable(&mut self, scope: LocalScopeRef, variable: &Variable) -> Option<TypedVariable> {
        let var_type = {
            if let Some(var_type_specifier) = &variable.ty {
                match self.resolve_type(var_type_specifier.clone(), variable.loc.clone(), variable.span.end) {
                    Some(concrete_type) => Some(concrete_type),
                    None => return None,
                }
            } else {
                None
            }
        };

        let typed_rhs = {
            if let Some(rhs) = &variable.rhs {
                match self.resolve_expr(Some(Rc::clone(&scope)), rhs) {
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

        let mut scope_borrowed = scope.borrow_mut();
        scope_borrowed.insert(
            variable.identifier.name.clone(),
            LocalSymbol::Variable(typed_variable.clone()),
        );
        drop(scope_borrowed);

        Some(typed_variable)
    }

    fn declare_local_typedef(
        &mut self,
        scope: LocalScopeRef,
        module_id: &ModuleID,
        typedef: &Typedef,
    ) -> Option<TypedStatement> {
        let concrete_type =
            match self.resolve_type(typedef.type_specifier.clone(), typedef.loc.clone(), typedef.span.end) {
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
            module_id: *module_id,
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
                    match self.declare_local_variable(Rc::clone(&local_scope), &variable) {
                        Some(typed_var) => {
                            typed_body.push(TypedStatement::Variable(typed_var));
                        }
                        None => {
                            todo!();
                            continue;
                        }
                    }
                }
                Statement::Expression(expr) => match self.resolve_expr(Some(Rc::clone(&local_scope)), expr) {
                    Some(typed_expr) => {
                        typed_body.push(TypedStatement::Expression(typed_expr));
                    }
                    None => continue,
                },
                Statement::If(if_stmt) => {
                    let typed_condition = match self.resolve_expr(Some(Rc::clone(&local_scope)), &if_stmt.condition) {
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
                            match self.resolve_expr(Some(Rc::clone(&local_scope)), argument) {
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
                            match self.declare_local_variable(Rc::clone(&local_scope), &variable) {
                                Some(typed_var) => Some(typed_var),
                                None => continue,
                            }
                        } else {
                            None
                        }
                    };

                    let condition = {
                        if let Some(expr) = &for_stmt.condition {
                            match self.resolve_expr(Some(Rc::clone(&local_scope)), expr) {
                                Some(typed_expr) => Some(typed_expr),
                                None => continue,
                            }
                        } else {
                            None
                        }
                    };

                    let increment = {
                        if let Some(expr) = &for_stmt.increment {
                            match self.resolve_expr(Some(Rc::clone(&local_scope)), expr) {
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
                Statement::Foreach(foreach) => todo!(),
                Statement::Switch(switch) => todo!(),
                Statement::Enum(enum_) => match self.resolve_enum(Some(Rc::clone(&local_scope)), enum_) {
                    Some(typed_stmt) => {
                        typed_body.push(typed_stmt);
                    }
                    None => continue,
                },
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
                    match self.declare_local_typedef(Rc::clone(&local_scope), &module_id, typedef) {
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

    fn resolve_expr(&self, scope: Option<LocalScopeRef>, expr: &Expression) -> Option<TypedExpression> {
        match expr {
            Expression::Identifier(ident) => {
                // let name = &ident.name;
                // let mut scope_borrowed = scope.borrow_mut();
                // let symbol_entry = scope_borrowed.resolve(name).cloned();
                // if symbol_entry.is_none() {
                //     return Err(ResolverDiagKind::SymbolNotFound { name: name.clone() });
                // }

                // scope_borrowed.insert(name.clone(), symbol_entry.unwrap());
                // drop(scope_borrowed);

                // Ok(TypedExpression::Identifier(TypedIdentifier {
                //     name: name.clone(),
                //     loc: ident.loc.clone(),
                // }))
                todo!()
            }
            Expression::FuncCall(func_call) => {
                let typed_operand = self.resolve_expr(scope.clone(), &func_call.operand.clone())?;
                let mut typed_args: Vec<TypedExpression> = Vec::new();

                for arg in &func_call.args {
                    match self.resolve_expr(scope.clone(), &arg.clone()) {
                        Some(typed_expr) => {
                            typed_args.push(typed_expr);
                        }
                        None => continue,
                    }
                }

                Some(TypedExpression::FuncCall(TypedFuncCall {
                    args: typed_args,
                    operand: Box::new(typed_operand),
                    loc: func_call.loc.clone(),
                }))
            }
            Expression::FieldAccess(field) => {
                // self.resolve_expr(scope.clone(), *field.operand.clone())?;
                // optional: check field existence here if possible
                todo!();
            }
            Expression::Array(arr) => {
                // for item in &arr.elements {
                //     self.resolve_expr(scope.clone(), item.clone())?;
                // }
                todo!();
            }
            Expression::Infix(bin) => {
                // self.resolve_expr(scope.clone(), *bin.left.clone())?;
                // self.resolve_expr(scope.clone(), *bin.right.clone())?;
                todo!();
            }
            Expression::Prefix(unary) => {
                // self.resolve_expr(scope.clone(), *unary.expr.clone())?;
                todo!();
            }
            Expression::StructInit(init) => {
                // for (_, value) in &init.field_inits {
                //     self.resolve_expr(scope.clone(), value.clone())?;
                // }
                todo!();
            }
            Expression::Cast(cast) => todo!(),
            Expression::TypeSpecifier(type_specifier) => todo!(),
            Expression::ModuleImport(module_import) => todo!(),
            Expression::Assignment(assignment) => todo!(),
            Expression::Literal(literal) => Some(TypedExpression::Literal(literal.clone())),
            Expression::Unary(unary_expression) => todo!(),
            Expression::ArrayIndex(array_index) => todo!(),
            Expression::AddressOf(address_of) => todo!(),
            Expression::Dereference(dereference) => todo!(),
            Expression::MethodCall(method_call) => todo!(),
            Expression::UnnamedStructValue(unnamed_struct_value) => todo!(),
        }
    }

    pub fn lookup_symbol_id(&self, module_id: &ModuleID, name: &str) -> Option<SymbolID> {
        match self.global_symbols.get(module_id) {
            Some(symbol_table) => match symbol_table.names.get(name) {
                Some(symbol_id) => Some(*symbol_id),
                None => None,
            },
            None => None,
        }
    }

    pub fn lookup_symbol(&self, module_id: &ModuleID, name: &str) -> Option<&SymbolEntry> {
        match self.global_symbols.get(module_id) {
            Some(symbol_table) => match symbol_table.names.get(name) {
                Some(symbol_id) => match symbol_table.entries.get(symbol_id) {
                    Some(symbol_entry) => Some(symbol_entry),
                    None => None,
                },
                None => None,
            },
            None => None,
        }
    }
}

pub fn generate_module_id() -> ModuleID {
    let mut rng = rand::rng();
    rng.random::<u64>()
}
