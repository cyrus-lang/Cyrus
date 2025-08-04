use crate::{
    declsign::TypedefSig,
    diagnostics::ResolverDiagKind,
    scope::{LocalScope, LocalScopeRef, ResolvedTypedef, SymbolEntry, SymbolTable, generate_symbol_id},
};
use ast::{
    BlockStatement, Expression, FuncDef, ProgramTree, Statement, TypeSpecifier, Typedef,
    token::{Location, Span, Token, TokenKind},
};
use diagcentral::{Diag, DiagLevel, DiagLoc, reporter::DiagReporter};
use rand::Rng;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use typed_ast::{
    ModuleID, SymbolID, TypedExpression, TypedFuncCall, TypedIdentifier, TypedProgramTree, TypedStatement,
    TypedTypedef,
    types::{ConcreteType, TypedUnnamedStructType, TypedUnnamedStructTypeField},
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
            TypeSpecifier::Array(array_type_specifier) => todo!(),
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
                Statement::FuncDef(func_def) => match self.resoolve_func_def(&module_id, func_def) {
                    Some(typed_stmt) => Ok(typed_stmt),
                    None => continue,
                },
                Statement::FuncDecl(func_decl) => todo!(),
                Statement::Struct(_) => todo!(),
                Statement::Import(import) => todo!(),
                Statement::Enum(_) => todo!(),
                Statement::GlobalVariable(global_variable) => todo!(),

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

    fn resoolve_func_def(&mut self, module_id: &ModuleID, func_def: &FuncDef) -> Option<TypedStatement> {
        let return_type_specifier = func_def.return_type.clone().unwrap_or(TypeSpecifier::TypeToken(Token {
            kind: TokenKind::Void,
            span: Span::default(),
            loc: Location::default(),
        }));

        let return_type = match self.resolve_type(return_type_specifier, func_def.loc.clone(), func_def.span.end) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        todo!()
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

        dbg!(self.global_symbols.get(&module_id).unwrap().names.clone());

        let typed_body = self.resolve_definitions(module_id, &ast);

        // for symbol_entry in symbol_table.values().into_iter() {
        //     let resolved_func = match symbol_entry {
        //         SymbolEntry::Func(resolved_func) => resolved_func,
        //         _ => continue,
        //     };

        //     if let Some(body) = &resolved_func.body {
        //         self.resolve_func_body(body.clone());
        //     }
        // }

        TypedProgramTree { body: typed_body }
    }

    fn resolve_func_body(&self, func_body: BlockStatement) -> Result<(), ResolverDiagKind> {
        let local_scope: LocalScopeRef = Rc::new(RefCell::new(LocalScope::new(None)));

        // for stmt in func_body.exprs {
        //     self.resolve_expr(Rc::clone(&local_scope), stmt)?;
        // }

        Ok(())
    }

    fn resolve_expr(&self, scope: LocalScopeRef, expr: Expression) -> Result<TypedExpression, ResolverDiagKind> {
        match expr {
            Expression::Identifier(ident) => {
                let name = &ident.name;
                let mut scope_borrowed = scope.borrow_mut();
                let symbol_entry = scope_borrowed.resolve(name).cloned();
                if symbol_entry.is_none() {
                    return Err(ResolverDiagKind::SymbolNotFound { name: name.clone() });
                }

                scope_borrowed.insert(name.clone(), symbol_entry.unwrap());
                drop(scope_borrowed);

                Ok(TypedExpression::Identifier(TypedIdentifier {
                    name: name.clone(),
                    loc: ident.loc.clone(),
                }))
            }
            Expression::FuncCall(ref func_call) => {
                let typed_operand = self.resolve_expr(scope.clone(), *func_call.operand.clone())?;
                let typed_args = func_call
                    .args
                    .iter()
                    .map(|arg| self.resolve_expr(scope.clone(), arg.clone()))
                    .collect::<Result<Vec<TypedExpression>, ResolverDiagKind>>()?;

                Ok(TypedExpression::FuncCall(TypedFuncCall {
                    args: typed_args,
                    operand: Box::new(typed_operand),
                    loc: func_call.loc.clone(),
                }))
            }
            Expression::FieldAccess(ref field) => {
                // self.resolve_expr(scope.clone(), *field.operand.clone())?;
                // optional: check field existence here if possible
                todo!();
            }
            Expression::Array(ref arr) => {
                // for item in &arr.elements {
                //     self.resolve_expr(scope.clone(), item.clone())?;
                // }
                todo!();
            }
            Expression::Infix(ref bin) => {
                // self.resolve_expr(scope.clone(), *bin.left.clone())?;
                // self.resolve_expr(scope.clone(), *bin.right.clone())?;
                todo!();
            }
            Expression::Prefix(ref unary) => {
                // self.resolve_expr(scope.clone(), *unary.expr.clone())?;
                todo!();
            }
            Expression::StructInit(ref init) => {
                // for (_, value) in &init.field_inits {
                //     self.resolve_expr(scope.clone(), value.clone())?;
                // }
                todo!();
            }
            Expression::Cast(cast) => todo!(),
            Expression::TypeSpecifier(type_specifier) => todo!(),
            Expression::ModuleImport(module_import) => todo!(),
            Expression::Assignment(assignment) => todo!(),
            Expression::Literal(literal) => todo!(),
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
