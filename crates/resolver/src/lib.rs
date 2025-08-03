use crate::{
    diagnostics::ResolverDiagKind,
    scope::{LocalScope, LocalScopeRef, SymbolEntry, SymbolTable},
};
use ast::{BlockStatement, Expression, ProgramTree, TypeSpecifier};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use typed_ast::{ModuleID, TypedExpression, TypedFuncCall, TypedIdentifier, types::ConcreteType};

pub mod declsign;
mod diagnostics;
pub mod scope;
mod tests;

pub struct Resolver {
    pub global_symbols: HashMap<ModuleID, SymbolTable>,
    pub current_module: Option<ModuleID>,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            global_symbols: HashMap::new(),
            current_module: None,
        }
    }

    fn resolve_type(&self, type_specifier: TypeSpecifier) -> Result<ConcreteType, ResolverDiagKind> {
        match type_specifier {
            TypeSpecifier::TypeToken(token) => Ok(ConcreteType::from(token.kind.clone())),
            TypeSpecifier::Const(type_specifier) => {
                Ok(ConcreteType::Const(Box::new(self.resolve_type(*type_specifier)?)))
            }
            TypeSpecifier::Dereference(type_specifier) => todo!(),
            TypeSpecifier::Array(array_type_specifier) => todo!(),
            TypeSpecifier::UnnamedStruct(unnamed_struct_type) => todo!(),
            TypeSpecifier::ModuleImport(module_import) => {
                // let module_symbols = self.modules.get(&module_import.module_id)
                //     .ok_or_else(|| ResolverDiagKind::ModuleNotFound(module_import.module_id.clone()))?;
                todo!();
            }
            TypeSpecifier::Identifier(identifier) => {
                let symbol_table = self.global_symbols.get(&self.current_module.unwrap()).unwrap();
                match symbol_table.get(&identifier.name.clone()) {
                    Some(symbol_entry) => match symbol_entry {
                        SymbolEntry::Typedef(type_entry) => Ok(type_entry.typedef_sig.ty.clone()),
                        _ => Err(ResolverDiagKind::TypeNotFound {
                            name: identifier.name.clone(),
                        }),
                    },
                    None => Err(ResolverDiagKind::TypeNotFound {
                        name: identifier.name.clone(),
                    }),
                }
            }
        }
    }

    pub fn resolve_module(&mut self, module_id: ModuleID, ast: &ProgramTree) -> Result<(), ResolverDiagKind> {
        self.current_module = Some(module_id);

        let mut symbol_table = SymbolTable::new();

        // Collect function declarations
        for stmt in &ast.body {
            match stmt {
                // Statement::Import(import) => todo!(),
                // Statement::Typedef(typedef) => {
                //     let typedef = ResolvedTypedef {
                //         module_id,
                //         typedef_sig: TypedefSig {
                //             name: typedef.identifier.name.clone(),
                //             ty: self.resolve_type(typedef.type_specifier.clone())?,
                //             vis: typedef.vis.clone(),
                //             loc: typedef.loc.clone(),
                //         },
                //     };

                //     symbol_table.insert(typedef.typedef_sig.name.clone(), SymbolEntry::Typedef(typedef));
                // }
                // Statement::GlobalVariable(global_variable) => {
                //     // let global_var_type = self.resolve_type(
                //     //     global_variable.type_specifier.unwrap_or(
                //     //         self.resolve_expr(scope, global_variable.expr.unwrap())
                //     //     )
                //     // )?;

                //     // let global_var = ResolvedGlobalVar {
                //     //     module_id,
                //     //     global_var_sig: GlobalVarSig {
                //     //         name: global_variable.identifier.name.clone(),
                //     //         ty: global_var_type,
                //     //         vis: global_variable.vis.clone(),
                //     //         loc: global_variable.loc.clone(),
                //     //     },
                //     // };

                //     // symbol_table.insert(
                //     //     global_var.global_var_sig.name.clone(),
                //     //     SymbolEntry::GlobalVar(global_var),
                //     // );
                // }
                // Statement::FuncDef(func_def) => {
                //     let func_sig = FuncSig {
                //         name: func_def.name.clone(),
                //         params: func_def.params.clone(),
                //         return_type: func_def.return_type.clone(),
                //         vis: func_def.vis.clone(),
                //         loc: func_def.loc.clone(),
                //     };

                //     let resolved_fn = ResolvedFunction {
                //         module_id,
                //         func_sig: func_sig.clone(),
                //         body: Some(*func_def.body.clone()),
                //     };

                //     symbol_table.insert(func_sig.name, SymbolEntry::Func(resolved_fn));
                // }
                // Statement::FuncDecl(func_decl) => {
                //     let func_sig = FuncSig {
                //         name: func_decl.name.clone(),
                //         params: func_decl.params.clone(),
                //         return_type: func_decl.return_type.clone(),
                //         vis: func_decl.vis.clone(),
                //         loc: func_decl.loc.clone(),
                //     };

                //     let resolved_fn = ResolvedFunction {
                //         module_id,
                //         func_sig: func_sig.clone(),
                //         body: None,
                //     };

                //     symbol_table.insert(func_sig.name, SymbolEntry::Func(resolved_fn));
                // }
                // Statement::Struct(struct_decl) => {
                //     let struct_sig = StructSig {
                //         name: struct.name.clone(),
                //         fields: struct.fields.clone(),
                //         methods: struct.methods.clone(),
                //         vis: struct.vis.clone(),
                //         loc: struct.loc.clone(),
                //     };

                //     let resolved_struct = ResolvedStruct {
                //         module_id,
                //         struct_sig: struct_sig.clone(),
                //     };

                //     symbol_table.insert(struct_sig.name, SymbolEntry::Struct(resolved_struct));
                // }
                // Statement::Enum(enum) => {
                //     let enum_sig = EnumSig {
                //         name: enum.name.clone(),
                //         variants: enum.variants.clone(),
                //         vis: enum.vis.clone(),
                //         loc: enum.loc.clone(),
                //     };

                //     let resolved_enum = ResolvedEnum {
                //         module_id,
                //         enum_sig: enum_sig.clone(),
                //     };

                //     symbol_table.insert(enum_sig.name, SymbolEntry::Enum(resolved_enum));
                // }
                _ => unreachable!(),
            }
        }

        for symbol_entry in symbol_table.values().into_iter() {
            let resolved_func = match symbol_entry {
                SymbolEntry::Func(resolved_func) => resolved_func,
                _ => continue,
            };

            if let Some(body) = &resolved_func.body {
                self.resolve_func_body(body.clone())?;
            }
        }

        self.global_symbols.insert(module_id, symbol_table);
        Ok(())
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

    pub fn lookup_symbol(&self, module_id: &ModuleID, name: &str) -> Option<&SymbolEntry> {
        self.global_symbols
            .get(module_id)
            .and_then(|symbol_table| symbol_table.get(name))
    }
}
