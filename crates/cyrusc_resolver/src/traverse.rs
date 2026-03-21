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

use crate::{Resolver, scope_required};
use cyrusc_ast::*;
use cyrusc_internal::symbols::table::GlobalSymbolQuery;
use cyrusc_tast::exprs::*;
use cyrusc_tast::stmts::*;
use cyrusc_tast::*;
use cyrusc_tokens::literals::Literal;
use cyrusc_tokens::loc::Loc;
use cyrusc_tokens::loc::Location;

// Resolver endpoints.
impl Resolver {
    // Scans the top-level AST for declarations (typedefs, functions, structs, etc.)
    // And Registers each declared name into the current module’s symbol table. (first pass)
    pub(crate) fn resolve_decl_names(&mut self, ast: &ProgramTree) {
        let module_id = self.module_id.unwrap();
        let file_path = self.current_file_path();

        for stmt in ast.body.as_ref() {
            // skip statements that if it's not a declaration symbol.
            let Some(decl_name) = stmt.decl_name() else {
                continue;
            };

            let loc = SourceLoc::from_loc(stmt.loc(), file_path.clone());

            if self.report_if_duplicate_symbol(decl_name.as_string(), loc) {
                continue;
            }

            self.global_symbols_registry
                .insert_symbol_name(module_id, &decl_name.value);
        }
    }

    // Resolves the full meaning of each top-level declaration in the AST (second pass)
    pub(crate) fn resolve_decl_full(&mut self, ast: &ProgramTree) -> Vec<TypedStmt> {
        let mut typed_body = Vec::new();
        let module_id = self.module_id.unwrap();
        let file_path = self.current_file_path();

        for stmt in ast.body.as_ref() {
            let typed_stmt = match stmt {
                Stmt::Import(..) | Stmt::Expr(..) => continue,
                Stmt::Builtin(builtin) => self.resolve_builtin(builtin).map(TypedStmt::Builtin),
                Stmt::GlobalVar(global_var) => self.resolve_global_var_stmt(global_var),
                Stmt::Typedef(typedef) => self.resolve_typedef(typedef),
                Stmt::FuncDef(func_def) => self.resolve_func_def(func_def),
                Stmt::FuncDecl(func_decl) => self.resolve_func_decl(func_decl),
                Stmt::Struct(struct_) => self.resolve_struct_stmt(struct_),
                Stmt::Enum(enum_) => self.resolve_enum_stmt(enum_),
                Stmt::Union(union_) => self.resolve_union_stmt(union_),
                Stmt::Interface(interface) => self.resolve_interface_stmt(interface),

                // invalid top-level statements
                _ => {
                    let loc = SourceLoc::from_loc(stmt.loc(), file_path.clone());

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::InvalidTopLevelStatement),
                        location: Some(DiagLoc::new(loc)),
                        hint: None,
                    });

                    continue;
                }
            };

            if let Some(stmt) = typed_stmt {
                typed_body.push(stmt);
            }
        }

        typed_body
    }

    fn resolve_local_module_import(&mut self, module_import: &ModuleImport) -> Option<SymbolID> {
        let module_id = self.module_id.unwrap();

        if let Some(ident) = module_import.as_identifier() {
            if let Some(scope_rc) = scope_opt {
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
                location: Some(DiagLoc::new(SourceLoc::from_loc(ident.loc, self.current_file_path()))),
                hint: None,
            });
            return None;
        }

        self.resolve_module_import(module_import.clone())
            .map(|symbol_id| symbol_id)
    }

    fn resolve_ident(&mut self, ident: &Ident) -> Option<SymbolID> {
        let module_id = self.module_id.unwrap();

        if let Some(scope_rc) = &scope_opt {
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
            location: Some(DiagLoc::new(SourceLoc::from_loc(ident.loc, self.current_file_path()))),
            hint: None,
        });

        None
    }

    fn resolve_module_import(&mut self, mut module_import: ModuleImport) -> Option<SymbolID> {
        let module_id = self.module_id.unwrap();

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
                    location: Some(DiagLoc::new(SourceLoc::from_loc(ident.loc, self.current_file_path()))),
                    hint: None,
                });
            } else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::ExpectedIdentifierInImport),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        module_import.loc,
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
                    module_import.loc,
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
                    module_import.loc,
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
                    module_import.loc,
                    self.current_file_path(),
                ))),
                hint: None,
            });
            return None;
        };

        Some(symbol_id)
    }

    fn resolve_ident_expr(&mut self, ident: &Ident) -> Option<TypedExprStmt> {
        let symbol_id = self.resolve_ident(ident)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Symbol(TypedSymbolExpr::new(symbol_id, ident.loc)),
            sema_ty: None,
            mloc: MemoryLocation::LValue,
            loc,
        })
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Option<TypedExprStmt> {
        match expr {
            Expr::Ident(ident) => self.resolve_ident_expr(ident),
            Expr::Infix(infix_expr) => self.resolve_infix_expr(infix_expr),
            Expr::Prefix(prefix_expr) => self.resolve_prefix_expr(prefix_expr),
            Expr::Unary(unary) => self.resolve_unary_expr(unary),
            Expr::Assign(assignment) => self.resolve_assign_expr(assignment),
            Expr::FieldAccess(field_access) => self.resolve_field_access(field_access),
            Expr::MethodCall(method_call) => self.resolve_method_call(method_call),
            Expr::StructInit(struct_init) => self.resolve_struct_init(struct_init),
            Expr::ModuleImport(module_import) => self.resolve_module_import_expr(module_import),
            Expr::FuncCall(func_call) => self.resolve_func_call(func_call),
            Expr::Array(array) => self.resolve_array_expr(array),
            Expr::Literal(literal) => self.resolve_literal_expr(literal),
            Expr::ArrayIndex(array_index) => self.resolve_array_index_expr(array_index),
            Expr::AddrOf(address_of) => self.resolve_address_of_expr(address_of),
            Expr::Deref(dereference) => self.resolve_deref_expr(dereference),
            Expr::Lambda(lambda) => self.resolve_lambda_expr(lambda),
            Expr::Tuple(tuple_value) => self.resolve_tuple_expr(tuple_value),
            Expr::TupleAccess(tuple_member_access) => self.resolve_tuple_member_access(tuple_member_access),
            Expr::Dynamic(dynamic_expr) => self.resolve_dynamic_expr(dynamic_expr),
            Expr::UnnamedStructValue(unnamed_struct_value) => self.resolve_unnamed_struct_value(unnamed_struct_value),
            Expr::UnnamedEnumValue(unnamed_enum_value) => self.resolve_unnamed_enum_value(unnamed_enum_value),
            Expr::UnnamedUnionValue(unnamed_union_value) => self.resolve_unnamed_union_value(unnamed_union_value),
            Expr::UntypedArray(untyped_array) => self.resolve_untyped_array_expr(untyped_array),
            Expr::Builtin(builtin) => match self.resolve_builtin(builtin) {
                Some(typed_builtin) => {
                    let kind = TypedExprKind::Builtin(typed_builtin);

                    Some(TypedExprStmt {
                        kind,
                        sema_ty: None,
                        mloc: MemoryLocation::RValue,
                        loc: Loc::from_loc(builtin.loc(), self.current_file_path()),
                    })
                }
                None => None,
            },
            Expr::TypeSpecifier(type_specifier) => self.resolve_type_specifier_expr(type_specifier),
        }
    }

    // FIXME
    fn resolve_type(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        type_specifier: TypeSpecifier,
        loc: Loc,
    ) -> Option<SemanticType> {
        todo!();

        // let result = match &type_specifier {
        //     TypeSpecifier::GenericInst(generic_inst) => {
        //         let base_type = self.resolve_type(generic_params, *generic_inst.base.clone(), loc)?;

        //         let is_const = base_type.is_const();
        //         if let Some(symbol_id) = base_type.const_inner().as_unresolved_symbol() {
        //             let type_args = self.resolve_type_args(generic_params, &generic_inst.type_args, loc)?;

        //             Ok(SemanticType::GenericType(GenericType {
        //                 base: symbol_id,
        //                 type_args: Some(type_args),
        //                 mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
        //                 mapping_ctx_arena: self.mapping_ctx_arena.clone(),
        //                 generic_params: TypedGenericParamsList::new(),
        //                 is_const,
        //                 loc: Loc::from_loc(generic_inst.loc, self.current_file_path()),
        //             }))
        //         } else {
        //             Err(ResolverDiagKind::TypeDoesNotAcceptTypeArgs {
        //                 type_name: generic_inst.base.to_string(),
        //             })
        //         }
        //     }
        //     TypeSpecifier::Tuple(tuple_type) => {
        //         let type_list: Vec<SemanticType> = tuple_type
        //             .type_list
        //             .iter()
        //             .map(|elem| self.resolve_type(generic_params, elem.clone(), loc))
        //             .collect::<Option<Vec<_>>>()?;

        //         Ok(SemanticType::Tuple(TypedTupleType {
        //             type_list,
        //             loc: Loc::from_loc(tuple_type.loc, self.current_file_path()),
        //         }))
        //     }
        //     TypeSpecifier::FuncType(func_type) => {
        //         let params: Vec<SemanticType> = func_type
        //             .params
        //             .list
        //             .iter()
        //             .map(|param| self.resolve_type(generic_params, param.clone(), loc))
        //             .collect::<Option<Vec<_>>>()?;

        //         let variadic = match &func_type.params.variadic {
        //             Some(FuncTypeVariadicParams::UntypedCStyle) => {
        //                 Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle))
        //             }
        //             Some(FuncTypeVariadicParams::Typed(spec)) => {
        //                 let ct = self.resolve_type(generic_params, spec.clone(), loc)?;
        //                 Some(Box::new(TypedFuncTypeVariadicParams::Typed(ct)))
        //             }
        //             None => None,
        //         };

        //         let return_type = self.resolve_type(generic_params, *func_type.return_type.clone(), loc)?;

        //         Ok(SemanticType::FuncType(TypedFuncType {
        //             symbol_id: None,
        //             def_module_id: None,
        //             params: TypedFuncTypeParams { list: params, variadic },
        //             return_type: Box::new(return_type),
        //             is_public: true,
        //             loc: Loc::from_loc(loc, self.current_file_path()),
        //         }))
        //     }
        //     TypeSpecifier::TypeToken(token) => {
        //         SemanticType::try_from(token.kind.clone()).map_err(|_| ResolverDiagKind::TypeNotFound {
        //             name: token.kind.to_string(),
        //         })
        //     }
        //     TypeSpecifier::Const(inner) => {
        //         let inner = self.resolve_type(generic_params, scope_opt, *inner.clone(), loc)?;
        //         Ok(SemanticType::Const(Box::new(inner)))
        //     }
        //     TypeSpecifier::Deref(inner) => {
        //         let inner = self.resolve_type(generic_params, scope_opt, *inner.clone(), loc)?;
        //         Ok(SemanticType::Pointer(Box::new(inner)))
        //     }
        //     TypeSpecifier::Array(array_type) => {
        //         let element_type = self.resolve_type(generic_params, *array_type.element_type.clone(), loc)?;

        //         let capacity = match &array_type.size {
        //             ArrayCapacity::Fixed(expr) => {
        //                 let expr = self.resolve_expr(expr)?;

        //                 TypedArrayCapacity::Fixed(Box::new(expr))
        //             }
        //             ArrayCapacity::Dynamic => TypedArrayCapacity::Dynamic,
        //         };

        //         Ok(SemanticType::Array(TypedArrayType {
        //             element_type: Box::new(element_type),
        //             capacity,
        //             loc: Loc::from_loc(loc, self.current_file_path()),
        //         }))
        //     }
        //     TypeSpecifier::UnnamedUnion(unnamed_union_type) => {
        //         let mut fields = Vec::new();

        //         for field in &unnamed_union_type.fields {
        //             if let Some(ty) = self.resolve_type(generic_params, field.field_ty.clone(), field.loc) {
        //                 fields.push(TypedUnnamedUnionTypeField {
        //                     name: field.field_name.value.clone(),
        //                     ty: Box::new(ty),
        //                     loc: Loc::from_loc(field.loc, self.current_file_path()),
        //                 });
        //             }
        //         }

        //         Ok(SemanticType::UnnamedUnion(TypedUnnamedUnionType {
        //             fields,
        //             repr_attr: unnamed_union_type.repr_attr.clone(),
        //             align: unnamed_union_type.align.clone(),
        //             loc: Loc::from_loc(unnamed_union_type.loc, self.current_file_path()),
        //         }))
        //     }
        //     TypeSpecifier::UnnamedEnum(unnamed_enum_type) => {
        //         let mut variants = Vec::new();

        //         for variant in &unnamed_enum_type.variants {
        //             match variant {
        //                 UnnamedEnumVariant::Ident(ident) => {
        //                     variants.push(TypedUnnamedEnumVariant::Ident(ident.clone()));
        //                 }
        //                 UnnamedEnumVariant::Valued(ident, expr) => {
        //                     let typed_expr = match self.resolve_expr(expr) {
        //                         Some(expr) => expr,
        //                         None => continue,
        //                     };

        //                     variants.push(TypedUnnamedEnumVariant::Valued(ident.clone(), Box::new(typed_expr)));
        //                 }
        //                 UnnamedEnumVariant::Variant(ident, unnamed_enum_valued_fields) => {
        //                     let mut valued_fields = Vec::new();

        //                     for valued_field in unnamed_enum_valued_fields {
        //                         match self.resolve_type(&None, valued_field.ty.clone(), valued_field.loc) {
        //                             Some(ty) => {
        //                                 valued_fields.push(TypedUnnamedEnumValuedField {
        //                                     ty,
        //                                     loc: Loc::from_loc(valued_field.loc, self.current_file_path()),
        //                                 });
        //                             }
        //                             None => todo!(),
        //                         }
        //                     }

        //                     variants.push(TypedUnnamedEnumVariant::Variant(ident.clone(), valued_fields));
        //                 }
        //             }
        //         }

        //         let tag_type = if let Some(type_specifier) = &unnamed_enum_type.tag_type {
        //             Some(Box::new(self.resolve_type(
        //                 &None,
        //                 *type_specifier.clone(),
        //                 unnamed_enum_type.loc,
        //             )?))
        //         } else {
        //             None
        //         };

        //         Ok(SemanticType::UnnamedEnum(TypedUnnamedEnumType {
        //             variants,
        //             tag_type,
        //             repr_attr: unnamed_enum_type.repr_attr.clone(),
        //             align: unnamed_enum_type.align.clone(),
        //             loc: Loc::from_loc(unnamed_enum_type.loc, self.current_file_path()),
        //         }))
        //     }
        //     TypeSpecifier::UnnamedStruct(unnamed_struct_type) => {
        //         let mut fields = Vec::new();

        //         for field in &unnamed_struct_type.fields {
        //             if let Some(ty) = self.resolve_type(generic_params, field.field_ty.clone(), field.loc) {
        //                 fields.push(TypedUnnamedStructTypeField {
        //                     name: field.field_name.value.clone(),
        //                     ty: Box::new(ty),
        //                     loc: Loc::from_loc(field.loc, self.current_file_path()),
        //                 });
        //             }
        //         }

        //         Ok(SemanticType::UnnamedStruct(TypedUnnamedStructType {
        //             fields,
        //             repr_attr: unnamed_struct_type.repr_attr.clone(),
        //             align: unnamed_struct_type.align.clone(),
        //             loc: Loc::from_loc(unnamed_struct_type.loc, self.current_file_path()),
        //         }))
        //     }
        //     TypeSpecifier::ModuleImport(module_import) => self
        //         .resolve_module_import(module_import.clone())
        //         .map(SemanticType::UnresolvedSymbol)
        //         .ok_or(ResolverDiagKind::TypeNotFound {
        //             name: module_import.to_string(),
        //         }),
        //     TypeSpecifier::Ident(ident) => {
        //         let mut sema_ty_opt: Option<SemanticType> = self.resolve_generic_param_as_type(generic_params, ident);

        //         if sema_ty_opt.is_none() {
        //             sema_ty_opt = self.lookup_symbol_id(&ident.value).map(SemanticType::UnresolvedSymbol);
        //         }

        //         if sema_ty_opt.is_none() {
        //             sema_ty_opt = scope_opt
        //                 .and_then(|scope_rc| self.resolve_symbol_id_from_local_scope(scope_rc, &ident.value))
        //                 .map(SemanticType::UnresolvedSymbol);
        //         }

        //         if let Some(sema_ty) = sema_ty_opt {
        //             Ok(sema_ty)
        //         } else {
        //             Err(ResolverDiagKind::TypeNotFound {
        //                 name: ident.value.clone(),
        //             })
        //         }
        //     }
        //     TypeSpecifier::SelfType(self_type) => Ok(SemanticType::SelfType(TypedSelfType {
        //         loc: Loc::from_loc(self_type.loc, self.current_file_path()),
        //     })),
        // };

        // match result {
        //     Ok(ct) => Some(ct),
        //     Err(kind) => {
        //         self.reporter.report(Diag {
        //             level: DiagLevel::Error,
        //             kind: Box::new(kind),
        //             location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.current_file_path()))),
        //             hint: None,
        //         });
        //         None
        //     }
        // }
    }

    fn resolve_type_args(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        type_args: &TypeArgs,
        loc: Loc,
    ) -> Option<TypedTypeArgs> {
        type_args
            .iter()
            .enumerate()
            .map(|(i, type_arg)| match type_arg {
                TypeArg::Positional(type_specifier) => {
                    let loc = type_specifier.loc();
                    let ty = self.resolve_type(generic_params, type_specifier.clone(), loc)?;

                    Some(TypedTypeArg::Positional { i, ty, loc })
                }
                TypeArg::Named { key, ty } => {
                    let ty = self.resolve_type(generic_params, ty.clone(), loc)?;

                    Some(TypedTypeArg::Named {
                        key: key.as_string(),
                        ty,
                        loc: Loc::from_loc(key.loc, self.current_file_path()),
                    })
                }
            })
            .collect::<Option<_>>()
    }

    // FIXME
    fn resolve_generic_params(&mut self, generic_params: &GenericParamsList) -> Option<TypedGenericParamsList> {
        todo!();
        // let list = generic_params
        //     .iter()
        //     .map(|generic_param| {
        //         let bounds = if let Some(bounds_list) = &generic_param.bounds {
        //             let resolved_bounds = bounds_list
        //                 .iter()
        //                 .map(|bound| {
        //                     Some(TypedBound {
        //                         symbol: bound.symbol.clone(),
        //                         type_args: self.resolve_type_args(
        //                             &None,
        //                             self.module_id?,
        //                             None,
        //                             &bound.type_args,
        //                             generic_param.param_name.loc,
        //                             generic_param.param_name.span.end,
        //                         )?,
        //                     })
        //                 })
        //                 .collect::<Option<Vec<_>>>()?;

        //             Some(resolved_bounds)
        //         } else {
        //             None
        //         };

        //         let default = if let Some(default_type_specifier) = &generic_param.default {
        //             Some(Box::new(self.resolve_type(
        //                 &None,
        //                 None,
        //                 self.module_id?,
        //                 default_type_specifier.clone(),
        //                 generic_param.param_name.loc,
        //                 generic_param.param_name.span.end,
        //             )?))
        //         } else {
        //             None
        //         };

        //         Some(TypedGenericParam {
        //             param_name: TypedIdentifier {
        //                 name: generic_param.param_name.as_string(),
        //                 symbol_id: generate_symbol_id(),
        //                 loc: Loc::from_loc(generic_param.param_name.loc, self.current_file_path()),
        //             },
        //             bounds,
        //             default,
        //         })
        //     })
        //     .collect::<Option<Vec<_>>>()?;

        // Some(TypedGenericParamsList { list })
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

    fn resolve_typedef(&mut self, typedef: &Typedef) -> Option<TypedStmt> {
        let module_id = self.module_id.unwrap();
        let symbol_id = self
            .lookup_symbol_id(module_id, &typedef.ident.value)
            .unwrap_or_else(|| generate_symbol_id());

        let generic_params = typedef
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        let sema_ty = self.resolve_type(&generic_params, typedef.type_specifier.clone(), typedef.loc)?;

        let typedef_sig = TypedefSig {
            name: typedef.ident.value.clone(),
            generic_params,
            ty: sema_ty.clone(),
            vis: typedef.vis.clone(),
            loc: Loc::from_loc(typedef.loc, self.current_file_path()),
        };

        let resolved_typedef = ResolvedTypedef {
            symbol_id,
            typedef_sig: typedef_sig.clone(),
        };

        if let Some(scope_rc) = &scope_opt {
            scope_rc.borrow_mut().insert(
                typedef.ident.value.clone(),
                LocalSymbol::new(LocalSymbolKind::Typedef(resolved_typedef)),
            );
        } else {
            self.insert_symbol_entry(symbol_id, SymbolEntry::new(SymbolEntryKind::Typedef(resolved_typedef)));
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

    // ANCHOR: New feature
    fn resolve_builtin(&mut self, builtin: &Builtin) -> Option<TypedBuiltin> {
        // FIXME
        // match builtin {
        //     Builtin::BuiltinFunc(builtin_func) => self.resolve_builtin_func(scope_id, scope, builtin_func),
        //     Builtin::BuiltinScope(builtin_scope) => {
        //         // self.resolve_builtin_scope( scope, builtin_scope)
        //         todo!();
        //     }
        // }
        todo!()
    }

    // ANCHOR: New feature
    fn resolve_builtin_func(&mut self, builtin_func: &BuiltinFunc) -> Option<TypedBuiltin> {
        // let args: Vec<TypedExprStmt> = builtin_func
        //     .args
        //     .iter()
        //     .filter_map(|arg| self.resolve_expr(Some(scope_opt.clone()), arg))
        //     .collect();

        // let child_stmt = builtin_func
        //     .child_stmt
        //     .clone()
        //     .and_then(|stmt| self.resolve_stmt(scope_id, scope_opt, &stmt))
        //     .map(Box::new);

        // let builtin_func = TypedBuiltinFunc {
        //     name: builtin_func.name.clone(),
        //     args,
        //     child_stmt,
        //     loc: Loc::from_loc(builtin_func.loc, self.current_file_path()),
        // };

        // TypedBuiltin::BuiltinFunc(())
        todo!();
    }

    fn resolve_builtin_scope(&self, builtin_scope: &BuiltinScope) -> Option<TypedBuiltin> {
        todo!();
    }

    fn resolve_interface_stmt(&mut self, interface: &Interface) -> Option<TypedStmt> {
        let module_id = self.module_id.unwrap();
        let symbol_id = scope_opt
            .as_ref()
            .map(|_| generate_symbol_id())
            .unwrap_or_else(|| self.lookup_symbol_id(module_id, &interface.ident.value).unwrap());

        let generic_params = interface
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        self.current_object_generic_params = generic_params.clone();

        let typed_methods: Vec<TypedFuncDeclStmt> = interface
            .methods
            .iter()
            .filter_map(|func_decl| {
                if func_decl.renamed_as.is_some() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::RenameInterfaceMethod),
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            func_decl.loc,
                            self.current_file_path(),
                        ))),
                        hint: None,
                    });
                }

                let (return_type, typed_func_params, typed_variadic_param, generic_params) =
                    self.resolve_func(func_decl)?;

                Some(TypedFuncDeclStmt {
                    module_id: self.module_id.unwrap(),
                    symbol_id,
                    name: func_decl.ident.value.clone(),
                    generic_params,
                    params: TypedFuncParams {
                        list: typed_func_params,
                        variadic: typed_variadic_param,
                    },
                    return_type,
                    modifiers: func_decl.modifiers.clone(),
                    renamed_as: None,
                    loc: Loc::from_loc(func_decl.loc, self.current_file_path()),
                })
            })
            .collect();

        let resolved_interface = ResolvedInterface {
            symbol_id,
            interface_sig: InterfaceSig {
                symbol_id,
                name: interface.ident.value.clone(),
                methods: typed_methods.clone(),
                generic_params: generic_params.clone(),
                vis: interface.vis.clone(),
                loc: Loc::from_loc(interface.loc, self.current_file_path()),
            },
        };

        match scope_opt {
            Some(scope_rc) => {
                scope_rc.borrow_mut().insert(
                    interface.ident.value.clone(),
                    LocalSymbol::new(LocalSymbolKind::Interface(resolved_interface)),
                );
            }
            None => {
                self.insert_symbol_entry(
                    symbol_id,
                    SymbolEntry::new(SymbolEntryKind::Interface(resolved_interface)),
                );
            }
        }

        Some(TypedStmt::Interface(TypedInterfaceStmt {
            name: interface.ident.value.clone(),
            symbol_id,
            methods: typed_methods,
            generic_params,
            vis: interface.vis.clone(),
            loc: Loc::from_loc(interface.loc, self.current_file_path()),
        }))
    }

    fn resolve_union_stmt(&mut self, union_decl: &Union) -> Option<TypedStmt> {
        let module_id = self.module_id.unwrap();
        let symbol_id = scope_opt
            .as_ref()
            .map(|_| generate_symbol_id())
            .unwrap_or_else(|| self.lookup_symbol_id(module_id, &union_decl.ident.value).unwrap());

        self.current_object = Some(symbol_id);

        let mut typed_union_fields: Vec<TypedUnionField> = Vec::new();

        let generic_params = union_decl
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        self.current_object_generic_params = generic_params.clone();

        for field in &union_decl.fields {
            match self.resolve_type(&generic_params, field.ty.clone(), field.loc) {
                Some(sema_ty) => {
                    typed_union_fields.push(TypedUnionField {
                        name: field.ident.value.clone(),
                        ty: sema_ty,
                        loc: Loc::from_loc(field.loc, self.current_file_path()),
                    });
                }
                None => continue,
            }
        }

        self.check_duplicate_method_names(&union_decl.ident.value, union_decl.methods.clone());

        let methods = match self.resolve_methods(&union_decl.methods, symbol_id, generic_params.is_some()) {
            Some(methods) => methods,
            None => return None,
        };

        let resolved_union = ResolvedUnion {
            symbol_id,
            union_sig: UnionSig {
                symbol_id,
                name: union_decl.ident.value.clone(),
                fields: typed_union_fields.clone(),
                methods: methods.clone(),
                generic_params: generic_params.clone(),
                modifiers: union_decl.modifiers.clone(),
                align: union_decl.align.clone(),
                loc: Loc::from_loc(union_decl.loc, self.current_file_path()),
            },
        };

        if let Some(scope_rc) = &scope_opt {
            let mut scope = scope_rc.borrow_mut();
            scope.insert(
                union_decl.ident.value.clone(),
                LocalSymbol::new(LocalSymbolKind::Union(resolved_union)),
            );
            drop(scope);
        } else {
            self.insert_symbol_entry(symbol_id, SymbolEntry::new(SymbolEntryKind::Union(resolved_union)));
        }

        let impls = self.resolve_object_impls(&union_decl.impls, union_decl.loc);

        Some(TypedStmt::Union(TypedUnionStmt {
            symbol_id,
            module_id,
            name: union_decl.ident.value.clone(),
            fields: typed_union_fields,
            methods,
            generic_params,
            modifiers: union_decl.modifiers.clone(),
            impls,
            loc: Loc::from_loc(union_decl.ident.loc, self.current_file_path()),
            align: union_decl.align.clone(),
        }))
    }

    fn resolve_enum_stmt(&mut self, enum_decl: &Enum) -> Option<TypedStmt> {
        let module_id = self.module_id.unwrap();
        let symbol_id = scope_opt
            .as_ref()
            .map(|_| generate_symbol_id())
            .unwrap_or_else(|| self.lookup_symbol_id(module_id, &enum_decl.ident.value).unwrap());

        self.current_object = Some(symbol_id);

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
                        let ty = match self.resolve_type(&generic_params, valued_field.ty.clone(), valued_field.loc) {
                            Some(sema_ty) => sema_ty,
                            None => continue,
                        };

                        fields.push(TypedEnumValuedField {
                            ty,
                            loc: Loc::from_loc(valued_field.loc, self.current_file_path()),
                        });
                    }
                    TypedEnumVariant::Variant(ident.clone(), fields)
                }
                EnumVariant::Valued(ident, expr) => match self.resolve_expr(expr) {
                    Some(typed_expr) => TypedEnumVariant::Valued(ident.clone(), Box::new(typed_expr)),
                    None => continue,
                },
            };

            variants.push(typed_variant);
        }

        self.check_duplicate_method_names(&enum_decl.ident.value, enum_decl.methods.clone());

        let methods = match self.resolve_methods(&enum_decl.methods, symbol_id, generic_params.is_some()) {
            Some(methods) => methods,
            None => return None,
        };

        let tag_type = if let Some(type_specifier) = enum_decl.tag_type.clone() {
            self.resolve_type(&None, type_specifier, enum_decl.loc)
        } else {
            None
        };

        let resolved_enum = ResolvedEnum {
            symbol_id,
            enum_sig: EnumSig {
                symbol_id,
                name: enum_decl.ident.value.clone(),
                methods: methods.clone(),
                variants: variants.clone(),
                generic_params: generic_params.clone(),
                tag_type,
                modifiers: enum_decl.modifiers.clone(),
                align: enum_decl.align.clone(),
                loc: enum_decl.loc,
            },
        };

        if let Some(scope_rc) = &scope_opt {
            let mut scope = scope_rc.borrow_mut();
            scope.insert(
                enum_decl.ident.value.clone(),
                LocalSymbol::new(LocalSymbolKind::Enum(resolved_enum)),
            );
            drop(scope);
        } else {
            self.insert_symbol_entry(symbol_id, SymbolEntry::new(SymbolEntryKind::Enum(resolved_enum)));
        }

        let impls = self.resolve_object_impls(&enum_decl.impls, enum_decl.loc);

        let tag_type = if let Some(type_specifier) = &enum_decl.tag_type {
            self.resolve_type(&None, type_specifier.clone(), enum_decl.loc)
        } else {
            None
        };

        Some(TypedStmt::Enum(TypedEnumStmt {
            symbol_id,
            module_id,
            name: enum_decl.ident.value.clone(),
            variants,
            methods,
            generic_params,
            impls,
            tag_type,
            modifiers: enum_decl.modifiers.clone(),
            align: enum_decl.align.clone(),
            loc: enum_decl.loc,
        }))
    }

    fn resolve_global_var_stmt(&mut self, global_var: &GlobalVar) -> Option<TypedStmt> {
        let sema_ty = global_var
            .type_specifier
            .clone()
            .and_then(|ty| self.resolve_type(&None, ty, global_var.loc));

        let typed_expr = global_var.expr.as_ref().and_then(|expr| self.resolve_expr(expr));

        let module_id = self.module_id.unwrap();
        let symbol_id = self.lookup_symbol_id(module_id, &global_var.ident.value).unwrap();

        let resolved_global_var = ResolvedGlobalVar {
            symbol_id,
            global_var_sig: GlobalVarSig {
                symbol_id,
                name: global_var.ident.value.clone(),
                ty: sema_ty.clone(),
                rhs: typed_expr.clone(),
                analyzed: true,
                is_const: global_var.is_const,
                modifiers: global_var.modifiers.clone(),
                loc: Loc::from_loc(global_var.loc, self.current_file_path()),
            },
        };

        self.insert_symbol_entry(
            symbol_id,
            SymbolEntry::new(SymbolEntryKind::GlobalVar(resolved_global_var)),
        );

        Some(TypedStmt::GlobalVar(TypedGlobalVarStmt {
            symbol_id,
            module_id,
            name: global_var.ident.value.clone(),
            ty: sema_ty,
            expr: typed_expr,
            modifiers: global_var.modifiers.clone(),
            is_const: global_var.is_const,
            loc: Loc::from_loc(global_var.loc, self.current_file_path()),
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
                        func_def.loc,
                        self.current_file_path(),
                    ))),
                    hint: Some("Consider to rename the method to a different name.".to_string()),
                });
                continue;
            }

            method_names.push(method_name);
        }
    }

    // FIXME
    fn resolve_methods(
        &mut self,
        methods_list: &[FuncDef],
        symbol_id: SymbolID,
        generic_object: bool,
    ) -> Option<HashMap<String, SymbolID>> {
        todo!();

        // let mut methods: HashMap<String, SymbolID> = HashMap::new();
        // let mut method_bodies: HashMap<SymbolID, (LocalScopeRef, Box<BlockStmt>, ScopeID, bool)> = HashMap::new();

        // for func_def in methods_list {
        //     let method_scope_id = generate_scope_id();
        //     let scope_rc = LocalScope::new(None);
        //     self.insert_scope_ref(method_scope_id, Rc::clone(&scope_rc));

        //     if let Some((return_type, mut typed_func_params, typed_variadic_param, generic_params)) =
        //         self.resolve_func(&func_def.as_func_decl())
        //     {
        //         let original_method_name = func_def.ident.value.clone();
        //         let unique_method_name = method_symbol_name_for_struct(symbol_id, original_method_name.clone());

        //         typed_func_params = typed_func_params
        //             .into_iter()
        //             .map(|param_kind| match param_kind {
        //                 TypedFuncParamKind::FuncParam(p) => TypedFuncParamKind::FuncParam(p),
        //                 TypedFuncParamKind::SelfModifier(mut s) => {
        //                     s.symbol_id = Some(symbol_id);
        //                     TypedFuncParamKind::SelfModifier(s)
        //                 }
        //             })
        //             .collect();

        //         let symbol_id = self.insert_symbol_name(&unique_method_name);

        //         methods.insert(original_method_name.clone(), symbol_id);

        //         let func_sig = FuncSig {
        //             symbol_id: Some(symbol_id),
        //             name: original_method_name.clone(),
        //             is_func_decl: false,
        //             generic_params,
        //             params: TypedFuncParams {
        //                 list: typed_func_params,
        //                 variadic: typed_variadic_param,
        //             },
        //             return_type,
        //             modifiers: func_def.modifiers.clone(),
        //             loc: Loc::from_loc(func_def.loc, self.current_file_path()),
        //         };

        //         let is_generic = func_sig.generic_params.is_some() || generic_object;

        //         let resolved_method = ResolvedMethod {
        //             symbol_id,
        //             func_sig,
        //             func_body: None,
        //         };

        //         self.insert_symbol_entry(symbol_id, SymbolEntry::new(SymbolEntryKind::Method(resolved_method)));

        //         method_bodies.insert(
        //             symbol_id,
        //             (Rc::clone(&scope_rc), func_def.body.clone(), method_scope_id, is_generic),
        //         );
        //     }
        // }

        // for (&symbol_id, (scope_rc, method_body, method_scope_id, is_generic)) in &method_bodies {
        //     let mut resolved_method = match self.lookup_symbol_entry_with_id(symbol_id).unwrap().kind {
        //         SymbolEntryKind::Method(m) => m,
        //         _ => unreachable!(),
        //     };

        //     for param in &mut resolved_method.func_sig.params.list {
        //         if let TypedFuncParamKind::SelfModifier(self_modifier) = param {
        //             let self_symbol_id = generate_symbol_id();
        //             self_modifier.self_symbol_id = Some(self_symbol_id);

        //             let local_symbol = LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
        //                 symbol_id: self_symbol_id,
        //                 typed_variable: TypedVarStmt {
        //                     symbol_id: self_symbol_id,
        //                     name: "self".to_string(),
        //                     ty: match self_modifier.kind {
        //                         SelfModifierKind::Copied => Some(SemanticType::UnresolvedSymbol(symbol_id)),
        //                         SelfModifierKind::Referenced => Some(SemanticType::Pointer(Box::new(
        //                             SemanticType::UnresolvedSymbol(symbol_id),
        //                         ))),
        //                     },
        //                     rhs: None,
        //                     is_const: false,
        //                     analyzed: true,
        //                     loc: resolved_method.func_sig.loc,
        //                 },
        //             }));

        //             scope_rc.borrow_mut().insert("self".to_string(), local_symbol);
        //         }
        //     }

        //     if let Some(typed_func_body) = self.resolve_block_stmt(*method_scope_id, scope_rc.clone(), method_body) {
        //         if *is_generic {
        //             // generic method body is being analyzed when called
        //             monomorph_registry!(self, ctx, {
        //                 ctx.register_template(symbol_id, typed_func_body);
        //             });
        //         } else {
        //             resolved_method.func_body = Some(Box::new(typed_func_body));
        //         }

        //         self.insert_symbol_entry(symbol_id, SymbolEntry::new(SymbolEntryKind::Method(resolved_method)));
        //     } else {
        //         return None;
        //     }
        // }

        // Some(methods)
    }

    fn resolve_object_impls(&mut self, impls: &Vec<TypeSpecifier>, loc: Location) -> Vec<TypedImplementInterface> {
        let mut symbol_ids: Vec<TypedImplementInterface> = Vec::new();

        for type_specifier in impls {
            let (symbol_id, type_args) = match type_specifier {
                TypeSpecifier::ModuleImport(module_import) => {
                    let Some(symbol_id) = self.resolve_local_module_import(module_import) else {
                        continue;
                    };

                    (symbol_id, None)
                }
                TypeSpecifier::Ident(ident) => {
                    let Some(symbol_id) = self.resolve_ident(ident) else {
                        continue;
                    };

                    (symbol_id, None)
                }
                TypeSpecifier::GenericInst(generic_inst) => {
                    let (loc) = type_specifier.loc();

                    let base_symbol_id = match self
                        .resolve_type(&None, *generic_inst.base.clone(), loc)
                        .and_then(|sema_ty| sema_ty.as_unresolved_symbol())
                    {
                        Some(symbol_id) => symbol_id,
                        None => {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(ResolverDiagKind::InvalidImplementInterface),
                                location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.current_file_path()))),
                                hint: None,
                            });
                            return symbol_ids;
                        }
                    };

                    let type_args = self.resolve_type_args(&None, &generic_inst.type_args, loc);

                    (base_symbol_id, type_args)
                }
                _ => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::InvalidImplementInterface),
                        location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.current_file_path()))),
                        hint: None,
                    });
                    return symbol_ids;
                }
            };

            symbol_ids.push(TypedImplementInterface {
                symbol_id,
                type_args,
                loc: Loc::from_loc(
                    type_specifier.loc().0,
                    self.resolve_module_file_path(module_id).unwrap(),
                ),
            });
        }

        symbol_ids
    }

    fn resolve_struct_stmt(&mut self, struct_decl: &Struct) -> Option<TypedStmt> {
        let module_id = self.module_id.unwrap();
        let symbol_id = scope_opt
            .as_ref()
            .map(|_| generate_symbol_id())
            .unwrap_or_else(|| self.lookup_symbol_id(module_id, &struct_decl.ident.value).unwrap());

        self.current_object = Some(symbol_id);

        let generic_params = struct_decl
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        self.current_object_generic_params = generic_params.clone();

        let typed_struct_fields: Vec<TypedStructField> = struct_decl
            .fields
            .iter()
            .filter_map(|field| {
                self.resolve_type(&generic_params, field.ty.clone(), field.loc)
                    .map(|ty| TypedStructField {
                        name: field.ident.value.clone(),
                        vis: field.vis.clone(),
                        ty,
                        loc: Loc::from_loc(field.loc, self.current_file_path()),
                    })
            })
            .collect();

        self.check_duplicate_method_names(&struct_decl.ident.value, struct_decl.methods.clone());

        let methods = self.resolve_methods(&struct_decl.methods, symbol_id, generic_params.is_some())?;
        let impls = self.resolve_object_impls(&struct_decl.impls, struct_decl.loc);

        let resolved_struct = ResolvedStruct {
            symbol_id,
            struct_sig: StructSig {
                name: struct_decl.ident.value.clone(),
                fields: typed_struct_fields.clone(),
                generic_params,
                impls: impls.clone(),
                methods: methods.clone(),
                modifiers: struct_decl.modifiers.clone(),
                align: struct_decl.align.clone(),
                loc: Loc::from_loc(struct_decl.loc, self.current_file_path()),
            },
        };

        if let Some(scope_rc) = scope_opt {
            scope_rc.borrow_mut().insert(
                struct_decl.ident.value.clone(),
                LocalSymbol::new(LocalSymbolKind::Struct(resolved_struct)),
            );
        } else {
            self.insert_symbol_entry(symbol_id, SymbolEntry::new(SymbolEntryKind::Struct(resolved_struct)));
        }

        Some(TypedStmt::Struct(TypedStructStmt {
            module_id,
            symbol_id,
            name: struct_decl.ident.value.clone(),
            fields: typed_struct_fields,
            methods,
            generic_params,
            impls,
            modifiers: struct_decl.modifiers.clone(),
            align: struct_decl.align.clone(),
            is_packed: struct_decl.is_packed,
            loc: Loc::from_loc(struct_decl.loc, self.current_file_path()),
        }))
    }

    fn resolve_func(
        &mut self,
        func_decl: &FuncDecl,
    ) -> Option<(
        SemanticType,
        Vec<TypedFuncParamKind>,
        Option<TypedFuncVariadicParams>,
        Option<TypedGenericParamsList>,
    )> {
        let return_type = return_type_or_default_void(func_decl.return_type.clone(), func_decl.loc);

        let generic_params = func_decl
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        let typed_return_type = self.resolve_type(&generic_params, return_type, func_decl.loc)?;

        let (typed_func_params, typed_variadic_param) = self.resolve_func_params(&generic_params, &func_decl.params)?;

        Some((
            typed_return_type,
            typed_func_params,
            typed_variadic_param,
            generic_params,
        ))
    }

    // FIXME
    fn resolve_func_params(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        params: &FuncParams,
    ) -> Option<(Vec<TypedFuncParamKind>, Option<TypedFuncVariadicParams>)> {
        todo!();
        // let mut func_params = Vec::with_capacity(params.list.len());

        // for param in &params.list {
        //     match param {
        //         FuncParamKind::FuncParam(func_param) => {
        //             let param_type = match &func_param.ty {
        //                 Some(type_specifier) => self.resolve_type(
        //                     generic_params,
        //                     type_specifier.clone(),
        //                     func_param.loc,
        //                 )?,
        //                 None => {
        //                     self.reporter.report(Diag {
        //                         level: DiagLevel::Error,
        //                         kind: Box::new(ResolverDiagKind::InvalidUntypedFuncParam),
        //                         location: Some(DiagLoc::new(SourceLoc::from_loc(
        //                             func_param.loc,
        //                             self.current_file_path(),
        //                         ))),
        //                         hint: None,
        //                     });
        //                     continue;
        //                 }
        //             };

        //             let symbol_id = generate_symbol_id();
        //             if let Some(scope_rc) = &scope_opt {
        //                 let mut scope = scope_rc.borrow_mut();
        //                 scope.insert(
        //                     func_param.ident.value.clone(),
        //                     LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
        //                         symbol_id,
        //                         typed_variable: TypedVarStmt {
        //                             symbol_id,
        //                             name: func_param.ident.value.clone(),
        //                             ty: Some(param_type.clone()),
        //                             rhs: None,
        //                             is_const: false,
        //                             analyzed: true,
        //                             loc: Loc::from_loc(func_param.loc, self.current_file_path()),
        //                         },
        //                     })),
        //                 );
        //             }

        //             func_params.push(TypedFuncParamKind::FuncParam(TypedFuncParam {
        //                 symbol_id,
        //                 name: func_param.ident.value.clone(),
        //                 ty: param_type,
        //                 loc: Loc::from_loc(func_param.loc, self.current_file_path()),
        //             }));
        //         }
        //         FuncParamKind::SelfModifier(self_modifier) => {
        //             func_params.push(TypedFuncParamKind::SelfModifier(TypedSelfModifier {
        //                 symbol_id: None,
        //                 self_symbol_id: None,
        //                 ty: None,
        //                 kind: self_modifier.kind.clone(),
        //                 loc: Loc::from_loc(self_modifier.loc, self.current_file_path()),
        //             }));
        //         }
        //     }
        // }

        // let variadic_param = params.variadic.as_ref().and_then(|variadic| match variadic {
        //     FuncVariadicParams::UntypedCStyle => Some(TypedFuncVariadicParams::UntypedCStyle),
        //     FuncVariadicParams::Typed(ident, type_specifier) => {
        //         let variadic_type = self.resolve_type(
        //             &None,

        //             type_specifier.clone(),
        //             ident.loc,
        //             ident.span.end,
        //         )?;

        //         let symbol_id = generate_symbol_id();

        //         if let Some(scope_rc) = &scope_opt {
        //             let mut scope = scope_rc.borrow_mut();
        //             scope.insert(
        //                 ident.value.clone(),
        //                 LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
        //                     symbol_id,
        //                     typed_variable: TypedVarStmt {
        //                         symbol_id,
        //                         name: ident.value.clone(),
        //                         ty: Some(variadic_type.clone()),
        //                         rhs: None,
        //                         is_const: false,
        //                         analyzed: true,
        //                         loc: Loc::from_loc(ident.loc, self.current_file_path()),
        //                     },
        //                 })),
        //             );
        //         }

        //         Some(TypedFuncVariadicParams::Typed(
        //             TypedIdentifier {
        //                 name: ident.as_string(),
        //                 symbol_id,
        //                 loc: Loc::from_loc(ident.loc, self.current_file_path()),
        //             },
        //             variadic_type,
        //         ))
        //     }
        // });

        // Some((func_params, variadic_param))
    }

    // FIXME
    fn resolve_func_decl(&mut self, func_decl: &FuncDecl) -> Option<TypedStmt> {
        todo!();

        // let symbol_id = self.lookup_symbol_id(&func_decl.usable_name()).unwrap();

        // let (return_type, typed_func_params, typed_variadic_param, generic_params) =
        //     self.resolve_func(None, func_decl)?;

        // let func_sig = FuncSig {
        //     symbol_id: Some(symbol_id),

        //     name: func_decl.ident.value.clone(),
        //     is_func_decl: true,
        //     generic_params: generic_params.clone(),
        //     params: TypedFuncParams {
        //         list: typed_func_params.clone(),
        //         variadic: typed_variadic_param.clone(),
        //     },
        //     return_type: return_type.clone(),
        //     modifiers: func_decl.modifiers.clone(),
        //     loc: Loc::from_loc(func_decl.loc, self.current_file_path()),
        // };

        // self.insert_symbol_entry(
        //     symbol_id,
        //     SymbolEntry::new(SymbolEntryKind::Func(ResolvedFunction { symbol_id, func_sig })),
        // );

        // Some(TypedStmt::FuncDecl(TypedFuncDeclStmt {
        //     symbol_id,
        //     name: func_decl.ident.value.clone(),
        //     generic_params,
        //     params: TypedFuncParams {
        //         list: typed_func_params,
        //         variadic: typed_variadic_param,
        //     },
        //     return_type,
        //     modifiers: func_decl.modifiers.clone(),
        //     renamed_as: func_decl.renamed_as.as_ref().map(|id| id.as_string()),
        //     loc: Loc::from_loc(func_decl.loc, self.current_file_path()),
        // }))
    }

    // FIXME
    fn resolve_func_def(&mut self, func_def: &FuncDef) -> Option<TypedStmt> {
        todo!();

        // let scope_id = generate_scope_id();
        // let body_scope = LocalScope::new(None);
        // self.insert_scope_ref(scope_id, body_scope.clone());

        // let symbol_id = self.lookup_symbol_id(&func_def.ident.value)?;

        // let (return_type, typed_func_params, typed_variadic_param, generic_params) =
        //     self.resolve_func(Some(body_scope.clone()), &func_def.as_func_decl())?;

        // let func_sig = FuncSig {
        //     symbol_id: Some(symbol_id),

        //     name: func_def.ident.value.clone(),
        //     generic_params: generic_params.clone(),
        //     is_func_decl: false,
        //     params: TypedFuncParams {
        //         list: typed_func_params.clone(),
        //         variadic: typed_variadic_param.clone(),
        //     },
        //     return_type: return_type.clone(),
        //     modifiers: func_def.modifiers.clone(),
        //     loc: Loc::from_loc(func_def.loc, self.current_file_path()),
        // };

        // self.insert_symbol_entry(
        //     symbol_id,
        //     SymbolEntry::new(SymbolEntryKind::Func(ResolvedFunction { symbol_id, func_sig })),
        // );

        // let typed_func_body = self.resolve_block_stmt(scope_id, body_scope, &func_def.body)?;

        // monomorph_registry!(self, ctx, {
        //     ctx.register_template(symbol_id, typed_func_body.clone());
        // });

        // Some(TypedStmt::FuncDef(TypedFuncDefStmt {
        //     symbol_id,

        //     name: func_def.ident.value.clone(),
        //     generic_params,
        //     params: TypedFuncParams {
        //         list: typed_func_params,
        //         variadic: typed_variadic_param,
        //     },
        //     return_type,
        //     modifiers: func_def.modifiers.clone(),
        //     loc: Loc::from_loc(func_def.loc, self.current_file_path()),
        //     body: Box::new(typed_func_body),
        // }))
    }

    // FIXME
    fn resolve_if_stmt(&mut self, if_stmt: &If) -> Option<TypedIfStmt> {
        todo!();

        // let cond = match self.resolve_expr(Some(Rc::clone(&scope)), &if_stmt.condition) {
        //     Some(typed_expr) => typed_expr,
        //     None => return None,
        // };

        // let then_block_scope_id = generate_scope_id();
        // let then_block_scope = LocalScope::new(Some(scope.clone()));
        // self.insert_scope_ref(then_block_scope_id, then_block_scope.clone());

        // let then_block = match self.resolve_block_stmt(then_block_scope_id, then_block_scope, &if_stmt.consequent) {
        //     Some(typed_block) => Box::new(typed_block),
        //     None => return None,
        // };

        // let else_block = {
        //     if let Some(alternate) = &if_stmt.alternate {
        //         let else_block_scope_id = generate_scope_id();
        //         let else_block_scope = LocalScope::new(Some(scope.clone()));
        //         self.insert_scope_ref(else_block_scope_id, else_block_scope.clone());

        //         match self.resolve_block_stmt(else_block_scope_id, else_block_scope.clone(), &*alternate) {
        //             Some(typed_block) => Some(Box::new(typed_block)),
        //             None => return None,
        //         }
        //     } else {
        //         None
        //     }
        // };

        // let mut branches: Vec<TypedIfStmt> = Vec::new();

        // for else_if_stmt in &if_stmt.branches {
        //     let if_stmt = match self.resolve_if_stmt(scope.clone(), else_if_stmt) {
        //         Some(typed_if) => typed_if,
        //         None => continue,
        //     };

        //     branches.push(if_stmt);
        // }

        // Some(TypedIfStmt {
        //     cond,
        //     else_block,
        //     then_block,
        //     branches,
        //     loc: Loc::from_loc(if_stmt.loc, self.current_file_path()),
        // })
    }

    // FIXME
    fn resolve_block_stmt(&mut self, block_statement: &BlockStmt) -> Option<TypedBlockStmt> {
        todo!();

        // let module_id = self.module_id.unwrap();
        // let mut typed_body: Vec<TypedStmt> = Vec::new();
        // let mut defers: Vec<TypedDeferStmt> = Vec::new();

        // for stmt in &block_statement.exprs {
        //     match stmt {
        //         Stmt::Defer(defer) => {
        //             if let Some(typed_stmt) = self.resolve_stmt(scope_id, scope.clone(), &defer.operand) {
        //                 defers.push(TypedDeferStmt {
        //                     operand: Box::new(typed_stmt),
        //                     loc: Loc::from_loc(defer.loc, self.current_file_path()),
        //                 });
        //             }
        //         }
        //         _ => {
        //             if let Some(typed_stmt) = self.resolve_stmt(scope_id, scope.clone(), stmt) {
        //                 typed_body.push(typed_stmt);
        //             }
        //         }
        //     }
        // }

        // Some(TypedBlockStmt {
        //     scope_id,
        //     stmts: typed_body,
        //     defers,
        //     loc: Loc::from_loc(block_statement.loc, self.current_file_path()),
        // })
    }

    // FIXME
    fn resolve_export_tuple(&mut self, export_tuple: &ExportTuple) -> Option<TypedStmt> {
        todo!();

        // let var_type = export_tuple.ty.as_ref().and_then(|ty_spec| {
        //     self.resolve_type(
        //         &None,
        //         Some(scope.clone()),
        //         ty_spec.clone(),
        //         export_tuple.loc,
        //         export_tuple.span.end,
        //     )
        // });

        // let typed_rhs = export_tuple
        //     .rhs
        //     .as_ref()
        //     .and_then(|expr| self.resolve_expr(Some(scope.clone()), expr));

        // let define_identifier = |this: &mut Resolver, ident: &Ident| -> Option<SymbolID> {
        //     let symbol_id = generate_symbol_id();

        //     let mut scope_ref = scope.borrow_mut();

        //     if scope_ref.resolve(&ident.value).is_some() {
        //         this.reporter.report(Diag {
        //             level: DiagLevel::Error,
        //             kind: Box::new(ResolverDiagKind::DuplicateSymbolInThisScope {
        //                 symbol_name: ident.value.clone(),
        //             }),
        //             location: Some(DiagLoc::new(SourceLoc::from_loc(
        //                 ident.loc,
        //                 this.current_file_path(),
        //             ))),
        //             hint: None,
        //         });
        //         return None;
        //     }

        //     let typed_variable = TypedVarStmt {
        //         symbol_id,
        //         name: ident.as_string(),
        //         ty: None,
        //         rhs: None,
        //         is_const: false,
        //         analyzed: typed_rhs.is_some(),
        //         loc: Loc::from_loc(ident.loc, this.current_file_path()),
        //     };

        //     let resolved_var = ResolvedVariable {
        //         symbol_id,
        //         typed_variable: typed_variable.clone(),
        //     };

        //     scope_ref.insert(
        //         ident.as_string(),
        //         LocalSymbol::new(LocalSymbolKind::Variable(resolved_var)),
        //     );

        //     drop(scope_ref);
        //     Some(symbol_id)
        // };

        // let pattern = match &export_tuple.pattern {
        //     ExportPattern::Ident(ident) => {
        //         let symbol_id = define_identifier(self, ident)?;
        //         TypedExportPattern::Ident(symbol_id)
        //     }
        //     ExportPattern::Tuple(patterns) => {
        //         let mut typed_patterns = Vec::new();

        //         for sub_pattern in patterns {
        //             match sub_pattern {
        //                 ExportPattern::Ident(ident) => {
        //                     let symbol_id = define_identifier(self, ident)?;
        //                     typed_patterns.push(TypedExportPattern::Ident(symbol_id));
        //                 }
        //                 ExportPattern::Tuple(inner) => {
        //                     let inner_export = ExportPattern::Tuple(inner.clone());
        //                     let inner_stmt = self.resolve_export_tuple(
        //                         scope_id,
        //                         scope.clone(),
        //                         &ExportTuple {
        //                             pattern: inner_export,
        //                             ty: None,
        //                             rhs: None,
        //                             is_const: export_tuple.is_const,
        //                             loc: export_tuple.loc,
        //                             span: export_tuple.span,
        //                         },
        //                     )?;

        //                     if let TypedStmt::ExportTuple(inner_typed) = inner_stmt {
        //                         typed_patterns
        //                             .push(TypedExportPattern::Tuple(inner_typed.pattern.into_tuple().to_vec()));
        //                     }
        //                 }
        //             }
        //         }

        //         TypedExportPattern::Tuple(typed_patterns)
        //     }
        // };

        // Some(TypedStmt::ExportTuple(TypedExportTupleStmt {
        //     pattern,
        //     ty: var_type,
        //     rhs: typed_rhs,
        //     is_const: export_tuple.is_const,
        //     loc: Loc::from_loc(export_tuple.loc, self.current_file_path()),
        // }))
    }

    // FIXME
    fn resolve_stmt(&mut self, stmt: &Stmt) -> Option<TypedStmt> {
        todo!();

        // let typed_stmt = match stmt {
        //     Stmt::ExportTuple(export_tuple) => self.resolve_export_tuple(scope_id, scope, export_tuple),
        //     Stmt::Variable(variable) => {
        //         let typed_var = self.declare_local_variable(scope.clone(), &variable)?;
        //         Some(TypedStmt::Variable(typed_var))
        //     }
        //     Stmt::Builtin(builtin) => match self.resolve_builtin(scope_id, scope.clone(), builtin) {
        //         Some(typed_builtin) => Some(TypedStmt::Builtin(typed_builtin)),
        //         None => None,
        //     },
        //     Stmt::Expr(expr) => {
        //         let typed_expr = self.resolve_expr(Some(scope.clone()), expr)?;
        //         Some(TypedStmt::Expr(typed_expr))
        //     }
        //     Stmt::If(if_stmt) => {
        //         let typed_if = self.resolve_if_stmt(scope.clone(), if_stmt)?;
        //         Some(TypedStmt::If(typed_if))
        //     }
        //     Stmt::Return(return_stmt) => {
        //         let arg = if let Some(argument) = &return_stmt.argument {
        //             Some(self.resolve_expr(Some(scope.clone()), argument)?)
        //         } else {
        //             None
        //         };
        //         Some(TypedStmt::Return(TypedReturnStmt {
        //             arg,
        //             loc: Loc::from_loc(return_stmt.loc, self.current_file_path()),
        //         }))
        //     }
        //     Stmt::Foreach(..) => todo!(),
        //     Stmt::For(for_stmt) => {
        //         let body_scope_id = generate_scope_id();
        //         let body_scope = LocalScope::new(Some(scope.clone()));
        //         self.insert_scope_ref(body_scope_id, body_scope.clone());

        //         let initializer = if let Some(variable) = &for_stmt.initializer {
        //             Some(self.declare_local_variable(Rc::clone(&body_scope), &variable)?)
        //         } else {
        //             None
        //         };

        //         let cond = if let Some(expr) = &for_stmt.condition {
        //             Some(self.resolve_expr(Some(Rc::clone(&body_scope)), expr)?)
        //         } else {
        //             None
        //         };

        //         let increment = if let Some(expr) = &for_stmt.increment {
        //             Some(self.resolve_expr(Some(Rc::clone(&body_scope)), expr)?)
        //         } else {
        //             None
        //         };

        //         let body = Box::new(self.resolve_block_stmt(body_scope_id, Rc::clone(&body_scope), &*for_stmt.body)?);

        //         Some(TypedStmt::For(TypedForStmt {
        //             initializer,
        //             cond,
        //             increment,
        //             body,
        //             loc: Loc::from_loc(for_stmt.loc, self.current_file_path()),
        //         }))
        //     }
        //     Stmt::While(while_stmt) => {
        //         let body_scope_id = generate_scope_id();
        //         let body_scope = LocalScope::new(Some(scope.clone()));
        //         self.insert_scope_ref(body_scope_id, body_scope.clone());

        //         let cond = self.resolve_expr(Some(Rc::clone(&body_scope)), &while_stmt.condition)?;

        //         let while_typed_body =
        //             Box::new(self.resolve_block_stmt(body_scope_id, Rc::clone(&body_scope), &*while_stmt.body)?);

        //         Some(TypedStmt::While(TypedWhileStmt {
        //             cond,
        //             body: while_typed_body,
        //             loc: Loc::from_loc(while_stmt.loc, self.current_file_path()),
        //         }))
        //     }
        //     Stmt::Switch(switch) => {
        //         let operand = self.resolve_expr(Some(Rc::clone(&scope)), &switch.operand)?;

        //         let mut cases: Vec<TypedSwitchCase> = Vec::new();
        //         for case in &switch.cases {
        //             let case_scope_rc = LocalScope::new(Some(scope.clone()));
        //             let case_scope_id = generate_scope_id();
        //             self.insert_scope_ref(case_scope_id, case_scope_rc.clone());

        //             let mut patterns = Vec::new();

        //             for pattern in &case.patterns {
        //                 let typed_pattern = match pattern {
        //                     SwitchCasePattern::Expr(expr) => {
        //                         let typed_expr = self.resolve_expr(Some(Rc::clone(&scope)), &expr)?;
        //                         let loc = typed_expr.loc;
        //                         TypedSwitchCasePattern::Expr(typed_expr, loc)
        //                     }
        //                     SwitchCasePattern::Ident(ident) => {
        //                         let symbol_id = generate_symbol_id();
        //                         let mut case_scope = case_scope_rc.borrow_mut();
        //                         case_scope.insert(
        //                             ident.value.clone(),
        //                             LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
        //                                 symbol_id,
        //                                 typed_variable: TypedVarStmt {
        //                                     symbol_id,
        //                                     name: ident.value.clone(),
        //                                     ty: None,
        //                                     rhs: None,
        //                                     is_const: false,
        //                                     analyzed: true,
        //                                     loc: Loc::from_loc(ident.loc, self.current_file_path()),
        //                                 },
        //                             })),
        //                         );
        //                         drop(case_scope);

        //                         TypedSwitchCasePattern::Ident(
        //                             ident.value.clone(),
        //                             SourceLoc::from_loc(ident.loc, self.current_file_path()),
        //                         )
        //                     }
        //                     SwitchCasePattern::EnumVariant(ident, valued_fields) => {
        //                         TypedSwitchCasePattern::EnumVariant(
        //                             ident.value.clone(),
        //                             valued_fields
        //                                 .iter()
        //                                 .map(|ident| {
        //                                     let symbol_id = generate_symbol_id();
        //                                     let mut case_scope = case_scope_rc.borrow_mut();
        //                                     case_scope.insert(
        //                                         ident.value.clone(),
        //                                         LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
        //                                             symbol_id,
        //                                             typed_variable: TypedVarStmt {
        //                                                 symbol_id,
        //                                                 name: ident.value.clone(),
        //                                                 ty: None,
        //                                                 rhs: None,
        //                                                 is_const: false,
        //                                                 analyzed: true,
        //                                                 loc: Loc::from_loc(ident.loc, self.current_file_path()),
        //                                             },
        //                                         })),
        //                                     );
        //                                     drop(case_scope);
        //                                     TypedIdentifier {
        //                                         name: ident.value.clone(),
        //                                         symbol_id,
        //                                         loc: Loc::from_loc(ident.loc, self.current_file_path()),
        //                                     }
        //                                 })
        //                                 .collect(),
        //                             SourceLoc::from_loc(ident.loc, self.current_file_path()),
        //                         )
        //                     }
        //                     SwitchCasePattern::Range(range) => {
        //                         let lower = self.resolve_expr(Some(scope.clone()), &range.lower);
        //                         let upper = self.resolve_expr(Some(scope.clone()), &range.upper);

        //                         TypedSwitchCasePattern::Range(TypedRange {
        //                             lower: lower?,
        //                             upper: upper?,
        //                             inclusive_upper: range.inclusive_upper,
        //                             loc: Loc::from_loc(range.loc, self.current_file_path()),
        //                         })
        //                     }
        //                 };

        //                 patterns.push(typed_pattern);
        //             }

        //             let mut body = self.resolve_block_stmt(scope_id, case_scope_rc.clone(), &case.body)?;
        //             body.scope_id = case_scope_id;

        //             cases.push(TypedSwitchCase {
        //                 patterns,
        //                 body: Box::new(body),
        //                 loc: Loc::from_loc(case.loc, self.current_file_path()),
        //             });
        //             drop(case_scope_rc);
        //         }

        //         let default_case = if let Some(default_case) = &switch.default_case {
        //             let body_scope_id = generate_scope_id();
        //             let body_scope = LocalScope::new(Some(scope.clone()));
        //             self.insert_scope_ref(body_scope_id, body_scope.clone());

        //             Some(self.resolve_block_stmt(body_scope_id, body_scope.clone(), &default_case)?)
        //         } else {
        //             None
        //         };

        //         Some(TypedStmt::Switch(TypedSwitchStmt {
        //             operand,
        //             cases,
        //             default_case,
        //             loc: Loc::from_loc(switch.loc, self.current_file_path()),
        //         }))
        //     }
        //     Stmt::Enum(enum_decl) => {
        //         let typed_stmt = self.resolve_enum_stmt(Some(Rc::clone(&scope)), enum_decl, Some(scope_id))?;
        //         Some(typed_stmt)
        //     }
        //     Stmt::Union(union_decl) => {
        //         let typed_stmt = self.resolve_union_stmt(Some(Rc::clone(&scope)), union_decl, Some(scope_id))?;
        //         Some(typed_stmt)
        //     }
        //     Stmt::Interface(interface) => {
        //         let typed_stmt = self.resolve_interface_stmt(Some(scope.clone()), interface)?;
        //         Some(typed_stmt)
        //     }
        //     Stmt::Struct(struct_decl) => {
        //         let typed_stmt = self.resolve_struct_stmt(Some(scope.clone()), struct_decl, Some(scope_id))?;
        //         Some(typed_stmt)
        //     }
        //     Stmt::BlockStmt(block_statement) => {
        //         let scope_id = generate_scope_id();
        //         let new_local_scope = LocalScope::new(Some(scope.clone()));

        //         self.insert_scope_ref(scope_id, new_local_scope.clone());

        //         let typed_stmt = self.resolve_block_stmt(scope_id, new_local_scope, block_statement)?;
        //         Some(TypedStmt::BlockStmt(typed_stmt))
        //     }
        //     Stmt::Break(break_stmt) => Some(TypedStmt::Break(TypedBreakStmt {
        //         loc: Loc::from_loc(break_stmt.loc, self.current_file_path()),
        //     })),
        //     Stmt::Continue(continue_stmt) => Some(TypedStmt::Continue(TypedContinueStmt {
        //         loc: Loc::from_loc(continue_stmt.loc, self.current_file_path()),
        //     })),
        //     Stmt::Typedef(typedef) => {
        //         let typed_stmt = self.resolve_typedef(Some(scope.clone()), &typedef)?;
        //         Some(typed_stmt)
        //     }
        //     Stmt::GlobalVar(..) | Stmt::FuncDef(..) | Stmt::FuncDecl(..) | Stmt::Import(..) => {
        //         self.reporter.report(Diag {
        //             level: DiagLevel::Error,
        //             kind: Box::new(ResolverDiagKind::InvalidStatement),
        //             location: Some(DiagLoc::new(SourceLoc::from_loc(stmt.loc(), self.current_file_path()))),
        //             hint: None,
        //         });
        //         None
        //     }
        //     Stmt::Label(label) => self.resolve_label(scope, label),
        //     Stmt::Goto(goto) => self.resolve_goto(goto),
        //     Stmt::Defer(_) => unreachable!(),
        // };

        // typed_stmt
    }

    fn resolve_goto(&self, goto: &Goto) -> Option<TypedStmt> {
        Some(TypedStmt::Goto(TypedGotoStmt {
            name: goto.name.as_string(),
            label_id: None,
            loc: Loc::from_loc(goto.loc, self.current_file_path()),
        }))
    }

    // FIXME
    fn resolve_label(&mut self, label: &Label) -> Option<TypedStmt> {
        todo!();

        // {
        //     let scope_ref = scope.borrow();
        //     if scope_ref.resolve_label(&label.name.as_string()).is_some() {
        //         // label already declare for current scope
        //         self.reporter.report(Diag {
        //             level: DiagLevel::Error,
        //             kind: Box::new(ResolverDiagKind::LabelAlreadyDefined {
        //                 label_name: label.name.to_string(),
        //             }),
        //             location: Some(DiagLoc::new(SourceLoc::from_loc(
        //                 label.loc,
        //                 self.current_file_path(),
        //             ))),
        //             hint: None,
        //         });
        //         return None;
        //     }
        // }

        // let label_id = generate_label_id();

        // {
        //     let mut scope_ref = scope.borrow_mut();
        //     scope_ref.insert_label(label.name.as_string(), label_id);
        // }

        // Some(TypedStmt::Label(TypedLabelStmt {
        //     name: label.name.as_string(),
        //     label_id,
        //     loc: Loc::from_loc(label.loc, self.current_file_path()),
        // }))
    }

    fn resolve_module_import_expr(&mut self, module_import: &ModuleImport) -> Option<TypedExprStmt> {
        if let Some(ident) = module_import.as_identifier() {
            self.resolve_ident_expr(&ident)
        } else {
            self.resolve_module_import(module_import.clone())
                .map(|symbol_id| TypedExprStmt {
                    kind: TypedExprKind::Symbol(TypedSymbolExpr::new(symbol_id, module_import.loc)),
                    sema_ty: None,
                    mloc: MemoryLocation::LValue,
                    loc: Loc::from_loc(module_import.loc, self.current_file_path()),
                })
        }
    }

    fn resolve_unnamed_union_value(&mut self, unnamed_union_value: &UnnamedUnionValue) -> Option<TypedExprStmt> {
        let field_value = self.resolve_expr(&unnamed_union_value.field_value)?;

        let kind = TypedExprKind::UnnamedUnionValue(TypedUnnamedUnionValue {
            field_name: unnamed_union_value.field_name.clone(),
            field_value: Box::new(field_value),
            union_ty: None,
            is_const: unnamed_union_value.is_const,
            loc: Loc::from_loc(unnamed_union_value.loc, self.current_file_path()),
        });

        Some(TypedExprStmt {
            kind,
            sema_ty: None,
            mloc: MemoryLocation::RValue,
            loc: Loc::from_loc(unnamed_union_value.loc, self.current_file_path()),
        })
    }

    fn resolve_unnamed_enum_value(&mut self, unnamed_enum_value: &UnnamedEnumValue) -> Option<TypedExprStmt> {
        let kind = match &unnamed_enum_value.kind {
            UnnamedEnumValueKind::Plain => TypedUnnamedEnumValueKind::Plain,
            UnnamedEnumValueKind::Fielded(exprs) => {
                let mut typed_exprs: Vec<TypedExprStmt> = Vec::new();
                for expr in exprs {
                    match self.resolve_expr(expr) {
                        Some(typed_expr) => typed_exprs.push(typed_expr),
                        None => continue,
                    }
                }
                TypedUnnamedEnumValueKind::Fielded(typed_exprs)
            }
        };

        Some(TypedExprStmt {
            kind: TypedExprKind::UnnamedEnumValue(TypedUnnamedEnumValue {
                ident: unnamed_enum_value.ident.clone(),
                kind,
                enum_ty: None,
                loc: Loc::from_loc(unnamed_enum_value.loc, self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: Loc::from_loc(unnamed_enum_value.loc, self.current_file_path()),
        })
    }

    fn resolve_dynamic_expr(&mut self, dynamic_expr: &Dynamic) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&dynamic_expr.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Dynamic(TypedDynamicExpr {
                operand: Box::new(operand),
                vtable_id: None,
                object_name: None,
                loc: Loc::from_loc(dynamic_expr.loc, self.current_file_path()),
            }),
            sema_ty: None,
            mloc: MemoryLocation::RValue,
            loc: Loc::from_loc(dynamic_expr.loc, self.current_file_path()),
        })
    }

    fn resolve_tuple_member_access(&mut self, tuple_member_access: &TupleAccess) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&tuple_member_access.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::TupleAccess(TypedTupleAccessExpr {
                operand: Box::new(operand),
                index: tuple_member_access.index,
                loc: Loc::from_loc(tuple_member_access.loc, self.current_file_path()),
            }),
            sema_ty: None,
            mloc: MemoryLocation::LValue,
            loc: Loc::from_loc(tuple_member_access.loc, self.current_file_path()),
        })
    }

    fn resolve_tuple_expr(&mut self, tuple_value: &TupleValue) -> Option<TypedExprStmt> {
        let mut elements: Vec<TypedExprStmt> = Vec::new();

        for expr in &tuple_value.elements {
            match self.resolve_expr(expr) {
                Some(typed_expr) => elements.push(typed_expr),
                None => continue,
            }
        }

        Some(TypedExprStmt {
            kind: TypedExprKind::Tuple(TypedTupleExpr {
                elements,
                loc: Loc::from_loc(tuple_value.loc, self.current_file_path()),
            }),
            sema_ty: None,
            mloc: MemoryLocation::RValue,
            loc: Loc::from_loc(tuple_value.loc, self.current_file_path()),
        })
    }

    fn resolve_lambda_expr(&mut self, lambda: &Lambda) -> Option<TypedExprStmt> {
        let scope_id = generate_scope_id();
        let scope_rc = LocalScope::new(None);
        self.insert_scope_ref(scope_id, scope_rc.clone());

        let (list, variadic) = self.resolve_func_params(&None, &lambda.params)?;

        let body = match self.resolve_block_stmt(&lambda.body) {
            Some(typed_block) => Box::new(typed_block),
            None => return None,
        };

        let return_type = self.resolve_type(&None, lambda.return_type.clone(), lambda.loc)?;

        let loc = SourceLoc::from_loc(lambda.loc, self.current_file_path());

        Some(TypedExprStmt {
            kind: TypedExprKind::Lambda(TypedLambdaExpr {
                params: TypedFuncParams { list, variadic },
                body,
                return_type,
                inline: lambda.inline,
                loc: loc,
            }),
            sema_ty: None,
            mloc: MemoryLocation::RValue,
            loc,
        })
    }

    fn resolve_field_access(&mut self, field_access: &FieldAccess) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&field_access.operand)?;

        let type_args = field_access
            .type_args
            .clone()
            .and_then(|type_args| self.resolve_type_args(&None, &type_args, field_access.loc));

        Some(TypedExprStmt {
            kind: TypedExprKind::FieldAccess(TypedFieldAccess {
                operand: Box::new(operand),
                field_name: field_access.field_name.value.clone(),
                is_fat_arrow: field_access.is_fat_arrow,
                field_index: None,
                field_ty: None,
                object_symbol_id: None,
                type_args,
                loc: Loc::from_loc(field_access.loc, self.current_file_path()),
            }),
            mloc: MemoryLocation::LValue,
            sema_ty: None,
            loc: Loc::from_loc(field_access.loc, self.current_file_path()),
        })
    }

    fn resolve_method_call(&mut self, method_call: &MethodCall) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&method_call.operand)?;

        let args: Vec<TypedExprStmt> = method_call
            .args
            .iter()
            .filter_map(|arg| self.resolve_expr(arg))
            .collect();

        let type_args = method_call
            .type_args
            .clone()
            .and_then(|type_args| self.resolve_type_args(&None, &type_args, method_call.loc));

        Some(TypedExprStmt {
            kind: TypedExprKind::MethodCall(TypedMethodCall {
                operand: Box::new(operand),
                func_sig: None,
                type_args,
                object_name: None,
                method_name: method_call.method_name.value.clone(),
                is_fat_arrow: method_call.is_fat_arrow,
                monomorph_key: None,
                self_ty: None,
                enum_const: None,
                method_call_on_interface: None,
                loc: Loc::from_loc(method_call.loc, self.current_file_path()),
                args,
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: Loc::from_loc(method_call.loc, self.current_file_path()),
        })
    }

    fn resolve_struct_init(&mut self, struct_init: &StructInit) -> Option<TypedExprStmt> {
        let symbol_id = self.resolve_local_module_import(&struct_init.struct_name)?;

        let field_inits: Vec<TypedStructFieldInit> = struct_init
            .field_inits
            .iter()
            .filter_map(|field_init| {
                self.resolve_expr(&field_init.value).map(|value| TypedStructFieldInit {
                    name: field_init.ident.value.clone(),
                    value,
                    loc: Loc::from_loc(field_init.loc, self.current_file_path()),
                })
            })
            .collect();

        let type_args = struct_init
            .type_args
            .clone()
            .and_then(|type_args| self.resolve_type_args(&None, &type_args, struct_init.loc));

        Some(TypedExprStmt {
            kind: TypedExprKind::StructInit(TypedStructInitExpr {
                symbol_id,
                fields: field_inits,
                type_args,
                is_const: struct_init.is_const,
                loc: Loc::from_loc(struct_init.loc, self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: Loc::from_loc(struct_init.loc, self.current_file_path()),
        })
    }

    fn resolve_func_call(&mut self, func_call: &FuncCall) -> Option<TypedExprStmt> {
        if scope_required!(self, func_call.loc) {
            return None;
        }

        let operand = self.resolve_expr(&func_call.operand)?;

        let typed_args: Vec<TypedExprStmt> = func_call.args.iter().filter_map(|arg| self.resolve_expr(arg)).collect();

        let type_args = func_call
            .type_args
            .clone()
            .and_then(|type_args| self.resolve_type_args(&None, &type_args, func_call.loc));

        Some(TypedExprStmt {
            kind: TypedExprKind::FuncCall(TypedFuncCall {
                operand: Box::new(operand),
                args: typed_args,
                type_args,
                return_type: None,
                monomorph_key: None,
                loc: Loc::from_loc(func_call.loc, self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: Loc::from_loc(func_call.loc, self.current_file_path()),
        })
    }

    fn resolve_untyped_array_expr(&mut self, untyped_array: &UntypedArray) -> Option<TypedExprStmt> {
        let elements: Vec<TypedExprStmt> = untyped_array
            .elements
            .iter()
            .filter_map(|item| self.resolve_expr(item))
            .collect();

        Some(TypedExprStmt {
            kind: TypedExprKind::Array(TypedArrayExpr {
                array_type: None,
                elements,
                loc: Loc::from_loc(untyped_array.loc, self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: Loc::from_loc(untyped_array.loc, self.current_file_path()),
        })
    }

    fn resolve_array_expr(&mut self, arr: &Array) -> Option<TypedExprStmt> {
        let array_type = self.resolve_type(&None, arr.data_type.clone(), arr.loc)?;

        let typed_elements: Vec<TypedExprStmt> =
            arr.elements.iter().filter_map(|item| self.resolve_expr(item)).collect();

        Some(TypedExprStmt {
            kind: TypedExprKind::Array(TypedArrayExpr {
                array_type: Some(array_type),
                elements: typed_elements,
                loc: Loc::from_loc(arr.loc, self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: Loc::from_loc(arr.loc, self.current_file_path()),
        })
    }

    fn resolve_infix_expr(&mut self, infix: &InfixExpr) -> Option<TypedExprStmt> {
        let lhs = self.resolve_expr(&*infix.lhs)?;
        let rhs = self.resolve_expr(&*infix.rhs)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Infix(TypedInfixExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: infix.op.clone(),
                loc: infix.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: infix.loc,
        })
    }

    fn resolve_prefix_expr(&mut self, prefix: &PrefixExpr) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&*prefix.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Prefix(TypedPrefixExpr {
                operand: Box::new(operand),
                op: prefix.op.clone(),
                loc: prefix.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: prefix.loc,
        })
    }

    fn resolve_type_specifier_expr(&mut self, type_specifier: &TypeSpecifier) -> Option<TypedExprStmt> {
        let loc = type_specifier.loc();

        let symbol_id = match type_specifier {
            TypeSpecifier::Ident(ident) => self.resolve_ident(&ident)?,
            TypeSpecifier::ModuleImport(module_import) => self.resolve_module_import(module_import.clone())?,
            _ => {
                let sema_ty = self.resolve_type(&None, type_specifier.clone(), loc)?;
                return Some(TypedExprStmt {
                    kind: TypedExprKind::SemanticType(sema_ty.clone()),
                    mloc: MemoryLocation::RValue,
                    sema_ty: Some(sema_ty),
                    loc: Loc::from_loc(loc, self.current_file_path()),
                });
            }
        };

        Some(TypedExprStmt {
            kind: TypedExprKind::Symbol(TypedSymbolExpr::new(symbol_id, loc)),
            mloc: MemoryLocation::LValue,
            sema_ty: None,
            loc,
        })
    }

    fn resolve_assign_expr(&mut self, assign: &Assign) -> Option<TypedExprStmt> {
        let lhs = self.resolve_expr(&assign.lhs)?;
        let rhs = self.resolve_expr(&assign.rhs)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Assign(TypedAssignExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                kind: assign.kind.clone(),
                loc: assign.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: assign.loc,
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
                                location: Some(literal.loc),
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
                            let len_expr = literal_expr_from_const_int(len.try_into().unwrap(), literal.loc);

                            Some(SemanticType::Array(TypedArrayType {
                                element_type: Box::new(SemanticType::PlainType(PlainType::Char)),
                                capacity: TypedArrayCapacity::Fixed(Box::new(len_expr)),
                                loc: literal.loc,
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
            loc: literal.loc,
        };

        Some(TypedExprStmt {
            kind: TypedExprKind::Literal(typed_literal.clone()),
            sema_ty: None,
            mloc: MemoryLocation::RValue,
            loc: typed_literal.loc,
        })
    }

    fn resolve_unary_expr(&mut self, unary: &UnaryExpr) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&*unary.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Unary(TypedUnaryExpr {
                op: unary.op.clone(),
                operand: Box::new(operand),
                loc: Loc::from_loc(unary.loc, self.current_file_path()),
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: Loc::from_loc(unary.loc, self.current_file_path()),
        })
    }

    fn resolve_array_index_expr(&mut self, array_index: &ArrayIndex) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&array_index.operand)?;
        let index = self.resolve_expr(&array_index.index)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::ArrayIndex(TypedArrayIndexExpr {
                operand: Box::new(operand),
                index: Box::new(index),
                loc: array_index.loc,
            }),
            mloc: MemoryLocation::LValue,
            sema_ty: None,
            loc: array_index.loc,
        })
    }

    fn resolve_address_of_expr(&mut self, address_of: &AddrOf) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&address_of.expr)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::AddrOf(TypedAddrOfExpr {
                operand: Box::new(operand),
                loc: Loc::from_loc(address_of.loc, self.current_file_path()),
            }),
            mloc: MemoryLocation::LValue,
            sema_ty: None,
            loc: Loc::from_loc(address_of.loc, self.current_file_path()),
        })
    }

    fn resolve_deref_expr(&mut self, deref: &Deref) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&deref.expr)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Deref(TypedDerefExpr {
                operand: Box::new(operand),
                loc: deref.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: deref.loc,
        })
    }

    fn resolve_unnamed_struct_value(&mut self, unnamed_struct_value: &UnnamedStructValue) -> Option<TypedExprStmt> {
        let mut fields: Vec<TypedUnnamedStructValueField> = Vec::new();

        for field in &unnamed_struct_value.fields {
            let name = field.field_name.as_string();

            let ty = if let Some(type_specifier) = &field.field_ty {
                match self.resolve_type(&None, type_specifier.clone(), field.loc) {
                    Some(sema_ty) => Some(sema_ty),
                    None => continue,
                }
            } else {
                None
            };

            let value = match self.resolve_expr(&field.field_value) {
                Some(typed_expr) => typed_expr,
                None => continue,
            };

            fields.push(TypedUnnamedStructValueField {
                name,
                ty,
                field_value: Box::new(value),
                loc: field.loc,
            });
        }

        Some(TypedExprStmt {
            kind: TypedExprKind::UnnamedStructValue(TypedUnnamedStructValue {
                fields,
                repr_attr: unnamed_struct_value.repr_attr.clone(),
                align: unnamed_struct_value.align.clone(),
                loc: unnamed_struct_value.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_ty: None,
            loc: unnamed_struct_value.loc,
        })
    }
}

// Resolver helper methods.
impl<'diag> Resolver<'diag> {
    // FIXME: Rename function and write comment for it.
    fn declare_local_variable(&mut self, scope_rc: LocalScopeRef, variable: &Variable) -> Option<TypedVarStmt> {
        todo!();
        // if scope_rc.borrow().resolve(&variable.ident.value).is_some() {
        //     self.reporter.report(Diag {
        //         level: DiagLevel::Error,
        //         kind: Box::new(ResolverDiagKind::DuplicateSymbolInThisScope {
        //             symbol_name: variable.ident.value.clone(),
        //         }),
        //         location: Some(DiagLoc::new(SourceLoc::from_loc(
        //             variable.loc,
        //             self.current_file_path(),
        //         ))),
        //         hint: None,
        //     });
        //     return None;
        // }

        // let var_type = variable.ty.as_ref().and_then(|ty_spec| {
        //     self.resolve_type(
        //         &None,
        //         Some(scope_rc.clone()),
        //         ty_spec.clone(),
        //         variable.loc,
        //         variable.span.end,
        //     )
        // });

        // let typed_rhs = variable
        //     .rhs
        //     .as_ref()
        //     .and_then(|expr| self.resolve_expr(Some(scope_rc.clone()), expr));

        // let symbol_id = generate_symbol_id();

        // let typed_variable = TypedVarStmt {
        //     symbol_id,
        //     name: variable.ident.value.clone(),
        //     ty: var_type.clone(),
        //     rhs: typed_rhs.clone(),
        //     is_const: variable.is_const,
        //     analyzed: typed_rhs.is_some(),
        //     loc: Loc::from_loc(variable.loc, self.current_file_path()),
        // };

        // let resolved_var = ResolvedVariable {
        //     symbol_id,
        //     typed_variable: typed_variable.clone(),
        // };

        // scope_rc.borrow_mut().insert(
        //     variable.ident.value.clone(),
        //     LocalSymbol::new(LocalSymbolKind::Variable(resolved_var)),
        // );

        // Some(typed_variable)
    }

    fn report_if_duplicate_symbol(&mut self, symbol_name: String, loc: Loc) -> bool {
        let module_id = self.module_id.unwrap();

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
}
