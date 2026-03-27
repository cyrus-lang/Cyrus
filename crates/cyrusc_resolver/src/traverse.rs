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

use crate::Resolver;
use crate::diagnostics::ResolverDiagKind;
use crate::with_local_scope;
use cyrusc_ast::format::format_module_segments;
use cyrusc_ast::*;
use cyrusc_diagcentral::Diag;
use cyrusc_diagcentral::DiagLevel;
use cyrusc_internal::local_scope::LocalScope;
use cyrusc_internal::symbols::symbols::*;
use cyrusc_internal::symbols::table::*;
use cyrusc_source_loc::Loc;
use cyrusc_tokens::Token;
use cyrusc_tokens::TokenKind;
use cyrusc_tokens::literals::{ASTLiteralExpr, LiteralKind, StringPrefix};
use cyrusc_typed_ast::exprs::*;
use cyrusc_typed_ast::generics::generic_type::GenericType;
use cyrusc_typed_ast::generics::mapping_ctx::GenericMappingCtx;
use cyrusc_typed_ast::sigs::*;
use cyrusc_typed_ast::stmts::*;
use cyrusc_typed_ast::types::*;
use cyrusc_typed_ast::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// Resolver endpoints.
impl Resolver {
    // Scans the top-level AST for declarations (typedefs, functions, structs, etc.)
    // And Registers each declared name into the current module’s symbol table. (first pass)
    pub(crate) fn resolve_decl_names(&mut self, ast: &ProgramTree) {
        for stmt in ast.body.as_ref() {
            // skip statement if it's not a declaration symbol
            let Some(decl_name) = stmt.decl_name() else {
                continue;
            };

            if self.report_if_duplicate_symbol(self.current_scope.unwrap(), decl_name.as_string(), stmt.loc()) {
                continue;
            }

            let symbol_id = self
                .global_symbols
                .insert_symbol_entry(SymbolEntry::unresolved(stmt.vis(), self.current_scope));

            self.global_symbols
                .insert_symbol_name(self.current_scope.unwrap(), symbol_id, &decl_name.value);
        }
    }

    // Resolves the full meaning of each top-level declaration in the AST (second pass)
    pub(crate) fn resolve_decl_full(&mut self, ast: &ProgramTree) -> Vec<TypedStmt> {
        let mut typed_body = Vec::new();

        for stmt in ast.body.as_ref() {
            let typed_stmt = match stmt {
                ASTStmt::Import(..) | ASTStmt::Expr(..) => continue,
                ASTStmt::Builtin(builtin) => self.resolve_builtin(builtin).map(TypedStmt::Builtin),
                ASTStmt::GlobalVar(global_var) => self.resolve_global_var_stmt(global_var),
                ASTStmt::Typedef(typedef) => self.resolve_typedef(typedef),
                ASTStmt::FuncDef(func_def) => self.resolve_func_def(func_def),
                ASTStmt::FuncDecl(func_decl) => self.resolve_func_decl(func_decl),
                ASTStmt::Struct(struct_) => self.resolve_struct_stmt(struct_),
                ASTStmt::Enum(enum_) => self.resolve_enum_stmt(enum_),
                ASTStmt::Union(union_) => self.resolve_union_stmt(union_),
                ASTStmt::Interface(interface) => self.resolve_interface_stmt(interface),

                // invalid top-level statements
                _ => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::InvalidTopLevelStatement),
                        loc: Some(stmt.loc()),
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

    /// Resolve an identifier in the current lexical context.
    ///
    /// Resolution order:
    /// 1. Local scope stack (variables, parameters, labels)
    /// 2. Special contextual symbols (e.g. `Self`)
    /// 3. Global symbols defined in the current module
    ///
    /// If no symbol can be found, an error diagnostic is emitted.
    fn resolve_ident(&mut self, ident: &Ident) -> Option<SymbolID> {
        if let Some(symbol_id) = self.resolve_local_scope_symbol(&ident.value) {
            return Some(symbol_id);
        }

        if ident.value == "Self" {
            return self.current_object;
        }

        if let Some(symbol_id) = self.lookup_symbol_id(self.current_scope.unwrap(), &ident.value) {
            return Some(symbol_id);
        }

        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(ResolverDiagKind::SymbolNotFound {
                name: ident.as_string(),
            }),
            loc: Some(ident.loc),
            hint: None,
        });

        None
    }

    /// Resolve a module import that refers to a single identifier.
    ///
    /// This is equivalent to resolving the identifier within the
    /// current lexical and module scope.
    fn resolve_local_module_import(&mut self, module_import: &ASTModuleImport) -> Option<SymbolID> {
        if let Some(ident) = module_import.as_ident() {
            return self.resolve_ident(&ident);
        }

        None
    }

    fn resolve_module_import(&mut self, module_import: ASTModuleImport) -> Option<SymbolID> {
        if let Some(ident) = module_import.as_ident() {
            return self.resolve_ident(&ident);
        }

        assert!(!module_import.segments.is_empty());

        let mut iter = module_import.segments.iter();
        let first = iter.next().unwrap();

        let Some(first_ident) = first.as_ident() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::ExpectedIdentifierInImport),
                loc: Some(first.loc()),
                hint: None,
            });
            return None;
        };

        let current_scope_id = self.current_scope.unwrap();

        let mut current_symbol = match self.lookup_symbol_id(current_scope_id, &first_ident.value) {
            Some(symbol_id) => symbol_id,
            None => {
                let root_scope = self.global_symbols.root_scope();

                if let Some(global_id) = self
                    .global_symbols
                    .lookup_symbol_id_in_scope(root_scope, &first_ident.value)
                {
                    if self
                        .global_symbols
                        .lookup_symbol_id_in_scope(current_scope_id, &first_ident.value)
                        .is_none()
                    {
                        let proxy_id =
                            self.global_symbols
                                .insert_proxied_module(current_scope_id, &first_ident.value, global_id);

                        self.global_symbols
                            .insert_symbol_name(current_scope_id, proxy_id, &first_ident.value);
                    }
                    global_id
                } else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::SymbolNotFound {
                            name: first_ident.value.clone(),
                        }),
                        loc: Some(first_ident.loc),
                        hint: None,
                    });
                    return None;
                }
            }
        };

        let mut i = 1;
        for segment in iter {
            let Some(seg_ident) = segment.as_ident() else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::ExpectedIdentifierInImport),
                    loc: Some(segment.loc()),
                    hint: None,
                });
                return None;
            };

            let name = &seg_ident.value;

            let scope_id = self.global_symbols.resolve_concrete_scope_id(current_symbol);

            let Some(next_symbol) = self.lookup_symbol_id_in_scope(scope_id, name) else {
                let module_name = format_module_segments(&module_import.segments[0..i]);
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::SymbolIsNotDefinedInModule {
                        symbol_name: name.clone(),
                        module_name,
                    }),
                    loc: Some(seg_ident.loc),
                    hint: None,
                });
                return None;
            };

            if self
                .global_symbols
                .lookup_symbol_id_in_scope(current_scope_id, name)
                .is_none()
            {
                let proxy_id = self
                    .global_symbols
                    .insert_proxied_module(current_scope_id, name, next_symbol);

                self.global_symbols.insert_symbol_name(current_scope_id, proxy_id, name);
            }

            current_symbol = next_symbol;
            i += 1;
        }

        Some(current_symbol)
    }

    fn resolve_ident_expr(&mut self, ident: &Ident) -> Option<TypedExprStmt> {
        let symbol_id = self.resolve_ident(ident)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Symbol(TypedSymbolExpr::new(symbol_id, ident.loc)),
            sema_type: None,
            mloc: MemoryLocation::LValue,
            loc: ident.loc,
        })
    }

    fn resolve_expr(&mut self, expr: &ASTExpr) -> Option<TypedExprStmt> {
        match expr {
            ASTExpr::Ident(ident) => self.resolve_ident_expr(ident),
            ASTExpr::Infix(infix_expr) => self.resolve_infix_expr(infix_expr),
            ASTExpr::Prefix(prefix_expr) => self.resolve_prefix_expr(prefix_expr),
            ASTExpr::Unary(unary) => self.resolve_unary_expr(unary),
            ASTExpr::Assign(assignment) => self.resolve_assign_expr(assignment),
            ASTExpr::FieldAccess(field_access) => self.resolve_field_access(field_access),
            ASTExpr::MethodCall(method_call) => self.resolve_method_call(method_call),
            ASTExpr::StructInit(struct_init) => self.resolve_struct_init(struct_init),
            ASTExpr::ModuleImport(module_import) => self.resolve_module_import_expr(module_import),
            ASTExpr::FuncCall(func_call) => self.resolve_func_call(func_call),
            ASTExpr::Array(array) => self.resolve_array_expr(array),
            ASTExpr::Literal(literal) => self.resolve_literal_expr(literal),
            ASTExpr::ArrayIndex(array_index) => self.resolve_array_index_expr(array_index),
            ASTExpr::AddrOf(address_of) => self.resolve_addr_of_expr(address_of),
            ASTExpr::Deref(dereference) => self.resolve_deref_expr(dereference),
            ASTExpr::Lambda(lambda) => self.resolve_lambda_expr(lambda),
            ASTExpr::Tuple(tuple_value) => self.resolve_tuple_expr(tuple_value),
            ASTExpr::TupleAccess(tuple_member_access) => self.resolve_tuple_member_access(tuple_member_access),
            ASTExpr::Dynamic(dynamic) => self.resolve_dynamic_expr(dynamic),
            ASTExpr::UntypedArray(untyped_array) => self.resolve_untyped_array_expr(untyped_array),
            ASTExpr::UnnamedEnumValue(unnamed_enum_value) => self.resolve_unnamed_enum_value(unnamed_enum_value),
            ASTExpr::UnnamedUnionValue(unnamed_union_value) => self.resolve_unnamed_union_value(unnamed_union_value),
            ASTExpr::UnnamedStructValue(unnamed_struct_value) => {
                self.resolve_unnamed_struct_value(unnamed_struct_value)
            }
            ASTExpr::Builtin(builtin) => match self.resolve_builtin(builtin) {
                Some(typed_builtin) => {
                    let kind = TypedExprKind::Builtin(typed_builtin);

                    Some(TypedExprStmt {
                        kind,
                        sema_type: None,
                        mloc: MemoryLocation::RValue,
                        loc: builtin.loc(),
                    })
                }
                None => None,
            },
            ASTExpr::TypeSpecifier(type_spec) => self.resolve_type_specifier_expr(type_spec),
        }
    }

    fn resolve_type(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        type_spec: TypeSpecifier,
        loc: Loc,
    ) -> Option<SemanticType> {
        match type_spec {
            TypeSpecifier::GenericInst(inst) => self.resolve_generic_inst_type(generic_params, inst, loc),
            TypeSpecifier::Tuple(tuple) => self.resolve_tuple_type(generic_params, tuple),
            TypeSpecifier::FuncType(func) => self.resolve_func_type(generic_params, *func, loc),
            TypeSpecifier::Array(array) => self.resolve_array_type(generic_params, array, loc),
            TypeSpecifier::Const(inner) => {
                let inner = self.resolve_type(generic_params, *inner, loc)?;
                Some(SemanticType::Const(Box::new(inner)))
            }
            TypeSpecifier::Deref(inner) => {
                let inner = self.resolve_type(generic_params, *inner, loc)?;
                Some(SemanticType::Pointer(Box::new(inner)))
            }
            TypeSpecifier::TypeToken(token) => self.resolve_builtin_type(token, loc),
            TypeSpecifier::Ident(ident) => self.resolve_ident_type(generic_params, ident),
            TypeSpecifier::ModuleImport(import) => self.resolve_module_import_type(import),
            TypeSpecifier::UnnamedUnion(union_ty) => self.resolve_union_type(generic_params, union_ty),
            TypeSpecifier::UnnamedEnum(enum_ty) => self.resolve_enum_type(enum_ty),
            TypeSpecifier::UnnamedStruct(struct_ty) => self.resolve_struct_type(generic_params, struct_ty),
            TypeSpecifier::SelfType(self_ty) => Some(SemanticType::SelfType(TypedSelfType { loc: self_ty.loc })),
        }
    }

    fn resolve_builtin_type(&mut self, token: Token, loc: Loc) -> Option<SemanticType> {
        match SemanticType::try_from(token.kind.clone()) {
            Ok(ty) => Some(ty),
            Err(_) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::TypeNotFound {
                        name: token.kind.to_string(),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
                None
            }
        }
    }

    fn resolve_ident_type(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        ident: Ident,
    ) -> Option<SemanticType> {
        if let Some(ty) = self.resolve_generic_param_as_type(generic_params, &ident) {
            return Some(ty);
        }

        if let Some(symbol_id) = self.resolve_local_scope_symbol(&ident.value) {
            return Some(SemanticType::UnresolvedSymbol(symbol_id));
        }

        if let Some(symbol_id) = self.lookup_symbol_id(self.current_scope.unwrap(), &ident.value) {
            return Some(SemanticType::UnresolvedSymbol(symbol_id));
        }

        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(ResolverDiagKind::TypeNotFound {
                name: ident.as_string(),
            }),
            loc: Some(ident.loc),
            hint: None,
        });

        None
    }

    fn resolve_generic_param_as_type(
        &mut self,
        generic_params_list_opt: &Option<TypedGenericParamsList>,
        ident: &Ident,
    ) -> Option<SemanticType> {
        let name = ident.as_string();

        if let Some(generic_params) = generic_params_list_opt {
            if let Some(param) = generic_params
                .list
                .iter()
                .find(|generic_param| generic_param.name.value == name)
            {
                return Some(SemanticType::GenericParam(param.clone()));
            }
        }

        if let Some(object_params) = &self.current_object_generic_params {
            if let Some(param) = object_params.lookup_named(&ident.value) {
                return Some(SemanticType::GenericParam(param.clone()));
            }
        }

        None
    }

    fn resolve_module_import_type(&mut self, module_import: ASTModuleImport) -> Option<SemanticType> {
        self.resolve_module_import(module_import.clone())
            .map(SemanticType::UnresolvedSymbol)
            .or_else(|| {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::TypeNotFound {
                        name: module_import.to_string(),
                    }),
                    loc: Some(module_import.loc),
                    hint: None,
                });
                None
            })
    }

    fn resolve_func_type(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        func: FuncType,
        loc: Loc,
    ) -> Option<SemanticType> {
        let mut params = Vec::with_capacity(func.params.list.len());

        for param in func.params.list {
            let ty = self.resolve_type(generic_params, param, loc)?;
            params.push(ty);
        }

        let variadic = match func.params.variadic {
            Some(FuncTypeVariadicParams::UntypedCStyle) => Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle)),

            Some(FuncTypeVariadicParams::Typed(spec)) => {
                let ty = self.resolve_type(generic_params, spec, loc)?;
                Some(Box::new(TypedFuncTypeVariadicParams::Typed(ty)))
            }

            None => None,
        };

        let ret_type = self.resolve_type(generic_params, *func.ret_type, loc)?;

        Some(SemanticType::FuncType(TypedFuncType {
            symbol_id: None,
            params: TypedFuncTypeParams { list: params, variadic },
            ret_type: Box::new(ret_type),
            is_public: true,
            loc,
        }))
    }

    fn resolve_generic_inst_type(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        inst: GenericInst,
        loc: Loc,
    ) -> Option<SemanticType> {
        let base = self.resolve_type(generic_params, *inst.base.clone(), loc)?;

        let symbol_id = match base.const_inner().as_unresolved_symbol() {
            Some(id) => id,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::TypeDoesNotAcceptTypeArgs {
                        type_name: inst.base.to_string(),
                    }),
                    loc: Some(inst.loc),
                    hint: None,
                });

                return None;
            }
        };

        let type_args = self.resolve_type_args(generic_params, &inst.type_args, loc)?;

        Some(SemanticType::GenericType(GenericType {
            base: symbol_id,
            type_args: Some(type_args),
            mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
            mapping_ctx_arena: self.mapping_ctx_arena.clone(),
            generic_params: TypedGenericParamsList::new(),
            loc: inst.loc,
        }))
    }

    fn resolve_tuple_type(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        tuple: TupleType,
    ) -> Option<SemanticType> {
        let mut elements = Vec::new();

        for type_spec in tuple.type_list {
            elements.push(self.resolve_type(generic_params, type_spec, tuple.loc)?);
        }

        Some(SemanticType::Tuple(TypedTupleType {
            elements,
            loc: tuple.loc,
        }))
    }

    fn resolve_array_type(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        array: ArrayType,
        loc: Loc,
    ) -> Option<SemanticType> {
        let element_type = self.resolve_type(generic_params, *array.element_type, loc)?;

        let capacity = match array.size {
            ArrayCapacity::Fixed(expr) => {
                let expr = self.resolve_expr(&expr)?;
                TypedArrayCapacity::Fixed(Box::new(expr))
            }
            ArrayCapacity::Dynamic => TypedArrayCapacity::Dynamic,
        };

        Some(SemanticType::Array(TypedArrayType {
            element_type: Box::new(element_type),
            capacity,
            loc,
        }))
    }

    fn resolve_union_type(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        union: UnnamedUnionType,
    ) -> Option<SemanticType> {
        let fields = self.resolve_union_fields(generic_params, &union)?;

        Some(SemanticType::UnnamedUnion(TypedUnnamedUnionType {
            fields,
            repr_attr: union.repr_attr,
            align: union.align,
            loc: union.loc,
        }))
    }

    fn resolve_union_fields(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        union: &UnnamedUnionType,
    ) -> Option<Vec<TypedUnnamedUnionTypeField>> {
        let mut fields = Vec::with_capacity(union.fields.len());

        for field in &union.fields {
            let ty = self.resolve_type(generic_params, field.field_ty.clone(), field.loc)?;

            fields.push(TypedUnnamedUnionTypeField {
                name: field.field_name.value.clone(),
                ty: Box::new(ty),
                loc: field.loc,
            });
        }

        Some(fields)
    }

    fn resolve_enum_type(&mut self, unnamed_enum_type: UnnamedEnumType) -> Option<SemanticType> {
        let variants = self.resolve_enum_variants(&unnamed_enum_type)?;

        let tag_type = match unnamed_enum_type.tag_type {
            Some(tag) => {
                let ty = self.resolve_type(&None, *tag, unnamed_enum_type.loc)?;
                Some(Box::new(ty))
            }
            None => None,
        };

        Some(SemanticType::UnnamedEnum(TypedUnnamedEnumType {
            variants,
            tag_type,
            repr_attr: unnamed_enum_type.repr_attr,
            align: unnamed_enum_type.align,
            loc: unnamed_enum_type.loc,
        }))
    }

    fn resolve_enum_variants(&mut self, unnamed_enum_type: &UnnamedEnumType) -> Option<Vec<TypedUnnamedEnumVariant>> {
        let mut variants = Vec::with_capacity(unnamed_enum_type.variants.len());

        for variant in &unnamed_enum_type.variants {
            let typed_variant = match variant {
                UnnamedEnumVariant::Ident(ident) => TypedUnnamedEnumVariant::Ident(ident.clone()),
                UnnamedEnumVariant::Valued(ident, expr) => {
                    let expr = self.resolve_expr(expr)?;
                    TypedUnnamedEnumVariant::Valued(ident.clone(), Box::new(expr))
                }
                UnnamedEnumVariant::Variant(ident, fields) => {
                    let valued_fields = self.resolve_enum_variant_fields(fields)?;
                    TypedUnnamedEnumVariant::Variant(ident.clone(), valued_fields)
                }
            };

            variants.push(typed_variant);
        }

        Some(variants)
    }

    fn resolve_enum_variant_fields(
        &mut self,
        fields: &[UnnamedEnumValuedField],
    ) -> Option<Vec<TypedUnnamedEnumValuedField>> {
        let mut typed_fields = Vec::with_capacity(fields.len());

        for field in fields {
            let ty = self.resolve_type(&None, field.ty.clone(), field.loc)?;

            typed_fields.push(TypedUnnamedEnumValuedField { ty, loc: field.loc });
        }

        Some(typed_fields)
    }

    fn resolve_struct_type(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        unnamed_struct_type: UnnamedStructType,
    ) -> Option<SemanticType> {
        let fields = self.resolve_struct_fields(generic_params, &unnamed_struct_type)?;

        Some(SemanticType::UnnamedStruct(TypedUnnamedStructType {
            fields,
            repr_attr: unnamed_struct_type.repr_attr,
            align: unnamed_struct_type.align,
            loc: unnamed_struct_type.loc,
        }))
    }

    fn resolve_struct_fields(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        unnamed_struct_type: &UnnamedStructType,
    ) -> Option<Vec<TypedUnnamedStructTypeField>> {
        let mut fields = Vec::with_capacity(unnamed_struct_type.fields.len());

        for field in &unnamed_struct_type.fields {
            let ty = self.resolve_type(generic_params, field.ty.clone(), field.loc)?;

            fields.push(TypedUnnamedStructTypeField {
                name: field.name.value.clone(),
                ty: Box::new(ty),
                loc: field.loc,
            });
        }

        Some(fields)
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
                TypeArg::Positional(type_spec) => {
                    let loc = type_spec.loc();
                    let ty = self.resolve_type(generic_params, type_spec.clone(), loc)?;

                    Some(TypedTypeArg::Positional { i, ty, loc })
                }
                TypeArg::Named { key, ty } => {
                    let ty = self.resolve_type(generic_params, ty.clone(), loc)?;

                    Some(TypedTypeArg::Named {
                        key: key.as_string(),
                        ty,
                        loc: key.loc,
                    })
                }
            })
            .collect::<Option<_>>()
    }

    fn resolve_generic_params(&mut self, generic_params: &GenericParamsList) -> Option<TypedGenericParamsList> {
        let mut list = Vec::with_capacity(generic_params.len());

        for generic_param in generic_params {
            let bounds = self.resolve_generic_param_bounds(generic_param)?;
            let default = self.resolve_generic_param_default(generic_param)?;

            let name = Ident {
                value: generic_param.param_name.as_string(),
                loc: generic_param.param_name.loc,
            };

            list.push(TypedGenericParam { name, bounds, default });
        }

        Some(TypedGenericParamsList { list })
    }

    fn resolve_generic_param_bounds(&mut self, generic_param: &GenericParam) -> Option<Option<Vec<TypedBound>>> {
        let bounds_list = match &generic_param.bounds {
            Some(b) => b,
            None => return Some(None),
        };

        let mut typed_bounds = Vec::with_capacity(bounds_list.len());

        for bound in bounds_list {
            let type_args = self.resolve_type_args(&None, &bound.type_args, generic_param.param_name.loc)?;

            typed_bounds.push(TypedBound {
                symbol: bound.symbol.clone(),
                type_args,
            });
        }

        Some(Some(typed_bounds))
    }

    fn resolve_generic_param_default(&mut self, generic_param: &GenericParam) -> Option<Option<Box<SemanticType>>> {
        match &generic_param.default {
            Some(default_ty) => {
                let ty = self.resolve_type(&None, default_ty.clone(), generic_param.param_name.loc)?;

                Some(Some(Box::new(ty)))
            }
            None => Some(None),
        }
    }

    fn resolve_typedef(&mut self, typedef: &ASTTypedefStmt) -> Option<TypedStmt> {
        let symbol_id = self
            .lookup_symbol_id(self.current_scope.unwrap(), &typedef.ident.value)
            .unwrap();

        let generic_params = typedef
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        let sema_type = self.resolve_type(&generic_params, typedef.type_spec.clone(), typedef.loc)?;

        let typedef_sig = TypedefSig {
            name: typedef.ident.as_string(),
            generic_params: generic_params.clone(),
            ty: sema_type.clone(),
            vis: typedef.vis.clone(),
            loc: typedef.loc,
        };

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::Typedef(ResolvedTypedef { symbol_id, typedef_sig })
        });

        Some(TypedStmt::Typedef(TypedTypedefStmt {
            symbol_id,
            name: typedef.ident.as_string(),
            ty: sema_type,
            generic_params,
            vis: typedef.vis.clone(),
            loc: typedef.loc,
        }))
    }

    // ANCHOR: New feature
    #[allow(unused)]
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
    #[allow(unused)]
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

    // ANCHOR: New feature
    #[allow(unused)]
    fn resolve_builtin_scope(&self, builtin_scope: &BuiltinScope) -> Option<TypedBuiltin> {
        todo!();
    }

    fn resolve_interface_stmt(&mut self, interface: &ASTInterfaceStmt) -> Option<TypedStmt> {
        let name = interface.ident.as_string();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();
        let loc = interface.loc;

        let typed_generic_params = match &interface.generic_params {
            Some(params) => self.resolve_generic_params(params)?,
            None => TypedGenericParamsList { list: Vec::new() },
        };

        // Install these as active for method resolution
        self.current_object_generic_params = Some(typed_generic_params.clone());

        let mut typed_methods = Vec::with_capacity(interface.methods.len());

        for func_decl in &interface.methods {
            // Interfaces never allow renaming
            if func_decl.renamed_as.is_some() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::RenameInterfaceMethod),
                    loc: Some(func_decl.loc),
                    hint: None,
                });
                continue;
            }

            let resolved = self.resolve_func(func_decl, true)?;
            let (resolved_return_type, resolved_params_list, resolved_variadic_param, resolved_func_generic_params) =
                resolved;

            typed_methods.push(TypedFuncDeclStmt {
                symbol_id,
                name: func_decl.ident.as_string(),
                generic_params: resolved_func_generic_params,
                params: TypedFuncParams {
                    list: resolved_params_list,
                    variadic: resolved_variadic_param,
                },
                ret_type: resolved_return_type,
                modifiers: func_decl.modifiers.clone(),
                renamed_as: None,
                loc: func_decl.loc,
            });
        }

        let interface_sig = InterfaceSig {
            symbol_id,
            name: name.clone(),
            methods: typed_methods.clone(),
            generic_params: Some(typed_generic_params.clone()),
            vis: interface.vis.clone(),
            loc,
        };

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::Interface(ResolvedInterface {
                symbol_id,
                interface_sig,
            })
        });

        Some(TypedStmt::Interface(TypedInterfaceStmt {
            name,
            symbol_id,
            methods: typed_methods,
            generic_params: Some(typed_generic_params),
            vis: interface.vis.clone(),
            loc,
        }))
    }

    fn resolve_union_stmt(&mut self, union_decl: &ASTUnionStmt) -> Option<TypedStmt> {
        let name = union_decl.ident.as_string();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();
        let loc = union_decl.loc;

        self.current_object = Some(symbol_id);

        let generic_params = match &union_decl.generic_params {
            Some(params) => Some(self.resolve_generic_params(params)?),
            None => None,
        };

        self.current_object_generic_params = generic_params.clone();

        let mut typed_union_fields = Vec::with_capacity(union_decl.fields.len());

        for field in &union_decl.fields {
            let sema_type = match self.resolve_type(&generic_params, field.ty.clone(), field.loc) {
                Some(ty) => ty,
                None => continue,
            };

            typed_union_fields.push(TypedUnionField {
                name: field.ident.as_string(),
                ty: sema_type,
                loc: field.loc,
            });
        }

        self.check_duplicate_method_names(&name, union_decl.methods.clone());

        let methods = self.resolve_methods(&union_decl.methods, symbol_id, generic_params.is_some())?;

        let union_sig = UnionSig {
            symbol_id,
            name: name.clone(),
            fields: typed_union_fields.clone(),
            methods: methods.clone(),
            generic_params: generic_params.clone(),
            modifiers: union_decl.modifiers.clone(),
            align: union_decl.align.clone(),
            loc,
        };

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::Union(ResolvedUnion { symbol_id, union_sig })
        });

        let impls = self.resolve_object_implements_interface_list(&union_decl.impls, union_decl.loc);

        Some(TypedStmt::Union(TypedUnionStmt {
            symbol_id,
            name,
            fields: typed_union_fields,
            methods,
            generic_params,
            modifiers: union_decl.modifiers.clone(),
            impls,
            loc: union_decl.ident.loc,
            align: union_decl.align.clone(),
        }))
    }

    fn resolve_enum_stmt(&mut self, enum_decl: &ASTEnumStmt) -> Option<TypedStmt> {
        let name = enum_decl.ident.as_string();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();
        let loc = enum_decl.loc;

        self.current_object = Some(symbol_id);

        let generic_params = match &enum_decl.generic_params {
            Some(params) => Some(self.resolve_generic_params(params)?),
            None => None,
        };

        self.current_object_generic_params = generic_params.clone();

        let mut variants: Vec<TypedEnumVariant> = Vec::with_capacity(enum_decl.variants.len());

        for variant in &enum_decl.variants {
            let typed_variant = match variant {
                EnumVariant::Ident(ident) => TypedEnumVariant::Ident(ident.clone()),

                EnumVariant::Variant(ident, valued_fields) => {
                    let mut fields = Vec::with_capacity(valued_fields.len());

                    for valued_field in valued_fields {
                        let ty = match self.resolve_type(&generic_params, valued_field.ty.clone(), valued_field.loc) {
                            Some(ty) => ty,
                            None => continue,
                        };

                        fields.push(TypedEnumValuedField {
                            ty,
                            loc: valued_field.loc,
                        });
                    }

                    TypedEnumVariant::Variant(ident.clone(), fields)
                }

                EnumVariant::Valued(ident, expr) => {
                    let typed_expr = match self.resolve_expr(expr) {
                        Some(expr) => expr,
                        None => continue,
                    };

                    TypedEnumVariant::Valued(ident.clone(), Box::new(typed_expr))
                }
            };

            variants.push(typed_variant);
        }

        self.check_duplicate_method_names(&name, enum_decl.methods.clone());

        let methods = self.resolve_methods(&enum_decl.methods, symbol_id, generic_params.is_some())?;

        let tag_type = match &enum_decl.tag_type {
            Some(ty) => self.resolve_type(&None, ty.clone(), enum_decl.loc),
            None => None,
        };

        let enum_sig = EnumSig {
            symbol_id,
            name: name.clone(),
            methods: methods.clone(),
            variants: variants.clone(),
            generic_params: generic_params.clone(),
            tag_type: tag_type.clone(),
            modifiers: enum_decl.modifiers.clone(),
            align: enum_decl.align.clone(),
            loc,
        };

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::Enum(ResolvedEnum { symbol_id, enum_sig })
        });

        let impls = self.resolve_object_implements_interface_list(&enum_decl.impls, enum_decl.loc);

        Some(TypedStmt::Enum(TypedEnumStmt {
            symbol_id,
            name,
            variants,
            methods,
            generic_params,
            impls,
            tag_type,
            modifiers: enum_decl.modifiers.clone(),
            align: enum_decl.align.clone(),
            loc,
        }))
    }

    fn resolve_global_var_stmt(&mut self, global_var: &ASTGlobalVarStmt) -> Option<TypedStmt> {
        let name = global_var.ident.as_string();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();
        let loc = global_var.loc;

        let sema_type = match &global_var.type_spec {
            Some(ty) => self.resolve_type(&None, ty.clone(), loc),
            None => None,
        };

        let typed_expr = match &global_var.expr {
            Some(expr) => self.resolve_expr(expr),
            None => None,
        };

        let global_var_sig = GlobalVarSig {
            symbol_id,
            name: name.clone(),
            ty: sema_type.clone(),
            rhs: typed_expr.clone(),
            analyzed: true,
            is_const: global_var.is_const,
            modifiers: global_var.modifiers.clone(),
            loc,
        };

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::GlobalVar(ResolvedGlobalVar {
                symbol_id,
                global_var_sig,
            })
        });

        Some(TypedStmt::GlobalVar(TypedGlobalVarStmt {
            file_id: self.current_module_file_id.unwrap(),
            symbol_id,
            name,
            ty: sema_type,
            expr: typed_expr,
            modifiers: global_var.modifiers.clone(),
            is_const: global_var.is_const,
            loc,
        }))
    }

    fn check_duplicate_method_names(&mut self, struct_name: &str, methods_list: Vec<ASTFuncDefStmt>) {
        let mut method_names: Vec<String> = Vec::new();

        for func_def in methods_list {
            let method_name = func_def.ident.as_string();

            if method_names.contains(&method_name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::DuplicateMethodName {
                        struct_name: struct_name.to_string(),
                        method_name: method_name.clone(),
                    }),
                    loc: Some(func_def.loc),
                    hint: Some("Consider to rename the method to a different name.".to_string()),
                });
                continue;
            }

            method_names.push(method_name);
        }
    }

    fn resolve_methods(
        &mut self,
        methods_list: &[ASTFuncDefStmt],
        object_symbol_id: SymbolID,
        generic_object: bool,
    ) -> Option<HashMap<String, SymbolID>> {
        let mut methods = HashMap::with_capacity(methods_list.len());
        let mut method_bodies: HashMap<SymbolID, (LocalScope, &ASTBlockStmt, bool)> =
            HashMap::with_capacity(methods_list.len());

        for func_def in methods_list {
            let scope = LocalScope::new();

            let (ret_type, mut params, variadic, generic_params) = match with_local_scope!(self, scope.clone(), {
                self.resolve_func(&func_def.as_func_decl(), false)
            }) {
                Some(v) => v,
                None => continue,
            };

            let original_name = func_def.ident.as_string();
            let unique_name = unique_object_method_name(object_symbol_id, original_name.clone());

            for param in &mut params {
                if let TypedFuncParamKind::SelfModifier(self_mod) = param {
                    self_mod.symbol_id = Some(object_symbol_id);
                }
            }

            let method_id = self
                .global_symbols
                .insert_symbol_entry(SymbolEntry::unresolved(None, self.current_scope));

            self.global_symbols
                .insert_symbol_name(self.current_scope.unwrap(), method_id, &unique_name);

            methods.insert(original_name.clone(), method_id);

            let func_sig = FuncSig {
                symbol_id: Some(method_id),
                name: original_name,
                is_func_decl: false,
                generic_params: generic_params.clone(),
                params: TypedFuncParams { list: params, variadic },
                ret_type,
                modifiers: func_def.modifiers.clone(),
                loc: func_def.loc,
            };

            let is_generic = func_sig.is_generic() || generic_object;

            self.with_method_mut(method_id, |resolved_method| {
                resolved_method.func_sig = func_sig;
            });

            method_bodies.insert(method_id, (scope, &func_def.body, is_generic));
        }

        for (method_id, (scope, body, is_generic)) in method_bodies {
            let SymbolEntryKind::Method(mut resolved_method) = self.get_symbol_entry(method_id).unwrap().kind else {
                unreachable!();
            };

            with_local_scope!(self, scope, {
                for param in &mut resolved_method.func_sig.params.list {
                    if let TypedFuncParamKind::SelfModifier(self_modifier) = param {
                        let self_id = self
                            .global_symbols
                            .insert_symbol_entry(SymbolEntry::unresolved(None, self.current_scope));
                        self_modifier.self_id = Some(self_id);

                        let ty = match self_modifier.kind {
                            SelfModifierKind::Copied => Some(SemanticType::UnresolvedSymbol(object_symbol_id)),
                            SelfModifierKind::Referenced => Some(SemanticType::Pointer(Box::new(
                                SemanticType::UnresolvedSymbol(object_symbol_id),
                            ))),
                        };

                        let self_name = "self";

                        let self_var = TypedVarStmt {
                            symbol_id: self_id,
                            name: self_name.to_string(),
                            ty,
                            rhs: None,
                            is_const: false,
                            loc: resolved_method.func_sig.loc,
                        };

                        self.with_var_mut(self_id, |resolved_var| resolved_var.variable = self_var);

                        self.current_local_scope_mut()
                            .unwrap()
                            .insert(self_name.to_string().clone(), self_id);
                    }
                }

                let Some(typed_body) = self.resolve_block_stmt(&body) else {
                    return None;
                };

                if is_generic {
                    monomorph_registry!(self, ctx, {
                        ctx.register_template(method_id, typed_body);
                    });
                } else {
                    self.with_method_mut(method_id, |resolved_method| {
                        resolved_method.func_body = Some(Box::new(typed_body));
                    });
                }
            });
        }

        Some(methods)
    }

    fn resolve_object_implements_interface_list(
        &mut self,
        impls: &Vec<TypeSpecifier>,
        loc: Loc,
    ) -> Vec<TypedImplementInterface> {
        let mut symbol_ids: Vec<TypedImplementInterface> = Vec::new();

        for type_spec in impls {
            let (symbol_id, type_args) = match type_spec {
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
                    let base_symbol_id = match self
                        .resolve_type(&None, *generic_inst.base.clone(), loc)
                        .and_then(|sema_type| sema_type.as_unresolved_symbol())
                    {
                        Some(symbol_id) => symbol_id,
                        None => {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(ResolverDiagKind::InvalidImplementInterface),
                                loc: Some(loc),
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
                        loc: Some(loc),
                        hint: None,
                    });
                    return symbol_ids;
                }
            };

            symbol_ids.push(TypedImplementInterface {
                symbol_id,
                type_args,
                loc,
            });
        }

        symbol_ids
    }

    fn resolve_struct_stmt(&mut self, struct_decl: &ASTStructStmt) -> Option<TypedStmt> {
        let name = struct_decl.ident.as_string();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();

        self.current_object = Some(symbol_id);

        let generic_params = struct_decl
            .generic_params
            .as_ref()
            .and_then(|g| self.resolve_generic_params(g));

        self.current_object_generic_params = generic_params.clone();

        let typed_struct_fields: Vec<TypedStructField> = struct_decl
            .fields
            .iter()
            .filter_map(|field| {
                self.resolve_type(&generic_params, field.ty.clone(), field.loc)
                    .map(|ty| TypedStructField {
                        name: field.ident.as_string(),
                        vis: field.vis.clone(),
                        ty,
                        loc: field.loc,
                    })
            })
            .collect();

        self.check_duplicate_method_names(&struct_decl.ident.value, struct_decl.methods.clone());

        let methods = self.resolve_methods(&struct_decl.methods, symbol_id, generic_params.is_some())?;

        let impls = self.resolve_object_implements_interface_list(&struct_decl.impls, struct_decl.loc);

        let struct_sig = StructSig {
            symbol_id,
            name: struct_decl.ident.as_string(),
            fields: typed_struct_fields.clone(),
            generic_params: generic_params.clone(),
            impls: impls.clone(),
            methods: methods.clone(),
            modifiers: struct_decl.modifiers.clone(),
            align: struct_decl.align.clone(),
            loc: struct_decl.loc,
        };

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::Struct(ResolvedStruct { symbol_id, struct_sig })
        });

        Some(TypedStmt::Struct(TypedStructStmt {
            symbol_id,
            name: struct_decl.ident.as_string(),
            fields: typed_struct_fields,
            methods,
            generic_params,
            impls,
            modifiers: struct_decl.modifiers.clone(),
            align: struct_decl.align.clone(),
            is_packed: struct_decl.is_packed,
            loc: struct_decl.loc,
        }))
    }

    fn resolve_func(
        &mut self,
        func_decl: &ASTFuncDeclStmt,
        is_decl: bool,
    ) -> Option<(
        SemanticType,
        Vec<TypedFuncParamKind>,
        Option<TypedFuncVariadicParams>,
        Option<TypedGenericParamsList>,
    )> {
        let ret_type = return_type_or_default_void(func_decl.ret_type.clone(), func_decl.loc);

        let generic_params = func_decl
            .generic_params
            .clone()
            .and_then(|generic_params| self.resolve_generic_params(&generic_params));

        let typed_return_type = self.resolve_type(&generic_params, ret_type, func_decl.loc)?;

        let (typed_func_params, typed_variadic_param) =
            self.resolve_func_params(&generic_params, &func_decl.params, is_decl)?;

        Some((
            typed_return_type,
            typed_func_params,
            typed_variadic_param,
            generic_params,
        ))
    }

    fn resolve_func_params(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        params: &FuncParams,
        is_decl: bool,
    ) -> Option<(Vec<TypedFuncParamKind>, Option<TypedFuncVariadicParams>)> {
        let mut typed_params = Vec::with_capacity(params.list.len());

        for param in &params.list {
            match param {
                FuncParamKind::FuncParam(func_param) => {
                    let typed = self.resolve_func_param(generic_params, func_param, is_decl)?;
                    typed_params.push(TypedFuncParamKind::FuncParam(typed));
                }
                FuncParamKind::SelfModifier(self_modifier) => {
                    let typed = self.resolve_self_modifier_param(self_modifier);
                    typed_params.push(TypedFuncParamKind::SelfModifier(typed));
                }
            }
        }

        let variadic = match &params.variadic {
            Some(variadic) => self.resolve_func_variadic_param(variadic)?,
            None => None,
        };

        Some((typed_params, variadic))
    }

    fn resolve_func_param(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        param: &FuncParam,
        is_decl: bool,
    ) -> Option<TypedFuncParam> {
        let ty = match &param.ty {
            Some(spec) => self.resolve_type(generic_params, spec.clone(), param.loc)?,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::InvalidUntypedFuncParam),
                    loc: Some(param.loc),
                    hint: None,
                });
                return None;
            }
        };

        // TODO: Const func param not implemented yet.
        let is_const_param = false;

        let symbol_id = {
            if !is_decl {
                Some(self.insert_variable(&param.ident, Some(ty.clone()), is_const_param)?)
            } else {
                None
            }
        };

        Some(TypedFuncParam {
            symbol_id,
            name: param.ident.as_string(),
            ty,
            loc: param.loc,
        })
    }

    fn resolve_self_modifier_param(&mut self, self_modifier: &SelfModifier) -> TypedSelfModifier {
        TypedSelfModifier {
            symbol_id: None,
            self_id: None,
            ty: None,
            kind: self_modifier.kind.clone(),
            loc: self_modifier.loc,
        }
    }

    fn resolve_func_variadic_param(
        &mut self,
        variadic: &FuncVariadicParams,
    ) -> Option<Option<TypedFuncVariadicParams>> {
        match variadic {
            FuncVariadicParams::UntypedCStyle => Some(Some(TypedFuncVariadicParams::UntypedCStyle)),
            FuncVariadicParams::Typed(ident, type_spec) => {
                let ty = self.resolve_type(&None, type_spec.clone(), ident.loc)?;

                let symbol_id = self.insert_variable(ident, Some(ty.clone()), false)?;

                Some(Some(TypedFuncVariadicParams::Typed(
                    TypedIdent {
                        name: ident.as_string(),
                        symbol_id,
                        loc: ident.loc,
                    },
                    ty,
                )))
            }
        }
    }

    fn resolve_func_decl(&mut self, func_decl: &ASTFuncDeclStmt) -> Option<TypedStmt> {
        let name = func_decl.usable_name();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();

        let (ret_type, typed_func_params, typed_variadic_param, generic_params) = self.resolve_func(func_decl, true)?;

        let func_sig = FuncSig {
            symbol_id: Some(symbol_id),
            name,
            is_func_decl: true,
            generic_params: generic_params.clone(),
            params: TypedFuncParams {
                list: typed_func_params.clone(),
                variadic: typed_variadic_param.clone(),
            },
            ret_type: ret_type.clone(),
            modifiers: func_decl.modifiers.clone(),
            loc: func_decl.loc,
        };

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::Func(ResolvedFunc { symbol_id, func_sig })
        });

        Some(TypedStmt::FuncDecl(TypedFuncDeclStmt {
            symbol_id,
            name: func_decl.ident.as_string(),
            generic_params,
            params: TypedFuncParams {
                list: typed_func_params,
                variadic: typed_variadic_param,
            },
            ret_type,
            modifiers: func_decl.modifiers.clone(),
            renamed_as: func_decl.renamed_as.as_ref().map(|id| id.as_string()),
            loc: func_decl.loc,
        }))
    }

    fn resolve_func_def(&mut self, func_def: &ASTFuncDefStmt) -> Option<TypedStmt> {
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &func_def.ident.value)?;

        let scope = LocalScope::new();

        self.enter_local_scope(scope);

        let (ret_type, typed_func_params, typed_variadic_param, generic_params) =
            self.resolve_func(&func_def.as_func_decl(), false)?;

        let func_sig = FuncSig {
            symbol_id: Some(symbol_id),
            name: func_def.ident.as_string(),
            generic_params: generic_params.clone(),
            is_func_decl: false,
            params: TypedFuncParams {
                list: typed_func_params.clone(),
                variadic: typed_variadic_param.clone(),
            },
            ret_type: ret_type.clone(),
            modifiers: func_def.modifiers.clone(),
            loc: func_def.loc,
        };

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::Func(ResolvedFunc { symbol_id, func_sig })
        });

        let typed_func_body = self.resolve_block_stmt(&func_def.body)?;

        self.exit_local_scope();

        monomorph_registry!(self, ctx, {
            ctx.register_template(symbol_id, typed_func_body.clone());
        });

        Some(TypedStmt::FuncDef(TypedFuncDefStmt {
            symbol_id,
            name: func_def.ident.as_string(),
            generic_params,
            params: TypedFuncParams {
                list: typed_func_params,
                variadic: typed_variadic_param,
            },
            ret_type,
            modifiers: func_def.modifiers.clone(),
            loc: func_def.loc,
            body: Box::new(typed_func_body),
        }))
    }

    fn resolve_if_stmt(&mut self, if_stmt: &ASTIfStmt) -> Option<TypedIfStmt> {
        let cond = self.resolve_expr(&if_stmt.condition)?;

        let then_scope = LocalScope::new();
        self.enter_local_scope(then_scope);

        let then_block = Box::new(self.resolve_block_stmt(&if_stmt.then_block)?);
        self.exit_local_scope();

        let else_block = if let Some(block) = &if_stmt.else_block {
            let else_scope = LocalScope::new();
            self.enter_local_scope(else_scope);
            let block = self.resolve_block_stmt(block)?;
            self.exit_local_scope();
            Some(Box::new(block))
        } else {
            None
        };

        let mut branches = Vec::new();

        for else_if in &if_stmt.branches {
            if let Some(if_stmt) = self.resolve_if_stmt(else_if) {
                branches.push(if_stmt);
            }
        }

        Some(TypedIfStmt {
            cond,
            then_block,
            else_block,
            branches,
            loc: if_stmt.loc,
        })
    }

    fn resolve_block_stmt(&mut self, block_stmt: &ASTBlockStmt) -> Option<TypedBlockStmt> {
        let mut typed_body: Vec<TypedStmt> = Vec::new();
        let mut defers: Vec<TypedDeferStmt> = Vec::new();

        for stmt in &block_stmt.exprs {
            match stmt {
                ASTStmt::Defer(defer) => {
                    if let Some(typed_stmt) = self.resolve_stmt(&defer.operand) {
                        defers.push(TypedDeferStmt {
                            operand: Box::new(typed_stmt),
                            loc: defer.loc,
                        });
                    }
                }
                _ => {
                    if let Some(typed_stmt) = self.resolve_stmt(stmt) {
                        typed_body.push(typed_stmt);
                    }
                }
            }
        }

        Some(TypedBlockStmt {
            stmts: typed_body,
            defers,
            loc: block_stmt.loc,
        })
    }

    fn resolve_export_tuple_stmt(&mut self, export_tuple: &ASTExportTupleStmt) -> Option<TypedStmt> {
        let var_type = export_tuple
            .ty
            .as_ref()
            .and_then(|ty_spec| self.resolve_type(&None, ty_spec.clone(), export_tuple.loc));

        let typed_rhs = export_tuple.rhs.as_ref().and_then(|expr| self.resolve_expr(expr));

        let pattern = match &export_tuple.pattern {
            ExportPattern::Ident(ident) => {
                let symbol_id = self.insert_variable(ident, None, export_tuple.is_const)?;

                TypedExportPattern::Ident(symbol_id)
            }
            ExportPattern::Tuple(patterns) => {
                let mut typed_patterns = Vec::new();

                for sub_pattern in patterns {
                    match sub_pattern {
                        ExportPattern::Ident(ident) => {
                            let symbol_id = self.insert_variable(ident, None, export_tuple.is_const)?;

                            typed_patterns.push(TypedExportPattern::Ident(symbol_id));
                        }
                        ExportPattern::Tuple(inner) => {
                            let inner_stmt = self.resolve_export_tuple_stmt(&ASTExportTupleStmt {
                                pattern: ExportPattern::Tuple(inner.clone()),
                                ty: None,
                                rhs: None,
                                is_const: export_tuple.is_const,
                                loc: export_tuple.loc,
                            })?;

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
            loc: export_tuple.loc,
        }))
    }

    fn resolve_stmt(&mut self, stmt: &ASTStmt) -> Option<TypedStmt> {
        match stmt {
            ASTStmt::ExportTuple(export_tuple) => self.resolve_export_tuple_stmt(export_tuple),
            ASTStmt::Variable(variable) => self.resolve_var(variable).map(TypedStmt::Variable),
            ASTStmt::Builtin(builtin) => self.resolve_builtin(builtin).map(TypedStmt::Builtin),
            ASTStmt::Expr(expr) => self.resolve_expr(expr).map(TypedStmt::Expr),
            ASTStmt::If(if_stmt) => self.resolve_if_stmt(if_stmt).map(TypedStmt::If),
            ASTStmt::For(for_stmt) => self.resolve_for_stmt(for_stmt).map(TypedStmt::For),
            ASTStmt::While(while_stmt) => self.resolve_while_stmt(while_stmt).map(TypedStmt::While),
            ASTStmt::Switch(switch_stmt) => self.resolve_switch_stmt(switch_stmt).map(TypedStmt::Switch),
            ASTStmt::Enum(enum_decl) => self.resolve_enum_stmt(enum_decl),
            ASTStmt::Union(union_decl) => self.resolve_union_stmt(union_decl),
            ASTStmt::Interface(interface) => self.resolve_interface_stmt(interface),
            ASTStmt::Struct(struct_decl) => self.resolve_struct_stmt(struct_decl),
            ASTStmt::BlockStmt(block) => self.resolve_block_stmt(block).map(TypedStmt::BlockStmt),
            ASTStmt::Return(return_stmt) => self.resolve_return_stmt(return_stmt).map(TypedStmt::Return),
            ASTStmt::Break(break_stmt) => self.resolve_break_stmt(break_stmt).map(TypedStmt::Break),
            ASTStmt::Continue(continue_stmt) => self.resolve_continue_stmt(continue_stmt).map(TypedStmt::Continue),
            ASTStmt::Typedef(typedef) => self.resolve_typedef(typedef),
            ASTStmt::Label(label) => self.resolve_label_stmt(label),
            ASTStmt::Goto(goto) => self.resolve_goto_stmt(goto),
            ASTStmt::GlobalVar(_) | ASTStmt::FuncDef(_) | ASTStmt::FuncDecl(_) | ASTStmt::Import(_) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::InvalidStatement),
                    loc: Some(stmt.loc()),
                    hint: None,
                });
                None
            }
            ASTStmt::Defer(_) => unreachable!(),
            ASTStmt::Foreach(_foreach_stmt) => unimplemented!(),
        }
    }

    fn resolve_for_stmt(&mut self, for_stmt: &ASTForStmt) -> Option<TypedForStmt> {
        let scope = LocalScope::new();

        with_local_scope!(self, scope, {
            let initializer = if let Some(var) = &for_stmt.initializer {
                Some(self.resolve_var(var)?)
            } else {
                None
            };

            let cond = if let Some(expr) = &for_stmt.condition {
                Some(self.resolve_expr(expr)?)
            } else {
                None
            };

            let increment = if let Some(expr) = &for_stmt.increment {
                Some(self.resolve_expr(expr)?)
            } else {
                None
            };

            let body = Box::new(self.resolve_block_stmt(&for_stmt.body)?);

            Some(TypedForStmt {
                initializer,
                cond,
                increment,
                body,
                loc: for_stmt.loc,
            })
        })
    }

    fn resolve_while_stmt(&mut self, while_stmt: &ASTWhileStmt) -> Option<TypedWhileStmt> {
        let scope = LocalScope::new();

        with_local_scope!(self, scope, {
            let cond = self.resolve_expr(&while_stmt.condition)?;
            let body = Box::new(self.resolve_block_stmt(&while_stmt.body)?);

            Some(TypedWhileStmt {
                cond,
                body,
                loc: while_stmt.loc,
            })
        })
    }

    fn resolve_switch_stmt(&mut self, switch: &ASTSwitchStmt) -> Option<TypedSwitchStmt> {
        let operand = self.resolve_expr(&switch.operand)?;
        let cases = self.resolve_switch_cases(&switch.cases)?;
        let default_case = self.resolve_switch_default_case(&switch.default_case)?;

        Some(TypedSwitchStmt {
            operand,
            cases,
            default_case,
            loc: switch.loc,
        })
    }

    fn resolve_switch_cases(&mut self, cases: &[SwitchCase]) -> Option<Vec<TypedSwitchCase>> {
        let mut typed_cases = Vec::with_capacity(cases.len());

        for case in cases {
            typed_cases.push(self.resolve_switch_case(case)?);
        }

        Some(typed_cases)
    }

    fn resolve_switch_case(&mut self, case: &SwitchCase) -> Option<TypedSwitchCase> {
        let case_scope = LocalScope::new();

        with_local_scope!(self, case_scope, {
            let patterns = self.resolve_switch_patterns(&case.patterns)?;
            let body = Box::new(self.resolve_block_stmt(&case.body)?);

            Some(TypedSwitchCase {
                patterns,
                body,
                loc: case.loc,
            })
        })
    }

    fn resolve_switch_patterns(&mut self, patterns: &[SwitchCasePattern]) -> Option<Vec<TypedSwitchCasePattern>> {
        let mut typed_patterns = Vec::with_capacity(patterns.len());

        for pattern in patterns {
            typed_patterns.push(self.resolve_switch_pattern(pattern)?);
        }

        Some(typed_patterns)
    }

    fn resolve_switch_pattern(&mut self, pattern: &SwitchCasePattern) -> Option<TypedSwitchCasePattern> {
        match pattern {
            SwitchCasePattern::Ident(ident) => Some(TypedSwitchCasePattern::Ident(ident.clone())),
            SwitchCasePattern::Expr(expr) => {
                let typed_expr = self.resolve_expr(expr)?;

                Some(TypedSwitchCasePattern::Expr(typed_expr))
            }
            SwitchCasePattern::EnumVariant(ident, valued_fields) => {
                let mut fields = Vec::with_capacity(valued_fields.len());

                for ident in valued_fields {
                    let symbol_id = self.insert_variable(ident, None, false)?;

                    fields.push(TypedIdent {
                        name: ident.as_string(),
                        symbol_id,
                        loc: ident.loc,
                    });
                }

                Some(TypedSwitchCasePattern::EnumVariant(ident.clone(), fields, ident.loc))
            }

            SwitchCasePattern::Range(range) => {
                let lower = self.resolve_expr(&range.lower)?;
                let upper = self.resolve_expr(&range.upper)?;

                Some(TypedSwitchCasePattern::Range(TypedRange {
                    lower,
                    upper,
                    inclusive_upper: range.inclusive_upper,
                    loc: range.loc,
                }))
            }
        }
    }

    fn resolve_switch_default_case(&mut self, default_case: &Option<ASTBlockStmt>) -> Option<Option<TypedBlockStmt>> {
        if let Some(default_case) = default_case {
            let scope = LocalScope::new();
            let body = with_local_scope!(self, scope, { self.resolve_block_stmt(default_case) })?;

            Some(Some(body))
        } else {
            Some(None)
        }
    }

    fn resolve_var(&mut self, variable: &ASTVarStmt) -> Option<TypedVarStmt> {
        let ty = variable
            .ty
            .as_ref()
            .and_then(|type_spec| self.resolve_type(&None, type_spec.clone(), variable.loc));

        let typed_rhs = variable.rhs.as_ref().and_then(|expr| self.resolve_expr(expr));

        let symbol_id = self.insert_variable(&variable.ident, ty.clone(), variable.is_const)?;

        Some(TypedVarStmt {
            symbol_id,
            name: variable.ident.as_string(),
            ty,
            rhs: typed_rhs,
            is_const: variable.is_const,
            loc: variable.loc,
        })
    }

    fn resolve_continue_stmt(&mut self, cont: &ASTContinueStmt) -> Option<TypedContinueStmt> {
        Some(TypedContinueStmt { loc: cont.loc })
    }

    fn resolve_break_stmt(&mut self, break_stmt: &ASTBreakStmt) -> Option<TypedBreakStmt> {
        Some(TypedBreakStmt { loc: break_stmt.loc })
    }

    fn resolve_return_stmt(&mut self, ret: &ASTReturnStmt) -> Option<TypedReturnStmt> {
        let arg = if let Some(argument) = &ret.argument {
            Some(self.resolve_expr(argument)?)
        } else {
            None
        };

        Some(TypedReturnStmt { arg, loc: ret.loc })
    }

    fn resolve_goto_stmt(&self, goto: &ASTGotoStmt) -> Option<TypedStmt> {
        Some(TypedStmt::Goto(TypedGotoStmt {
            name: goto.name.as_string(),
            label_id: None,
            loc: goto.loc,
        }))
    }

    fn resolve_label_stmt(&mut self, label: &ASTLabelStmt) -> Option<TypedStmt> {
        let name = label.name.as_string();

        if self.current_local_scope().unwrap().contains_label(&name) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::LabelAlreadyDefined {
                    label_name: label.name.to_string(),
                }),
                loc: Some(label.loc),
                hint: None,
            });

            return None;
        }

        let label_id = self.id_gen.label_id();

        self.current_local_scope_mut()
            .unwrap()
            .insert_label(name.clone(), label_id);

        Some(TypedStmt::Label(TypedLabelStmt {
            name,
            label_id,
            loc: label.loc,
        }))
    }

    fn resolve_module_import_expr(&mut self, module_import: &ASTModuleImport) -> Option<TypedExprStmt> {
        if let Some(ident) = module_import.as_ident() {
            self.resolve_ident_expr(&ident)
        } else {
            self.resolve_module_import(module_import.clone())
                .map(|symbol_id| TypedExprStmt {
                    kind: TypedExprKind::Symbol(TypedSymbolExpr::new(symbol_id, module_import.loc)),
                    sema_type: None,
                    mloc: MemoryLocation::LValue,
                    loc: module_import.loc,
                })
        }
    }

    fn resolve_unnamed_union_value(&mut self, unnamed_union_value: &ASTUnnamedUnionValueExpr) -> Option<TypedExprStmt> {
        let field_value = self.resolve_expr(&unnamed_union_value.field_value)?;

        let kind = TypedExprKind::UnnamedUnionValue(TypedUnnamedUnionValue {
            field_name: unnamed_union_value.field_name.clone(),
            field_value: Box::new(field_value),
            union_ty: None,
            is_const: unnamed_union_value.is_const,
            loc: unnamed_union_value.loc,
        });

        Some(TypedExprStmt {
            kind,
            sema_type: None,
            mloc: MemoryLocation::RValue,
            loc: unnamed_union_value.loc,
        })
    }

    fn resolve_unnamed_enum_value(&mut self, unnamed_enum_value: &ASTUnnamedEnumValueExpr) -> Option<TypedExprStmt> {
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
                loc: unnamed_enum_value.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_type: None,
            loc: unnamed_enum_value.loc,
        })
    }

    fn resolve_dynamic_expr(&mut self, dynamic: &ASTDynamicExpr) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&dynamic.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Dynamic(TypedDynamicExpr {
                operand: Box::new(operand),
                vtable_id: None,
                object_name: None,
                loc: dynamic.loc,
            }),
            sema_type: None,
            mloc: MemoryLocation::RValue,
            loc: dynamic.loc,
        })
    }

    fn resolve_tuple_member_access(&mut self, tuple_member_access: &ASTTupleAccessExpr) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&tuple_member_access.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::TupleAccess(TypedTupleAccessExpr {
                operand: Box::new(operand),
                index: tuple_member_access.index,
                loc: tuple_member_access.loc,
            }),
            sema_type: None,
            mloc: MemoryLocation::LValue,
            loc: tuple_member_access.loc,
        })
    }

    fn resolve_tuple_expr(&mut self, tuple_value: &ASTTupleValueExpr) -> Option<TypedExprStmt> {
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
                loc: tuple_value.loc,
            }),
            sema_type: None,
            mloc: MemoryLocation::RValue,
            loc: tuple_value.loc,
        })
    }

    fn resolve_lambda_expr(&mut self, lambda: &ASTLambdaExpr) -> Option<TypedExprStmt> {
        let scope = LocalScope::new();

        with_local_scope!(self, scope, {
            let (list, variadic) = self.resolve_func_params(&None, &lambda.params, false)?;

            let body = match self.resolve_block_stmt(&lambda.body) {
                Some(block) => Box::new(block),
                None => return None,
            };

            let ret_type = self.resolve_type(&None, lambda.ret_type.clone(), lambda.loc)?;

            Some(TypedExprStmt {
                kind: TypedExprKind::Lambda(TypedLambdaExpr {
                    params: TypedFuncParams { list, variadic },
                    body,
                    ret_type,
                    inline: lambda.inline,
                    loc: lambda.loc,
                }),
                sema_type: None,
                mloc: MemoryLocation::RValue,
                loc: lambda.loc,
            })
        })
    }

    fn resolve_field_access(&mut self, field_access: &ASTFieldAccessExpr) -> Option<TypedExprStmt> {
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
                loc: field_access.loc,
            }),
            mloc: MemoryLocation::LValue,
            sema_type: None,
            loc: field_access.loc,
        })
    }

    fn resolve_method_call(&mut self, method_call: &ASTMethodCallExpr) -> Option<TypedExprStmt> {
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
                monomorph_id: None,
                self_ty: None,
                enum_constructor: None,
                method_call_on_interface: None,
                loc: method_call.loc,
                args,
            }),
            mloc: MemoryLocation::RValue,
            sema_type: None,
            loc: method_call.loc,
        })
    }

    fn resolve_struct_init(&mut self, struct_init: &ASTStructInitExpr) -> Option<TypedExprStmt> {
        let symbol_id = self.resolve_local_module_import(&struct_init.struct_name)?;

        let field_inits: Vec<TypedStructFieldInit> = struct_init
            .field_inits
            .iter()
            .filter_map(|field_init| {
                self.resolve_expr(&field_init.value).map(|value| TypedStructFieldInit {
                    name: field_init.ident.as_string(),
                    value,
                    loc: field_init.loc,
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
                loc: struct_init.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_type: None,
            loc: struct_init.loc,
        })
    }

    fn resolve_func_call(&mut self, func_call: &ASTFuncCallExpr) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&func_call.operand)?;

        let args: Vec<TypedExprStmt> = func_call.args.iter().filter_map(|arg| self.resolve_expr(arg)).collect();

        let type_args = func_call
            .type_args
            .as_ref()
            .and_then(|type_args| self.resolve_type_args(&None, type_args, func_call.loc));

        let loc = func_call.loc;

        Some(TypedExprStmt {
            kind: TypedExprKind::FuncCall(TypedFuncCall {
                operand: Box::new(operand),
                args,
                type_args,
                ret_type: None,
                monomorph_id: None,
                loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_type: None,
            loc,
        })
    }

    fn resolve_untyped_array_expr(&mut self, untyped_array: &ASTUntypedArrayExpr) -> Option<TypedExprStmt> {
        let elements: Vec<TypedExprStmt> = untyped_array
            .elements
            .iter()
            .filter_map(|item| self.resolve_expr(item))
            .collect();

        Some(TypedExprStmt {
            kind: TypedExprKind::Array(TypedArrayExpr {
                ty: None,
                elements,
                loc: untyped_array.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_type: None,
            loc: untyped_array.loc,
        })
    }

    fn resolve_array_expr(&mut self, array: &ASTArrayExpr) -> Option<TypedExprStmt> {
        let array_type = self.resolve_type(&None, array.data_type.clone(), array.loc)?;

        let typed_elements: Vec<TypedExprStmt> = array
            .elements
            .iter()
            .filter_map(|item| self.resolve_expr(item))
            .collect();

        Some(TypedExprStmt {
            kind: TypedExprKind::Array(TypedArrayExpr {
                ty: Some(array_type),
                elements: typed_elements,
                loc: array.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_type: None,
            loc: array.loc,
        })
    }

    fn resolve_infix_expr(&mut self, infix: &ASTInfixExpr) -> Option<TypedExprStmt> {
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
            sema_type: None,
            loc: infix.loc,
        })
    }

    fn resolve_prefix_expr(&mut self, prefix: &ASTPrefixExpr) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&*prefix.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Prefix(TypedPrefixExpr {
                operand: Box::new(operand),
                op: prefix.op.clone(),
                loc: prefix.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_type: None,
            loc: prefix.loc,
        })
    }

    fn resolve_type_specifier_expr(&mut self, type_spec: &TypeSpecifier) -> Option<TypedExprStmt> {
        let loc = type_spec.loc();

        let symbol_id = match type_spec {
            TypeSpecifier::Ident(ident) => self.resolve_ident(&ident)?,
            TypeSpecifier::ModuleImport(module_import) => self.resolve_module_import(module_import.clone())?,
            _ => {
                let sema_type = self.resolve_type(&None, type_spec.clone(), loc)?;
                return Some(TypedExprStmt {
                    kind: TypedExprKind::SemanticType(sema_type.clone()),
                    mloc: MemoryLocation::RValue,
                    sema_type: Some(sema_type),
                    loc,
                });
            }
        };

        Some(TypedExprStmt {
            kind: TypedExprKind::Symbol(TypedSymbolExpr::new(symbol_id, loc)),
            mloc: MemoryLocation::LValue,
            sema_type: None,
            loc,
        })
    }

    fn resolve_assign_expr(&mut self, assign: &ASTAssignExpr) -> Option<TypedExprStmt> {
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
            sema_type: None,
            loc: assign.loc,
        })
    }

    fn resolve_literal_expr(&mut self, literal: &ASTLiteralExpr) -> Option<TypedExprStmt> {
        let literal_type = self.resolve_literal_type(literal)?;

        let typed_literal = TypedLiteralExpr {
            ty: literal_type,
            kind: literal.kind.clone(),
            loc: literal.loc,
        };

        Some(TypedExprStmt {
            kind: TypedExprKind::Literal(typed_literal.clone()),
            sema_type: None,
            mloc: MemoryLocation::RValue,
            loc: typed_literal.loc,
        })
    }

    fn resolve_literal_type(&mut self, literal: &ASTLiteralExpr) -> Option<Option<SemanticType>> {
        match &literal.kind {
            LiteralKind::Integer(_, suffix_opt) | LiteralKind::Float(_, suffix_opt) => {
                self.resolve_number_literal_type(suffix_opt, literal.loc)
            }

            LiteralKind::String(value, prefix) => Some(self.resolve_string_literal_type(value, prefix, literal.loc)),

            LiteralKind::Bool(_) | LiteralKind::Char(_) | LiteralKind::Null => {
                Some(self.resolve_plain_literal_type(&literal.kind))
            }
        }
    }

    fn resolve_number_literal_type(
        &mut self,
        suffix_opt: &Option<Box<TokenKind>>,
        loc: Loc,
    ) -> Option<Option<SemanticType>> {
        if let Some(token_kind) = suffix_opt {
            match SemanticType::try_from(*token_kind.clone()) {
                Ok(sema_type) => Some(Some(sema_type)),
                Err(_) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::InvalidLiteralSuffix),
                        loc: Some(loc),
                        hint: None,
                    });
                    None
                }
            }
        } else {
            Some(None)
        }
    }

    fn resolve_string_literal_type(
        &mut self,
        string_value: &String,
        string_prefix: &Option<StringPrefix>,
        loc: Loc,
    ) -> Option<SemanticType> {
        match string_prefix {
            Some(StringPrefix::B) => {
                let len = string_value.len() + 1;
                let len_expr = literal_expr_from_const_int(len.try_into().unwrap(), loc);

                Some(SemanticType::Array(TypedArrayType {
                    element_type: Box::new(SemanticType::PlainType(PlainType::Char)),
                    capacity: TypedArrayCapacity::Fixed(Box::new(len_expr)),
                    loc,
                }))
            }

            Some(StringPrefix::C) => Some(SemanticType::Pointer(Box::new(SemanticType::PlainType(
                PlainType::Char,
            )))),

            None => Some(SemanticType::Pointer(Box::new(SemanticType::PlainType(
                PlainType::Char,
            )))),
        }
    }

    fn resolve_plain_literal_type(&self, kind: &LiteralKind) -> Option<SemanticType> {
        match kind {
            LiteralKind::Bool(_) => Some(SemanticType::PlainType(PlainType::Bool)),
            LiteralKind::Char(_) => Some(SemanticType::PlainType(PlainType::Char)),
            LiteralKind::Null => Some(SemanticType::Pointer(Box::new(SemanticType::PlainType(
                PlainType::Void,
            )))),
            _ => None,
        }
    }

    fn resolve_unary_expr(&mut self, unary: &ASTUnaryExpr) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&*unary.operand)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Unary(TypedUnaryExpr {
                op: unary.op.clone(),
                operand: Box::new(operand),
                loc: unary.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_type: None,
            loc: unary.loc,
        })
    }

    fn resolve_array_index_expr(&mut self, array_index: &ASTArrayIndexExpr) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&array_index.operand)?;
        let index = self.resolve_expr(&array_index.index)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::ArrayIndex(TypedArrayIndexExpr {
                operand: Box::new(operand),
                index: Box::new(index),
                loc: array_index.loc,
            }),
            mloc: MemoryLocation::LValue,
            sema_type: None,
            loc: array_index.loc,
        })
    }

    fn resolve_addr_of_expr(&mut self, addr_of: &ASTAddrOfExpr) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&addr_of.expr)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::AddrOf(TypedAddrOfExpr {
                operand: Box::new(operand),
                loc: addr_of.loc,
            }),
            mloc: MemoryLocation::LValue,
            sema_type: None,
            loc: addr_of.loc,
        })
    }

    fn resolve_deref_expr(&mut self, deref: &ASTDerefExpr) -> Option<TypedExprStmt> {
        let operand = self.resolve_expr(&deref.expr)?;

        Some(TypedExprStmt {
            kind: TypedExprKind::Deref(TypedDerefExpr {
                operand: Box::new(operand),
                loc: deref.loc,
            }),
            mloc: MemoryLocation::RValue,
            sema_type: None,
            loc: deref.loc,
        })
    }

    fn resolve_unnamed_struct_value(
        &mut self,
        unnamed_struct_value: &ASTUnnamedStructValueExpr,
    ) -> Option<TypedExprStmt> {
        let mut fields: Vec<TypedUnnamedStructValueField> = Vec::new();

        for field in &unnamed_struct_value.fields {
            let name = field.field_name.as_string();

            let ty = if let Some(type_spec) = &field.field_ty {
                match self.resolve_type(&None, type_spec.clone(), field.loc) {
                    Some(sema_type) => Some(sema_type),
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
            sema_type: None,
            loc: unnamed_struct_value.loc,
        })
    }
}

// Resolver helper methods.
impl Resolver {
    fn insert_variable(&mut self, ident: &Ident, ty: Option<SemanticType>, is_const: bool) -> Option<SymbolID> {
        let name = ident.as_string();

        if self.current_local_scope().unwrap().contains(&name) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::DuplicateSymbolInThisScope {
                    symbol_name: ident.as_string(),
                }),
                loc: Some(ident.loc),
                hint: None,
            });

            return None;
        }

        // allocate placeholder entry
        let symbol_id = self
            .global_symbols
            .insert_symbol_entry(SymbolEntry::unresolved(None, self.current_scope));

        let variable = TypedVarStmt {
            symbol_id,
            name: name.clone(),
            ty,
            rhs: None,
            is_const,
            loc: ident.loc,
        };

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::Var(ResolvedVar { symbol_id, variable })
        });

        // insert into current scope
        self.current_local_scope_mut().unwrap().insert(name.clone(), symbol_id);

        Some(symbol_id)
    }

    fn report_if_duplicate_symbol(&mut self, scope_id: SymbolID, symbol_name: String, loc: Loc) -> bool {
        match self.lookup_symbol_id_in_scope(scope_id, &symbol_name) {
            Some(_) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::DuplicateSymbol { symbol_name }),
                    loc: Some(loc),
                    hint: None,
                });
                true
            }
            None => false,
        }
    }
}

/// Constructs a unique name for an object method by prefixing with the object's SymbolID.
/// This allows the resolver to differentiate between for example `Point.distance()` and `Circle.distance()`.
fn unique_object_method_name(object_id: SymbolID, method_name: String) -> String {
    format!("object${}.{}", object_id, method_name)
}
