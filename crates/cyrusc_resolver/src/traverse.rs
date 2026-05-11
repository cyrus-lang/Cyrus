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
use cyrusc_ast::abi::Visibility;
use cyrusc_ast::format::format_module_segments;
use cyrusc_ast::modifiers::EnumModifiers;
use cyrusc_ast::modifiers::FuncModifiers;
use cyrusc_ast::modifiers::StructModifiers;
use cyrusc_ast::modifiers::UnionModifiers;
use cyrusc_ast::*;
use cyrusc_diagcentral::Diag;
use cyrusc_diagcentral::DiagLevel;
use cyrusc_internal::local_scope::LocalScope;
use cyrusc_internal::symbols::SymbolQuery;
use cyrusc_internal::symbols::symbols::*;
use cyrusc_source_loc::Loc;
use cyrusc_tokens::Token;
use cyrusc_tokens::TokenKind;
use cyrusc_tokens::literals::{ASTLiteralExpr, LiteralKind, StringPrefix};
use cyrusc_typed_ast::builtins::TypedBuiltin;
use cyrusc_typed_ast::builtins::TypedBuiltinBlock;
use cyrusc_typed_ast::builtins::TypedBuiltinFunc;
use cyrusc_typed_ast::builtins::TypedBuiltinKind;
use cyrusc_typed_ast::builtins::TypedBuiltinPhase;
use cyrusc_typed_ast::builtins::builtin_spec_of;
use cyrusc_typed_ast::builtins::lookup_builtin;
use cyrusc_typed_ast::decls::*;
use cyrusc_typed_ast::exprs::*;
use cyrusc_typed_ast::stmts::*;
use cyrusc_typed_ast::types::*;
use cyrusc_typed_ast::*;
use fx_hash::FxHashSet;
use fx_hash::FxHashSetExt;

// Resolver endpoints.
impl<'a> Resolver<'a> {
    // Scans the top-level AST for declarations (typedefs, functions, structs, etc.)
    // And Registers each declared name into the current module’s symbol table. (first pass)
    pub(crate) fn resolve_decl_names(&mut self, stmts: &[ASTStmt]) {
        for stmt in stmts {
            if let ASTStmt::ModuleDecl(module_decl) = stmt {
                self.resolve_module_decl_names(module_decl);
                continue;
            }

            if let ASTStmt::Builtin(builtin) = stmt {
                if let Builtin::BuiltinBlock(builtin_block) = builtin {
                    if !self.is_builtin_active(&builtin_block.name.value) {
                        continue;
                    }

                    if let Some(builtin_kind) = lookup_builtin(&builtin_block.name.value) {
                        let builtin_spec = builtin_spec_of(builtin_kind);

                        if builtin_spec.phase == TypedBuiltinPhase::Resolver {
                            self.resolve_decl_names(&builtin_block.block.stmts);
                        }
                    }
                }
                continue;
            }

            // skip statement if it's not a declaration symbol
            let Some(decl_name) = stmt.decl_name() else {
                continue;
            };

            if self.report_if_duplicate_symbol(self.current_scope.unwrap(), decl_name.as_string(), stmt.loc()) {
                continue;
            }

            let symbol_id = self.global_symbols.insert_symbol_entry(SymbolEntry::unresolved(
                stmt.vis(),
                self.current_scope,
                Some(stmt.loc()),
            ));

            self.global_symbols
                .insert_symbol_name(self.current_scope.unwrap(), symbol_id, &decl_name.value);
        }
    }

    // Resolves the full meaning of each top-level declaration in the AST (second pass)
    pub(crate) fn resolve_decl_full(&mut self, ast: &ProgramTree) -> Vec<TypedStmt> {
        let mut typed_body = Vec::new();

        for stmt in ast.body.as_ref() {
            let resolved = self.resolve_toplevel_stmt(stmt);

            typed_body.extend(resolved);
        }

        typed_body
    }

    fn resolve_toplevel_stmt(&mut self, stmt: &ASTStmt) -> Vec<TypedStmt> {
        match stmt {
            ASTStmt::ModuleDecl(module_decl) => {
                return self.resolve_module_decl(module_decl);
            }
            ASTStmt::Import(..) | ASTStmt::Expr(..) => {
                return Vec::new();
            }
            ASTStmt::Builtin(builtin) => {
                if let Some(stmt) = self.resolve_builtin(builtin).map(TypedStmt::Builtin) {
                    return vec![stmt];
                }
                return Vec::new();
            }
            ASTStmt::GlobalVar(global_var) => {
                if let Some(stmt) = self.resolve_global_var_stmt(global_var) {
                    return vec![stmt];
                }
                return Vec::new();
            }
            ASTStmt::Typedef(typedef) => {
                if let Some(stmt) = self.resolve_typedef(typedef) {
                    return vec![stmt];
                }
                return Vec::new();
            }
            ASTStmt::FuncDef(func_def) => {
                if let Some(stmt) = self.resolve_func_def(func_def) {
                    return vec![stmt];
                }
                return Vec::new();
            }
            ASTStmt::FuncDecl(func_decl) => {
                if let Some(stmt) = self.resolve_func_decl(func_decl) {
                    return vec![stmt];
                }
                return Vec::new();
            }
            ASTStmt::Struct(struct_) => {
                if let Some(stmt) = self.resolve_struct_stmt(struct_) {
                    return vec![stmt];
                }
                return Vec::new();
            }
            ASTStmt::Enum(enum_) => {
                if let Some(stmt) = self.resolve_enum_stmt(enum_) {
                    return vec![stmt];
                }
                return Vec::new();
            }
            ASTStmt::Union(union_) => {
                if let Some(stmt) = self.resolve_union_stmt(union_) {
                    return vec![stmt];
                }
                return Vec::new();
            }
            ASTStmt::Interface(interface) => {
                if let Some(stmt) = self.resolve_interface_stmt(interface) {
                    return vec![stmt];
                }
                return Vec::new();
            }
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::InvalidTopLevelStatement),
                    loc: Some(stmt.loc()),
                    hint: None,
                });
                Vec::new()
            }
        }
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
            ASTStmt::Foreach(_foreach_stmt) => unimplemented!(), // TODO

            // invalid statements
            ASTStmt::GlobalVar(_)
            | ASTStmt::FuncDef(_)
            | ASTStmt::FuncDecl(_)
            | ASTStmt::Import(_)
            | ASTStmt::ModuleDecl(_) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::InvalidStatement),
                    loc: Some(stmt.loc()),
                    hint: None,
                });
                None
            }
            ASTStmt::Defer(_) => unreachable!(),
        }
    }

    fn resolve_module_decl_names(&mut self, module_decl: &ASTModuleDecl) {
        let parent_scope_id = self.current_scope.unwrap();

        let module_scope_id = {
            if let Some(module_symbol_id) = self.lookup_symbol_id_in_scope(parent_scope_id, &module_decl.ident.value) {
                module_symbol_id
            } else {
                self.global_symbols.insert_namespace_symbol(
                    parent_scope_id,
                    &module_decl.ident.value,
                    module_decl.loc,
                    Some(module_decl.vis),
                )
            }
        };

        self.enter_scope_table(module_scope_id);
        self.resolve_decl_names(&module_decl.stmts);
        self.exit_scope_table();
    }

    fn resolve_module_decl(&mut self, module_decl: &ASTModuleDecl) -> Vec<TypedStmt> {
        let mut module_decl_stmts = Vec::new();

        let parent_scope_id = self.current_scope.unwrap();
        let module_scope_id = self
            .lookup_symbol_id_in_scope(parent_scope_id, &module_decl.ident.value)
            .unwrap();

        self.enter_scope_table(module_scope_id);
        for stmt in &module_decl.stmts {
            module_decl_stmts.extend(self.resolve_toplevel_stmt(stmt));
        }
        self.exit_scope_table();
        module_decl_stmts
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

            self.report_if_symbol_is_private(next_symbol, module_import.loc);

            current_symbol = next_symbol;
            i += 1;
        }

        Some(current_symbol)
    }

    #[inline]
    fn resolve_ident_expr(&mut self, ident: &Ident) -> Option<TypedExpr> {
        if let Some(symbol_id) = self.resolve_ident(ident) {
            return Some(TypedExpr {
                kind: TypedExprKind::Symbol(TypedSymbolExpr::Unresolved {
                    symbol_id,
                    loc: ident.loc,
                }),
                ty: None,
                val_cat: ValueCategory::Unknown,
                loc: ident.loc,
            });
        }

        if let Some(generic_param_id) = self.resolve_generic_param_as_type(&ident) {
            return Some(TypedExpr {
                kind: TypedExprKind::SemaType(SemaType::GenericParam(generic_param_id)),
                ty: None,
                val_cat: ValueCategory::Unknown,
                loc: ident.loc,
            });
        }

        None
    }

    fn resolve_expr(&mut self, expr: &ASTExpr) -> Option<TypedExpr> {
        match expr {
            ASTExpr::Ident(ident) => self.resolve_ident_expr(ident),
            ASTExpr::ModuleImport(module_import) => self.resolve_module_import_expr(module_import),

            ASTExpr::Infix(infix_expr) => self.resolve_infix_expr(infix_expr),
            ASTExpr::Prefix(prefix_expr) => self.resolve_prefix_expr(prefix_expr),
            ASTExpr::Unary(unary) => self.resolve_unary_expr(unary),
            ASTExpr::Assign(assignment) => self.resolve_assign_expr(assignment),
            ASTExpr::FieldAccess(field_access) => self.resolve_field_access(field_access),
            ASTExpr::MethodCall(method_call) => self.resolve_method_call(method_call),
            ASTExpr::StructInit(struct_init) => self.resolve_struct_init(struct_init),
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
            ASTExpr::EnumStructVariantInit(struct_variant_init) => {
                self.resolve_enum_struct_variant_init(struct_variant_init)
            }
            ASTExpr::UnnamedUnionValue(unnamed_union_value) => self.resolve_unnamed_union_value(unnamed_union_value),
            ASTExpr::UnnamedStructValue(unnamed_struct_value) => {
                self.resolve_unnamed_struct_value(unnamed_struct_value)
            }

            ASTExpr::Builtin(builtin) => match self.resolve_builtin(builtin) {
                Some(typed_builtin) => {
                    let kind = TypedExprKind::Builtin(typed_builtin);

                    Some(TypedExpr {
                        kind,
                        ty: None,
                        val_cat: ValueCategory::Unknown,
                        loc: builtin.loc(),
                    })
                }
                None => None,
            },

            ASTExpr::TypeSpecifier(type_spec) => self.resolve_type_specifier_expr(type_spec),
        }
    }

    fn resolve_type(&mut self, type_spec: TypeSpecifier, loc: Loc) -> Option<SemaType> {
        match type_spec {
            TypeSpecifier::Ident(ident) => {
                if ident.value == "Self" {
                    return Some(SemaType::SelfType(TypedSelfType { loc: ident.loc }));
                }

                self.resolve_ident_type(ident)
            }
            TypeSpecifier::ModuleImport(import) => self.resolve_module_import_type(import),
            TypeSpecifier::TypeToken(token) => self.resolve_builtin_type(token, loc),
            TypeSpecifier::Const(inner) => {
                let inner = self.resolve_type(*inner, loc)?;
                Some(SemaType::Const(Box::new(inner)))
            }
            TypeSpecifier::SelfType(self_ty) => Some(SemaType::SelfType(TypedSelfType { loc: self_ty.loc })),
            TypeSpecifier::GenericInst(inst) => self.resolve_generic_inst_type(inst, loc),
            TypeSpecifier::Tuple(tuple) => self.resolve_tuple_type(tuple),
            TypeSpecifier::FuncType(func) => self.resolve_func_type(*func, loc),
            TypeSpecifier::Array(array) => self.resolve_array_type(array, loc),
            TypeSpecifier::Deref(inner) => {
                let inner = self.resolve_type(*inner, loc)?;
                Some(SemaType::Pointer(Box::new(inner)))
            }
            TypeSpecifier::UnnamedUnion(union_ty) => self.resolve_unnamed_union_type(union_ty),
            TypeSpecifier::UnnamedEnum(enum_type) => self.resolve_unnamed_enum_type(enum_type),
            TypeSpecifier::UnnamedStruct(struct_type) => self.resolve_unnamed_struct_type(struct_type),
        }
    }

    fn resolve_builtin_type(&mut self, token: Token, loc: Loc) -> Option<SemaType> {
        match SemaType::try_from(token.kind.clone()) {
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

    fn resolve_ident_type(&mut self, ident: Ident) -> Option<SemaType> {
        if let Some(symbol_id) = self.resolve_local_scope_symbol(&ident.value) {
            return Some(SemaType::Unresolved(UnresolvedType::Decl(symbol_id)));
        }

        if let Some(generic_param_id) = self.resolve_generic_param_as_type(&ident) {
            return Some(SemaType::GenericParam(generic_param_id));
        }

        if let Some(symbol_id) = self.lookup_symbol_id(self.current_scope.unwrap(), &ident.value) {
            return Some(SemaType::Unresolved(UnresolvedType::Decl(symbol_id)));
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

    fn resolve_generic_param_as_type(&mut self, ident: &Ident) -> Option<GenericParamID> {
        let name = ident.as_string();

        for generic_scope in self.generic_scopes_iter() {
            if let Some(generic_param_id) = generic_scope.lookup(&name) {
                return Some(generic_param_id);
            }
        }

        None
    }

    fn resolve_module_import_type(&mut self, module_import: ASTModuleImport) -> Option<SemaType> {
        self.resolve_module_import(module_import.clone())
            .map(|symbol_id| SemaType::Unresolved(UnresolvedType::Decl(symbol_id)))
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

    fn resolve_func_type(&mut self, func: FuncType, loc: Loc) -> Option<SemaType> {
        let mut params = Vec::with_capacity(func.params.list.len());

        for param in func.params.list {
            params.push(self.resolve_type(param, loc)?);
        }

        let variadic = match func.params.variadic {
            Some(FuncTypeVariadicParams::UntypedCStyle) => Some(Box::new(TypedFuncTypeVariadicParam::UntypedCStyle)),

            Some(FuncTypeVariadicParams::Typed(spec)) => {
                let ty = self.resolve_type(spec, loc)?;
                Some(Box::new(TypedFuncTypeVariadicParam::Typed(ty)))
            }

            None => None,
        };

        let ret_type = self.resolve_type(*func.ret_type, loc)?;

        Some(SemaType::FuncType(TypedFuncType {
            params: TypedFuncTypeParams { list: params, variadic },
            ret_type: Box::new(ret_type),
            is_public: true,
            loc,
        }))
    }

    fn resolve_generic_inst_type(&mut self, inst: GenericInst, loc: Loc) -> Option<SemaType> {
        let base_type = self.resolve_type(*inst.base.clone(), loc)?;

        if base_type.is_self_type() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::SelfTypeDoesNotAcceptTypeArgs),
                loc: Some(inst.loc),
                hint: None,
            });
            return None;
        }

        let base_decl_id = match base_type.const_inner().as_unresolved_decl_id() {
            Some(decl_id) => decl_id,
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

        let type_args = self.resolve_type_args(&inst.type_args)?;

        Some(SemaType::Unresolved(UnresolvedType::GenericInst {
            base_symbol_id: base_decl_id,
            type_args,
        }))
    }

    fn resolve_tuple_type(&mut self, tuple: TupleType) -> Option<SemaType> {
        let mut elements = Vec::new();

        for type_spec in tuple.type_list {
            elements.push(self.resolve_type(type_spec, tuple.loc)?);
        }

        Some(SemaType::Tuple(TypedTupleType {
            elements,
            loc: tuple.loc,
        }))
    }

    fn resolve_array_type(&mut self, array: ArrayType, loc: Loc) -> Option<SemaType> {
        let element_type = self.resolve_type(*array.element_type, loc)?;

        let capacity = match array.size {
            ArrayCapacity::Fixed(expr) => {
                let expr = self.resolve_expr(&expr)?;
                TypedArrayCapacity::Fixed(Box::new(expr))
            }
            ArrayCapacity::Dynamic => TypedArrayCapacity::Dynamic,
        };

        Some(SemaType::Array(TypedArrayType {
            element_type: Box::new(element_type),
            capacity,
            loc,
        }))
    }

    fn resolve_unnamed_union_type(&mut self, union_type: UnnamedUnionType) -> Option<SemaType> {
        let mut fields = Vec::with_capacity(union_type.fields.len());

        for field in &union_type.fields {
            let ty = self.resolve_type(field.field_ty.clone(), field.loc)?;

            fields.push(TypedUnionField {
                name: field.ident.as_string(),
                ty,
                loc: field.loc,
            });
        }

        let mut union_modifiers = UnionModifiers::default();
        union_modifiers.repr_attr = union_type.repr_attr;

        let union_decl_id = self.decl_tables.insert_union(UnionDecl {
            name: None,
            fields,
            methods: MethodDecls::new(),
            impls: Vec::new(),
            generic_params: TypedGenericParams::new(),
            modifiers: union_modifiers,
            align: union_type.align,
            loc: union_type.loc,

            is_normalized: false,
        });

        Some(SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Union(union_decl_id),
            type_args: TypedTypeArgs::new(),
        }))
    }

    fn resolve_unnamed_enum_type(&mut self, enum_type: UnnamedEnumType) -> Option<SemaType> {
        let variants = self.resolve_enum_variants(&enum_type.variants)?;

        let tag_type = enum_type
            .tag_type
            .and_then(|type_spec| self.resolve_type(*type_spec, enum_type.loc));

        let mut enum_modifiers = EnumModifiers::default();
        enum_modifiers.repr_attr = enum_type.repr_attr;

        let enum_decl_id = self.decl_tables.insert_enum(EnumDecl {
            name: None,
            variants,
            tag_type,
            methods: MethodDecls::new(),
            impls: Vec::new(),
            generic_params: TypedGenericParams::new(),
            modifiers: enum_modifiers,
            align: enum_type.align,
            loc: enum_type.loc,

            is_normalized: false,
        });

        Some(SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Enum(enum_decl_id),
            type_args: TypedTypeArgs::new(),
        }))
    }

    fn resolve_unnamed_struct_type(&mut self, struct_type: UnnamedStructType) -> Option<SemaType> {
        let mut fields = Vec::with_capacity(struct_type.fields.len());

        // unnamed struct field visibility is always public
        let field_vis = Visibility::Public;

        for field in &struct_type.fields {
            let ty = self.resolve_type(field.ty.clone(), field.loc)?;

            fields.push(TypedStructField {
                name: field.name.value.clone(),
                ty,
                vis: field_vis,
                loc: field.loc,
            });
        }

        let mut struct_modifiers = StructModifiers::default();
        struct_modifiers.repr_attr = struct_type.repr_attr;

        let struct_decl_id = self.decl_tables.insert_struct(StructDecl {
            name: None,
            fields,
            impls: Vec::new(),
            methods: MethodDecls::new(),
            generic_params: TypedGenericParams::new(),
            modifiers: struct_modifiers,
            align: struct_type.align,
            loc: struct_type.loc,

            is_normalized: false,
        });

        Some(SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Struct(struct_decl_id),
            type_args: TypedTypeArgs::new(),
        }))
    }

    fn resolve_type_args(&mut self, type_args: &TypeArgs) -> Option<TypedTypeArgs> {
        type_args
            .iter()
            .map(|type_arg| match type_arg {
                TypeArg::Type(type_spec) => {
                    let sema_type = self.resolve_type(type_spec.clone(), type_spec.loc())?;
                    Some(TypedTypeArg::Type(sema_type, type_spec.loc()))
                }
                TypeArg::Infer => Some(TypedTypeArg::Infer),
            })
            .collect::<Option<_>>()
    }

    fn resolve_generic_params(&mut self, generic_params: &GenericParams) -> Option<TypedGenericParams> {
        let mut list = Vec::with_capacity(generic_params.len());
        let mut seen = FxHashSet::new();

        for generic_param in generic_params {
            let name = &generic_param.param_name.value;

            if !seen.insert(name.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::DuplicateGenericParam { name: name.clone() }),
                    loc: Some(generic_param.param_name.loc),
                    hint: None,
                });
            }

            let bounds = self.resolve_generic_param_bounds(generic_param)?;
            let default = self.resolve_generic_param_default(generic_param)?;

            let name = Ident {
                value: name.clone(),
                loc: generic_param.param_name.loc,
            };

            let generic_param = TypedGenericParam { name, bounds, default };

            let generic_param_id = self.decl_tables.insert_generic_param(generic_param.clone());

            list.push(generic_param_id);
        }

        Some(TypedGenericParams(list))
    }

    fn resolve_generic_param_bounds(&mut self, generic_param: &GenericParam) -> Option<Vec<TypedBound>> {
        let bounds_list = match &generic_param.bounds {
            Some(b) => b,
            None => return Some(Vec::new()),
        };

        let mut typed_bounds = Vec::with_capacity(bounds_list.len());

        for bound in bounds_list {
            let ty = match self.resolve_type(bound.type_spec.clone(), bound.loc) {
                Some(ty) => ty,
                None => continue,
            };

            typed_bounds.push(TypedBound { ty, loc: bound.loc });
        }

        Some(typed_bounds)
    }

    fn resolve_generic_param_default(&mut self, generic_param: &GenericParam) -> Option<Option<Box<SemaType>>> {
        match &generic_param.default {
            Some(default_ty) => {
                let ty = self.resolve_type(default_ty.clone(), generic_param.param_name.loc)?;

                Some(Some(Box::new(ty)))
            }
            None => Some(None),
        }
    }

    fn resolve_typedef(&mut self, typedef: &ASTTypedefStmt) -> Option<TypedStmt> {
        let symbol_id = self
            .lookup_symbol_id(self.current_scope.unwrap(), &typedef.ident.value)
            .unwrap();

        let generic_params = self.resolve_generic_params(&typedef.generic_params)?;

        self.with_generic_scope(&generic_params.clone(), |this| {
            let sema_type = this.resolve_type(typedef.type_spec.clone(), typedef.loc)?;

            let typedef_decl_id = this.decl_tables.insert_typedef(TypedefDecl {
                name: typedef.ident.as_string(),
                generic_params: generic_params.clone(),
                ty: Box::new(sema_type.clone()),
                vis: typedef.vis.clone(),
                loc: typedef.loc,
            });

            this.with_global_symbol_mut(symbol_id, |symbol_entry| {
                symbol_entry.kind = SymbolEntryKind::Typedef(typedef_decl_id)
            });

            Some(TypedStmt::Typedef(TypedTypedefStmt {
                typedef_decl_id,
                name: typedef.ident.as_string(),
                ty: sema_type,
                generic_params,
                vis: typedef.vis.clone(),
                loc: typedef.loc,
            }))
        })
    }

    #[allow(unused)]
    fn resolve_builtin(&mut self, builtin: &Builtin) -> Option<TypedBuiltin> {
        match builtin {
            Builtin::BuiltinFunc(builtin_func) => self.resolve_builtin_func(builtin_func),
            Builtin::BuiltinBlock(builtin_block) => self.resolve_builtin_block(builtin_block),
        }
    }

    fn resolve_builtin_func(&mut self, builtin_func: &BuiltinFunc) -> Option<TypedBuiltin> {
        let args = builtin_func
            .args
            .iter()
            .filter_map(|arg| self.resolve_expr(arg))
            .collect();

        let child_stmt = builtin_func
            .child_stmt
            .clone()
            .and_then(|stmt| self.resolve_stmt(&stmt))
            .map(Box::new);

        let builtin_func = TypedBuiltinFunc {
            name: builtin_func.name.clone(),
            args,
            child_stmt,
            ret_type: None,
            loc: builtin_func.loc,
        };

        Some(TypedBuiltin::BuiltinFunc(builtin_func))
    }

    fn resolve_builtin_block(&mut self, builtin_block: &BuiltinBlock) -> Option<TypedBuiltin> {
        if !self.is_builtin_active(&builtin_block.name.value) {
            return None;
        }

        let args = builtin_block
            .args
            .iter()
            .filter_map(|arg| self.resolve_expr(arg))
            .collect();

        let stmts = {
            if builtin_block.is_toplevel_block {
                let typed_stmts: Vec<TypedStmt> = builtin_block
                    .block
                    .stmts
                    .iter()
                    .flat_map(|stmt| self.resolve_toplevel_stmt(stmt))
                    .collect();

                typed_stmts
            } else {
                let typed_stmts: Vec<TypedStmt> = builtin_block
                    .block
                    .stmts
                    .iter()
                    .flat_map(|stmt| self.resolve_stmt(stmt))
                    .collect();

                typed_stmts
            }
        };

        let block = TypedBlockStmt {
            stmts,
            defers: Vec::new(),
            loc: builtin_block.loc,
        };

        let built_block = TypedBuiltinBlock {
            name: builtin_block.name.clone(),
            args,
            block: Box::new(block),
            is_toplevel: None,
            loc: builtin_block.loc,
        };

        Some(TypedBuiltin::BuiltinBlock(built_block))
    }

    fn resolve_interface_stmt(&mut self, interface: &ASTInterfaceStmt) -> Option<TypedStmt> {
        let name = interface.ident.as_string();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();
        let loc = interface.loc;

        let generic_params = self.resolve_generic_params(&interface.generic_params)?;

        self.with_generic_scope(&generic_params.clone(), |this| {
            let mut interface_methods = MethodDecls::new();

            for func_decl_stmt in &interface.methods {
                if func_decl_stmt.renamed_as.is_some() {
                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::RenameInterfaceMethod),
                        loc: Some(func_decl_stmt.loc),
                        hint: None,
                    });
                    continue;
                }

                let method_generic_params = this.resolve_generic_params(&func_decl_stmt.generic_params)?;

                let (params, variadic, ret_type) = this.with_generic_scope(&method_generic_params, |this| {
                    let (params, variadic) = this.resolve_func_params(&func_decl_stmt.params)?;

                    let ret_type = this.resolve_type(
                        return_type_or_default_void(func_decl_stmt.ret_type.clone(), func_decl_stmt.loc),
                        func_decl_stmt.loc,
                    )?;

                    Some((params, variadic, ret_type))
                })?;

                let method_decl = MethodDecl {
                    func_decl: FuncDecl {
                        is_func_decl: true,
                        body: None,

                        file_id: this.current_module_file_id.unwrap(),
                        name: func_decl_stmt.ident.as_string(),
                        generic_params: method_generic_params,
                        params: TypedFuncParams { list: params, variadic },
                        ret_type,
                        modifiers: FuncModifiers {
                            vis: Visibility::Public,
                            ..Default::default()
                        },
                        loc: func_decl_stmt.loc,
                    },
                    body: None,
                    object_name: Some(name.clone()),
                };

                let method_decl_id = this.decl_tables.insert_method(method_decl);

                interface_methods
                    .0
                    .insert(func_decl_stmt.ident.as_string(), method_decl_id);
            }

            let interface_decl_id = this.decl_tables.insert_interface(InterfaceDecl {
                name: name.clone(),
                methods: interface_methods.clone(),
                generic_params: generic_params.clone(),
                vis: interface.vis.clone(),
                loc,
            });

            this.with_global_symbol_mut(symbol_id, |symbol_entry| {
                symbol_entry.kind = SymbolEntryKind::Interface(interface_decl_id)
            });

            Some(TypedStmt::Interface(TypedInterfaceStmt {
                name,
                interface_decl_id,
                methods: interface_methods,
                generic_params,
                vis: interface.vis.clone(),
                loc,
            }))
        })
    }

    fn resolve_union_stmt(&mut self, union_decl: &ASTUnionStmt) -> Option<TypedStmt> {
        let name = union_decl.ident.as_string();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();
        let loc = union_decl.loc;

        let generic_params = self.resolve_generic_params(&union_decl.generic_params)?;

        self.with_generic_scope(&generic_params.clone(), |this| {
            let mut typed_union_fields = Vec::with_capacity(union_decl.fields.len());

            for field in &union_decl.fields {
                let sema_type = match this.resolve_type(field.ty.clone(), field.loc) {
                    Some(ty) => ty,
                    None => continue,
                };

                typed_union_fields.push(TypedUnionField {
                    name: field.name.as_string(),
                    ty: sema_type,
                    loc: field.loc,
                });
            }

            this.report_if_duplicate_method_names(&name, &union_decl.methods);

            let impls = this.resolve_object_implements_interfaces(&union_decl.impls, union_decl.loc)?;

            let union_decl_id = this.decl_tables.insert_union(UnionDecl {
                name: Some(name.clone()),
                fields: typed_union_fields.clone(),
                methods: MethodDecls::new(), // placeholder
                impls: impls.clone(),
                generic_params: generic_params.clone(),
                modifiers: union_decl.modifiers.clone(),
                align: union_decl.align.clone(),
                loc,

                is_normalized: false,
            });

            this.with_global_symbol_mut(symbol_id, |symbol_entry| {
                symbol_entry.kind = SymbolEntryKind::Union(union_decl_id)
            });

            let methods = this.resolve_methods(&union_decl.methods, symbol_id, &name);

            this.decl_tables.with_union_decl_mut(union_decl_id, |_union_decl| {
                _union_decl.methods = methods.clone();
            });

            Some(TypedStmt::Union(TypedUnionStmt {
                union_decl_id,
                name,
                fields: typed_union_fields,
                methods,
                generic_params,
                modifiers: union_decl.modifiers.clone(),
                impls,
                align: union_decl.align.clone(),
                loc: union_decl.ident.loc,
            }))
        })
    }

    fn resolve_enum_struct_variant_init(
        &mut self,
        struct_variant_init: &ASTEnumStructVariantInit,
    ) -> Option<TypedExpr> {
        let operand = self.resolve_expr(&struct_variant_init.operand)?;

        let mut typed_field_inits = Vec::new();

        for field_init in &struct_variant_init.field_inits {
            match self.resolve_expr(&field_init.value) {
                Some(typed_expr) => {
                    typed_field_inits.push(TypedEnumStructVariantFieldInit {
                        name: field_init.name.clone(),
                        value: Box::new(typed_expr),
                        loc: field_init.loc,
                    });
                }
                None => continue,
            }
        }

        Some(TypedExpr {
            kind: TypedExprKind::EnumStructVariantInit(TypedEnumStructVariantInit {
                enum_decl_id: None,
                operand: Box::new(operand),
                ident: struct_variant_init.ident.clone(),
                field_inits: typed_field_inits,
                loc: struct_variant_init.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: struct_variant_init.loc,
        })
    }

    fn resolve_enum_variants(&mut self, variants: &[EnumVariant]) -> Option<Vec<TypedEnumVariant>> {
        let mut typed_variants: Vec<TypedEnumVariant> = Vec::with_capacity(variants.len());

        for variant in variants {
            let typed_variant = match variant {
                EnumVariant::Unit(ident) => TypedEnumVariant::Unit(ident.clone()),
                EnumVariant::Valued { ident, value } => {
                    let typed_expr = match self.resolve_expr(&value) {
                        Some(expr) => expr,
                        None => continue,
                    };

                    TypedEnumVariant::Valued {
                        ident: ident.clone(),
                        value: Box::new(typed_expr),
                    }
                }
                EnumVariant::Tuple { ident, fields } => {
                    let mut typed_fields = Vec::new();

                    for tuple_field in fields {
                        match self.resolve_type(tuple_field.ty.clone(), ident.loc) {
                            Some(sema_type) => typed_fields.push(TypedEnumVariantTupleField {
                                ty: sema_type,
                                loc: tuple_field.loc,
                            }),
                            None => continue,
                        }
                    }

                    TypedEnumVariant::Tuple {
                        ident: ident.clone(),
                        fields: typed_fields,
                    }
                }
                EnumVariant::Struct { ident, fields } => {
                    let mut typed_fields = Vec::new();

                    for struct_field in fields {
                        match self.resolve_type(struct_field.ty.clone(), ident.loc) {
                            Some(sema_type) => typed_fields.push(TypedEnumVariantStructField {
                                name: struct_field.name.clone(),
                                ty: sema_type,
                                loc: struct_field.loc,
                            }),
                            None => continue,
                        }
                    }

                    TypedEnumVariant::Struct {
                        ident: ident.clone(),
                        fields: typed_fields,
                    }
                }
            };

            typed_variants.push(typed_variant);
        }

        Some(typed_variants)
    }

    fn resolve_enum_stmt(&mut self, enum_decl: &ASTEnumStmt) -> Option<TypedStmt> {
        let name = enum_decl.ident.as_string();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();
        let loc = enum_decl.loc;

        let generic_params = self.resolve_generic_params(&enum_decl.generic_params)?;

        self.with_generic_scope(&generic_params.clone(), |this| {
            let variants = this.resolve_enum_variants(&enum_decl.variants)?;

            this.report_if_duplicate_method_names(&name, &enum_decl.methods);

            let tag_type = match &enum_decl.tag_type {
                Some(ty) => this.resolve_type(ty.clone(), enum_decl.loc),
                None => None,
            };

            let impls = this.resolve_object_implements_interfaces(&enum_decl.impls, enum_decl.loc)?;

            let enum_decl_id = this.decl_tables.insert_enum(EnumDecl {
                name: Some(name.clone()),
                variants: variants.clone(),
                methods: MethodDecls::new(), // placeholder
                impls: impls.clone(),
                generic_params: generic_params.clone(),
                tag_type: tag_type.clone(),
                modifiers: enum_decl.modifiers.clone(),
                align: enum_decl.align.clone(),
                loc,

                is_normalized: false,
            });

            this.with_global_symbol_mut(symbol_id, |symbol_entry| {
                symbol_entry.kind = SymbolEntryKind::Enum(enum_decl_id)
            });

            let methods = this.resolve_methods(&enum_decl.methods, symbol_id, &name);

            this.decl_tables.with_enum_decl_mut(enum_decl_id, |_enum_decl| {
                _enum_decl.methods = methods.clone();
            });

            Some(TypedStmt::Enum(TypedEnumStmt {
                enum_decl_id,
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
        })
    }

    fn resolve_global_var_stmt(&mut self, global_var: &ASTGlobalVarStmt) -> Option<TypedStmt> {
        let name = global_var.ident.as_string();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();
        let loc = global_var.loc;

        let sema_type = match &global_var.type_spec {
            Some(ty) => self.resolve_type(ty.clone(), loc),
            None => None,
        };

        let typed_expr = match &global_var.expr {
            Some(expr) => self.resolve_expr(expr),
            None => None,
        };

        let global_var_decl_id = self.decl_tables.insert_global_var(GlobalVarDecl {
            file_id: self.current_module_file_id.unwrap(),
            name: name.clone(),
            ty: sema_type.clone(),
            rhs: typed_expr.clone(),
            analyzed: true,
            is_const: global_var.is_const,
            modifiers: global_var.modifiers.clone(),
            loc,
        });

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::GlobalVar(global_var_decl_id)
        });

        Some(TypedStmt::GlobalVar(TypedGlobalVarStmt {
            file_id: self.current_module_file_id.unwrap(),
            global_var_decl_id,
            name,
            ty: sema_type,
            expr: typed_expr,
            modifiers: global_var.modifiers.clone(),
            is_const: global_var.is_const,
            loc,
        }))
    }

    fn resolve_method_decls(&mut self, object_name: &str, methods: &[ASTFuncDefStmt]) -> MethodDecls {
        let mut method_decls = MethodDecls::new();

        for ast_method in methods {
            let method_name = &ast_method.ident.value;

            let method_generic_params = match self.resolve_generic_params(&ast_method.generic_params) {
                Some(g) => g,
                None => continue,
            };

            let (params, variadic, ret_type) = match self.with_generic_scope(&method_generic_params, |this| {
                let (params, variadic) = this.resolve_func_params(&ast_method.params)?;

                let ret_type = this.resolve_type(
                    return_type_or_default_void(ast_method.ret_type.clone(), ast_method.loc),
                    ast_method.loc,
                )?;

                Some((params, variadic, ret_type))
            }) {
                Some(v) => v,
                None => continue,
            };

            let method_decl_id = self.decl_tables.insert_method(MethodDecl {
                func_decl: FuncDecl {
                    is_func_decl: false,
                    body: None,

                    file_id: self.current_module_file_id.unwrap(),
                    name: method_name.clone(),
                    params: TypedFuncParams { list: params, variadic },
                    ret_type,
                    generic_params: method_generic_params,
                    modifiers: ast_method.modifiers.clone(),
                    loc: ast_method.loc,
                },
                body: None,
                object_name: Some(object_name.to_string()),
            });

            method_decls.insert(method_name.to_string(), method_decl_id);
        }

        method_decls
    }

    fn resolve_method_bodies(&mut self, method_decls: &MethodDecls, ast_methods: &[ASTFuncDefStmt]) {
        for ast_method in ast_methods {
            let Some(method_decl_id) = method_decls.get(&ast_method.ident.value) else {
                continue;
            };

            let scope = LocalScope::new();

            let typed_body = with_local_scope!(self, scope.clone(), {
                let mut method_decl = self.decl_tables.method_decl(method_decl_id);

                self.insert_func_params_into_current_scope(
                    &mut method_decl.func_decl.params.list,
                    &mut method_decl.func_decl.params.variadic,
                );

                self.decl_tables.with_method_decl_mut(method_decl_id, |_method_decl| {
                    *_method_decl = method_decl;
                });

                self.resolve_block_stmt(&ast_method.body)
            });

            if let Some(typed_body) = typed_body {
                let body_id = self.decl_tables.insert_body(typed_body);

                self.decl_tables.with_method_decl_mut(method_decl_id, |_method_decl| {
                    _method_decl.body = Some(body_id);
                });
            }
        }
    }

    fn resolve_methods(
        &mut self,
        ast_methods: &[ASTFuncDefStmt],
        object_symbol_id: SymbolID,
        object_name: &String,
    ) -> MethodDecls {
        self.current_object_symbol_id = Some(object_symbol_id);

        self.report_if_duplicate_method_names(object_name, ast_methods);

        let method_decls = self.resolve_method_decls(object_name, ast_methods);

        self.resolve_method_bodies(&method_decls, ast_methods);

        method_decls
    }

    fn resolve_object_implements_interfaces(
        &mut self,
        impls: &Vec<ImplementInterface>,
        loc: Loc,
    ) -> Option<Vec<TypedImplementInterface>> {
        let mut typed_impls: Vec<TypedImplementInterface> = Vec::new();

        for implement_interface in impls {
            let Some(ty) = self.resolve_type(implement_interface.ty.clone(), implement_interface.loc) else {
                continue;
            };

            typed_impls.push(TypedImplementInterface { ty, loc })
        }

        Some(typed_impls)
    }

    fn resolve_struct_stmt(&mut self, struct_decl: &ASTStructStmt) -> Option<TypedStmt> {
        let name = struct_decl.ident.as_string();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();
        let loc = struct_decl.loc;

        let generic_params = self.resolve_generic_params(&struct_decl.generic_params)?;

        self.with_generic_scope(&generic_params.clone(), |this| {
            let typed_struct_fields: Vec<TypedStructField> = struct_decl
                .fields
                .iter()
                .filter_map(|field| {
                    this.resolve_type(field.ty.clone(), field.loc)
                        .map(|ty| TypedStructField {
                            name: field.name.as_string(),
                            vis: field.vis.clone(),
                            ty,
                            loc: field.loc,
                        })
                })
                .collect();

            this.report_if_duplicate_method_names(&name, &struct_decl.methods);

            let impls = this.resolve_object_implements_interfaces(&struct_decl.impls, struct_decl.loc)?;

            let struct_decl_id = this.decl_tables.insert_struct(StructDecl {
                name: Some(name.clone()),
                fields: typed_struct_fields.clone(),
                generic_params: generic_params.clone(),
                impls: impls.clone(),
                methods: MethodDecls::new(), // placeholder
                modifiers: struct_decl.modifiers.clone(),
                align: struct_decl.align.clone(),
                loc,

                is_normalized: false,
            });

            this.with_global_symbol_mut(symbol_id, |symbol_entry| {
                symbol_entry.kind = SymbolEntryKind::Struct(struct_decl_id)
            });

            let methods = this.resolve_methods(&struct_decl.methods, symbol_id, &name);

            this.decl_tables.with_struct_decl_mut(struct_decl_id, |_struct_decl| {
                _struct_decl.methods = methods.clone();
            });

            Some(TypedStmt::Struct(TypedStructStmt {
                struct_decl_id,
                name,
                fields: typed_struct_fields,
                methods,
                generic_params,
                impls,
                modifiers: struct_decl.modifiers.clone(),
                align: struct_decl.align.clone(),
                loc,
            }))
        })
    }

    fn resolve_func_params(
        &mut self,
        params: &FuncParams,
    ) -> Option<(Vec<TypedFuncParamKind>, Option<TypedFuncVariadicParam>)> {
        let mut typed_params = Vec::with_capacity(params.list.len());

        for param in &params.list {
            match param {
                FuncParamKind::FuncParam(func_param) => {
                    let typed_param = self.resolve_func_param(func_param)?;

                    typed_params.push(TypedFuncParamKind::FuncParam(typed_param));
                }
                FuncParamKind::SelfModifier(self_modifier) => {
                    let typed_self_modifier = self.resolve_self_modifier_param(self_modifier);

                    typed_params.push(TypedFuncParamKind::SelfModifier(typed_self_modifier));
                }
            }
        }

        let variadic = match &params.variadic {
            Some(variadic) => self.resolve_func_variadic_param(variadic)?,
            None => None,
        };

        Some((typed_params, variadic))
    }

    fn resolve_func_param(&mut self, param: &FuncParam) -> Option<TypedFuncParam> {
        let ty = match &param.ty {
            Some(type_spec) => self.resolve_type(type_spec.clone(), param.loc)?,
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

        Some(TypedFuncParam {
            var_decl_id: None,
            ident: param.ident.clone(),
            ty,
            mutability: param.mutability,
            loc: param.loc,
        })
    }

    fn insert_func_params_into_current_scope(
        &mut self,
        params_kinds: &mut Vec<TypedFuncParamKind>,
        _variadic: &mut Option<TypedFuncVariadicParam>,
    ) -> Option<()> {
        for param_kind in params_kinds {
            let is_const_param = param_kind.is_const();

            match param_kind {
                TypedFuncParamKind::FuncParam(func_param) => {
                    let var_decl_id: VarDeclID =
                        self.insert_variable_decl(&func_param.ident, Some(func_param.ty.clone()), None, is_const_param);

                    self.insert_variable_symbol_to_current_scope(&func_param.ident, var_decl_id)?;

                    func_param.var_decl_id = Some(var_decl_id);
                }
                TypedFuncParamKind::SelfModifier(self_modifier) => {
                    let self_ident = Ident {
                        value: "self".to_string(),
                        loc: self_modifier.loc,
                    };

                    let var_decl_id: VarDeclID =
                        self.insert_variable_decl(&self_ident, Some(self_modifier.ty.clone()), None, is_const_param);

                    self.insert_variable_symbol_to_current_scope(&self_ident, var_decl_id)?;

                    self_modifier.var_decl_id = Some(var_decl_id);
                }
            };
        }

        Some(())
    }

    fn resolve_self_modifier_param(&mut self, self_modifier: &SelfModifier) -> TypedSelfModifier {
        let self_type = SemaType::SelfType(TypedSelfType { loc: self_modifier.loc });

        let ty = match &self_modifier.kind {
            SelfModifierKind::Copied => self_type,
            SelfModifierKind::Referenced => SemaType::Pointer(Box::new(self_type)),
        };

        TypedSelfModifier {
            var_decl_id: None,
            ty,
            kind: self_modifier.kind.clone(),
            mutability: self_modifier.mutability,
            loc: self_modifier.loc,
        }
    }

    fn resolve_func_variadic_param(&mut self, variadic: &FuncVariadicParam) -> Option<Option<TypedFuncVariadicParam>> {
        match variadic {
            FuncVariadicParam::UntypedCStyle => Some(Some(TypedFuncVariadicParam::UntypedCStyle)),
            FuncVariadicParam::Typed(ident, type_spec) => {
                let ty = self.resolve_type(type_spec.clone(), ident.loc)?;

                let var_decl_id = self.insert_variable_decl(ident, Some(ty.clone()), None, false);
                self.insert_variable_symbol_to_current_scope(ident, var_decl_id)?;

                Some(Some(TypedFuncVariadicParam::Typed {
                    var_decl_id,
                    ty,
                    loc: ident.loc,
                }))
            }
        }
    }

    fn resolve_func_decl(&mut self, ast_func_decl: &ASTFuncDeclStmt) -> Option<TypedStmt> {
        let name = ast_func_decl.usable_name();
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &name).unwrap();

        let generic_params = self.resolve_generic_params(&ast_func_decl.generic_params)?;

        let (func_params, variadic_param, ret_type) = self.with_generic_scope(&generic_params, |this| {
            let (params, variadic) = this.resolve_func_params(&ast_func_decl.params)?;

            let ret_type = this.resolve_type(
                return_type_or_default_void(ast_func_decl.ret_type.clone(), ast_func_decl.loc),
                ast_func_decl.loc,
            )?;

            Some((params, variadic, ret_type))
        })?;

        let func_decl = FuncDecl {
            is_func_decl: true,
            body: None,

            file_id: self.current_module_file_id.unwrap(),
            name,
            generic_params: generic_params.clone(),
            params: TypedFuncParams {
                list: func_params.clone(),
                variadic: variadic_param.clone(),
            },
            ret_type: ret_type.clone(),
            modifiers: ast_func_decl.modifiers.clone(),
            loc: ast_func_decl.loc,
        };

        let func_decl_id = self.decl_tables.insert_func(func_decl);

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::Func(func_decl_id)
        });

        Some(TypedStmt::FuncDecl(TypedFuncDeclStmt {
            file_id: self.current_module_file_id.unwrap(),
            func_decl_id,
            name: ast_func_decl.ident.as_string(),
            generic_params,
            params: TypedFuncParams {
                list: func_params,
                variadic: variadic_param,
            },
            ret_type,
            modifiers: ast_func_decl.modifiers.clone(),
            renamed_as: ast_func_decl.renamed_as.as_ref().map(|id| id.as_string()),
            loc: ast_func_decl.loc,
        }))
    }

    fn resolve_func_def(&mut self, func_def: &ASTFuncDefStmt) -> Option<TypedStmt> {
        let symbol_id = self.lookup_symbol_id(self.current_scope.unwrap(), &func_def.ident.value)?;

        let scope = LocalScope::new();

        let generic_params = self.resolve_generic_params(&func_def.generic_params)?;

        let (mut params, mut variadic, ret_type) = self.with_generic_scope(&generic_params, |this| {
            let (params, variadic) = this.resolve_func_params(&func_def.params)?;

            let ret_type = this.resolve_type(
                return_type_or_default_void(func_def.ret_type.clone(), func_def.loc),
                func_def.loc,
            )?;

            Some((params, variadic, ret_type))
        })?;

        let func_decl = FuncDecl {
            is_func_decl: false,
            body: None,

            file_id: self.current_module_file_id.unwrap(),
            name: func_def.ident.as_string(),
            generic_params: generic_params.clone(),
            params: TypedFuncParams {
                list: params.clone(),
                variadic: variadic.clone(),
            },
            ret_type: ret_type.clone(),
            modifiers: func_def.modifiers.clone(),
            loc: func_def.loc,
        };

        let is_generic = func_decl.is_generic();
        let func_decl_id = self.decl_tables.insert_func(func_decl);

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::Func(func_decl_id)
        });

        let typed_body = self.with_generic_scope(&generic_params, |this| {
            with_local_scope!(this, scope, {
                this.insert_func_params_into_current_scope(&mut params, &mut variadic)?;
                this.resolve_block_stmt(&func_def.body)
            })
        })?;

        // we later need the var_dec_lid of the params
        // that's why having this update is very important!
        self.decl_tables.with_func_decl_mut(func_decl_id, |_func_decl| {
            _func_decl.params = TypedFuncParams {
                list: params.clone(),
                variadic: variadic.clone(),
            };
        });

        // only store body for generic functions (used for monomorphization)
        if is_generic {
            let body_id = self.decl_tables.insert_body(typed_body.clone());

            self.decl_tables.with_func_decl_mut(func_decl_id, |_func_decl| {
                _func_decl.body = Some(body_id);
            })
        }

        Some(TypedStmt::FuncDef(TypedFuncDefStmt {
            func_decl_id: Some(func_decl_id),
            name: func_def.ident.as_string(),
            generic_params,
            params: TypedFuncParams { list: params, variadic },
            ret_type,
            modifiers: func_def.modifiers.clone(),
            loc: func_def.loc,
            body: Box::new(typed_body),
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

        self.collect_labels_in_block(block_stmt);

        for stmt in &block_stmt.stmts {
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
        let typed_rhs = export_tuple.rhs.as_ref().and_then(|expr| self.resolve_expr(expr));

        let pattern = self.resolve_export_pattern(&export_tuple.pattern, export_tuple.is_const)?;

        Some(TypedStmt::TupleExport(TypedTupleExportStmt {
            pattern,
            rhs: typed_rhs,
            is_const: export_tuple.is_const,
            loc: export_tuple.loc,
        }))
    }

    fn resolve_export_pattern(
        &mut self,
        pattern: &ExportPattern,
        stmt_is_const: bool,
    ) -> Option<TypedTupleExportPattern> {
        let kind = match &pattern.kind {
            ExportPatternKind::Ident(ident) => {
                let is_const = pattern
                    .mutability
                    .map(|mutability| match mutability {
                        Mutability::Const => true,
                        Mutability::Var => false,
                    })
                    .unwrap_or(stmt_is_const /* follow base stmt */);

                let var_decl_id = self.insert_variable_decl(ident, None, None, is_const);

                self.insert_variable_symbol_to_current_scope(ident, var_decl_id)?;

                TypedTupleExportPatternKind::Ident(var_decl_id)
            }

            ExportPatternKind::Tuple(patterns) => {
                let mut typed_patterns = Vec::with_capacity(patterns.len());

                for pat in patterns {
                    typed_patterns.push(self.resolve_export_pattern(pat, stmt_is_const)?);
                }

                TypedTupleExportPatternKind::Tuple(typed_patterns)
            }

            ExportPatternKind::Ignore => TypedTupleExportPatternKind::Ignore,
        };

        let ty = pattern
            .ty
            .as_ref()
            .and_then(|t| self.resolve_type(t.clone(), pattern.loc));

        Some(TypedTupleExportPattern {
            kind,
            ty,
            mutability: pattern.mutability,
            loc: pattern.loc,
        })
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
            all_cases_covered: None,
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
        match &pattern.kind {
            // `_`
            SwitchCasePatternKind::Wildcard => Some(TypedSwitchCasePattern {
                kind: TypedSwitchCasePatternKind::Wildcard,
                loc: pattern.loc,
            }),

            // literal / const expression
            SwitchCasePatternKind::Expr(expr) => {
                let typed_expr = self.resolve_expr(expr)?;

                Some(TypedSwitchCasePattern {
                    kind: TypedSwitchCasePatternKind::Expr(typed_expr),
                    loc: pattern.loc,
                })
            }

            // range: `1...10` or `1..=10`
            SwitchCasePatternKind::Range(range) => {
                let lower = self.resolve_expr(&range.lower)?;
                let upper = self.resolve_expr(&range.upper)?;

                Some(TypedSwitchCasePattern {
                    kind: TypedSwitchCasePatternKind::Range(TypedRange {
                        lower,
                        upper,
                        inclusive_upper: range.inclusive_upper,
                        loc: range.loc,
                    }),
                    loc: pattern.loc,
                })
            }

            // enum unit variant: `.A`
            SwitchCasePatternKind::EnumUnit(variant) => Some(TypedSwitchCasePattern {
                kind: TypedSwitchCasePatternKind::EnumUnit(variant.clone()),
                loc: pattern.loc,
            }),

            // enum tuple variant: `.C(a, b, _)`
            //
            // IMPORTANT:
            //   - recursively resolve items
            //   - bindings inside get inserted into the case-local scope
            SwitchCasePatternKind::EnumTupleVariant { variant, items } => {
                let mut typed_items = Vec::with_capacity(items.len());

                for item in items {
                    let typed_item = self.resolve_switch_pattern(item)?;
                    typed_items.push(typed_item);
                }

                Some(TypedSwitchCasePattern {
                    kind: TypedSwitchCasePatternKind::EnumTupleVariant {
                        ident: variant.clone(),
                        items: typed_items,
                    },
                    loc: pattern.loc,
                })
            }

            // `.D { a, b: x, c: _, .. }`
            //
            // IMPORTANT:
            //   - resolve each field recursively
            //   - bindings inserted into scope
            SwitchCasePatternKind::EnumStructVariant {
                variant,
                items,
                has_rest,
            } => {
                let mut typed_items = Vec::with_capacity(items.len());

                for field in items {
                    let typed_pattern = self.resolve_switch_pattern(&field.pattern)?;

                    typed_items.push(TypedSwitchCaseEnumStructPatternField {
                        name: field.name.clone(),
                        pattern: typed_pattern,
                    });
                }

                Some(TypedSwitchCasePattern {
                    kind: TypedSwitchCasePatternKind::EnumStructVariant {
                        ident: variant.clone(),
                        items: typed_items,
                        has_rest: *has_rest,
                    },
                    loc: pattern.loc,
                })
            }

            // ident binding
            SwitchCasePatternKind::Binding(ident) => {
                // treat like 'var ident'
                let var_decl_id = self.insert_variable_decl(ident, None, None, false);

                self.insert_variable_symbol_to_current_scope(ident, var_decl_id)?;

                Some(TypedSwitchCasePattern {
                    kind: TypedSwitchCasePatternKind::Binding {
                        name: ident.clone(),
                        var_decl_id,
                    },
                    loc: pattern.loc,
                })
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

    fn resolve_var(&mut self, var: &ASTVarStmt) -> Option<TypedVarStmt> {
        let ty = var
            .ty
            .as_ref()
            .and_then(|type_spec| self.resolve_type(type_spec.clone(), var.loc));

        let rhs = var.rhs.as_ref().and_then(|expr| self.resolve_expr(expr));

        let var_decl_id = self.insert_variable_decl(&var.ident, ty.clone(), rhs.clone(), var.is_const);
        self.insert_variable_symbol_to_current_scope(&var.ident, var_decl_id)?;

        Some(TypedVarStmt {
            var_decl_id,
            name: var.ident.as_string(),
            ty,
            rhs,
            is_const: var.is_const,
            loc: var.loc,
        })
    }

    fn resolve_continue_stmt(&mut self, continue_stmt: &ASTContinueStmt) -> Option<TypedContinueStmt> {
        Some(TypedContinueStmt { loc: continue_stmt.loc })
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
        if let Some(label_id) = self.resolve_local_scope_label(&goto.name.value) {
            Some(TypedStmt::Goto(TypedGotoStmt {
                name: goto.name.as_string(),
                label_id: Some(label_id),
                loc: goto.loc,
            }))
        } else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(ResolverDiagKind::LabelNotDefined {
                    label_name: goto.name.as_string(),
                }),
                loc: Some(goto.name.loc),
                hint: None,
            });
            return None;
        }
    }

    fn resolve_label_stmt(&mut self, label: &ASTLabelStmt) -> Option<TypedStmt> {
        let name = label.name.as_string();

        let label_id = self.resolve_local_scope_label(&name).unwrap();

        Some(TypedStmt::Label(TypedLabelStmt {
            name,
            label_id,
            loc: label.loc,
        }))
    }

    #[inline]
    fn resolve_module_import_expr(&mut self, module_import: &ASTModuleImport) -> Option<TypedExpr> {
        if let Some(ident) = module_import.as_ident() {
            if let Some(generic_param_id) = self.resolve_generic_param_as_type(&ident) {
                return Some(TypedExpr {
                    kind: TypedExprKind::SemaType(SemaType::GenericParam(generic_param_id)),
                    ty: None,
                    val_cat: ValueCategory::Unknown,
                    loc: module_import.loc,
                });
            }
        }

        if let Some(symbol_id) = self.resolve_module_import(module_import.clone()) {
            return Some(TypedExpr {
                kind: TypedExprKind::Symbol(TypedSymbolExpr::Unresolved {
                    symbol_id,
                    loc: module_import.loc,
                }),
                ty: None,
                val_cat: ValueCategory::Unknown,
                loc: module_import.loc,
            });
        }

        None
    }

    fn resolve_unnamed_union_value(&mut self, unnamed_union_value: &ASTUnnamedUnionValueExpr) -> Option<TypedExpr> {
        let fields = unnamed_union_value
            .fields
            .iter()
            .filter_map(|field| {
                self.resolve_expr(&field.value)
                    .map(|value| TypedUnnamedUnionValueField {
                        name: field.name.as_string(),
                        value: Box::new(value),
                        loc: field.loc,
                    })
            })
            .collect();

        let kind = TypedExprKind::UnnamedUnionValue(TypedUnnamedUnionValue {
            union_decl_id: None,
            fields,
            loc: unnamed_union_value.loc,
        });

        Some(TypedExpr {
            kind,
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: unnamed_union_value.loc,
        })
    }

    fn resolve_unnamed_enum_value(&mut self, unnamed_enum_value: &ASTUnnamedEnumValueExpr) -> Option<TypedExpr> {
        let kind = match &unnamed_enum_value.kind {
            UnnamedEnumValueKind::Plain => TypedUnnamedEnumValueKind::Unit,
            UnnamedEnumValueKind::Tuple(exprs) => {
                let mut typed_exprs: Vec<TypedExpr> = Vec::new();
                for expr in exprs {
                    match self.resolve_expr(expr) {
                        Some(typed_expr) => typed_exprs.push(typed_expr),
                        None => continue,
                    }
                }
                TypedUnnamedEnumValueKind::Tuple(typed_exprs)
            }
            UnnamedEnumValueKind::Struct(field_inits) => {
                let mut typed_field_inits = Vec::new();

                for field_init in field_inits {
                    match self.resolve_expr(&field_init.value) {
                        Some(typed_expr) => typed_field_inits.push(TypedEnumStructVariantFieldInit {
                            name: field_init.name.clone(),
                            value: Box::new(typed_expr.clone()),
                            loc: field_init.loc,
                        }),
                        None => continue,
                    }
                }

                TypedUnnamedEnumValueKind::Struct(typed_field_inits)
            }
        };

        Some(TypedExpr {
            kind: TypedExprKind::UnnamedEnumValue(TypedUnnamedEnumValue {
                enum_decl_id: None,
                ident: unnamed_enum_value.ident.clone(),
                kind,
                loc: unnamed_enum_value.loc,
            }),
            val_cat: ValueCategory::Unknown,
            ty: None,
            loc: unnamed_enum_value.loc,
        })
    }

    fn resolve_dynamic_expr(&mut self, dynamic: &ASTDynamicExpr) -> Option<TypedExpr> {
        let operand = self.resolve_expr(&dynamic.operand)?;

        Some(TypedExpr {
            kind: TypedExprKind::Dynamic(TypedDynamicExpr {
                operand: Box::new(operand),
                interface_object_type: None,
                concrete_type: None,
                object_name: None,
                loc: dynamic.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: dynamic.loc,
        })
    }

    fn resolve_tuple_member_access(&mut self, tuple_member_access: &ASTTupleAccessExpr) -> Option<TypedExpr> {
        let operand = self.resolve_expr(&tuple_member_access.operand)?;

        Some(TypedExpr {
            kind: TypedExprKind::TupleAccess(TypedTupleAccessExpr {
                operand: Box::new(operand),
                index: tuple_member_access.index,
                loc: tuple_member_access.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: tuple_member_access.loc,
        })
    }

    fn resolve_tuple_expr(&mut self, tuple_value: &ASTTupleValueExpr) -> Option<TypedExpr> {
        let mut elements: Vec<TypedExpr> = Vec::new();

        for expr in &tuple_value.elements {
            match self.resolve_expr(expr) {
                Some(typed_expr) => elements.push(typed_expr),
                None => continue,
            }
        }

        Some(TypedExpr {
            kind: TypedExprKind::Tuple(TypedTupleExpr {
                elements,
                loc: tuple_value.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: tuple_value.loc,
        })
    }

    fn resolve_lambda_expr(&mut self, lambda: &ASTLambdaExpr) -> Option<TypedExpr> {
        let scope = LocalScope::new();

        with_local_scope!(self, scope, {
            let (mut params, mut variadic, ret_type) = {
                let (params, variadic) = self.resolve_func_params(&lambda.params)?;

                let ret_type = self.resolve_type(
                    return_type_or_default_void(Some(lambda.ret_type.clone()), lambda.loc),
                    lambda.loc,
                )?;

                (params, variadic, ret_type)
            };

            self.insert_func_params_into_current_scope(&mut params, &mut variadic)?;

            let body = match self.resolve_block_stmt(&lambda.body) {
                Some(block) => Box::new(block),
                None => return None,
            };

            Some(TypedExpr {
                kind: TypedExprKind::Lambda(TypedLambdaExpr {
                    params: TypedFuncParams { list: params, variadic },
                    body,
                    ret_type,
                    inline: lambda.inline,
                    loc: lambda.loc,
                }),
                ty: None,
                val_cat: ValueCategory::Unknown,
                loc: lambda.loc,
            })
        })
    }

    fn resolve_field_access(&mut self, field_access: &ASTFieldAccessExpr) -> Option<TypedExpr> {
        let operand = self.resolve_expr(&field_access.operand)?;

        Some(TypedExpr {
            kind: TypedExprKind::FieldAccess(TypedFieldAccess {
                operand: Box::new(operand),
                name: field_access.field_name.value.clone(),
                is_thin_arrow: field_access.is_thin_arrow,
                dispatch: TypedFieldAccessDispatch::Unresolved,
                ty: None,
                loc: field_access.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: field_access.loc,
        })
    }

    fn resolve_method_call(&mut self, method_call: &ASTMethodCallExpr) -> Option<TypedExpr> {
        let operand = self.resolve_expr(&method_call.operand)?;

        let args: Vec<TypedExpr> = method_call
            .args
            .iter()
            .filter_map(|arg| self.resolve_expr(arg))
            .collect();

        let type_args = self.resolve_type_args(&method_call.type_args)?;

        Some(TypedExpr {
            kind: TypedExprKind::MethodCall(TypedMethodCall {
                operand: Box::new(operand),
                name: method_call.method_name.value.clone(),
                args,
                type_args,

                dispatch: TypedMethodCallDispatch::Unresolved,

                is_thin_arrow: method_call.is_thin_arrow,
                loc: method_call.loc,
            }),
            val_cat: ValueCategory::Unknown,
            ty: None,
            loc: method_call.loc,
        })
    }

    fn resolve_struct_init(&mut self, struct_init: &ASTStructInitExpr) -> Option<TypedExpr> {
        let operand = self.resolve_type(struct_init.operand.clone(), struct_init.loc)?;

        let fields: Vec<TypedFieldInit> = struct_init
            .field_inits
            .iter()
            .filter_map(|field_init| {
                self.resolve_expr(&field_init.value).map(|value| TypedFieldInit {
                    name: field_init.ident.as_string(),
                    value,
                    loc: field_init.loc,
                })
            })
            .collect();

        Some(TypedExpr {
            kind: TypedExprKind::StructInit(TypedStructInitExpr {
                operand,
                fields,
                loc: struct_init.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: struct_init.loc,
        })
    }

    fn resolve_unnamed_struct_value(&mut self, unnamed_struct_value: &ASTUnnamedStructValueExpr) -> Option<TypedExpr> {
        let fields = unnamed_struct_value
            .fields
            .iter()
            .filter_map(|field| {
                self.resolve_expr(&field.value)
                    .map(|value| TypedUnnamedStructValueField {
                        name: field.name.as_string(),
                        value: Box::new(value),
                        loc: field.loc,
                    })
            })
            .collect();

        Some(TypedExpr {
            kind: TypedExprKind::UnnamedStructValue(TypedUnnamedStructValue {
                struct_decl_id: None,
                fields,
                loc: unnamed_struct_value.loc,
                repr_attr: unnamed_struct_value.repr_attr.clone(),
                align: unnamed_struct_value.align,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: unnamed_struct_value.loc,
        })
    }

    fn resolve_func_call(&mut self, func_call: &ASTFuncCallExpr) -> Option<TypedExpr> {
        let operand = self.resolve_expr(&func_call.operand)?;

        let args: Vec<TypedExpr> = func_call.args.iter().filter_map(|arg| self.resolve_expr(arg)).collect();

        let type_args = self.resolve_type_args(&func_call.type_args)?;

        let loc = func_call.loc;

        Some(TypedExpr {
            kind: TypedExprKind::FuncCall(TypedFuncCall {
                operand: Box::new(operand),
                args,
                type_args,
                dispatch: TypedFuncCallDispatch::Unresolved,
                loc,
            }),
            val_cat: ValueCategory::Unknown,
            ty: None,
            loc,
        })
    }

    fn resolve_untyped_array_expr(&mut self, untyped_array: &ASTUntypedArrayExpr) -> Option<TypedExpr> {
        let elements: Vec<TypedExpr> = untyped_array
            .elements
            .iter()
            .filter_map(|item| self.resolve_expr(item))
            .collect();

        Some(TypedExpr {
            kind: TypedExprKind::Array(TypedArrayExpr {
                ty: None,
                elements,
                loc: untyped_array.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: untyped_array.loc,
        })
    }

    fn resolve_array_expr(&mut self, array: &ASTArrayExpr) -> Option<TypedExpr> {
        let array_type = self.resolve_type(array.data_type.clone(), array.loc)?;

        let typed_elements: Vec<TypedExpr> = array
            .elements
            .iter()
            .filter_map(|item| self.resolve_expr(item))
            .collect();

        Some(TypedExpr {
            kind: TypedExprKind::Array(TypedArrayExpr {
                ty: Some(array_type),
                elements: typed_elements,
                loc: array.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: array.loc,
        })
    }

    fn resolve_infix_expr(&mut self, infix: &ASTInfixExpr) -> Option<TypedExpr> {
        let lhs = self.resolve_expr(&*infix.lhs)?;
        let rhs = self.resolve_expr(&*infix.rhs)?;

        Some(TypedExpr {
            kind: TypedExprKind::Infix(TypedInfixExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: infix.op.clone(),
                loc: infix.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: infix.loc,
        })
    }

    fn resolve_prefix_expr(&mut self, prefix: &ASTPrefixExpr) -> Option<TypedExpr> {
        let operand = self.resolve_expr(&*prefix.operand)?;

        Some(TypedExpr {
            kind: TypedExprKind::Prefix(TypedPrefixExpr {
                operand: Box::new(operand),
                op: prefix.op.clone(),
                loc: prefix.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: prefix.loc,
        })
    }

    fn resolve_type_specifier_expr(&mut self, type_spec: &TypeSpecifier) -> Option<TypedExpr> {
        let loc = type_spec.loc();

        let symbol_id = match type_spec {
            TypeSpecifier::Ident(ident) => self.resolve_ident(&ident)?,
            TypeSpecifier::ModuleImport(module_import) => self.resolve_module_import(module_import.clone())?,
            _ => {
                let ty = self.resolve_type(type_spec.clone(), loc)?;

                return Some(TypedExpr {
                    kind: TypedExprKind::SemaType(ty.clone()),
                    ty: Some(ty),
                    val_cat: ValueCategory::Unknown,
                    loc,
                });
            }
        };

        Some(TypedExpr {
            kind: TypedExprKind::Symbol(TypedSymbolExpr::Unresolved { symbol_id, loc }),
            val_cat: ValueCategory::Unknown,
            ty: None,
            loc,
        })
    }

    fn resolve_assign_expr(&mut self, assign: &ASTAssignExpr) -> Option<TypedExpr> {
        let lhs = self.resolve_expr(&assign.lhs)?;
        let rhs = self.resolve_expr(&assign.rhs)?;

        Some(TypedExpr {
            kind: TypedExprKind::Assign(TypedAssignExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                kind: assign.kind.clone(),
                loc: assign.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: assign.loc,
        })
    }

    fn resolve_literal_expr(&mut self, literal: &ASTLiteralExpr) -> Option<TypedExpr> {
        let literal_type = self.resolve_literal_type(literal)?;

        let typed_literal = TypedLiteralExpr {
            ty: literal_type,
            kind: literal.kind.clone(),
            loc: literal.loc,
        };

        Some(TypedExpr {
            kind: TypedExprKind::Literal(typed_literal.clone()),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: typed_literal.loc,
        })
    }

    fn resolve_literal_type(&mut self, literal: &ASTLiteralExpr) -> Option<Option<SemaType>> {
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
    ) -> Option<Option<SemaType>> {
        if let Some(token_kind) = suffix_opt {
            match SemaType::try_from(*token_kind.clone()) {
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
    ) -> Option<SemaType> {
        match string_prefix {
            Some(StringPrefix::B) => {
                let len = string_value.len() + 1;
                let len_expr = literal_expr_from_const_int(len.try_into().unwrap(), loc);

                Some(SemaType::Array(TypedArrayType {
                    element_type: Box::new(SemaType::Plain(PlainType::Char)),
                    capacity: TypedArrayCapacity::Fixed(Box::new(len_expr)),
                    loc,
                }))
            }

            Some(StringPrefix::C) => Some(SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char)))),

            None => Some(SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char)))),
        }
    }

    fn resolve_plain_literal_type(&self, kind: &LiteralKind) -> Option<SemaType> {
        match kind {
            LiteralKind::Bool(_) => Some(SemaType::Plain(PlainType::Bool)),
            LiteralKind::Char(_) => Some(SemaType::Plain(PlainType::Char)),
            LiteralKind::Null => Some(SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Void)))),
            _ => None,
        }
    }

    fn resolve_unary_expr(&mut self, unary: &ASTUnaryExpr) -> Option<TypedExpr> {
        let operand = self.resolve_expr(&*unary.operand)?;

        Some(TypedExpr {
            kind: TypedExprKind::Unary(TypedUnaryExpr {
                op: unary.op.clone(),
                operand: Box::new(operand),
                loc: unary.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: unary.loc,
        })
    }

    fn resolve_array_index_expr(&mut self, array_index: &ASTArrayIndexExpr) -> Option<TypedExpr> {
        let operand = self.resolve_expr(&array_index.operand)?;
        let index = self.resolve_expr(&array_index.index)?;

        Some(TypedExpr {
            kind: TypedExprKind::ArrayIndex(TypedArrayIndexExpr {
                operand: Box::new(operand),
                index: Box::new(index),
                loc: array_index.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: array_index.loc,
        })
    }

    fn resolve_addr_of_expr(&mut self, addr_of: &ASTAddrOfExpr) -> Option<TypedExpr> {
        let operand = self.resolve_expr(&addr_of.expr)?;

        Some(TypedExpr {
            kind: TypedExprKind::AddrOf(TypedAddrOfExpr {
                operand: Box::new(operand),
                loc: addr_of.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: addr_of.loc,
        })
    }

    fn resolve_deref_expr(&mut self, deref: &ASTDerefExpr) -> Option<TypedExpr> {
        let operand = self.resolve_expr(&deref.expr)?;

        Some(TypedExpr {
            kind: TypedExprKind::Deref(TypedDerefExpr {
                operand: Box::new(operand),
                loc: deref.loc,
            }),
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: deref.loc,
        })
    }
}

// Resolver helper methods.
impl<'a> Resolver<'a> {
    fn insert_variable_decl(
        &mut self,
        ident: &Ident,
        ty: Option<SemaType>,
        rhs: Option<TypedExpr>,
        is_const: bool,
    ) -> VarDeclID {
        self.decl_tables.insert_var(VarDecl {
            name: ident.as_string(),
            ty,
            rhs,
            analyzed: false,
            is_const,
            loc: ident.loc,
        })
    }

    fn insert_variable_symbol_to_current_scope(&mut self, ident: &Ident, var_decl_id: VarDeclID) -> Option<()> {
        if self.current_local_scope().unwrap().contains(&ident.value) {
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

        let symbol_id =
            self.global_symbols
                .insert_symbol_entry(SymbolEntry::unresolved(None, self.current_scope, Some(ident.loc)));

        self.with_global_symbol_mut(symbol_id, |symbol_entry| {
            symbol_entry.kind = SymbolEntryKind::Var(var_decl_id)
        });

        self.current_local_scope_mut()
            .unwrap()
            .insert(ident.as_string(), symbol_id);

        Some(())
    }

    fn collect_labels_in_block(&mut self, block_stmt: &ASTBlockStmt) {
        for stmt in &block_stmt.stmts {
            if let ASTStmt::Label(label_stmt) = stmt {
                let name = label_stmt.name.as_string();

                if self.resolve_local_scope_label(&name).is_some() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(ResolverDiagKind::LabelAlreadyDefined {
                            label_name: label_stmt.name.to_string(),
                        }),
                        loc: Some(label_stmt.loc),
                        hint: None,
                    });
                    continue;
                }

                let label_id = self.id_gen.label_id();

                self.current_local_scope_mut()
                    .unwrap()
                    .insert_label(name.clone(), label_id);
            }
        }
    }

    fn is_builtin_active(&self, name: &str) -> bool {
        if let Some(builtin_kind) = lookup_builtin(name) {
            let builtin_spec = builtin_spec_of(builtin_kind);

            if builtin_spec.phase == TypedBuiltinPhase::Resolver {
                match builtin_spec.kind {
                    TypedBuiltinKind::Debug => return self.opts.profile.is_debug(),
                    TypedBuiltinKind::Release => return self.opts.profile.is_release(),
                    _ => {}
                }
            }
        }

        true
    }
}
