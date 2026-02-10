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
use crate::{
    analyze::AnalysisContext,
    diagnostics::AnalyzerDiagKind,
    format::format_missing_fields,
    inference_ctx::{InferenceCtx, struct_sig_as_inference_ctx, unnamed_struct_type_as_inference_ctx},
};
use cyrusc_abi::visibility::Visibility;
use cyrusc_ast::{SelfModifierKind, UnnamedEnumValueKind};
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc, source_loc::SourceLoc};
use cyrusc_resolver::{
    symbols::{LocalOrGlobalSymbol, LocalScopeRef, ResolvedEnum, ResolvedStruct, ResolvedUnion, SymbolEntryKind},
    update_global_symbol,
};
use cyrusc_strescape::unescape_string;
use cyrusc_tast::{
    exprs::*,
    format::{format_func_ty, format_sema_ty, format_typed_expr, format_unnamed_enum_ty},
    generics::{
        generic_type::GenericType,
        mapping_ctx::GenericMappingCtx,
        substitute::{
            substitute_enum_sig, substitute_func_sig, substitute_struct_sig, substitute_type, substitute_union_sig,
        },
    },
    sigs::{
        EnumSig, FuncSig, UnionSig, set_self_modifier_symbol_id_in_func_sig, set_self_modifier_type_in_func_sig,
        typed_func_decl_as_func_sig, typed_func_params_as_func_type_params,
    },
    stmts::{
        TypedEnumValuedField, TypedEnumVariant, TypedFuncParamKind, TypedFuncParams, TypedFuncTypeVariadicParams,
        TypedFuncVariadicParams, TypedGenericParamsList, TypedSelfModifier, TypedStructField, TypedTypeArgs,
    },
    types::*,
    *,
};
use cyrusc_tokens::{
    TokenKind,
    literals::{LiteralKind, StringPrefix},
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

// ============================================================
// Analysis Entry Points
// ============================================================
// These functions are the primary entry points for type checking different
// expression categories. They handle top-level analysis and dispatch to
// specialized helpers for detailed checking.
impl<'a> AnalysisContext<'a> {
    /// Entry point for expression type analysis with pre-validation and generic handling.
    ///
    /// This function serves as the public entry point for type checking expressions.
    /// It performs initial validation and generic parameter handling before delegating
    /// to `analyze_expr_non_terminal` for the actual type analysis.
    ///
    /// # Process
    /// 1. **Symbol Validation**: For symbol expressions, verifies the symbol refers to
    ///    a valid variable or function. Reports errors for non-variable/non-function
    ///    symbols (e.g., types used as values).
    /// 2. **Generic Parameter Resolution**: If the expected type is a generic parameter
    ///    with a default, uses the default type as the expected type.
    /// 3. **Core Analysis**: Delegates to `analyze_expr_non_terminal` for the actual
    ///    type analysis of all expression kinds.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Optional scope ident for name resolution.
    /// - `typed_expr`: The expression statement to analyze. Modified in-place with
    ///   the inferred semantic type.
    /// - `expected_type`: Optional type expected by the surrounding context.
    ///   May be replaced with a generic parameter's default type if applicable.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The inferred semantic type if analysis succeeds.
    /// - `None`: If analysis fails due to:
    ///   - Unknown or invalid symbols (non-variable/non-function symbols)
    ///   - Type errors reported by `analyze_expr_non_terminal`
    ///
    pub(crate) fn analyze_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &mut TypedExprStmt,
        mut expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        match &typed_expr.kind {
            TypedExprKind::Symbol(symbol_id, _) => {
                let scope_opt =
                    scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, *symbol_id)
                    .unwrap();

                if !sym.is_kind_of_variable() && !sym.as_func().is_some() {
                    let symbol_name = (self.symbol_formatter)(scope_id_opt)(*symbol_id);

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::UnknownSymbol { symbol_name }),
                        location: Some(DiagLoc::new(typed_expr.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
            _ => {}
        };

        // if the expected type is a generic parameter with a default, use the default type
        if let Some(sema_ty) = &expected_type {
            if let Some(generic_param) = sema_ty.as_generic_param() {
                expected_type = generic_param.default.clone().map(|sema_ty| *sema_ty);
            }
        }

        self.analyze_expr_non_terminal(scope_id_opt, typed_expr, expected_type)
    }

    /// Type-checks a non-terminal expression by dispatching to appropriate analyzers.
    ///
    /// This is the core type analysis function that handles all non-terminal expressions.
    /// It serves as a dispatcher that delegates to specialized analyzers based on the expression kind.
    ///
    /// # Process
    /// 1. **Const Stripping**: If an `expected_type` is provided, removes any `const`
    ///    qualifier since constness doesn't affect type compatibility for expressions.
    /// 2. **Expression Lowering**: Calls `deduce_special_exprs` to transform certain
    ///    expression patterns before type checking.
    /// 3. **Kind-based Dispatch**: Delegates to specialized analyzers for each
    ///    expression kind (symbols, literals, prefix/infix operators, function calls,
    ///    struct initializations, etc.).
    /// 4. **Type Normalization**: Applies type normalization rules (type aliases,
    ///    generic substitutions, etc.) to the inferred type.
    /// 5. **Validation**: In debug builds, ensures no unresolved symbol types remain
    ///    and that a type was successfully assigned.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Optional scope ident for name resolution.
    /// - `typed_expr`: The expression statement to analyze. Modified in-place with
    ///   the inferred semantic type.
    /// - `expected_type`: Optional type expected by the surrounding context.
    ///   Used for type inference and validation.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The normalized semantic type if analysis succeeds.
    /// - `None`: If analysis fails due to:
    ///   - Invalid usage of semantic type as an expression
    ///   - Type errors in sub-expressions (reported via diagnostics)
    ///
    pub(crate) fn analyze_expr_non_terminal(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &mut TypedExprStmt,
        mut expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        if let Some(sema_ty) = expected_type {
            expected_type = Some(sema_ty.const_inner().clone());
        }

        self.deduce_special_exprs(scope_id_opt, typed_expr, expected_type.clone());

        let sema_ty = match &mut typed_expr.kind {
            TypedExprKind::Symbol(symbol_id, loc) => {
                let scope_opt =
                    scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, *symbol_id)
                    .unwrap();

                let sema_ty = self.resolve_symbol_type(scope_id_opt, sym.symbol_id(), loc.clone());
                typed_expr.sema_ty = sema_ty.clone();
                sema_ty
            }
            TypedExprKind::Literal(typed_literal) => self.analyze_literal(typed_literal, expected_type),
            TypedExprKind::Prefix(typed_prefix_expr) => {
                self.analyze_prefix_expr_type(scope_id_opt, typed_prefix_expr, expected_type)
            }
            TypedExprKind::Infix(typed_infix_expr) => {
                self.analyze_infix_expr_type(scope_id_opt, typed_infix_expr, expected_type)
            }
            TypedExprKind::Unary(typed_unary_expr) => self.analyze_unary_expr_type(scope_id_opt, typed_unary_expr),
            TypedExprKind::Assign(typed_assign) => {
                self.analyze_assign(scope_id_opt, typed_assign);
                typed_assign.rhs.sema_ty.clone()
            }
            TypedExprKind::Cast(typed_cast) => self.analyze_cast(scope_id_opt, typed_cast),
            TypedExprKind::Array(typed_array) => self.analyze_array(scope_id_opt, typed_array, expected_type),
            TypedExprKind::ArrayIndex(typed_array_index) => self.analyze_array_index(scope_id_opt, typed_array_index),
            TypedExprKind::AddrOf(typed_address_of) => self.analyze_addr_of_expr_type(scope_id_opt, typed_address_of),
            TypedExprKind::Deref(typed_deref) => self.analyze_deref_expr_type(scope_id_opt, typed_deref),
            TypedExprKind::StructInit(struct_init) => {
                self.analyze_struct_init(scope_id_opt, struct_init, expected_type)
            }
            TypedExprKind::FuncCall(typed_func_call) => {
                self.analyze_func_call(scope_id_opt, typed_func_call, expected_type)
            }
            TypedExprKind::UnnamedStructValue(typed_unnamed_struct_value) => {
                self.analyze_unnamed_struct_value(scope_id_opt, typed_unnamed_struct_value, expected_type)
            }
            TypedExprKind::UnnamedEnumValue(typed_unnamed_enum_value) => {
                self.analyze_unnamed_enum_value(scope_id_opt, typed_unnamed_enum_value, expected_type)
            }
            TypedExprKind::FieldAccess(field_access) => {
                self.analyze_field_access_type(scope_id_opt, field_access, expected_type)
            }
            TypedExprKind::MethodCall(method_call) => {
                self.analyze_method_call(scope_id_opt, method_call, expected_type)
            }
            TypedExprKind::SizeOf(typed_size_of_expression) => {
                self.analyze_sizeof_expr_type(scope_id_opt, typed_size_of_expression, expected_type)
            }
            TypedExprKind::Lambda(typed_lambda) => self.analyze_lambda(scope_id_opt, typed_lambda),
            TypedExprKind::Tuple(tuple_value) => self.analyze_tuple_value(scope_id_opt, tuple_value, expected_type),
            TypedExprKind::TupleAccess(tuple_member_access) => {
                self.analyze_tuple_member_access(scope_id_opt, tuple_member_access, expected_type)
            }
            TypedExprKind::SemanticType(..) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidUsageOfTheSemanticType),
                    location: Some(DiagLoc::new(typed_expr.loc.clone())),
                    hint: None,
                });
                return None;
            }
            TypedExprKind::Dynamic(typed_dynamic_expr) => {
                self.analyze_dynamic_expr(scope_id_opt, typed_dynamic_expr, expected_type)
            }
        };

        let normalized_type = self.normalize_sema_type(scope_id_opt, sema_ty.clone()?, typed_expr.loc.clone());
        typed_expr.sema_ty = Some(normalized_type.clone()?);

        if cfg!(debug_assertions) {
            if let Some(concrete_type_clone) = typed_expr.sema_ty.clone() {
                let is_unresolved_symbol = matches!(concrete_type_clone, SemanticType::UnresolvedSymbol(..));
                assert!(is_unresolved_symbol == false);
            }

            if typed_expr.sema_ty.is_none() {
                panic!("typed_expr.sema_ty is empty!");
            }
        }

        normalized_type
    }

    /// Analyzes and infers the semantic type for a literal expression.
    ///
    /// This function processes literal expressions (integers, floats, booleans, characters,
    /// null, and strings) by inferring their semantic types based on both the literal's
    /// intrinsic properties and any contextually expected type. For string literals,
    /// it also performs unescaping validation.
    ///
    /// # Process
    /// 1. **Type Inference**: Attempts to infer the most appropriate semantic type
    ///    for the literal based on:
    ///    - The literal's kind (integer, float, bool, etc.)
    ///    - Any explicit suffix (e.g., `42u8`, `3.14f32`)
    ///    - Contextual expectations from the surrounding expression (`expected_type`)
    ///
    /// 2. **String Processing**: For string literals, performs double unescaping
    ///    (e.g., `\\n` → `\n`) and validates escape sequences. Also handles
    ///    string prefixes (`C` for C-style strings, `B` for byte arrays).
    ///
    /// 3. **Type Assignment**: If successful, attaches the inferred type to the
    ///    `typed_literal` AST node and returns it.
    ///
    /// # Parameters
    /// - `typed_literal`: The literal expression to analyze. Modified in-place to
    ///   receive the inferred type.
    /// - `expected_type`: Optional type expected by the surrounding context.
    ///   Used as a hint for type inference (e.g., to choose between `i32` and `i64`).
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The inferred semantic type if analysis succeeds.
    /// - `None`: If analysis fails due to:
    ///   - Invalid integer/float suffixes
    ///   - Unescape errors in string literals
    ///   - Type inference errors
    ///
    /// # Diagnostics
    /// Reports diagnostics through `self.reporter` for:
    /// - Type inference failures (invalid suffixes, mismatched expectations)
    /// - String unescape errors (malformed escape sequences)
    ///
    /// # Notes
    /// - String literals are unescaped twice to handle nested escape sequences.
    /// - Integer and float inference delegates to `infer_integer_type` and
    ///   `infer_float_type` respectively.
    /// - The function clones `typed_literal` once at the beginning to avoid
    ///   borrow conflicts during pattern matching.
    ///
    pub(crate) fn analyze_literal(
        &mut self,
        typed_literal: &mut TypedLiteralExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let typed_literal_clone = typed_literal.clone();

        let ty_opt = match &mut typed_literal.kind {
            LiteralKind::Integer(_, suffix_opt) => {
                match infer_integer_type(&typed_literal_clone, suffix_opt, expected_type.clone()) {
                    Ok(ty) => Some(ty),
                    Err(diag) => {
                        self.reporter.report(diag);
                        None
                    }
                }
            }
            LiteralKind::Float(_, suffix_opt) => {
                match infer_float_type(&typed_literal_clone, suffix_opt, expected_type.clone()) {
                    Ok(ty) => Some(ty),
                    Err(diag) => {
                        self.reporter.report(diag);
                        None
                    }
                }
            }
            LiteralKind::Bool(_) => Some(SemanticType::PlainType(PlainType::Bool)),
            LiteralKind::Char(_) => Some(SemanticType::PlainType(PlainType::Char)),
            LiteralKind::Null => Some(SemanticType::PlainType(PlainType::Null)),
            LiteralKind::String(value, prefix_opt) => {
                *value = match unescape_string(&value).and_then(|v| unescape_string(&v)) {
                    Ok(v) => v,
                    Err(unescape_err) => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::UnescapeError(unescape_err)),
                            location: Some(DiagLoc::new(typed_literal.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                };

                let ty = if let Some(prefix) = prefix_opt {
                    match prefix {
                        StringPrefix::C => SemanticType::Pointer(Box::new(SemanticType::PlainType(PlainType::Char))),
                        StringPrefix::B => SemanticType::Array(TypedArrayType {
                            element_type: Box::new(SemanticType::Const(Box::new(SemanticType::PlainType(
                                PlainType::Char,
                            )))),
                            capacity: TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(
                                value.len().try_into().unwrap(),
                            )),
                            loc: typed_literal.loc.clone(),
                        }),
                    }
                } else {
                    SemanticType::Pointer(Box::new(SemanticType::PlainType(PlainType::Char)))
                };
                Some(ty)
            }
        };

        if let Some(ref ty) = ty_opt {
            typed_literal.ty = Some(ty.clone());
        }

        ty_opt
    }

    /// Analyzes lambda expressions, creating and type-checking anonymous functions.
    ///
    /// Processes lambda expressions by normalizing their parameters and return type,
    /// then type-checks the lambda body in the context of the lambda's function type.
    /// Maintains and restores the current function context during analysis.
    ///
    /// # Process
    /// 1. **Parameter Normalization**: Applies type normalization to lambda parameters.
    /// 2. **Return Type Normalization**: Normalizes the lambda's declared return type.
    /// 3. **Function Type Creation**: Constructs a function type from parameters and return type.
    /// 4. **Body Analysis**: Type-checks the lambda body with the lambda as current function context.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type normalization.
    /// - `lambda`: The lambda expression to analyze.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The function type of the lambda expression.
    /// - `None`: If type normalization fails (errors reported during normalization).
    fn analyze_lambda(&mut self, scope_id_opt: Option<ScopeID>, lambda: &mut TypedLambdaExpr) -> Option<SemanticType> {
        let current_func_clone = self.ty_ctx.current_func.clone();

        self.normalize_func_params(&mut lambda.params, lambda.loc.clone());
        lambda.return_type = self.normalize_sema_type(scope_id_opt, lambda.return_type.clone(), lambda.loc.clone())?;
        let params = typed_func_params_as_func_type_params(&lambda.params);
        let func_type = TypedFuncType {
            symbol_id: None,
            def_module_id: Some(self.module_id),
            params,
            return_type: Box::new(lambda.return_type.clone()),
            is_public: true,
            loc: lambda.loc.clone(),
        };

        self.ty_ctx.current_func = Some(func_type.clone());
        self.analyze_block_stmt(&mut lambda.body);

        self.ty_ctx.current_func = current_func_clone;
        Some(SemanticType::FuncType(func_type))
    }

    /// Analyzes tuple value expressions, inferring types from elements.
    ///
    /// Type-checks tuple literals by analyzing each element expression and
    /// constructing a tuple type from the element types. Uses expected type
    /// context to guide element type inference when available.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for element expression analysis.
    /// - `tuple_value`: The tuple literal expression to analyze.
    /// - `expected_type`: Optional expected tuple type for element inference.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The inferred tuple type.
    /// - `None`: If element analysis fails.
    ///
    fn analyze_tuple_value(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        tuple_value: &mut TypedTupleExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let mut type_list: Vec<SemanticType> = Vec::new();

        let tuple_type_opt = match expected_type {
            Some(sema_ty) => sema_ty.as_tuple_type().cloned(),
            None => None,
        };

        for (idx, expr) in &mut tuple_value.expr_list.iter_mut().enumerate() {
            let mut expected_type: Option<SemanticType> = None;
            if let Some(tuple_type) = &tuple_type_opt {
                expected_type = tuple_type.type_list.get(idx).cloned();
            }

            match self.analyze_expr(scope_id_opt, expr, expected_type) {
                Some(sema_ty) => type_list.push(sema_ty),
                None => continue,
            }
        }

        Some(SemanticType::Tuple(TypedTupleType {
            type_list,
            loc: tuple_value.loc.clone(),
        }))
    }

    fn analyze_unnamed_enum_value(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_enum_value: &mut TypedUnnamedEnumValue,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        let from_unnamed_enum_type = |this: &mut AnalysisContext,
                                          unnamed_enum_type: &TypedUnnamedEnumType,
                                          unnamed_enum_value_kind: &mut TypedUnnamedEnumValueKind|
         -> Option<(SemanticType, TypedUnnamedEnumValueTy)> {
            let Some(variant) = unnamed_enum_type
                .variants
                .iter()
                .find(|variant| *variant.ident() == unnamed_enum_value.ident)
            else {
                let enum_name = format_unnamed_enum_ty(unnamed_enum_type, &(this.symbol_formatter)(scope_id_opt));

                this.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                        enum_name,
                        variant_name: unnamed_enum_value.ident.as_string(),
                    }),
                    location: Some(DiagLoc::new(unnamed_enum_value.loc.clone())),
                    hint: None,
                });
                return None;
            };

            match unnamed_enum_value_kind {
                TypedUnnamedEnumValueKind::Plain => {
                    if !matches!(
                        variant,
                        TypedUnnamedEnumVariant::Ident(..) | TypedUnnamedEnumVariant::Valued(..)
                    ) {
                        let enum_name =
                            format_unnamed_enum_ty(unnamed_enum_type, &(this.symbol_formatter)(scope_id_opt));

                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::VariantMissingFields {
                                enum_name,
                                variant_name: unnamed_enum_value.ident.as_string(),
                            }),
                            location: Some(DiagLoc::new(unnamed_enum_value.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                }
                TypedUnnamedEnumValueKind::Fielded(values) => match variant {
                    TypedUnnamedEnumVariant::Ident(_) | TypedUnnamedEnumVariant::Valued(_, _) => {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::EnumVariantDoesNotAcceptFields {
                                variant_name: unnamed_enum_value.ident.as_string(),
                            }),
                            location: Some(DiagLoc::new(unnamed_enum_value.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                    TypedUnnamedEnumVariant::Variant(_, values_fields) => {
                        if values_fields.len() != values.len() {
                            this.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::EnumVariantArgCountMismatch {
                                    variant_name: unnamed_enum_value.ident.as_string(),
                                    expected: values_fields.len() as u32,
                                    provided: values_fields.len() as u32,
                                }),
                                location: Some(DiagLoc::new(unnamed_enum_value.loc.clone())),
                                hint: None,
                            });
                            return None;
                        }

                        for (mut expr, field) in values.iter_mut().zip(values_fields) {
                            this.analyze_expr(scope_id_opt, &mut expr, Some(field.ty.clone()));
                        }
                    }
                },
            }

            Some((
                SemanticType::UnnamedEnum(unnamed_enum_type.clone()),
                TypedUnnamedEnumValueTy::UnnamedEnum(unnamed_enum_type.clone()),
            ))
        };

        let mut from_enum_sig = |this: &mut AnalysisContext,
                                 generic_type_opt: &Option<GenericType>,
                                 enum_sig: &EnumSig,
                                 mut unnamed_enum_value_kind: &mut TypedUnnamedEnumValueKind|
         -> Option<(SemanticType, TypedUnnamedEnumValueTy)> {
            let variant_opt = enum_sig
                .variants
                .iter()
                .find(|variant| *variant.ident() == unnamed_enum_value.ident);

            match variant_opt {
                Some(variant) => {
                    match &mut unnamed_enum_value_kind {
                        TypedUnnamedEnumValueKind::Plain => {
                            if !matches!(variant, TypedEnumVariant::Ident(..) | TypedEnumVariant::Valued(..)) {
                                this.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(AnalyzerDiagKind::VariantMissingFields {
                                        enum_name: enum_sig.name.clone(),
                                        variant_name: unnamed_enum_value.ident.as_string(),
                                    }),
                                    location: Some(DiagLoc::new(unnamed_enum_value.loc.clone())),
                                    hint: None,
                                });
                                return None;
                            }
                        }
                        TypedUnnamedEnumValueKind::Fielded(values) => match variant {
                            TypedEnumVariant::Ident(_) | TypedEnumVariant::Valued(_, _) => {
                                this.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(AnalyzerDiagKind::EnumVariantDoesNotAcceptFields {
                                        variant_name: unnamed_enum_value.ident.as_string(),
                                    }),
                                    location: Some(DiagLoc::new(unnamed_enum_value.loc.clone())),
                                    hint: None,
                                });
                                return None;
                            }
                            TypedEnumVariant::Variant(_, values_fields) => {
                                if values_fields.len() != values.len() {
                                    this.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: Box::new(AnalyzerDiagKind::EnumVariantArgCountMismatch {
                                            variant_name: unnamed_enum_value.ident.as_string(),
                                            expected: values_fields.len() as u32,
                                            provided: values_fields.len() as u32,
                                        }),
                                        location: Some(DiagLoc::new(unnamed_enum_value.loc.clone())),
                                        hint: None,
                                    });
                                    return None;
                                }

                                for (mut expr, field) in values.iter_mut().zip(values_fields) {
                                    this.analyze_expr(scope_id_opt, &mut expr, Some(field.ty.clone()));
                                }
                            }
                        },
                    }

                    if let Some(generic_type) = generic_type_opt {
                        Some((
                            SemanticType::GenericType(generic_type.clone()),
                            TypedUnnamedEnumValueTy::EnumSig(enum_sig.clone()),
                        ))
                    } else {
                        Some((
                            SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(enum_sig.symbol_id)),
                            TypedUnnamedEnumValueTy::EnumSig(enum_sig.clone()),
                        ))
                    }
                }
                None => None,
            }
        };

        let (generic_type_opt, enum_sig_opt): (Option<GenericType>, Option<EnumSig>) = {
            if let Some(sema_ty) = expected_type {
                if let Some(unnamed_enum_type) = sema_ty.as_unnamed_enum() {
                    return match from_unnamed_enum_type(self, &unnamed_enum_type, &mut unnamed_enum_value.kind) {
                        Some((sema_ty, enum_ty)) => {
                            unnamed_enum_value.enum_ty = Some(enum_ty);
                            Some(sema_ty)
                        }
                        None => None,
                    };
                } else if let Some(enum_symbol_id) = sema_ty.as_enum_symbol_id() {
                    let resolved_enum_opt =
                        match self.resolver.resolve_local_or_global_symbol(scope_opt, enum_symbol_id) {
                            Some(sym) => sym.as_enum().cloned(),
                            None => None,
                        };

                    match resolved_enum_opt {
                        Some(resolved_enum) => (None, Some(resolved_enum.enum_sig)),
                        None => (None, None),
                    }
                } else if let Some(generic_type) = sema_ty.as_generic_type() {
                    let resolved_enum_opt = match self
                        .resolver
                        .resolve_local_or_global_symbol(scope_opt, generic_type.base)
                    {
                        Some(sym) => sym.as_enum().cloned(),
                        None => None,
                    };

                    match resolved_enum_opt {
                        Some(resolved_enum) => {
                            let enum_sig = substitute_enum_sig(
                                self.mapping_ctx_arena.clone(),
                                &resolved_enum.enum_sig,
                                generic_type.mapping_ctx.clone(),
                            )
                            .unwrap();

                            (Some(generic_type.clone()), Some(enum_sig))
                        }
                        None => (None, None),
                    }
                } else {
                    (None, None)
                }
            } else {
                (None, None)
            }
        };

        match enum_sig_opt {
            Some(enum_sig) => {
                return match from_enum_sig(self, &generic_type_opt, &enum_sig, &mut unnamed_enum_value.kind) {
                    Some((sema_ty, enum_ty)) => {
                        unnamed_enum_value.enum_ty = Some(enum_ty);
                        Some(sema_ty)
                    }
                    None => None,
                };
            }
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::UnnamedEnumValueInfering {
                        variant_name: unnamed_enum_value.ident.as_string(),
                    }),
                    location: Some(DiagLoc::new(unnamed_enum_value.loc.clone())),
                    hint: None,
                });
                return None;
            }
        }
    }

    /// Analyzes unnamed (anonymous) struct value expressions.
    ///
    /// Type-checks anonymous struct literals by analyzing each field expression
    /// and constructing an unnamed struct type. Handles both explicitly typed
    /// and inferred field types, and applies const qualifier if specified.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for field expression analysis.
    /// - `unnamed_struct_value`: The unnamed struct literal expression to analyze.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The constructed unnamed struct type (possibly const).
    /// - `None`: If field analysis fails.
    ///
    fn analyze_unnamed_struct_value(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_struct_value: &mut TypedUnnamedStructValue,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        let infer_ctx = self.inference_ctx_from_struct_type(scope_opt, expected_type);

        let mut fields: Vec<TypedUnnamedStructTypeField> = Vec::new();

        for field in &mut unnamed_struct_value.fields {
            let field_expected_type = field.ty.clone().or(infer_ctx.get(&field.name).cloned());

            let field_value_type = match self.analyze_expr(scope_id_opt, &mut field.field_value, field_expected_type) {
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            if let Some(explicit_field_ty) = &field.ty {
                if !self.check_type_mismatch(
                    scope_id_opt,
                    field_value_type.clone(),
                    explicit_field_ty.clone(),
                    field.loc.clone(),
                ) {
                    let lhs_type = format_sema_ty(explicit_field_ty.clone(), &(self.symbol_formatter)(scope_id_opt));
                    let rhs_type = format_sema_ty(field_value_type, &(self.symbol_formatter)(scope_id_opt));
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type }),
                        location: Some(DiagLoc::new(field.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }

            fields.push(TypedUnnamedStructTypeField {
                name: field.name.clone(),
                ty: Box::new(field.ty.clone().unwrap_or(field_value_type)),
                loc: field.loc.clone(),
            });
        }

        let unnamed_struct_type = TypedUnnamedStructType {
            fields,
            is_packed: unnamed_struct_value.is_packed,
            loc: unnamed_struct_value.loc.clone(),
        };

        unnamed_struct_value.unnamed_struct_type = Some(unnamed_struct_type.clone());

        if unnamed_struct_value.is_const {
            Some(SemanticType::Const(Box::new(SemanticType::UnnamedStruct(
                unnamed_struct_type,
            ))))
        } else {
            Some(SemanticType::UnnamedStruct(unnamed_struct_type))
        }
    }

    /// Analyzes array index expressions with bounds and type validation.
    ///
    /// Type-checks array indexing operations on both arrays and pointers.
    /// Validates the index expression is integer type and the operand is
    /// indexable (array or pointer). Returns the element type with proper
    /// const qualification.
    ///
    /// # Validation
    /// - Operand must be array type or pointer type.
    /// - Index expression must be integer type.
    /// - For pointer operands, element type must not be void.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for operand and index analysis.
    /// - `array_index`: The array index expression to analyze.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The element type (const-qualified if operand is const).
    /// - `None`: If operand isn't indexable, index isn't integer, or void pointer dereference.
    ///
    fn analyze_array_index(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        array_index: &mut TypedArrayIndexExpr,
    ) -> Option<SemanticType> {
        let operand_type = match self.analyze_expr(scope_id_opt, &mut array_index.operand, None) {
            Some(sema_ty) => sema_ty,
            None => return None,
        };

        let is_operand_const = operand_type.is_const();
        let is_operand_array = operand_type.const_inner().is_array();

        if !(operand_type.is_pointer() || is_operand_array) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ArrayIndexOnNonArrayOperand),
                location: Some(DiagLoc::new(array_index.loc.clone())),
                hint: None,
            });
            return None;
        }

        let index_inner_type = array_index.index.sema_ty.clone();
        let index_concrete_type = match self.analyze_expr(scope_id_opt, &mut array_index.index, index_inner_type) {
            Some(sema_ty) => sema_ty,
            None => return None,
        };

        if !index_concrete_type
            .const_inner()
            .as_basic_type()
            .and_then(|b| Some(b.is_integer()))
            .is_some()
        {
            let found_type = format_sema_ty(index_concrete_type, &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ArrayNonIntegerIndex { found_type }),
                location: Some(DiagLoc::new(array_index.loc.clone())),
                hint: None,
            });
            return None;
        }

        let sema_ty = array_index.operand.sema_ty.clone().unwrap();

        let element_type: SemanticType;
        if is_operand_array {
            let array_type = sema_ty.as_array_type().unwrap();
            element_type = *array_type.element_type.clone();
        } else {
            // array index on pointer operand
            element_type = sema_ty.pointer_inner().clone();

            if element_type.is_void() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DerefVoidPointerValue),
                    location: Some(DiagLoc::new(array_index.loc.clone())),
                    hint: None,
                });
                return None;
            }
        }

        if is_operand_const {
            Some(element_type.as_const())
        } else {
            Some(element_type)
        }
    }

    /// Analyzes field access expressions with multi-dispatch for different types.
    ///
    /// Type-checks field accesses on structs, unions, and anonymous structs.
    /// Detects enum variant access patterns, handles generic types, and validates
    /// operator syntax. Dispatches to appropriate specialized analyzer based on
    /// the operand's type category.
    ///
    /// # Process
    /// 1. **Operand Analysis**: Type-checks the field access operand.
    /// 2. **Enum Variant Detection**: Checks if field access is enum unit variant.
    /// 3. **Type Category Resolution**: Determines operand type (struct/union/unnamed).
    /// 4. **Generic Substitution**: Applies generic type substitutions if needed.
    /// 5. **Specialized Analysis**: Delegates to appropriate field access analyzer.
    /// 6. **Const Propagation**: Preserves const qualification from operand.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and symbol resolution.
    /// - `field_access`: The field access expression to analyze.
    /// - `expected_type`: Optional expected type for context.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The type of the accessed field (const if operand is const).
    /// - `None`: If operand doesn't support fields, field doesn't exist, or validation fails.
    ///
    fn analyze_field_access_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        macro_rules! not_supports_fields {
            ($this:expr, $loc:expr) => {{
                $this.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsFields),
                    location: Some(DiagLoc::new($loc)),
                    hint: None,
                });
                return None;
            }};
        }

        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        // check for enum variant

        self.analyze_expr_non_terminal(scope_id_opt, &mut field_access.operand, expected_type.clone())?;
        let is_operand_const = field_access
            .operand
            .sema_ty
            .as_ref()
            .map(|sema_ty| sema_ty.is_const())
            .unwrap_or(false);

        let mut field_access_operand_ty = field_access
            .operand
            .sema_ty
            .as_ref()
            .map(|sema_ty| sema_ty.const_inner())
            .cloned()?;

        {
            if let Some(symbol_id) = field_access_operand_ty.symbol_id() {
                if let Some(sym) = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt.clone(), symbol_id)
                {
                    if self.check_unexpected_type_args(
                        &sym.symbol_generic_params(),
                        &field_access.type_args,
                        field_access.loc.clone(),
                    ) {
                        return None;
                    };
                }
            }
        }

        if let Some(sema_ty) =
            self.merge_generic_operand_with_expected_type(field_access_operand_ty.clone(), expected_type.clone())
        {
            field_access_operand_ty = SemanticType::GenericType(sema_ty);
        }

        {
            let (detected_as_enum_variant, sema_ty) = self.maybe_enum_variant_constructor_from_field_access(
                scope_id_opt,
                scope_opt.clone(),
                field_access_operand_ty.clone(),
                field_access,
                expected_type.clone(),
            );
            if detected_as_enum_variant {
                return sema_ty;
            }
        }

        // multiplex field access

        let sema_ty = self.analyze_expr(scope_id_opt, &mut field_access.operand, expected_type.clone())?;

        // for thin-arrow field access, unwrap const and pointer layers
        // to obtain the underlying pointee type used as the operand.
        let operand_ty = sema_ty.const_inner().pointer_inner();

        let generic_type_opt = operand_ty.as_generic_type();

        let (return_sema_ty, is_generic) = match self.resolve_member_access_kind(
            scope_id_opt,
            scope_opt.clone(),
            &mut field_access.operand,
            expected_type.clone(),
            field_access.loc.clone(),
        ) {
            Some(member_access_kind) => match member_access_kind {
                FieldAccessKind::UnnamedStruct(unnamed_struct_type) => (
                    self.analyze_unnamed_struct_field_access(
                        scope_id_opt,
                        &unnamed_struct_type,
                        field_access,
                        expected_type.clone(),
                    ),
                    false,
                ),
                FieldAccessKind::NamedStruct(resolved_struct) => {
                    let mut struct_sig = resolved_struct.struct_sig.clone();
                    if let Some(generic_type) = generic_type_opt {
                        struct_sig = substitute_struct_sig(
                            self.mapping_ctx_arena.clone(),
                            &struct_sig,
                            generic_type.mapping_ctx.clone(),
                        )?;
                    }

                    (
                        self.analyze_struct_field_access(
                            scope_id_opt,
                            field_access,
                            struct_sig.name.clone(),
                            struct_sig.fields.clone(),
                            struct_sig.methods.clone(),
                            resolved_struct.symbol_id,
                        ),
                        struct_sig.generic_params.is_some(),
                    )
                }
                FieldAccessKind::Union(resolved_union) => {
                    let mut union_sig = resolved_union.union_sig.clone();
                    if let Some(generic_type) = generic_type_opt {
                        union_sig = substitute_union_sig(
                            self.mapping_ctx_arena.clone(),
                            &union_sig,
                            generic_type.mapping_ctx.clone(),
                        )?;
                    }

                    (
                        self.analyze_union_field_access(scope_id_opt, &union_sig, field_access, expected_type),
                        union_sig.generic_params.is_some(),
                    )
                }
            },
            None => not_supports_fields!(self, field_access.loc.clone()),
        };

        if !is_generic && field_access.type_args.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
            });
            return None;
        }

        if is_operand_const {
            return_sema_ty.map(|sema_ty| sema_ty.as_const())
        } else {
            return_sema_ty
        }
    }

    /// Analyzes struct/union initialization expressions (e.g., `Point { x: 1, y: 2 }`).
    ///
    /// Type-checks struct and union initialization syntax, handling both generic and
    /// non-generic types. Delegates to specialized analyzers for structs vs unions
    /// after performing common validation and generic type initialization.
    ///
    /// # Process
    /// 1. **Symbol Resolution**: Resolves the struct/union symbol being initialized.
    /// 2. **Type Argument Validation**: Checks for unexpected type arguments on non-generic types.
    /// 3. **Full Type Resolution**: Resolves the complete semantic type of the symbol.
    /// 4. **Generic Type Initialization**: Initializes generic type parameters if applicable.
    /// 5. **Type-Specific Analysis**: Delegates to struct or union initialization analyzers.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and symbol resolution.
    /// - `struct_init`: The struct initialization AST node to analyze.
    /// - `expected_type`: Optional expected type context for generic inference.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The type of the initialized struct/union value.
    /// - `None`: If symbol is not a struct/union, validation fails, or generic instantiation fails.
    ///
    fn analyze_struct_init(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        struct_init: &mut TypedStructInitExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        let mut sym = self
            .resolver
            .resolve_local_or_global_symbol(scope_opt.clone(), struct_init.symbol_id)
            .unwrap();

        self.check_unexpected_type_args(
            &sym.symbol_generic_params(),
            &struct_init.type_args,
            struct_init.loc.clone(),
        );

        let mut sema_ty = self.resolve_symbol_type(scope_id_opt, sym.symbol_id(), struct_init.loc.clone())?;

        if let Some(new_sema_ty) = self.merge_generic_operand_with_expected_type(sema_ty.clone(), expected_type.clone())
        {
            sema_ty = SemanticType::GenericType(new_sema_ty);
        }

        let Some(pure_symbol_id) = sema_ty.maybe_generic_base_symbol_id() else {
            // normalize the type to provide a meaningful error message.
            // If the symbol isn't a struct/enum/union, this might be an incorrect
            // array initialization attempt. Normalization ensures we report the
            // actual resolved type rather than a placeholder.
            let normalized = self.normalize_sema_type(scope_id_opt, sema_ty, struct_init.loc.clone())?;
            let symbol_name = format_sema_ty(normalized, &(self.symbol_formatter)(scope_id_opt));
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NonStructSymbol { symbol_name }),
                location: Some(DiagLoc::new(struct_init.loc.clone())),
                hint: None,
            });
            return None;
        };

        let (generic_params, parent_mapping_ctx) = self.extract_and_merge_generic_context(
            &sema_ty,
            sym.symbol_generic_params().as_ref(),
            expected_type.clone(),
        );

        let generic_type_opt = match self.init_generic_type_with_symbol_id(
            scope_id_opt,
            scope_opt.clone(),
            pure_symbol_id,
            &mut struct_init.type_args,
            parent_mapping_ctx,
            generic_params.as_ref(),
            struct_init.is_const,
            struct_init.loc.clone(),
        ) {
            Ok(opt) => opt?.1,
            Err(diag) => {
                self.reporter.report(diag);
                return None;
            }
        };

        {
            struct_init.symbol_id = pure_symbol_id;

            // optimized
            if pure_symbol_id != sym.symbol_id() {
                sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt.clone(), pure_symbol_id)
                    .unwrap();
            }

            if let Some(resolved_union) = sym.as_union() {
                return self.analyze_regular_union_init(scope_id_opt, struct_init, resolved_union, &generic_type_opt);
            } else if let Some(resolved_struct) = sym.as_struct() {
                let infer_ctx = self.inference_ctx_from_struct_type(scope_opt, expected_type);
                return self.analyze_regular_struct_init(
                    scope_id_opt,
                    struct_init,
                    resolved_struct,
                    &generic_type_opt,
                    &infer_ctx,
                );
            }
        }

        let symbol_name = (self.symbol_formatter)(scope_id_opt)(pure_symbol_id);
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(AnalyzerDiagKind::NonStructSymbol { symbol_name }),
            location: Some(DiagLoc::new(struct_init.loc.clone())),
            hint: None,
        });
        None
    }

    /// Analyzes function call expressions, handling both named functions and function values.
    ///
    /// Type-checks function calls for both named functions and first-class function values
    /// (function pointers, lambdas). Handles generic function instantiation, private function
    /// access validation, and delegates to appropriate checking routines based on call type.
    ///
    /// # Process
    /// 1. **Operand Analysis**: Type-checks the function expression operand.
    /// 2. **Function Type Detection**: Determines if operand is a function type or value.
    /// 3. **Access Control**: Validates private function access across modules.
    /// 4. **Generic Instantiation**: Initializes generic types for generic functions.
    /// 5. **Call Validation**: Delegates to `check_func_call` or `check_func_type_call`.
    /// 6. **Generic Specialization**: Registers monomorphized functions for generic calls.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and symbol resolution.
    /// - `func_call`: The function call AST node to analyze.
    /// - `expected_type`: Optional expected return type for generic inference.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The return type of the function call.
    /// - `None`: If operand isn't callable, access violation, or validation fails.
    ///
    fn analyze_func_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_call: &mut TypedFuncCall,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let operand_ty = self.analyze_expr_non_terminal(scope_id_opt, &mut func_call.operand, None)?;

        #[allow(unused_assignments)]
        let mut generic_type_opt: Option<GenericType> = None;
        let mut func_sig: FuncSig;

        if let Some(mut func_type) = operand_ty.const_inner().as_func_type().cloned() {
            if !func_type.is_public && func_type.def_module_id != Some(self.module_id) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::PrivateFunctionCall {
                        name: format_typed_expr(&func_call.operand, &(self.symbol_formatter)(scope_id_opt)),
                    }),
                    location: Some(DiagLoc::new(func_call.loc.clone())),
                    hint: None,
                });
                return None;
            }

            if let Some(symbol_id) = func_type.symbol_id {
                let scope_opt =
                    scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt.clone(), symbol_id)
                    .unwrap();

                func_sig = sym.as_func().unwrap().func_sig.clone();

                if self.check_unexpected_type_args(
                    &func_sig.generic_params,
                    &func_call.type_args,
                    func_call.loc.clone(),
                ) {
                    return None;
                }

                let expected_mapping_ctx = self.export_expected_generic_mapping_ctx(expected_type);

                let (_, inner_generic_type_opt) = match self.init_generic_type_with_symbol_id(
                    scope_id_opt,
                    scope_opt.clone(),
                    symbol_id,
                    &mut func_call.type_args,
                    expected_mapping_ctx,
                    func_sig.generic_params.as_ref(),
                    operand_ty.is_const(),
                    func_call.loc.clone(),
                ) {
                    Ok(opt) => opt?,
                    Err(diag) => {
                        self.reporter.report(diag);
                        return None;
                    }
                };

                generic_type_opt = inner_generic_type_opt;
            } else {
                if func_call.type_args.is_some() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                        location: Some(DiagLoc::new(func_call.loc.clone())),
                        hint: Some("Lambdas never accept type args.".to_string()),
                    });
                    return None;
                }

                // normalize if is lambda call
                self.normalize_func_type_params(&mut func_type.params, func_call.loc.clone());

                let return_type = self.check_func_type_call(
                    scope_id_opt,
                    &mut func_type,
                    &mut func_call.args,
                    func_call.loc.clone(),
                )?;

                func_call.return_type = Some(return_type.clone());
                return Some(return_type);
            }
        } else {
            let symbol_name = format_sema_ty(operand_ty, &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NonFunctionSymbol { symbol_name }),
                location: Some(DiagLoc::new(func_call.loc.clone())),
                hint: None,
            });
            return None;
        }

        self.normalize_func_params(&mut func_sig.params, func_call.loc.clone());

        self.check_func_call(
            scope_id_opt,
            &mut func_sig,
            &generic_type_opt,
            &mut func_call.args,
            func_call.loc.clone(),
            false,
        )?;

        update_global_symbol!(self, func_sig.module_id, func_sig.symbol_id.unwrap(),
            SymbolEntryKind::Func(resolved_func) => resolved_func, {
                resolved_func.func_sig.params = resolved_func.func_sig.params.clone();
                resolved_func.func_sig.return_type = func_sig.return_type.clone();
            }
        );

        // validate generic type instantiation
        if let Some(generic_type) = generic_type_opt {
            // substitution before body analysis
            func_sig = substitute_func_sig(
                self.mapping_ctx_arena.clone(),
                &func_sig,
                generic_type.mapping_ctx.clone(),
            )
            .unwrap();

            if let Err(diag) = generic_type.finalize(
                self.mapping_ctx_arena.clone(),
                func_sig.generic_params.clone().unwrap(),
                (self.symbol_formatter)(scope_id_opt),
            ) {
                self.reporter.report(diag);
                return None;
            }

            if !func_sig.is_func_decl {
                // only specialize function definition which necessarily includes the body block
                func_call.monomorph_key =
                    self.register_specialized_generic_func(&mut func_sig, &generic_type, None, &func_call.loc);
            }

            // substitutes the func type inside of the func_call operand
            func_call.operand.sema_ty = substitute_type(
                self.mapping_ctx_arena.clone(),
                func_call.operand.sema_ty.clone().unwrap(),
                generic_type.mapping_ctx,
            );
        }

        func_call.return_type = Some(func_sig.return_type.clone());
        Some(func_sig.return_type)
    }

    /// Analyzes method call expressions, handling both static and instance methods.
    ///
    /// Type-checks method calls by determining whether the call is on a type (static)
    /// or instance, resolving the object type, and delegating to regular method analysis.
    /// Special handling for enum variant constructors via method call syntax.
    ///
    /// # Process
    /// 1. **Operand Analysis**: Type-checks the method call operand.
    /// 2. **Call Type Detection**: Determines if call is on instance or type.
    /// 3. **Enum Variant Detection**: Checks if method call is actually enum variant construction.
    /// 4. **Object Type Resolution**: Extracts the underlying object type from the operand.
    /// 5. **Generic Type Handling**: Manages generic type construction for non-generic operands.
    /// 6. **Regular Method Analysis**: Delegates to `analyze_regular_method_call`.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and symbol resolution.
    /// - `method_call`: The method call AST node to analyze.
    /// - `expected_type`: Optional expected return type for generic inference.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The return type of the method call.
    /// - `None`: If object doesn't support methods, validation fails, or method not found.
    ///
    fn analyze_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        method_call: &mut TypedMethodCall,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));
        let loc = method_call.loc.clone();

        self.analyze_expr_non_terminal(scope_id_opt, &mut method_call.operand, expected_type.clone());

        let mut method_call_operand_ty = method_call
            .operand
            .sema_ty
            .as_ref()
            .map(|sema_ty| sema_ty.const_inner())
            .cloned()?;

        method_call_operand_ty = self
            .normalize_sema_type(scope_id_opt, method_call_operand_ty, method_call.loc.clone())
            .unwrap();

        // try as interface method call
        if let Some(interface_type) = method_call_operand_ty.as_interface_type() {
            return self.analyze_interface_method_call(scope_id_opt, method_call, interface_type);
        }

        // try as enum variant constructor
        {
            let (detected_as_enum_variant, sema_ty) = self.maybe_enum_variant_constructor_from_method_call(
                scope_id_opt,
                scope_opt.clone(),
                method_call_operand_ty.clone(),
                method_call,
                expected_type.clone(),
            );

            if detected_as_enum_variant {
                return sema_ty;
            }
        }

        // method call analysis

        // this only used to determine that, it's instance/static method call.
        let unresolved_symbol_id = method_call.operand.kind.as_symbol_id();

        let is_instance_method_operand = unresolved_symbol_id
            .and_then(|symbol_id| {
                self.resolver
                    .resolve_local_or_global_symbol(scope_opt.clone(), symbol_id)
            })
            .map_or(false, |sym| {
                sym.as_variable().is_some() || sym.as_global_var().is_some()
            });

        let object_symbol_id = {
            let operand_ty = method_call_operand_ty
                .symbol_id()
                .and_then(|symbol_id| {
                    self.analyze_var_or_global_var_type(
                        scope_id_opt,
                        scope_opt.clone(),
                        symbol_id,
                        method_call.loc.clone(),
                    )
                })
                .or(self.analyze_expr_non_terminal(scope_id_opt, &mut method_call.operand, expected_type.clone()))
                .map(|sema_ty| sema_ty.const_inner().clone())?;

            match operand_ty {
                SemanticType::Interface(interface_type) => interface_type.symbol_id,
                SemanticType::GenericType(generic_type) => generic_type.base,
                SemanticType::ResolvedSymbol(resolved_symbol) => resolved_symbol.symbol_id(),
                SemanticType::Pointer(sema_ty) => {
                    if sema_ty.is_void() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsMethods),
                            location: Some(DiagLoc::new(method_call.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }

                    self.extract_object_symbol_id(scope_id_opt, *sema_ty.clone(), loc.clone())
                        .unwrap()
                }
                _ => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsMethods),
                        location: Some(DiagLoc::new(method_call.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        };

        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        let sym = match self
            .resolver
            .resolve_local_or_global_symbol(scope_opt.clone(), object_symbol_id)
        {
            Some(sym) => sym,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsMethods),
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let object_id_opt = {
            if let Some(resolved_struct) = sym.as_struct() {
                // static method call
                Some(resolved_struct.symbol_id)
            } else if let Some(resolved_enum) = sym.as_enum() {
                Some(resolved_enum.symbol_id)
            } else if let Some(resolved_union) = sym.as_union() {
                Some(resolved_union.symbol_id)
            } else {
                // instance method call
                if let Some(resolved_var) = sym.as_variable() {
                    let var_type = resolved_var
                        .typed_variable
                        .ty
                        .clone()
                        .unwrap_or({
                            self.analyze_expr(
                                scope_id_opt,
                                &mut resolved_var.typed_variable.rhs.clone().unwrap(),
                                expected_type.clone(),
                            )
                            .unwrap()
                        })
                        .const_inner()
                        .clone();

                    match self.extract_object_symbol_id(scope_id_opt, var_type, loc) {
                        Some(object_id) => Some(object_id),
                        None => None,
                    }
                } else if let Some(resolved_global_var) = sym.as_global_var() {
                    let var_type = resolved_global_var
                        .global_var_sig
                        .ty
                        .clone()
                        .unwrap()
                        .const_inner()
                        .clone();

                    match self.extract_object_symbol_id(scope_id_opt, var_type, loc) {
                        Some(object_id) => Some(object_id),
                        None => None,
                    }
                } else if let Some(resolved_interface) = sym.as_interface() {
                    Some(resolved_interface.interface_sig.symbol_id)
                } else {
                    None
                }
            }
        };

        let object_id = match object_id_opt {
            Some(object_id) => object_id,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsFields),
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        // constructing generic type manually if operand is not generic but symbol is.
        // this is necessary because without it, a generic method that uses object's generic params
        // can never be inferred.
        if sym.symbol_generic_params().is_some() && method_call_operand_ty.pointer_inner().as_generic_type().is_none() {
            method_call_operand_ty = SemanticType::GenericType(GenericType {
                base: method_call_operand_ty.maybe_generic_base_symbol_id().unwrap(),
                type_args: None,
                mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
                mapping_ctx_arena: self.mapping_ctx_arena.clone(),
                generic_params: sym.symbol_generic_params().unwrap(),
                is_const: false,
                loc: method_call.loc.clone(),
            });
        }

        self.analyze_regular_method_call(
            scope_id_opt,
            scope_opt,
            method_call,
            object_id,
            method_call_operand_ty,
            is_instance_method_operand,
            expected_type,
        )
    }

    /// Analyzes array literal expressions with type and capacity validation.
    ///
    /// Type-checks array literals by validating each element type matches the array's
    /// element type and verifying the element count matches the declared array capacity.
    ///
    /// # Validation
    /// - Each array element type must match the declared array element type.
    /// - Number of elements must exactly match the declared array capacity.
    /// - Array type is normalized before validation.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and normalization.
    /// - `typed_array`: The array literal expression to analyze.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The array type if validation passes.
    /// - `None`: If type mismatch, capacity mismatch, or normalization fails.
    ///
    fn analyze_array(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_array: &mut TypedArrayExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        macro_rules! array_type {
            () => {
                typed_array.array_type.clone().unwrap().as_array_type().unwrap()
            };
        }

        let mut analyzed_first_element = false;

        let expected_element_type = expected_type.and_then(|sema_ty| {
            sema_ty
                .as_array_type()
                .map(|array_type| *array_type.element_type.clone())
        });

        // try to infer from first element
        if typed_array.array_type.is_none() {
            if let Some(first_elem) = typed_array.elements.first_mut() {
                if let Some(sema_ty) = self.analyze_expr(scope_id_opt, first_elem, expected_element_type.clone()) {
                    let elements_count = typed_array.elements.len();

                    typed_array.array_type = Some(SemanticType::Array(TypedArrayType {
                        element_type: Box::new(sema_ty),
                        capacity: TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(
                            elements_count.try_into().unwrap(),
                        )),
                        loc: typed_array.loc.clone(),
                    }));
                }
                analyzed_first_element = true;
            }
        }

        // try to infer from expected type
        if typed_array.array_type.is_none() {
            if let Some(sema_ty) = expected_element_type {
                let elements_count = typed_array.elements.len();

                typed_array.array_type = Some(SemanticType::Array(TypedArrayType {
                    element_type: Box::new(sema_ty),
                    capacity: TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(
                        elements_count.try_into().unwrap(),
                    )),
                    loc: typed_array.loc.clone(),
                }));
            }
        }

        if typed_array.array_type.is_none() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UntypedArrayCannotBeInferred),
                location: Some(DiagLoc::new(typed_array.loc.clone())),
                hint: None,
            });
        }

        typed_array.array_type =
            match self.normalize_sema_type(scope_id_opt, typed_array.array_type.clone()?, typed_array.loc.clone()) {
                Some(sema_ty) => Some(sema_ty),
                None => return None,
            };

        for (argument_idx, argument) in typed_array.elements.iter_mut().enumerate() {
            let argument_type = {
                if analyzed_first_element && argument.sema_ty.is_some() {
                    argument.sema_ty.clone().unwrap()
                } else {
                    match self.analyze_expr(scope_id_opt, argument, Some(*array_type!().element_type.clone())) {
                        Some(sema_ty) => sema_ty,
                        None => continue,
                    }
                }
            };

            let element_type =
                match self.normalize_sema_type(scope_id_opt, *array_type!().element_type.clone(), argument.loc.clone())
                {
                    Some(sema_ty) => sema_ty,
                    None => continue,
                };

            if !self.check_type_mismatch(scope_id_opt, argument_type.clone(), element_type, argument.loc.clone()) {
                let element_type = format_sema_ty(argument_type, &(self.symbol_formatter)(scope_id_opt));
                let expected_type = format_sema_ty(
                    *array_type!().element_type.clone(),
                    &(self.symbol_formatter)(scope_id_opt),
                );

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ArrayElementTypeMismatch {
                        element_type,
                        element_index: argument_idx.try_into().unwrap(),
                        expected_type,
                    }),
                    location: Some(DiagLoc::new(typed_array.loc.clone())),
                    hint: None,
                });
            }
        }

        let array_type = array_type!().clone();
        let array_capacity = match &array_type.capacity {
            TypedArrayCapacity::Fixed(capacity_value) => match capacity_value {
                TypedArrayFixedCapacityValue::Expr(typed_expr) => {
                    self.const_expr_as_raw_integer(scope_id_opt, typed_expr)?
                }
                TypedArrayFixedCapacityValue::Value(value) => *value as i128,
            },
            TypedArrayCapacity::Dynamic => todo!(),
        };

        if typed_array.elements.len() != array_capacity.try_into().unwrap() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ArrayElementsCountMismatch {
                    elements: typed_array.elements.len().try_into().unwrap(),
                    expected: array_capacity.try_into().unwrap(),
                }),
                location: Some(DiagLoc::new(typed_array.loc.clone())),
                hint: None,
            });
            return None;
        }

        Some(SemanticType::Array(
            typed_array.array_type.clone().unwrap().as_array_type().unwrap().clone(),
        ))
    }

    /// Analyzes explicit type cast expressions.
    ///
    /// Validates explicit type casts by checking if the cast is allowed between
    /// the source and target types. Supports both compatible type casts and
    /// explicit conversions between distinct but convertible types.
    ///
    /// # Validation
    /// - Source type must be type-compatible with target type, or
    /// - Explicit conversion must be defined between the types.
    /// - Both operand and target types are normalized before checking.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and normalization.
    /// - `cast`: The type cast expression to analyze.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The target type if cast is valid.
    /// - `None`: If cast is invalid (type mismatch and no explicit conversion).
    ///
    fn analyze_cast(&mut self, scope_id_opt: Option<ScopeID>, cast: &mut TypedCastExpr) -> Option<SemanticType> {
        let operand = match self.analyze_expr(scope_id_opt, &mut cast.operand, Some(cast.target_type.clone())) {
            Some(sema_ty) => sema_ty.const_inner().clone(),
            None => return None,
        };

        cast.target_type = self
            .normalize_sema_type(scope_id_opt, cast.target_type.clone(), cast.loc.clone())
            .unwrap()
            .const_inner()
            .clone();

        if !(self.check_type_mismatch(
            scope_id_opt,
            operand.clone(),
            cast.target_type.clone(),
            cast.loc.clone(),
        ) || self.check_explicit_typecast(operand.clone(), cast.target_type.clone()))
        {
            let lhs_type = format_sema_ty(cast.target_type.clone(), &(self.symbol_formatter)(scope_id_opt));
            let rhs_type = format_sema_ty(operand, &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CastTypeMismatch { lhs_type, rhs_type }),
                location: Some(DiagLoc::new(cast.loc.clone())),
                hint: None,
            });
            return None;
        }

        Some(cast.target_type.clone())
    }

    /// Analyzes dynamic expression operations (dynamic dispatch/interface implementation).
    ///
    /// Processes dynamic expressions that enable runtime polymorphism through interface types,
    /// validating type compatibility, constructing virtual method tables (vtables), and
    /// establishing dynamic dispatch capabilities. This function transforms static type
    /// information into runtime-usable dynamic type information.
    ///
    /// # Key Responsibilities
    /// 1. Operand Analysis: Ensures the operand expression is semantically valid.
    /// 2. Nesting Prevention: Guards against invalid nested dynamic expressions.
    /// 3. Interface Type Validation: Confirms the expected type is a valid interface.
    /// 4. Method Adaptation: Adjusts interface method signatures for the concrete type.
    /// 5. VTable Construction: Creates and registers runtime dispatch information.
    ///
    /// # Process
    /// 1. Operand Validation: Analyzes the base expression for type correctness.
    /// 2. Dynamic Nesting Check: Prevents invalid dynamic(dynamic(...)) patterns.
    /// 3. Interface Extraction: Extracts interface type from expected type context.
    /// 4. Method Signature Adaptation: Updates self parameters in interface methods.
    /// 5. VTable Registration: Creates runtime dispatch table for concrete-to-interface mapping.
    /// 6. Dynamic Type Creation: Produces the resulting dynamic semantic type.
    ///
    /// # Parameters
    /// - scope_id_opt: Scope for type analysis and symbol resolution.
    /// - dynamic_expr: The dynamic expression to analyze (modified in-place).
    /// - expected_type: Optional expected semantic type (must be an interface type).
    ///
    /// # Returns
    /// - Some(SemanticType): The resulting dynamic type encapsulating interface and vtable information.
    /// - None: If analysis fails due to type errors or invalid expressions.
    ///
    fn analyze_dynamic_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        dynamic_expr: &mut TypedDynamicExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let operand_ty = self.analyze_expr(scope_id_opt, &mut dynamic_expr.operand, None)?;

        if dynamic_expr.operand.kind.is_dynamic_expr() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidMultipleDynamicType),
                location: Some(DiagLoc::new(dynamic_expr.loc.clone())),
                hint: None,
            });
            return None;
        }

        let Some(mut interface_type) = expected_type.and_then(|sema_ty| sema_ty.as_interface_type().cloned()) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotInferDynamicInterfaceType),
                location: Some(DiagLoc::new(dynamic_expr.loc.clone())),
                hint: None,
            });
            return None;
        };

        let interface_name = (self.symbol_formatter)(scope_id_opt)(interface_type.symbol_id);

        interface_type.methods.iter_mut().for_each(|func_sig| {
            set_self_modifier_type_in_func_sig(func_sig, &dynamic_expr.operand.sema_ty.as_ref().unwrap());
            set_self_modifier_symbol_id_in_func_sig(func_sig, 0);
        });

        {
            let mut vtable_registry = self.vtable_registry.lock().unwrap();
            let vtable_id = vtable_registry.register(
                operand_ty,
                interface_type.symbol_id,
                interface_name,
                interface_type.methods,
            );

            dynamic_expr.vtable_id = Some(vtable_id);
            dynamic_expr.object_name = Some((self.symbol_formatter)(scope_id_opt)(
                dynamic_expr.operand.sema_ty.as_ref().unwrap().symbol_id().unwrap(),
            ));

            Some(SemanticType::DynamicType(DynamicType {
                interface_symbol_id: interface_type.symbol_id,
                vtable_id,
                loc: dynamic_expr.loc.clone(),
            }))
        }
    }
}

// ============================================================
// Helper Functions
// ============================================================
impl<'a> AnalysisContext<'a> {
    /// Validates that generic types have provided type arguments when required.
    ///
    /// Checks whether a semantic type which might have generic params
    /// has the required type arguments specified. Reports an error if type args
    /// are used without type arguments in a context where they're mandatory.
    ///
    /// # Validation
    /// - If the type references a generic object (has generic parameters defined).
    /// - And the type is not already instantiated as a generic type (missing type args).
    /// - Then reports a missing type arguments error.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for symbol resolution and type name formatting.
    /// - `sema_ty`: The semantic type to check for missing type arguments.
    /// - `loc`: Source location for error reporting.
    ///
    pub(crate) fn check_sema_ty_for_missing_type_args(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        sema_ty: &SemanticType,
        loc: SourceLoc,
    ) {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        if let Some(symbol_id) = sema_ty.symbol_id() {
            let sym = self
                .resolver
                .resolve_local_or_global_symbol(scope_opt, symbol_id)
                .unwrap();

            let is_generic_object = sym.symbol_generic_params().is_some();
            let is_generic_type = sema_ty.as_generic_type().is_some();

            if is_generic_object && !is_generic_type {
                let type_name = format_sema_ty(sema_ty.clone(), &(self.symbol_formatter)(scope_id_opt));

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::MissingTypeArgs { type_name }),
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
                return;
            }
        }
    }

    fn analyze_interface_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        method_call: &mut TypedMethodCall,
        interface_type: &InterfaceType,
    ) -> Option<SemanticType> {
        let method_idx = interface_type
            .methods
            .iter()
            .position(|method| method.name == method_call.method_name)?;

        let mut func_sig = interface_type.methods[method_idx].clone();

        // interface uses void* for SelfType
        let self_type = SemanticType::Pointer(Box::new(SemanticType::PlainType(PlainType::Void)));
        set_self_modifier_type_in_func_sig(&mut func_sig, &self_type);
        set_self_modifier_symbol_id_in_func_sig(&mut func_sig, 0);

        method_call.method_call_on_interface = Some(TypedInterfaceMethodCallMetadata {
            method_idx,
            methods_len: interface_type.methods.len(),
            method_sig: func_sig.clone(),
        });

        let return_type = self.check_func_call(
            scope_id_opt,
            &mut func_sig,
            &None,
            &mut method_call.args,
            method_call.loc.clone(),
            true, // always instance method call for interfaces
        )?;

        method_call.func_sig = Some(func_sig.clone());
        Some(return_type)
    }

    /// Analyzes regular method calls on objects (structs, enums, unions).
    ///
    /// Type-checks method calls on object instances, handling instance vs static
    /// method dispatch, generic method instantiation, and self parameter handling.
    /// Performs comprehensive validation including method existence, access control,
    /// and generic type inference from arguments.
    ///
    /// # Process
    /// 1. **Method Resolution**: Locates the method in the object's method table.
    /// 2. **Signature Validation**: Checks instance vs static method compatibility.
    /// 3. **Access Control**: Validates method visibility and access syntax.
    /// 4. **Generic Instantiation**: Initializes generic types for generic methods.
    /// 5. **Self Parameter Handling**: Processes and inserts self argument for instance methods.
    /// 6. **Argument Type Checking**: Validates argument types with generic inference.
    /// 7. **Generic Specialization**: Registers monomorphized function if generic.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and symbol resolution.
    /// - `scope_opt`: Local scope for symbol lookup.
    /// - `method_call`: The method call AST node to analyze.
    /// - `object_id`: Symbol ID of the object type (struct/enum/union).
    /// - `method_call_operand_ty`: Type of the operand the method is called on.
    /// - `is_instance_method_operand`: Whether the operand is an instance (vs type).
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The return type of the method call.
    /// - `None`: If method doesn't exist, validation fails, or generic instantiation fails.
    ///
    fn analyze_regular_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        scope_opt: Option<LocalScopeRef>,
        method_call: &mut TypedMethodCall,
        object_id: SymbolID,
        operand_ty: SemanticType,
        is_instance_method_operand: bool,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        fn infer_generic_method_params(
            this: &mut AnalysisContext,
            generic_type_opt: Option<&GenericType>,
            scope_id_opt: &Option<u32>,
            func_sig_params_list: &Vec<TypedFuncParamKind>,
            method_call_args: &mut Vec<TypedExprStmt>,
        ) {
            // infer generic params from arguments
            for (idx, arg) in method_call_args.iter_mut().enumerate() {
                let target_type = match func_sig_params_list.get(idx).and_then(|param| param.param_type()) {
                    Some(sema_ty) => sema_ty,
                    None => continue,
                };

                this.analyze_expr(*scope_id_opt, arg, None);

                if let Some(sema_ty) = this.infer_generic_param(
                    *scope_id_opt,
                    generic_type_opt,
                    target_type,
                    arg.sema_ty.clone(),
                    arg.loc.clone(),
                ) {
                    arg.sema_ty = Some(sema_ty);
                }
            }
        }

        let object_name: String;
        let object_methods: Option<HashMap<String, SymbolID>>;
        let sym = self.resolver.lookup_symbol_entry_with_id(object_id).unwrap();

        macro_rules! lookup_object_method {
            () => {{
                match object_methods.as_ref().unwrap().get(&method_call.method_name) {
                    Some(method_symbol_id) => self
                        .resolver
                        .lookup_symbol_entry_with_id(*method_symbol_id)
                        .and_then(|sym| Some(sym.as_method().unwrap().clone().func_sig)),
                    None => None,
                }
            }};
        }

        let mut func_sig = {
            match match sym.kind.clone() {
                SymbolEntryKind::Struct(resolved_struct) => {
                    object_name = resolved_struct.struct_sig.name;
                    object_methods = Some(resolved_struct.struct_sig.methods);
                    lookup_object_method!()
                }
                SymbolEntryKind::Enum(resolved_enum) => {
                    object_name = resolved_enum.enum_sig.name;
                    object_methods = Some(resolved_enum.enum_sig.methods);
                    lookup_object_method!()
                }
                SymbolEntryKind::Union(resolved_union) => {
                    object_name = resolved_union.union_sig.name;
                    object_methods = Some(resolved_union.union_sig.methods);
                    lookup_object_method!()
                }
                SymbolEntryKind::Interface(resolved_interface) => {
                    object_name = resolved_interface.interface_sig.name.clone();
                    object_methods = None;

                    let Some(interface_method_idx) = resolved_interface
                        .interface_sig
                        .methods
                        .iter()
                        .position(|method| method.name == method_call.method_name.clone())
                    else {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ObjectMethodNotDefined {
                                object_name: object_name.clone(),
                                method_name: method_call.method_name.clone(),
                            }),
                            location: Some(DiagLoc::new(method_call.loc.clone())),
                            hint: None,
                        });
                        return None;
                    };

                    let func_decl = &resolved_interface.interface_sig.methods[interface_method_idx];
                    let mut func_sig = typed_func_decl_as_func_sig(func_decl);

                    // NOTE: Interface is not sensitive to SelfType, that's why we can fake it with a `void*` type.
                    set_self_modifier_type_in_func_sig(
                        &mut func_sig,
                        &SemanticType::Pointer(Box::new(SemanticType::PlainType(PlainType::Void))),
                    );
                    set_self_modifier_symbol_id_in_func_sig(&mut func_sig, 0);

                    method_call.method_call_on_interface = Some(TypedInterfaceMethodCallMetadata {
                        method_idx: interface_method_idx,
                        methods_len: resolved_interface.interface_sig.methods.len(),
                        method_sig: func_sig.clone(),
                    });

                    Some(func_sig)
                }
                _ => return None,
            } {
                Some(func_sig) => func_sig,
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ObjectMethodNotDefined {
                            object_name: object_name.clone(),
                            method_name: method_call.method_name.clone(),
                        }),
                        location: Some(DiagLoc::new(method_call.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        };

        // object name used later to make mangled abi name
        method_call.object_name = Some(object_name.clone());

        if self.check_unexpected_type_args(
            &func_sig.generic_params,
            &method_call.type_args,
            method_call.loc.clone(),
        ) {
            return None;
        }

        let is_instance_method_sig = func_sig.is_instance_method();
        let first_param_opt = func_sig.params.list.first();

        // invalid if static method called on instance
        let invalid_call = !is_instance_method_sig && is_instance_method_operand;

        if invalid_call {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::StaticMethodCallOnInstance {
                    method_name: method_call.method_name.clone(),
                }),
                location: Some(DiagLoc::new(method_call.loc.clone())),
                hint: Some(format!(
                    "Call it on a value of type '{}', or declare it as a static function if no instance is required.",
                    format_sema_ty(operand_ty, &(self.symbol_formatter)(scope_id_opt))
                )),
            });
            return None;
        }

        if !self.validate_method_call(
            scope_id_opt,
            object_id,
            &method_call.method_name,
            operand_ty.clone(),
            method_call.is_fat_arrow,
            method_call.method_call_on_interface.is_some(),
            first_param_opt,
            object_methods,
            object_name.clone(),
            &func_sig,
            method_call.loc.clone(),
        ) {
            return None;
        }

        self.ty_ctx.current_method_symbol_id = Some(func_sig.symbol_id.unwrap());

        let instance_method_call =
            (is_instance_method_sig && is_instance_method_operand) || method_call.method_call_on_interface.is_some();

        let mut operand_generic_type_opt = { operand_ty.pointer_inner().as_generic_type().cloned() };

        let mut method_generic_type_opt = {
            match self.init_generic_type_with_symbol_id(
                scope_id_opt,
                scope_opt.clone(),
                func_sig.symbol_id.unwrap(),
                &mut method_call.type_args,
                None,
                func_sig.generic_params.as_ref(),
                false,
                method_call.loc.clone(),
            ) {
                Ok(opt) => opt?.1,
                Err(diag) => {
                    self.reporter.report(diag);
                    return None;
                }
            }
        };

        // 1. Generic Method, Generic Operand
        if let (Some(generic_method), Some(generic_operand)) = (&mut method_generic_type_opt, &operand_generic_type_opt)
        {
            let mut merged_generic_type = self.merge_generic_type(generic_method, generic_operand);

            if let Err(diag) =
                merged_generic_type.init(self.mapping_ctx_arena.clone(), &(self.symbol_formatter)(scope_id_opt))
            {
                self.reporter.report(diag);
                return None;
            }

            infer_generic_method_params(
                self,
                Some(&merged_generic_type),
                &scope_id_opt,
                &func_sig.params.list,
                &mut method_call.args,
            );

            method_generic_type_opt = Some(merged_generic_type);
        }
        // 2. Generic Method, Concrete Operand
        else if let (Some(generic_method), None) = (&mut method_generic_type_opt, &operand_generic_type_opt) {
            if let Err(diag) =
                generic_method.init(self.mapping_ctx_arena.clone(), &(self.symbol_formatter)(scope_id_opt))
            {
                self.reporter.report(diag);
                return None;
            }

            infer_generic_method_params(
                self,
                Some(generic_method),
                &scope_id_opt,
                &func_sig.params.list,
                &mut method_call.args,
            );

            method_generic_type_opt = match generic_method.finalize(
                self.mapping_ctx_arena.clone(),
                func_sig.generic_params.clone().unwrap(),
                &(self.symbol_formatter)(scope_id_opt),
            ) {
                Ok(generic_type) => Some(generic_type.clone()),
                Err(diag) => {
                    self.reporter.report(diag);
                    return None;
                }
            };
        }
        // 3. Concrete Method, Generic Operand
        else if let (None, Some(generic_operand)) = (&method_generic_type_opt, &mut operand_generic_type_opt) {
            if let Err(diag) =
                generic_operand.init(self.mapping_ctx_arena.clone(), &(self.symbol_formatter)(scope_id_opt))
            {
                self.reporter.report(diag);
                return None;
            }

            infer_generic_method_params(
                self,
                Some(generic_operand),
                &scope_id_opt,
                &func_sig.params.list,
                &mut method_call.args,
            );
        }
        // 4. Concrete Method, Concrete Operand (Non-Generic)
        else {
            // process non-generic method call
        }

        if !is_instance_method_operand && is_instance_method_sig {
            // explicit instance method
            // inferring self type from first argument type
            if let Some(mut expr) = method_call.args.first().cloned() {
                if let Some(sema_ty) = self.analyze_expr(scope_id_opt, &mut expr, None) {
                    operand_generic_type_opt = sema_ty.as_generic_type().cloned();

                    self.set_ty_ctx_self_type(method_call, &sema_ty);
                    set_self_modifier_type_in_func_sig(&mut func_sig, &sema_ty);
                }
            }
        }

        // merge method_mapping_ctx + operand_mapping_ctx
        let merged_generic_type_opt = {
            let method_mapping_ctx_sema_ty =
                method_generic_type_opt.map(|generic_type| SemanticType::GenericType(generic_type));

            self.merge_generic_operand_with_expected_type(operand_ty.clone(), method_mapping_ctx_sema_ty.clone())
        };

        if let Some(generic_type) = &merged_generic_type_opt {
            // infer remaining generic params from expected type
            self.unify_generic_types_from_expected_type(scope_id_opt, &generic_type, expected_type);
        }

        // instance self type
        self.set_ty_ctx_self_type(method_call, &operand_ty);

        func_sig.return_type = self.check_func_call(
            scope_id_opt,
            &mut func_sig,
            &operand_generic_type_opt,
            &mut method_call.args,
            method_call.loc.clone(),
            instance_method_call,
        )?;

        if let Some(generic_type) = &merged_generic_type_opt {
            func_sig.return_type = substitute_type(
                self.mapping_ctx_arena.clone(),
                func_sig.return_type.clone(),
                generic_type.mapping_ctx.clone(),
            )
            .unwrap();
        }

        // validate generic type instantiation
        if let Some(generic_type) = &merged_generic_type_opt {
            func_sig = substitute_func_sig(
                self.mapping_ctx_arena.clone(),
                &func_sig,
                generic_type.mapping_ctx.clone(),
            )
            .unwrap();

            if let Some(generic_params) = func_sig.generic_params.clone() {
                if let Err(diag) = generic_type.finalize(
                    self.mapping_ctx_arena.clone(),
                    generic_params,
                    (self.symbol_formatter)(scope_id_opt),
                ) {
                    self.reporter.report(diag);
                    return None;
                }
            }

            let func_call_loc = func_sig.loc.clone();
            method_call.monomorph_key = self.register_specialized_generic_func(
                &mut func_sig,
                &generic_type,
                Some(SemanticType::GenericType(merged_generic_type_opt.clone().unwrap())),
                &func_call_loc,
            );

            // substitutes the func type inside of the func_call operand
            method_call.operand.sema_ty = substitute_type(
                self.mapping_ctx_arena.clone(),
                method_call.operand.sema_ty.clone().unwrap(),
                generic_type.mapping_ctx.clone(),
            );
        }

        if instance_method_call {
            let self_modifier_type = self_modifier_param_type(&func_sig.params, operand_ty).unwrap();

            set_self_modifier_type_in_func_sig(&mut func_sig, &self_modifier_type);

            let self_modifier = func_sig.params.list.first().unwrap().as_self_modifier().unwrap();

            if method_call.method_call_on_interface.is_none() {
                method_call.args.insert(
                    0,
                    self.analyze_object_self_modifier_argument(
                        &method_call.operand,
                        method_call.is_fat_arrow,
                        self_modifier,
                    ),
                );
            }
        }

        method_call.func_sig = Some(func_sig.clone());
        Some(func_sig.return_type.clone())
    }

    /// Extracts the pure symbol ID from a type, normalizing it first.
    ///
    /// Normalizes the given semantic type (resolving aliases, applying generics)
    /// then extracts the underlying pure symbol ID if the type represents a
    /// named struct, union, or enum.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type normalization.
    /// - `var_type`: The semantic type to extract the symbol ID from.
    /// - `loc`: Source location for error reporting during normalization.
    ///
    /// # Returns
    /// - `Some(u32)`: The pure symbol ID if the type resolves to a named type.
    /// - `None`: If normalization fails or the type doesn't have a pure symbol ID.
    ///
    fn extract_object_symbol_id<'b>(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        var_type: SemanticType,
        loc: SourceLoc,
    ) -> Option<u32> {
        self.normalize_sema_type(scope_id_opt, var_type, loc.clone())?
            .maybe_generic_base_symbol_id()
    }

    /// Validates function calls against their signature, checking argument counts and types.
    ///
    /// Performs comprehensive validation of function calls including argument count checking,
    /// type compatibility validation, and variadic argument handling. Supports both regular
    /// functions and instance methods (with self parameter adjustment).
    ///
    /// # Validation Steps
    /// 1. **Argument Count**: Validates against parameter count (with variadic support).
    /// 2. **Variadic Arguments**: Type-checks variadic arguments if present.
    /// 3. **Static Arguments**: Type-checks each argument against its corresponding parameter.
    /// 4. **Generic Inference**: Infers generic parameters from argument types when possible.
    /// 5. **Duplicate Parameter Names**: Checks for duplicate parameter names in signature.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis.
    /// - `func_sig`: The function signature being called (modified with inferred generics).
    /// - `generic_type_opt`: Optional generic type context for generic functions.
    /// - `args`: The function call arguments to validate.
    /// - `loc`: Source location for error reporting.
    /// - `instance_method_call`: Whether this is an instance method call (adjusts for self).
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The normalized return type of the function.
    /// - `None`: If validation fails (errors reported via diagnostics).
    ///
    fn check_func_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_sig: &mut FuncSig,
        generic_type_opt: &Option<GenericType>,
        args: &mut Vec<TypedExprStmt>,
        loc: SourceLoc,
        instance_method_call: bool,
    ) -> Option<SemanticType> {
        let is_variadic = func_sig.params.variadic.is_some();
        let mut expected_args_len = func_sig.params.list.len();

        // if this is an instance method call, self modifier will be pushed later
        if instance_method_call && !func_sig.params.list.is_empty() {
            expected_args_len = expected_args_len.saturating_sub(1);
        }

        // check argument count
        if (!is_variadic && args.len() != expected_args_len) || (is_variadic && args.len() < expected_args_len) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: expected_args_len as u32,
                    func_name: func_sig.name.clone(),
                }),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
            return None;
        }

        // handle variadic arguments
        if is_variadic {
            self.check_func_variadic_arguments(scope_id_opt, &func_sig, args, loc.clone());
        }

        // analyze static arguments
        let start_idx = if instance_method_call { 1 } else { 0 };

        for (param_idx, (param, arg)) in func_sig
            .params
            .list
            .iter_mut()
            .skip(start_idx)
            .zip(args.iter_mut())
            .enumerate()
        {
            let mut param_type = match self.func_param_type(scope_id_opt, param) {
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            let arg_type = match self.analyze_expr(scope_id_opt, arg, Some(param_type.clone())) {
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            param_type = match self.normalize_and_check_sema_ty(scope_id_opt, param_type, param.loc()) {
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            if let Some(sema_ty) = self.infer_generic_param(
                scope_id_opt,
                generic_type_opt.as_ref(),
                param_type.clone(),
                Some(arg_type.clone()),
                arg.loc.clone(),
            ) {
                param_type = sema_ty;
            }

            // skip if param type not inferred yet
            if param_type.as_generic_param().is_none()
                && !self.check_type_mismatch(scope_id_opt, arg_type.clone(), param_type.clone(), arg.loc.clone())
            {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type: format_sema_ty(param_type.clone(), &(self.symbol_formatter)(scope_id_opt)),
                        argument_type: format_sema_ty(arg_type, &(self.symbol_formatter)(scope_id_opt)),
                        argument_idx: param_idx as u32,
                    }),
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
            }
        }

        self.normalize_sema_type(scope_id_opt, func_sig.return_type.clone(), loc)
    }

    /// Validates variadic arguments in function calls, ensuring type compatibility and proper analysis.
    ///
    /// Performs type checking and semantic analysis for variadic arguments in function calls,
    /// handling both typed variadic parameters (with explicit type requirements) and untyped
    /// C-style variadic parameters. This function processes arguments beyond the static
    /// parameter list according to the function's variadic specification.
    ///
    /// # Variadic Handling Modes
    /// 1. Typed Variadic: Each variadic argument must match the specified variadic parameter type.
    /// 2. Untyped C-style: Arguments are analyzed without type constraints (compatibility checks deferred).
    ///
    /// # Processing Steps
    /// 1. Argument Extraction: Identifies the variadic portion of arguments (after static parameters).
    /// 2. Mode Dispatch: Routes to appropriate handler based on variadic parameter type.
    /// 3. Type Validation: For typed variadic, checks each argument against required type.
    /// 4. Semantic Analysis: Ensures all arguments are properly analyzed in current scope.
    ///
    /// # Parameters
    /// - scope_id_opt: Scope for type analysis and symbol resolution.
    /// - func_sig: The function signature containing variadic parameter specification.
    /// - args: The complete argument list (modified with updated semantic information).
    /// - loc: Source location for error reporting and diagnostics.
    ///
    /// # Behavior
    /// - Typed Variadic: Reports type mismatch errors for non-conforming arguments.
    /// - Untyped C-style: Performs semantic analysis without type enforcement.
    /// - No Variadic: No-op if function has no variadic parameters.
    ///
    fn check_func_variadic_arguments(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_sig: &FuncSig,
        args: &mut Vec<TypedExprStmt>,
        loc: SourceLoc,
    ) {
        let static_params_len = func_sig.params.list.len();
        let variadic_args = &mut args[static_params_len..];

        if let Some(var_param) = &func_sig.params.variadic {
            match var_param.clone() {
                TypedFuncVariadicParams::Typed(_, variadic_param_type) => {
                    for (idx, arg) in variadic_args.iter_mut().enumerate() {
                        if let Some(arg_type) = self.analyze_expr(scope_id_opt, arg, arg.sema_ty.clone()) {
                            if !self.check_type_mismatch(
                                scope_id_opt,
                                arg_type.clone(),
                                variadic_param_type.clone(),
                                arg.loc.clone(),
                            ) {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                        param_type: format_sema_ty(
                                            variadic_param_type.clone(),
                                            &(self.symbol_formatter)(scope_id_opt),
                                        ),
                                        argument_type: format_sema_ty(arg_type, &(self.symbol_formatter)(scope_id_opt)),
                                        argument_idx: (idx + static_params_len) as u32,
                                    }),
                                    location: Some(DiagLoc::new(loc.clone())),
                                    hint: None,
                                });
                            }
                        }
                    }
                }
                TypedFuncVariadicParams::UntypedCStyle => {
                    for arg in variadic_args.iter_mut() {
                        self.analyze_expr(scope_id_opt, arg, arg.sema_ty.clone());
                    }
                }
            }
        }
    }

    /// Validates calls to function type values (function pointers, lambdas).
    ///
    /// Type-checks calls to first-class function values (function types) rather than
    /// named functions. Similar to `check_func_call` but works with function type
    /// definitions rather than function signatures.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis.
    /// - `func_type`: The function type being called.
    /// - `args`: The function call arguments to validate.
    /// - `loc`: Source location for error reporting.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The return type of the function type.
    /// - `None`: If validation fails (errors reported via diagnostics).
    ///
    fn check_func_type_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_type: &mut TypedFuncType,
        args: &mut Vec<TypedExprStmt>,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let is_variadic = func_type.params.variadic.is_some();
        let expected_args_len = func_type.params.list.len();
        let func_name = format_func_ty(func_type, &(self.symbol_formatter)(scope_id_opt));

        // check argument count
        if (!is_variadic && args.len() != expected_args_len) || (is_variadic && args.len() < expected_args_len) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: expected_args_len as u32,
                    func_name,
                }),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
            return None;
        }

        // handle variadic arguments
        if is_variadic {
            let static_params_len = func_type.params.list.len();
            let variadic_args = &mut args[static_params_len..];

            if let Some(var_param) = &func_type.params.variadic {
                match *var_param.clone() {
                    TypedFuncTypeVariadicParams::Typed(variadic_param_type) => {
                        for (idx, arg) in variadic_args.iter_mut().enumerate() {
                            if let Some(arg_type) = self.analyze_expr(scope_id_opt, arg, arg.sema_ty.clone()) {
                                if !self.check_type_mismatch(
                                    scope_id_opt,
                                    arg_type.clone(),
                                    variadic_param_type.clone(),
                                    arg.loc.clone(),
                                ) {
                                    self.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: Box::new(AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                            param_type: format_sema_ty(
                                                variadic_param_type.clone(),
                                                &(self.symbol_formatter)(scope_id_opt),
                                            ),
                                            argument_type: format_sema_ty(
                                                arg_type,
                                                &(self.symbol_formatter)(scope_id_opt),
                                            ),
                                            argument_idx: (idx + static_params_len) as u32,
                                        }),
                                        location: Some(DiagLoc::new(loc.clone())),
                                        hint: None,
                                    });
                                }
                            }
                        }
                    }
                    TypedFuncTypeVariadicParams::UntypedCStyle => {
                        for arg in variadic_args.iter_mut() {
                            self.analyze_expr(scope_id_opt, arg, arg.sema_ty.clone());
                        }
                    }
                }
            }
        }

        // analyze static arguments
        let start_idx = 0;
        for (param_idx, (param, arg)) in func_type
            .params
            .list
            .iter_mut()
            .skip(start_idx)
            .zip(args.iter_mut())
            .enumerate()
        {
            let param_type = self
                .normalize_sema_type(scope_id_opt, param.clone(), loc.clone())
                .unwrap();

            let arg_type = match self.analyze_expr(scope_id_opt, arg, Some(param_type.clone())) {
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            if !self.check_type_mismatch(scope_id_opt, arg_type.clone(), param_type.clone(), arg.loc.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type: format_sema_ty(param_type.clone(), &(self.symbol_formatter)(scope_id_opt)),
                        argument_type: format_sema_ty(arg_type, &(self.symbol_formatter)(scope_id_opt)),
                        argument_idx: param_idx as u32,
                    }),
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
            }
        }

        Some(*func_type.return_type.clone())
    }

    /// Extracts and normalizes the semantic type from a function parameter.
    ///
    /// Processes function parameters (regular parameters and self modifiers) by
    /// extracting their declared type and applying type normalization rules.
    /// Ensures parameter types are fully resolved before use in type checking.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type normalization.
    /// - `param`: The function parameter to process (regular or self modifier).
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The normalized type of the parameter.
    /// - `None`: If type normalization fails.
    ///
    fn func_param_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        param: &mut TypedFuncParamKind,
    ) -> Option<SemanticType> {
        match param {
            TypedFuncParamKind::FuncParam(param) => {
                let normalized = self.normalize_sema_type(scope_id_opt, param.ty.clone(), param.loc.clone())?;
                param.ty = normalized.clone();
                Some(normalized)
            }
            TypedFuncParamKind::SelfModifier(self_modifier) => {
                let normalized = self.normalize_sema_type(
                    scope_id_opt,
                    self_modifier.ty.clone().unwrap(),
                    self_modifier.loc.clone(),
                )?;
                Some(normalized)
            }
        }
    }

    /// Analyzes union initialization expressions with exactly one field.
    ///
    /// Type-checks union initialization syntax which requires exactly one field
    /// to be initialized. Handles generic union types and validates field existence
    /// and type compatibility.
    ///
    /// # Validation
    /// - Exactly one field must be provided (unions hold one value at a time).
    /// - The field must exist in the union definition.
    /// - Field value type must match the union field's type (with generic inference).
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis.
    /// - `struct_init`: The struct initialization AST node (reused for unions).
    /// - `resolved_union`: The resolved union type definition.
    /// - `generic_type_opt`: Optional generic type context.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The type of the initialized union (possibly generic).
    /// - `None`: If validation fails or generic instantiation fails.
    ///
    fn analyze_regular_union_init(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        struct_init: &mut TypedStructInitExpr,
        resolved_union: &ResolvedUnion,
        generic_type_opt: &Option<GenericType>,
    ) -> Option<SemanticType> {
        if struct_init.fields.len() != 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnionInitWithInvalidFields),
                location: Some(DiagLoc::new(struct_init.loc.clone())),
                hint: None,
            });
            return None;
        }

        let field_init = &mut struct_init.fields[0];

        let field = match resolved_union
            .union_sig
            .fields
            .iter()
            .find(|f| f.name == field_init.name)
        {
            Some(field) => field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: resolved_union.union_sig.name.clone(),
                        field_name: field_init.name.clone(),
                    }),
                    location: Some(DiagLoc::new(field_init.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let field_expected_type = self
            .try_infer_generic_param_as_expected_type(field.ty.clone(), &generic_type_opt)
            .unwrap_or(field.ty.clone());

        field_init.value.sema_ty = self.analyze_expr(scope_id_opt, &mut field_init.value, Some(field_expected_type));

        if let Some(sema_ty) = self.infer_generic_param(
            scope_id_opt,
            generic_type_opt.as_ref(),
            field.ty.clone(),
            field_init.value.sema_ty.clone(),
            field_init.value.loc.clone(),
        ) {
            field_init.value.sema_ty = Some(sema_ty);
        }

        if let Some(generic_type) = generic_type_opt {
            // validate generic type instantiation
            let final_generic_type = match generic_type.finalize(
                self.mapping_ctx_arena.clone(),
                resolved_union.union_sig.generic_params.clone().unwrap(),
                (self.symbol_formatter)(scope_id_opt),
            ) {
                Ok(generic_type) => generic_type,
                Err(diag) => {
                    self.reporter.report(diag);
                    return None;
                }
            };

            Some(SemanticType::GenericType(final_generic_type.clone()))
        } else {
            if struct_init.is_const {
                Some(SemanticType::Const(Box::new(SemanticType::ResolvedSymbol(
                    ResolvedSymbol::Union(struct_init.symbol_id),
                ))))
            } else {
                Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Union(
                    struct_init.symbol_id,
                )))
            }
        }
    }

    /// Analyzes struct initialization expressions with field assignments.
    ///
    /// Type-checks struct initialization syntax with named field assignments.
    /// Validates field existence, duplicate assignments, and missing required fields.
    /// Handles generic struct types with type inference from initialization values.
    ///
    /// # Validation
    /// - No duplicate field assignments.
    /// - All assigned fields exist in the struct definition.
    /// - All non-optional fields are provided (no missing fields).
    /// - Field value types match struct field types (with generic inference).
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis.
    /// - `struct_init`: The struct initialization AST node.
    /// - `resolved_struct`: The resolved struct type definition.
    /// - `generic_type_opt`: Optional generic type context.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The type of the initialized struct (possibly generic).
    /// - `None`: If validation fails, fields missing, or generic instantiation fails.
    ///
    fn analyze_regular_struct_init(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        struct_init: &mut TypedStructInitExpr,
        resolved_struct: &ResolvedStruct,
        generic_type_opt: &Option<GenericType>,
        infer_ctx: &InferenceCtx,
    ) -> Option<SemanticType> {
        // check duplicate field inits
        let mut field_names: Vec<String> = Vec::new();
        for field_init in &struct_init.fields {
            let struct_name = (self.symbol_formatter)(scope_id_opt)(struct_init.symbol_id);

            if field_names.contains(&field_init.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldName {
                        object_name: struct_name,
                        field_name: field_init.name.clone(),
                    }),
                    location: Some(DiagLoc::new(field_init.loc.clone())),
                    hint: None,
                });
                continue;
            }

            field_names.push(field_init.name.clone());
        }

        let mut missing_fields: Vec<String> = resolved_struct
            .struct_sig
            .fields
            .iter()
            .map(|field| field.name.clone())
            .collect();

        for field_init in struct_init.fields.iter_mut() {
            let field = match resolved_struct
                .struct_sig
                .fields
                .iter()
                .cloned()
                .find(|field| field.name == field_init.name)
            {
                Some(field) => field,
                None => {
                    let struct_name = (self.symbol_formatter)(scope_id_opt)(struct_init.symbol_id);

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                            struct_name,
                            field_name: field_init.name.clone(),
                        }),
                        location: Some(DiagLoc::new(field_init.loc.clone())),
                        hint: None,
                    });
                    continue;
                }
            };

            let infer_ctx_expected_type = infer_ctx.get(&field.name).cloned();
            let field_expected_type = self
                .try_infer_generic_param_as_expected_type(field.ty.clone(), &generic_type_opt)
                .or(infer_ctx_expected_type)
                .unwrap_or(field.ty.clone());

            let field_value_ty = self.analyze_expr(scope_id_opt, &mut field_init.value, Some(field_expected_type));

            if let Some(sema_ty) = self.infer_generic_param(
                scope_id_opt,
                generic_type_opt.as_ref(),
                field.ty.clone(),
                field_value_ty.clone(),
                field_init.loc.clone(),
            ) {
                field_init.value.sema_ty = Some(sema_ty);
            }

            let missing_fields_idx = match missing_fields
                .iter()
                .position(|field_name| *field_name == field_init.name.clone())
            {
                Some(idx) => idx,
                None => continue,
            };

            missing_fields.remove(missing_fields_idx);
        }

        if !missing_fields.is_empty() {
            let struct_name = (self.symbol_formatter)(scope_id_opt)(struct_init.symbol_id);
            let missing_field_names = format_missing_fields(&missing_fields);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::StructMissingFields {
                    struct_name,
                    missing_field_names,
                }),
                location: Some(DiagLoc::new(struct_init.loc.clone())),
                hint: None,
            });
        }

        if let Some(generic_type) = generic_type_opt {
            let final_generic_type = {
                // validate generic type instantiation
                match generic_type.finalize(
                    self.mapping_ctx_arena.clone(),
                    resolved_struct.struct_sig.generic_params.clone().unwrap(),
                    (self.symbol_formatter)(scope_id_opt),
                ) {
                    Ok(generic_type) => generic_type,
                    Err(diag) => {
                        self.reporter.report(diag);
                        return None;
                    }
                }
            };

            Some(SemanticType::GenericType(final_generic_type.clone()))
        } else {
            if struct_init.is_const {
                Some(SemanticType::Const(Box::new(SemanticType::ResolvedSymbol(
                    ResolvedSymbol::NamedStruct(struct_init.symbol_id),
                ))))
            } else {
                Some(SemanticType::ResolvedSymbol(ResolvedSymbol::NamedStruct(
                    struct_init.symbol_id,
                )))
            }
        }
    }

    /// Attempts to interpret a field access as an enum unit variant constructor.
    ///
    /// Detects when a field access on an enum type is actually accessing a unit variant
    /// (e.g., `Color::Red`). If successful, performs the appropriate enum variant analysis.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis.
    /// - `scope_opt`: Local scope for symbol resolution.
    /// - `operand_ty`: Type of the operand (should be an enum type).
    /// - `field_access`: The field access AST node to reinterpret.
    /// - `expected_type`: Optional type context for generic inference.
    ///
    /// # Returns
    /// Tuple of:
    /// - `bool`: Whether an enum variant interpretation was attempted.
    /// - `Option<SemanticType>`: The resulting enum type if interpretation succeeded.
    ///
    fn maybe_enum_variant_constructor_from_field_access(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        scope_opt: Option<LocalScopeRef>,
        operand_ty: SemanticType,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<SemanticType>,
    ) -> (bool, Option<SemanticType>) {
        let Some(symbol_id) = operand_ty.maybe_generic_base_symbol_id() else {
            return (false, None);
        };

        let Some(resolved_enum) = self.resolver.resolve_enum_symbol(scope_opt.clone(), symbol_id) else {
            return (false, None);
        };

        let attempted = true;

        field_access.object_symbol_id = Some(resolved_enum.symbol_id);

        {
            let (generic_params, mapping_ctx) = self.extract_and_merge_generic_context(
                &operand_ty,
                resolved_enum.enum_sig.generic_params.as_ref(),
                expected_type,
            );

            let sema = self.analyze_enum_variant_no_field(
                scope_id_opt,
                scope_opt,
                &resolved_enum,
                field_access,
                generic_params.as_ref(),
                mapping_ctx,
            );
            (attempted, sema)
        }
    }

    /// Attempts to interpret a method call as an enum variant constructor with fields.
    ///
    /// Detects when a method call on an enum type is actually constructing a fielded
    /// enum variant (e.g., `Color::Rgb(255, 0, 0)`). If successful, performs the
    /// appropriate enum variant analysis with field validation.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis.
    /// - `scope_opt`: Local scope for symbol resolution.
    /// - `operand_ty`: Type of the operand (should be an enum type).
    /// - `method_call`: The method call AST node to reinterpret.
    /// - `expected_type`: Optional type context for generic inference.
    ///
    /// # Returns
    /// Tuple of:
    /// - `bool`: Whether an enum variant interpretation was attempted.
    /// - `Option<SemanticType>`: The resulting enum type if interpretation succeeded.
    ///
    fn maybe_enum_variant_constructor_from_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        scope_opt: Option<LocalScopeRef>,
        operand_ty: SemanticType,
        method_call: &mut TypedMethodCall,
        expected_type: Option<SemanticType>,
    ) -> (bool, Option<SemanticType>) {
        let Some(symbol_id) = operand_ty.maybe_generic_base_symbol_id() else {
            return (false, None);
        };

        let Some(resolved_enum) = self.resolver.resolve_enum_symbol(scope_opt.clone(), symbol_id) else {
            return (false, None);
        };

        method_call.enum_const = Some(resolved_enum.symbol_id);

        let enum_variant_opt = resolved_enum
            .enum_sig
            .variants
            .iter()
            .find(|v| v.ident().as_string() == method_call.method_name);

        let Some(enum_variant) = enum_variant_opt else {
            return (false, None);
        };

        // method_call.object_symbol_id = Some(resolved_enum.symbol_id);

        let (generic_params, mapping_ctx) = self.extract_and_merge_generic_context(
            &operand_ty,
            resolved_enum.enum_sig.generic_params.as_ref(),
            expected_type,
        );

        let sema_ty_opt = self.analyze_enum_variant(
            scope_id_opt,
            scope_opt,
            enum_variant.clone(),
            method_call,
            &resolved_enum,
            generic_params.as_ref(),
            mapping_ctx,
        );

        (true, sema_ty_opt)
    }

    /// Analyzes field access expressions on union types.
    ///
    /// Type-checks field accesses on union instances by verifying the field exists
    /// in the union definition and the access uses correct pointer syntax.
    /// Annotates the field access AST node with resolved type and index information.
    ///
    /// # Process
    /// 1. **Operand Analysis**: Type-checks the union expression operand.
    /// 2. **Field Resolution**: Locates the field in the union signature by name.
    /// 3. **Field Type Normalization**: Normalizes the field's declared type.
    /// 4. **Access Validation**: Validates pointer access operator usage.
    /// 5. **AST Annotation**: Attaches resolved field metadata to the AST node.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and normalization.
    /// - `union_sig`: The union type signature containing field definitions.
    /// - `field_access`: The field access expression to analyze.
    /// - `expected_type`: Optional type context for operand analysis.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The normalized type of the accessed union field.
    /// - `None`: If field doesn't exist, type normalization fails, or access is invalid.
    ///
    fn analyze_union_field_access(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        union_sig: &UnionSig,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let operand_type = match self.analyze_expr(scope_id_opt, &mut field_access.operand, expected_type) {
            Some(sema_ty) => sema_ty,
            None => return None,
        };

        field_access.operand.sema_ty = Some(operand_type.clone());

        let union_field_idx = match union_sig
            .fields
            .iter()
            .position(|field| *field.name == field_access.field_name.clone())
        {
            Some(union_field) => union_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: union_sig.name.clone(),
                        field_name: field_access.field_name.clone(),
                    }),
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let field = &union_sig.fields[union_field_idx];
        let field_ty = match self.normalize_sema_type(scope_id_opt, field.ty.clone(), field_access.loc.clone()) {
            Some(sema_ty) => sema_ty,
            None => return None,
        };

        if !self.validate_union_field_access(operand_type.const_inner().clone(), &field_access) {
            return None;
        }

        field_access.field_index = Some(union_field_idx);
        field_access.field_ty = Some(field_ty.clone());
        field_access.object_symbol_id = Some(union_sig.symbol_id);

        Some(field_ty)
    }

    /// Analyzes field access expressions on unnamed (anonymous) struct types.
    ///
    /// Type-checks field accesses on instances of anonymous structs by locating the
    /// field in the struct's type definition and resolving its type. Unlike named
    /// structs, anonymous structs require special handling for error messages and
    /// type resolution.
    ///
    /// # Process
    /// 1. **Operand Analysis**: Type-checks the anonymous struct expression operand.
    /// 2. **Field Resolution**: Locates the field by name in the anonymous struct's field list.
    /// 3. **Field Type Normalization**: Normalizes the resolved field type.
    /// 4. **AST Annotation**: Attaches field index and type to the AST node.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and normalization.
    /// - `unnamed_struct_type`: The anonymous struct type definition.
    /// - `field_access`: The field access expression to analyze.
    /// - `expected_type`: Optional type context for operand analysis.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The normalized type of the accessed field.
    /// - `None`: If field doesn't exist or type normalization fails.
    ///
    fn analyze_unnamed_struct_field_access(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_struct_type: &TypedUnnamedStructType,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let operand_type = match self.analyze_expr(scope_id_opt, &mut field_access.operand, expected_type) {
            Some(sema_ty) => sema_ty,
            None => return None,
        };

        field_access.operand.sema_ty = Some(operand_type.clone());

        let field_idx = match unnamed_struct_type
            .fields
            .iter()
            .position(|field| *field.name == field_access.field_name.clone())
        {
            Some(union_field) => union_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: format_sema_ty(
                            SemanticType::UnnamedStruct(unnamed_struct_type.clone()),
                            &(self.symbol_formatter)(scope_id_opt),
                        ),
                        field_name: field_access.field_name.clone(),
                    }),
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let field = &unnamed_struct_type.fields[field_idx];
        let field_ty = match self.normalize_sema_type(scope_id_opt, *field.ty.clone(), field_access.loc.clone()) {
            Some(sema_ty) => sema_ty,
            None => return None,
        };

        field_access.field_index = Some(field_idx);
        field_access.field_ty = Some(field_ty.clone());

        Some(field_ty.clone())
    }

    /// Analyzes field access expressions on named struct types.
    ///
    /// Type-checks field accesses on struct instances by resolving the field name,
    /// normalizing its type, and validating access permissions and syntax.
    /// Handles both visibility checks and pointer access operator validation.
    ///
    /// # Process
    /// 1. **Field Resolution**: Locates the field by name in the struct's field list.
    /// 2. **Type Normalization**: Normalizes the field's declared type.
    /// 3. **Access Validation**: Checks field visibility and correct operator usage.
    /// 4. **AST Annotation**: Attaches resolved field metadata to the AST node.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type normalization.
    /// - `field_access`: The field access expression to analyze.
    /// - `struct_name`: Name of the struct type for error messages.
    /// - `struct_fields`: List of fields defined on the struct.
    /// - `struct_methods`: Methods of the struct for visibility context.
    /// - `struct_symbol_id`: Symbol ID of the struct type.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The normalized type of the accessed struct field.
    /// - `None`: If field doesn't exist, access is invalid, or normalization fails.
    ///
    fn analyze_struct_field_access(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        field_access: &mut TypedFieldAccess,
        struct_name: String,
        struct_fields: Vec<TypedStructField>,
        struct_methods: HashMap<String, SymbolID>,
        struct_symbol_id: SymbolID,
    ) -> Option<SemanticType> {
        let field_index = match struct_fields
            .iter()
            .position(|typed_struct_field| typed_struct_field.name == field_access.field_name)
        {
            Some(typed_struct_field) => typed_struct_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name,
                        field_name: field_access.field_name.clone(),
                    }),
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let mut typed_struct_field = struct_fields.get(field_index).unwrap().clone();

        typed_struct_field.ty = self
            .normalize_sema_type(scope_id_opt, typed_struct_field.ty.clone(), field_access.loc.clone())
            .unwrap();

        if !self.validate_struct_field_access(
            &field_access.operand,
            &field_access,
            typed_struct_field.vis.clone(),
            &struct_methods,
            &struct_name,
        ) {
            return None;
        }

        field_access.field_index = Some(field_index);
        field_access.field_ty = Some(typed_struct_field.ty.clone());
        field_access.object_symbol_id = Some(struct_symbol_id);

        Some(typed_struct_field.ty.clone())
    }

    /// Analyzes enum variant construction via method-call syntax.
    ///
    /// Type-checks enum variant instantiation that uses method-call syntax (e.g., `Color.Red(255)`).
    /// Handles generic enum instantiation, field type checking, and generic parameter inference.
    ///
    /// # Process
    /// 1. **Variant Validation**: Confirms the variant accepts fields and argument count matches.
    /// 2. **Generic Instantiation**: Initializes generic type if enum is generic.
    /// 3. **Field Type Checking**: Type-checks each argument against the variant's field types.
    /// 4. **Generic Inference**: Infers generic parameters from argument types when needed.
    /// 5. **Type Mismatch Validation**: Ensures argument types match field types (for non-generic enums).
    /// 6. **Generic Finalization**: Validates and finalizes generic type instantiation.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and generic resolution.
    /// - `scope_opt`: Local scope for symbol lookup.
    /// - `enum_variant`: The enum variant being constructed.
    /// - `method_call`: The method call AST node representing variant construction.
    /// - `resolved_enum`: The resolved enum type definition.
    /// - `generic_params`: Generic parameters from context (if any).
    /// - `mapping_ctx`: Generic mapping context for type substitution.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The type of the constructed enum value (possibly generic).
    /// - `None`: If validation fails, type mismatch occurs, or generic instantiation fails.
    ///
    fn analyze_enum_variant(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        scope_opt: Option<LocalScopeRef>,
        enum_variant: TypedEnumVariant,
        method_call: &mut TypedMethodCall,
        resolved_enum: &ResolvedEnum,
        generic_params: Option<&TypedGenericParamsList>,
        mapping_ctx: Option<Rc<GenericMappingCtx>>,
    ) -> Option<SemanticType> {
        let mut valued_fields = self.analyze_enum_fielded_variant(&enum_variant, method_call)?;

        let is_const = false;

        let (_, generic_type_opt) = match self.init_generic_type_with_symbol_id(
            scope_id_opt,
            scope_opt.clone(),
            resolved_enum.symbol_id,
            &mut method_call.type_args,
            mapping_ctx,
            generic_params,
            is_const,
            method_call.loc.clone(),
        ) {
            Ok(opt) => opt?,
            Err(diag) => {
                self.reporter.report(diag);
                return None;
            }
        };

        for (typed_expr, enum_valued_field) in method_call.args.iter_mut().zip(valued_fields.iter_mut()) {
            let field_expected_type = self
                .try_infer_generic_param_as_expected_type(enum_valued_field.ty.clone(), &generic_type_opt)
                .unwrap_or(enum_valued_field.ty.clone());

            self.analyze_expr(scope_id_opt, typed_expr, Some(field_expected_type));

            if let Some(sema_ty) = self.infer_generic_param(
                scope_id_opt,
                generic_type_opt.as_ref(),
                enum_valued_field.ty.clone(),
                typed_expr.sema_ty.clone(),
                typed_expr.loc.clone(),
            ) {
                enum_valued_field.ty = sema_ty;
            }

            if generic_type_opt.is_none() {
                if !self.check_type_mismatch(
                    scope_id_opt,
                    typed_expr.sema_ty.clone().unwrap(),
                    enum_valued_field.ty.clone(),
                    typed_expr.loc.clone(),
                ) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                            lhs_type: format_sema_ty(
                                enum_valued_field.ty.clone(),
                                &(self.symbol_formatter)(scope_id_opt),
                            ),
                            rhs_type: format_sema_ty(
                                typed_expr.sema_ty.clone().unwrap(),
                                &(self.symbol_formatter)(scope_id_opt),
                            ),
                        }),
                        location: Some(DiagLoc::new(typed_expr.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        }

        if let Some(generic_type) = generic_type_opt {
            // validate generic type instantiation

            let generic_params = generic_params
                .or(resolved_enum.enum_sig.generic_params.as_ref())
                .cloned()
                .unwrap();

            let final_generic_type = match generic_type.finalize(
                self.mapping_ctx_arena.clone(),
                generic_params,
                (self.symbol_formatter)(scope_id_opt),
            ) {
                Ok(generic_type) => generic_type,
                Err(diag) => {
                    self.reporter.report(diag);
                    return None;
                }
            };

            method_call.operand.sema_ty = Some(SemanticType::GenericType(final_generic_type.clone()));
            Some(SemanticType::GenericType(final_generic_type.clone()))
        } else {
            method_call.operand.sema_ty = Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                resolved_enum.symbol_id,
            )));

            if is_const {
                Some(SemanticType::Const(Box::new(SemanticType::ResolvedSymbol(
                    ResolvedSymbol::Enum(resolved_enum.symbol_id),
                ))))
            } else {
                Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                    resolved_enum.symbol_id,
                )))
            }
        }
    }

    /// Analyzes method calls as enum variant constructions with field values.
    ///
    /// Validates and extracts field information when an enum variant is being
    /// constructed with field values via method-call syntax. Ensures the variant
    /// accepts fields and the argument count matches the variant's field count.
    ///
    /// # Validation
    /// - Confirms the variant is a fielded variant (not a unit variant).
    /// - Validates argument count matches variant field count.
    /// - Provides detailed error messages for mismatches.
    ///
    /// # Parameters
    /// - `enum_variant`: The enum variant definition being constructed.
    /// - `method_call`: The method call AST node representing the variant construction.
    ///
    /// # Returns
    /// - `Some(Vec<TypedEnumValuedField>)`: The variant's field definitions if valid.
    /// - `None`: If the variant doesn't accept fields or argument count mismatches.
    ///
    fn analyze_enum_fielded_variant<'b>(
        &mut self,
        mut enum_variant: &'b TypedEnumVariant,
        method_call: &TypedMethodCall,
    ) -> Option<Vec<TypedEnumValuedField>> {
        match &mut enum_variant {
            TypedEnumVariant::Variant(_, valued_fields) => {
                if valued_fields.len() != method_call.args.len() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::EnumVariantArgCountMismatch {
                            variant_name: method_call.method_name.clone(),
                            expected: valued_fields.len() as u32,
                            provided: method_call.args.len() as u32,
                        }),
                        location: Some(DiagLoc::new(method_call.loc.clone())),
                        hint: None,
                    });
                    return None;
                }

                Some(valued_fields.clone())
            }
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::EnumVariantDoesNotAcceptFields {
                        variant_name: method_call.method_name.clone(),
                    }),
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        }
    }

    /// Analyzes enum variant access for unit (fieldless) variants.
    ///
    /// Type-checks accesses to enum variants without fields (unit variants) through
    /// field access syntax (e.g., `Color.Red`). Handles both non-generic and generic
    /// enum types, and validates that the accessed variant is indeed a unit variant.
    ///
    /// # Process
    /// 1. **Variant Resolution**: Locates the variant by name in the enum definition.
    /// 2. **Variant Validation**: Ensures the variant is a unit variant (not fielded).
    /// 3. **Generic Instantiation**: Initializes generic type if enum is generic.
    /// 4. **Generic Finalization**: Validates and finalizes generic type instantiation.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and generic resolution.
    /// - `scope_opt`: Local scope for symbol lookup.
    /// - `resolved_enum`: The resolved enum type definition.
    /// - `field_access`: The field access AST node representing variant access.
    /// - `generic_params`: Generic parameters from context (if any).
    /// - `mapping_ctx`: Generic mapping context for type substitution.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The type of the enum (possibly generic instantiation).
    /// - `None`: If variant doesn't exist, is a fielded variant, or generic instantiation fails.
    ///
    fn analyze_enum_variant_no_field(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        scope_opt: Option<LocalScopeRef>,
        resolved_enum: &ResolvedEnum,
        field_access: &mut TypedFieldAccess,
        generic_params: Option<&TypedGenericParamsList>,
        mapping_ctx: Option<Rc<GenericMappingCtx>>,
    ) -> Option<SemanticType> {
        field_access.object_symbol_id = Some(resolved_enum.symbol_id);

        let enum_variant_idx_opt = resolved_enum
            .enum_sig
            .variants
            .iter()
            .position(|variant| variant.ident().as_string() == field_access.field_name);

        let enum_variant_opt =
            enum_variant_idx_opt.and_then(|idx| Some(resolved_enum.enum_sig.variants.get(idx).unwrap()));

        if enum_variant_opt.is_none() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VariantNotDefinedForEnum {
                    enum_name: resolved_enum.enum_sig.name.clone(),
                    variant_name: field_access.field_name.clone(),
                }),
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
            });
            return None;
        } else if matches!(enum_variant_opt, Some(TypedEnumVariant::Variant(..))) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VariantMissingFields {
                    enum_name: resolved_enum.enum_sig.name.clone(),
                    variant_name: field_access.field_name.clone(),
                }),
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
            });
            return None;
        }

        field_access.field_index = Some(enum_variant_idx_opt.unwrap());

        let is_const = false;

        let (_, generic_type_opt) = match self.init_generic_type_with_symbol_id(
            scope_id_opt,
            scope_opt.clone(),
            resolved_enum.symbol_id,
            &mut field_access.type_args,
            mapping_ctx,
            generic_params,
            is_const,
            field_access.loc.clone(),
        ) {
            Ok(opt) => opt?,
            Err(diag) => {
                self.reporter.report(diag);
                return None;
            }
        };

        if let Some(generic_type) = generic_type_opt {
            // validate generic type instantiation
            let final_generic_type = match generic_type.finalize(
                self.mapping_ctx_arena.clone(),
                resolved_enum.enum_sig.generic_params.clone().unwrap(),
                (self.symbol_formatter)(scope_id_opt),
            ) {
                Ok(generic_type) => generic_type,
                Err(diag) => {
                    self.reporter.report(diag);
                    return None;
                }
            };

            field_access.operand.sema_ty = Some(SemanticType::GenericType(final_generic_type.clone()));
            Some(SemanticType::GenericType(final_generic_type.clone()))
        } else {
            field_access.operand.sema_ty = Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                resolved_enum.symbol_id,
            )));

            if is_const {
                Some(SemanticType::Const(Box::new(SemanticType::ResolvedSymbol(
                    ResolvedSymbol::Enum(resolved_enum.symbol_id),
                ))))
            } else {
                Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                    resolved_enum.symbol_id,
                )))
            }
        }
    }

    /// Resolves the kind of member access operation based on operand type analysis.
    ///
    /// Determines whether a member access expression refers to a named struct, unnamed struct,
    /// or union member by analyzing the operand's type and resolving the underlying symbol.
    /// This function handles multiple type categories and extracts the appropriate access
    /// context for subsequent field/member resolution.
    ///
    /// # Resolution Pipeline
    /// 1. Operand Type Analysis: Determines the semantic type of the access operand.
    /// 2. Symbol ID Extraction: Extracts the underlying symbol ID from the operand type.
    /// 3. Symbol Resolution: Resolves the symbol to its declaration (struct/union).
    /// 4. Access Kind Categorization: Classifies the access as named/unnamed struct or union.
    ///
    /// # Supported Operand Types
    /// - Symbol References: Variables, parameters, or global symbols.
    /// - General Expressions: Arbitrary expressions yielding structured types.
    /// - Pointers: Pointer types to structs/unions (excluding void pointers).
    /// - Unnamed Structs: Direct unnamed struct types.
    /// - Generic Types: Generic type instances with struct/union bases.
    ///
    /// # Return Variants
    /// - FieldAccessKind::NamedStruct: Access to a named struct type's members.
    /// - FieldAccessKind::UnnamedStruct: Access to an anonymous struct's members.
    /// - FieldAccessKind::Union: Access to a union type's members.
    /// - None: Invalid access (unsupported type, resolution failure, or error).
    ///
    /// # Parameters
    /// - scope_id_opt: Scope for type analysis and symbol resolution.
    /// - scope_opt: Local scope reference for variable resolution.
    /// - operand: The member access operand expression (may be modified during analysis).
    /// - expected_type: Optional expected type context for expression analysis.
    /// - loc: Source location for error reporting and diagnostics.
    ///
    /// # Returns
    /// - Some(FieldAccessKind): The resolved access category with type information.
    /// - None: If resolution fails due to invalid types, unresolved symbols, or errors.
    ///
    fn resolve_member_access_kind(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        scope_opt: Option<LocalScopeRef>,
        operand: &mut TypedExprStmt,
        expected_type: Option<SemanticType>,
        loc: SourceLoc,
    ) -> Option<FieldAccessKind> {
        let operand_type = match &operand.kind {
            TypedExprKind::Symbol(instance_symbol_id, ..) => {
                let resolved_var_type = match self.analyze_var_or_global_var_type(
                    scope_id_opt,
                    scope_opt.clone(),
                    *instance_symbol_id,
                    loc.clone(),
                ) {
                    Some(sema_ty) => sema_ty,
                    None => return None,
                };

                resolved_var_type.clone()
            }
            _ => match self.analyze_expr(scope_id_opt, operand, expected_type.clone()) {
                Some(sema_ty) => sema_ty,
                None => return None,
            },
        };

        let object_symbol_id = match match operand_type.const_inner() {
            SemanticType::ResolvedSymbol(resolved_symbol) => Some(resolved_symbol.symbol_id()),
            SemanticType::Pointer(sema_ty) => {
                if sema_ty.is_void() {
                    return None;
                } else if let Some(unnamed_struct_type) = sema_ty.as_unnamed_struct() {
                    return Some(FieldAccessKind::UnnamedStruct(Box::new(unnamed_struct_type)));
                }

                self.extract_object_symbol_id(scope_id_opt, *sema_ty.clone(), loc.clone())
            }
            SemanticType::UnnamedStruct(unnamed_struct_type) => {
                return Some(FieldAccessKind::UnnamedStruct(Box::new(unnamed_struct_type.clone())));
            }
            SemanticType::GenericType(generic_type) => Some(generic_type.base),
            _ => None,
        } {
            Some(symbol_id) => symbol_id,
            None => return None,
        };

        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        let sym = match self
            .resolver
            .resolve_local_or_global_symbol(scope_opt, object_symbol_id)
        {
            Some(sym) => sym,
            None => return None,
        };

        sym.as_struct()
            .map(|resolved_struct| FieldAccessKind::NamedStruct(Box::new(resolved_struct.clone())))
            .or_else(|| {
                sym.as_union()
                    .map(|resolved_union| FieldAccessKind::Union(Box::new(resolved_union.clone())))
            })
    }

    /// Analyzes tuple member access expressions (e.g., `tuple.0`, `tuple.1`).
    ///
    /// Type-checks tuple indexing operations by verifying the operand is a tuple type
    /// and the index is within bounds for that tuple. Returns the type of the accessed
    /// element if the access is valid.
    ///
    /// # Validation Steps
    /// 1. **Operand Type Analysis**: Type-checks the tuple expression operand.
    /// 2. **Tuple Type Verification**: Ensures the operand is actually a tuple type.
    /// 3. **Bounds Checking**: Validates that the index is within the tuple's length.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for operand type checking.
    /// - `tuple_member_access`: The tuple access expression to analyze.
    /// - `expected_type`: Optional type context for operand analysis.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The type of the accessed tuple element.
    /// - `None`: If the operand is not a tuple or index is out of bounds (errors reported).
    ///
    fn analyze_tuple_member_access(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        tuple_member_access: &mut TypedTupleAccessExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let operand_type = self.analyze_expr(scope_id_opt, &mut tuple_member_access.operand, expected_type)?;

        if !operand_type.const_inner().as_tuple_type().is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand),
                location: Some(DiagLoc::new(tuple_member_access.loc.clone())),
                hint: None,
            });
            return None;
        }

        let tuple_type = operand_type.as_tuple_type().unwrap();

        // inbounds check for tuple type

        if tuple_member_access.index > (tuple_type.type_list.len() - 1) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleIndexOutOfRange {
                    index: tuple_member_access.index.try_into().unwrap(),
                    length: tuple_type.type_list.len(),
                }),
                location: Some(DiagLoc::new(tuple_member_access.loc.clone())),
                hint: None,
            });
            return None;
        }

        let element_type = tuple_type.type_list.get(tuple_member_access.index).unwrap();

        Some(element_type.clone())
    }

    /// Sets contextual type information for the current method call's 'self' parameter.
    ///
    /// Updates context state with the type of the object instance being operated on
    /// in a method call. This contextual information is used for subsequent semantic
    /// analysis within the method body (e.g., field access validation, implicit 'self').
    ///
    /// # State Updates
    /// - `current_obj_operand_ty`: The non-const, non-pointer inner type of the instance.
    /// - `current_self`: The exact semantic type of the 'self' parameter.
    /// - `method_call.self_ty`: The type attached to the method call AST node.
    ///
    /// # Parameters
    /// - `method_call`: The method call AST node to annotate with self type.
    /// - `sema_ty`: The semantic type of the 'self' parameter (may include const/pointer qualifiers).
    pub(crate) fn set_ty_ctx_self_type(&mut self, method_call: &mut TypedMethodCall, sema_ty: &SemanticType) {
        self.ty_ctx.current_obj_operand_ty = Some(sema_ty.const_inner().pointer_inner().const_inner().clone());
        self.ty_ctx.current_self = Some(sema_ty.clone());
        method_call.self_ty = Some(sema_ty.clone());
    }

    /// Analyzes and updates the type of 'self' modifier parameters in generic contexts.
    ///
    /// Processes the first parameter of a function when it is a 'self' modifier (copied
    /// or referenced), updating its semantic type based on the resolved generic type.
    /// This ensures that 'self' parameters in generic methods receive the correct
    /// concrete type when the method is instantiated.
    ///
    /// # Behavior
    /// - For `SelfModifierKind::Copied`: Sets the parameter type directly to `sema_ty`.
    /// - For `SelfModifierKind::Referenced`: Sets the parameter type to a pointer to `sema_ty`.
    ///
    /// # Parameters
    /// - `scope_id`: Scope containing the parameter symbols.
    /// - `params`: Function parameters to analyze.
    /// - `sema_ty`: The resolved semantic type to apply to the 'self' parameter.
    ///
    pub(crate) fn analyze_generic_self_modifier(
        &self,
        scope_id: ScopeID,
        params: &TypedFuncParams,
        sema_ty: SemanticType,
    ) {
        let scope_rc = self.resolver.resolve_local_scope(self.module_id, scope_id).unwrap();

        if let Some(first_param) = params.list.first() {
            if let Some(self_modifier) = first_param.as_self_modifier() {
                let mut scope_ref = scope_rc.borrow_mut();

                let new_self_modifier_ty = match self_modifier.kind {
                    SelfModifierKind::Copied => sema_ty,
                    SelfModifierKind::Referenced => SemanticType::Pointer(Box::new(sema_ty)),
                };

                scope_ref.with_symbol_id_mut(self_modifier.self_symbol_id.unwrap(), |local_symbol| {
                    let resolved_var = local_symbol.as_variable_mut().unwrap();
                    resolved_var.typed_variable.ty = Some(new_self_modifier_ty);
                });
                drop(scope_ref);
            }
        }
    }

    /// Analyzes and potentially transforms the 'self' argument for object method calls.
    ///
    /// Processes the operand passed to a method call based on the 'self' modifier kind
    /// and access operator. For referenced 'self' parameters with dot notation ('.'),
    /// automatically inserts an address-of operation to create a pointer to the object.
    ///
    /// # Transformations
    /// - `SelfModifierKind::Copied`: Uses the operand directly (pass by value).
    /// - `SelfModifierKind::Referenced`:
    ///   - With '->': Uses pointer operand directly.
    ///   - With '.': Creates an address-of expression to obtain a pointer.
    ///
    /// # Parameters
    /// - `operand`: The expression being passed as the 'self' argument.
    /// - `is_fat_arrow`: Whether the method was accessed using '->' operator.
    /// - `self_modifier`: The 'self' modifier specification from the method signature.
    ///
    /// # Returns
    /// The transformed expression ready to be used as the method's 'self' argument.
    ///
    fn analyze_object_self_modifier_argument(
        &mut self,
        operand: &TypedExprStmt,
        is_fat_arrow: bool,
        self_modifier: &TypedSelfModifier,
    ) -> TypedExprStmt {
        match self_modifier.kind {
            SelfModifierKind::Copied => operand.clone(),
            SelfModifierKind::Referenced => {
                // only take address if not a fat arrow
                if is_fat_arrow {
                    operand.clone()
                } else {
                    let expr_ty = operand.sema_ty.clone().unwrap();
                    TypedExprStmt {
                        kind: TypedExprKind::AddrOf(TypedAddrOfExpr {
                            operand: Box::new(operand.clone()),
                            loc: operand.loc.clone(),
                        }),
                        sema_ty: Some(SemanticType::Pointer(Box::new(expr_ty))),
                        mloc: MemoryLocation::LValue,
                        loc: operand.loc.clone(),
                    }
                }
            }
        }
    }

    /// Validates that type arguments are not provided for non-generic types.
    ///
    /// Checks whether type arguments (e.g., `<T, U>`) are unexpectedly provided
    /// for a type that does not have generic parameters. This prevents syntax
    /// like `NonGenericType<int>` which would be invalid.
    ///
    /// # Parameters
    /// - `generic_params`: Optional generic parameters defined on the type.
    /// - `type_args`: Optional type arguments provided at the usage site.
    /// - `loc`: Source location for error reporting.
    ///
    /// # Returns
    /// - `true`: If unexpected type arguments were found (error reported).
    /// - `false`: If type arguments are properly used or absent.
    ///
    fn check_unexpected_type_args(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        type_args: &Option<TypedTypeArgs>,
        loc: SourceLoc,
    ) -> bool {
        if generic_params.is_none() && type_args.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                location: Some(DiagLoc::new(loc)),
                hint: None,
            });
            return true;
        }
        false
    }

    /// Resolves and normalizes the semantic type of a variable or global variable.
    ///
    /// This function analyzes variable declarations (both local and global) to determine
    /// their semantic types. For local variables, it may need to infer the type from
    /// the initialization expression if not explicitly annotated. For global variables,
    /// it retrieves the pre-computed type signature.
    ///
    /// # Process
    /// 1. **Symbol Resolution**: Locates the variable symbol in either local or global scope.
    /// 2. **Local Variable Analysis**:
    ///    - If the variable has an explicit type annotation, normalizes that type.
    ///    - If unannotated, type-checks the initialization expression to infer the type.
    /// 3. **Global Variable Analysis**: Retrieves the pre-computed type from the global
    ///    variable's signature.
    /// 4. **Type Normalization**: Applies type normalization (alias resolution, generic
    ///    substitutions) to ensure consistency.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Optional scope ident for name resolution and type normalization.
    /// - `scope_opt`: Optional reference to the local scope for symbol lookup.
    /// - `instance_symbol_id`: Ident of the variable symbol to analyze.
    /// - `loc`: Source location for error reporting during type normalization.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The normalized semantic type of the variable.
    /// - `None`: If:
    ///   - The symbol doesn't resolve to a variable (global symbol is not a global var)
    ///   - Type inference fails for unannotated local variables
    ///   - Type normalization fails
    ///
    fn analyze_var_or_global_var_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        scope_opt: Option<LocalScopeRef>,
        instance_symbol_id: SymbolID,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(scope_opt.clone(), instance_symbol_id)
            .unwrap();

        let sema_ty = match match &sym {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => {
                let typed_variable = &local_symbol.as_variable().unwrap().typed_variable;

                match &typed_variable.ty {
                    Some(sema_ty) => self.normalize_sema_type(scope_id_opt, sema_ty.clone(), loc.clone()),
                    None => {
                        let rhs = typed_variable.rhs.clone().unwrap();
                        self.analyze_expr(scope_id_opt, &mut rhs.clone(), None)
                    }
                }
            }
            LocalOrGlobalSymbol::GlobalSymbol(global_symbol) => match global_symbol.as_global_var() {
                Some(resolved_global_var) => Some(resolved_global_var.global_var_sig.ty.clone().unwrap()),
                None => None,
            },
        } {
            Some(sema_ty) => Some(sema_ty),
            None => None,
        };

        if sema_ty.is_some() {
            let normalized_type = self
                .normalize_sema_type(scope_id_opt, sema_ty.unwrap(), loc.clone())
                .unwrap();

            Some(normalized_type)
        } else {
            None
        }
    }

    /// Validates struct field access syntax, visibility, and pointer semantics.
    ///
    /// Ensures that field accesses comply with access control rules and use correct
    /// syntax operators ('.' vs '->') based on the base type's characteristics.
    /// Performs both visibility checks and pointer/object distinction validation.
    ///
    /// # Validation Steps
    /// 1. **Visibility Check**: Verifies field accessibility based on visibility
    ///    modifiers (public/private) and the calling context.
    /// 2. **Syntax Operator Validation**: Ensures correct use of '.' for direct
    ///    struct access and '->' for pointer-to-struct access.
    ///
    /// # Parameters
    /// - `operand`: The expression being accessed (the struct/pointer instance).
    /// - `field_access`: The field access AST node containing field details.
    /// - `field_vis`: Visibility modifier of the accessed field.
    /// - `struct_methods`: Methods defined on the struct for context checking.
    /// - `struct_name`: Name of the struct type for error messages.
    ///
    /// # Returns
    /// - `true`: If field access passes all validations.
    /// - `false`: If any validation fails (errors are reported via diagnostics).
    ///
    /// # Error Conditions
    /// - Accessing non-public fields from outside the struct's methods.
    /// - Using '.' operator on pointers or '->' operator on non-pointers.
    ///
    fn validate_struct_field_access(
        &mut self,
        operand: &TypedExprStmt,
        field_access: &TypedFieldAccess,
        field_vis: Visibility,
        struct_methods: &HashMap<String, SymbolID>,
        struct_name: &str,
    ) -> bool {
        let mut result = true;

        let access_violation = if let Some(current_method_symbol_id) = self.ty_ctx.current_method_symbol_id {
            let method_symbol_ids = struct_methods.values().cloned().collect::<Vec<SymbolID>>();

            if method_symbol_ids.contains(&current_method_symbol_id) {
                false
            } else {
                !field_vis.is_public()
            }
        } else {
            !field_vis.is_public()
        };

        if access_violation {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InternalFieldAccess {
                    field_name: field_access.field_name.clone(),
                    struct_name: struct_name.to_string(),
                }),
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
            });
            result = false;
        }

        let base_type = operand
            .sema_ty
            .as_ref()
            .expect("SemanticType should be set before field access")
            .const_inner();

        let is_pointer = base_type.is_pointer() || base_type.as_generic_type().is_some();
        let is_struct = base_type.is_resolved_symbol() || base_type.as_generic_type().is_some();

        if field_access.is_fat_arrow {
            if !is_pointer {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidThinArrow),
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: Some("Use '.' instead of '->'.".to_string()),
                });
                result = false;
            }
        } else {
            if !is_struct {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::UseThinArrow),
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: Some("Use '->' when accessing through a pointer.".to_string()),
                });
                result = false;
            }
        }

        result
    }

    /// Validates union field access operator usage based on pointer semantics.
    ///
    /// Ensures the correct field access operator ('.' or '->') is used when accessing
    /// union fields, depending on whether the operand is a direct union or a pointer
    /// to a union. This validation is specific to union types.
    ///
    /// # Operator Rules
    /// - Use '.' for direct union value access: `union_var.field`
    /// - Use '->' for pointer-to-union access: `union_ptr->field`
    ///
    /// # Parameters
    /// - `operand_ty`: The semantic type of the union operand.
    /// - `field_access`: The field access AST node to validate.
    ///
    /// # Returns
    /// - `true`: If the operator usage is correct.
    /// - `false`: If the wrong operator is used (errors are reported via diagnostics).
    ///
    fn validate_union_field_access(&mut self, operand_ty: SemanticType, field_access: &TypedFieldAccess) -> bool {
        let mut result = true;

        if operand_ty.is_pointer() && !field_access.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UseThinArrow),
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: Some("Use '->' when accessing through a pointer.".to_string()),
            });
            result = false;
        } else if !operand_ty.is_pointer() && field_access.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidThinArrow),
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: Some("Use '.' instead of '->'.".to_string()),
            });
            result = false;
        }

        result
    }

    /// Validates a method call's accessibility, syntax, and mutability constraints.
    ///
    /// Performs comprehensive validation of method calls including visibility checks,
    /// pointer access syntax validation, and const-correctness enforcement. Ensures
    /// that method calls adhere to the language's access control and safety rules.
    ///
    /// # Validation Steps
    /// 1. **Access Control**: Checks if non-public methods are called from outside
    ///    their defining type's methods.
    /// 2. **Syntax Validation**: Verifies correct use of '.' vs '->' operators based
    ///    on whether the operand is a pointer or object value.
    /// 3. **Mutability Checking**: Prevents mutation of const instances through
    ///    non-const methods.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for symbol formatting in error messages.
    /// - `instance_symbol_id`: Symbol ID of the method call instance.
    /// - `method_name`: Name of the method being called.
    /// - `method_call_operand_ty`: Type of the instance the method is called on.
    /// - `is_fat_arrow`: Whether the '->' operator was used.
    /// - `first_param_opt`: Optional first parameter (for self-modifier checking).
    /// - `object_methods`: Map of method names to symbols for the object type.
    /// - `object_name`: Name of the object/struct type.
    /// - `resolved_method`: The resolved method signature being called.
    /// - `loc`: Source location for error reporting.
    ///
    /// # Returns
    /// - `true`: If all validations pass.
    /// - `false`: If any validation fails (errors are reported via the diagnostic reporter).
    ///
    /// # Error Conditions
    /// - Accessing non-public methods from outside the type's methods.
    /// - Using '.' on pointers or '->' on non-pointers.
    /// - Calling mutating methods on const instances.
    ///
    fn validate_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        instance_symbol_id: SymbolID,
        method_name: &String,
        method_call_operand_ty: SemanticType,
        is_fat_arrow: bool,
        method_call_on_interface: bool,
        first_param_opt: Option<&TypedFuncParamKind>,
        object_methods_opt: Option<HashMap<String, SymbolID>>,
        object_name: String,
        func_sig: &FuncSig,
        loc: SourceLoc,
    ) -> bool {
        let mut result = true;
        let method_vis = &func_sig.modifiers.vis;

        let access_violation = if let Some(current_method_symbol_id) = self.ty_ctx.current_method_symbol_id {
            let object_contains_method = {
                if let Some(object_methods) = object_methods_opt {
                    object_methods
                        .values()
                        .cloned()
                        .collect::<Vec<SymbolID>>()
                        .contains(&current_method_symbol_id)
                } else {
                    true
                }
            };

            if object_contains_method {
                false
            } else {
                !method_vis.is_public() && !method_call_on_interface
            }
        } else {
            !method_vis.is_public() && !method_call_on_interface
        };

        if access_violation {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InternalMethodCall {
                    method_name: func_sig.name.clone(),
                    object_name,
                }),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
            result = false;
        }

        let is_pointer = method_call_operand_ty.const_inner().is_pointer();
        let is_operand_const = method_call_operand_ty.is_const();
        let is_object = method_call_operand_ty.const_inner().is_resolved_symbol()
            || method_call_operand_ty.as_generic_type().is_some()
            || method_call_on_interface;

        if is_fat_arrow {
            if !is_pointer {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidThinArrow),
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: Some("Use '.' instead of '->'.".to_string()),
                });
                result = false;
            }
        } else {
            if !is_object {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::UseThinArrow),
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: Some("Use '->' when accessing through a pointer.".to_string()),
                });
                result = false;
            }
        }

        if let Some(first_param) = first_param_opt {
            if let TypedFuncParamKind::SelfModifier(typed_self_modifier) = first_param {
                if typed_self_modifier.kind == SelfModifierKind::Referenced && is_operand_const {
                    let instance_name = (self.symbol_formatter)(scope_id_opt)(instance_symbol_id);

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::MutationPossibleMethodCallOnConstInstance {
                            method_name: method_name.clone(),
                            instance_name: instance_name.clone(),
                        }),
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: Some(format!(
                            "Instance '{}' is declared as 'const' and cannot be modified.",
                            instance_name
                        )),
                    });
                    result = false;
                }
            }
        }

        result
    }

    /// Validates that an expression type is suitable for use as a boolean condition.
    ///
    /// This function checks whether the given semantic type can be used as a condition
    /// in control flow statements (`if`, `while`, `for`, etc.).
    /// In our type system, only `bool` types are valid conditions.
    ///
    /// # Parameters
    /// - `sema_ty`: The semantic type to validate as a condition.
    /// - `loc`: Source location of the conditional expression, used for error reporting.
    ///
    /// # Diagnostics
    /// Reports a type error if `sema_ty` is not a boolean type.
    ///
    /// # Notes
    /// - Unlike some languages that allow implicit conversions (e.g., C/C++ where
    ///   non-zero values are truthy), our type system requires explicit boolean types.
    /// - This validation is typically called after type checking conditional expressions
    ///   but before generating code for control flow constructs.
    ///
    pub(crate) fn check_expr_type_must_be_condition(&mut self, sema_ty: SemanticType, loc: SourceLoc) {
        if !sema_ty.is_bool() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ConditionExprMustBeOfTypeBool),
                location: Some(DiagLoc::new(loc)),
                hint: None,
            });
        }
    }

    pub(crate) fn inference_ctx_from_struct_type(
        &self,
        scope_opt: Option<LocalScopeRef>,
        expected_type: Option<SemanticType>,
    ) -> InferenceCtx {
        let Some(sema_ty) = expected_type else {
            return InferenceCtx::default();
        };

        if let Some(unnamed_struct_type) = sema_ty.as_unnamed_struct() {
            unnamed_struct_type_as_inference_ctx(&unnamed_struct_type)
        } else if let Some(struct_symbol_id) = sema_ty.as_struct_symbol_id() {
            let sym = self
                .resolver
                .resolve_local_or_global_symbol(scope_opt, struct_symbol_id)
                .unwrap();
            let resolved_struct = sym.as_struct().unwrap();

            struct_sig_as_inference_ctx(&resolved_struct.struct_sig)
        } else if let Some(generic_type) = sema_ty.as_generic_type() {
            let sym = self
                .resolver
                .resolve_local_or_global_symbol(scope_opt, generic_type.base)
                .unwrap();
            let resolved_struct = sym.as_struct().unwrap();

            let struct_sig = substitute_struct_sig(
                self.mapping_ctx_arena.clone(),
                &resolved_struct.struct_sig,
                generic_type.mapping_ctx.clone(),
            )
            .unwrap();

            struct_sig_as_inference_ctx(&struct_sig)
        } else {
            InferenceCtx::default()
        }
    }
}

#[derive(Debug, Clone)]
enum FieldAccessKind {
    UnnamedStruct(Box<TypedUnnamedStructType>),
    NamedStruct(Box<ResolvedStruct>),
    Union(Box<ResolvedUnion>),
}

fn self_modifier_param_type(params: &TypedFuncParams, sema_ty: SemanticType) -> Option<SemanticType> {
    if let Some(first_param) = params.list.first() {
        if let Some(self_modifier) = first_param.as_self_modifier() {
            return Some(match self_modifier.kind {
                SelfModifierKind::Copied => sema_ty,
                SelfModifierKind::Referenced => SemanticType::Pointer(Box::new(sema_ty)),
            });
        }
    }
    None
}

fn infer_integer_type(
    literal: &TypedLiteralExpr,
    suffix_opt: &Option<Box<TokenKind>>,
    expected: Option<SemanticType>,
) -> Result<SemanticType, Diag> {
    if let Some(suffix) = suffix_opt {
        return match map_integer_suffix_to_sema_type(&suffix) {
            Some(ty) => Ok(ty),
            None => Err(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidIntegerLiteralSuffix),
                location: Some(DiagLoc::new(literal.loc.clone())),
                hint: Some(format!("Invalid suffix {:?} for integer literal.", suffix)),
            }),
        };
    } else if let Some(ty) = expected {
        if ty.is_integer() {
            return Ok(ty);
        }
    }

    // default integer type
    Ok(SemanticType::PlainType(PlainType::Int))
}

fn infer_float_type(
    literal: &TypedLiteralExpr,
    suffix_opt: &Option<Box<TokenKind>>,
    expected: Option<SemanticType>,
) -> Result<SemanticType, Diag> {
    if let Some(suffix) = suffix_opt {
        return match map_float_suffix_to_sema_type(&suffix) {
            Some(ty) => Ok(ty),
            None => Err(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidFloatLiteralSuffix),
                location: Some(DiagLoc::new(literal.loc.clone())),
                hint: Some(format!("Invalid suffix {} for float literal.", suffix)),
            }),
        };
    } else if let Some(ty) = expected {
        if ty.is_float() {
            return Ok(ty);
        }
    }

    // default float type
    Ok(SemanticType::PlainType(PlainType::Float64))
}
