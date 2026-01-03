// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
use crate::{
    analyze::AnalysisContext, diagnostics::AnalyzerDiagKind, format::format_missing_fields, update_global_symbol,
};
use cyrusc_abi::visibility::Visibility;
use cyrusc_ast::{LiteralKind, SelfModifierKind, StringPrefix, source_loc::SourceLoc, token::TokenKind};
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc};
use cyrusc_resolver::symbols::{
    LocalOrGlobalSymbol, LocalScopeRef, ResolvedEnum, ResolvedMethod, ResolvedStruct, ResolvedUnion, SymbolEntryKind,
};
use cyrusc_strescape::unescape_string;
use cyrusc_tast::{
    exprs::*,
    format::{format_func_ty, format_sema_ty, format_typed_expr},
    generics::{
        generic_type::GenericType,
        mapping_ctx::GenericMappingCtx,
        substitute::{substitute_func_sig, substitute_struct_sig, substitute_type, substitute_union_sig},
    },
    sigs::{FuncSig, UnionSig, set_self_modifier_type_in_func_sig, typed_func_params_as_func_type_params},
    stmts::{
        TypedEnumValuedField, TypedEnumVariant, TypedFuncParamKind, TypedFuncParams, TypedFuncTypeVariadicParams,
        TypedFuncVariadicParams, TypedGenericParamsList, TypedStructField, TypedTypeArgs,
    },
    types::*,
    *,
};
use std::{collections::HashMap, rc::Rc};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_explicit_sema_ty(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        sema_ty: &SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        if let Some(symbol_id) = sema_ty.get_symbol_id() {
            let sym = self
                .resolver
                .resolve_local_or_global_symbol(local_scope_opt, symbol_id)?;

            let is_generic_object = sym.get_generic_params().is_some();
            let is_generic_type = sema_ty.as_generic_type().is_some();

            if is_generic_object && !is_generic_type {
                let type_name = format_sema_ty(sema_ty.clone(), &(self.symbol_formatter)(scope_id_opt));

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::MissingTypeArgs { type_name }),
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
                return None;
            }
        }

        Some(sema_ty.clone())
    }

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

    pub(crate) fn analyze_expr_non_terminal(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &mut TypedExprStmt,
        mut expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        if let Some(sema_ty) = expected_type {
            expected_type = Some(sema_ty.get_const_inner().clone());
        }

        self.lower_special_exprs(scope_id_opt, typed_expr, expected_type.clone());

        let sema_ty = match &mut typed_expr.kind {
            TypedExprKind::Symbol(symbol_id, ..) => {
                let local_scope_opt =
                    scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, *symbol_id)
                    .unwrap();

                let sema_ty = self.resolve_full_type_from_local_or_global_symbol(scope_id_opt, sym);
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
            TypedExprKind::Array(typed_array) => self.analyze_array(scope_id_opt, typed_array),
            TypedExprKind::ArrayIndex(typed_array_index) => self.analyze_array_index(scope_id_opt, typed_array_index),
            TypedExprKind::AddrOf(typed_address_of) => self.analyze_addr_of_expr_type(scope_id_opt, typed_address_of),
            TypedExprKind::Deref(typed_deref) => self.analyze_deref_expr_type(scope_id_opt, typed_deref),
            TypedExprKind::StructInit(struct_init) => {
                self.analyze_struct_init(scope_id_opt, struct_init, expected_type)
            }
            TypedExprKind::FuncCall(typed_func_call) => {
                self.analyze_func_call(scope_id_opt, typed_func_call, expected_type)
            }
            TypedExprKind::UStructValue(typed_unnamed_struct_value) => {
                self.analyze_unnamed_struct_value(scope_id_opt, typed_unnamed_struct_value)
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
        };

        let normalized_type = self.normalize_type(scope_id_opt, sema_ty.clone()?, typed_expr.loc.clone(), true);
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

    pub(crate) fn analyze_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &mut TypedExprStmt,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        match &typed_expr.kind {
            TypedExprKind::Symbol(symbol_id, _) => {
                let local_scope_opt =
                    scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, *symbol_id)
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

        self.analyze_expr_non_terminal(scope_id_opt, typed_expr, expected_type)
    }

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

    fn analyze_tuple_member_access(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        tuple_member_access: &mut TypedTupleAccessExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let operand_type = self.analyze_expr(scope_id_opt, &mut tuple_member_access.operand, expected_type)?;

        if !operand_type.get_const_inner().as_tuple_type().is_some() {
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

    fn analyze_lambda(&mut self, scope_id_opt: Option<ScopeID>, lambda: &mut TypedLambdaExpr) -> Option<SemanticType> {
        let current_func_clone = self.current_func.clone();

        self.normalize_func_params(&mut lambda.params, lambda.loc.clone());
        lambda.return_type =
            self.normalize_type(scope_id_opt, lambda.return_type.clone(), lambda.loc.clone(), false)?;
        let params = typed_func_params_as_func_type_params(&lambda.params);
        let func_type = TypedFuncType {
            symbol_id: None,
            def_module_id: Some(self.module_id),
            params,
            return_type: Box::new(lambda.return_type.clone()),
            is_public: true,
            loc: lambda.loc.clone(),
        };

        self.current_func = Some(func_type.clone());
        self.analyze_block_stmt(&mut lambda.body);

        self.current_func = current_func_clone;
        Some(SemanticType::FuncType(func_type))
    }

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

    fn analyze_unnamed_struct_value(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_struct_value: &mut TypedUStructValue,
    ) -> Option<SemanticType> {
        let mut fields: Vec<TypedUnnamedStructTypeField> = Vec::new();

        for field in &mut unnamed_struct_value.fields {
            let field_value_type = match self.analyze_expr(scope_id_opt, &mut field.field_value, field.field_ty.clone())
            {
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            fields.push(TypedUnnamedStructTypeField {
                field_name: field.field_name.clone(),
                field_ty: Box::new(field.field_ty.clone().unwrap_or(field_value_type)),
                loc: field.loc.clone(),
            });
        }

        let unnamed_struct_type = TypedUStructType {
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
        let is_operand_array = operand_type.get_const_inner().is_array();

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
            .get_const_inner()
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
            element_type = sema_ty.get_pointer_inner().clone();

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

    fn validate_field_access(
        &mut self,
        operand: &TypedExprStmt,
        field_access: &TypedFieldAccess,
        field_vis: Visibility,
        struct_methods: &HashMap<String, SymbolID>,
        struct_name: &str,
    ) -> bool {
        let mut result = true;

        let access_violation = if let Some(current_method_symbol_id) = self.current_method_symbol_id {
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
            .get_const_inner();

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

    fn validate_union_field_access(&mut self, operand_ty: SemanticType, field_access: &TypedFieldAccess) -> bool {
        let mut result = true;

        if operand_ty.is_pointer() && !field_access.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UseThinArrow),
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
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
        let field_ty = match self.normalize_type(scope_id_opt, field.ty.clone(), field_access.loc.clone(), false) {
            Some(sema_ty) => sema_ty,
            None => return None,
        };

        if !self.validate_union_field_access(operand_type.get_const_inner().clone(), &field_access) {
            return None;
        }

        field_access.field_index = Some(union_field_idx);
        field_access.field_ty = Some(field_ty.clone());
        field_access.object_symbol_id = Some(union_sig.symbol_id);

        Some(field_ty)
    }

    fn analyze_unnamed_struct_field_access(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_struct_type: &TypedUStructType,
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
            .position(|field| *field.field_name == field_access.field_name.clone())
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
        let field_ty = match self.normalize_type(scope_id_opt, *field.field_ty.clone(), field_access.loc.clone(), false)
        {
            Some(sema_ty) => sema_ty,
            None => return None,
        };

        field_access.field_index = Some(field_idx);
        field_access.field_ty = Some(field_ty.clone());

        Some(field_ty.clone())
    }

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
            .normalize_type(
                scope_id_opt,
                typed_struct_field.ty.clone(),
                field_access.loc.clone(),
                false,
            )
            .unwrap();

        if !self.validate_field_access(
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

    fn analyze_enum_variant(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
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
            local_scope_opt.clone(),
            resolved_enum.symbol_id,
            &method_call.type_args,
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

        method_call.object_symbol_id = Some(resolved_enum.symbol_id);

        for (typed_expr, enum_valued_field) in method_call.args.iter_mut().zip(valued_fields.iter_mut()) {
            let field_expected_type = self
                .try_infer_generic_param_as_expected_type(enum_valued_field.ty.clone(), &generic_type_opt)
                .unwrap_or(enum_valued_field.ty.clone());

            self.analyze_expr(scope_id_opt, typed_expr, Some(field_expected_type));

            if let Some(sema_ty) = self.infer_generic_param(
                scope_id_opt,
                &generic_type_opt,
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

            let final_generic_type = match generic_type.finalize(generic_params, (self.symbol_formatter)(scope_id_opt))
            {
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

    fn analyze_enum_variant_no_field(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
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
            .position(|variant| variant.get_identifier().as_string() == field_access.field_name);

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
            local_scope_opt.clone(),
            resolved_enum.symbol_id,
            &field_access.type_args,
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

    fn resolve_member_access_kind(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
        operand: &mut TypedExprStmt,
        expected_type: Option<SemanticType>,
        loc: SourceLoc,
    ) -> Option<MemberAccessKind> {
        let operand_type = match &operand.kind {
            TypedExprKind::Symbol(instance_symbol_id, ..) => {
                let resolved_var_type = match self.analyze_var_or_global_var_type(
                    scope_id_opt,
                    local_scope_opt.clone(),
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

        let object_symbol_id = match match operand_type.get_const_inner() {
            SemanticType::ResolvedSymbol(resolved_symbol) => Some(resolved_symbol.get_symbol_id()),
            SemanticType::Pointer(sema_ty) => {
                if sema_ty.is_void() {
                    return None;
                } else if let Some(unnamed_struct_type) = sema_ty.as_unnamed_struct() {
                    return Some(MemberAccessKind::UnnamedStruct(Box::new(unnamed_struct_type)));
                }

                self.extract_object_symbol_id(scope_id_opt, *sema_ty.clone(), loc.clone())
            }
            SemanticType::UnnamedStruct(unnamed_struct_type) => {
                return Some(MemberAccessKind::UnnamedStruct(Box::new(unnamed_struct_type.clone())));
            }
            SemanticType::GenericType(generic_type) => Some(generic_type.base),
            _ => None,
        } {
            Some(symbol_id) => symbol_id,
            None => return None,
        };

        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let sym = match self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, object_symbol_id)
        {
            Some(sym) => sym,
            None => return None,
        };

        sym.as_struct()
            .map(|resolved_struct| MemberAccessKind::NamedStruct(Box::new(resolved_struct.clone())))
            .or_else(|| {
                sym.as_union()
                    .map(|resolved_union| MemberAccessKind::Union(Box::new(resolved_union.clone())))
            })
    }

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

        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

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
            .map(|sema_ty| sema_ty.get_const_inner())
            .cloned()?;

        {
            if let Some(symbol_id) = field_access_operand_ty.get_symbol_id() {
                if let Some(sym) = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)
                {
                    if self.check_unexpected_type_args(
                        &sym.get_generic_params(),
                        &field_access.type_args,
                        field_access.loc.clone(),
                    ) {
                        return None;
                    };
                }
            }
        }

        if let Some(sema_ty) =
            self.merge_generic_operand_as_expected_type(field_access_operand_ty.clone(), expected_type.clone())
        {
            field_access_operand_ty = sema_ty;
        }

        {
            let (detected_as_enum_variant, sema_ty) = self.maybe_enum_variant_constructor_from_field_access(
                scope_id_opt,
                local_scope_opt.clone(),
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
        let operand_ty = sema_ty.get_const_inner().get_pointer_inner();

        let generic_type_opt = operand_ty.as_generic_type();

        let (return_sema_ty, is_generic) = match self.resolve_member_access_kind(
            scope_id_opt,
            local_scope_opt.clone(),
            &mut field_access.operand,
            expected_type.clone(),
            field_access.loc.clone(),
        ) {
            Some(member_access_kind) => match member_access_kind {
                MemberAccessKind::UnnamedStruct(unnamed_struct_type) => (
                    self.analyze_unnamed_struct_field_access(
                        scope_id_opt,
                        &unnamed_struct_type,
                        field_access,
                        expected_type.clone(),
                    ),
                    false,
                ),
                MemberAccessKind::NamedStruct(resolved_struct) => {
                    let mut struct_sig = resolved_struct.struct_sig.clone();
                    if let Some(generic_type) = generic_type_opt {
                        struct_sig = substitute_struct_sig(&struct_sig, generic_type.mapping_ctx.clone())?;
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
                MemberAccessKind::Union(resolved_union) => {
                    let mut union_sig = resolved_union.union_sig.clone();
                    if let Some(generic_type) = generic_type_opt {
                        union_sig = substitute_union_sig(&union_sig, generic_type.mapping_ctx.clone())?;
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

    fn analyze_struct_init(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        struct_init: &mut TypedStructInitExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let mut sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), struct_init.symbol_id)
            .unwrap();

        self.check_unexpected_type_args(
            &sym.get_generic_params(),
            &struct_init.type_args,
            struct_init.loc.clone(),
        );

        let mut sema_ty = self.resolve_full_type_from_local_or_global_symbol(scope_id_opt, sym.clone())?;

        if let Some(new_sema_ty) = self.merge_generic_operand_as_expected_type(sema_ty.clone(), expected_type.clone()) {
            sema_ty = new_sema_ty;
        }

        let (generic_params, mapping_ctx) =
            self.initial_generic_params_and_mapping_ctx(&sema_ty, sym.get_generic_params().as_ref(), expected_type);

        let pure_symbol_id = sema_ty.get_pure_symbol_id().unwrap();

        let generic_type_opt = match self.init_generic_type_with_symbol_id(
            scope_id_opt,
            local_scope_opt.clone(),
            pure_symbol_id,
            &struct_init.type_args,
            mapping_ctx,
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
            if pure_symbol_id != sym.get_symbol_id() {
                sym = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt.clone(), pure_symbol_id)
                    .unwrap();
            }

            if let Some(resolved_union) = sym.as_union() {
                return self.analyze_regular_union_init(scope_id_opt, struct_init, resolved_union, &generic_type_opt);
            } else if let Some(resolved_struct) = sym.as_struct() {
                return self.analyze_regular_struct_init(scope_id_opt, struct_init, resolved_struct, &generic_type_opt);
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
                hint: Some("Union initialization must contain exactly one field".to_string()),
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
            generic_type_opt,
            field.ty.clone(),
            field_init.value.sema_ty.clone(),
            field_init.value.loc.clone(),
        ) {
            field_init.value.sema_ty = Some(sema_ty);
        }

        if let Some(generic_type) = generic_type_opt {
            // validate generic type instantiation
            let final_generic_type = match generic_type.finalize(
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

    fn analyze_regular_struct_init(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        struct_init: &mut TypedStructInitExpr,
        resolved_struct: &ResolvedStruct,
        generic_type_opt: &Option<GenericType>,
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

            let field_expected_type = self
                .try_infer_generic_param_as_expected_type(field.ty.clone(), &generic_type_opt)
                .unwrap_or(field.ty.clone());

            let field_value_ty = self.analyze_expr(scope_id_opt, &mut field_init.value, Some(field_expected_type));

            if let Some(sema_ty) = self.infer_generic_param(
                scope_id_opt,
                generic_type_opt,
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
            // validate generic type instantiation
            let final_generic_type = match generic_type.finalize(
                resolved_struct.struct_sig.generic_params.clone().unwrap(),
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
                    ResolvedSymbol::NamedStruct(struct_init.symbol_id),
                ))))
            } else {
                Some(SemanticType::ResolvedSymbol(ResolvedSymbol::NamedStruct(
                    struct_init.symbol_id,
                )))
            }
        }
    }

    fn get_func_param_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        param: &mut TypedFuncParamKind,
    ) -> Option<SemanticType> {
        match param {
            TypedFuncParamKind::FuncParam(param) => {
                let normalized = self.normalize_type(scope_id_opt, param.ty.clone(), param.loc.clone(), false)?;
                param.ty = normalized.clone();
                Some(normalized)
            }
            TypedFuncParamKind::SelfModifier(self_modifier) => {
                let normalized = self.normalize_type(
                    scope_id_opt,
                    self_modifier.ty.clone().unwrap(),
                    self_modifier.loc.clone(),
                    false,
                )?;
                Some(normalized)
            }
        }
    }

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
                    TypedFuncVariadicParams::UntypedCStyle => {
                        for arg in variadic_args.iter_mut() {
                            self.analyze_expr(scope_id_opt, arg, arg.sema_ty.clone());
                        }
                    }
                }
            }
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
            let mut param_type = match self.get_func_param_type(scope_id_opt, param) {
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            let arg_type = match self.analyze_expr(scope_id_opt, arg, Some(param_type.clone())) {
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            if let Some(sema_ty) = self.infer_generic_param(
                scope_id_opt,
                generic_type_opt,
                param_type.clone(),
                Some(arg_type.clone()),
                arg.loc.clone(),
            ) {
                param_type = sema_ty;
            }

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

        // check for duplicate parameter names
        self.check_duplicate_param_names(
            &func_sig.params.list,
            func_sig.params.variadic.as_ref(),
            DiagLoc::new(loc.clone()),
        );

        self.normalize_type(scope_id_opt, func_sig.return_type.clone(), loc, false)
    }

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
                .normalize_type(scope_id_opt, param.clone(), loc.clone(), false)
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

        if let Some(mut func_type) = operand_ty.get_const_inner().as_func_type().cloned() {
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
                let local_scope_opt =
                    scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)
                    .unwrap();

                func_sig = sym.as_func().unwrap().func_sig.clone();

                let expected_mapping_ctx = self.export_expected_generic_mapping_ctx(expected_type);

                let (_, inner_generic_type_opt) = match self.init_generic_type_with_symbol_id(
                    scope_id_opt,
                    local_scope_opt.clone(),
                    symbol_id,
                    &func_call.type_args,
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
            func_sig = substitute_func_sig(&func_sig, generic_type.mapping_ctx.clone()).unwrap();

            if let Err(diag) = generic_type.finalize(
                func_sig.generic_params.clone().unwrap(),
                (self.symbol_formatter)(scope_id_opt),
            ) {
                self.reporter.report(diag);
                return None;
            }

            if !func_sig.is_func_decl {
                // only specialize function definition which necessarily includes the body block
                func_call.monomorph_key =
                    self.register_specialized_generic_func(&func_sig, &generic_type, None, &func_call.loc);
            }

            // substitutes the func type inside of the func_call operand
            func_call.operand.sema_ty =
                substitute_type(func_call.operand.sema_ty.clone().unwrap(), generic_type.mapping_ctx);
        }

        func_call.return_type = Some(func_sig.return_type.clone());
        Some(func_sig.return_type)
    }

    fn extract_object_symbol_id<'b>(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        var_type: SemanticType,
        loc: SourceLoc,
    ) -> Option<u32> {
        self.normalize_type(scope_id_opt, var_type, loc.clone(), false)?
            .get_pure_symbol_id()
    }

    fn maybe_enum_variant_constructor_from_field_access(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
        operand_ty: SemanticType,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<SemanticType>,
    ) -> (bool, Option<SemanticType>) {
        let Some(symbol_id) = operand_ty.get_pure_symbol_id() else {
            return (false, None);
        };

        let Some(resolved_enum) = self.resolver.resolve_enum_symbol(local_scope_opt.clone(), symbol_id) else {
            return (false, None);
        };

        let attempted = true;

        field_access.object_symbol_id = Some(resolved_enum.symbol_id);

        {
            let (generic_params, mapping_ctx) = self.initial_generic_params_and_mapping_ctx(
                &operand_ty,
                resolved_enum.enum_sig.generic_params.as_ref(),
                expected_type,
            );

            let sema = self.analyze_enum_variant_no_field(
                scope_id_opt,
                local_scope_opt,
                &resolved_enum,
                field_access,
                generic_params.as_ref(),
                mapping_ctx,
            );
            (attempted, sema)
        }
    }

    fn maybe_enum_variant_constructor_from_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
        operand_ty: SemanticType,
        method_call: &mut TypedMethodCall,
        expected_type: Option<SemanticType>,
    ) -> (bool, Option<SemanticType>) {
        let Some(symbol_id) = operand_ty.get_pure_symbol_id() else {
            return (false, None);
        };

        let Some(resolved_enum) = self.resolver.resolve_enum_symbol(local_scope_opt.clone(), symbol_id) else {
            return (false, None);
        };

        let enum_variant_opt = resolved_enum
            .enum_sig
            .variants
            .iter()
            .find(|v| v.get_identifier().as_string() == method_call.method_name);

        let Some(enum_variant) = enum_variant_opt else {
            return (false, None);
        };

        method_call.object_symbol_id = Some(resolved_enum.symbol_id);

        let (generic_params, mapping_ctx) = self.initial_generic_params_and_mapping_ctx(
            &operand_ty,
            resolved_enum.enum_sig.generic_params.as_ref(),
            expected_type,
        );

        let sema = self.analyze_enum_variant(
            scope_id_opt,
            local_scope_opt,
            enum_variant.clone(),
            method_call,
            &resolved_enum,
            generic_params.as_ref(),
            mapping_ctx,
        );

        (true, sema)
    }

    fn analyze_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        method_call: &mut TypedMethodCall,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));
        let loc = method_call.loc.clone();

        self.analyze_expr_non_terminal(scope_id_opt, &mut method_call.operand, expected_type.clone());

        let mut method_call_operand_ty = method_call
            .operand
            .sema_ty
            .as_ref()
            .map(|sema_ty| sema_ty.get_const_inner())
            .cloned()?;

        // this only used to determine that, it's instance/static method call.
        let unresolved_symbol_id = method_call.operand.kind.as_symbol_id();

        if let Some(symbol_id) = unresolved_symbol_id {
            if let Some(sym) = self
                .resolver
                .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)
            {
                if self.check_unexpected_type_args(
                    &sym.get_generic_params(),
                    &method_call.type_args,
                    method_call.loc.clone(),
                ) {
                    return None;
                };

                if sym.as_variable().is_some() || sym.as_global_var().is_some() {
                    method_call.is_instance_method_operand = true;
                }
            }
        }

        if let Some(sema_ty) =
            self.merge_generic_operand_as_expected_type(method_call_operand_ty.clone(), expected_type.clone())
        {
            method_call_operand_ty = sema_ty;
        }

        {
            let (detected_as_enum_variant, sema_ty) = self.maybe_enum_variant_constructor_from_method_call(
                scope_id_opt,
                local_scope_opt.clone(),
                method_call_operand_ty.clone(),
                method_call,
                expected_type.clone(),
            );

            if detected_as_enum_variant {
                return sema_ty;
            }
        }

        let object_symbol_id = {
            let operand_ty = method_call_operand_ty
                .get_symbol_id()
                .and_then(|symbol_id| {
                    self.analyze_var_or_global_var_type(
                        scope_id_opt,
                        local_scope_opt.clone(),
                        symbol_id,
                        method_call.loc.clone(),
                    )
                })
                .or(self.analyze_expr_non_terminal(scope_id_opt, &mut method_call.operand, expected_type.clone()))
                .map(|sema_ty| sema_ty.get_const_inner().clone())?;

            match operand_ty {
                SemanticType::GenericType(generic_type) => generic_type.base,
                SemanticType::ResolvedSymbol(resolved_symbol) => resolved_symbol.get_symbol_id(),
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

        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let sym = match self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), object_symbol_id)
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
                        .get_const_inner()
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
                        .get_const_inner()
                        .clone();

                    match self.extract_object_symbol_id(scope_id_opt, var_type, loc) {
                        Some(object_id) => Some(object_id),
                        None => None,
                    }
                } else {
                    None
                }
            }
        };

        let object_id = match object_id_opt {
            Some(object_id) => object_id,
            None => {
                let symbol_name = (self.symbol_formatter)(scope_id_opt)(object_symbol_id);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::NonStructSymbol { symbol_name }),
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        self.analyze_regular_method_call(
            scope_id_opt,
            local_scope_opt,
            method_call,
            object_id,
            method_call_operand_ty,
        )
    }

    fn set_method_call_self_type(&mut self, method_call: &mut TypedMethodCall, sema_ty: &SemanticType) {
        self.current_self = Some(sema_ty.clone());
        self.current_obj_operand_ty = Some(sema_ty.clone());
        method_call.self_ty = Some(sema_ty.clone());
    }

    fn clear_method_call_self_type(&mut self) {
        self.current_self = None;
        self.current_obj_operand_ty = None;
    }

    fn analyze_regular_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
        method_call: &mut TypedMethodCall,
        object_id: SymbolID,
        method_call_operand_ty: SemanticType,
    ) -> Option<SemanticType> {
        self.set_method_call_self_type(method_call, &method_call_operand_ty);

        method_call.object_symbol_id = Some(object_id);
        let symbol_entry = self.resolver.lookup_symbol_entry_with_id(object_id).unwrap();

        let (object_name, object_methods) = {
            match symbol_entry.kind {
                SymbolEntryKind::Struct(resolved_struct) => {
                    (resolved_struct.struct_sig.name, resolved_struct.struct_sig.methods)
                }
                SymbolEntryKind::Enum(resolved_enum) => (resolved_enum.enum_sig.name, resolved_enum.enum_sig.methods),
                SymbolEntryKind::Union(resolved_union) => {
                    (resolved_union.union_sig.name, resolved_union.union_sig.methods)
                }
                _ => unreachable!(),
            }
        };

        let method_name = method_call.method_name.clone();
        let method_symbol_id = match object_methods.get(&method_name) {
            Some(symbol_id) => *symbol_id,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectMethodNotDefined {
                        struct_name: object_name.clone(),
                        method_name: method_name.clone(),
                    }),
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                self.clear_method_call_self_type();
                return None;
            }
        };

        let mut method_symbol_entry = self.resolver.lookup_symbol_entry_with_id(method_symbol_id).unwrap();

        let resolved_method = match &mut method_symbol_entry.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method,
            _ => unreachable!(),
        };

        if resolved_method.func_sig.generic_params.is_none() && method_call.type_args.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                location: Some(DiagLoc::new(method_call.loc.clone())),
                hint: None,
            });
            self.clear_method_call_self_type();
            return None;
        }

        method_call.method_symbol_id = Some(resolved_method.symbol_id);

        let first_param_opt = resolved_method.func_sig.params.list.first();

        let is_instance_method_sig = resolved_method.func_sig.is_instance_method();

        // invalid if static method called on instance
        let invalid_call = !is_instance_method_sig && method_call.is_instance_method_operand;

        if invalid_call {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::StaticMethodCallOnInstance { method_name }),
                location: Some(DiagLoc::new(method_call.loc.clone())),
                hint: Some(format!(
                    "Call it on a value of type '{}', or declare it as a static function if no instance is required.",
                    format_sema_ty(method_call_operand_ty, &(self.symbol_formatter)(scope_id_opt))
                )),
            });
            self.clear_method_call_self_type();
            return None;
        }

        if !self.validate_method_call(
            scope_id_opt,
            object_id,
            &method_call.method_name,
            method_call_operand_ty.clone(),
            method_call.is_fat_arrow,
            first_param_opt,
            object_methods,
            object_name.clone(),
            &resolved_method,
            method_call.loc.clone(),
        ) {
            self.clear_method_call_self_type();
            return None;
        }

        let instance_method_call = is_instance_method_sig && method_call.is_instance_method_operand;
        let mut generic_type_opt = method_call_operand_ty.as_generic_type().cloned();

        // init method generic mapping ctx
        let (_, mut method_generic_type_opt) = match self.init_generic_type_with_symbol_id(
            scope_id_opt,
            local_scope_opt.clone(),
            resolved_method.symbol_id,
            &method_call.type_args,
            None,
            resolved_method.func_sig.generic_params.as_ref(),
            false,
            method_call.loc.clone(),
        ) {
            Ok(opt) => opt?,
            Err(diag) => {
                self.reporter.report(diag);
                self.clear_method_call_self_type();
                return None;
            }
        };

        let method_mapping_ctx = {
            if let Some(method_generic_type) = &mut method_generic_type_opt {
                Some(method_generic_type.mapping_ctx.borrow().clone())
            } else {
                None
            }
        };

        if !method_call.is_instance_method_operand && is_instance_method_sig {
            // explicit instance method
            // inferring self type from first argument type
            if let Some(mut expr) = method_call.args.first().cloned() {
                if let Some(sema_ty) = self.analyze_expr(scope_id_opt, &mut expr, None) {
                    generic_type_opt = sema_ty.as_generic_type().cloned();

                    self.set_method_call_self_type(method_call, &sema_ty);
                    set_self_modifier_type_in_func_sig(&mut resolved_method.func_sig, &sema_ty);
                }
            }
        }

        method_call.return_type = Some(self.check_func_call(
            scope_id_opt,
            &mut resolved_method.func_sig,
            &generic_type_opt,
            &mut method_call.args,
            method_call.loc.clone(),
            instance_method_call,
        )?);

        // validate generic type instantiation
        if let Some(generic_type) = generic_type_opt {
            {
                let mut ctx = generic_type.mapping_ctx.borrow_mut();
                if let Some(method_ctx) = method_mapping_ctx {
                    let method_ctx = Rc::new(method_ctx);
                    self.mapping_ctx_arena.push(method_ctx.clone());

                    ctx.parent = Some(Rc::downgrade(&method_ctx));
                }
            }

            resolved_method.func_sig =
                substitute_func_sig(&resolved_method.func_sig, generic_type.mapping_ctx.clone()).unwrap();

            if let Some(generic_params) = resolved_method.func_sig.generic_params.clone() {
                if let Err(diag) = generic_type.finalize(generic_params, (self.symbol_formatter)(scope_id_opt)) {
                    self.reporter.report(diag);
                    self.clear_method_call_self_type();
                    return None;
                }
            }

            method_call.monomorph_key = self.register_specialized_generic_func(
                &resolved_method.func_sig,
                &generic_type,
                Some(method_call_operand_ty),
                &resolved_method.func_sig.loc,
            );

            // substitutes the func type inside of the func_call operand
            method_call.operand.sema_ty =
                substitute_type(method_call.operand.sema_ty.clone().unwrap(), generic_type.mapping_ctx);
        }

        method_call.return_type.clone()
    }

    pub(crate) fn analyze_generic_self_modifier(
        &self,
        scope_id: ScopeID,
        params: &TypedFuncParams,
        sema_ty: SemanticType,
    ) {
        let local_scope_rc = self.resolver.get_scope_ref(self.module_id, scope_id).unwrap();

        if let Some(first_param) = params.list.first() {
            if let Some(self_modifier) = first_param.as_self_modifier() {
                let mut local_scope_ref = local_scope_rc.borrow_mut();

                let new_self_modifier_ty = match self_modifier.kind {
                    SelfModifierKind::Copied => sema_ty,
                    SelfModifierKind::Referenced => SemanticType::Pointer(Box::new(sema_ty)),
                };

                local_scope_ref.with_symbol_id_mut(self_modifier.self_symbol_id.unwrap(), |local_symbol| {
                    let resolved_var = local_symbol.as_variable_mut().unwrap();
                    resolved_var.typed_variable.ty = Some(new_self_modifier_ty);
                });
                drop(local_scope_ref);
            }
        }
    }

    fn validate_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        instance_symbol_id: SymbolID,
        method_name: &String,
        method_call_operand_ty: SemanticType,
        is_fat_arrow: bool,
        first_param_opt: Option<&TypedFuncParamKind>,
        object_methods: HashMap<String, SymbolID>,
        object_name: String,
        resolved_method: &ResolvedMethod,
        loc: SourceLoc,
    ) -> bool {
        let mut result = true;
        let method_vis = &resolved_method.func_sig.modifiers.vis;

        let access_violation = if let Some(current_method_symbol_id) = self.current_method_symbol_id {
            let method_symbol_ids = object_methods.values().cloned().collect::<Vec<SymbolID>>();

            if method_symbol_ids.contains(&current_method_symbol_id) {
                false
            } else {
                !method_vis.is_public()
            }
        } else {
            !method_vis.is_public()
        };

        if access_violation {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InternalMethodCall {
                    method_name: resolved_method.func_sig.name.clone(),
                    object_name,
                }),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
            result = false;
        }

        let is_pointer = method_call_operand_ty.get_const_inner().is_pointer();
        let is_operand_const = method_call_operand_ty.is_const();
        let is_object = method_call_operand_ty.get_const_inner().is_resolved_symbol()
            || method_call_operand_ty.as_generic_type().is_some();

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

    fn analyze_array(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_array: &mut TypedArrayExpr,
    ) -> Option<SemanticType> {
        typed_array.array_type = match self.normalize_type(
            scope_id_opt,
            typed_array.array_type.clone(),
            typed_array.loc.clone(),
            false,
        ) {
            Some(sema_ty) => sema_ty,
            None => return None,
        };

        for (argument_idx, argument) in typed_array.elements.iter_mut().enumerate() {
            let argument_type = match self.analyze_expr(
                scope_id_opt,
                argument,
                Some(*typed_array.array_type.as_array_type().unwrap().element_type.clone()),
            ) {
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            let element_type = match self.normalize_type(
                scope_id_opt,
                *typed_array.array_type.as_array_type().unwrap().element_type.clone(),
                argument.loc.clone(),
                false,
            ) {
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            if !self.check_type_mismatch(scope_id_opt, argument_type.clone(), element_type, argument.loc.clone()) {
                let element_type = format_sema_ty(argument_type, &(self.symbol_formatter)(scope_id_opt));
                let expected_type = format_sema_ty(
                    *typed_array.array_type.as_array_type().unwrap().element_type.clone(),
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

        let array_type = typed_array.array_type.as_array_type().unwrap();
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
            typed_array.array_type.as_array_type().unwrap().clone(),
        ))
    }

    fn analyze_cast(&mut self, scope_id_opt: Option<ScopeID>, cast: &mut TypedCastExpr) -> Option<SemanticType> {
        let operand = match self.analyze_expr(scope_id_opt, &mut cast.operand, Some(cast.target_type.clone())) {
            Some(sema_ty) => sema_ty.get_const_inner().clone(),
            None => return None,
        };

        cast.target_type = self
            .normalize_type(scope_id_opt, cast.target_type.clone(), cast.loc.clone(), false)
            .unwrap()
            .get_const_inner()
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

    fn analyze_var_or_global_var_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
        instance_symbol_id: SymbolID,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), instance_symbol_id)
            .unwrap();

        let sema_ty = match match &sym {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => {
                let typed_variable = &local_symbol.as_variable().unwrap().typed_variable;

                match &typed_variable.ty {
                    Some(sema_ty) => self.normalize_type(scope_id_opt, sema_ty.clone(), loc.clone(), false),
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
                .normalize_type(scope_id_opt, sema_ty.unwrap(), loc.clone(), false)
                .unwrap();

            Some(normalized_type)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
enum MemberAccessKind {
    UnnamedStruct(Box<TypedUStructType>),
    NamedStruct(Box<ResolvedStruct>),
    Union(Box<ResolvedUnion>),
}

fn infer_integer_type(
    literal: &TypedLiteralExpr,
    suffix_opt: &Option<Box<TokenKind>>,
    expected: Option<SemanticType>,
) -> Result<SemanticType, Diag> {
    if let Some(suffix) = suffix_opt {
        match map_integer_suffix_to_type(&suffix) {
            Some(ty) => Ok(ty),
            None => Err(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidIntegerLiteralSuffix),
                location: Some(DiagLoc::new(literal.loc.clone())),
                hint: Some(format!("Invalid suffix {:?} for integer literal.", suffix)),
            }),
        }
    } else if let Some(ctx_ty) = expected {
        if is_integer_type(&ctx_ty) {
            Ok(ctx_ty)
        } else {
            Ok(SemanticType::PlainType(PlainType::Int)) // safe default
        }
    } else {
        Ok(SemanticType::PlainType(PlainType::Int))
    }
}

fn infer_float_type(
    literal: &TypedLiteralExpr,
    suffix_opt: &Option<Box<TokenKind>>,
    expected: Option<SemanticType>,
) -> Result<SemanticType, Diag> {
    if let Some(suffix) = suffix_opt {
        match map_float_suffix_to_type(&suffix) {
            Some(ty) => Ok(ty),
            None => Err(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidFloatLiteralSuffix),
                location: Some(DiagLoc::new(literal.loc.clone())),
                hint: Some(format!("Invalid suffix {:?} for float literal.", suffix)),
            }),
        }
    } else if let Some(ctx_ty) = expected {
        if is_float_type(&ctx_ty) {
            Ok(ctx_ty)
        } else {
            Ok(SemanticType::PlainType(PlainType::Float64)) // safe default
        }
    } else {
        Ok(SemanticType::PlainType(PlainType::Float64))
    }
}

fn map_integer_suffix_to_type(suffix: &TokenKind) -> Option<SemanticType> {
    let ty = match suffix {
        TokenKind::UIntPtr => PlainType::UIntPtr,
        TokenKind::IntPtr => PlainType::IntPtr,
        TokenKind::SizeT => PlainType::SizeT,
        TokenKind::Int => PlainType::Int,
        TokenKind::Int8 => PlainType::Int8,
        TokenKind::Int16 => PlainType::Int16,
        TokenKind::Int32 => PlainType::Int32,
        TokenKind::Int64 => PlainType::Int64,
        TokenKind::Int128 => PlainType::Int128,
        TokenKind::UInt => PlainType::UInt,
        TokenKind::UInt8 => PlainType::UInt8,
        TokenKind::UInt16 => PlainType::UInt16,
        TokenKind::UInt32 => PlainType::UInt32,
        TokenKind::UInt64 => PlainType::UInt64,
        TokenKind::UInt128 => PlainType::UInt128,
        _ => return None,
    };
    Some(SemanticType::PlainType(ty))
}

fn map_float_suffix_to_type(suffix: &TokenKind) -> Option<SemanticType> {
    let ty = match suffix {
        TokenKind::Float16 => PlainType::Float16,
        TokenKind::Float32 => PlainType::Float32,
        TokenKind::Float64 => PlainType::Float64,
        TokenKind::Float128 => PlainType::Float128,
        _ => return None,
    };
    Some(SemanticType::PlainType(ty))
}

fn is_integer_type(ty: &SemanticType) -> bool {
    matches!(
        ty,
        SemanticType::PlainType(
            PlainType::Int
                | PlainType::Int8
                | PlainType::Int16
                | PlainType::Int32
                | PlainType::Int64
                | PlainType::Int128
                | PlainType::UInt
                | PlainType::UInt8
                | PlainType::UInt16
                | PlainType::UInt32
                | PlainType::UInt64
                | PlainType::UInt128
                | PlainType::IntPtr
                | PlainType::UIntPtr
                | PlainType::SizeT
        )
    )
}

fn is_float_type(ty: &SemanticType) -> bool {
    matches!(
        ty,
        SemanticType::PlainType(PlainType::Float16 | PlainType::Float32 | PlainType::Float64 | PlainType::Float128)
    )
}
