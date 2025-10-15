use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, generic_mapping_ctx_scope};
use ast::{AccessSpecifier, LiteralKind, SelfModifierKind, StringPrefix, source_loc::SourceLoc, token::TokenKind};
use diagcentral::{Diag, DiagLevel, DiagLoc};
use partialmatch::partial_match;
use resolver::{
    scope::{
        LocalOrGlobalSymbol, LocalScopeRef, ResolvedEnum, ResolvedMethod, ResolvedStruct, ResolvedUnion,
        SymbolEntryKind,
    },
    signatures::{FuncSig, UnionSig},
    typed_func_params_as_func_type_params,
};
use std::collections::HashMap;
use typed_ast::{
    format::{format_concrete_type, format_func_type, format_typed_expr},
    types::{
        BasicConcreteType, ConcreteType, GenericType, ResolvedSymbol, TypedArrayCapacity, TypedArrayFixedCapacityValue,
        TypedArrayType, TypedFuncType, TypedTupleType, TypedUnnamedStructType, TypedUnnamedStructTypeField,
    },
    *,
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_literal_type(
        &mut self,
        typed_literal: &mut TypedLiteral,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let ty_opt = match &typed_literal.kind {
            LiteralKind::Integer(_, suffix_opt) => {
                match infer_integer_type(typed_literal, suffix_opt, expected_type.clone()) {
                    Ok(ty) => Some(ty),
                    Err(diag) => {
                        self.reporter.report(diag);
                        None
                    }
                }
            }
            LiteralKind::Float(_, suffix_opt) => {
                match infer_float_type(typed_literal, suffix_opt, expected_type.clone()) {
                    Ok(ty) => Some(ty),
                    Err(diag) => {
                        self.reporter.report(diag);
                        None
                    }
                }
            }
            LiteralKind::Bool(_) => Some(ConcreteType::BasicType(BasicConcreteType::Bool)),
            LiteralKind::Char(_) => Some(ConcreteType::BasicType(BasicConcreteType::Char)),
            LiteralKind::Null => Some(ConcreteType::BasicType(BasicConcreteType::Null)),
            LiteralKind::String(value, prefix_opt) => {
                let ty = if let Some(prefix) = prefix_opt {
                    match prefix {
                        StringPrefix::C => {
                            ConcreteType::Pointer(Box::new(ConcreteType::BasicType(BasicConcreteType::Char)))
                        }
                        StringPrefix::B => ConcreteType::Array(TypedArrayType {
                            element_type: Box::new(ConcreteType::Const(Box::new(ConcreteType::BasicType(
                                BasicConcreteType::Char,
                            )))),
                            capacity: TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(value.len())),
                            loc: typed_literal.loc.clone(),
                        }),
                    }
                } else {
                    ConcreteType::Pointer(Box::new(ConcreteType::BasicType(BasicConcreteType::Char)))
                };
                Some(ty)
            }
        };

        if let Some(ref ty) = ty_opt {
            typed_literal.ty = Some(ty.clone());
        }

        ty_opt
    }

    pub(crate) fn analyze_typed_expr_type_non_terminal(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &mut TypedExpression,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        self.apply_possible_expr_lowerings(scope_id_opt, typed_expr, expected_type.clone());

        let concrete_type = match &mut typed_expr.kind {
            TypedExpressionKind::Symbol(symbol_id, ..) => {
                let local_scope_opt =
                    scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

                let local_or_global_symbol = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, *symbol_id)?;

                self.resolve_full_type_from_local_or_global_symbol(scope_id_opt, local_or_global_symbol)
            }
            TypedExpressionKind::Literal(typed_literal) => self.analyze_literal_type(typed_literal, expected_type),
            TypedExpressionKind::Prefix(typed_prefix_expr) => {
                self.analyze_prefix_expr_type(scope_id_opt, typed_prefix_expr, expected_type)
            }
            TypedExpressionKind::Infix(typed_infix_expr) => {
                self.analyze_infix_expr_type(scope_id_opt, typed_infix_expr, expected_type)
            }
            TypedExpressionKind::Unary(typed_unary_expr) => {
                self.analyze_unary_expr_type(scope_id_opt, typed_unary_expr)
            }
            TypedExpressionKind::Assignment(typed_assignment) => {
                self.analyze_assignment(scope_id_opt, typed_assignment);
                None
            }
            TypedExpressionKind::Cast(typed_cast) => self.analyze_cast_expr_type(scope_id_opt, typed_cast),
            TypedExpressionKind::Array(typed_array) => self.analyze_array_expr_type(scope_id_opt, typed_array),
            TypedExpressionKind::ArrayIndex(typed_array_index) => {
                self.analyze_array_index_expr_type(scope_id_opt, typed_array_index)
            }
            TypedExpressionKind::AddressOf(typed_address_of) => {
                self.analyze_address_of_expr_type(scope_id_opt, typed_address_of)
            }
            TypedExpressionKind::Dereference(typed_dereference) => {
                self.analyze_dereference_expr_type(scope_id_opt, typed_dereference)
            }
            TypedExpressionKind::StructInit(struct_init) => {
                self.analyze_struct_init_expr_type(scope_id_opt, struct_init)
            }
            TypedExpressionKind::FuncCall(typed_func_call) => {
                self.analyze_func_call_expr_type(scope_id_opt, typed_func_call)
            }
            TypedExpressionKind::UnnamedStructValue(typed_unnamed_struct_value) => {
                self.analyze_unnamed_struct_value_expr_type(scope_id_opt, typed_unnamed_struct_value)
            }
            TypedExpressionKind::FieldAccess(field_access) => {
                self.analyze_field_access_type(scope_id_opt, field_access, expected_type)
            }
            TypedExpressionKind::MethodCall(method_call) => {
                self.analyze_method_call_expr_type(scope_id_opt, method_call, expected_type)
            }
            TypedExpressionKind::SizeOfExpression(typed_size_of_expression) => {
                self.analyze_sizeof_expr_type(scope_id_opt, typed_size_of_expression, expected_type)
            }
            TypedExpressionKind::Lambda(typed_lambda) => self.analyze_lambda_expr(scope_id_opt, typed_lambda),
            TypedExpressionKind::Tuple(tuple_value) => {
                self.analyze_tuple_value(scope_id_opt, tuple_value, expected_type)
            }
            TypedExpressionKind::TupleMemberAccess(tuple_member_access) => {
                self.analyze_tuple_member_access(scope_id_opt, tuple_member_access, expected_type)
            }
            TypedExpressionKind::ConcreteType(..) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidUsageOfTheConcreteType,
                    location: Some(DiagLoc::new(typed_expr.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let normalized_type = self.normalize_type(scope_id_opt, concrete_type.clone()?, typed_expr.loc.clone());
        typed_expr.concrete_type = normalized_type.clone();

        if cfg!(debug_assertions) {
            if let Some(concrete_type_clone) = typed_expr.concrete_type.clone() {
                let is_unresolved_symbol = matches!(concrete_type_clone, ConcreteType::UnresolvedSymbol(..));
                assert!(is_unresolved_symbol == false);
            }
            assert!(typed_expr.concrete_type != None);
        }

        normalized_type
    }

    pub(crate) fn analyze_typed_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &mut TypedExpression,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        partial_match!(&typed_expr.kind, {
            TypedExpressionKind::Symbol(symbol_id, _) => {
                let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

                let local_or_global_symbol = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, *symbol_id)?;

                if !local_or_global_symbol.is_kind_of_variable() {
                    let symbol_name = (self.symbol_formatter)(scope_id_opt)(*symbol_id);

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::UnknownSymbol { symbol_name },
                        location: Some(DiagLoc::new(typed_expr.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        });

        self.analyze_typed_expr_type_non_terminal(scope_id_opt, typed_expr, expected_type)
    }

    pub(crate) fn check_expr_type_must_be_condition(&mut self, concrete_type: ConcreteType, loc: SourceLoc) {
        if !concrete_type.is_bool() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ConditionExprMustBeOfTypeBool,
                location: Some(DiagLoc::new(loc)),
                hint: None,
            });
        }
    }

    fn analyze_tuple_member_access(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        tuple_member_access: &mut TypedTupleMemberAccess,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let operand_type =
            self.analyze_typed_expr_type(scope_id_opt, &mut tuple_member_access.operand, expected_type)?;
        let index_type = self.analyze_typed_expr_type(scope_id_opt, &mut tuple_member_access.index, None)?;

        if !operand_type.get_const_inner().as_tuple_type().is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand,
                location: Some(DiagLoc::new(tuple_member_access.loc.clone())),
                hint: None,
            });
            return None;
        }

        if !index_type.is_integer() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::TupleNonIntegerIndex,
                location: Some(DiagLoc::new(tuple_member_access.loc.clone())),
                hint: None,
            });
            return None;
        }

        let index_value = match self.const_expr_as_raw_integer(scope_id_opt, &tuple_member_access.index) {
            Some(index_value) => index_value,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::TupleNonIntegerIndex,
                    location: Some(DiagLoc::new(tuple_member_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let tuple_type = operand_type.as_tuple_type().unwrap();

        // inbounds check for tuple type

        if index_value > (tuple_type.type_list.len() - 1).try_into().unwrap() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::TupleIndexOutOfRange {
                    index: index_value.try_into().unwrap(),
                    length: tuple_type.type_list.len(),
                },
                location: Some(DiagLoc::new(tuple_member_access.loc.clone())),
                hint: None,
            });
            return None;
        }

        let element_type = tuple_type.type_list.get(index_value as usize).unwrap();

        Some(element_type.clone())
    }

    fn analyze_lambda_expr(&mut self, scope_id_opt: Option<ScopeID>, lambda: &mut TypedLambda) -> Option<ConcreteType> {
        let current_func_clone = self.current_func.clone();

        self.normalize_func_params(&mut lambda.params, lambda.loc.clone());
        lambda.return_type = self.normalize_type(scope_id_opt, lambda.return_type.clone(), lambda.loc.clone())?;
        let params = typed_func_params_as_func_type_params(&lambda.params);
        let func_type = TypedFuncType {
            def_module_id: Some(self.module_id),
            params,
            return_type: Box::new(lambda.return_type.clone()),
            vis_opt: None,
            loc: lambda.loc.clone(),
        };

        self.current_func = Some(func_type.clone());
        self.analyze_block_statement(&mut lambda.body);

        self.current_func = current_func_clone;
        Some(ConcreteType::FuncType(func_type))
    }

    fn analyze_tuple_value(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        tuple_value: &mut TypedTupleValue,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let mut type_list: Vec<ConcreteType> = Vec::new();

        let tuple_type_opt = match expected_type {
            Some(concrete_type) => concrete_type.as_tuple_type().cloned(),
            None => None,
        };

        for (idx, expr) in &mut tuple_value.expr_list.iter_mut().enumerate() {
            let mut expected_type: Option<ConcreteType> = None;
            if let Some(tuple_type) = &tuple_type_opt {
                expected_type = tuple_type.type_list.get(idx).cloned();
            }

            match self.analyze_typed_expr_type(scope_id_opt, expr, expected_type) {
                Some(concrete_type) => type_list.push(concrete_type),
                None => continue,
            }
        }

        Some(ConcreteType::Tuple(TypedTupleType {
            type_list,
            loc: tuple_value.loc.clone(),
        }))
    }

    fn analyze_unnamed_struct_value_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_struct_value: &mut TypedUnnamedStructValue,
    ) -> Option<ConcreteType> {
        let mut fields: Vec<TypedUnnamedStructTypeField> = Vec::new();

        for field in &mut unnamed_struct_value.fields {
            let field_value_type =
                match self.analyze_typed_expr_type(scope_id_opt, &mut field.field_value, field.field_type.clone()) {
                    Some(concrete_type) => concrete_type,
                    None => continue,
                };

            fields.push(TypedUnnamedStructTypeField {
                field_name: field.field_name.clone(),
                field_type: Box::new(field.field_type.clone().unwrap_or(field_value_type)),
                loc: field.loc.clone(),
            });
        }

        let unnamed_struct_type = TypedUnnamedStructType {
            fields,
            packed: unnamed_struct_value.packed,
            loc: unnamed_struct_value.loc.clone(),
        };

        unnamed_struct_value.unnamed_struct_type = Some(unnamed_struct_type.clone());

        if unnamed_struct_value.is_const {
            Some(ConcreteType::Const(Box::new(ConcreteType::UnnamedStruct(
                unnamed_struct_type,
            ))))
        } else {
            Some(ConcreteType::UnnamedStruct(unnamed_struct_type))
        }
    }

    fn analyze_array_index_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        array_index: &mut TypedArrayIndex,
    ) -> Option<ConcreteType> {
        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut array_index.operand, None) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let is_operand_array = operand_type.is_array();

        if !(operand_type.is_pointer() || is_operand_array) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ArrayIndexOnNonArrayOperand,
                location: Some(DiagLoc::new(array_index.loc.clone())),
                hint: None,
            });
            return None;
        }

        let index_inner_type = array_index.index.concrete_type.clone();
        let index_concrete_type =
            match self.analyze_typed_expr_type(scope_id_opt, &mut array_index.index, index_inner_type) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        if !index_concrete_type
            .get_const_inner()
            .as_basic_type()
            .and_then(|b| Some(b.is_integer()))
            .is_some()
        {
            let found_type = format_concrete_type(index_concrete_type, &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ArrayNonIntegerIndex { found_type },
                location: Some(DiagLoc::new(array_index.loc.clone())),
                hint: None,
            });
            return None;
        }

        let concrete_type = array_index.operand.concrete_type.clone().unwrap();

        if is_operand_array {
            let array_type = concrete_type.as_array_type().unwrap();
            Some(*array_type.element_type.clone())
        } else {
            // array index on pointer operand
            let element_type = concrete_type.get_pointer_inner().unwrap();

            if element_type.is_void() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DerefVoidPointerValue,
                    location: Some(DiagLoc::new(array_index.loc.clone())),
                    hint: None,
                });
                return None;
            }

            Some(element_type.clone())
        }
    }

    fn validate_field_access(
        &mut self,
        operand: &TypedExpression,
        field_access: &TypedFieldAccess,
        field_vis: AccessSpecifier,
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
                kind: AnalyzerDiagKind::InternalFieldAccess {
                    field_name: field_access.field_name.clone(),
                    struct_name: struct_name.to_string(),
                },
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
            });
            result = false;
        }

        let base_type = operand
            .concrete_type
            .as_ref()
            .expect("ConcreteType should be set before field access")
            .get_const_inner();

        let is_pointer = base_type.is_pointer() || base_type.as_generic_type().is_some();
        let is_struct = base_type.is_resolved_symbol() || base_type.as_generic_type().is_some();

        if field_access.is_fat_arrow {
            if !is_pointer {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidFatArrow,
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: Some("Use '.' instead of '->'.".to_string()),
                });
                result = false;
            }
        } else {
            if !is_struct {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::UseFatArrow,
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: Some("Use '->' when accessing through a pointer.".to_string()),
                });
                result = false;
            }
        }

        result
    }

    fn validate_union_field_access(&mut self, operand_ty: ConcreteType, field_access: &TypedFieldAccess) -> bool {
        let mut result = true;

        if operand_ty.is_pointer() && !field_access.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::UseFatArrow,
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
            });
            result = false;
        } else if !operand_ty.is_pointer() && field_access.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidFatArrow,
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: Some("Use '.' instead of '->'.".to_string()),
            });
            result = false;
        }

        result
    }

    fn analyze_union_field_access_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        union_sig: &UnionSig,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut field_access.operand, expected_type) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        field_access.operand.concrete_type = Some(operand_type.clone());

        let union_field_idx = match union_sig
            .fields
            .iter()
            .position(|field| *field.name == field_access.field_name.clone())
        {
            Some(union_field) => union_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: union_sig.name.clone(),
                        field_name: field_access.field_name.clone(),
                    },
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let union_field = &union_sig.fields[union_field_idx];
        let union_field_type = match self.normalize_type(scope_id_opt, union_field.ty.clone(), field_access.loc.clone())
        {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        if !self.validate_union_field_access(operand_type.get_const_inner().clone(), &field_access) {
            return None;
        }

        if field_access.is_fat_arrow {
            field_access.operand = Box::new(TypedExpression {
                kind: TypedExpressionKind::Dereference(TypedDereference {
                    operand: field_access.operand.clone(),
                    loc: field_access.loc.clone(),
                }),
                concrete_type: None,
                value_category: ValueCategory::Lvalue,
                loc: field_access.loc.clone(),
            });
        }

        field_access.field_index = Some(union_field_idx);
        field_access.field_ty = Some(union_field_type);
        field_access.object_symbol_id = Some(union_sig.symbol_id);

        Some(union_field.ty.clone())
    }

    fn analyze_unnamed_struct_field_access_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_struct_type: &TypedUnnamedStructType,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut field_access.operand, expected_type) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        field_access.operand.concrete_type = Some(operand_type.clone());

        let field_idx = match unnamed_struct_type
            .fields
            .iter()
            .position(|field| *field.field_name == field_access.field_name.clone())
        {
            Some(union_field) => union_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: format_concrete_type(
                            ConcreteType::UnnamedStruct(unnamed_struct_type.clone()),
                            &(self.symbol_formatter)(scope_id_opt),
                        ),
                        field_name: field_access.field_name.clone(),
                    },
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let field = &unnamed_struct_type.fields[field_idx];
        let field_type = match self.normalize_type(scope_id_opt, *field.field_type.clone(), field_access.loc.clone()) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        field_access.field_index = Some(field_idx);
        field_access.field_ty = Some(field_type.clone());

        Some(field_type.clone())
    }

    fn analyze_struct_field_access_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        field_access: &mut TypedFieldAccess,
        struct_name: String,
        struct_fields: Vec<TypedStructField>,
        struct_methods: HashMap<String, SymbolID>,
        struct_symbol_id: SymbolID,
    ) -> Option<ConcreteType> {
        let field_index = match struct_fields
            .iter()
            .position(|typed_struct_field| typed_struct_field.name == field_access.field_name)
        {
            Some(typed_struct_field) => typed_struct_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name,
                        field_name: field_access.field_name.clone(),
                    },
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let mut typed_struct_field = struct_fields.get(field_index).unwrap().clone();
        typed_struct_field.ty = self
            .normalize_type(scope_id_opt, typed_struct_field.ty.clone(), field_access.loc.clone())
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

    fn analyze_enum_variant(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        enum_symbol_id: SymbolID,
        enum_variant: &TypedEnumVariant,
        method_call: &mut TypedMethodCall,
        resolved_enum: &ResolvedEnum,
    ) -> Option<ConcreteType> {
        let valued_fields = match enum_variant {
            TypedEnumVariant::Variant(_, valued_fields) => {
                if valued_fields.len() != method_call.args.len() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::EnumVariantArgCountMismatch {
                            variant_name: method_call.method_name.clone(),
                            expected: valued_fields.len() as u32,
                            provided: method_call.args.len() as u32,
                        },
                        location: Some(DiagLoc::new(method_call.loc.clone())),
                        hint: None,
                    });
                    return None;
                }

                valued_fields
            }
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::EnumVariantDoesNotAcceptFields {
                        variant_name: method_call.method_name.clone(),
                    },
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let operand_ty = method_call.operand.concrete_type.clone().unwrap();
        let generic_type_opt = operand_ty.as_generic_type();

        let mut enum_sig = resolved_enum.enum_sig.clone();
        self.substitute_enum_type_args(scope_id_opt, &mut enum_sig, generic_type_opt, method_call.loc.clone());
        self.substitute_field_access_type(
            &mut method_call.operand,
            &enum_sig.generic_params,
            generic_type_opt,
            method_call.loc.clone(),
        );

        generic_mapping_ctx_scope!(
            self,
            resolved_enum.enum_sig.generic_params,
            method_call.type_args,
            method_call.loc.clone(),
            generic_mapping_ctx,
            {
                for (typed_expr, enum_valued_field) in method_call.args.iter_mut().zip(valued_fields) {
                    typed_expr.concrete_type = self.substitute_type_or_infer_with(
                        scope_id_opt,
                        enum_valued_field.field_type.clone(),
                        typed_expr,
                        &mut generic_mapping_ctx,
                    );
                }

                if let Some(type_args) = self.normalize_type_args_and_register(
                    enum_symbol_id,
                    &resolved_enum.enum_sig.generic_params,
                    &generic_mapping_ctx,
                ) {
                    method_call.type_args = Some(self.inferred_types_as_positional_type_args(type_args));
                }
            }
        );

        if let Some(type_args) = &method_call.type_args {
            Some(ConcreteType::GenericType(GenericType {
                base: resolved_enum.symbol_id,
                type_args: type_args.clone(),
                is_const: false,
            }))
        } else {
            Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(enum_symbol_id)))
        }
    }

    fn analyze_enum_variant_no_field(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        enum_symbol_id: SymbolID,
        field_access: &TypedFieldAccess,
    ) -> Option<ConcreteType> {
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, enum_symbol_id)
            .unwrap();
        let resolved_enum = local_or_global_symbol.as_enum().unwrap();

        let enum_variant = match resolved_enum
            .enum_sig
            .variants
            .iter()
            .find(|variant| variant.get_identifier().as_string() == field_access.field_name)
        {
            Some(enum_variant) => enum_variant,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::VariantNotDefinedForEnum {
                        enum_name: resolved_enum.enum_sig.name.clone(),
                        variant_name: field_access.field_name.clone(),
                    },
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        if matches!(enum_variant, TypedEnumVariant::Variant(..)) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::VariantMissingFields {
                    enum_name: resolved_enum.enum_sig.name.clone(),
                    variant_name: field_access.field_name.clone(),
                },
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
            });
            return None;
        }

        Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(
            resolved_enum.symbol_id,
        )))
    }

    fn resolve_member_access_kind(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
        operand: &mut TypedExpression,
        expected_type: Option<ConcreteType>,
        loc: SourceLoc,
    ) -> Option<MemberAccessKind> {
        let operand_type = match &operand.kind {
            TypedExpressionKind::Symbol(instance_symbol_id, ..) => {
                let resolved_var_type = match self.analyze_var_or_global_var_type(
                    scope_id_opt,
                    local_scope_opt.clone(),
                    *instance_symbol_id,
                    loc.clone(),
                ) {
                    Some(concrete_type) => concrete_type,
                    None => return None,
                };

                resolved_var_type.clone()
            }
            _ => match self.analyze_typed_expr_type(scope_id_opt, operand, expected_type.clone()) {
                Some(concrete_type) => concrete_type,
                None => return None,
            },
        };

        let object_symbol_id = match match operand_type.get_const_inner() {
            ConcreteType::ResolvedSymbol(resolved_symbol) => Some(resolved_symbol.get_symbol_id()),
            ConcreteType::Pointer(concrete_type) => {
                if concrete_type.is_void() {
                    return None;
                } else if let Some(unnamed_struct_type) = concrete_type.as_unnamed_struct() {
                    return Some(MemberAccessKind::UnnamedStruct(Box::new(unnamed_struct_type)));
                }

                self.extract_object_symbol_id(scope_id_opt, *concrete_type.clone(), loc.clone())
            }
            ConcreteType::UnnamedStruct(unnamed_struct_type) => {
                return Some(MemberAccessKind::UnnamedStruct(Box::new(unnamed_struct_type.clone())));
            }
            ConcreteType::GenericType(generic_type) => Some(generic_type.base),
            _ => None,
        } {
            Some(symbol_id) => symbol_id,
            None => return None,
        };

        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let local_or_global_symbol = match self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, object_symbol_id)
        {
            Some(local_or_global_symbol) => local_or_global_symbol,
            None => return None,
        };

        local_or_global_symbol
            .as_struct()
            .map(|resolved_struct| MemberAccessKind::NamedStruct(Box::new(resolved_struct.clone())))
            .or_else(|| {
                local_or_global_symbol
                    .as_union()
                    .map(|resolved_union| MemberAccessKind::Union(Box::new(resolved_union.clone())))
            })
            .or_else(|| {
                local_or_global_symbol
                    .as_enum()
                    .map(|resolved_enum| MemberAccessKind::Enum(Box::new(resolved_enum.clone())))
            })
    }

    fn analyze_field_access_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        macro_rules! not_supports_fields {
            ($this:expr, $loc:expr) => {{
                $this.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                    location: Some(DiagLoc::new($loc)),
                    hint: None,
                });
                return None;
            }};
        }

        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let concrete_type =
            self.analyze_typed_expr_type(scope_id_opt, &mut field_access.operand, expected_type.clone())?;
        field_access.operand.concrete_type = Some(concrete_type.get_const_inner().clone());
        let operand_ty = concrete_type.get_const_inner();

        // multiplex field access

        let generic_type_opt = operand_ty.as_generic_type();

        match self.resolve_member_access_kind(
            scope_id_opt,
            local_scope_opt.clone(),
            &mut field_access.operand,
            expected_type.clone(),
            field_access.loc.clone(),
        ) {
            Some(member_access_kind) => match member_access_kind {
                MemberAccessKind::UnnamedStruct(unnamed_struct_type) => self.analyze_unnamed_struct_field_access_type(
                    scope_id_opt,
                    &unnamed_struct_type,
                    field_access,
                    expected_type.clone(),
                ),
                MemberAccessKind::NamedStruct(resolved_struct) => {
                    let mut struct_sig = resolved_struct.struct_sig.clone();
                    self.substitute_struct_type_args(&mut struct_sig, generic_type_opt, field_access.loc.clone());
                    self.substitute_field_access_type(
                        &mut field_access.operand,
                        &struct_sig.generic_params,
                        generic_type_opt,
                        field_access.loc.clone(),
                    );
                    self.analyze_struct_field_access_type(
                        scope_id_opt,
                        field_access,
                        struct_sig.name.clone(),
                        struct_sig.fields.clone(),
                        struct_sig.methods.clone(),
                        resolved_struct.symbol_id,
                    )
                }
                MemberAccessKind::Enum(resolved_enum) => {
                    self.analyze_enum_variant_no_field(local_scope_opt.clone(), resolved_enum.symbol_id, &field_access)
                }
                MemberAccessKind::Union(resolved_union) => {
                    let mut union_sig = resolved_union.union_sig.clone();
                    self.substitute_union_type_args(&mut union_sig, generic_type_opt, field_access.loc.clone());
                    self.substitute_field_access_type(
                        &mut field_access.operand,
                        &union_sig.generic_params,
                        generic_type_opt,
                        field_access.loc.clone(),
                    );
                    self.analyze_union_field_access_type(scope_id_opt, &union_sig, field_access, expected_type)
                }
            },
            None => not_supports_fields!(self, field_access.loc.clone()),
        }
    }

    fn analyze_union_init_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        union_symbol_id: SymbolID,
        struct_init: &mut TypedStructInit,
    ) -> Option<ConcreteType> {
        if struct_init.fields.len() > 1 || struct_init.fields.len() == 0 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::UnionInitWithInvalidFields,
                location: Some(DiagLoc::new(struct_init.loc.clone())),
                hint: None,
            });
            return None;
        }

        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, union_symbol_id)
            .unwrap();

        let resolved_union = match local_or_global_symbol.as_union() {
            Some(resolved_union) => resolved_union,
            None => {
                let symbol_name = format_concrete_type(
                    ConcreteType::UnresolvedSymbol(union_symbol_id),
                    &(self.symbol_formatter)(scope_id_opt),
                );

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::NonUnionSymbol { symbol_name },
                    location: Some(DiagLoc::new(struct_init.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let field_init = &mut struct_init.fields[0];
        let field = match resolved_union
            .union_sig
            .fields
            .iter()
            .find(|field| field.name == field_init.name)
        {
            Some(union_field) => union_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: resolved_union.union_sig.name.clone(),
                        field_name: field_init.name.clone(),
                    },
                    location: Some(DiagLoc::new(struct_init.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        generic_mapping_ctx_scope!(
            self,
            resolved_union.union_sig.generic_params,
            struct_init.type_args,
            struct_init.loc.clone(),
            generic_mapping_ctx,
            {
                self.substitute_type_or_infer_with(
                    scope_id_opt,
                    field.ty.clone(),
                    &mut field_init.value,
                    &mut generic_mapping_ctx,
                )?;

                if let Some(type_args) = self.normalize_type_args_and_register(
                    resolved_union.symbol_id,
                    &resolved_union.union_sig.generic_params,
                    &generic_mapping_ctx,
                ) {
                    struct_init.type_args = Some(self.inferred_types_as_positional_type_args(type_args));
                }
            }
        );

        let pure_union_type = if struct_init.is_const {
            ConcreteType::Const(Box::new(ConcreteType::ResolvedSymbol(ResolvedSymbol::Union(
                struct_init.symbol_id,
            ))))
        } else {
            ConcreteType::ResolvedSymbol(ResolvedSymbol::Union(struct_init.symbol_id))
        };

        if let Some(type_args) = &struct_init.type_args {
            Some(ConcreteType::GenericType(GenericType {
                base: struct_init.symbol_id,
                type_args: type_args.clone(),
                is_const: struct_init.is_const,
            }))
        } else {
            Some(pure_union_type)
        }
    }

    fn analyze_struct_init_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        struct_init: &mut TypedStructInit,
    ) -> Option<ConcreteType> {
        let normalized = self
            .normalize_type(
                scope_id_opt,
                ConcreteType::UnresolvedSymbol(struct_init.symbol_id),
                struct_init.loc.clone(),
            )
            .unwrap();

        let struct_symbol_id = match normalized.get_const_inner().as_struct_symbol_id() {
            Some(symbol_id) => symbol_id,
            None => {
                if let Some(union_symbol_id) = normalized.as_union_symbol_id() {
                    return self.analyze_union_init_expr_type(scope_id_opt, union_symbol_id, struct_init);
                } else {
                    let symbol_name = format_concrete_type(normalized, &(self.symbol_formatter)(scope_id_opt));

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::NonStructSymbol { symbol_name },
                        location: Some(DiagLoc::new(struct_init.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        };

        let resolved_struct = self
            .resolve_symbol_as_struct(scope_id_opt, struct_symbol_id, struct_init.loc.clone())
            .unwrap();

        // check duplicate field inits
        let mut field_names: Vec<String> = Vec::new();
        for field_init in &struct_init.fields {
            let struct_name = (self.symbol_formatter)(scope_id_opt)(struct_init.symbol_id);

            if field_names.contains(&field_init.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DuplicateFieldName {
                        object_name: struct_name,
                        field_name: field_init.name.clone(),
                    },
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

        generic_mapping_ctx_scope!(
            self,
            resolved_struct.struct_sig.generic_params,
            struct_init.type_args,
            struct_init.loc.clone(),
            generic_mapping_ctx,
            {
                for field_init in &mut struct_init.fields {
                    let field = match resolved_struct
                        .struct_sig
                        .fields
                        .iter()
                        .find(|field| field.name == field_init.name)
                    {
                        Some(field) => field,
                        None => {
                            let struct_name = (self.symbol_formatter)(scope_id_opt)(struct_init.symbol_id);

                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: AnalyzerDiagKind::ObjectHasNoFieldNamed {
                                    struct_name,
                                    field_name: field_init.name.clone(),
                                },
                                location: Some(DiagLoc::new(field_init.loc.clone())),
                                hint: None,
                            });
                            continue;
                        }
                    };

                    self.substitute_type_or_infer_with(
                        scope_id_opt,
                        field.ty.clone(),
                        &mut field_init.value,
                        &mut generic_mapping_ctx,
                    )?;

                    let missing_fields_idx = match missing_fields
                        .iter()
                        .position(|field_name| *field_name == field_init.name.clone())
                    {
                        Some(idx) => idx,
                        None => continue,
                    };

                    missing_fields.remove(missing_fields_idx);

                    if let Some(type_args) = self.normalize_type_args_and_register(
                        resolved_struct.symbol_id,
                        &resolved_struct.struct_sig.generic_params,
                        &generic_mapping_ctx,
                    ) {
                        struct_init.type_args = Some(self.inferred_types_as_positional_type_args(type_args));
                    }
                }
            }
        );

        if !missing_fields.is_empty() {
            let struct_name = (self.symbol_formatter)(scope_id_opt)(struct_init.symbol_id);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::StructMissingFields {
                    struct_name,
                    missing_field_names: missing_fields,
                },
                location: Some(DiagLoc::new(struct_init.loc.clone())),
                hint: None,
            });
        }

        struct_init.symbol_id = normalized.as_struct_symbol_id().unwrap();

        let pure_struct_type = if struct_init.is_const {
            ConcreteType::Const(Box::new(ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(
                struct_init.symbol_id,
            ))))
        } else {
            ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(struct_init.symbol_id))
        };

        if let Some(type_args) = &struct_init.type_args {
            Some(ConcreteType::GenericType(GenericType {
                base: struct_init.symbol_id,
                type_args: type_args.clone(),
                is_const: struct_init.is_const,
            }))
        } else {
            Some(pure_struct_type)
        }
    }

    fn check_func_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_sig: &mut FuncSig,
        args: &mut Vec<TypedExpression>,
        loc: SourceLoc,
        instance_method_call: bool,
    ) -> Option<ConcreteType> {
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
                kind: AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: expected_args_len as u32,
                    func_name: func_sig.name.clone(),
                },
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
                            if let Some(arg_type) =
                                self.analyze_typed_expr_type(scope_id_opt, arg, arg.concrete_type.clone())
                            {
                                if !self.check_type_mismatch(
                                    scope_id_opt,
                                    arg_type.clone(),
                                    variadic_param_type.clone(),
                                    arg.loc.clone(),
                                ) {
                                    self.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                            param_type: format_concrete_type(
                                                variadic_param_type.clone(),
                                                &(self.symbol_formatter)(scope_id_opt),
                                            ),
                                            argument_type: format_concrete_type(
                                                arg_type,
                                                &(self.symbol_formatter)(scope_id_opt),
                                            ),
                                            argument_idx: (idx + static_params_len) as u32,
                                        },
                                        location: Some(DiagLoc::new(loc.clone())),
                                        hint: None,
                                    });
                                }
                            }
                        }
                    }
                    TypedFuncVariadicParams::UntypedCStyle => {
                        for arg in variadic_args.iter_mut() {
                            self.analyze_typed_expr_type(scope_id_opt, arg, arg.concrete_type.clone());
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
            let param_type = match param {
                TypedFuncParamKind::FuncParam(p) => {
                    let normalized = self.normalize_type(scope_id_opt, p.ty.clone(), p.loc.clone()).unwrap();
                    p.ty = normalized.clone();
                    normalized
                }
                TypedFuncParamKind::SelfModifier(s) => {
                    let normalized = self
                        .normalize_type(scope_id_opt, s.ty.clone().unwrap(), s.loc.clone())
                        .unwrap();
                    s.ty = Some(match s.kind {
                        SelfModifierKind::Copied => normalized.clone(),
                        SelfModifierKind::Referenced => ConcreteType::Pointer(Box::new(normalized.clone())),
                    });
                    s.ty.clone().unwrap()
                }
            };

            let arg_type = match self.analyze_typed_expr_type(scope_id_opt, arg, Some(param_type.clone())) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            if !self.check_type_mismatch(scope_id_opt, arg_type.clone(), param_type.clone(), arg.loc.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type: format_concrete_type(param_type.clone(), &(self.symbol_formatter)(scope_id_opt)),
                        argument_type: format_concrete_type(arg_type, &(self.symbol_formatter)(scope_id_opt)),
                        argument_idx: param_idx as u32,
                    },
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
            }
        }

        // check for duplicate parameter names
        self.check_duplicate_param_names(
            &func_sig.params.list,
            func_sig.params.variadic.as_ref(),
            DiagLoc::new(loc),
        );

        Some(func_sig.return_type.clone())
    }

    fn check_func_type_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_type: &mut TypedFuncType,
        args: &mut Vec<TypedExpression>,
        loc: SourceLoc,
    ) -> Option<ConcreteType> {
        let is_variadic = func_type.params.variadic.is_some();
        let expected_args_len = func_type.params.list.len();
        let func_name = format_func_type(func_type, &(self.symbol_formatter)(scope_id_opt));

        // check argument count
        if (!is_variadic && args.len() != expected_args_len) || (is_variadic && args.len() < expected_args_len) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: expected_args_len as u32,
                    func_name,
                },
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
                            if let Some(arg_type) =
                                self.analyze_typed_expr_type(scope_id_opt, arg, arg.concrete_type.clone())
                            {
                                if !self.check_type_mismatch(
                                    scope_id_opt,
                                    arg_type.clone(),
                                    variadic_param_type.clone(),
                                    arg.loc.clone(),
                                ) {
                                    self.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                            param_type: format_concrete_type(
                                                variadic_param_type.clone(),
                                                &(self.symbol_formatter)(scope_id_opt),
                                            ),
                                            argument_type: format_concrete_type(
                                                arg_type,
                                                &(self.symbol_formatter)(scope_id_opt),
                                            ),
                                            argument_idx: (idx + static_params_len) as u32,
                                        },
                                        location: Some(DiagLoc::new(loc.clone())),
                                        hint: None,
                                    });
                                }
                            }
                        }
                    }
                    TypedFuncTypeVariadicParams::UntypedCStyle => {
                        for arg in variadic_args.iter_mut() {
                            self.analyze_typed_expr_type(scope_id_opt, arg, arg.concrete_type.clone());
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
            let param_type = self.normalize_type(scope_id_opt, param.clone(), loc.clone()).unwrap();

            let arg_type = match self.analyze_typed_expr_type(scope_id_opt, arg, Some(param_type.clone())) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            if !self.check_type_mismatch(scope_id_opt, arg_type.clone(), param_type.clone(), arg.loc.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type: format_concrete_type(param_type.clone(), &(self.symbol_formatter)(scope_id_opt)),
                        argument_type: format_concrete_type(arg_type, &(self.symbol_formatter)(scope_id_opt)),
                        argument_idx: param_idx as u32,
                    },
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
            }
        }

        Some(*func_type.return_type.clone())
    }

    fn analyze_func_call_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_call: &mut TypedFuncCall,
    ) -> Option<ConcreteType> {
        let operand_ty = self.analyze_typed_expr_type_non_terminal(scope_id_opt, &mut func_call.operand, None)?;

        if let Some(mut func_type) = operand_ty.as_func_type().cloned() {
            if let Some(vis) = &func_type.vis_opt {
                if vis.is_private() && func_type.def_module_id != Some(self.module_id) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::PrivateFunctionCall {
                            name: format_typed_expr(&func_call.operand, &(self.symbol_formatter)(scope_id_opt)),
                        },
                        location: Some(DiagLoc::new(func_call.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }

            self.normalize_func_type_params(&mut func_type.params, func_call.loc.clone());

            let return_type =
                self.check_func_type_call(scope_id_opt, &mut func_type, &mut func_call.args, func_call.loc.clone());

            return_type
        } else {
            let symbol_name = format_concrete_type(operand_ty, &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::NonFunctionSymbol { symbol_name },
                location: Some(DiagLoc::new(func_call.loc.clone())),
                hint: None,
            });
            return None;
        }
    }

    fn extract_object_symbol_id<'b>(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        var_type: ConcreteType,
        loc: SourceLoc,
    ) -> Option<u32> {
        let normalized = self.normalize_type(scope_id_opt, var_type, loc.clone())?;

        match normalized {
            ConcreteType::ResolvedSymbol(resolved_symbol) => Some(resolved_symbol.get_symbol_id()),
            ConcreteType::Pointer(concrete_type) => self.extract_object_symbol_id(scope_id_opt, *concrete_type, loc),
            _ => None,
        }
    }

    fn analyze_method_call_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        method_call: &mut TypedMethodCall,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let method_name = method_call.method_name.clone();
        let loc = method_call.loc.clone();

        method_call.operand.concrete_type = match self.analyze_typed_expr_type_non_terminal(
            scope_id_opt,
            &mut method_call.operand,
            expected_type.clone(),
        ) {
            Some(concrete_type) => Some(concrete_type),
            None => return None,
        };

        if let Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(enum_symbol_id))) =
            method_call.operand.concrete_type
        {
            let local_scope_opt =
                scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

            let local_or_global_symbol = self
                .resolver
                .resolve_local_or_global_symbol(local_scope_opt.clone(), enum_symbol_id)
                .unwrap();
            let resolved_enum = local_or_global_symbol.as_enum().unwrap();

            let enum_variant_opt = resolved_enum
                .enum_sig
                .variants
                .iter()
                .find(|variant| variant.get_identifier().as_string() == method_call.method_name);

            method_call.object_symbol_id = Some(resolved_enum.symbol_id);

            if let Some(enum_variant) = enum_variant_opt {
                return self.analyze_enum_variant(
                    scope_id_opt,
                    enum_symbol_id,
                    enum_variant,
                    method_call,
                    resolved_enum,
                );
            }
        }

        let object_symbol_id = {
            let operand_type = match &method_call.operand.kind {
                TypedExpressionKind::Symbol(instance_symbol_id, ..) => {
                    let local_scope_opt =
                        scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

                    self.mark_local_symbol_used_once(
                        local_scope_opt.clone().unwrap(),
                        self.module_id,
                        *instance_symbol_id,
                    );

                    if let Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(..))) =
                        method_call.operand.concrete_type
                    {
                        method_call.operand.concrete_type.clone().unwrap()
                    } else {
                        let resolved_var_type = self
                            .analyze_var_or_global_var_type(
                                scope_id_opt,
                                local_scope_opt.clone(),
                                *instance_symbol_id,
                                method_call.loc.clone(),
                            )
                            .unwrap();

                        resolved_var_type
                    }
                }
                _ => {
                    match self.analyze_typed_expr_type_non_terminal(
                        scope_id_opt,
                        &mut method_call.operand,
                        expected_type.clone(),
                    ) {
                        Some(concrete_type) => concrete_type,
                        None => return None,
                    }
                }
            };

            match operand_type.get_const_inner() {
                ConcreteType::ResolvedSymbol(resolved_symbol) => resolved_symbol.get_symbol_id(),
                ConcreteType::Pointer(concrete_type) => {
                    if concrete_type.is_void() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                            location: Some(DiagLoc::new(method_call.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }

                    self.extract_object_symbol_id(scope_id_opt, *concrete_type.clone(), loc.clone())
                        .unwrap()
                }
                _ => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                        location: Some(DiagLoc::new(method_call.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        };

        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let local_or_global_symbol = match self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, object_symbol_id)
        {
            Some(local_or_global_symbol) => local_or_global_symbol,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let struct_ids_opt = {
            if let Some(resolved_struct) = local_or_global_symbol.as_struct() {
                // static method call
                Some((resolved_struct.module_id, resolved_struct.symbol_id))
            } else if let Some(resolved_enum) = local_or_global_symbol.as_enum() {
                Some((resolved_enum.module_id, resolved_enum.symbol_id))
            } else if let Some(resolved_union) = local_or_global_symbol.as_union() {
                Some((resolved_union.module_id, resolved_union.symbol_id))
            } else {
                // instance method call
                if let Some(resolved_var) = local_or_global_symbol.as_variable() {
                    let var_type = resolved_var
                        .typed_variable
                        .ty
                        .clone()
                        .unwrap_or({
                            self.analyze_typed_expr_type(
                                scope_id_opt,
                                &mut resolved_var.typed_variable.rhs.unwrap(),
                                expected_type.clone(),
                            )
                            .unwrap()
                        })
                        .get_const_inner()
                        .clone();

                    match self.extract_object_symbol_id(scope_id_opt, var_type, loc) {
                        Some(struct_id) => Some((resolved_var.module_id, struct_id)),
                        None => None,
                    }
                } else if let Some(resolved_global_var) = local_or_global_symbol.as_global_var() {
                    let var_type = resolved_global_var.global_var_sig.ty.unwrap().get_const_inner().clone();

                    match self.extract_object_symbol_id(scope_id_opt, var_type, loc) {
                        Some(struct_id) => Some((resolved_global_var.module_id, struct_id)),
                        None => None,
                    }
                } else {
                    None
                }
            }
        };

        let (module_id, struct_id) = match struct_ids_opt {
            Some((module_id, struct_id)) => (module_id, struct_id),
            None => {
                let symbol_name = (self.symbol_formatter)(scope_id_opt)(object_symbol_id);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::NonStructSymbol { symbol_name },
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        method_call.object_symbol_id = Some(struct_id);
        let symbol_entry = self.resolver.lookup_symbol_entry_with_id(module_id, struct_id).unwrap();

        let (object_name, object_methods, object_module_id) = {
            match symbol_entry.kind {
                SymbolEntryKind::Struct(resolved_struct) => (
                    resolved_struct.struct_sig.name,
                    resolved_struct.struct_sig.methods,
                    resolved_struct.module_id,
                ),
                SymbolEntryKind::Enum(resolved_enum) => (
                    resolved_enum.enum_sig.name,
                    resolved_enum.enum_sig.methods,
                    resolved_enum.module_id,
                ),
                SymbolEntryKind::Union(resolved_union) => (
                    resolved_union.union_sig.name,
                    resolved_union.union_sig.methods,
                    resolved_union.module_id,
                ),
                _ => unreachable!(),
            }
        };

        let method_symbol_id = match object_methods.get(&method_name) {
            Some(symbol_id) => *symbol_id,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::StructMethodNotDefined {
                        struct_name: object_name.clone(),
                        method_name: method_name.clone(),
                    },
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let mut method_symbol_entry = self
            .resolver
            .lookup_symbol_entry_with_id(object_module_id, method_symbol_id)
            .unwrap();

        let resolved_method = match &mut method_symbol_entry.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method,
            _ => unreachable!(),
        };

        let first_param_opt = resolved_method.func_sig.params.list.first();
        let is_instance_method_call = {
            match first_param_opt {
                Some(first_param) => match first_param {
                    TypedFuncParamKind::FuncParam(..) => false,
                    TypedFuncParamKind::SelfModifier(..) => true,
                },
                None => false,
            }
        };

        if !self.validate_method_call(
            scope_id_opt,
            object_symbol_id,
            method_call,
            first_param_opt,
            object_methods,
            object_name.clone(),
            &resolved_method,
        ) {
            return None;
        }

        self.check_func_call(
            scope_id_opt,
            &mut resolved_method.func_sig,
            &mut method_call.args,
            method_call.loc.clone(),
            is_instance_method_call,
        );

        Some(resolved_method.func_sig.return_type.clone())
    }

    fn validate_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        instance_symbol_id: SymbolID,
        method_call: &TypedMethodCall,
        first_param_opt: Option<&TypedFuncParamKind>,
        object_methods: HashMap<String, SymbolID>,
        object_name: String,
        resolved_method: &ResolvedMethod,
    ) -> bool {
        let mut result = true;
        let method_vis = &resolved_method.func_sig.vis;

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
                kind: AnalyzerDiagKind::InternalMethodCall {
                    method_name: resolved_method.func_sig.name.clone(),
                    object_name,
                },
                location: Some(DiagLoc::new(method_call.loc.clone())),
                hint: None,
            });
            result = false;
        }

        let is_pointer = method_call
            .operand
            .concrete_type
            .clone()
            .unwrap()
            .get_const_inner()
            .is_pointer();
        let is_struct = method_call
            .operand
            .concrete_type
            .clone()
            .unwrap()
            .get_const_inner()
            .is_resolved_symbol();

        let is_operand_const = method_call.operand.concrete_type.clone().unwrap().is_const();

        if method_call.is_fat_arrow {
            if !is_pointer {
                dbg!(method_call.operand.concrete_type.clone());

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidFatArrow,
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: Some("Use '.' instead of '->'.".to_string()),
                });
                result = false;
            }
        } else {
            if !is_struct {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::UseFatArrow,
                    location: Some(DiagLoc::new(method_call.loc.clone())),
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
                        kind: AnalyzerDiagKind::MutationPossibleMethodCallOnConstInstance {
                            method_name: method_call.method_name.clone(),
                            instance_name: instance_name.clone(),
                        },
                        location: Some(DiagLoc::new(method_call.loc.clone())),
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

    fn analyze_array_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_array: &mut TypedArray,
    ) -> Option<ConcreteType> {
        typed_array.array_type =
            match self.normalize_type(scope_id_opt, typed_array.array_type.clone(), typed_array.loc.clone()) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        for (argument_idx, argument) in typed_array.elements.iter_mut().enumerate() {
            let argument_type = match self.analyze_typed_expr_type(
                scope_id_opt,
                argument,
                Some(*typed_array.array_type.as_array_type().unwrap().element_type.clone()),
            ) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            let element_type = match self.normalize_type(
                scope_id_opt,
                *typed_array.array_type.as_array_type().unwrap().element_type.clone(),
                argument.loc.clone(),
            ) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            if !self.check_type_mismatch(scope_id_opt, argument_type.clone(), element_type, argument.loc.clone()) {
                let element_type = format_concrete_type(argument_type, &(self.symbol_formatter)(scope_id_opt));
                let expected_type = format_concrete_type(
                    *typed_array.array_type.as_array_type().unwrap().element_type.clone(),
                    &(self.symbol_formatter)(scope_id_opt),
                );

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ArrayElementTypeMismatch {
                        element_type,
                        element_index: argument_idx.try_into().unwrap(),
                        expected_type,
                    },
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
                TypedArrayFixedCapacityValue::Value(value) => *value as i64,
            },
            TypedArrayCapacity::Dynamic => todo!(),
        };

        if typed_array.elements.len() != array_capacity.try_into().unwrap() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ArrayElementsCountMismatch {
                    elements: typed_array.elements.len().try_into().unwrap(),
                    expected: array_capacity.try_into().unwrap(),
                },
                location: Some(DiagLoc::new(typed_array.loc.clone())),
                hint: None,
            });
            return None;
        }

        Some(ConcreteType::Array(
            typed_array.array_type.as_array_type().unwrap().clone(),
        ))
    }

    fn analyze_cast_expr_type(&mut self, scope_id_opt: Option<ScopeID>, cast: &mut TypedCast) -> Option<ConcreteType> {
        let operand =
            match self.analyze_typed_expr_type(scope_id_opt, &mut cast.operand, Some(cast.target_type.clone())) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        if !(self.check_type_mismatch(
            scope_id_opt,
            operand.clone(),
            cast.target_type.clone(),
            cast.loc.clone(),
        ) || self.check_explicit_typecast(operand.clone(), cast.target_type.clone()))
        {
            let lhs_type = format_concrete_type(cast.target_type.clone(), &(self.symbol_formatter)(scope_id_opt));
            let rhs_type = format_concrete_type(operand, &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::CastTypeMismatch { lhs_type, rhs_type },
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
    ) -> Option<ConcreteType> {
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), instance_symbol_id)
            .unwrap();

        let concrete_type = match match &local_or_global_symbol {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => {
                let typed_variable = &local_symbol.as_variable().unwrap().typed_variable;

                match &typed_variable.ty {
                    Some(concrete_type) => self.normalize_type(scope_id_opt, concrete_type.clone(), loc.clone()),
                    None => {
                        let rhs = typed_variable.rhs.clone().unwrap();
                        self.analyze_typed_expr_type(scope_id_opt, &mut rhs.clone(), None)
                    }
                }
            }
            LocalOrGlobalSymbol::GlobalSymbol(global_symbol) => match global_symbol.as_global_var() {
                Some(resolved_global_var) => Some(resolved_global_var.global_var_sig.ty.clone().unwrap()),
                None => None,
            },
        } {
            Some(concrete_type) => Some(concrete_type),
            None => None,
        };

        if concrete_type.is_some() {
            let normalized_type = self
                .normalize_type(scope_id_opt, concrete_type.unwrap(), loc.clone())
                .unwrap();

            Some(normalized_type)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
enum MemberAccessKind {
    UnnamedStruct(Box<TypedUnnamedStructType>),
    NamedStruct(Box<ResolvedStruct>),
    Union(Box<ResolvedUnion>),
    Enum(Box<ResolvedEnum>),
}

fn infer_integer_type(
    literal: &TypedLiteral,
    suffix_opt: &Option<Box<TokenKind>>,
    expected: Option<ConcreteType>,
) -> Result<ConcreteType, Diag<AnalyzerDiagKind>> {
    if let Some(suffix) = suffix_opt {
        match map_integer_suffix_to_type(&suffix) {
            Some(ty) => Ok(ty),
            None => Err(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidIntegerLiteralSuffix,
                location: Some(DiagLoc::new(literal.loc.clone())),
                hint: Some(format!("Invalid suffix {:?} for integer literal.", suffix)),
            }),
        }
    } else if let Some(ctx_ty) = expected {
        if is_integer_type(&ctx_ty) {
            Ok(ctx_ty)
        } else {
            Ok(ConcreteType::BasicType(BasicConcreteType::Int)) // safe default
        }
    } else {
        Ok(ConcreteType::BasicType(BasicConcreteType::Int))
    }
}

fn infer_float_type(
    literal: &TypedLiteral,
    suffix_opt: &Option<Box<TokenKind>>,
    expected: Option<ConcreteType>,
) -> Result<ConcreteType, Diag<AnalyzerDiagKind>> {
    if let Some(suffix) = suffix_opt {
        match map_float_suffix_to_type(&suffix) {
            Some(ty) => Ok(ty),
            None => Err(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidFloatLiteralSuffix,
                location: Some(DiagLoc::new(literal.loc.clone())),
                hint: Some(format!("Invalid suffix {:?} for float literal.", suffix)),
            }),
        }
    } else if let Some(ctx_ty) = expected {
        if is_float_type(&ctx_ty) {
            Ok(ctx_ty)
        } else {
            Ok(ConcreteType::BasicType(BasicConcreteType::Float64)) // safe default
        }
    } else {
        Ok(ConcreteType::BasicType(BasicConcreteType::Float64))
    }
}

fn map_integer_suffix_to_type(suffix: &TokenKind) -> Option<ConcreteType> {
    let ty = match suffix {
        TokenKind::UIntPtr => BasicConcreteType::UIntPtr,
        TokenKind::IntPtr => BasicConcreteType::IntPtr,
        TokenKind::SizeT => BasicConcreteType::SizeT,
        TokenKind::Int => BasicConcreteType::Int,
        TokenKind::Int8 => BasicConcreteType::Int8,
        TokenKind::Int16 => BasicConcreteType::Int16,
        TokenKind::Int32 => BasicConcreteType::Int32,
        TokenKind::Int64 => BasicConcreteType::Int64,
        TokenKind::Int128 => BasicConcreteType::Int128,
        TokenKind::UInt => BasicConcreteType::UInt,
        TokenKind::UInt8 => BasicConcreteType::UInt8,
        TokenKind::UInt16 => BasicConcreteType::UInt16,
        TokenKind::UInt32 => BasicConcreteType::UInt32,
        TokenKind::UInt64 => BasicConcreteType::UInt64,
        TokenKind::UInt128 => BasicConcreteType::UInt128,
        _ => return None,
    };
    Some(ConcreteType::BasicType(ty))
}

fn map_float_suffix_to_type(suffix: &TokenKind) -> Option<ConcreteType> {
    let ty = match suffix {
        TokenKind::Float16 => BasicConcreteType::Float16,
        TokenKind::Float32 => BasicConcreteType::Float32,
        TokenKind::Float64 => BasicConcreteType::Float64,
        TokenKind::Float128 => BasicConcreteType::Float128,
        _ => return None,
    };
    Some(ConcreteType::BasicType(ty))
}

fn is_integer_type(ty: &ConcreteType) -> bool {
    matches!(
        ty,
        ConcreteType::BasicType(
            BasicConcreteType::Int
                | BasicConcreteType::Int8
                | BasicConcreteType::Int16
                | BasicConcreteType::Int32
                | BasicConcreteType::Int64
                | BasicConcreteType::Int128
                | BasicConcreteType::UInt
                | BasicConcreteType::UInt8
                | BasicConcreteType::UInt16
                | BasicConcreteType::UInt32
                | BasicConcreteType::UInt64
                | BasicConcreteType::UInt128
                | BasicConcreteType::IntPtr
                | BasicConcreteType::UIntPtr
                | BasicConcreteType::SizeT
        )
    )
}

fn is_float_type(ty: &ConcreteType) -> bool {
    matches!(
        ty,
        ConcreteType::BasicType(
            BasicConcreteType::Float16
                | BasicConcreteType::Float32
                | BasicConcreteType::Float64
                | BasicConcreteType::Float128
        )
    )
}
