use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, with_monomorph_registry};
use ast::source_loc::SourceLoc;
use diagcentral::{Diag, DiagLevel, DiagLoc};
use partialmatch::partial_match;
use resolver::signatures::{EnumSig, StructSig, UnionSig};
use std::collections::HashMap;
use typed_ast::{
    ScopeID, SymbolID, TypedEnumVariant, TypedExpression, TypedFuncTypeParams, TypedGenericParamsList, TypedTypeArg,
    TypedTypeArgs,
    format::format_concrete_type,
    types::{
        ConcreteType, GenericType, TypedArrayType, TypedFuncType, TypedTupleType, TypedUnnamedStructType,
        TypedUnnamedStructTypeField,
    },
};

#[derive(Debug, Clone)]
pub(crate) struct GenericMappingCtx {
    pub(crate) mapping: HashMap<String, ConcreteType>,
}

impl<'a> AnalysisContext<'a> {
    pub(crate) fn substitute_field_access_type(
        &mut self,
        field_access_operand: &mut TypedExpression,
        generic_params: &Option<TypedGenericParamsList>,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!((generic_type_opt, field_access_operand.concrete_type.clone()), {
            (Some(generic_type), Some(operand_ty)) => {
                let mapping_ctx = self.get_generic_mapping_ctx(&generic_params, &Some(generic_type.type_args.clone()), loc);
                field_access_operand.concrete_type = self.substitute_type(operand_ty, &mapping_ctx);
            }
        })
    }

    pub(crate) fn substitute_struct_type_args(
        &mut self,
        struct_sig: &mut StructSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!(generic_type_opt, {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(&struct_sig.generic_params, &Some(generic_type.type_args.clone()), loc);
                struct_sig.fields.iter_mut().for_each(|field| {
                    if let Some(concrete_type) = self.substitute_type(field.ty.clone(), &mapping_ctx){
                        field.ty = concrete_type;
                    }
                });
            }
        })
    }

    pub(crate) fn substitute_union_type_args(
        &mut self,
        union_sig: &mut UnionSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!(generic_type_opt, {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(&union_sig.generic_params, &Some(generic_type.type_args.clone()), loc);
                union_sig.fields.iter_mut().for_each(|field| {
                    if let Some(concrete_type) = self.substitute_type(field.ty.clone(), &mapping_ctx){
                        field.ty = concrete_type;
                    }
                });
            }
        })
    }

    pub(crate) fn substitute_enum_type_args(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        enum_sig: &mut EnumSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!(generic_type_opt, {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(&enum_sig.generic_params, &Some(generic_type.type_args.clone()), loc);
                enum_sig.variants.iter_mut().for_each(|variant| {
                    match variant {
                        TypedEnumVariant::Identifier(..) => {},
                        TypedEnumVariant::Valued(_, typed_expr) => {
                            self.analyze_typed_expr_type(scope_id_opt, typed_expr, None).inspect(|concrete_type| {
                                self.substitute_type(concrete_type.clone(), &mapping_ctx);
                            });
                        },
                        TypedEnumVariant::Variant(_, typed_enum_valued_fields) => {
                            for valued_field in typed_enum_valued_fields {
                                if let Some(substituted_type) = self.substitute_type(valued_field.field_type.clone(), &mapping_ctx) {
                                    valued_field.field_type = substituted_type;
                                }
                            }
                        },
                    }
                });
            }
        })
    }

    pub(crate) fn normalize_type_args_and_register(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
        generic_params: &Option<TypedGenericParamsList>,
        generic_mapping_ctx: &GenericMappingCtx,
        expected_type: Option<ConcreteType>,
        loc: SourceLoc,
    ) -> Option<Vec<ConcreteType>> {
        let mut inferred_all = true;

        if let Some(generic_params) = &generic_params {
            // normalize type arguments using current mapping
            let mut normalized_type_args: Vec<ConcreteType> = Vec::new();
            let mut missing: Vec<String> = Vec::new();

            for param in generic_params {
                let key = param.param_name.as_string();

                if let Some(concrete_type) = generic_mapping_ctx.mapping.get(&key).cloned() {
                    // explicit
                    normalized_type_args.push(concrete_type);
                } else if let Some(default_ty) = &param.default {
                    // use default
                    normalized_type_args.push(default_ty.clone());
                } else {
                    inferred_all = false;
                    missing.push(format!("'{}'", key));
                }
            }

            if inferred_all {
                with_monomorph_registry!(self, registry, {
                    registry.register(symbol_id, normalized_type_args.clone());
                });
            } else {
                // infer from expected type
                if let Some(type_args) = self.infer_generic_type_from_expected_type(
                    symbol_id,
                    generic_params,
                    generic_mapping_ctx,
                    expected_type,
                ) {
                    return Some(type_args);
                }
            }

            if !missing.is_empty() {
                let type_name = (self.symbol_formatter)(scope_id_opt)(symbol_id);
                let hint = format!("Provide explicit type arguments for {}", missing.join(", "));

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ExplicitTypeArgsRequired { type_name },
                    location: Some(DiagLoc::new(loc)),
                    hint: Some(hint),
                });

                return None;
            }

            return Some(normalized_type_args);
        }

        None
    }

    fn infer_generic_type_from_expected_type(
        &mut self,
        symbol_id: SymbolID,
        generic_params: &TypedGenericParamsList,
        generic_mapping_ctx: &GenericMappingCtx,
        expected_type: Option<ConcreteType>,
    ) -> Option<Vec<ConcreteType>> {
        let expected = match expected_type.as_ref() {
            Some(e) => e,
            None => return None,
        };

        if let ConcreteType::GenericType(generic_type) = expected {
            if generic_type.base != symbol_id {
                return None;
            }

            if generic_type.type_args.len() != generic_params.len() {
                return None;
            }

            let mut merged_args: Vec<ConcreteType> = Vec::with_capacity(generic_params.len());
            for (i, param) in generic_params.iter().enumerate() {
                let key = param.param_name.as_string();

                if let Some(concrete) = generic_mapping_ctx.mapping.get(&key).cloned() {
                    merged_args.push(concrete);
                } else if let Some(default_ty) = &param.default {
                    merged_args.push(default_ty.clone());
                } else {
                    let concrete_type = match generic_type.type_args[i].clone() {
                        TypedTypeArg::Positional(concrete_type) => concrete_type,
                        TypedTypeArg::Named { value, .. } => value,
                    };
                    merged_args.push(concrete_type);
                }
            }

            with_monomorph_registry!(self, registry, {
                registry.register(symbol_id, merged_args.clone());
            });

            return Some(merged_args);
        }

        None
    }

    pub(crate) fn inferred_types_as_positional_type_args(&self, types: Vec<ConcreteType>) -> TypedTypeArgs {
        types
            .iter()
            .map(|concrete_type| TypedTypeArg::Positional(concrete_type.clone()))
            .collect()
    }

    pub(crate) fn substitute_type_or_infer_with(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        ty: ConcreteType,
        expr: &mut TypedExpression,
        generic_mapping_ctx: &mut GenericMappingCtx,
    ) -> Option<ConcreteType> {
        match self.substitute_type(ty.clone(), generic_mapping_ctx) {
            Some(substituted) => {
                // analyze the expression with the expected substituted type
                if let Some(expr_type) = self.analyze_typed_expr_type(scope_id_opt, expr, Some(substituted.clone())) {
                    if !self.check_type_mismatch(scope_id_opt, expr_type.clone(), substituted.clone(), expr.loc.clone())
                    {
                        let expected_type =
                            format_concrete_type(substituted.clone(), &(self.symbol_formatter)(scope_id_opt));
                        let found_type =
                            format_concrete_type(expr_type.clone(), &(self.symbol_formatter)(scope_id_opt));
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::AssignmentTypeMismatch {
                                lhs_type: expected_type,
                                rhs_type: found_type,
                            },
                            location: Some(DiagLoc::new(expr.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                    Some(substituted)
                } else {
                    None
                }
            }
            None => {
                // could not substitute: attempt to infer from expression
                if let Some(expr_type) = self.analyze_typed_expr_type(scope_id_opt, expr, None) {
                    partial_match!(ty.as_generic_param(), {
                        Some(generic_param) => {
                            generic_mapping_ctx.insert_custom(generic_param.name.clone(), expr_type.clone());
                        }
                    });
                    self.substitute_type(ty, generic_mapping_ctx)
                } else {
                    None
                }
            }
        }
    }

    pub(crate) fn substitute_type(
        &self,
        concrete_type: ConcreteType,
        generic_mapping_ctx: &GenericMappingCtx,
    ) -> Option<ConcreteType> {
        match concrete_type {
            ConcreteType::GenericParam(param) => generic_mapping_ctx
                .mapping
                .iter()
                .find(|(k, _)| **k == param.name)
                .map(|(_, concrete_type)| concrete_type)
                .cloned(),
            ConcreteType::Pointer(inner) => Some(ConcreteType::Pointer(Box::new(
                self.substitute_type(*inner, generic_mapping_ctx)?,
            ))),
            ConcreteType::Array(inner) => Some(ConcreteType::Array(TypedArrayType {
                element_type: Box::new(self.substitute_type(*inner.element_type, generic_mapping_ctx)?),
                capacity: inner.capacity,
                loc: inner.loc.clone(),
            })),
            ConcreteType::Const(inner) => Some(ConcreteType::Const(Box::new(
                self.substitute_type(*inner, generic_mapping_ctx)?,
            ))),
            ConcreteType::Tuple(tuple) => {
                let new_list = tuple
                    .type_list
                    .into_iter()
                    .map(|t| self.substitute_type(t, generic_mapping_ctx))
                    .collect::<Option<Vec<_>>>()?;

                Some(ConcreteType::Tuple(TypedTupleType {
                    type_list: new_list,
                    loc: tuple.loc.clone(),
                }))
            }
            ConcreteType::FuncType(func) => {
                let new_params = func
                    .params
                    .list
                    .into_iter()
                    .map(|p| self.substitute_type(p, generic_mapping_ctx))
                    .collect::<Option<Vec<_>>>()?;

                let new_return = Box::new(self.substitute_type(*func.return_type, generic_mapping_ctx)?);
                Some(ConcreteType::FuncType(TypedFuncType {
                    def_module_id: func.def_module_id,
                    params: TypedFuncTypeParams {
                        list: new_params,
                        variadic: func.params.variadic,
                    },
                    return_type: new_return,
                    vis_opt: func.vis_opt,
                    loc: func.loc,
                }))
            }
            ConcreteType::UnnamedStruct(s) => {
                let new_fields = s
                    .fields
                    .iter()
                    .map(|f| {
                        Some(TypedUnnamedStructTypeField {
                            field_name: f.field_name.clone(),
                            field_type: Box::new(self.substitute_type(*f.field_type.clone(), generic_mapping_ctx)?),
                            loc: f.loc.clone(),
                        })
                    })
                    .collect::<Option<Vec<_>>>()?;

                Some(ConcreteType::UnnamedStruct(TypedUnnamedStructType {
                    fields: new_fields,
                    packed: s.packed,
                    loc: s.loc.clone(),
                }))
            }
            other => Some(other),
        }
    }

    pub(crate) fn get_generic_mapping_ctx(
        &mut self,
        generic_params_opt: &Option<TypedGenericParamsList>,
        type_args_opt: &Option<TypedTypeArgs>,
        loc: SourceLoc,
    ) -> GenericMappingCtx {
        let mut mapping = HashMap::new();

        if let Some(generic_params) = generic_params_opt {
            if let Some(type_args) = type_args_opt.as_ref().map(|args| args.clone()) {
                for (param, arg) in generic_params.iter().zip(type_args.iter()) {
                    let concrete_type = match arg {
                        TypedTypeArg::Positional(ct) => ct.clone(),
                        TypedTypeArg::Named { value, .. } => value.clone(),
                    };
                    mapping.insert(param.param_name.as_string(), concrete_type);
                }
            }
        } else if type_args_opt.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::UnexpectedTypeArgs,
                location: Some(DiagLoc::new(loc.clone())),
                hint: Some("Remove the arguments or add generic parameters to the type.".to_string()),
            });
        }

        GenericMappingCtx { mapping }
    }
}

#[macro_export]
macro_rules! generic_mapping_ctx_scope {
    ($self:ident, $generic_params:expr, $type_args:expr, $loc:expr, $ctx:ident, $body:block) => {{
        let mut $ctx =
            $self.get_generic_mapping_ctx(&$generic_params, &$type_args, $loc);

        $self.generic_ctx_stack.push($ctx.clone());
        $body
        $self.generic_ctx_stack.pop();
    }};
}

impl GenericMappingCtx {
    pub fn insert_custom(&mut self, key: String, ty: ConcreteType) {
        self.mapping.insert(key, ty);
    }
}
