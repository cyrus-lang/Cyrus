use crate::{context::AnalysisContext, with_monomorph_registry};
use ast::Identifier;
use partialmatch::partial_match;
use resolver::signatures::StructSig;
use std::collections::HashMap;
use typed_ast::{
    SymbolID, TypedFieldAccess, TypedFuncTypeParams, TypedGenericParamsList, TypedTypeArg, TypedTypeArgs,
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
        &self,
        field_access: &mut TypedFieldAccess,
        struct_sig: &StructSig,
        generic_type_opt: Option<&GenericType>,
    ) {
        partial_match!((generic_type_opt, field_access.operand.concrete_type.clone()), {
            (Some(generic_type), Some(operand_ty)) => {
                let mapping_ctx = self.get_generic_mapping_ctx(&struct_sig.generic_params, &Some(generic_type.type_args.clone()));
                field_access.operand.concrete_type = self.substitute_type(operand_ty, &mapping_ctx);
            }
        })
    }

    pub(crate) fn substitute_struct_type_args(
        &mut self,
        struct_sig: &mut StructSig,
        generic_type_opt: Option<&GenericType>,
    ) {
        partial_match!(generic_type_opt, {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(&struct_sig.generic_params, &Some(generic_type.type_args.clone()));
                struct_sig.fields.iter_mut().for_each(|field| {
                    if let Some(concrete_type) = self.substitute_type(field.ty.clone(), &mapping_ctx){
                        field.ty = concrete_type;
                    }
                });
            }
        })
    }

    pub(crate) fn normalize_type_args_and_register(
        &mut self,
        symbol_id: SymbolID,
        generic_params: &Option<TypedGenericParamsList>,
        generic_mapping_ctx: &GenericMappingCtx,
    ) -> Option<Vec<ConcreteType>> {
        let mut inferred_all = true;
        if let Some(generic_params) = &generic_params {
            // normalize type arguments using current mapping
            let mut normalized_type_args: Vec<ConcreteType> = Vec::new();
            for param in generic_params {
                match generic_mapping_ctx.mapping.get(&param.param_name.as_string()).cloned() {
                    Some(concrete_type) => normalized_type_args.push(concrete_type),
                    None => {
                        inferred_all = false;
                        continue;
                    }
                }
            }

            if inferred_all {
                with_monomorph_registry!(self, registry, {
                    registry.register(symbol_id, normalized_type_args.clone());
                });
            }

            return Some(normalized_type_args);
        }

        return None;
    }

    pub(crate) fn inferred_types_as_positional_type_args(&self, types: Vec<ConcreteType>) -> TypedTypeArgs {
        types
            .iter()
            .map(|concrete_type| TypedTypeArg::Positional(concrete_type.clone()))
            .collect()
    }

    // pub(crate) fn substitute_type_or_infer_with(
    //     &self,
    //     concrete_type: ConcreteType,
    //     generic_mapping_ctx: &GenericMappingCtx,
    //     infer: Option<&TypedExpression>,
    // ) -> ConcreteType {
    //     if let Some(field_target_type) = self.substitute_type(field.ty.clone(), &generic_mapping_ctx) {
    //         match self.analyze_typed_expr_type(
    //             scope_id_opt,
    //             &mut field_init.value,
    //             Some(field_target_type.clone()),
    //         ) {
    //             Some(concrete_type) => return concrete_type,
    //             None => {}
    //         };
    //     }

    //     todo!();
    // }

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
        &self,
        generic_params_opt: &Option<TypedGenericParamsList>,
        type_args_opt: &Option<TypedTypeArgs>,
    ) -> GenericMappingCtx {
        let mut mapping = HashMap::new();

        if let Some(generic_params) = generic_params_opt {
            if let Some(type_args) = type_args_opt.as_ref().map(|args| args.clone()) {
                if generic_params.len() != type_args.len() {
                    panic!(
                        "Generic parameters count ({}) does not match type arguments count ({})",
                        generic_params.len(),
                        type_args.len()
                    ); // TODO: proper AnalyzerDiagKind
                }

                for (param, arg) in generic_params.iter().zip(type_args.iter()) {
                    let concrete_type = match arg {
                        TypedTypeArg::Positional(ct) => ct.clone(),
                        TypedTypeArg::Named { value, .. } => value.clone(),
                    };
                    mapping.insert(param.param_name.as_string(), concrete_type);
                }
            }
        } else if type_args_opt.is_some() {
            panic!("Type arguments provided but no generic parameters exist"); // TODO: proper AnalyzerDiagKind
        }

        GenericMappingCtx { mapping }
    }
}

#[macro_export]
macro_rules! generic_mapping_ctx_scope {
    ($self:ident, $resolved_struct:expr, $struct_init:expr, $ctx:ident, $body:block) => {{
        let mut $ctx =
            $self.get_generic_mapping_ctx(&$resolved_struct.struct_sig.generic_params, &$struct_init.type_args);

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
