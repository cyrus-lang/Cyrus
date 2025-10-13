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
    pub(crate) mapping: HashMap<Identifier, ConcreteType>,
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
                field_access.operand.concrete_type = Some(self.substitute_type(operand_ty, &mapping_ctx));
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
                    field.ty = self.substitute_type(field.ty.clone(), &mapping_ctx);
                });
            }
        })
    }

    pub(crate) fn normalize_type_args_and_register(
        &mut self,
        symbol_id: SymbolID,
        generic_params: &Option<TypedGenericParamsList>,
        generic_mapping_ctx: &GenericMappingCtx,
    ) {
        if let Some(generic_params) = &generic_params {
            // normalize type arguments using current mapping
            let normalized_type_args: Vec<ConcreteType> = generic_params
                .iter()
                .map(|param| {
                    generic_mapping_ctx
                        .mapping
                        .get(&param.param_name)
                        .cloned()
                        .unwrap_or_else(|| {
                            panic!("Generic param '{}' not found in mapping.", param.param_name.as_string())
                        })
                })
                .collect();

            with_monomorph_registry!(self, registry, {
                registry.register(symbol_id, normalized_type_args.clone());
            });
        }
    }

    pub(crate) fn substitute_type(
        &self,
        concrete_type: ConcreteType,
        generic_mapping_ctx: &GenericMappingCtx,
    ) -> ConcreteType {
        match concrete_type {
            ConcreteType::GenericParam(param) => generic_mapping_ctx
                .mapping
                .iter()
                .find(|(k, _)| k.as_string() == param.name)
                .unwrap()
                .1
                .clone(),
            ConcreteType::Pointer(inner) => {
                ConcreteType::Pointer(Box::new(self.substitute_type(*inner, generic_mapping_ctx)))
            }
            ConcreteType::Array(inner) => ConcreteType::Array(TypedArrayType {
                element_type: Box::new(self.substitute_type(*inner.element_type, generic_mapping_ctx)),
                capacity: inner.capacity,
                loc: inner.loc.clone(),
            }),
            ConcreteType::Const(inner) => {
                ConcreteType::Const(Box::new(self.substitute_type(*inner, generic_mapping_ctx)))
            }
            ConcreteType::Tuple(tuple) => ConcreteType::Tuple(TypedTupleType {
                type_list: tuple
                    .type_list
                    .into_iter()
                    .map(|t| self.substitute_type(t, generic_mapping_ctx))
                    .collect(),
                loc: tuple.loc.clone(),
            }),
            ConcreteType::FuncType(func) => {
                let new_params = func
                    .params
                    .list
                    .into_iter()
                    .map(|p| self.substitute_type(p, generic_mapping_ctx))
                    .collect();
                let new_return = Box::new(self.substitute_type(*func.return_type, generic_mapping_ctx));
                ConcreteType::FuncType(TypedFuncType {
                    def_module_id: func.def_module_id,
                    params: TypedFuncTypeParams {
                        list: new_params,
                        variadic: func.params.variadic,
                    },
                    return_type: new_return,
                    vis_opt: func.vis_opt,
                    loc: func.loc,
                })
            }
            ConcreteType::UnnamedStruct(s) => {
                let new_fields = s
                    .fields
                    .iter()
                    .map(|f| TypedUnnamedStructTypeField {
                        field_name: f.field_name.clone(),
                        field_type: Box::new(self.substitute_type(*f.field_type.clone(), generic_mapping_ctx)),
                        loc: f.loc.clone(),
                    })
                    .collect();

                ConcreteType::UnnamedStruct(TypedUnnamedStructType {
                    fields: new_fields,
                    packed: s.packed,
                    loc: s.loc.clone(),
                })
            }
            other => other,
        }
    }

    pub(crate) fn get_generic_mapping_ctx(
        &self,
        generic_params_opt: &Option<TypedGenericParamsList>,
        type_args_opt: &Option<TypedTypeArgs>,
    ) -> GenericMappingCtx {
        if generic_params_opt.is_some() && type_args_opt.is_none() {
            panic!("Generic parameters provided but type arguments missing"); // FIXME Change to AnalyzerDiagKind
        }

        if generic_params_opt.is_none() && type_args_opt.is_some() {
            panic!("Type arguments provided but generic parameters missing"); // FIXME Change to AnalyzerDiagKind
        }

        if let (Some(generic_params), Some(type_args)) = (generic_params_opt, type_args_opt) {
            if generic_params.len() != type_args.len() {
                panic!(
                    "Generic parameters count ({}) does not match type arguments count ({})",
                    generic_params.len(),
                    type_args.len()
                ); // FIXME Change to AnalyzerDiagKind
            }

            let mapping = generic_params
                .iter()
                .zip(type_args)
                .map(|(param, arg)| {
                    (
                        param.param_name.clone(),
                        match arg {
                            TypedTypeArg::Positional(ct) => ct.clone(),
                            TypedTypeArg::Named { key: _, value } => value.clone(),
                        },
                    )
                })
                .collect::<HashMap<_, _>>();

            GenericMappingCtx { mapping }
        } else {
            GenericMappingCtx {
                mapping: HashMap::new(),
            }
        }
    }
}

#[macro_export]
macro_rules! generic_mapping_ctx_scope {
    ($self:ident, $resolved_struct:expr, $struct_init:expr, $ctx:ident, $body:block) => {{
        let $ctx =
            $self.get_generic_mapping_ctx(&$resolved_struct.struct_sig.generic_params, &$struct_init.type_args);

        $self.generic_ctx_stack.push($ctx.clone());
        $body
        $self.generic_ctx_stack.pop();
    }};
}
