use crate::context::AnalysisContext;
use ast::Identifier;
use std::collections::HashMap;
use typed_ast::{
    TypedFuncTypeParams, TypedGenericParamsList, TypedTypeArg, TypedTypeArgs,
    types::{
        ConcreteType, TypedArrayType, TypedFuncType, TypedTupleType, TypedUnnamedStructType,
        TypedUnnamedStructTypeField,
    },
};

#[derive(Debug, Clone)]
pub(crate) struct GenericMappingCtx {
    pub(crate) mapping: HashMap<Identifier, ConcreteType>,
}

impl<'a> AnalysisContext<'a> {
    pub(crate) fn substitute_type(
        &self,
        concrete_type: ConcreteType,
        mapping: &HashMap<Identifier, ConcreteType>,
    ) -> ConcreteType {
        match concrete_type {
            ConcreteType::GenericParam(param) => mapping
                .iter()
                .find(|(k, _)| k.as_string() == param.name)
                .unwrap()
                .1
                .clone(),
            ConcreteType::Pointer(inner) => ConcreteType::Pointer(Box::new(self.substitute_type(*inner, mapping))),
            ConcreteType::Array(inner) => ConcreteType::Array(TypedArrayType {
                element_type: Box::new(self.substitute_type(*inner.element_type, mapping)),
                capacity: inner.capacity,
                loc: inner.loc.clone(),
            }),
            ConcreteType::Const(inner) => ConcreteType::Const(Box::new(self.substitute_type(*inner, mapping))),
            ConcreteType::Tuple(tuple) => ConcreteType::Tuple(TypedTupleType {
                type_list: tuple
                    .type_list
                    .into_iter()
                    .map(|t| self.substitute_type(t, mapping))
                    .collect(),
                loc: tuple.loc.clone(),
            }),
            ConcreteType::FuncType(func) => {
                let new_params = func
                    .params
                    .list
                    .into_iter()
                    .map(|p| self.substitute_type(p, mapping))
                    .collect();
                let new_return = Box::new(self.substitute_type(*func.return_type, mapping));
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
                        field_type: Box::new(self.substitute_type(*f.field_type.clone(), mapping)),
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
            panic!("Generic parameters provided but type arguments missing");
        }

        if generic_params_opt.is_none() && type_args_opt.is_some() {
            panic!("Type arguments provided but generic parameters missing");
        }

        if let (Some(generic_params), Some(type_args)) = (generic_params_opt, type_args_opt) {
            if generic_params.len() != type_args.len() {
                panic!(
                    "Generic parameters count ({}) does not match type arguments count ({})",
                    generic_params.len(),
                    type_args.len()
                );
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
    ($self:ident, $resolved_struct:expr, $struct_init:expr, $body:block) => {{
        let generic_mapping_ctx =
            $self.get_generic_mapping_ctx(&$resolved_struct.struct_sig.generic_params, &$struct_init.type_args);

        $self.generic_ctx_stack.push(generic_mapping_ctx);

            $body

        $self.generic_ctx_stack.pop();
    }};
}
