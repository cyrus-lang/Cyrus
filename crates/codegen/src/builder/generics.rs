use crate::builder::module::{CodeGenBuilder, LocalIRValue};
use inkwell::types::{AnyTypeEnum, StructType};
use resolver::{scope::LocalScopeRef, signatures::StructSig};
use static_analyzer::{
    monomorph::{MonomorphKey, NormalizedTypeArgs},
    with_monomorph_registry,
};
use std::collections::HashMap;
use typed_ast::{
    SymbolID, TypedFuncTypeParams, TypedFuncTypeVariadicParams, TypedStructField, TypedTypeArg, TypedTypeArgs,
    types::{
        ConcreteType, ResolvedGeneric, TypedArrayType, TypedFuncType, TypedTupleType, TypedUnnamedStructType,
        TypedUnnamedStructTypeField,
    },
};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_resolved_generic_type(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        resolved_generic: &ResolvedGeneric,
    ) -> AnyTypeEnum<'a> {
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), resolved_generic.base)
            .unwrap();

        if local_or_global_symbol.as_struct().is_some() {
            self.get_or_declare_struct_monomorph(
                local_scope_opt,
                resolved_generic.base,
                &Some(resolved_generic.type_args.clone()),
            )
            .into()
        } else {
            unreachable!()
        }
    }

    pub(crate) fn get_or_declare_struct_monomorph(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
        typed_args: &Option<TypedTypeArgs>,
    ) -> StructType<'a> {
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)
            .unwrap();
        let resolved_struct = local_or_global_symbol.as_struct().unwrap();

        if let Some(_) = &resolved_struct.struct_sig.generic_params {
            let typed_args = typed_args.as_ref().expect("Generic struct used without type args!");

            let normalized_args: NormalizedTypeArgs = typed_args
                .iter()
                .map(|type_arg| match type_arg {
                    TypedTypeArg::Positional(concrete_type) => concrete_type.clone(),
                    TypedTypeArg::Named { value, .. } => value.clone(),
                })
                .collect();
            let key = MonomorphKey::new(symbol_id, normalized_args);

            let monomorph_id = with_monomorph_registry!(self, ctx, { ctx.get_with_key(key.clone()).unwrap() });

            let substituted_struct_sig =
                self.substitute_struct_sig_generic_params(resolved_struct.struct_sig.clone(), &key.normalized_args);

            match self
                .get_ir_value(monomorph_id)
                .and_then(|local_ir_value| local_ir_value.as_struct().cloned())
            {
                Some(struct_type) => struct_type,
                None => {
                    let struct_type = self.build_struct_only_type(&substituted_struct_sig);
                    self.insert_ir_value(monomorph_id, LocalIRValue::Struct(struct_type));
                    struct_type
                }
            }
        } else {
            self.get_or_declare_struct(symbol_id, &resolved_struct.struct_sig)
        }
    }

    pub(crate) fn substitute_struct_sig_generic_params(
        &mut self,
        mut struct_sig: StructSig,
        normalized_args: &NormalizedTypeArgs,
    ) -> StructSig {
        let Some(generic_params) = &struct_sig.generic_params else {
            return struct_sig.clone();
        };

        assert_eq!(
            generic_params.len(),
            normalized_args.len(),
            "Generic parameter count mismatch for struct '{}'.",
            struct_sig.name
        );

        use std::collections::HashMap;
        let mut subst_map: HashMap<String, ConcreteType> = HashMap::new();

        for (param, arg) in generic_params.iter().zip(normalized_args.iter()) {
            subst_map.insert(param.param_name.to_string(), arg.clone());
        }

        let substituted_fields: Vec<TypedStructField> = struct_sig
            .fields
            .iter()
            .cloned()
            .map(|mut field| {
                let substituted_ty = self.substitute_concrete_type(&field.ty, &subst_map);
                field.ty = substituted_ty;
                field
            })
            .collect();

        struct_sig.generic_params = None;
        struct_sig.fields = substituted_fields;
        struct_sig
    }

    fn substitute_concrete_type(&self, ty: &ConcreteType, subst_map: &HashMap<String, ConcreteType>) -> ConcreteType {
        match ty {
            ConcreteType::GenericParam(typed_identifier) => subst_map
                .get(&typed_identifier.name)
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            ConcreteType::Pointer(inner) => {
                ConcreteType::Pointer(Box::new(self.substitute_concrete_type(inner, subst_map)))
            }
            ConcreteType::Array(typed_array_type) => {
                let element_type = self.substitute_concrete_type(&typed_array_type.element_type, subst_map);
                ConcreteType::Array(TypedArrayType {
                    element_type: Box::new(element_type),
                    capacity: typed_array_type.capacity.clone(),
                    loc: typed_array_type.loc.clone(),
                })
            }
            ConcreteType::Const(inner) => {
                ConcreteType::Const(Box::new(self.substitute_concrete_type(inner, subst_map)))
            }
            ConcreteType::FuncType(func_ty) => {
                let new_return_type = self.substitute_concrete_type(&func_ty.return_type, subst_map);
                let new_params = TypedFuncTypeParams {
                    list: func_ty
                        .params
                        .list
                        .iter()
                        .map(|concrete_type| self.substitute_concrete_type(&concrete_type, subst_map))
                        .collect(),
                    variadic: func_ty.params.variadic.as_ref().map(|variadic| match &**variadic {
                        TypedFuncTypeVariadicParams::UntypedCStyle => {
                            Box::new(TypedFuncTypeVariadicParams::UntypedCStyle)
                        }
                        TypedFuncTypeVariadicParams::Typed(inner_ty) => Box::new(TypedFuncTypeVariadicParams::Typed(
                            self.substitute_concrete_type(inner_ty, subst_map),
                        )),
                    }),
                };

                ConcreteType::FuncType(TypedFuncType {
                    def_module_id: func_ty.def_module_id,
                    return_type: Box::new(new_return_type),
                    params: new_params,
                    loc: func_ty.loc.clone(),
                    vis_opt: func_ty.vis_opt.clone(),
                })
            }
            ConcreteType::Tuple(tuple_ty) => {
                let type_list = tuple_ty
                    .type_list
                    .iter()
                    .map(|elem| self.substitute_concrete_type(elem, subst_map))
                    .collect();

                ConcreteType::Tuple(TypedTupleType {
                    type_list,
                    loc: tuple_ty.loc.clone(),
                })
            }
            ConcreteType::UnnamedStruct(unnamed) => {
                let new_fields = unnamed
                    .fields
                    .iter()
                    .map(|field| {
                        let new_ty = self.substitute_concrete_type(&field.field_type, subst_map);
                        TypedUnnamedStructTypeField {
                            field_name: field.field_name.clone(),
                            field_type: Box::new(new_ty),
                            loc: field.loc.clone(),
                        }
                    })
                    .collect();

                ConcreteType::UnnamedStruct(TypedUnnamedStructType {
                    fields: new_fields,
                    packed: unnamed.packed,
                    loc: unnamed.loc.clone(),
                })
            }
            ty @ _ => ty.clone(),
        }
    }
}
