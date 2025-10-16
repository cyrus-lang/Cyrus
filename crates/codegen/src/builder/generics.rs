use crate::builder::{
    abi::{
        generate_monomorphic_enum_abi_name, generate_monomorphic_struct_abi_name, generate_monomorphic_union_abi_name,
    },
    module::{CodeGenBuilder, LocalIRValue},
};
use inkwell::types::{AnyTypeEnum, ArrayType, StructType};
use resolver::{
    scope::{LocalScopeRef, ResolvedEnum, ResolvedStruct, ResolvedUnion},
    signatures::{EnumSig, StructSig, UnionSig},
};
use static_analyzer::{
    monomorph::{MonomorphKey, NormalizedTypeArgs},
    with_monomorph_registry,
};
use std::collections::HashMap;
use typed_ast::{
    SymbolID, TypedEnumVariant, TypedFuncTypeParams, TypedFuncTypeVariadicParams, TypedStructField, TypedTypeArg,
    TypedTypeArgs, TypedUnionField,
    types::{
        ConcreteType, GenericType, TypedArrayType, TypedFuncType, TypedTupleType, TypedUnnamedStructType,
        TypedUnnamedStructTypeField,
    },
};

trait GenericMonomorphTarget<'a> {
    fn build_monomorph(
        &self,
        codegen: &mut CodeGenBuilder<'a>,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
        type_args: &Option<TypedTypeArgs>,
    ) -> AnyTypeEnum<'a>;
}

impl<'a> GenericMonomorphTarget<'a> for ResolvedUnion {
    fn build_monomorph(
        &self,
        codegen: &mut CodeGenBuilder<'a>,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
        type_args: &Option<TypedTypeArgs>,
    ) -> AnyTypeEnum<'a> {
        let local_or_global_symbol = codegen
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .unwrap();

        let resolved_union = local_or_global_symbol.as_union().unwrap();
        codegen.get_or_declare_union_monomorph(resolved_union, type_args).into()
    }
}

impl<'a> GenericMonomorphTarget<'a> for ResolvedStruct {
    fn build_monomorph(
        &self,
        codegen: &mut CodeGenBuilder<'a>,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
        type_args: &Option<TypedTypeArgs>,
    ) -> AnyTypeEnum<'a> {
        let local_or_global_symbol = codegen
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .unwrap();

        let resolved_struct = local_or_global_symbol.as_struct().unwrap();
        codegen
            .get_or_declare_struct_monomorph(resolved_struct, type_args)
            .into()
    }
}

impl<'a> GenericMonomorphTarget<'a> for ResolvedEnum {
    fn build_monomorph(
        &self,
        codegen: &mut CodeGenBuilder<'a>,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
        type_args: &Option<TypedTypeArgs>,
    ) -> AnyTypeEnum<'a> {
        let local_or_global_symbol = codegen
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .unwrap();

        let resolved_enum = local_or_global_symbol.as_enum().unwrap();
        let enum_struct_type = codegen.get_or_declare_enum_monomorph(resolved_enum, type_args).0;
        enum_struct_type.into()
    }
}

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_resolved_generic_type(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        resolved_generic: &GenericType,
    ) -> AnyTypeEnum<'a> {
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), resolved_generic.base)
            .unwrap();

        if let Some(resolved) = local_or_global_symbol.as_struct() {
            return resolved.build_monomorph(
                self,
                local_scope_opt,
                resolved_generic.base,
                &Some(resolved_generic.type_args.clone()),
            );
        }

        if let Some(resolved) = local_or_global_symbol.as_enum() {
            return resolved.build_monomorph(
                self,
                local_scope_opt,
                resolved_generic.base,
                &Some(resolved_generic.type_args.clone()),
            );
        }

        if let Some(resolved) = local_or_global_symbol.as_union() {
            return resolved.build_monomorph(
                self,
                local_scope_opt,
                resolved_generic.base,
                &Some(resolved_generic.type_args.clone()),
            );
        }

        unreachable!("Unsupported generic monomorph target");
    }

    pub(crate) fn get_or_declare_enum_monomorph(
        &mut self,
        resolved_enum: &ResolvedEnum,
        type_args: &Option<TypedTypeArgs>,
    ) -> (StructType<'a>, ArrayType<'a>) {
        if let Some(_) = &resolved_enum.enum_sig.generic_params {
            let type_args = type_args.as_ref().expect("Generic struct used without type args!");
            let normalized_args = self.get_normalized_type_args(type_args);

            let key = MonomorphKey::new(resolved_enum.symbol_id, normalized_args);
            let monomorph_id = with_monomorph_registry!(self, ctx, {
                ctx.get_with_key(key.clone())
                    .unwrap_or_else(|| ctx.register(resolved_enum.symbol_id, key.normalized_args.clone()))
            });

            let substituted_sig =
                self.substitute_enum_sig_generic_params(resolved_enum.enum_sig.clone(), &key.normalized_args);

            // insert to irreg

            let enum_type_opt = self.get_ir_value(monomorph_id).and_then(|local_ir_value| {
                let (struct_type, payload_type) = local_ir_value.as_enum().unwrap();
                Some((struct_type.clone(), payload_type.clone()))
            });

            match enum_type_opt {
                Some(enum_type) => enum_type,
                None => {
                    let llvm_struct_name = generate_monomorphic_enum_abi_name(
                        &self.get_module_name(self.module_id),
                        &resolved_enum.enum_sig.name,
                        monomorph_id,
                    );
                    let (struct_type, payload_type) = self.build_enum_type(&substituted_sig, Some(llvm_struct_name));
                    self.insert_ir_value(monomorph_id, LocalIRValue::Enum((struct_type, payload_type)));
                    (struct_type, payload_type)
                }
            }
        } else {
            self.get_or_declare_enum(resolved_enum.symbol_id, &resolved_enum.enum_sig)
        }
    }

    pub(crate) fn get_or_declare_union_monomorph(
        &mut self,
        resolved_union: &ResolvedUnion,
        type_args: &Option<TypedTypeArgs>,
    ) -> StructType<'a> {
        if let Some(_) = &resolved_union.union_sig.generic_params {
            let type_args = type_args.as_ref().expect("Generic struct used without type args!");
            let normalized_args = self.get_normalized_type_args(type_args);

            let key = MonomorphKey::new(resolved_union.symbol_id, normalized_args);
            let monomorph_id = with_monomorph_registry!(self, ctx, {
                ctx.get_with_key(key.clone())
                    .unwrap_or_else(|| ctx.register(resolved_union.symbol_id, key.normalized_args.clone()))
            });

            let substituted_sig =
                self.substitute_union_sig_generic_params(resolved_union.union_sig.clone(), &key.normalized_args);

            // insert to irreg

            let struct_type_opt = self
                .get_ir_value(monomorph_id)
                .and_then(|local_ir_value| local_ir_value.as_struct().cloned());

            match struct_type_opt {
                Some(struct_type) => struct_type,
                None => {
                    let llvm_struct_name = generate_monomorphic_union_abi_name(
                        &self.get_module_name(self.module_id),
                        &resolved_union.union_sig.name,
                        monomorph_id,
                    );
                    let struct_type = self.build_union_type(&substituted_sig, Some(llvm_struct_name));
                    self.insert_ir_value(monomorph_id, LocalIRValue::Struct(struct_type));
                    struct_type
                }
            }
        } else {
            self.get_or_declare_union(resolved_union.symbol_id, &resolved_union.union_sig)
        }
    }

    pub(crate) fn get_or_declare_struct_monomorph(
        &mut self,
        resolved_struct: &ResolvedStruct,
        type_args: &Option<TypedTypeArgs>,
    ) -> StructType<'a> {
        if let Some(_) = &resolved_struct.struct_sig.generic_params {
            let type_args = type_args.as_ref().expect("Generic struct used without type args!");
            let normalized_args = self.get_normalized_type_args(type_args);

            let key = MonomorphKey::new(resolved_struct.symbol_id, normalized_args);
            let monomorph_id = with_monomorph_registry!(self, ctx, {
                ctx.get_with_key(key.clone())
                    .unwrap_or_else(|| ctx.register(resolved_struct.symbol_id, key.normalized_args.clone()))
            });

            let substituted_sig =
                self.substitute_struct_sig_generic_params(resolved_struct.struct_sig.clone(), &key.normalized_args);

            // insert to irreg

            let struct_type_opt = self
                .get_ir_value(monomorph_id)
                .and_then(|local_ir_value| local_ir_value.as_struct().cloned());

            match struct_type_opt {
                Some(struct_type) => struct_type,
                None => {
                    let llvm_struct_name = generate_monomorphic_struct_abi_name(
                        &self.get_module_name(self.module_id),
                        &resolved_struct.struct_sig.name,
                        monomorph_id,
                    );
                    let struct_type = self.build_struct_type(&substituted_sig, Some(llvm_struct_name));
                    self.insert_ir_value(monomorph_id, LocalIRValue::Struct(struct_type));
                    struct_type
                }
            }
        } else {
            self.get_or_declare_struct(resolved_struct.symbol_id, &resolved_struct.struct_sig)
        }
    }

    pub(crate) fn substitute_enum_sig_generic_params(
        &mut self,
        mut enum_sig: EnumSig,
        normalized_args: &NormalizedTypeArgs,
    ) -> EnumSig {
        let Some(generic_params) = &enum_sig.generic_params else {
            return enum_sig.clone();
        };

        let mut subst_map: HashMap<String, ConcreteType> = HashMap::new();

        for (param, arg) in generic_params.iter().zip(normalized_args.iter()) {
            subst_map.insert(param.param_name.to_string(), arg.clone());
        }

        let substituted_variants: Vec<TypedEnumVariant> = enum_sig
            .variants
            .iter()
            .cloned()
            .map(|variant| match variant {
                TypedEnumVariant::Identifier(..) | TypedEnumVariant::Valued(..) => variant,
                TypedEnumVariant::Variant(identifier, mut valued_fields) => {
                    for valued_field in &mut valued_fields {
                        let substituted_ty = self.substitute_concrete_type(&valued_field.field_type, &subst_map);
                        valued_field.field_type = substituted_ty;
                    }
                    TypedEnumVariant::Variant(identifier, valued_fields)
                }
            })
            .collect();

        enum_sig.generic_params = None;
        enum_sig.variants = substituted_variants;
        enum_sig
    }

    pub(crate) fn substitute_union_sig_generic_params(
        &mut self,
        mut union_sig: UnionSig,
        normalized_args: &NormalizedTypeArgs,
    ) -> UnionSig {
        let Some(generic_params) = &union_sig.generic_params else {
            return union_sig.clone();
        };

        let mut subst_map: HashMap<String, ConcreteType> = HashMap::new();

        for (param, arg) in generic_params.iter().zip(normalized_args.iter()) {
            subst_map.insert(param.param_name.to_string(), arg.clone());
        }

        let substituted_fields: Vec<TypedUnionField> = union_sig
            .fields
            .iter()
            .cloned()
            .map(|mut field| {
                let substituted_ty = self.substitute_concrete_type(&field.ty, &subst_map);
                field.ty = substituted_ty;
                field
            })
            .collect();

        union_sig.generic_params = None;
        union_sig.fields = substituted_fields;
        union_sig
    }

    pub(crate) fn substitute_struct_sig_generic_params(
        &mut self,
        mut struct_sig: StructSig,
        normalized_args: &NormalizedTypeArgs,
    ) -> StructSig {
        let Some(generic_params) = &struct_sig.generic_params else {
            return struct_sig.clone();
        };

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

    pub(crate) fn get_normalized_type_args(&self, type_args: &TypedTypeArgs) -> NormalizedTypeArgs {
        type_args
            .iter()
            .map(|type_arg| match type_arg {
                TypedTypeArg::Positional(concrete_type) => concrete_type.clone(),
                TypedTypeArg::Named { value, .. } => value.clone(),
            })
            .collect()
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
