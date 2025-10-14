use super::module::CodeGenBuilder;
use crate::builder::{
    abi::generate_struct_abi_name,
    module::LocalIRValue,
    values::{InternalValue, InternalValueKind},
};
use inkwell::{
    AddressSpace,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, PointerType, StructType},
    values::{AnyValue, AnyValueEnum},
};
use resolver::{
    scope::{LocalOrGlobalSymbol, LocalScopeRef, LocalSymbolKind, SymbolEntryKind},
    signatures::StructSig,
    typed_func_type_from_func_sig,
};
use typed_ast::{
    SymbolID, TypedTupleValue,
    types::{
        BasicConcreteType, ConcreteType, ResolvedSymbol, TypedArrayCapacity, TypedArrayFixedCapacityValue,
        TypedArrayType, TypedTupleType, TypedUnnamedStructType,
    },
};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_cast(
        &self,
        target_type: AnyTypeEnum<'a>,
        internal_value: InternalValue<'a>,
    ) -> AnyValueEnum<'a> {
        if let Some(fn_value) = internal_value.as_func_value() {
            return self.build_cast_fn_value_to_pointer(fn_value);
        }

        let basic_value = internal_value.as_basic_value();

        match target_type {
            AnyTypeEnum::IntType(int_type) => {
                let val = basic_value;
                if val.is_int_value() {
                    // int -> int
                    AnyValueEnum::IntValue(
                        self.llvmbuilder
                            .build_int_cast(val.into_int_value(), int_type, "cast")
                            .unwrap(),
                    )
                } else if val.is_pointer_value() {
                    // ptr -> int
                    AnyValueEnum::IntValue(
                        self.llvmbuilder
                            .build_ptr_to_int(val.into_pointer_value(), int_type, "ptr_to_int")
                            .unwrap(),
                    )
                } else {
                    panic!("Invalid cast to int");
                }
            }

            AnyTypeEnum::FloatType(float_type) => AnyValueEnum::FloatValue(
                self.llvmbuilder
                    .build_float_cast(basic_value.into_float_value(), float_type, "cast")
                    .unwrap(),
            ),

            AnyTypeEnum::PointerType(ptr_type) => {
                let val = basic_value;
                if val.is_pointer_value() {
                    // ptr -> ptr
                    AnyValueEnum::PointerValue(
                        self.llvmbuilder
                            .build_pointer_cast(val.into_pointer_value(), ptr_type, "cast")
                            .unwrap(),
                    )
                } else if val.is_int_value() {
                    // int -> ptr
                    AnyValueEnum::PointerValue(
                        self.llvmbuilder
                            .build_int_to_ptr(val.into_int_value(), ptr_type, "int_to_ptr")
                            .unwrap(),
                    )
                } else {
                    panic!("Invalid cast to pointer");
                }
            }

            _ => internal_value.as_basic_value().as_any_value_enum(),
        }
    }

    pub(crate) fn build_implicit_cast(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        target_type: ConcreteType,
        rvalue: InternalValue<'a>,
    ) -> InternalValue<'a> {
        let target_any_type = self.build_concrete_type(local_scope_opt, target_type.clone());
        let any_value = self.build_cast(target_any_type, rvalue);
        InternalValue::new(target_type, InternalValueKind::RValue(any_value.try_into().unwrap()))
    }

    pub(crate) fn build_struct_type(&mut self, struct_sig: &StructSig, custom_name: Option<String>) -> StructType<'a> {
        let llvm_struct_name = custom_name.unwrap_or(generate_struct_abi_name(
            &self.get_module_name(self.module_id),
            &struct_sig.name,
        ));
        let struct_type = self
            .llvmctx
            .get_struct_type(&llvm_struct_name)
            .or_else(|| Some(self.llvmctx.opaque_struct_type(&llvm_struct_name)))
            .unwrap();

        let field_types: Vec<BasicTypeEnum<'a>> = struct_sig
            .fields
            .iter()
            .map(|field| self.build_concrete_type(None, field.ty.clone()).try_into().unwrap())
            .collect();

        struct_type.set_body(&field_types, struct_sig.packed);
        struct_type
    }

    pub(crate) fn build_concrete_type_from_symbol_id(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> AnyTypeEnum<'a> {
        if local_scope_opt.is_some() {
            let local_or_global_symbol = match self
                .resolver
                .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)
            {
                Some(local_or_global_symbol) => local_or_global_symbol,
                None => {
                    panic!("Undefined type symbol.")
                }
            };

            let irreg_symbol_id = local_or_global_symbol.get_symbol_id();

            let irreg = self.irreg.borrow();
            let local_ir_value_opt = irreg.get(&irreg_symbol_id).cloned();
            drop(irreg);

            if let Some(typedef) = local_or_global_symbol.as_typedef() {
                self.build_concrete_type(local_scope_opt, typedef.typedef_sig.ty.clone())
            } else {
                let local_ir_value = match local_ir_value_opt {
                    Some(local_ir_value) => local_ir_value,
                    None => self.build_concrete_type_declare_fresh(local_or_global_symbol),
                };

                let any_type_enum = match local_ir_value {
                    LocalIRValue::Struct(struct_type) => AnyTypeEnum::StructType(struct_type.clone()),
                    LocalIRValue::Enum((struct_type, ..)) => AnyTypeEnum::StructType(struct_type.clone()),
                    _ => unreachable!(),
                };

                any_type_enum
            }
        } else {
            let module_id = self.resolver.lookup_symbol_id_in_modules(symbol_id).unwrap();
            let symbol_entry = self.resolver.lookup_symbol_entry_with_id(module_id, symbol_id).unwrap();

            let any_type_enum = {
                match symbol_entry.kind {
                    SymbolEntryKind::Typedef(resolved_typedef) => {
                        self.build_concrete_type(local_scope_opt, resolved_typedef.typedef_sig.ty)
                    }
                    _ => {
                        let local_or_global_symbol = match self
                            .resolver
                            .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)
                        {
                            Some(local_or_global_symbol) => local_or_global_symbol,
                            None => {
                                panic!("Undefined type symbol.")
                            }
                        };

                        let irreg_symbol_id = local_or_global_symbol.get_symbol_id();

                        let irreg = self.irreg.borrow();
                        let local_ir_value_opt = irreg.get(&irreg_symbol_id).cloned();
                        drop(irreg);

                        let local_ir_value = match local_ir_value_opt {
                            Some(local_ir_value) => local_ir_value,
                            None => self.build_concrete_type_declare_fresh(local_or_global_symbol),
                        };

                        match local_ir_value {
                            LocalIRValue::Struct(struct_type) => AnyTypeEnum::StructType(struct_type.clone()),
                            LocalIRValue::Enum((struct_type, _)) => AnyTypeEnum::StructType(struct_type.clone()),
                            _ => unreachable!(),
                        }
                    }
                }
            };

            any_type_enum
        }
    }

    pub(crate) fn build_array_capacity(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        typed_array_type: &TypedArrayType,
    ) -> usize {
        match &typed_array_type.capacity {
            TypedArrayCapacity::Fixed(capacity_value) => match capacity_value {
                TypedArrayFixedCapacityValue::Expr(typed_expr) => {
                    let internal_value = self.build_expr(local_scope_opt, &typed_expr);
                    let int_value = internal_value.as_basic_value().into_int_value();
                    int_value.get_zero_extended_constant().unwrap() as usize
                }
                TypedArrayFixedCapacityValue::Value(value) => *value,
            },
            TypedArrayCapacity::Dynamic => todo!(),
        }
    }

    pub(crate) fn build_concrete_type(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        concrete_type: ConcreteType,
    ) -> AnyTypeEnum<'a> {
        match concrete_type {
            ConcreteType::GenericParam(..) => unreachable!(),
            ConcreteType::GenericType(ref resolved_generic) => {
                self.build_resolved_generic_type(local_scope_opt, resolved_generic)
            }
            ConcreteType::UnresolvedSymbol(symbol_id) => {
                self.build_concrete_type_from_symbol_id(local_scope_opt, symbol_id)
            }
            ConcreteType::ResolvedSymbol(resolved_symbol) => match resolved_symbol {
                ResolvedSymbol::Enum(symbol_id)
                | ResolvedSymbol::Union(symbol_id)
                | ResolvedSymbol::Typedef(symbol_id)
                | ResolvedSymbol::NamedStruct(symbol_id)
                | ResolvedSymbol::Interface(symbol_id)
                | ResolvedSymbol::GlobalVar(symbol_id)
                | ResolvedSymbol::Variable(symbol_id)
                | ResolvedSymbol::Func(symbol_id)
                | ResolvedSymbol::Method(symbol_id) => {
                    self.build_concrete_type_from_symbol_id(local_scope_opt, symbol_id)
                }
            },
            ConcreteType::BasicType(basic_concrete_type) => self.build_basic_concrete_type(basic_concrete_type),
            ConcreteType::Array(typed_array_type) => {
                let element_type: BasicTypeEnum = self
                    .build_concrete_type(local_scope_opt.clone(), *typed_array_type.element_type.clone())
                    .try_into()
                    .unwrap();

                let array_capacity = self.build_array_capacity(local_scope_opt, &typed_array_type);

                AnyTypeEnum::ArrayType(element_type.array_type(array_capacity.try_into().unwrap()))
            }
            ConcreteType::Const(concrete_type) => self.build_concrete_type(local_scope_opt, *concrete_type),
            ConcreteType::Pointer(_) => {
                let ptr: PointerType<'a> = self.llvmctx.ptr_type(AddressSpace::default());
                AnyTypeEnum::PointerType(ptr)
            }
            ConcreteType::UnnamedStruct(typed_unnamed_struct_type) => {
                self.build_unnamed_struct_type(local_scope_opt, &typed_unnamed_struct_type)
            }
            ConcreteType::FuncType(..) => self.llvmctx.ptr_type(AddressSpace::default()).into(),
            ConcreteType::Tuple(tuple_type) => self.build_tuple_type(local_scope_opt, &tuple_type),
        }
    }

    pub(crate) fn get_tuple_type_from_tuple_value(&self, tuple_value: &TypedTupleValue) -> TypedTupleType {
        TypedTupleType {
            type_list: get_elements_type_from_tuple_value(tuple_value),
            loc: tuple_value.loc.clone(),
        }
    }

    pub(crate) fn build_tuple_type(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        tuple_type: &TypedTupleType,
    ) -> AnyTypeEnum<'a> {
        let basic_types: Vec<BasicTypeEnum<'a>> = tuple_type
            .type_list
            .iter()
            .map(|concrete_type| {
                self.build_concrete_type(local_scope_opt.clone(), concrete_type.clone())
                    .try_into()
                    .unwrap()
            })
            .collect();

        self.llvmctx.struct_type(&basic_types, false).into()
    }

    pub(crate) fn build_unnamed_struct_type(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        unnamed_struct_type: &TypedUnnamedStructType,
    ) -> AnyTypeEnum<'a> {
        let field_types: Vec<BasicTypeEnum<'_>> = unnamed_struct_type
            .fields
            .iter()
            .map(|field| {
                self.build_concrete_type(local_scope_opt.clone(), *field.field_type.clone())
                    .try_into()
                    .unwrap()
            })
            .collect();

        AnyTypeEnum::StructType(self.llvmctx.struct_type(&field_types, unnamed_struct_type.packed))
    }

    pub(crate) fn build_basic_concrete_type(&self, basic_concrete_type: BasicConcreteType) -> AnyTypeEnum<'a> {
        let target_data = self.llvmtm.get_target_data();
        let ptr_sized_int_type = AnyTypeEnum::IntType(self.llvmctx.ptr_sized_int_type(&target_data, None));

        match basic_concrete_type {
            BasicConcreteType::UIntPtr => ptr_sized_int_type,
            BasicConcreteType::IntPtr => ptr_sized_int_type,
            BasicConcreteType::SizeT => ptr_sized_int_type,
            BasicConcreteType::Int => AnyTypeEnum::IntType(self.llvmctx.i32_type()),
            BasicConcreteType::Int8 => AnyTypeEnum::IntType(self.llvmctx.i8_type()),
            BasicConcreteType::Int16 => AnyTypeEnum::IntType(self.llvmctx.i16_type()),
            BasicConcreteType::Int32 => AnyTypeEnum::IntType(self.llvmctx.i32_type()),
            BasicConcreteType::Int64 => AnyTypeEnum::IntType(self.llvmctx.i64_type()),
            BasicConcreteType::Int128 => AnyTypeEnum::IntType(self.llvmctx.i128_type()),
            BasicConcreteType::UInt => AnyTypeEnum::IntType(self.llvmctx.i32_type()),
            BasicConcreteType::UInt8 => AnyTypeEnum::IntType(self.llvmctx.i8_type()),
            BasicConcreteType::UInt16 => AnyTypeEnum::IntType(self.llvmctx.i16_type()),
            BasicConcreteType::UInt32 => AnyTypeEnum::IntType(self.llvmctx.i32_type()),
            BasicConcreteType::UInt64 => AnyTypeEnum::IntType(self.llvmctx.i64_type()),
            BasicConcreteType::UInt128 => AnyTypeEnum::IntType(self.llvmctx.i128_type()),
            BasicConcreteType::Float16 => AnyTypeEnum::FloatType(self.llvmctx.f16_type()),
            BasicConcreteType::Float32 => AnyTypeEnum::FloatType(self.llvmctx.f32_type()),
            BasicConcreteType::Float64 => AnyTypeEnum::FloatType(self.llvmctx.f64_type()),
            BasicConcreteType::Float128 => AnyTypeEnum::FloatType(self.llvmctx.f128_type()),
            BasicConcreteType::Char => AnyTypeEnum::IntType(self.llvmctx.i8_type()),
            BasicConcreteType::Bool => AnyTypeEnum::IntType(self.llvmctx.bool_type()),
            BasicConcreteType::Null => AnyTypeEnum::PointerType(self.llvmctx.ptr_type(AddressSpace::default())),
            BasicConcreteType::Void => AnyTypeEnum::VoidType(self.llvmctx.void_type()),
        }
    }

    fn build_concrete_type_declare_fresh(&mut self, local_or_global_symbol: LocalOrGlobalSymbol) -> LocalIRValue<'a> {
        match &local_or_global_symbol {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                LocalSymbolKind::Struct(resolved_struct) => {
                    let struct_type =
                        self.get_or_declare_struct(resolved_struct.symbol_id, &resolved_struct.struct_sig);

                    LocalIRValue::Struct(struct_type)
                }
                LocalSymbolKind::Enum(_resolved_enum) => todo!(),
                _ => {
                    unreachable!()
                }
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Typedef(..) => {
                    unreachable!()
                }
                SymbolEntryKind::Func(..) | SymbolEntryKind::Interface(..) => {
                    unreachable!()
                }
                SymbolEntryKind::Method(resolved_method) => {
                    let fn_value =
                        self.get_or_declare_func(resolved_method.symbol_id, resolved_method.func_sig.clone());
                    LocalIRValue::Func(
                        fn_value,
                        ConcreteType::FuncType(typed_func_type_from_func_sig(&resolved_method.func_sig)),
                    )
                }
                SymbolEntryKind::GlobalVar(resolved_global_var) => {
                    let global_value = self.get_or_declare_global_var(resolved_global_var.global_var_sig.clone());
                    LocalIRValue::GlobalValue(global_value, resolved_global_var.global_var_sig.ty.clone().unwrap())
                }
                SymbolEntryKind::Struct(resolved_struct) => {
                    let struct_type =
                        self.get_or_declare_struct(resolved_struct.symbol_id, &resolved_struct.struct_sig);

                    LocalIRValue::Struct(struct_type)
                }
                SymbolEntryKind::Enum(resolved_enum) => {
                    LocalIRValue::Struct(self.get_or_declare_enum(resolved_enum.symbol_id, &resolved_enum.enum_sig))
                }
                SymbolEntryKind::Union(resolved_union) => {
                    LocalIRValue::Struct(self.get_or_declare_union(resolved_union.symbol_id, &resolved_union.union_sig))
                }
            },
        }
    }
}

fn get_elements_type_from_tuple_value(tuple_value: &TypedTupleValue) -> Vec<ConcreteType> {
    tuple_value
        .expr_list
        .iter()
        .map(|expr| expr.concrete_type.clone().unwrap())
        .collect()
}
