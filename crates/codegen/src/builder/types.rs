use super::module::CodeGenBuilder;
use crate::builder::{
    module::LocalIRValue,
    values::{InternalValue, InternalValueKind},
};
use inkwell::{
    AddressSpace,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, PointerType},
    values::{AnyValue, AnyValueEnum},
};
use resolver::scope::{LocalOrGlobalSymbol, LocalScopeRef, LocalSymbolKind, SymbolEntry, SymbolEntryKind};
use typed_ast::{
    SymbolID,
    types::{BasicConcreteType, ConcreteType, ResolvedSymbol, TypedArrayCapacity, TypedUnnamedStructType},
};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_cast(
        &self,
        target_type: AnyTypeEnum<'a>,
        internal_value: InternalValue<'a>,
    ) -> AnyValueEnum<'a> {
        let basic_value = internal_value.as_basic_value();

        match target_type {
            AnyTypeEnum::IntType(int_type) => AnyValueEnum::IntValue(
                self.llvmbuilder
                    .build_int_cast(basic_value.into_int_value(), int_type, "cast")
                    .unwrap(),
            ),
            AnyTypeEnum::FloatType(float_type) => AnyValueEnum::FloatValue(
                self.llvmbuilder
                    .build_float_cast(basic_value.into_float_value(), float_type, "cast")
                    .unwrap(),
            ),
            AnyTypeEnum::PointerType(ptr_type) => AnyValueEnum::PointerValue(
                self.llvmbuilder
                    .build_pointer_cast(basic_value.into_pointer_value(), ptr_type, "cast")
                    .unwrap(),
            ),
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

            dbg!(local_or_global_symbol.clone());
            todo!();

            // let type_symbol_id = match local_or_global_symbol {
            //     LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match local_symbol.kind {
            //         LocalSymbolKind::Variable(resolved_variable) => {
            //             todo!();
            //             // self.build_concrete_type(local_scope_opt, resolved_variable.typed_variable.ty.clone().unwrap())
            //         }
            //         LocalSymbolKind::Typedef(resolved_typedef) => {
            //             return self.build_concrete_type(local_scope_opt, resolved_typedef.typedef_sig.ty);
            //         }
            //         _ => local_symbol.get_symbol_id(),
            //     },
            //     LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match symbol_entry.kind {
            //         SymbolEntryKind::Typedef(resolved_typedef) => {
            //             return self.build_concrete_type(local_scope_opt, resolved_typedef.typedef_sig.ty);
            //         }
            //         _ => symbol_entry.get_symbol_id(),
            //     },
            // };

            // let irreg = self.irreg.borrow();
            // let local_ir_value = irreg.get(&type_symbol_id).unwrap();

            // let any_type_enum = match local_ir_value {
            //     LocalIRValue::Struct(struct_type) => AnyTypeEnum::StructType(struct_type.clone()),
            //     _ => unreachable!(),
            // };

            // drop(irreg);
            // any_type_enum
        } else {
            let module_id = self.resolver.lookup_symbol_id_in_modules(symbol_id).unwrap();
            let symbol_entry = self.resolver.lookup_symbol_entry_with_id(module_id, symbol_id).unwrap();

            let any_type_enum = {
                match symbol_entry.kind {
                    SymbolEntryKind::Typedef(resolved_typedef) => {
                        self.build_concrete_type(local_scope_opt, resolved_typedef.typedef_sig.ty)
                    }
                    _ => {
                        let irreg = self.irreg.borrow();
                        let local_ir_value = irreg.get(&symbol_entry.get_symbol_id()).unwrap();
                        match local_ir_value {
                            LocalIRValue::Struct(struct_type) => AnyTypeEnum::StructType(struct_type.clone()),
                            _ => unreachable!(),
                        }
                    }
                }
            };

            any_type_enum
        }
    }

    pub(crate) fn build_concrete_type(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        concrete_type: ConcreteType,
    ) -> AnyTypeEnum<'a> {
        match concrete_type {
            ConcreteType::UnresolvedSymbol(..) => unreachable!(),
            ConcreteType::ResolvedSymbol(resolved_symbol) => match resolved_symbol {
                ResolvedSymbol::Enum(symbol_id)
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
                    .build_concrete_type(local_scope_opt, *typed_array_type.element_type)
                    .try_into()
                    .unwrap();

                let array_capacity = match typed_array_type.capacity {
                    TypedArrayCapacity::Fixed(fixed) => fixed,
                    TypedArrayCapacity::Dynamic => todo!(),
                };
                AnyTypeEnum::ArrayType(element_type.array_type(array_capacity))
            }
            ConcreteType::Const(concrete_type) => self.build_concrete_type(local_scope_opt, *concrete_type),
            ConcreteType::Pointer(_) => {
                let ptr: PointerType<'a> = self.llvmctx.ptr_type(AddressSpace::default());
                AnyTypeEnum::PointerType(ptr)
            }
            ConcreteType::UnnamedStruct(typed_unnamed_struct_type) => {
                self.build_unnamed_struct_type(local_scope_opt, &typed_unnamed_struct_type)
            }
        }
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
}
