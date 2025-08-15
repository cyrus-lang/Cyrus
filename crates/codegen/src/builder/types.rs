use super::module::CodeGenBuilder;
use crate::builder::{
    module::LocalIRValue,
    values::{InternalValue, InternalValueKind},
};
use inkwell::{
    AddressSpace,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, PointerType},
    values::AnyValueEnum,
};
use resolver::scope::{LocalScopeRef, SymbolEntry};
use typed_ast::{
    SymbolID,
    types::{BasicConcreteType, ConcreteType, TypedArrayCapacity},
};

impl<'a> CodeGenBuilder<'a> {
    fn build_cast(&self, target_type: AnyTypeEnum<'a>, internal_value: InternalValue<'a>) -> AnyValueEnum<'a> {
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
            _ => panic!("Unsupported cast type."),
        }
    }

    pub(crate) fn build_implicit_cast(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        target_type: ConcreteType,
        rvalue: InternalValue<'a>,
    ) -> InternalValue<'a> {
        let target_any_type = self.build_concrete_type(local_scope_opt, target_type.clone());
        let any_value = self.build_cast(target_any_type, rvalue);
        InternalValue::new(target_type, InternalValueKind::RValue(any_value.try_into().unwrap()))
    }

    pub(crate) fn build_concrete_type_from_symbol_id(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> AnyTypeEnum<'a> {
        if let Some(local_scope) = local_scope_opt {
            todo!();
        } else {
            fn resolve_final_symbol_entry(this: &CodeGenBuilder<'_>, symbol_id: SymbolID) -> SymbolEntry {
                let module_id = this.resolver.lookup_symbol_id_in_modules(symbol_id).unwrap();
                let symbol_entry = this.resolver.lookup_symbol_entry_with_id(module_id, symbol_id).unwrap();

                match &symbol_entry {
                    SymbolEntry::Typedef(resolved_typedef) => match &resolved_typedef.typedef_sig.ty {
                        ConcreteType::Symbol(inner_type_symbol_id) => {
                            resolve_final_symbol_entry(this, *inner_type_symbol_id)
                        }
                        _ => symbol_entry.clone(),
                    },
                    _ => symbol_entry.clone(),
                }
            }

            let final_symbol_entry = resolve_final_symbol_entry(self, symbol_id);

            let irreg = self.irreg.borrow();
            let local_ir_value = irreg.get(&final_symbol_entry.get_symbol_id()).unwrap();

            let any_type_enum = match local_ir_value {
                LocalIRValue::Struct(struct_type) => AnyTypeEnum::StructType(struct_type.clone()),
                _ => unreachable!(),
            };

            drop(irreg);
            any_type_enum
        }
    }

    pub(crate) fn build_concrete_type(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        concrete_type: ConcreteType,
    ) -> AnyTypeEnum<'a> {
        match concrete_type {
            ConcreteType::Symbol(symbol_id) => self.build_concrete_type_from_symbol_id(local_scope_opt, symbol_id),
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
                let field_types: Vec<BasicTypeEnum<'_>> = typed_unnamed_struct_type
                    .fields
                    .iter()
                    .map(|field| {
                        self.build_concrete_type(local_scope_opt.clone(), *field.field_type.clone())
                            .try_into()
                            .unwrap()
                    })
                    .collect();

                AnyTypeEnum::StructType(self.llvmctx.struct_type(&field_types, typed_unnamed_struct_type.packed))
            }
        }
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
