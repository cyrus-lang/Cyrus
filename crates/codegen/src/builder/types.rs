use crate::builder::module::LocalIRValue;

use super::module::CodeGenBuilder;
use inkwell::{
    AddressSpace,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, PointerType},
};
use typed_ast::{
    ScopeID, SymbolID,
    types::{BasicConcreteType, ConcreteType, TypedArrayCapacity},
};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_concrete_type_from_symbol_id(
        &self,
        local_scope_opt: Option<ScopeID>,
        symbol_id: SymbolID,
    ) -> AnyTypeEnum<'a> {
        if let Some(local_scope) = local_scope_opt {
            todo!();
        } else {
            let module_id = self.resolver.lookup_symbol_id_in_modules(symbol_id).unwrap();
            let symbol_entry = self.resolver.lookup_symbol_entry_with_id(module_id, symbol_id).unwrap();
            let irreg = self.irreg.borrow();
            let local_ir_value = irreg.get(&symbol_entry.get_symbol_id()).unwrap();

            let any_type_enum = match local_ir_value {
                LocalIRValue::Func(_) => unreachable!(),
                LocalIRValue::GlobalValue(_) => unreachable!(),
                LocalIRValue::Struct(struct_type) => AnyTypeEnum::StructType(struct_type.clone()),
            };

            drop(irreg);
            any_type_enum
        }
    }

    pub(crate) fn build_concrete_type(
        &self,
        local_scope_opt: Option<ScopeID>,
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
                        self.build_concrete_type(local_scope_opt, *field.field_type.clone())
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
