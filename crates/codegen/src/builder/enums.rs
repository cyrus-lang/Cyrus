use crate::builder::{abi::generate_enum_variant_abi_name, module::CodeGenBuilder};
use inkwell::{
    module::Linkage,
    types::{AnyType, BasicTypeEnum},
};
use typed_ast::{TypedEnum, TypedEnumVariant};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_enum_def(&mut self, typed_enum: &TypedEnum) {
        let local_scope_opt = None;

        let enum_name = typed_enum.name.clone();
        let enum_opaque_struct = {
            let irreg = self.irreg.borrow();
            let local_ir_value = irreg.get(&typed_enum.symbol_id).unwrap();
            let struct_type = local_ir_value.as_struct().unwrap().clone();
            drop(irreg);
            struct_type
        };

        let mut enum_iota = 0;
        let mut payload_size = 0;

        typed_enum.variants.iter().for_each(|variant| {
            match variant {
                TypedEnumVariant::Identifier(_) => {
                    // no payload
                }
                TypedEnumVariant::Valued(identifier, typed_expr) => {
                    let lvalue = self.build_expr(local_scope_opt.clone(), &**typed_expr);
                    let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
                    let basic_rvalue = rvalue.as_basic_value();

                    let store_size = {
                        self.llvmtm
                            .get_target_data()
                            .get_store_size(&basic_rvalue.get_type().as_any_type_enum())
                    };

                    if store_size > payload_size {
                        payload_size = store_size;
                    }

                    let module = self.llvmmodule.borrow_mut();
                    let global_value = module.add_global(
                        basic_rvalue.get_type(),
                        None,
                        &generate_enum_variant_abi_name(
                            self.get_module_name(self.module_id),
                            enum_name.clone(),
                            identifier.name.clone(),
                        ),
                    );
                    global_value.set_linkage(Linkage::External);
                    global_value.set_initializer(&basic_rvalue);
                    global_value.set_constant(true);
                    drop(module);
                }
                TypedEnumVariant::Variant(_, enum_valued_fields) => {
                    let variant_fields: Vec<BasicTypeEnum<'a>> = enum_valued_fields
                        .iter()
                        .map(|enum_valued_field| {
                            TryInto::<BasicTypeEnum<'a>>::try_into(
                                self.build_concrete_type(local_scope_opt.clone(), enum_valued_field.field_type.clone()),
                            )
                            .unwrap()
                        })
                        .collect();

                    let struct_type = self.llvmctx.struct_type(&variant_fields, false);
                    let store_size = { self.llvmtm.get_target_data().get_store_size(&struct_type) };
                    if store_size > payload_size {
                        payload_size = store_size;
                    }
                }
            }

            enum_iota += 1;
        });

        let payload_type = self.llvmctx.i8_type().array_type(payload_size.try_into().unwrap());
        let field_types: &[BasicTypeEnum<'a>; 2] = &[
            BasicTypeEnum::IntType(self.llvmctx.i32_type()),
            BasicTypeEnum::ArrayType(payload_type),
        ];
        enum_opaque_struct.set_body(field_types, false);
    }

    pub(crate) fn build_local_enum_def(&self, _typed_enum: &TypedEnum) {
        todo!();
    }
}
