use inkwell::types::{AnyType, BasicTypeEnum, StructType};
use resolver::{sigs::UnionSig, typed_union_as_union_sig};
use tast::TypedUnionStmt;

use crate::builder::{abi::generate_union_abi_name, module::CodeGenBuilder};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_union_def(&mut self, typed_union: &TypedUnionStmt) {
        if typed_union.generic_params.is_none() {
            self.get_or_declare_union(typed_union.symbol_id, &typed_union_as_union_sig(typed_union));
            self.build_methods(typed_union.module_id, &typed_union.methods);
        } else {
            // generic union is generated at use time
        }
    }

    pub(crate) fn build_union_type(
        &mut self,
        union_sig: &UnionSig,
        custom_name: Option<String>,
    ) -> StructType<'a> {
        let llvm_struct_name = custom_name.unwrap_or(generate_union_abi_name(
            &self.get_module_name(self.module_id),
            &union_sig.name.clone(),
        ));
        let union_opaque_struct = self.llvmctx.opaque_struct_type(&llvm_struct_name);

        let field_types: Vec<BasicTypeEnum<'a>> = union_sig
            .fields
            .iter()
            .map(|field| self.build_concrete_type(None, field.ty.clone()).try_into().unwrap())
            .collect();

        let mut largest_field_type: BasicTypeEnum = BasicTypeEnum::IntType(self.llvmctx.bool_type());

        field_types.iter().for_each(|basic_type| {
            let largest_store_size = self
                .llvmtm
                .get_target_data()
                .get_store_size(&largest_field_type.as_any_type_enum());

            let field_store_size = self
                .llvmtm
                .get_target_data()
                .get_store_size(&basic_type.as_any_type_enum());

            if field_store_size > largest_store_size {
                largest_field_type = basic_type.clone();
            }
        });

        union_opaque_struct.set_body(&[largest_field_type], true);
        union_opaque_struct
    }
}
