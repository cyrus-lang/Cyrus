// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::builder::{builder::CodeGenIRBuilder, irreg::LocalIRValue};
use cyrusc_internal::{cir::types::cir_fat_ptr_type, vtable::VTableInfo};
use inkwell::{
    AddressSpace,
    module::Linkage,
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum, PointerValue},
};

impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_vtable_decls(&mut self) {
        for vtable_info in self.vtable_registry.iter() {
            let irv_id = vtable_info.vtable_irv_id.unwrap();

            let method_decls = vtable_info.cir_method_decls.as_ref().unwrap();

            let ptr_type = self.llvmctx.ptr_type(AddressSpace::default());

            let field_types: Vec<BasicTypeEnum<'ll>> = method_decls.iter().map(|_| ptr_type.into()).collect();

            let vtable_struct_type = self.llvmctx.struct_type(&field_types, false);

            let global_value = {
                let llvmmodule = self.llvmmodule.borrow_mut();
                llvmmodule.add_global(vtable_struct_type, None, &vtable_info.abi_name.unwrap())
            };

            global_value.set_linkage(Linkage::Internal);

            self.insert_local_ir_value(
                irv_id,
                LocalIRValue::Global(global_value, cir_fat_ptr_type(vtable_info.loc)),
            );
        }
    }

    pub(crate) fn emit_vtable_defs(&mut self) {
        for vtable_info in self.vtable_registry.iter() {
            let mut fn_ptr_constants = Vec::with_capacity(vtable_info.method_decls.len());

            for (idx, _) in vtable_info.method_decls.iter().enumerate() {
                let fn_ptr = {
                    if let Some(Some(monomorph_id)) = vtable_info.monomorphized_methods.get(idx) {
                        // method is generic, use monomorphized
                        let irv_id = self
                            .cir_module
                            .monomorph_to_ir_value_map
                            .get(monomorph_id)
                            .copied()
                            .unwrap();

                        let ir_value = self.lookup_local_ir_value(irv_id).unwrap();
                        let llvm_func_value = ir_value.as_func().unwrap();
                        llvm_func_value.as_global_value().as_pointer_value()
                    } else {
                        // method is concrete, use cir_method_decls
                        let irv_id = vtable_info.cir_method_decls.as_ref().unwrap()[idx];

                        let llvm_func_value = self.get_or_declare_function(irv_id).as_func().cloned().unwrap();
                        llvm_func_value.as_global_value().as_pointer_value()
                    }
                };

                fn_ptr_constants.push(fn_ptr.into());
            }

            self.emit_single_vtable(&vtable_info, fn_ptr_constants);
        }
    }

    fn emit_single_vtable(&mut self, vtable_info: &VTableInfo, fn_ptrs: Vec<PointerValue<'ll>>) {
        let ir_value = self.lookup_local_ir_value(vtable_info.vtable_irv_id.unwrap()).unwrap();
        let global_value = ir_value.as_global().unwrap();

        let struct_values: Vec<BasicValueEnum<'ll>> = fn_ptrs.iter().map(|ptr| ptr.as_basic_value_enum()).collect();
        let struct_const = self.llvmctx.const_struct(&struct_values, false);

        global_value.set_initializer(&struct_const);
        global_value.set_constant(true);
    }
}
