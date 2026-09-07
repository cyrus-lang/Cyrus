// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::builder::{
    builder::CodeGenIRBuilder,
    values::{InternalValue, InternalValueKind},
};
use cyrusc_internal::cir::types::CIRType;
use inkwell::{
    AddressSpace,
    module::Linkage,
    types::{BasicMetadataTypeEnum, BasicTypeEnum},
    values::{BasicMetadataValueEnum, IntValue, PointerValue},
};

const GLOBAL_VAR_CTORS_FN_NAME: &str = "__cyrus_global_var_ctors";

impl<'ll> CodeGenIRBuilder<'ll> {
    pub fn emit_global_var_ctors_function(&mut self) {
        if self.global_var_lazy_initializers.is_empty() {
            return;
        }

        if self
            .llvm_module
            .borrow()
            .get_function(GLOBAL_VAR_CTORS_FN_NAME)
            .is_some()
        {
            return;
        }

        let void_type = self.llvm_ctx.void_type();
        let ctor_fn_type = void_type.fn_type(&[], false);

        let llvm_func =
            self.llvm_module
                .borrow()
                .add_function(GLOBAL_VAR_CTORS_FN_NAME, ctor_fn_type, Some(Linkage::Internal));

        let entry_block = self.llvm_ctx.append_basic_block(llvm_func, "entry");
        self.llvm_builder.position_at_end(entry_block);

        for ctor in std::mem::take(&mut self.global_var_lazy_initializers) {
            let lvalue = self.emit_expr(&ctor.expr, &None);
            let rvalue = self.load_rvalue(lvalue);
            self.emit_store(ctor.global_value.as_pointer_value(), rvalue, ctor.expr.ty.clone());
        }

        self.llvm_builder.position_at_end(entry_block);
        self.llvm_builder.build_return(None).unwrap();

        let i32_type = self.llvm_ctx.i32_type();
        let ptr_type = self.llvm_ctx.ptr_type(inkwell::AddressSpace::default());

        let priority = i32_type.const_int(65535, false);
        let fn_ptr = llvm_func.as_global_value().as_pointer_value();
        let null_data = ptr_type.const_null();

        let ctor_struct_val = self
            .llvm_ctx
            .const_struct(&[priority.into(), fn_ptr.into(), null_data.into()], false);

        // create 1-element array constant: [1 x { i32, void*, ptr }]
        let ctor_struct_type = ctor_struct_val.get_type();
        let ctor_array_type = ctor_struct_type.array_type(1);
        let ctor_array_val = ctor_struct_type.const_array(&[ctor_struct_val]);

        // emit the appending @llvm.global_ctors global
        let global_ctors =
            self.llvm_module
                .borrow()
                .add_global(ctor_array_type, Some(AddressSpace::default()), "llvm.global_ctors");

        global_ctors.set_linkage(Linkage::Appending);
        global_ctors.set_initializer(&ctor_array_val);
    }

    pub(crate) fn emit_inbounds_checked_array_index(
        &mut self,
        ptr: PointerValue<'ll>,
        pointee_ty: CIRType,
        index: InternalValue<'ll>,
        array_length: u32,
    ) -> InternalValue<'ll> {
        let pointee_basic_ty: BasicTypeEnum<'ll> = self.emit_type(pointee_ty.clone()).try_into().unwrap();

        let target_data = self.llvm_target_machine.get_target_data();
        let ptr_sized_int_type = self.llvm_ctx.ptr_sized_int_type(&target_data, None);
        let mut array_length_int_value = ptr_sized_int_type.const_int(array_length.into(), false);

        let mut index_int_value = index.as_basic_value().into_int_value();

        // implicit cast index and length type
        if index_int_value.get_type().get_bit_width() > array_length_int_value.get_type().get_bit_width() {
            array_length_int_value = self
                .llvm_builder
                .build_int_cast(array_length_int_value, index_int_value.get_type(), "cast")
                .unwrap();
        } else {
            index_int_value = self
                .llvm_builder
                .build_int_cast(index_int_value, array_length_int_value.get_type(), "cast")
                .unwrap();
        }

        let compare_result = self
            .llvm_builder
            .build_int_compare(
                inkwell::IntPredicate::ULT,
                index_int_value,
                array_length_int_value,
                "cmp",
            )
            .unwrap();

        if let Some(const_val) = compare_result.get_zero_extended_constant() {
            if const_val == 1 {
                // already true
                return self.emit_array_index_on_pointer(ptr, index, pointee_ty.clone());
            }
        }

        let cur_fn = self.cur_func.unwrap();

        let failure_block = self.llvm_ctx.append_basic_block(cur_fn, "inbounds_check.failure");
        let success_block = self.llvm_ctx.append_basic_block(cur_fn, "inbounds_check.success");

        self.llvm_builder
            .build_conditional_branch(compare_result, success_block, failure_block)
            .unwrap();

        self.llvm_builder.position_at_end(failure_block);

        let panic_msg = self.emit_const_str(format!(
            "panic: Index out of bounds!\nAttempted to access index %d in an array of size {}.",
            array_length
        ));

        let module = self.llvm_module.borrow_mut();

        // call fprintf to display panic message

        let ptr_type = self.llvm_ctx.ptr_type(AddressSpace::default());

        let void_type = self.llvm_ctx.void_type();
        let i32_type = self.llvm_ctx.i32_type();
        let fprintf_type = i32_type.fn_type(
            &[
                BasicMetadataTypeEnum::from(ptr_type), // FILE *stream
                BasicMetadataTypeEnum::from(ptr_type), // const char *format
            ],
            true,
        );

        let fprintf_fn_value = match module.get_function("fprintf") {
            Some(llvm_func_value) => llvm_func_value,
            None => module.add_function("fprintf", fprintf_type, None),
        };

        let stderr_global = match module.get_global("stderr") {
            Some(global_value) => global_value,
            None => {
                let global_value = module.add_global(ptr_type, None, "stderr");
                global_value.set_linkage(inkwell::module::Linkage::External);
                global_value
            }
        };

        let stderr_val = self
            .llvm_builder
            .build_load(ptr_type, stderr_global.as_pointer_value(), "stderr_val")
            .unwrap();

        self.llvm_builder
            .build_call(
                fprintf_fn_value,
                &[
                    BasicMetadataValueEnum::PointerValue(stderr_val.into_pointer_value()),
                    BasicMetadataValueEnum::PointerValue(panic_msg.into_pointer_value()),
                    index.as_basic_value().into(),
                ],
                "call",
            )
            .unwrap();

        // exit program with status code 1

        let error_status_code = i32_type.const_int(1, false);

        let exit_fn_value = match module.get_function("exit") {
            Some(llvm_func_value) => llvm_func_value,
            None => {
                let exit_fn_type = void_type.fn_type(
                    &[
                        BasicMetadataTypeEnum::from(i32_type), // int status
                    ],
                    false,
                );
                module.add_function("exit", exit_fn_type, None)
            }
        };

        self.llvm_builder
            .build_call(exit_fn_value, &[error_status_code.into()], "call")
            .unwrap();

        self.llvm_builder.build_unreachable().unwrap();

        self.llvm_builder.position_at_end(success_block);
        self.block_reg.cur_block = Some(success_block);

        let ordered_indexes: Vec<IntValue<'ll>> = vec![index.as_basic_value().into_int_value()];

        let pointer_value = unsafe {
            self.llvm_builder
                .build_in_bounds_gep(pointee_basic_ty, ptr, &ordered_indexes, "gep")
                .unwrap()
        };

        InternalValue::new(pointee_ty, InternalValueKind::LValue(pointer_value))
    }
}
