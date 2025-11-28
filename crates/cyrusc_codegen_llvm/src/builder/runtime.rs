use crate::builder::{
    builder::IRBuilderCtx,
    values::{InternalValue, InternalValueKind},
};
use cyrusc_cir::types::CIRTy;
use inkwell::{
    AddressSpace,
    types::{BasicMetadataTypeEnum, BasicTypeEnum},
    values::{BasicMetadataValueEnum, IntValue, PointerValue},
};

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_inbounds_checked_array_index(
        &mut self,
        ptr: PointerValue<'ll>,
        pointee_ty: CIRTy,
        index: InternalValue<'ll>,
        array_length: u32,
    ) -> InternalValue<'ll> {
        let pointee_basic_ty: BasicTypeEnum<'ll> = self.emit_ty(pointee_ty.clone()).try_into().unwrap();

        let target_data = self.llvmtm.get_target_data();
        let ptr_sized_int_type = self.llvmctx.ptr_sized_int_type(&target_data, None);
        let mut array_length_int_value = ptr_sized_int_type.const_int(array_length.into(), false);

        let mut index_int_value = index.as_basic_value().into_int_value();

        // implicit cast index and length type
        if index_int_value.get_type().get_bit_width() > array_length_int_value.get_type().get_bit_width() {
            array_length_int_value = self
                .llvmbuilder
                .build_int_cast(array_length_int_value, index_int_value.get_type(), "cast")
                .unwrap();
        } else {
            index_int_value = self
                .llvmbuilder
                .build_int_cast(index_int_value, array_length_int_value.get_type(), "cast")
                .unwrap();
        }

        let compare_result = self
            .llvmbuilder
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

        let cur_fn = self.cur_fn.unwrap();

        let failure_block = self.llvmctx.append_basic_block(cur_fn, "inbounds_check.failure");
        let success_block = self.llvmctx.append_basic_block(cur_fn, "inbounds_check.success");

        self.llvmbuilder
            .build_conditional_branch(compare_result, success_block, failure_block)
            .unwrap();

        self.llvmbuilder.position_at_end(failure_block);

        let panic_msg = self.emit_cstring(format!(
            "panic: Index out of bounds!\nAttempted to access index %d in an array of size {}.",
            array_length
        ));

        let module = self.llvmmodule.borrow_mut();

        // call fprintf to display panic message

        let ptr_type = self.llvmctx.ptr_type(AddressSpace::default());

        let void_type = self.llvmctx.void_type();
        let i32_type = self.llvmctx.i32_type();
        let fprintf_type = i32_type.fn_type(
            &[
                BasicMetadataTypeEnum::from(ptr_type), // FILE *stream
                BasicMetadataTypeEnum::from(ptr_type), // const char *format
            ],
            true,
        );

        let fprintf_fn_value = match module.get_function("fprintf") {
            Some(fn_value) => fn_value,
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
            .llvmbuilder
            .build_load(ptr_type, stderr_global.as_pointer_value(), "stderr_val")
            .unwrap();

        self.llvmbuilder
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
            Some(fn_value) => fn_value,
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

        self.llvmbuilder
            .build_call(exit_fn_value, &[error_status_code.into()], "call")
            .unwrap();

        self.llvmbuilder.build_unreachable().unwrap();

        self.llvmbuilder.position_at_end(success_block);
        self.blockreg.cur_block = Some(success_block);

        let ordered_indexes: Vec<IntValue<'ll>> = vec![index.as_basic_value().into_int_value()];

        let pointer_value = unsafe {
            self.llvmbuilder
                .build_in_bounds_gep(pointee_basic_ty, ptr, &ordered_indexes, "gep")
                .unwrap()
        };

        InternalValue::new(pointee_ty, InternalValueKind::LValue(pointer_value))
    }
}
