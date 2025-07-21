use crate::context::CodeGenLLVM;
use crate::{
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    types::{InternalIntType, InternalType},
    values::{InternalValue, Lvalue},
};
use ast::token::{Location, TokenKind};
use inkwell::{
    AddressSpace,
    types::{BasicMetadataTypeEnum},
    values::{BasicMetadataValueEnum, IntValue, PointerValue},
};
use std::process::exit;

impl<'ctx> CodeGenLLVM<'ctx> {
    #[allow(unused)]
    pub(crate) fn build_runtime_inbounds_check(
        &mut self,
        pointer: PointerValue<'ctx>,
        pointee_ty: InternalType<'ctx>,
        index: InternalValue<'ctx>,
        array_length: u32,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let pointee_basic_ty = match pointee_ty.to_basic_type(ptr_type) {
            Ok(pointee_ty) => pointee_ty,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(err.to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        };

        let array_length_int_value = self.build_index_value(array_length.try_into().unwrap());

        let current_block = self.get_current_block("runtime check bounds", loc.clone(), span_end);
        let current_func = self.get_current_func("runtime check bounds", loc.clone(), span_end);
        let func_value = self.get_local_func_ir_value(current_func.local_ir_value_id);

        if self.is_block_terminated(current_block) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(
                    "Cannot build runtime_inbounds_check because current block is already terminated.".to_string(),
                ),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: loc.line,
                    column: loc.column,
                    length: span_end,
                }),
            });
            exit(1);
        }

        let failure_block = self.context.append_basic_block(func_value, "inbounds_check.failure");
        let success_block = self.context.append_basic_block(func_value, "inbounds_check.success");

        let index_implicit_casted = self.implicit_cast(
            index,
            InternalType::IntType(InternalIntType {
                type_str: "uint32".to_string(),
                int_kind: TokenKind::UInt32,
                int_type: array_length_int_value.get_type(),
            }),
            loc,
            span_end,
        );

        let compare_result = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SLT,
                index_implicit_casted.into_int_value(),
                array_length_int_value,
                "icmp",
            )
            .unwrap();

        self.builder
            .build_conditional_branch(compare_result, success_block, failure_block);
        self.mark_block_terminated(current_block, false);

        self.builder.position_at_end(failure_block);

        let panic_msg = self.build_global_str(format!(
            "panic: Index out of bounds!\nAttempted to access index %d in an array of size {}.",
            array_length
        ), Location::default(), 0).0;

        let module = self.module.borrow_mut();

        // call fprintf to display panic message

        let void_type = self.context.void_type();
        let i32_type = self.context.i32_type();
        let fprintf_type = i32_type.fn_type(
            &[
                BasicMetadataTypeEnum::from(ptr_type), // FILE *stream
                BasicMetadataTypeEnum::from(ptr_type), // const char *format
            ],
            true,
        );
        let fprintf = module.add_function("fprintf", fprintf_type, None);

        let stderr_global = module.add_global(ptr_type, None, "stderr");
        stderr_global.set_linkage(inkwell::module::Linkage::External);
        let stderr_val = self
            .builder
            .build_load(ptr_type, stderr_global.as_pointer_value(), "stderr_val")
            .unwrap();

        self.builder
            .build_call(
                fprintf,
                &[
                    BasicMetadataValueEnum::PointerValue(stderr_val.into_pointer_value()),
                    BasicMetadataValueEnum::PointerValue(panic_msg.as_pointer_value()),
                    index_implicit_casted.into(),
                ],
                "call",
            )
            .unwrap();

        // exit program with status code 1

        let error_status_code = i32_type.const_int(1, false);
        let exit_type = void_type.fn_type(
            &[
                BasicMetadataTypeEnum::from(i32_type), // int status
            ],
            false,
        );
        let exit = module.add_function("exit", exit_type, None);

        self.builder
            .build_call(exit, &[error_status_code.into()], "call")
            .unwrap();

        self.builder.build_unreachable().unwrap();

        self.builder.position_at_end(success_block);
        self.block_registry.current_block_ref = Some(success_block);

        let ordered_indexes: Vec<IntValue<'ctx>> =
            vec![self.build_index_value(0), index_implicit_casted.into_int_value()];
        let gep = unsafe {
            self.builder
                .build_in_bounds_gep(pointee_basic_ty, pointer, &ordered_indexes, "gep")
                .unwrap()
        };

        InternalValue::Lvalue(Lvalue { ptr: gep, pointee_ty })
    }
}
