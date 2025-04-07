use crate::CodeGenLLVM;
use inkwell::{
    AddressSpace,
    module::Linkage,
    types::BasicMetadataTypeEnum,
    values::{AsValueRef, BasicValueEnum, FunctionValue},
};
use utils::generate_random_hex::generate_random_hex;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn load_runtime(&mut self) {
        self.internal_funcs_table
            .insert("malloc".to_string(), self.internal_malloc().as_value_ref());
    }
    
    fn internal_malloc(&self) -> FunctionValue<'ctx> {
        let i8_ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let i64_type = self.context.i64_type();

        let malloc_type = i8_ptr_type.fn_type(&[i64_type.into()], false);

        match self.module.get_function("malloc") {
            Some(v) => v,
            None => self.module.add_function("malloc", malloc_type, None),
        }
    }

    fn internal_check_bounds(&self) -> FunctionValue<'ctx> {
        let return_type = self.context.i32_type();
        let func_type = return_type.fn_type(
            &[
                BasicMetadataTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default())),
                BasicMetadataTypeEnum::IntType(self.context.i32_type()),
            ],
            false,
        );
        let func = self.module.add_function(
            &format!("check_bounds_{}", generate_random_hex()),
            func_type,
            Some(Linkage::Private),
        );

        let entry_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry_block);
        self.builder
            .build_return(Some(&BasicValueEnum::IntValue(
                self.context.i32_type().const_int(0, false),
            )))
            .unwrap();

        func
    }
}
