use crate::CodeGenLLVM;
use ast::{
    ast::{FuncDecl, FuncParams, StorageClass},
    token::{Location, Span},
};
use inkwell::{
    AddressSpace,
    module::Linkage,
    types::BasicMetadataTypeEnum,
    values::{BasicValueEnum, FunctionValue},
};
use std::ops::DerefMut;
use utils::generate_random_hex::generate_random_hex;

impl<'ctx> CodeGenLLVM<'ctx> {
    #[allow(unused)]
    fn runtime_check_bounds(&self) -> FunctionValue<'ctx> {
        let return_type = self.context.i32_type();
        
        let func_type = return_type.fn_type(
            &[
                BasicMetadataTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default())),
                BasicMetadataTypeEnum::IntType(self.context.i32_type()),
            ],
            false,
        );

        let func = self.module.borrow_mut().deref_mut().add_function(
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
