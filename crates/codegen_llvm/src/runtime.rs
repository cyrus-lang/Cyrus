use crate::CodeGenLLVM;
use inkwell::{
    module::Linkage, types::{BasicMetadataTypeEnum, PointerType}, values::{AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue}, AddressSpace
};
use utils::generate_random_hex::generate_random_hex;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn load_runtime(&mut self) {
        self.internal_funcs_table
            .insert("malloc".to_string(), self.internal_malloc().as_value_ref());

        self.internal_memcpy();
        self.internal_load_string();
    }

    fn internal_load_string(&self) {
        let malloc_fn = self.module.get_function("malloc").unwrap();
        let memcpy_fn = self.module.get_function("llvm.memcpy.p0i8.p0i8.i64").unwrap();

        if self.module.get_function("internal_load_string").is_none() {
            let param_types = &[
                BasicMetadataTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default())),
                BasicMetadataTypeEnum::IntType(self.context.i64_type()),
            ];
            let func_type = self.context.ptr_type(AddressSpace::default()).fn_type(param_types, false);
            let func = self.module.add_function("internal_load_string", func_type, None);
            let entry_block = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry_block);

            let ptr_param = func.get_nth_param(0).unwrap().into_pointer_value();
            let len_param = func.get_nth_param(1).unwrap().into_int_value();

            let malloc_call = self
                .builder
                .build_call(malloc_fn, &[len_param.into()], "malloc_call")
                .unwrap();

            let malloc_ptr = malloc_call.try_as_basic_value().left().unwrap().into_pointer_value();

            let dst_ptr = self.builder.build_pointer_cast(
                malloc_ptr,
                self.context.ptr_type(AddressSpace::default()),
                "dst_ptr",
            ).unwrap();

            self
                .builder
                .build_call(memcpy_fn, &[
                    BasicMetadataValueEnum::PointerValue(dst_ptr),
                    BasicMetadataValueEnum::PointerValue(ptr_param),
                    BasicMetadataValueEnum::IntValue(len_param),
                    self.context.bool_type().const_zero().into()
                ], "memcpy_call")
                .unwrap();


            // TODO

            self.builder.build_return(Some(&malloc_ptr.as_basic_value_enum())).unwrap();
        }
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

    fn internal_memcpy(&self) -> FunctionValue<'ctx> {
        let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();
        let bool_type = self.context.bool_type();
    
        // memcpy: void (i8* dest, i8* src, i64 len, i1 is_volatile)
        let memcpy_type = self.context.void_type().fn_type(
            &[
                i8_ptr_type.into(), // dest
                i8_ptr_type.into(), // src
                i64_type.into(),    // len
                bool_type.into(),   // isVolatile
            ],
            false,
        );
    
        match self.module.get_function("llvm.memcpy.p0i8.p0i8.i64") {
            Some(f) => f,
            None => self.module.add_function("llvm.memcpy.p0i8.p0i8.i64", memcpy_type, None),
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
