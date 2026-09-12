// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    module::Module,
    values::{FunctionValue, PointerValue},
};

pub struct LLVMLifetimeMarkers<'ll> {
    start_fn: FunctionValue<'ll>,
    end_fn: FunctionValue<'ll>,
}

impl<'ll> LLVMLifetimeMarkers<'ll> {
    /// Declares or retrieves `llvm.lifetime.start.p0` and `llvm.lifetime.end.p0`
    #[inline]
    pub fn get_or_insert(context: &'ll Context, module: &Module<'ll>) -> Self {
        let void_type = context.void_type();

        let ptr_type = context.ptr_type(AddressSpace::default());

        let fn_type = void_type.fn_type(&[ptr_type.into()], false);

        let start_fn = module
            .get_function("llvm.lifetime.start.p0")
            .unwrap_or_else(|| module.add_function("llvm.lifetime.start.p0", fn_type, None));

        let end_fn = module
            .get_function("llvm.lifetime.end.p0")
            .unwrap_or_else(|| module.add_function("llvm.lifetime.end.p0", fn_type, None));

        Self { start_fn, end_fn }
    }

    #[inline]
    pub fn emit_start(&self, builder: &Builder<'ll>, ptr: PointerValue<'ll>) {
        builder.build_direct_call(self.start_fn, &[ptr.into()], "").unwrap();
    }

    #[inline]
    pub fn emit_end(&self, builder: &Builder<'ll>, ptr: PointerValue<'ll>) {
        builder.build_direct_call(self.end_fn, &[ptr.into()], "").unwrap();
    }
}
