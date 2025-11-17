use crate::{
    builder::{
        builder::IRBuilderCtx,
        irreg::LocalIRValue,
        values::{InternalValue, InternalValueKind},
    },
    llvm::abi::modifiers::apply_func_modifiers,
};
use cyrusc_abi::{inline::Inlining, modifiers::FuncModifiers};
use cyrusc_cir::{CIRBlockStmt, CIRFuncDeclStmt, CIRFuncParams, CIRLambda, cir_func_decl_as_func_ty, types::CIRTy};
use cyrusc_tast::types::PlainType;
use inkwell::{
    types::BasicTypeEnum,
    values::{FunctionValue, PointerValue},
};

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_fn_as_ptr(&self, fn_value: FunctionValue<'ll>) -> PointerValue<'ll> {
        fn_value.as_global_value().as_pointer_value()
    }

    pub(crate) fn emit_func_params(&self, func_params: &CIRFuncParams) {
        func_params.list.iter().enumerate().for_each(|(param_idx, param)| {
            let basic_value = self
                .cur_fn
                .unwrap()
                .get_nth_param(param_idx.try_into().unwrap())
                .unwrap();

            let mut irreg = self.irreg.borrow_mut();
            if basic_value.is_pointer_value() {
                irreg.insert(
                    param.irv_id,
                    LocalIRValue::LValue(basic_value.into_pointer_value(), param.ty.clone()),
                );
            } else {
                let ty: BasicTypeEnum<'ll> = self.emit_ty(param.ty.clone()).try_into().unwrap();
                let ptr = self.llvmbuilder.build_alloca(ty, "param").unwrap();
                self.llvmbuilder.build_store(ptr, basic_value).unwrap();

                irreg.insert(param.irv_id, LocalIRValue::LValue(ptr, param.ty.clone()));
            }
            drop(irreg);
        });
    }

    pub(crate) fn emit_lambda(&mut self, lambda: &CIRLambda) -> InternalValue<'ll> {
        let parent_fn = self.cur_fn.clone();
        let parent_block = self.blockreg.cur_block;

        let mut modifiers = FuncModifiers::default();
        modifiers.inline = Some(Inlining::Inline);
        let func_decl = CIRFuncDeclStmt {
            irv_id: lambda.irv_id,
            name: "lambda".to_string(),
            params: lambda.params.clone(),
            ret: lambda.ret.clone(),
            modifiers,
        };
        let fn_value = self.emit_func_decl(&func_decl);
        self.cur_fn = Some(fn_value);

        self.emit_func_body(&lambda.params, &lambda.body);

        let fn_pointer = fn_value.as_global_value().as_pointer_value().into();

        self.cur_fn = parent_fn;
        self.blockreg.cur_block = parent_block;
        if let Some(basic_block) = parent_block {
            self.llvmbuilder.position_at_end(basic_block);
        }

        InternalValue::new(
            CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))),
            InternalValueKind::RValue(fn_pointer),
        )
    }

    pub(crate) fn emit_func_decl(&mut self, func_decl: &CIRFuncDeclStmt) -> FunctionValue<'ll> {
        let fn_type = self.emit_func_ty(cir_func_decl_as_func_ty(func_decl));

        let func_name = &func_decl.name;
        let llvmmodule = self.llvmmodule.borrow();

        let fn_value = llvmmodule
            .get_function(func_name)
            .unwrap_or_else(|| llvmmodule.add_function(func_name, fn_type, None));

        apply_func_modifiers(self.llvmctx, &fn_value, &func_decl.modifiers);

        {
            let cir_func_ty = cir_func_decl_as_func_ty(func_decl);
            let mut irreg = self.irreg.borrow_mut();
            irreg.insert(
                func_decl.irv_id,
                LocalIRValue::Func(fn_value, CIRTy::FuncType(cir_func_ty)),
            );
        }

        fn_value
    }

    pub(crate) fn emit_func_body(&mut self, func_params: &CIRFuncParams, cir_block: &CIRBlockStmt) {
        self.ensure_entry_block();
        self.emit_func_params(func_params);
        self.emit_body(cir_block);
        self.ensure_void_fn_terminated();
    }

    pub(crate) fn ensure_void_fn_terminated(&self) {
        let cur_fn = self.cur_fn.unwrap();
        if cur_fn.get_type().get_return_type().is_some() {
            return; // works only for void return type
        }

        if let Some(cur_block) = &self.blockreg.cur_block {
            self.llvmbuilder.position_at_end(*cur_block);
            if cur_block.get_terminator().is_some() {
                return;
            }

            self.llvmbuilder.build_return(None).unwrap();
        }
    }
}
