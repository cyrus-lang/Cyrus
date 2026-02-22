use std::ffi::CString;

/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::{
    builder::{
        builder::IRBuilderCtx,
        irreg::LocalIRValue,
        values::{InternalValue, InternalValueKind},
    },
    llvm::abi::modifiers::{apply_func_modifiers, apply_inlining_func},
};
use cyrusc_abi::{abi_ast_defs::Inlining, modifiers::FuncModifiers};
use cyrusc_abi_targets::{ABIArgInfo, ABIFunctionInfo, layout::type_layout};
use cyrusc_cir::{
    CIRBlockStmt, CIRExpr, CIRFuncDeclStmt, CIRFuncParams, CIRLambda, cir_func_decl_as_func_ty,
    monomorph::CIRMonomorphEntry,
    types::{CIRFuncTy, CIRTy},
};
use cyrusc_tast::generics::monomorph::MonomorphKey;
use inkwell::{
    context::AsContextRef,
    llvm_sys::core::{
        LLVMAddAttributeAtIndex, LLVMAddCallSiteAttribute, LLVMCreateTypeAttribute, LLVMGetEnumAttributeKindForName,
    },
    types::{AsTypeRef, BasicTypeEnum},
    values::{AsValueRef, BasicMetadataValueEnum, CallSiteValue, FunctionValue},
};

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_direct_func_call_args_attributes(
        &mut self,
        fn_value: &FunctionValue,
        abi_func_info: &ABIFunctionInfo,
    ) {
        let attr_name = CString::new("byval").unwrap();
        let attr_kind = unsafe { LLVMGetEnumAttributeKindForName(attr_name.as_ptr(), 5) };

        for (idx, param_info) in abi_func_info.params_infos.iter().enumerate() {
            if param_info.is_indirect_by_val() {
                let param = fn_value.get_nth_param(idx as u32).unwrap();
                let param_type = param.get_type();

                let attr =
                    unsafe { LLVMCreateTypeAttribute(self.llvmctx.as_ctx_ref(), attr_kind, param_type.as_type_ref()) };

                // LLVM parameter indices are 1-based; 0 is the return value
                unsafe {
                    LLVMAddAttributeAtIndex(fn_value.as_value_ref(), (idx as u32 + 1) as u32, attr);
                }
            }
        }
    }

    pub(crate) fn emit_indirect_func_call_args_attributes(
        &mut self,
        call_site: &CallSiteValue<'ll>,
        abi_func_info: &ABIFunctionInfo,
    ) {
        let attr_name = CString::new("byval").unwrap();
        let attr_kind = unsafe { LLVMGetEnumAttributeKindForName(attr_name.as_ptr(), 5) };

        for (idx, param_info) in abi_func_info.params_infos.iter().enumerate() {
            if !param_info.is_indirect_by_val() {
                continue;
            }

            let pointee_ty = self.emit_ty(abi_func_info.params_types.get(idx).unwrap().clone());

            let attr =
                unsafe { LLVMCreateTypeAttribute(self.llvmctx.as_ctx_ref(), attr_kind, pointee_ty.as_type_ref()) };

            // call-site attributes: index is 1-based, 0 is return
            unsafe {
                LLVMAddCallSiteAttribute(call_site.as_value_ref(), (idx as u32 + 1) as u32, attr);
            }
        }
    }

    pub(crate) fn emit_func_args(
        &mut self,
        args: &Vec<CIRExpr>,

        fn_ty: &CIRFuncTy,
    ) -> Vec<BasicMetadataValueEnum<'ll>> {
        let abi = &self.target.target_abi;

        let mut args_values: Vec<BasicMetadataValueEnum> = Vec::new();

        for (idx, expr) in args.iter().enumerate() {
            let lvalue = self.emit_expr(expr);
            let rvalue = self.load_rvalue(lvalue);

            let cir_ty = fn_ty.params.get(idx).unwrap_or(&rvalue.ty);
            let layout = type_layout(&self.target.info, cir_ty);

            match abi.classify_argument(&layout) {
                ABIArgInfo::Direct { coerce_to } => {
                    if let Some(str) = coerce_to {
                        // alter to coerced ty
                        let coerce_type =
                            unsafe { BasicTypeEnum::new(llvm_type_from_coerce_str(self.llvmctx.raw(), &str)) };

                        let coerced_value = self
                            .llvmbuilder
                            .build_bit_cast(rvalue.as_basic_value(), coerce_type, "bitcast.coerce")
                            .unwrap();

                        args_values.push(coerced_value.into());
                    } else {
                        if let Some(cir_ty) = fn_ty.params.get(idx) {
                            args_values.push(BasicMetadataValueEnum::from(
                                self.emit_implicit_cast(cir_ty, rvalue).as_basic_value(),
                            ));
                        } else {
                            args_values.push(BasicMetadataValueEnum::from(rvalue.as_basic_value()));
                        }
                    }
                }
                ABIArgInfo::Extend { signed } => {
                    // extend integer width if needed
                    let int_value = cir_ty.as_plain().map(|p| p.is_integer()).unwrap_or(false);

                    if int_value {
                        args_values.push(BasicMetadataValueEnum::from(
                            self.widen_int_arg(rvalue, signed).as_basic_value(),
                        ));
                    } else {
                        args_values.push(BasicMetadataValueEnum::from(rvalue.as_basic_value()));
                    }
                }
                ABIArgInfo::Indirect { by_val } => {
                    if !by_val && rvalue.is_lvalue_address() {
                        args_values.push(BasicMetadataValueEnum::from(rvalue.as_basic_value()));
                        continue;
                    }

                    let ty: BasicTypeEnum<'ll> = self.emit_ty(cir_ty.clone()).try_into().unwrap();
                    let alloca = self.llvmbuilder.build_alloca(ty, "indirect.arg").unwrap();

                    self.llvmbuilder.build_store(alloca, rvalue.as_basic_value()).unwrap();

                    args_values.push(BasicMetadataValueEnum::PointerValue(alloca));
                }
                ABIArgInfo::Expand => {
                    let struct_value = rvalue.as_basic_value().into_struct_value();
                    let mut expanded_values: Vec<BasicMetadataValueEnum<'_>> = Vec::new();
                    let num_fields = struct_value.count_fields();

                    for i in 0..num_fields {
                        let field_value = self
                            .llvmbuilder
                            .build_extract_value(struct_value, i, "extract")
                            .unwrap();

                        expanded_values.push(field_value.into());
                    }

                    args_values.extend(expanded_values);
                }
                ABIArgInfo::Ignore => {
                    // skip argument completely
                    args_values.push(self.llvmctx.i8_type().const_zero().into());
                }
            }
        }

        args_values
    }

    pub(crate) fn emit_monomorph_func_instance(
        &mut self,
        monomorph_key: &MonomorphKey,
    ) -> (FunctionValue<'ll>, CIRFuncTy) {
        {
            let monomorph_registry = self.monomorph_registry.lock().unwrap();
            let monomorph_entry = monomorph_registry.get(monomorph_key).cloned().unwrap();
            let monomorph_func_entry = match monomorph_entry {
                CIRMonomorphEntry::Func(entry) => entry,
            };

            let irreg = self.irreg.borrow();
            if let Some(local_ir_value) = irreg.get(monomorph_func_entry.irv_id) {
                // already exists in current module
                let fn_value = local_ir_value.as_func().cloned().unwrap();
                return (fn_value, monomorph_func_entry.func_ty.clone());
            }

            drop(monomorph_registry);
            drop(irreg);

            // insert func to current module
            let fn_ty = self.emit_func_ty(monomorph_func_entry.func_ty.clone());
            {
                let fn_value = {
                    let llvmmodule = self.llvmmodule.borrow_mut();
                    let mut irreg = self.irreg.borrow_mut();

                    let func_name = monomorph_func_name(monomorph_func_entry.irv_id);

                    let fn_value = match llvmmodule.get_function(&func_name) {
                        Some(f) => f,
                        None => llvmmodule.add_function(&func_name, fn_ty, None),
                    };

                    irreg.insert(
                        monomorph_func_entry.irv_id,
                        LocalIRValue::Func(fn_value, CIRTy::FuncType(monomorph_func_entry.func_ty.clone())),
                    );

                    fn_value
                };

                let parent_cur_fn = self.cur_fn.clone();
                let parent_blockreg = self.blockreg.clone();

                self.cur_fn = Some(fn_value);

                self.emit_func_body(&monomorph_func_entry.func_params, &monomorph_func_entry.body().unwrap());

                {
                    // back to parent state because we emitted a new function in the middle of an another function
                    self.cur_fn = parent_cur_fn;
                    self.blockreg = parent_blockreg;
                    if let Some(cur_block) = self.blockreg.cur_block {
                        self.emit_block(cur_block);
                    }
                }

                return (fn_value, monomorph_func_entry.func_ty.clone());
            }
        }
    }

    pub(crate) fn emit_func_params(&self, func_params: &CIRFuncParams) {
        func_params.list.iter().enumerate().for_each(|(param_idx, param)| {
            let basic_value = self
                .cur_fn
                .unwrap()
                .get_nth_param(param_idx.try_into().unwrap())
                .unwrap();

            let mut irreg = self.irreg.borrow_mut();

            let ty: BasicTypeEnum<'ll> = self.emit_ty(param.ty.clone()).try_into().unwrap();
            let ptr = self.llvmbuilder.build_alloca(ty, "param").unwrap();
            self.llvmbuilder.build_store(ptr, basic_value).unwrap();
            irreg.insert(param.irv_id, LocalIRValue::LValue(ptr, param.ty.clone()));

            drop(irreg);
        });
    }

    pub(crate) fn emit_lambda(&mut self, lambda: &CIRLambda) -> InternalValue<'ll> {
        let parent_fn = self.cur_fn.clone();
        let parent_blockreg = self.blockreg.clone();

        let lambda_name = self.increment_lambda_name();
        let func_decl = CIRFuncDeclStmt {
            irv_id: lambda.irv_id,
            name: lambda_name,
            params: lambda.params.clone(),
            ret: lambda.ret.clone(),
            modifiers: FuncModifiers::default(),
        };

        let fn_value = self.emit_func_decl(&func_decl);
        fn_value.set_linkage(inkwell::module::Linkage::Private);
        if lambda.inline {
            apply_inlining_func(self.llvmctx, &fn_value, Inlining::Inline);
        }

        self.cur_fn = Some(fn_value);
        self.emit_func_body(&lambda.params, &lambda.body);

        self.cur_fn = parent_fn;
        self.blockreg = parent_blockreg;
        if let Some(basic_block) = self.blockreg.cur_block {
            self.emit_block(basic_block);
        }

        let cir_fn_ty = cir_func_decl_as_func_ty(&func_decl);
        InternalValue::new(CIRTy::FuncType(cir_fn_ty), InternalValueKind::FuncValue(fn_value))
    }

    pub(crate) fn emit_func_decl(&mut self, func_decl: &CIRFuncDeclStmt) -> FunctionValue<'ll> {
        let cir_fn_ty = cir_func_decl_as_func_ty(func_decl);

        let fn_type = self.emit_func_ty(cir_fn_ty);

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
        debug_assert!(self.cur_fn.is_some());

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

    fn increment_lambda_name(&mut self) -> String {
        let id = self.lambda_id;
        let name = format!("lambda.{}", id);
        self.lambda_id += 1;
        name
    }
}

fn monomorph_func_name(irv_id: u32) -> String {
    format!("monomorph.instance@{}", irv_id)
}
