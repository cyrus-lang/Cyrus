// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

/* 
 * Copyright (c) 2026 The Cyrus Programming Language Project
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
use cyrusc_abi::{inline::Inlining, modifiers::FuncModifiers};
use cyrusc_cir::{
    CIRBlockStmt, CIRFuncDeclStmt, CIRFuncParams, CIRLambda, cir_func_decl_as_func_ty,
    monomorph::CIRMonomorphEntry,
    types::{CIRFuncTy, CIRTy},
};
use cyrusc_tast::generics::monomorph::MonomorphKey;
use inkwell::{types::BasicTypeEnum, values::FunctionValue};

impl<'ll> IRBuilderCtx<'ll> {
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

                self.emit_func_body(&monomorph_func_entry.func_params, &monomorph_func_entry.func_body);

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
        self.blockreg = parent_blockreg ;
        if let Some(basic_block) = self.blockreg.cur_block {
            self.emit_block(basic_block);
        }

        let cir_fn_ty = cir_func_decl_as_func_ty(&func_decl);
        InternalValue::new(CIRTy::FuncType(cir_fn_ty), InternalValueKind::FuncValue(fn_value))
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
