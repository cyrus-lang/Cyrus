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
    c,
    llvm::abi::{
        abi_type::abi_type_to_llvm_type,
        modifiers::{apply_func_modifiers, apply_inlining_func},
    },
};
use cyrusc_ast::{abi::Inlining, modifiers::FuncModifiers};
use cyrusc_internal::{
    abi::{
        args::{ABIArgInfo, ABIArgKind, ABIFunctionInfo, ExpandKind},
        types::ABIType,
    },
    cir::{
        cir::{CIRBlockStmt, CIRExpr, CIRFuncDeclStmt, CIRFuncParams, CIRLambda, cir_func_decl_as_func_ty},
        monomorph::CIRMonomorphEntry,
        types::{CIRFuncTy, CIRTy},
    },
};
use cyrusc_tast::generics::monomorph::MonomorphKey;
use inkwell::{
    context::AsContextRef,
    llvm_sys::core::{
        LLVMAddAttributeAtIndex, LLVMAddCallSiteAttribute, LLVMCreateTypeAttribute, LLVMGetEnumAttributeKindForName,
    },
    types::{AsTypeRef, BasicType, BasicTypeEnum},
    values::{AsValueRef, BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, FunctionValue},
};

pub(crate) enum FuncCallKind<'ll> {
    Direct(FunctionValue<'ll>),
    Indirect(CallSiteValue<'ll>),
}

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_func_args(
        &mut self,
        args: &Vec<CIRExpr>,
        fn_ty: &CIRFuncTy,
    ) -> Vec<BasicMetadataValueEnum<'ll>> {
        let abi_func_info = fn_ty.abi_func_info.as_ref().unwrap();

        let mut args_values = Vec::with_capacity(args.len());

        for (idx, expr) in args.iter().enumerate() {
            let lvalue = self.emit_expr(expr);
            let mut rvalue = self.load_rvalue(lvalue);

            if idx < abi_func_info.params_infos.len() {
                let abi_arg_info = &abi_func_info.params_infos[idx];

                match abi_arg_info.kind.clone() {
                    ABIArgKind::Direct { coerce_to } => {
                        self.emit_direct_arg(&mut args_values, &rvalue, coerce_to);
                    }
                    ABIArgKind::DirectPair { lo, hi } => {
                        self.emit_direct_pair_arg(&mut args_values, &rvalue, lo, hi);
                    }
                    ABIArgKind::DirectCoerce { ty } => {
                        self.emit_direct_coerce_arg(&mut args_values, &rvalue, ty);
                    }
                    ABIArgKind::Expand { kind } => {
                        self.emit_expand_arg(&mut args_values, &rvalue, kind, &abi_func_info.params_types);
                    }
                    ABIArgKind::Extend { signed } => {
                        self.emit_extend_arg(&mut args_values, rvalue, signed);
                    }
                    ABIArgKind::Indirect { align, ref ty } => {
                        self.emit_indirect_arg(&mut args_values, &rvalue, align, ty.clone(), &abi_arg_info);
                    }
                    ABIArgKind::Ignore => {
                        // skip argument (zero-sized types)
                    }
                }
            } else {
                // classify variadic argument value
                let promoted_rvalue_ty = self.target.target_abi.apply_variadic_argument_promote(&rvalue.ty);
                let llvm_promoted_type = self.emit_ty(promoted_rvalue_ty.clone());
                let promoted_value: BasicValueEnum<'ll> =
                    self.emit_cast(llvm_promoted_type, rvalue).try_into().unwrap();

                rvalue = InternalValue::new(promoted_rvalue_ty, InternalValueKind::RValue(promoted_value));

                let (abi_arg_info, _) = self.target.target_abi.classify_argument(&rvalue.ty, 0, false);

                match abi_arg_info.kind.clone() {
                    ABIArgKind::Direct { coerce_to } => {
                        self.emit_direct_arg(&mut args_values, &rvalue, coerce_to);
                    }
                    ABIArgKind::DirectPair { lo, hi } => {
                        self.emit_direct_pair_arg(&mut args_values, &rvalue, lo, hi);
                    }
                    ABIArgKind::DirectCoerce { ty } => {
                        self.emit_direct_coerce_arg(&mut args_values, &rvalue, ty);
                    }
                    _ => args_values.push(rvalue.as_basic_value().into()),
                }
            }
        }

        args_values
    }

    fn emit_direct_arg(
        &mut self,
        args_values: &mut Vec<BasicMetadataValueEnum<'ll>>,
        rvalue: &InternalValue<'ll>,
        coerce_to: Option<ABIType>,
    ) {
        if let Some(target_ty) = coerce_to {
            let coerced: BasicValueEnum<'ll> = self
                .emit_cast_func_arg(rvalue.as_basic_value(), &rvalue.ty, target_ty.clone())
                .try_into()
                .unwrap();

            args_values.push(coerced.into());
        } else {
            // pass as-is
            args_values.push(rvalue.as_basic_value().into());
        }
    }

    fn emit_direct_pair_arg(
        &mut self,
        args_values: &mut Vec<BasicMetadataValueEnum<'ll>>,
        rvalue: &InternalValue<'ll>,
        lo: ABIType,
        hi: ABIType,
    ) {
        if !rvalue.as_basic_value().is_struct_value() {
            // coerced to different type before, so push directly to args and we're done!
            args_values.push(rvalue.as_basic_value().into());
            return;
        }

        // value is split across two registers
        // need to extract lo and hi parts from the struct
        let struct_value = rvalue.as_basic_value().into_struct_value();

        // extract low part (first 8 bytes)
        let lo_value = self
            .llvmbuilder
            .build_extract_value(struct_value, 0, "lo.part")
            .unwrap();

        // extract high part (next 8 bytes)
        let hi_value = self
            .llvmbuilder
            .build_extract_value(struct_value, 1, "hi.part")
            .unwrap();

        let lo_llvm: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvmctx, &self.target.info, &lo)
            .try_into()
            .unwrap();

        let hi_llvm: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvmctx, &self.target.info, &hi)
            .try_into()
            .unwrap();

        let lo_coerced = self.intrinsic_coerce_through_alloca(lo_value, lo_llvm, "lo");
        let hi_coerced = self.intrinsic_coerce_through_alloca(hi_value, hi_llvm, "hi");

        // let lo_coerced = if lo_llvm == lo_val.get_type() {
        //     lo_val
        // } else {
        //     self.llvmbuilder.build_bit_cast(lo_val, lo_llvm, "coerce.lo").unwrap()
        // };

        // let hi_coerced = if hi_llvm == hi_val.get_type() {
        //     hi_val
        // } else {
        //     self.llvmbuilder.build_bit_cast(hi_val, hi_llvm, "coerce.hi").unwrap()
        // };

        args_values.push(lo_coerced.into());
        args_values.push(hi_coerced.into());
    }

    fn emit_direct_coerce_arg(
        &mut self,
        args_values: &mut Vec<BasicMetadataValueEnum<'ll>>,
        rvalue: &InternalValue<'ll>,
        coerce_to: ABIType,
    ) {
        let coerced: BasicMetadataValueEnum<'ll> = self
            .emit_cast_func_arg(rvalue.as_basic_value(), &rvalue.ty, coerce_to)
            .try_into()
            .unwrap();

        args_values.push(coerced.into());
    }

    fn emit_expand_arg(
        &mut self,
        args_values: &mut Vec<BasicMetadataValueEnum<'ll>>,
        rvalue: &InternalValue<'ll>,
        kind: ExpandKind,
        param_types: &Vec<ABIType>,
    ) {
        let struct_value = rvalue.as_basic_value().into_struct_value();
        let fields_cir_types = rvalue.ty.struct_or_union_fields().unwrap();

        match kind {
            ExpandKind::Simple => {
                // simple expansion, extract all fields
                let num_fields = struct_value.count_fields();

                for i in 0..num_fields {
                    let field_value = self
                        .llvmbuilder
                        .build_extract_value(struct_value, i, "expand.field")
                        .unwrap();

                    let field_cir_type = fields_cir_types.get(i as usize).unwrap();
                    let param_type = &param_types[i as usize];

                    let casted: BasicMetadataValueEnum<'ll> = self
                        .emit_cast_func_arg(field_value, field_cir_type, param_type.clone())
                        .try_into()
                        .unwrap();

                    args_values.push(casted.into());
                }
            }
            ExpandKind::Coerced { offset_hi, lo, hi, .. } => {
                let struct_value = rvalue.as_basic_value().into_struct_value();

                // extract and coerce low part
                let lo_value = self
                    .llvmbuilder
                    .build_extract_value(struct_value, 0, "expand.lo")
                    .unwrap();

                let lo_llvm: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvmctx, &self.target.info, &lo)
                    .try_into()
                    .unwrap();

                let lo_coerced = if lo_llvm == lo_value.get_type() {
                    lo_value
                } else {
                    let field_cir_type = fields_cir_types.get(0).unwrap();
                    let param_type = &param_types[0 as usize];

                    let casted: BasicValueEnum<'ll> = self
                        .emit_cast_func_arg(lo_value, field_cir_type, param_type.clone())
                        .try_into()
                        .unwrap();

                    casted
                };

                args_values.push(lo_coerced.into());

                // extract and coerce high part (may be at different index)
                let hi_value = self
                    .llvmbuilder
                    .build_extract_value(struct_value, offset_hi as u32, "expand.hi")
                    .unwrap();

                let hi_llvm: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvmctx, &self.target.info, &hi)
                    .try_into()
                    .unwrap();

                let hi_coerced = if hi_llvm == hi_value.get_type() {
                    hi_value
                } else {
                    let field_cir_type = fields_cir_types.get(offset_hi as usize).unwrap();
                    let param_type = &param_types[offset_hi as usize];

                    let casted: BasicValueEnum<'ll> = self
                        .emit_cast_func_arg(lo_value, field_cir_type, param_type.clone())
                        .try_into()
                        .unwrap();

                    casted
                };

                args_values.push(hi_coerced.into());
            }
            ExpandKind::Struct { field_count } => {
                let struct_value = rvalue.as_basic_value().into_struct_value();

                for i in 0..field_count as u32 {
                    let field_value = self
                        .llvmbuilder
                        .build_extract_value(struct_value, i, "expand.struct")
                        .unwrap();

                    let field_cir_type = fields_cir_types.get(i as usize).unwrap();
                    let param_type = &param_types[i as usize];

                    let casted: BasicValueEnum<'ll> = self
                        .emit_cast_func_arg(field_value, field_cir_type, param_type.clone())
                        .try_into()
                        .unwrap();

                    args_values.push(casted.into());
                }
            }
        }
    }

    fn emit_extend_arg(
        &mut self,
        args_values: &mut Vec<BasicMetadataValueEnum<'ll>>,
        rvalue: InternalValue<'ll>,
        signed: bool,
    ) {
        // integer extension
        if rvalue.as_basic_value().is_int_value() {
            let extended_value = self.widen_int_arg(rvalue, signed).as_basic_value();
            args_values.push(BasicMetadataValueEnum::from(extended_value));
        } else {
            args_values.push(rvalue.as_basic_value().into());
        }
    }

    fn emit_indirect_arg(
        &mut self,
        args_values: &mut Vec<BasicMetadataValueEnum<'ll>>,
        rvalue: &InternalValue<'ll>,
        align: u32,
        ty: ABIType,
        abi_arg_info: &ABIArgInfo,
    ) {
        // pass indirectly via pointer
        let llvm_ty: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvmctx, &self.target.info, &ty)
            .try_into()
            .unwrap();

        if rvalue.as_basic_value().is_pointer_value() && !abi_arg_info.attrs.by_val {
            // already a pointer, use directly
            args_values.push(rvalue.as_basic_value().into());
        } else {
            // create a copy
            let alloca = self.llvmbuilder.build_alloca(llvm_ty, "indirect.arg").unwrap();

            if align > 0 {
                self.llvmbuilder
                    .build_store(alloca, rvalue.as_basic_value())
                    .unwrap()
                    .set_alignment(align)
                    .unwrap();
            } else {
                self.llvmbuilder.build_store(alloca, rvalue.as_basic_value()).unwrap();
            }

            args_values.push(alloca.into());
        }
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
                return (fn_value, monomorph_func_entry.func_type.clone());
            }

            drop(monomorph_registry);
            drop(irreg);

            // insert func to current module
            let fn_ty = self.emit_func_ty(monomorph_func_entry.func_type.clone());
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
                        LocalIRValue::Func(fn_value, CIRTy::FuncType(monomorph_func_entry.func_type.clone())),
                    );

                    fn_value
                };

                let parent_cur_func = self.cur_func.clone();
                let parent_cur_abi_func_info = self.cur_abi_func_info.clone();
                let parent_blockreg = self.blockreg.clone();

                self.set_current_func(fn_value, monomorph_func_entry.abi_func_info.clone());

                self.emit_func_body(
                    &monomorph_func_entry.func_params,
                    &monomorph_func_entry.abi_func_info,
                    &monomorph_func_entry.body().unwrap(),
                );

                {
                    // back to parent state because we emitted a new function in the middle of an another function
                    if let Some(cur_func) = parent_cur_func {
                        self.set_current_func(cur_func, parent_cur_abi_func_info.unwrap());
                    }

                    self.blockreg = parent_blockreg;
                    if let Some(cur_block) = self.blockreg.cur_block {
                        self.emit_block(cur_block);
                    }
                }

                return (fn_value, monomorph_func_entry.func_type.clone());
            }
        }
    }

    pub(crate) fn emit_func_params(&self, func_params: CIRFuncParams, abi_func_info: &ABIFunctionInfo) {
        let mut llvm_param_index = 0;

        // handle hidden sret pointer
        if abi_func_info.ret_info.kind.is_indirect_sret() {
            llvm_param_index += 1;
        }

        for (idx, param) in func_params.list.iter().enumerate() {
            let abi_arg_info = &abi_func_info.params_infos[idx];

            match &abi_arg_info.kind {
                ABIArgKind::Direct { .. } | ABIArgKind::DirectCoerce { .. } | ABIArgKind::Extend { .. } => {
                    let llvm_param = self.cur_func.unwrap().get_nth_param(llvm_param_index as u32).unwrap();

                    llvm_param_index += 1;

                    let ty: BasicTypeEnum<'ll> = self.emit_ty(param.ty.clone()).try_into().unwrap();

                    let ptr = self.llvmbuilder.build_alloca(ty, "param").unwrap();
                    self.llvmbuilder.build_store(ptr, llvm_param).unwrap();

                    self.irreg
                        .borrow_mut()
                        .insert(param.irv_id, LocalIRValue::LValue(ptr, param.ty.clone()));
                }
                ABIArgKind::DirectPair { lo: _, hi: _ } => {
                    let ptr_type = self.llvmctx.ptr_type(inkwell::AddressSpace::default());

                    let lo = self.cur_func.unwrap().get_nth_param(llvm_param_index as u32).unwrap();

                    let hi = self
                        .cur_func
                        .unwrap()
                        .get_nth_param((llvm_param_index + 1) as u32)
                        .unwrap();

                    llvm_param_index += 2;

                    let param_ty: BasicTypeEnum<'ll> = self.emit_ty(param.ty.clone()).try_into().unwrap();

                    let param_alloca = self.llvmbuilder.build_alloca(param_ty, "param").unwrap();

                    let lo_ptr = self
                        .llvmbuilder
                        .build_struct_gep(param_ty, param_alloca, 0, "lo.ptr")
                        .unwrap();

                    self.llvmbuilder.build_store(lo_ptr, lo).unwrap();

                    let hi_ptr = self
                        .llvmbuilder
                        .build_struct_gep(param_ty, param_alloca, 1, "hi.ptr")
                        .unwrap();

                    // spill hi register
                    let tmp = self.llvmbuilder.build_alloca(hi.get_type(), "hi.temp").unwrap();

                    self.llvmbuilder.build_store(tmp, hi).unwrap();

                    let dst = self
                        .llvmbuilder
                        .build_bit_cast(hi_ptr, ptr_type, "")
                        .unwrap()
                        .into_pointer_value();

                    let src = self
                        .llvmbuilder
                        .build_bit_cast(tmp, ptr_type, "")
                        .unwrap()
                        .into_pointer_value();

                    let size = hi.get_type().size_of().unwrap();

                    self.llvmbuilder.build_memcpy(dst, 1, src, 1, size).unwrap();

                    self.irreg
                        .borrow_mut()
                        .insert(param.irv_id, LocalIRValue::LValue(param_alloca, param.ty.clone()));
                }
                ABIArgKind::Indirect { .. } => {
                    let ptr = self
                        .cur_func
                        .unwrap()
                        .get_nth_param(llvm_param_index as u32)
                        .unwrap()
                        .into_pointer_value();

                    llvm_param_index += 1;

                    self.irreg
                        .borrow_mut()
                        .insert(param.irv_id, LocalIRValue::LValue(ptr, param.ty.clone()));
                }
                ABIArgKind::Expand { kind } => {
                    let struct_ty: BasicTypeEnum<'ll> = self.emit_ty(param.ty.clone()).try_into().unwrap();

                    let param_alloca = self.llvmbuilder.build_alloca(struct_ty, "param").unwrap();

                    let fields_cir_types = param.ty.struct_or_union_fields().unwrap();

                    match kind {
                        ExpandKind::Simple => {
                            let field_count = fields_cir_types.len();

                            for i in 0..field_count {
                                let llvm_param = self.cur_func.unwrap().get_nth_param(llvm_param_index as u32).unwrap();

                                llvm_param_index += 1;

                                let field_ptr = self
                                    .llvmbuilder
                                    .build_struct_gep(struct_ty, param_alloca, i as u32, "expand.field.ptr")
                                    .unwrap();

                                self.llvmbuilder.build_store(field_ptr, llvm_param).unwrap();
                            }
                        }
                        ExpandKind::Struct { field_count } => {
                            for i in 0..*field_count {
                                let llvm_param = self.cur_func.unwrap().get_nth_param(llvm_param_index as u32).unwrap();

                                llvm_param_index += 1;

                                let field_ptr = self
                                    .llvmbuilder
                                    .build_struct_gep(struct_ty, param_alloca, i as u32, "expand.struct.ptr")
                                    .unwrap();

                                self.llvmbuilder.build_store(field_ptr, llvm_param).unwrap();
                            }
                        }
                        ExpandKind::Coerced { offset_hi, .. } => {
                            // first expanded param
                            let lo_param = self.cur_func.unwrap().get_nth_param(llvm_param_index as u32).unwrap();

                            llvm_param_index += 1;

                            let lo_ptr = self
                                .llvmbuilder
                                .build_struct_gep(struct_ty, param_alloca, 0, "expand.lo.ptr")
                                .unwrap();

                            self.llvmbuilder.build_store(lo_ptr, lo_param).unwrap();

                            // second expanded param
                            let hi_param = self.cur_func.unwrap().get_nth_param(llvm_param_index as u32).unwrap();

                            llvm_param_index += 1;

                            let hi_ptr = self
                                .llvmbuilder
                                .build_struct_gep(struct_ty, param_alloca, *offset_hi as u32, "expand.hi.ptr")
                                .unwrap();

                            self.llvmbuilder.build_store(hi_ptr, hi_param).unwrap();
                        }
                    }

                    self.irreg
                        .borrow_mut()
                        .insert(param.irv_id, LocalIRValue::LValue(param_alloca, param.ty.clone()));
                }
                ABIArgKind::Ignore => {
                    // zero sized type
                }
            }
        }
    }

    pub(crate) fn emit_lambda(&mut self, lambda: &CIRLambda) -> InternalValue<'ll> {
        let parent_func = self.cur_func.clone();
        let parent_cur_abi_func_info = self.cur_abi_func_info.clone();
        let parent_blockreg = self.blockreg.clone();

        let lambda_name = self.increment_lambda_name();
        let mut cir_func_decl = CIRFuncDeclStmt {
            irv_id: lambda.irv_id,
            name: lambda_name,
            params: lambda.params.clone(),
            ret: lambda.ret.clone(),
            modifiers: FuncModifiers::default(),
            abi_func_info: None,
        };

        let cir_func_type = cir_func_decl_as_func_ty(&cir_func_decl);
        cir_func_decl.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());

        let fn_value = self.emit_func_decl(&cir_func_decl);
        fn_value.set_linkage(inkwell::module::Linkage::Private);
        if lambda.inline {
            apply_inlining_func(self.llvmctx, &fn_value, Inlining::Inline);
        }

        self.set_current_func(fn_value, lambda.abi_func_info.clone());
        self.emit_func_body(&lambda.params, &lambda.abi_func_info, &lambda.body);

        if let Some(cur_func) = parent_func {
            self.set_current_func(cur_func, parent_cur_abi_func_info.unwrap());
        }

        self.blockreg = parent_blockreg;
        if let Some(basic_block) = self.blockreg.cur_block {
            self.emit_block(basic_block);
        }

        let cir_fn_ty = cir_func_decl_as_func_ty(&cir_func_decl);
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

    pub(crate) fn emit_func_body(
        &mut self,
        func_params: &CIRFuncParams,
        abi_func_info: &ABIFunctionInfo,
        cir_block: &CIRBlockStmt,
    ) {
        debug_assert!(self.cur_func.is_some());

        self.ensure_entry_block();
        self.emit_func_params(func_params.clone(), abi_func_info);
        self.emit_body(cir_block);
        self.ensure_void_fn_terminated();
    }

    pub(crate) fn ensure_void_fn_terminated(&self) {
        let cur_fn = self.cur_func.unwrap();
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

    pub(crate) fn emit_func_call_attributes(
        &mut self,
        abi_func_info: &ABIFunctionInfo,
        func_call_kind: FuncCallKind<'ll>,
    ) {
        match func_call_kind {
            FuncCallKind::Direct(fn_value) => self.emit_direct_func_call_args_attributes(&fn_value, abi_func_info),
            FuncCallKind::Indirect(call_site) => {
                self.emit_indirect_func_call_args_attributes(&call_site, abi_func_info)
            }
        }
    }

    fn emit_direct_func_call_args_attributes(&mut self, fn_value: &FunctionValue, abi_func_info: &ABIFunctionInfo) {
        let byval_attr_kind = unsafe { LLVMGetEnumAttributeKindForName(c!("byval").as_ptr(), 5) };
        let sret_attr_kind = unsafe { LLVMGetEnumAttributeKindForName(c!("sret").as_ptr(), 4) };

        let mut llvm_param_index_offset = 0;

        if abi_func_info.ret_info.kind.is_indirect_sret() {
            let ret_ty = &abi_func_info.ret_info.abi_type;
            let llvm_ret_ty = abi_type_to_llvm_type(self.llvmctx, &self.target.info, ret_ty);

            let attr = unsafe {
                LLVMCreateTypeAttribute(self.llvmctx.as_ctx_ref(), sret_attr_kind, llvm_ret_ty.as_type_ref())
            };

            unsafe {
                LLVMAddAttributeAtIndex(fn_value.as_value_ref(), 1, attr);
            }

            llvm_param_index_offset = 1;
        }

        for (idx, param_info) in abi_func_info.params_infos.iter().enumerate() {
            if param_info.is_indirect_by_val() {
                let struct_type = &abi_func_info.params_types[idx];
                let llvm_struct_type = abi_type_to_llvm_type(self.llvmctx, &self.target.info, struct_type);

                let attr = unsafe {
                    LLVMCreateTypeAttribute(
                        self.llvmctx.as_ctx_ref(),
                        byval_attr_kind,
                        llvm_struct_type.as_type_ref(),
                    )
                };

                // index is 1-based, 0 is return
                unsafe {
                    LLVMAddAttributeAtIndex(fn_value.as_value_ref(), idx as u32 + 1 + llvm_param_index_offset, attr);
                }
            }
        }
    }

    fn emit_indirect_func_call_args_attributes(
        &mut self,
        call_site: &CallSiteValue<'ll>,
        abi_func_info: &ABIFunctionInfo,
    ) {
        let byval_attr_kind = unsafe { LLVMGetEnumAttributeKindForName(c!("byval").as_ptr(), 5) };
        let sret_attr_kind = unsafe { LLVMGetEnumAttributeKindForName(c!("sret").as_ptr(), 4) };

        let mut llvm_param_index_offset = 0;

        if abi_func_info.ret_info.kind.is_indirect_sret() {
            let ret_ty = &abi_func_info.ret_info.abi_type;
            let llvm_ret_ty = abi_type_to_llvm_type(self.llvmctx, &self.target.info, ret_ty);

            let attr = unsafe {
                LLVMCreateTypeAttribute(self.llvmctx.as_ctx_ref(), sret_attr_kind, llvm_ret_ty.as_type_ref())
            };

            unsafe {
                LLVMAddCallSiteAttribute(call_site.as_value_ref(), 1, attr);
            }

            llvm_param_index_offset = 1;
        }

        for (idx, param_info) in abi_func_info.params_infos.iter().enumerate() {
            if !param_info.is_indirect_by_val() {
                continue;
            }

            let param_type = abi_func_info.params_types.get(idx).unwrap().clone();
            let pointee_type = abi_type_to_llvm_type(self.llvmctx, &self.target.info, &param_type);

            let attr = unsafe {
                LLVMCreateTypeAttribute(self.llvmctx.as_ctx_ref(), byval_attr_kind, pointee_type.as_type_ref())
            };

            // index is 1-based, 0 is return
            unsafe {
                LLVMAddCallSiteAttribute(call_site.as_value_ref(), idx as u32 + 1 + llvm_param_index_offset, attr);
            }
        }
    }
}

fn monomorph_func_name(irv_id: u32) -> String {
    format!("monomorph.instance@{}", irv_id)
}
