// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    builder::{
        builder::CodeGenIRBuilder,
        irreg::LocalIRValue,
        values::{InternalValue, InternalValueKind},
    },
    c,
    llvm::{
        abi::{abi_type::abi_type_to_llvm_type, modifiers::*},
        debug_info::*,
    },
};
use cyrusc_ast::{
    abi::{Inlining, Linkage},
    modifiers::FuncModifiers,
};
use cyrusc_internal::{
    abi::{
        args::{ABIArgInfo, ABIArgKind, ABIFunctionInfo, ExpandKind},
        types::ABIType,
    },
    cir::{cir::*, types::*},
};
use cyrusc_source_loc::Loc;
use inkwell::{
    AddressSpace,
    context::AsContextRef,
    llvm_sys::{
        core::{
            LLVMAddAttributeAtIndex, LLVMAddCallSiteAttribute, LLVMCreateTypeAttribute, LLVMGetEnumAttributeKindForName,
        },
        prelude::LLVMMetadataRef,
    },
    types::{AsTypeRef, BasicTypeEnum, StructType},
    values::{AsValueRef, BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, FunctionValue},
};

pub(crate) enum FuncCallKind<'ll> {
    Direct(FunctionValue<'ll>),
    Indirect(CallSiteValue<'ll>),
}

// Declaration.
impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_func_decl(&mut self, func_decl: &CIRFuncDeclStmt) -> FunctionValue<'ll> {
        let cir_func_type = cir_func_decl_as_func_type(func_decl);

        let llvm_func_type = self.emit_func_type(cir_func_type.clone());

        let func_name = &func_decl.name;

        let llvm_module = self.llvm_module.borrow();

        let llvm_func_value = llvm_module
            .get_function(func_name)
            .unwrap_or_else(|| llvm_module.add_function(func_name, llvm_func_type, None));

        apply_func_modifiers(self.llvm_ctx, &llvm_func_value, &func_decl.modifiers);

        self.insert_local_ir_value(
            func_decl.irv_id,
            LocalIRValue::Func(llvm_func_value, CIRType::FuncType(cir_func_type)),
        );

        llvm_func_value
    }

    pub(crate) fn get_or_declare_function(&mut self, irv_id: IRValueID) -> InternalValue<'ll> {
        if let Some(ir_value) = self.lookup_local_ir_value(irv_id) {
            if let LocalIRValue::Func(llvm_func_value, ty) = ir_value {
                return InternalValue::new(ty, InternalValueKind::FuncValue(llvm_func_value));
            }
        }

        let mut func_decl = self
            .cir_module
            .func_decls
            .get(&irv_id)
            .cloned()
            .expect("missing cir function declaration");

        func_decl.modifiers = FuncModifiers {
            linkage: Some(Linkage::Extern(None)),
            ..func_decl.modifiers
        };

        let llvm_func_value = self.emit_func_decl(&func_decl);

        let cir_func_type = cir_func_decl_as_func_type(&func_decl);

        InternalValue::new(
            CIRType::FuncType(cir_func_type),
            InternalValueKind::FuncValue(llvm_func_value),
        )
    }
}

// Body.
impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_func_params(&self, func_params: CIRFuncParams, abi_func_info: &ABIFunctionInfo) {
        let mut llvm_param_index = 0;

        // handle hidden sret pointer
        if abi_func_info.ret_info.kind.is_indirect_sret() {
            llvm_param_index += 1;
        }

        for (i, param) in func_params.list.iter().enumerate() {
            let abi_arg_info = &abi_func_info.params_infos[i];

            match &abi_arg_info.kind {
                ABIArgKind::Direct { .. } | ABIArgKind::DirectCoerce { .. } | ABIArgKind::Extend { .. } => {
                    let llvm_param = self.cur_func.unwrap().get_nth_param(llvm_param_index as u32).unwrap();

                    llvm_param_index += 1;

                    let ty: BasicTypeEnum<'ll> = self.emit_type(param.ty.clone()).try_into().unwrap();

                    let param_alloca = self.llvmbuilder.build_alloca(ty, "param").unwrap();

                    self.llvmbuilder.build_store(param_alloca, llvm_param).unwrap();

                    self.insert_local_ir_value(
                        param.irv_id.unwrap(),
                        LocalIRValue::LValue(param_alloca, param.ty.clone()),
                    );
                }
                ABIArgKind::DirectPair { lo: lo_ty, hi: hi_ty } => {
                    let ptr_type = self.llvm_ctx.ptr_type(AddressSpace::default());

                    let coerced_type = self.llvm_ctx.struct_type(
                        &[
                            abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, lo_ty)
                                .try_into()
                                .unwrap(),
                            abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, hi_ty)
                                .try_into()
                                .unwrap(),
                        ],
                        false,
                    );

                    let param_type: BasicTypeEnum<'ll> = self.emit_type(param.ty.clone()).try_into().unwrap();

                    let lo = self.cur_func.unwrap().get_nth_param(llvm_param_index as u32).unwrap();

                    let hi = self
                        .cur_func
                        .unwrap()
                        .get_nth_param((llvm_param_index + 1) as u32)
                        .unwrap();

                    llvm_param_index += 2;

                    let param_alloca = self.llvmbuilder.build_alloca(param_type, "param").unwrap();

                    // reinterpret memory as coercion struct
                    let coerced_ptr = self
                        .llvmbuilder
                        .build_pointer_cast(param_alloca, ptr_type, "coerce.ptr")
                        .unwrap();

                    let lo_ptr = self
                        .llvmbuilder
                        .build_struct_gep(coerced_type, coerced_ptr, 0, "lo.ptr")
                        .unwrap();

                    self.llvmbuilder.build_store(lo_ptr, lo).unwrap();

                    let hi_ptr = self
                        .llvmbuilder
                        .build_struct_gep(coerced_type, coerced_ptr, 1, "hi.ptr")
                        .unwrap();

                    self.llvmbuilder.build_store(hi_ptr, hi).unwrap();

                    self.insert_local_ir_value(
                        param.irv_id.unwrap(),
                        LocalIRValue::LValue(param_alloca, param.ty.clone()),
                    );
                }
                ABIArgKind::Indirect { .. } => {
                    let param_alloca = self
                        .cur_func
                        .unwrap()
                        .get_nth_param(llvm_param_index as u32)
                        .unwrap()
                        .into_pointer_value();

                    llvm_param_index += 1;

                    self.insert_local_ir_value(
                        param.irv_id.unwrap(),
                        LocalIRValue::LValue(param_alloca, param.ty.clone()),
                    );
                }
                ABIArgKind::Expand { kind } => {
                    let struct_type: BasicTypeEnum<'ll> = self.emit_type(param.ty.clone()).try_into().unwrap();

                    let param_alloca = self.llvmbuilder.build_alloca(struct_type, "param").unwrap();

                    let fields_cir_types = param.ty.struct_or_union_fields().unwrap();

                    match kind {
                        ExpandKind::Simple => {
                            let field_count = fields_cir_types.len();

                            for i in 0..field_count {
                                let llvm_param = self.cur_func.unwrap().get_nth_param(llvm_param_index as u32).unwrap();

                                llvm_param_index += 1;

                                let field_ptr = self
                                    .llvmbuilder
                                    .build_struct_gep(struct_type, param_alloca, i as u32, "expand.field.ptr")
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
                                    .build_struct_gep(struct_type, param_alloca, i as u32, "expand.struct.ptr")
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
                                .build_struct_gep(struct_type, param_alloca, 0, "expand.lo.ptr")
                                .unwrap();

                            self.llvmbuilder.build_store(lo_ptr, lo_param).unwrap();

                            // second expanded param
                            let hi_param = self.cur_func.unwrap().get_nth_param(llvm_param_index as u32).unwrap();

                            llvm_param_index += 1;

                            let hi_ptr = self
                                .llvmbuilder
                                .build_struct_gep(struct_type, param_alloca, *offset_hi as u32, "expand.hi.ptr")
                                .unwrap();

                            self.llvmbuilder.build_store(hi_ptr, hi_param).unwrap();
                        }
                    }

                    self.insert_local_ir_value(
                        param.irv_id.unwrap(),
                        LocalIRValue::LValue(param_alloca, param.ty.clone()),
                    );
                }
                ABIArgKind::Ignore => {
                    // zero sized type
                    continue;
                }
            }

            let param_name = param.name.clone().unwrap_or("<unnamed_param>".to_string());
            let param_ty_metadata = self.emit_debug_type_metadata(&param.ty);

            unsafe {
                create_debug_parameter(
                    &self.dctx,
                    &param_name,
                    param.loc.line as u32,
                    param_ty_metadata,
                    (i + 1) as u32,
                )
            };
        }
    }

    pub(crate) fn emit_func_body(
        &mut self,
        func_params: &CIRFuncParams,
        abi_func_info: &ABIFunctionInfo,
        cir_block: &CIRBlockStmt,
        func_metadata: LLVMMetadataRef,
        loc: Loc,
    ) {
        debug_assert!(self.cur_func.is_some());
        debug_assert!(self.dctx.compile_unit != std::ptr::null_mut());

        let cur_func = self.cur_func.unwrap();
        let cur_func_name = cur_func.get_name().to_str().unwrap();
        let parent_dctx_func = self.dctx.func;

        unsafe {
            emit_debug_function(
                &mut self.dctx,
                cur_func.as_value_ref(),
                cur_func_name,
                loc.line.try_into().unwrap(),
                func_metadata,
            )
        };

        unsafe {
            set_debug_location(
                &self.dctx,
                self.llvm_ctx,
                self.llvmbuilder,
                loc.line.try_into().unwrap(),
                loc.column.try_into().unwrap(),
            )
        };

        self.ensure_entry_block();
        self.blockreg.labels.clear();
        self.emit_func_params(func_params.clone(), abi_func_info);
        self.emit_body(cir_block);
        self.ensure_void_fn_terminated();

        self.dctx.func = parent_dctx_func;
    }

    pub(crate) fn ensure_void_fn_terminated(&mut self) {
        let cur_fn = self.cur_func.unwrap();

        if cur_fn.get_type().get_return_type().is_some() {
            return; // works only for void return type
        }

        if let Some(cur_block) = &self.blockreg.cur_block {
            self.llvmbuilder.position_at_end(*cur_block);
            if cur_block.get_terminator().is_some() {
                return;
            }

            self.emit_all_defers();
            self.llvmbuilder.build_return(None).unwrap();
        }
    }
}

// Lambda.
impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_lambda(&mut self, lambda: &CIRLambda) -> InternalValue<'ll> {
        let parent_func = self.cur_func.clone();
        let parent_cur_abi_func_info = self.cur_abi_func_info.clone();
        let parent_blockreg = self.blockreg.clone();

        let lambda_name = self.next_lambda_id();
        let mut cir_func_decl = CIRFuncDeclStmt {
            irv_id: lambda.irv_id,
            name: lambda_name,
            params: lambda.params.clone(),
            ret_type: lambda.ret.clone(),
            modifiers: FuncModifiers::default(),
            abi_func_info: None,
            loc: lambda.loc,
        };

        let cir_func_type = cir_func_decl_as_func_type(&cir_func_decl);
        cir_func_decl.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());

        let llvm_func_value = self.emit_func_decl(&cir_func_decl);
        llvm_func_value.set_linkage(inkwell::module::Linkage::Private);
        if lambda.inline {
            apply_inlining_func(self.llvm_ctx, &llvm_func_value, Inlining::Inline);
        }

        let func_metadata = self.emit_func_metadata(&cir_func_type);

        self.set_current_func(llvm_func_value, lambda.abi_func_info.clone());
        self.emit_func_body(
            &lambda.params,
            &lambda.abi_func_info,
            &lambda.body,
            func_metadata,
            lambda.loc,
        );

        if let Some(cur_func) = parent_func {
            self.set_current_func(cur_func, parent_cur_abi_func_info.unwrap());
        }

        self.blockreg = parent_blockreg;
        if let Some(basic_block) = self.blockreg.cur_block {
            self.emit_block(basic_block);
        }

        let cir_fn_ty = cir_func_decl_as_func_type(&cir_func_decl);
        InternalValue::new(
            CIRType::FuncType(cir_fn_ty),
            InternalValueKind::FuncValue(llvm_func_value),
        )
    }

    fn next_lambda_id(&mut self) -> String {
        let id = self.lambda_id;
        let name = format!("lambda.{}", id);
        self.lambda_id += 1;
        name
    }
}

// ABI helpers.
impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_func_metadata(&self, func_type: &CIRFuncType) -> LLVMMetadataRef {
        let ret_ty_meta = if !func_type.ret_type.is_void() {
            Some(self.emit_debug_type_metadata(&func_type.ret_type))
        } else {
            None
        };

        let params_metadata: Vec<LLVMMetadataRef> = func_type
            .params
            .iter()
            .map(|ty| self.emit_debug_type_metadata(&ty))
            .collect();

        unsafe { debug_func_type(&self.dctx, ret_ty_meta, &params_metadata) }
    }

    pub(crate) fn emit_abi_arg(
        &mut self,
        params_types: &[ABIType],
        abi_arg_info: &ABIArgInfo,
        lvalue: &InternalValue<'ll>,
        rvalue: &InternalValue<'ll>,
        out: &mut Vec<BasicMetadataValueEnum<'ll>>,
    ) {
        match abi_arg_info.kind.clone() {
            ABIArgKind::Direct { coerce_to } => {
                self.emit_direct_arg(out, rvalue, coerce_to);
            }
            ABIArgKind::DirectPair { lo, hi } => {
                self.emit_direct_pair_arg(out, lvalue, lo, hi);
            }
            ABIArgKind::DirectCoerce { ty } => {
                self.emit_direct_coerce_arg(out, rvalue, ty);
            }
            ABIArgKind::Expand { kind } => {
                self.emit_expand_arg(out, rvalue, kind, &params_types);
            }
            ABIArgKind::Extend { signed } => {
                self.emit_extend_arg(out, rvalue.clone(), signed);
            }
            ABIArgKind::Indirect { align, ty } => {
                self.emit_indirect_arg(out, rvalue, align, ty.clone(), abi_arg_info);
            }
            ABIArgKind::Ignore => {
                // skip zero-sized types
            }
        }
    }

    pub(crate) fn emit_func_args(
        &mut self,
        args: &Vec<CIRExpr>,
        params_infos: &[ABIArgInfo],
        params_types: &[ABIType],
        cir_params_types: &[CIRType],
    ) -> Vec<BasicMetadataValueEnum<'ll>> {
        let mut args_values = Vec::with_capacity(args.len());

        for (i, expr) in args.iter().enumerate() {
            let cir_param_type = &cir_params_types.get(i).cloned();

            let lvalue = self.emit_expr(expr, cir_param_type);
            let mut rvalue = self.load_rvalue(lvalue.clone());

            if i < params_infos.len() {
                self.emit_abi_arg(params_types, &params_infos[i], &lvalue, &rvalue, &mut args_values);
            } else {
                // classify variadic argument value
                let promoted_rvalue_type = self.target.target_abi.apply_variadic_argument_promote(&rvalue.ty);
                let llvm_promoted_type = self.emit_type(promoted_rvalue_type.clone());

                let promoted_value: BasicValueEnum<'ll> =
                    self.emit_cast(llvm_promoted_type, rvalue).try_into().unwrap();

                rvalue = InternalValue::new(promoted_rvalue_type, InternalValueKind::RValue(promoted_value));

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

    pub(crate) fn emit_abi_pair_llvm_type(&self, lo: &ABIType, hi: &ABIType) -> StructType<'ll> {
        let lo_ty: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, lo)
            .try_into()
            .unwrap();

        let hi_ty: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, hi)
            .try_into()
            .unwrap();

        self.llvm_ctx.struct_type(&[lo_ty.into(), hi_ty.into()], false)
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
        args: &mut Vec<BasicMetadataValueEnum<'ll>>,
        value: &InternalValue<'ll>,
        lo: ABIType,
        hi: ABIType,
    ) {
        match &value.kind {
            InternalValueKind::LValue(ptr) => {
                let pair_ty = self.emit_abi_pair_llvm_type(&lo, &hi);

                let lo_ty: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, &lo)
                    .try_into()
                    .unwrap();

                let lo_ptr = self.llvmbuilder.build_struct_gep(pair_ty, *ptr, 0, "lo.ptr").unwrap();

                let lo_val = self.llvmbuilder.build_load(lo_ty, lo_ptr, "lo").unwrap();

                let hi_ty: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, &hi)
                    .try_into()
                    .unwrap();

                let hi_ptr = self.llvmbuilder.build_struct_gep(pair_ty, *ptr, 1, "hi.ptr").unwrap();

                let hi_val = self.llvmbuilder.build_load(hi_ty, hi_ptr, "hi").unwrap();

                args.push(lo_val.into());
                args.push(hi_val.into());
            }

            InternalValueKind::RValue(basic_value) => match basic_value {
                BasicValueEnum::StructValue(struct_val) => {
                    let lo_val = self.llvmbuilder.build_extract_value(*struct_val, 0, "lo").unwrap();
                    let hi_val = self.llvmbuilder.build_extract_value(*struct_val, 1, "hi").unwrap();

                    args.push(lo_val.into());
                    args.push(hi_val.into());
                }

                BasicValueEnum::ArrayValue(array_val) => {
                    let lo_val = self.llvmbuilder.build_extract_value(*array_val, 0, "lo").unwrap();
                    let hi_val = self.llvmbuilder.build_extract_value(*array_val, 1, "hi").unwrap();

                    args.push(lo_val.into());
                    args.push(hi_val.into());
                }

                BasicValueEnum::IntValue(int_val) => {
                    // Split i128 into two i64 values (low and high parts)
                    let i64_type = self.llvm_ctx.i64_type();
                    let i128_type = self.llvm_ctx.i128_type();

                    let is_signed = value.ty.is_signed_integer();

                    // extract low 64 bits (mask with 0xFFFFFFFFFFFFFFFF)
                    let low_value = self.llvmbuilder.build_int_truncate(*int_val, i64_type, "lo").unwrap();

                    // extract high 64 bits (shift right by 64)

                    let shift_amount = i128_type.const_int(64, is_signed);
                    let high_val_i128 = self
                        .llvmbuilder
                        .build_right_shift(*int_val, shift_amount, false, "hi.tmp")
                        .unwrap();

                    let high_value = self
                        .llvmbuilder
                        .build_int_truncate(high_val_i128, i64_type, "hi")
                        .unwrap();

                    args.push(low_value.into());
                    args.push(high_value.into());
                }

                other => {
                    panic!("direct-pair rvalue must be struct or array, got {:?}", other);
                }
            },
            _ => unreachable!(),
        }
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
        param_types: &[ABIType],
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

                let lo_llvm: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, &lo)
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

                let hi_llvm: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, &hi)
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
        let llvm_ty: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, &ty)
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

    pub(crate) fn emit_func_call_attributes(
        &mut self,
        abi_func_info: &ABIFunctionInfo,
        func_call_kind: FuncCallKind<'ll>,
    ) {
        match func_call_kind {
            FuncCallKind::Direct(llvm_func_value) => {
                self.emit_direct_func_call_args_attributes(&llvm_func_value, abi_func_info)
            }
            FuncCallKind::Indirect(call_site) => {
                self.emit_indirect_func_call_args_attributes(&call_site, abi_func_info)
            }
        }
    }

    fn emit_direct_func_call_args_attributes(
        &mut self,
        llvm_func_value: &FunctionValue,
        abi_func_info: &ABIFunctionInfo,
    ) {
        let byval_attr_kind = unsafe { LLVMGetEnumAttributeKindForName(c!("byval").as_ptr(), 5) };
        let sret_attr_kind = unsafe { LLVMGetEnumAttributeKindForName(c!("sret").as_ptr(), 4) };

        let mut llvm_param_index_offset = 0;

        if abi_func_info.ret_info.kind.is_indirect_sret() {
            let ret_ty = &abi_func_info.ret_info.abi_type;
            let llvm_ret_ty = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, ret_ty);

            let attr = unsafe {
                LLVMCreateTypeAttribute(self.llvm_ctx.as_ctx_ref(), sret_attr_kind, llvm_ret_ty.as_type_ref())
            };

            unsafe {
                LLVMAddAttributeAtIndex(llvm_func_value.as_value_ref(), 1, attr);
            }

            llvm_param_index_offset = 1;
        }

        for (i, param_info) in abi_func_info.params_infos.iter().enumerate() {
            if param_info.is_indirect_by_val() {
                let struct_type = &abi_func_info.params_types[i];
                let llvm_struct_type = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, struct_type);

                let attr = unsafe {
                    LLVMCreateTypeAttribute(
                        self.llvm_ctx.as_ctx_ref(),
                        byval_attr_kind,
                        llvm_struct_type.as_type_ref(),
                    )
                };

                // index is 1-based, 0 is return
                unsafe {
                    LLVMAddAttributeAtIndex(
                        llvm_func_value.as_value_ref(),
                        i as u32 + 1 + llvm_param_index_offset,
                        attr,
                    );
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
            let llvm_ret_ty = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, ret_ty);

            let attr = unsafe {
                LLVMCreateTypeAttribute(self.llvm_ctx.as_ctx_ref(), sret_attr_kind, llvm_ret_ty.as_type_ref())
            };

            unsafe {
                LLVMAddCallSiteAttribute(call_site.as_value_ref(), 1, attr);
            }

            llvm_param_index_offset = 1;
        }

        for (i, param_info) in abi_func_info.params_infos.iter().enumerate() {
            if !param_info.is_indirect_by_val() {
                continue;
            }

            let param_type = abi_func_info.params_types.get(i).unwrap().clone();
            let pointee_type = abi_type_to_llvm_type(self.llvm_ctx, &self.target.info, &param_type);

            let attr = unsafe {
                LLVMCreateTypeAttribute(self.llvm_ctx.as_ctx_ref(), byval_attr_kind, pointee_type.as_type_ref())
            };

            // index is 1-based, 0 is return
            unsafe {
                LLVMAddCallSiteAttribute(call_site.as_value_ref(), i as u32 + 1 + llvm_param_index_offset, attr);
            }
        }
    }
}
