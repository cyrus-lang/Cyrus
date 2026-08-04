// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::builder::{
    builder::CodeGenIRBuilder,
    values::{InternalValue, InternalValueKind},
};
use cyrusc_internal::cir::{cir::CIRInlineAsm, types::CIRType};
use cyrusc_typed_ast::types::PlainType;
use inkwell::{
    llvm_sys::{core::LLVMGetInlineAsm, LLVMInlineAsmDialect},
    types::{AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::BasicMetadataValueEnum,
};
use std::ffi::CString;


fn is_memory_output(constraint: &str) -> bool {
    let after_dir = constraint.trim_start_matches(|c| c == '=' || c == '+');
    after_dir.starts_with('*')
}

impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_inline_asm(&mut self, asm: &CIRInlineAsm) -> InternalValue<'ll> {


        let mut constraints: Vec<String> = Vec::new();
        for op in &asm.outputs  { constraints.push(op.constraint.clone()); }
        for op in &asm.inputs   { constraints.push(op.constraint.clone()); }
        for clobber in &asm.clobbers { constraints.push(format!("~{{{}}}", clobber)); }
        let constraint_str = constraints.join(",");

        let (reg_out_indices, mem_out_indices): (Vec<usize>, Vec<usize>) =
            (0..asm.outputs.len()).partition(|&i| !is_memory_output(&asm.outputs[i].constraint));
        let mut arg_values:  Vec<BasicMetadataValueEnum<'ll>> = Vec::new();
        let mut param_types: Vec<BasicMetadataTypeEnum<'ll>>  = Vec::new();
        let ptr_type = self.llvm_ctx.ptr_type(inkwell::AddressSpace::default());
        
        for &i in &mem_out_indices {
            let op = &asm.outputs[i];
            let lvalue = self.emit_expr(&op.expr, &None);
            let ptr = match lvalue.kind {
                InternalValueKind::LValue(ptr) => ptr,
                _ => {
                    let llvm_ty: BasicTypeEnum<'ll> =
                        self.emit_type(lvalue.ty.clone()).try_into().unwrap();
                    self.llvmbuilder.build_alloca(llvm_ty, "asm.mem.out").unwrap()
                }
            };
            arg_values.push(ptr.into());
            param_types.push(ptr_type.into());
        }

        for op in &asm.inputs {
            let lvalue = self.emit_expr(&op.expr, &None);
            let rvalue = self.load_rvalue(lvalue);
            let llvm_ty: BasicTypeEnum<'ll> =
                self.emit_type(op.expr.ty.clone()).try_into().unwrap();
            arg_values.push(rvalue.as_basic_value().into());
            param_types.push(llvm_ty.into());
        }

        let reg_out_types: Vec<BasicTypeEnum<'ll>> = reg_out_indices
            .iter()
            .map(|&i| {
                self.emit_type(asm.outputs[i].expr.ty.clone())
                    .try_into()
                    .unwrap()
            })
            .collect();

        let fn_type: FunctionType<'ll> = match reg_out_types.len() {
            0 => self.llvm_ctx.void_type().fn_type(&param_types, false),
            1 => reg_out_types[0].fn_type(&param_types, false),
            _ => {
                let struct_ty = self.llvm_ctx.struct_type(&reg_out_types, false);
                struct_ty.fn_type(&param_types, false)
            }
        };

        let asm_cstr        = CString::new(asm.template.clone()).unwrap();
        let constraint_cstr = CString::new(constraint_str.clone()).unwrap();

        let llvm_inline_asm = unsafe {
            LLVMGetInlineAsm(
                fn_type.as_type_ref(),
                asm_cstr.as_ptr()        as *mut _,
                asm.template.len(),
                constraint_cstr.as_ptr() as *mut _,
                constraint_str.len(),
                asm.is_volatile as i32,
                0,
                LLVMInlineAsmDialect::LLVMInlineAsmDialectATT,
                0,
            )
        };

        let asm_ptr = unsafe { inkwell::values::PointerValue::new(llvm_inline_asm) };

        let call_site = self
            .llvmbuilder
            .build_indirect_call(fn_type, asm_ptr, &arg_values, "asm.call")
            .unwrap();

        match reg_out_indices.len() {
            0 => {  }
            1 => {
                if let Some(ret_val) = call_site.try_as_basic_value().basic() {
                    let op = &asm.outputs[reg_out_indices[0]];
                    let lvalue = self.emit_expr(&op.expr, &None);
                    if let InternalValueKind::LValue(ptr) = lvalue.kind {
                        self.llvmbuilder.build_store(ptr, ret_val).unwrap();
                    }
                }
            }
            _ => {
                if let Some(ret_val) = call_site.try_as_basic_value().basic() {
                    let struct_val = ret_val.into_struct_value();
                    for (field_idx, &out_idx) in reg_out_indices.iter().enumerate() {
                        let field = self.llvmbuilder
                            .build_extract_value(struct_val, field_idx as u32, "asm.reg.out")
                            .unwrap();
                        let op = &asm.outputs[out_idx];
                        let lvalue = self.emit_expr(&op.expr, &None);
                        if let InternalValueKind::LValue(ptr) = lvalue.kind {
                            self.llvmbuilder.build_store(ptr, field).unwrap();
                        }
                    }
                }
            }
        }

        let cir_void_ptr = CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void)));
        self.emit_null(cir_void_ptr)
    }
}