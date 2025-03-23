use crate::CodeGenLLVM;
use crate::diag::{Diag, DiagKind, DiagLevel, DiagLoc};
use ast::ast::{FuncDecl, FuncDef, FuncParam};
use ast::token::{Location, Span, Token, TokenKind};
use inkwell::llvm_sys::core::LLVMFunctionType;
use inkwell::types::FunctionType;
use inkwell::values::FunctionValue;
use inkwell::{llvm_sys::LLVMType, types::AsTypeRef};
use std::ptr::null_mut;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn compile_func_params(
        &mut self,
        func_name: String,
        func_loc: Location,
        span_end: usize,
        params: Vec<FuncParam>,
    ) -> Vec<*mut LLVMType> {
        params
            .iter()
            .map(|param| {
                if let Some(param_type_token) = &param.ty {
                    self.build_type(param_type_token.clone()).as_type_ref()
                } else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::TypeAnnotationRequired(param.identifier.name.clone(), func_name.clone()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: func_loc.line,
                            column: func_loc.column,
                            length: span_end,
                        }),
                    });
                    null_mut()
                }
            })
            .collect()
    }

    pub(crate) fn compile_func_decl(&mut self, func_decl: FuncDecl) -> FunctionValue {
        let is_var_args = func_decl.params.variadic.is_some();
        let mut param_types = self.compile_func_params(
            func_decl.name.clone(),
            func_decl.loc,
            func_decl.span.end,
            func_decl.params.list,
        );

        let return_type = self.build_type(
            func_decl
                .return_type
                .unwrap_or(Token {
                    kind: TokenKind::Void,
                    span: Span::default(),
                })
                .kind,
        );

        let fn_type = unsafe {
            FunctionType::new(LLVMFunctionType(
                return_type.as_type_ref(),
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                is_var_args as i32,
            ))
        };

        let func_linkage = self.build_linkage(func_decl.vis_type);
        self.module.add_function(&func_decl.name, fn_type, Some(func_linkage))
    }

    pub(crate) fn compile_func_def(&mut self, func_def: FuncDef) {
        let is_var_args = func_def.params.variadic.is_some();
        let mut param_types = self.compile_func_params(
            func_def.name.clone(),
            func_def.loc,
            func_def.span.end,
            func_def.params.list,
        );

        let return_type = self.build_type(
            func_def
                .return_type
                .unwrap_or(Token {
                    kind: TokenKind::Void,
                    span: Span::default(),
                })
                .kind,
        );

        let fn_type = unsafe {
            FunctionType::new(LLVMFunctionType(
                return_type.as_type_ref(),
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                is_var_args as i32,
            ))
        };

        let func_linkage = self.build_linkage(func_def.vis_type);
        let func = self.module.add_function(&func_def.name, fn_type, Some(func_linkage));

        let entry_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry_block);
    }
}
