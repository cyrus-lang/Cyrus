use ast::ast::{FuncDecl, FuncDef, FuncParam};
use ast::token::{Span, Token, TokenKind};
use inkwell::llvm_sys::core::LLVMFunctionType;
use inkwell::types::FunctionType;
use inkwell::values::FunctionValue;
use inkwell::{llvm_sys::LLVMType, types::AsTypeRef};
use utils::compile_time_errors::errors::*;
use utils::compiler_error;

use crate::CodeGenLLVM;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn compile_func_params(&self, func_name: String, params: Vec<FuncParam>) -> Vec<*mut LLVMType> {
        params
            .iter()
            .map(|param| {
                if let Some(param_type_token) = &param.ty {
                    self.build_type(param_type_token.clone()).as_type_ref()
                } else {
                    // FIXME
                    // Move to central diagnostics crate
                    compiler_error!(
                        format!(
                            "Type annotation required for parameter '{}' in function '{}'.",
                            param.identifier.name.clone(),
                            func_name,
                        ),
                        self.file_path.clone()
                    );
                }
            })
            .collect()
    }

    pub(crate) fn compile_func_decl(&self, func_decl: FuncDecl) -> FunctionValue {
        let is_var_args = func_decl.params.variadic.is_some();
        let mut param_types = self.compile_func_params(func_decl.name.clone(), func_decl.params.list.clone());

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

    pub(crate) fn compile_func_def(&self, func_def: FuncDef) {
        let func_decl = {
            self.compile_func_decl(FuncDecl {
                name: func_def.name.clone(),
                params: func_def.params,
                return_type: func_def.return_type,
                vis_type: func_def.vis_type,
                renamed_as: None,
                span: func_def.span,
                loc: func_def.loc,
            })
        };

        let entry_block = self.context.append_basic_block(func_decl, "entry");
        self.builder.position_at_end(entry_block);
        // TODO
    }
}
