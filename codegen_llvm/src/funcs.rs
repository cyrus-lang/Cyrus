use crate::CodeGenLLVM;
use crate::diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag};
use ast::ast::{FuncDecl, FuncDef, FuncParam};
use ast::token::{Location, Span, Token, TokenKind};
use inkwell::llvm_sys::core::LLVMFunctionType;
use inkwell::types::FunctionType;
use inkwell::values::{AnyValueEnum, FunctionValue};
use inkwell::{llvm_sys::LLVMType, types::AsTypeRef};
use std::process::exit;
use utils::generate_random_hex::generate_random_hex;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_func_params(
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
                    self.build_type(param_type_token.clone(), func_loc.clone(), span_end)
                        .as_type_ref()
                } else {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::TypeAnnotationRequired(param.identifier.name.clone(), func_name.clone()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: func_loc.line,
                            column: func_loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
            })
            .collect()
    }

    pub(crate) fn build_func_decl(&mut self, func_decl: FuncDecl) -> FunctionValue {
        let is_var_args = func_decl.params.variadic.is_some();
        let mut param_types = self.build_func_params(
            func_decl.name.clone(),
            func_decl.loc.clone(),
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
            func_decl.loc,
            func_decl.span.end,
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

    pub(crate) fn build_func_def(&mut self, mut func_def: FuncDef) -> FunctionValue {
        if func_def.name == "main" && self.entry_point.is_some() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(String::from("Multiple entry point not allowed.")),
                location: None,
            });
            exit(1);
        }

        let is_var_args = func_def.params.variadic.is_some();
        let mut param_types = self.build_func_params(
            func_def.name.clone(),
            func_def.loc.clone(),
            func_def.span.end,
            func_def.params.list,
        );

        let return_type_token = func_def
            .return_type
            .unwrap_or(Token {
                kind: TokenKind::Void,
                span: Span::default(),
            })
            .kind;

        let return_type = self.build_type(return_type_token.clone(), func_def.loc.clone(), func_def.span.end);

        let fn_type = unsafe {
            FunctionType::new(LLVMFunctionType(
                return_type.as_type_ref(),
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                is_var_args as i32,
            ))
        };

        let mut is_main = false;
        if func_def.name == "main" {
            func_def.name = format!("main_{}", generate_random_hex());
            is_main = true;
        }

        let func_linkage = self.build_linkage(func_def.vis_type);
        let func = self.module.add_function(&func_def.name, fn_type, Some(func_linkage));

        let entry_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry_block);

        let mut build_return = false;
        for expr in func_def.body.exprs {
            match expr {
                ast::ast::Statement::Return(return_statement) => {
                    build_return = true;
                    let expr = self.build_expr(return_statement.argument);
                    self.build_return(expr);
                }
                _ => self.compile_statement(expr),
            }
        }

        if return_type_token == TokenKind::Void {
            if build_return {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "The function '{}' is not allowed to have a return statement.",
                        func_def.name
                    )),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: func_def.loc.line,
                        column: func_def.loc.column,
                        length: func_def.span.end,
                    }),
                });
                exit(1);
            }

            let _ = self.builder.build_return(None).unwrap();
        } else if !build_return {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "The function '{}' is missing a return statement.",
                    func_def.name
                )),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: func_def.loc.line,
                    column: func_def.loc.column,
                    length: func_def.span.end,
                }),
            });
            exit(1);
        }

        func.verify(true);

        if is_main {
            self.entry_point = Some(func);
        }

        return func;
    }

    pub(crate) fn build_return(&self, value: AnyValueEnum) {
        let result = match value {
            AnyValueEnum::IntValue(int_value) => self.builder.build_return(Some(&int_value)),
            AnyValueEnum::FloatValue(float_value) => self.builder.build_return(Some(&float_value)),
            AnyValueEnum::PointerValue(pointer_value) => self.builder.build_return(Some(&pointer_value)),
            AnyValueEnum::VectorValue(vector_value) => self.builder.build_return(Some(&vector_value)),
            AnyValueEnum::ArrayValue(array_value) => self.builder.build_return(Some(&array_value)),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(String::from("Cannot build return statement with non-basic value.")),
                    location: None,
                });
                exit(1);
            }
        };

        if let Err(err) = result {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!("Cannot store value in pointer:\n{}", err.to_string())),
                location: None,
            });
            exit(1);
        }
    }
}
