use crate::CodeGenLLVM;
use crate::diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag};
use crate::scope::{Scope, ScopeRef};
use crate::values::AnyValue;
use ast::ast::{Expression, FieldAccessOrMethodCall, FuncCall, FuncDecl, FuncDef, FuncParam, VisType};
use ast::token::{Location, Span, Token, TokenKind};
use inkwell::builder::BuilderError;
use inkwell::llvm_sys::core::LLVMFunctionType;
use inkwell::llvm_sys::prelude::LLVMTypeRef;
use inkwell::types::FunctionType;
use inkwell::values::{BasicMetadataValueEnum, CallSiteValue, FunctionValue, InstructionValue};
use std::cell::RefCell;
use std::collections::HashMap;
use std::process::exit;
use std::rc::Rc;

pub struct FuncMetadata<'a> {
    pub ptr: FunctionValue<'a>,
    pub vis_type: VisType,
}

pub type FuncTable<'a> = HashMap<String, FuncMetadata<'a>>;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_func_params(
        &mut self,
        func_name: String,
        func_loc: Location,
        span_end: usize,
        params: Vec<FuncParam>,
    ) -> Vec<LLVMTypeRef> {
        params
            .iter()
            .map(|param| {
                if let Some(param_type_token) = &param.ty {
                    self.build_type(param_type_token.clone(), func_loc.clone(), span_end)
                        .as_type_ref()
                } else {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::TypeAnnotationRequiredForParam(
                            param.identifier.name.clone(),
                            func_name.clone(),
                        ),
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

        let func_linkage = self.build_linkage(func_decl.vis_type.clone());
        let func_ptr = self.module.add_function(&func_decl.name, fn_type, Some(func_linkage));

        self.func_table.insert(
            func_decl.name,
            FuncMetadata {
                ptr: func_ptr,
                vis_type: func_decl.vis_type,
            },
        );

        func_ptr
    }

    pub(crate) fn build_func_def(&mut self, func_def: FuncDef) -> FunctionValue<'ctx> {
        let scope: ScopeRef<'ctx> = Rc::new(RefCell::new(Scope::new()));

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

        let func_linkage = self.build_linkage(func_def.vis_type.clone());
        let func = self.module.add_function(&func_def.name, fn_type, Some(func_linkage));
        self.current_func_ref = Some(func);

        let entry_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry_block);
        self.current_block_ref = Some(entry_block);

        let mut build_return = false;
        for expr in func_def.body.exprs {
            match expr {
                ast::ast::Statement::Return(return_statement) => {
                    build_return = true;
                    let expr = self.build_expr(Rc::clone(&scope), return_statement.argument);
                    self.build_return(expr);
                }
                _ => self.build_statement(Rc::clone(&scope), expr),
            }
        }

        if return_type_token == TokenKind::Void {
            if build_return {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "The function '{}' is not allowed to have a return statement.",
                        &func_def.name
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
                    &func_def.name
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

        self.func_table.insert(
            func_def.name,
            FuncMetadata {
                ptr: func,
                vis_type: func_def.vis_type,
            },
        );

        return func;
    }

    pub(crate) fn build_return(&mut self, value: AnyValue) {
        let result: Result<InstructionValue, BuilderError> = match value {
            AnyValue::IntValue(int_value) => self.builder.build_return(Some(&int_value)),
            AnyValue::FloatValue(float_value) => self.builder.build_return(Some(&float_value)),
            AnyValue::PointerValue(pointer_value) => self.builder.build_return(Some(&pointer_value.ptr)),
            AnyValue::VectorValue(vector_value) => self.builder.build_return(Some(&vector_value)),
            AnyValue::ArrayValue(array_value) => self.builder.build_return(Some(&array_value)),
            AnyValue::StructValue(struct_value) => self.builder.build_return(Some(&struct_value)),
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
                kind: DiagKind::Custom(format!("Cannot build return statement:\n{}", err.to_string())),
                location: None,
            });
            exit(1);
        }

        // mark entry block terminated
        if let Some(current_block) = self.current_block_ref {
            self.mark_block_terminated(current_block);
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Failed to mark block terminated.".to_string()),
                location: None,
            });
        }
    }

    pub(crate) fn build_field_access_or_method_call(
        &self,
        scope: ScopeRef<'ctx>,
        field_access_or_method_call: FieldAccessOrMethodCall,
    ) -> AnyValue<'ctx> {
        let mut final_result = self.build_expr(Rc::clone(&scope), *field_access_or_method_call.expr);

        for item in field_access_or_method_call.chains {
            match item {
                either::Either::Left(method_call) => {
                    let call_site_value = self.build_func_call(Rc::clone(&scope), method_call);
                    if let Some(basic_value) = call_site_value.try_as_basic_value().left() {
                        final_result = AnyValue::try_from(basic_value).unwrap();
                    }
                }
                either::Either::Right(_) => todo!(),
            }
        }

        final_result
    }

    pub(crate) fn build_arguments(
        &self,
        scope: ScopeRef<'ctx>,
        arguments: Vec<Expression>,
        loc: Location,
        span_end: usize,
    ) -> Vec<BasicMetadataValueEnum<'ctx>> {
        arguments
            .iter()
            .map(|arg| match self.build_expr(Rc::clone(&scope), arg.clone()) {
                AnyValue::ArrayValue(array_value) => BasicMetadataValueEnum::ArrayValue(array_value),
                AnyValue::IntValue(int_value) => BasicMetadataValueEnum::IntValue(int_value),
                AnyValue::FloatValue(float_value) => BasicMetadataValueEnum::FloatValue(float_value),
                AnyValue::PointerValue(pointer_value) => BasicMetadataValueEnum::PointerValue(pointer_value.ptr),
                AnyValue::OpaquePointer(pointer_value) => BasicMetadataValueEnum::PointerValue(pointer_value),
                AnyValue::StructValue(struct_value) => BasicMetadataValueEnum::StructValue(struct_value),
                AnyValue::VectorValue(vector_value) => BasicMetadataValueEnum::VectorValue(vector_value),
                AnyValue::StringValue(string_value) => BasicMetadataValueEnum::StructValue(string_value.struct_value),
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Cannot build non-basic value as an argument in func call.".to_string()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
            })
            .collect()
    }

    // FIXME func_call.func_name.identifier.name
    pub(crate) fn build_func_call(&self, scope: ScopeRef<'ctx>, func_call: FuncCall) -> CallSiteValue<'ctx> {
        // FIXME
        todo!();
        
        // let arguments = &self.build_arguments(
        //     Rc::clone(&scope),
        //     func_call.arguments,
        //     func_call.loc.clone(),
        //     func_call.span.end,
        // );

        // if let Some(func_metadata) = self.func_table.get(&func_call.func_name.identifier.name.clone()) {
        //     self.builder.build_call(func_metadata.ptr, arguments, "call").unwrap()
        // } else {
        //     display_single_diag(Diag {
        //         level: DiagLevel::Error,
        //         kind: DiagKind::FuncNotFound(func_call.func_name.identifier.name),
        //         location: Some(DiagLoc {
        //             file: self.file_path.clone(),
        //             line: func_call.loc.line,
        //             column: func_call.loc.column,
        //             length: func_call.span.end,
        //         }),
        //     });
        //     exit(1);
        // }
    }
}
