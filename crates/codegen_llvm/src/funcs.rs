use crate::diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag};
use crate::scope::{Scope, ScopeRecord, ScopeRef};
use crate::values::InternalValue;
use crate::{CodeGenLLVM, InternalType};
use ast::ast::{Expression, FuncCall, FuncDecl, FuncDef, FuncParam, FuncParams, TypeSpecifier};
use ast::token::{Location, Span, Token, TokenKind};
use inkwell::builder::BuilderError;
use inkwell::llvm_sys::core::LLVMFunctionType;
use inkwell::llvm_sys::prelude::LLVMTypeRef;
use inkwell::types::FunctionType;
use inkwell::values::{BasicMetadataValueEnum, CallSiteValue, FunctionValue, InstructionValue};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::DerefMut;
use std::process::exit;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct FuncMetadata<'a> {
    pub ptr: FunctionValue<'a>,
    pub func_decl: FuncDecl,
    pub return_type: InternalType<'a>,
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

    pub(crate) fn build_func_decl(&mut self, func_decl: FuncDecl) -> FunctionValue<'ctx> {
        let is_var_args = func_decl.params.variadic.is_some();
        let mut param_types = self.build_func_params(
            func_decl.name.clone(),
            func_decl.loc.clone(),
            func_decl.span.end,
            func_decl.params.list.clone(),
        );

        let return_type = self.build_type(
            func_decl.return_type.clone().unwrap_or(TypeSpecifier::TypeToken(Token {
                kind: TokenKind::Void,
                span: Span::default(),
            })),
            func_decl.loc.clone(),
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

        let func_linkage = self.build_linkage(func_decl.storage_class.clone());
        let func_ptr = self
            .module
            .borrow_mut()
            .deref_mut()
            .add_function(&func_decl.name, fn_type, Some(func_linkage));

        self.func_table.insert(
            if let Some(renamed_as) = func_decl.renamed_as.clone() {
                renamed_as
            } else {
                func_decl.name.clone()
            },
            FuncMetadata {
                func_decl: func_decl.clone(),
                ptr: func_ptr,
                return_type,
            },
        );

        func_ptr
    }

    pub(crate) fn build_func_def(&mut self, func_def: FuncDef) -> FunctionValue<'ctx> {
        let scope: ScopeRef<'ctx> = Rc::new(RefCell::new(Scope::new()));

        let mut func_decl = FuncDecl {
            name: func_def.name.clone(),
            params: func_def.params.clone(),
            return_type: func_def.return_type.clone(),
            storage_class: func_def.storage_class.clone(),
            renamed_as: Some(func_def.name.clone()),
            span: func_def.span.clone(),
            loc: func_def.loc.clone(),
        };

        let is_var_args = func_def.params.variadic.is_some();
        let mut param_types = self.build_func_params(
            func_def.name.clone(),
            func_def.loc.clone(),
            func_def.span.end,
            func_def.params.list.clone(),
        );

        let return_type = self.build_type(
            TypeSpecifier::TypeToken(Token {
                kind: TokenKind::Void,
                span: Span::default(),
            }),
            func_def.loc.clone(),
            func_def.span.end,
        );

        let fn_type = unsafe {
            FunctionType::new(LLVMFunctionType(
                return_type.as_type_ref(),
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                is_var_args as i32,
            ))
        };

        let actual_func_name = format!("{}.{}", self.module_name, func_def.name.clone());
        func_decl.name = actual_func_name.clone();

        let func_linkage = self.build_linkage(func_def.storage_class.clone());
        let func = self
            .module
            .borrow_mut()
            .deref_mut()
            .add_function(&actual_func_name, fn_type, Some(func_linkage));

        self.current_func_ref = Some(func);

        let entry_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry_block);
        self.current_block_ref = Some(entry_block);

        let mut scope_borrowed = scope.borrow_mut();
        for (idx, param) in func.get_param_iter().enumerate() {
            let param_ptr = self.builder.build_alloca(param.get_type(), "param").unwrap();
            self.builder.build_store(param_ptr, param).unwrap();
            if let Some(func_param) = func_def.params.list.get(idx) {
                let param_ptr_type = param.get_type().try_into().unwrap();

                scope_borrowed.insert(
                    func_param.identifier.name.clone(),
                    ScopeRecord {
                        ptr: param_ptr,
                        ty: param_ptr_type,
                    },
                );
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Unmatched parameter for function '{}' when adding params to the scope.",
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
        }
        drop(scope_borrowed);

        let mut build_return = false;
        for expr in func_def.body.exprs {
            match expr {
                ast::ast::Statement::Return(return_statement) => {
                    build_return = true;
                    let expr =
                        self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), return_statement.argument));
                    self.build_return(expr);
                }
                _ => self.build_statement(Rc::clone(&scope), expr),
            }
        }

        if return_type.is_void_type() {
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
            if let Some(renamed_as) = func_decl.renamed_as.clone() {
                renamed_as
            } else {
                func_decl.name.clone()
            },
            FuncMetadata {
                func_decl,
                ptr: func,
                return_type,
            },
        );

        return func;
    }

    pub(crate) fn build_return(&mut self, value: InternalValue) {
        let result: Result<InstructionValue, BuilderError> = match value {
            InternalValue::IntValue(int_value, _) => self.builder.build_return(Some(&int_value)),
            InternalValue::FloatValue(float_value, _) => self.builder.build_return(Some(&float_value)),
            InternalValue::PointerValue(pointer_value) => self.builder.build_return(Some(&pointer_value.ptr)),
            InternalValue::VectorValue(vector_value, _) => self.builder.build_return(Some(&vector_value)),
            InternalValue::ArrayValue(array_value, _) => self.builder.build_return(Some(&array_value)),
            InternalValue::StructValue(struct_value, _) => self.builder.build_return(Some(&struct_value)),
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

    // FIXME
    // pub(crate) fn build_field_access_or_method_call(
    //     &self,
    //     scope: ScopeRef<'ctx>,
    //     field_access_or_method_call: FieldAccessOrMethodCall,
    // ) -> InternalValue<'ctx> {
    //     let mut final_result = self.build_expr(Rc::clone(&scope), *field_access_or_method_call.expr);

    //     for item in field_access_or_method_call.chains {
    //         match item {
    //             either::Either::Left(method_call) => {
    //                 // here we get the function from "imported module value"
    //                 // we call it, and assign the result into final_result and continue the process as you can see.
    //                 if let InternalValue::ImportedModuleValue(ref imported_module_value) = final_result {
    //                     if let Some(func_metadata) = imported_module_value
    //                         .metadata
    //                         .imported_funcs
    //                         .get(&method_call.identifier.name.clone())
    //                     {
    //                         let mut new_method_call = method_call.clone();
    //                         new_method_call.identifier.name = func_metadata.func_decl.name.clone();
    //                         let call_site_value = self.build_func_call(Rc::clone(&scope), new_method_call);
    //                         if let Some(basic_value) = call_site_value.try_as_basic_value().left() {
    //                             final_result = InternalValue::try_from(basic_value).unwrap();
    //                         }
    //                     } else {
    //                         display_single_diag(Diag {
    //                             level: DiagLevel::Error,
    //                             kind: DiagKind::Custom(format!(
    //                                 "Function '{}' not defined in module '{}'.",
    //                                 method_call.identifier.name, imported_module_value.metadata.identifier
    //                             )),
    //                             location: None,
    //                         });
    //                         exit(1);
    //                     }
    //                 } else {
    //                     // ordinary value
    //                     let call_site_value = self.build_func_call(Rc::clone(&scope), method_call);
    //                     if let Some(basic_value) = call_site_value.try_as_basic_value().left() {
    //                         final_result = InternalValue::try_from(basic_value).unwrap();
    //                     }
    //                 }
    //             }
    //             either::Either::Right(field_access) => {
    //                 if let InternalValue::StructValue(struct_value) = final_result {
    //                     let (struct_name, struct_metadata) = self.find_struct_by_type(
    //                         struct_value.get_type(),
    //                         field_access.loc.clone(),
    //                         field_access.span.end,
    //                     );

    //                     match struct_metadata
    //                         .fields
    //                         .iter()
    //                         .position(|f| f.name == field_access.identifier.name)
    //                     {
    //                         Some(field_idx) => {
    //                             let basic_value = self
    //                                 .builder
    //                                 .build_extract_value(struct_value, field_idx.try_into().unwrap(), "extract")
    //                                 .unwrap();
    //                             final_result = basic_value.try_into().unwrap();
    //                         }
    //                         None => {
    //                             display_single_diag(Diag {
    //                                 level: DiagLevel::Error,
    //                                 kind: DiagKind::Custom(format!(
    //                                     "Undefined field '{}' for struct '{}'.",
    //                                     field_access.identifier.name, struct_name,
    //                                 )),
    //                                 location: None,
    //                             });
    //                             exit(1);
    //                         }
    //                     }
    //                 } else {
    //                     display_single_diag(Diag {
    //                         level: DiagLevel::Error,
    //                         kind: DiagKind::Custom("Cannot build field access for non-struct values.".to_string()),
    //                         location: None,
    //                     });
    //                     exit(1);
    //                 }
    //             }
    //         }
    //     }

    //     final_result
    // }

    pub(crate) fn build_arguments(
        &self,
        scope: ScopeRef<'ctx>,
        arguments: Vec<Expression>,
        params: Option<FuncParams>,
    ) -> Vec<BasicMetadataValueEnum<'ctx>> {
        arguments
            .iter()
            .enumerate()
            .map(|(idx, arg)| {
                let rvalue = self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), arg.clone()));

                if let Some(params) = &params {
                    // checked before through check_func_args_count_mismatch
                    if let Some(param) = params.list.get(idx) {
                        if let Some(target_type) = &param.ty {
                            self.implicitly_casted(
                                rvalue,
                                self.build_type(target_type.clone(), param.loc.clone(), param.span.end),
                            )
                            .into()
                        } else {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom("Cannot build untyped function param as argument.".to_string()),
                                location: Some(DiagLoc {
                                    file: self.file_path.clone(),
                                    line: param.loc.line,
                                    column: param.loc.column,
                                    length: param.span.end,
                                }),
                            });
                            exit(1);
                        }
                    } else {
                        rvalue.to_basic_metadata()
                    }
                } else {
                    rvalue.to_basic_metadata()
                }
            })
            .collect()
    }

    pub(crate) fn check_func_args_count_mismatch(&self, func_name: String, func_decl: FuncDecl, func_call: FuncCall) {
        if func_decl.params.variadic.is_none() && func_decl.params.list.len() != func_call.arguments.len() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::FuncCallArgumentCountMismatch(
                    func_name.clone(),
                    func_call.arguments.len().try_into().unwrap(),
                    func_decl.params.list.len().try_into().unwrap(),
                ),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: func_call.loc.line,
                    column: func_call.loc.column,
                    length: func_call.span.end,
                }),
            });
            exit(1);
        } else if func_decl.params.variadic.is_some() && func_call.arguments.len() < func_decl.params.list.len() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::FuncCallArgumentCountMismatch(
                    func_name.clone(),
                    func_call.arguments.len().try_into().unwrap(),
                    func_decl.params.list.len().try_into().unwrap(),
                ),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: func_call.loc.line,
                    column: func_call.loc.column,
                    length: func_call.span.end,
                }),
            });
            exit(1);
        }
    }

    pub(crate) fn build_func_call(
        &self,
        scope: ScopeRef<'ctx>,
        func_call: FuncCall,
    ) -> (CallSiteValue<'ctx>, InternalType<'ctx>) {
        // FIXME
        todo!();

        // let func_name = func_call.identifier.name.clone();

        // if let Some(func_metadata) = self.func_table.get(&func_name.clone()) {
        //     let arguments = &self.build_arguments(
        //         Rc::clone(&scope),
        //         func_call.arguments.clone(),
        //         Some(func_metadata.func_decl.params.clone()),
        //     );

        //     self.check_func_args_count_mismatch(
        //         func_metadata.func_decl.name.clone(),
        //         func_metadata.func_decl.clone(),
        //         func_call.clone(),
        //     );

        //     (
        //         self.builder.build_call(func_metadata.ptr, arguments, "call").unwrap(),
        //         func_metadata.return_type.clone(),
        //     )
        // } else {
        //     display_single_diag(Diag {
        //         level: DiagLevel::Error,
        //         kind: DiagKind::FuncNotFound(func_name),
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
