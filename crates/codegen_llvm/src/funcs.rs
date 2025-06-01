use crate::diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag};
use crate::scope::{Scope, ScopeRecord, ScopeRef};
use crate::values::InternalValue;
use crate::{CodeGenLLVM, InternalType};
use ast::ast::{
    Expression, FuncCall, FuncDecl, FuncDef, FuncParam, FuncParams, ModuleSegment, StorageClass, TypeSpecifier,
};
use ast::format::module_segments_as_string;
use ast::token::{Location, Span, Token, TokenKind};
use inkwell::builder::BuilderError;
use inkwell::llvm_sys::core::LLVMFunctionType;
use inkwell::llvm_sys::prelude::LLVMTypeRef;
use inkwell::module::Linkage;
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
    fn build_func_linkage(&self, storage_class: StorageClass) -> Linkage {
        match storage_class {
            StorageClass::Extern => Linkage::External,
            StorageClass::Public => Linkage::External,
            StorageClass::Internal => Linkage::Private,
            StorageClass::Inline => Linkage::Internal,
            StorageClass::PublicInline => Linkage::LinkOnceODR,
            StorageClass::PublicExtern => Linkage::Appending,
        }
    }

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
                if let Some(type_specifier) = &param.ty {
                    if let TypeSpecifier::TypeToken(type_token) = type_specifier.clone() {
                        if type_token.kind == TokenKind::Void {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom("Cannot declare a func param with 'void' type.".to_string()),
                                location: None,
                            });
                            exit(1);
                        }
                    }

                    self.build_type(type_specifier.clone(), func_loc.clone(), span_end)
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

        let func_linkage = self.build_func_linkage(func_decl.storage_class.clone());
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

        let func_linkage = self.build_func_linkage(func_def.storage_class.clone());
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
                            self.implicit_cast(
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
        let expr = self.build_expr(Rc::clone(&scope), *func_call.operand.clone());

        let func_metadata = {
            match expr {
                InternalValue::ModuleValue(module_metadata) => {
                    if let Expression::ModuleImport(module_import) = *func_call.operand.clone() {
                        let module_import_str = module_segments_as_string(module_import.segments.clone());
                        let ModuleSegment::SubModule(func_name) = module_import.segments.last().unwrap();

                        match module_metadata.func_table.get(&func_name.name) {
                            Some(func_metadata) => {
                                if !(func_metadata.func_decl.storage_class == StorageClass::Public
                                    || func_metadata.func_decl.storage_class == StorageClass::PublicExtern
                                || func_metadata.func_decl.storage_class == StorageClass::PublicInline)
                                {
                                    display_single_diag(Diag {
                                        level: DiagLevel::Error,
                                        kind: DiagKind::Custom(format!(
                                            "Function '{}' defined locally and cannot be called here. Consider to make it public and try again.",
                                            module_import_str
                                        )),
                                        location: Some(DiagLoc {
                                            file: self.file_path.clone(),
                                            line: func_call.loc.line,
                                            column: func_call.loc.column,
                                            length: func_call.span.end,
                                        }),
                                    });
                                    exit(1);
                                }
                                func_metadata.clone()
                            }
                            None => {
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::Custom(format!(
                                        "Function '{}' not found in module '{}'.",
                                        func_name.name, module_import_str
                                    )),
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
                    } else {
                        unreachable!();
                    }
                }
                InternalValue::FunctionValue(func_metadata) => func_metadata,
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Function call with invalid value as expr is not allowed.".to_string()),
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
        };

        let arguments = &self.build_arguments(
            Rc::clone(&scope),
            func_call.arguments.clone(),
            Some(func_metadata.func_decl.params.clone()),
        );

        self.check_func_args_count_mismatch(
            func_metadata.func_decl.name.clone(),
            func_metadata.func_decl.clone(),
            func_call.clone(),
        );

        (
            self.builder.build_call(func_metadata.ptr, arguments, "call").unwrap(),
            func_metadata.return_type.clone(),
        )
    }
}
