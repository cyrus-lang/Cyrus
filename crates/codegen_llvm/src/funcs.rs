use crate::diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag};
use crate::scope::{Scope, ScopeRecord, ScopeRef};
use crate::values::InternalValue;
use crate::{CodeGenLLVM, InternalType};
use ast::ast::{
    Expression, FuncCall, FuncDecl, FuncDef, FuncParam, FuncParams, ModuleSegment, Return, StorageClass, TypeSpecifier,
};
use ast::format::module_segments_as_string;
use ast::token::{Location, Span, Token, TokenKind};
use inkwell::llvm_sys::core::LLVMFunctionType;
use inkwell::llvm_sys::prelude::LLVMTypeRef;
use inkwell::module::Linkage;
use inkwell::types::FunctionType;
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, IntValue};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{DerefMut, Index};
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
        &self,
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
                                location: Some(DiagLoc {
                                    file: self.file_path.clone(),
                                    line: param.loc.line,
                                    column: param.loc.column,
                                    length: param.span.end,
                                }),
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

    pub(crate) fn build_func_decl(
        &mut self,
        func_decl: FuncDecl,
        mut func_param_types: Vec<LLVMTypeRef>,
    ) -> FunctionValue<'ctx> {
        let is_var_args = func_decl.params.variadic.is_some();

        let return_type = self.build_type(
            func_decl.return_type.clone().unwrap_or(TypeSpecifier::TypeToken(Token {
                kind: TokenKind::Void,
                span: Span::default(),
                loc: Location::default(),
            })),
            func_decl.loc.clone(),
            func_decl.span.end,
        );

        let fn_type = unsafe {
            FunctionType::new(LLVMFunctionType(
                return_type.as_type_ref(),
                func_param_types.as_mut_ptr(),
                func_param_types.len() as u32,
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

    fn build_func_def_local_params(
        &mut self,
        scope: ScopeRef<'ctx>,
        func_value: FunctionValue<'ctx>,
        func_def: FuncDef,
    ) {
        let mut scope_borrowed = scope.borrow_mut();
        for (idx, param) in func_value.get_param_iter().enumerate() {
            let param_ptr = self.builder.build_alloca(param.get_type(), "param").unwrap();
            self.builder.build_store(param_ptr, param).unwrap();

            if let Some(func_param) = func_def.params.list.get(idx) {
                if let Some(param_type_specifier) = func_def.params.list.index(idx).ty.clone() {
                    let param_internal_type =
                        self.build_type(param_type_specifier, func_param.loc.clone(), func_param.span.end);

                    scope_borrowed.insert(
                        func_param.identifier.name.clone(),
                        ScopeRecord {
                            ptr: param_ptr,
                            ty: param_internal_type,
                        },
                    );
                } else {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Consider to add an type annotation for parameter '{}'.",
                            func_param.identifier.name.clone()
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
    }

    fn transform_to_func_decl(&self, func_def: FuncDef) -> FuncDecl {
        FuncDecl {
            name: func_def.name.clone(),
            params: func_def.params.clone(),
            return_type: func_def.return_type.clone(),
            storage_class: func_def.storage_class.clone(),
            renamed_as: Some(func_def.name.clone()),
            span: func_def.span.clone(),
            loc: func_def.loc.clone(),
        }
    }

    fn validate_func_storage_class(&self, func_def: FuncDef, is_entry_point: bool) {
        if is_entry_point && !matches!(func_def.storage_class, StorageClass::Internal) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(
                    "Module entry point cannot be declared with non-internal storage_class.".to_string(),
                ),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: func_def.loc.line,
                    column: func_def.loc.column,
                    length: func_def.span.end,
                }),
            });
            exit(1);
        }

        if func_def.storage_class == StorageClass::Extern {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(
                    "Extern storage class specifier is not permitted in function definitions.".to_string(),
                ),
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

    pub(crate) fn build_func_def(
        &mut self,
        func_def: FuncDef,
        mut func_param_types: Vec<LLVMTypeRef>,
        is_entry_point: bool,
    ) -> FunctionValue<'ctx> {
        self.validate_func_storage_class(func_def.clone(), is_entry_point);
        let scope: ScopeRef<'ctx> = Rc::new(RefCell::new(Scope::new()));
        let func_decl = self.transform_to_func_decl(func_def.clone());
        let is_var_args = func_def.params.variadic.is_some();

        let return_type = self.build_type(
            func_def.return_type.clone().unwrap_or(TypeSpecifier::TypeToken(Token {
                kind: TokenKind::Void,
                span: Span::default(),
                loc: Location::default(),
            })),
            func_def.loc.clone(),
            func_def.span.end,
        );

        let fn_type = unsafe {
            FunctionType::new(LLVMFunctionType(
                return_type.as_type_ref(),
                func_param_types.as_mut_ptr(),
                func_param_types.len() as u32,
                is_var_args as i32,
            ))
        };

        let actual_func_name = if !is_entry_point {
            format!("{}.{}", self.module_name, func_def.name.clone())
        } else {
            func_decl.name.clone()
        };

        let func_linkage: Option<Linkage> = if !is_entry_point {
            Some(self.build_func_linkage(func_def.storage_class.clone()))
        } else {
            None
        };

        let func_value = self
            .module
            .borrow_mut()
            .deref_mut()
            .add_function(&actual_func_name, fn_type, func_linkage);

        self.func_table.insert(
            if let Some(renamed_as) = func_decl.renamed_as.clone() {
                renamed_as
            } else {
                func_decl.name.clone()
            },
            FuncMetadata {
                func_decl,
                ptr: func_value,
                return_type: return_type.clone(),
            },
        );

        self.current_func_ref = Some(func_value);

        let entry_block = self.context.append_basic_block(func_value, "entry");
        self.builder.position_at_end(entry_block);
        self.current_block_ref = Some(entry_block);

        self.build_func_def_local_params(Rc::clone(&scope), func_value, func_def.clone());
        self.build_statements(Rc::clone(&scope), func_def.body.exprs);

        let current_block = self.get_current_block("func_def statement", func_def.loc.clone(), func_def.span.end);

        if !self.is_block_terminated(current_block) && return_type.is_void_type() {
            self.builder.build_return(None).unwrap();
        } 
        else if !self.is_block_terminated(current_block) {
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

        func_value.verify(true);
        return func_value;
    }

    pub(crate) fn build_return(&mut self, scope: ScopeRef<'ctx>, statement: Return) {
        let current_block = self.get_current_block("return statement", statement.loc.clone(), statement.span.end);
        let current_func = self.get_current_func("return statement", statement.loc.clone(), statement.span.end);

        if self.is_block_terminated(current_block) {
            return;
        }

        let func_metadata = self.func_table.values().find(|f| f.ptr == current_func).unwrap();

        if func_metadata.return_type.is_void_type() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "The function '{}' with void return type is not allowed to have a return statement.",
                    &func_metadata
                        .func_decl
                        .renamed_as
                        .clone()
                        .unwrap_or(func_metadata.func_decl.name.clone())
                )),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: statement.loc.line,
                    column: statement.loc.column,
                    length: statement.span.end,
                }),
            });
            exit(1);
        } else if !func_metadata.return_type.is_void_type() && statement.argument.is_none() {
            // FIXME We need accurate type name tracking here
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Function '{}' must return a value of type '{:?}'.",
                    &func_metadata
                        .func_decl
                        .renamed_as
                        .clone()
                        .unwrap_or(func_metadata.func_decl.name.clone()),
                    func_metadata.return_type
                )),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: statement.loc.line,
                    column: statement.loc.column,
                    length: statement.span.end,
                }),
            });
            exit(1);
        }

        match statement.argument {
            Some(argument) => {
                let argument_basic_value = self.implicit_cast(
                    self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), argument)),
                    func_metadata.return_type.clone(),
                );

                self.builder.build_return(Some(&argument_basic_value)).unwrap();
            }
            None => {
                self.builder.build_return(None).unwrap();
            }
        }

        self.mark_block_terminated(current_block, true);
    }

    pub(crate) fn build_arguments(
        &self,
        scope: ScopeRef<'ctx>,
        arguments: Vec<Expression>,
        params: Option<FuncParams>,
        func_name: String,
        loc: Location,
        span_end: usize,
    ) -> Vec<BasicMetadataValueEnum<'ctx>> {
        arguments
            .iter()
            .enumerate()
            .map(|(idx, arg)| {
                let rvalue = self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), arg.clone()));

                if let Some(params) = &params {
                    // checked before through check_func_args_count_mismatch
                    if let Some(param) = params.list.get(idx) {
                        if let Some(target_type_specifier) = &param.ty {
                            let target_type =
                                self.build_type(target_type_specifier.clone(), param.loc.clone(), param.span.end);

                            if !self.compatible_types(target_type.clone(), rvalue.get_type(self.string_type.clone())) {
                                // FIXME We need accurate type name tracking here
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::Custom(
                                        format!("Argument at index {} for function '{}' is not compatible with type '{:?}' for implicit casting.",
                                            idx,
                                            func_name,
                                            target_type
                                        )
                                    ),
                                    location: Some(DiagLoc {
                                        file: self.file_path.clone(),
                                        line: loc.line,
                                        column: loc.column,
                                        length: span_end,
                                    }),
                                });
                                exit(1);
                            }

                            self.implicit_cast(rvalue, target_type).into()
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

    pub(crate) fn check_func_args_count_mismatch(
        &self,
        func_name: String,
        func_decl: FuncDecl,
        arguments_length: usize,
        loc: Location,
        span_end: usize,
    ) {
        if func_decl.params.variadic.is_none() && func_decl.params.list.len() != arguments_length {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::FuncCallArgumentCountMismatch(
                    func_name.clone(),
                    arguments_length.try_into().unwrap(),
                    func_decl.params.list.len().try_into().unwrap(),
                ),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: loc.line,
                    column: loc.column,
                    length: span_end,
                }),
            });
            exit(1);
        } else if func_decl.params.variadic.is_some() && arguments_length < func_decl.params.list.len() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::FuncCallArgumentCountMismatch(
                    func_name.clone(),
                    arguments_length.try_into().unwrap(),
                    func_decl.params.list.len().try_into().unwrap(),
                ),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: loc.line,
                    column: loc.column,
                    length: span_end,
                }),
            });
            exit(1);
        }
    }

    pub(crate) fn check_method_args_count_mismatch(
        &self,
        func_name: String,
        func_decl: FuncDecl,
        arguments_length: usize,
        loc: Location,
        span_end: usize,
    ) {
        if func_decl.params.variadic.is_none() && func_decl.params.list.len() != arguments_length {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::FuncCallArgumentCountMismatch(
                    func_name.clone(),
                    arguments_length.try_into().unwrap(),
                    (func_decl.params.list.len() - 1).try_into().unwrap(),
                ),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: loc.line,
                    column: loc.column,
                    length: span_end,
                }),
            });
            exit(1);
        } else if func_decl.params.variadic.is_some() && arguments_length < func_decl.params.list.len() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::FuncCallArgumentCountMismatch(
                    func_name.clone(),
                    arguments_length.try_into().unwrap(),
                    (func_decl.params.list.len() - 1).try_into().unwrap(),
                ),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: loc.line,
                    column: loc.column,
                    length: span_end,
                }),
            });
            exit(1);
        }
    }

    pub(crate) fn build_func_call(&self, scope: ScopeRef<'ctx>, func_call: FuncCall) -> InternalValue<'ctx> {
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
                        kind: DiagKind::Custom("Cannot build function call with an invalid expression.".to_string()),
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
            func_metadata
                .func_decl
                .renamed_as
                .clone()
                .unwrap_or(func_metadata.func_decl.name.clone()),
            func_call.loc.clone(),
            func_call.span.end,
        );

        self.check_func_args_count_mismatch(
            func_metadata.func_decl.name.clone(),
            func_metadata.func_decl.clone(),
            func_call.arguments.len(),
            func_call.loc.clone(),
            func_call.span.end,
        );

        let call_site_value = self.builder.build_call(func_metadata.ptr, arguments, "call").unwrap();
        let return_type = func_metadata.return_type.clone();

        if let Some(value) = call_site_value.try_as_basic_value().left() {
            self.new_internal_value(value, return_type)
        } else {
            InternalValue::PointerValue(self.build_null())
        }
    }
}
