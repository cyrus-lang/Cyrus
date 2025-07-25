use crate::context::CodeGenLLVM;
use crate::diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag};
use crate::modules::{LocalIRValue, LocalIRValueID, ModuleID, generate_local_ir_value_id};
use crate::scope::{ScopeRecord, ScopeRef};
use crate::types::{InternalIntType, InternalType, InternalVoidType};
use crate::values::InternalValue;
use ast::ast::{
    AccessSpecifier, Expression, FuncCall, FuncDecl, FuncDef, FuncParamKind, FuncParams, FuncVariadicParams,
    Identifier, ModuleImport, Return, TypeSpecifier,
};
use ast::format::module_segments_as_string;
use ast::token::{Location, Span, Token, TokenKind};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::llvm_sys::core::LLVMFunctionType;
use inkwell::llvm_sys::prelude::LLVMTypeRef;
use inkwell::module::Linkage;
use inkwell::types::FunctionType;
use inkwell::values::{BasicMetadataValueEnum, FunctionValue};
use std::collections::HashMap;
use std::ops::DerefMut;
use std::process::exit;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct FuncMetadata<'a> {
    pub(crate) local_ir_value_id: LocalIRValueID,
    pub(crate) func_decl: FuncDecl,
    pub(crate) return_type: InternalType<'a>,
    pub(crate) imported_from: Option<ModuleID>,
    pub(crate) params_metadata: FuncParamsMetadata<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParamsMetadata<'a> {
    pub param_types: Vec<InternalType<'a>>,
    pub variadic_arguments: Option<(FuncVariadicParams, InternalType<'a>)>,
}

pub type FuncTable<'a> = HashMap<String, FuncMetadata<'a>>;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn add_function_attributes(&self, func: FunctionValue) {
        let attr_kind_id = |kind: &str| Attribute::get_named_enum_kind_id(kind);

        func.add_attribute(
            AttributeLoc::Function,
            self.context.create_enum_attribute(attr_kind_id("uwtable"), 2),
        );
        func.add_attribute(
            AttributeLoc::Function,
            self.context.create_enum_attribute(attr_kind_id("ssp"), 0),
        );
        func.add_attribute(
            AttributeLoc::Function,
            self.context.create_enum_attribute(attr_kind_id("nounwind"), 0),
        );

        func.add_attribute(
            AttributeLoc::Function,
            self.context.create_string_attribute("frame-pointer", "all"),
        );
        func.add_attribute(
            AttributeLoc::Function,
            self.context.create_string_attribute("no-trapping-math", "true"),
        );
        func.add_attribute(
            AttributeLoc::Function,
            self.context.create_string_attribute("stack-protector-buffer-size", "8"),
        );
    }

    pub(crate) fn build_func_linkage(&self, access_specifier: AccessSpecifier) -> Linkage {
        match access_specifier {
            AccessSpecifier::Extern => Linkage::External,
            AccessSpecifier::Public => Linkage::External,
            AccessSpecifier::Internal => Linkage::Private,
            AccessSpecifier::Inline => Linkage::Internal,
            AccessSpecifier::PublicInline => Linkage::LinkOnceODR,
            AccessSpecifier::PublicExtern => Linkage::External,
        }
    }

    pub(crate) fn build_func_params(
        &self,
        func_name: String,
        func_loc: Location,
        span_end: usize,
        params: Vec<FuncParamKind>,
        variadic: Option<FuncVariadicParams>,
        is_instance_method: bool,
    ) -> FuncParamsMetadata<'ctx> {
        let mut param_types: Vec<InternalType<'ctx>> = Vec::new();

        for param_kind in params {
            let param = match param_kind {
                FuncParamKind::FuncParam(func_param) => func_param,
                FuncParamKind::SelfModifier(_) => {
                    if !is_instance_method {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom("Functions cannot have a self modifier.".to_string()),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: func_loc.line,
                                column: func_loc.column,
                                length: span_end,
                            }),
                        });
                        exit(1);
                    }
                    continue;
                }
            };

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

                param_types.push(self.build_type(type_specifier.clone(), func_loc.clone(), span_end));
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::TypeAnnotationRequiredForParam(param.identifier.name.clone(), func_name.clone()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: func_loc.line,
                        column: func_loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        }

        let mut variadic_arguments: Option<(FuncVariadicParams, InternalType<'ctx>)> = None;

        if let Some(func_variadic_params) = variadic {
            match func_variadic_params.clone() {
                FuncVariadicParams::Typed(.., type_specifier) => {
                    // add a hidden va_args count parameter
                    let vargs_count_type = self.context.i32_type();
                    param_types.push(InternalType::IntType(InternalIntType {
                        type_str: "int32".to_string(),
                        int_kind: TokenKind::Int32,
                        int_type: vargs_count_type,
                    }));

                    variadic_arguments = Some((
                        func_variadic_params.clone(),
                        self.build_type(type_specifier, func_loc.clone(), span_end),
                    ));
                }
                FuncVariadicParams::UntypedCStyle => {
                    variadic_arguments = Some((
                        func_variadic_params.clone(),
                        InternalType::VoidType(InternalVoidType {
                            type_str: "void".to_string(),
                            void_type: self.context.void_type(),
                        }),
                    ));
                }
            }
        }

        FuncParamsMetadata {
            param_types,
            variadic_arguments,
        }
    }

    pub(crate) fn error_if_already_declared(&self, name: String, loc: Location, span_end: usize) {
        if self.resolve_metadata(self.module_id, name.clone()).is_some() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::DuplicateNaming(name),
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

    pub(crate) fn build_func_decl(
        &self,
        func_decl: FuncDecl,
        params_metadata: FuncParamsMetadata<'ctx>,
        return_type: InternalType<'ctx>,
        insert_to_func_table: bool,
    ) -> FunctionValue<'ctx> {
        self.error_if_already_declared(func_decl.get_usable_name(), func_decl.loc.clone(), func_decl.span.end);

        let param_types = params_metadata.param_types.clone();
        let is_var_args = params_metadata.variadic_arguments.is_some();

        let func_type = unsafe {
            FunctionType::new(LLVMFunctionType(
                return_type.as_type_ref(),
                param_types
                    .iter()
                    .map(|p| p.as_type_ref())
                    .collect::<Vec<LLVMTypeRef>>()
                    .as_mut_ptr(),
                param_types.len() as u32,
                is_var_args as i32,
            ))
        };

        if matches!(
            func_decl.access_specifier,
            AccessSpecifier::Inline | AccessSpecifier::PublicInline
        ) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Cannot declare function '{}' with 'inline' access specifier..",
                    func_decl.get_usable_name()
                )),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: func_decl.loc.line,
                    column: func_decl.loc.column,
                    length: func_decl.span.end,
                }),
            });
            exit(1);
        }

        let func_linkage = self.build_func_linkage(func_decl.access_specifier.clone());
        let func_value =
            self.module
                .borrow_mut()
                .deref_mut()
                .add_function(&func_decl.name, func_type, Some(func_linkage));

        self.add_function_attributes(func_value);

        if insert_to_func_table {
            let func_metadata = FuncMetadata {
                local_ir_value_id: generate_local_ir_value_id(),
                func_decl: func_decl.clone(),
                return_type,
                imported_from: None,
                params_metadata: params_metadata.clone(),
            };

            self.insert_local_ir_value(func_metadata.local_ir_value_id, LocalIRValue::Func(func_value));

            let mut module_metadata = self.get_module_metadata_by_module_id(self.module_id).unwrap();
            module_metadata.insert_func(func_decl.get_usable_name(), func_metadata);
            drop(module_metadata);
        }

        func_value
    }

    pub(crate) fn build_func_define_local_params(
        &mut self,
        scope: ScopeRef<'ctx>,
        func_value: FunctionValue<'ctx>,
        func_def: FuncDef,
        includes_self_modifier: bool,
    ) {
        let mut scope_borrowed = scope.borrow_mut();
        let func_params_iterator = func_value
            .get_param_iter()
            .skip(if includes_self_modifier { 1 } else { 0 });

        for (idx, param) in func_params_iterator.enumerate() {
            let param_ptr = self.builder.build_alloca(param.get_type(), "param").unwrap();
            self.builder.build_store(param_ptr, param).unwrap();

            match func_def.params.list.get(idx) {
                Some(func_param_kind) => {
                    let func_param = match func_param_kind {
                        FuncParamKind::FuncParam(func_param) => func_param,
                        FuncParamKind::SelfModifier(_) => {
                            panic!("An unexpected self modifier found in the middle of the func params.")
                        }
                    };

                    if let Some(param_type_specifier) = func_param.ty.clone() {
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
                }
                None => {
                    panic!("Unexpectedly couldn't find parameter when building local params.")
                }
            };
        }
    }

    pub(crate) fn transform_to_func_decl(&self, func_def: FuncDef) -> FuncDecl {
        FuncDecl {
            name: func_def.name.clone(),
            params: func_def.params.clone(),
            return_type: func_def.return_type.clone(),
            access_specifier: func_def.access_specifier.clone(),
            renamed_as: Some(func_def.name.clone()),
            span: func_def.span.clone(),
            loc: func_def.loc.clone(),
        }
    }

    fn validate_func_storage_class(&self, func_def: FuncDef, is_entry_point: bool) {
        if is_entry_point && !matches!(func_def.access_specifier, AccessSpecifier::Internal) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(
                    "Module entry point cannot be declared with non-internal access_specifier.".to_string(),
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

        if func_def.access_specifier == AccessSpecifier::Extern {
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

    #[allow(unused)]
    pub(crate) fn build_func_vargs(
        &mut self,
        scope: ScopeRef<'ctx>,
        identifier: Identifier,
        type_specifier: TypeSpecifier,
        func_params: FuncParams,
        func_value: FunctionValue<'ctx>,
        loc: Location,
        span_end: usize,
    ) {
        // TODO
        // The feature will be implemented after making Slices.

        // let ptr_type = self.context.ptr_type(AddressSpace::default());
        // let element_type = self.build_type(type_specifier, loc.clone(), span_end);
        // let element_basic_type = element_type.to_basic_type(ptr_type).unwrap(); // FIXME

        // let va_start_func = self.intrinsic_va_start_function();
        // let va_end_func = self.intrinsic_va_end_function();

        // let va_list = self
        //     .builder
        //     .build_alloca(element_basic_type.clone(), "va_list")
        //     .unwrap();
        // self.builder
        //     .build_call(va_start_func, &[BasicMetadataValueEnum::PointerValue(va_list)], "call")
        //     .unwrap();

        // let static_params_length = func_params.list.len();
        // let va_args_count = func_value
        //     .get_nth_param(static_params_length.try_into().unwrap())
        //     .unwrap()
        //     .into_int_value();

        // let vargs_array_ptr = self
        //     .builder
        //     .build_array_alloca(element_basic_type, va_args_count, "vargs_array_ptr")
        //     .unwrap();

        // let i32_type = self.context.i32_type();
        // let index_ptr = self.builder.build_alloca(i32_type, "vargs.idx").unwrap();
        // self.builder
        //     .build_store(index_ptr, self.build_integer_literal(0))
        //     .unwrap();

        // build_loop_statement!(
        //     self,
        //     scope,
        //     {
        //         let index_value = self.builder.build_load(i32_type, index_ptr, "vargs.idx").unwrap();

        //         self.builder
        //             .build_int_compare(IntPredicate::SLE, index_value.into_int_value(), va_args_count, "icmp")
        //             .unwrap()
        //     },
        //     {
        //         let index_value = self.builder.build_load(i32_type, index_ptr, "vargs.idx").unwrap();
        //         let va_arg = self
        //             .builder
        //             .build_va_arg(va_list, element_basic_type, "va_arg")
        //             .unwrap();
        //         let gep = unsafe {
        //             self.builder
        //                 .build_in_bounds_gep(
        //                     i32_type,
        //                     vargs_array_ptr,
        //                     &[
        //                         // self.build_integer_literal(0)
        //                         index_value.into_int_value(),
        //                     ],
        //                     "vargs_array.gep",
        //                 )
        //                 .unwrap()
        //         };
        //         self.builder.build_store(gep, va_arg).unwrap();
        //     },
        //     {
        //         let index_value = self.builder.build_load(i32_type, index_ptr, "vargs.idx").unwrap();
        //         let incremented_index = self
        //             .builder
        //             .build_int_add(
        //                 index_value.into_int_value(),
        //                 self.build_integer_literal(1),
        //                 "vargs.idx.increment",
        //             )
        //             .unwrap();
        //         self.builder.build_store(index_ptr, incremented_index).unwrap();
        //     },
        //     loc,
        //     span_end
        // );

        // scope.borrow_mut().insert(
        //     identifier.name.clone(),
        //     ScopeRecord {
        //         ptr: vargs_array_ptr,
        //         ty: InternalType::ArrayPtrType(InternalArrayPtrType {
        //             type_str: format!("{}[]", element_type.to_string()),
        //             inner_type: Box::new(element_type),
        //             ptr_type: vargs_array_ptr.get_type(),
        //         }),
        //     },
        // );

        // self.builder
        //     .build_call(va_end_func, &[BasicMetadataValueEnum::PointerValue(va_list)], "call")
        //     .unwrap();
    }

    pub(crate) fn build_func_def(
        &mut self,
        scope: ScopeRef<'ctx>,
        func_def: FuncDef,
        params_metadata: FuncParamsMetadata<'ctx>,
        is_entry_point: bool,
    ) -> FunctionValue<'ctx> {
        self.error_if_already_declared(func_def.name.clone(), func_def.loc.clone(), func_def.span.end);

        let param_types = params_metadata.param_types.clone();
        self.validate_func_storage_class(func_def.clone(), is_entry_point);
        let mut func_decl = self.transform_to_func_decl(func_def.clone());
        let is_variadic = func_def.params.variadic.is_some();

        let return_type = self.build_type(
            func_def.return_type.clone().unwrap_or(TypeSpecifier::TypeToken(Token {
                kind: TokenKind::Void,
                span: Span::default(),
                loc: Location::default(),
            })),
            func_def.loc.clone(),
            func_def.span.end,
        );

        let func_type = unsafe {
            FunctionType::new(LLVMFunctionType(
                return_type.as_type_ref(),
                param_types
                    .iter()
                    .map(|p| p.as_type_ref())
                    .collect::<Vec<LLVMTypeRef>>()
                    .as_mut_ptr(),
                param_types.len() as u32,
                is_variadic as i32,
            ))
        };

        let func_abi_name = self.generate_func_abi_name(self.module_name.clone(), func_def.name.clone());
        let actual_func_name = if self.is_current_module_entry_point() {
            func_decl.name.clone()
        } else {
            func_abi_name
        };

        func_decl.renamed_as = Some(func_decl.name.clone());
        func_decl.name = actual_func_name.clone();

        let func_linkage: Option<Linkage> = if !is_entry_point {
            Some(self.build_func_linkage(func_def.access_specifier.clone()))
        } else {
            None
        };

        let func_value = self
            .module
            .borrow_mut()
            .deref_mut()
            .add_function(&actual_func_name, func_type, func_linkage);

        self.add_function_attributes(func_value);

        let local_ir_value_id = generate_local_ir_value_id();

        let func_metadata = FuncMetadata {
            local_ir_value_id,
            func_decl: func_decl.clone(),
            return_type: return_type.clone(),
            imported_from: None,
            params_metadata: params_metadata.clone(),
        };

        self.insert_local_ir_value(func_metadata.local_ir_value_id, LocalIRValue::Func(func_value));

        let mut module_metadata = self.get_module_metadata_by_module_id(self.module_id).unwrap();
        module_metadata.insert_func(func_decl.get_usable_name(), func_metadata);
        drop(module_metadata);

        self.block_registry.current_func_ref = Some(FuncMetadata {
            local_ir_value_id,
            func_decl: func_decl.clone(),
            return_type: return_type.clone(),
            imported_from: None,
            params_metadata: params_metadata.clone(),
        });

        let entry_block = self.context.append_basic_block(func_value, "entry");
        self.builder.position_at_end(entry_block);
        self.block_registry.current_block_ref = Some(entry_block);

        self.build_func_define_local_params(Rc::clone(&scope), func_value, func_def.clone(), false);
        if let Some(variadic_type) = func_def.params.variadic.clone() {
            match variadic_type {
                FuncVariadicParams::Typed(identifier, type_specifier) => {
                    self.build_func_vargs(
                        Rc::clone(&scope),
                        identifier,
                        type_specifier,
                        func_def.params.clone(),
                        func_value.clone(),
                        func_def.loc.clone(),
                        func_def.span.end,
                    );
                }
                FuncVariadicParams::UntypedCStyle => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "C-style variadic arguments not supported in function definition. Consider to add a type annotation for the variadic parameter in function '{}'.",
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

        self.build_statements(Rc::clone(&scope), func_def.body.exprs);

        let current_block = self.get_current_block("func_def statement", func_def.loc.clone(), func_def.span.end);

        if !self.is_block_terminated(current_block) && return_type.is_void_type() {
            self.builder.build_return(None).unwrap();
        } else if !self.is_block_terminated(current_block) {
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
        let current_func_metadata =
            self.get_current_func("return statement", statement.loc.clone(), statement.span.end);

        let return_type = {
            if self.is_block_terminated(current_block) {
                return;
            }

            if current_func_metadata.return_type.is_void_type() {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "The function '{}' with void return type is not allowed to have a return statement.",
                        &current_func_metadata.func_decl.get_usable_name()
                    )),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: statement.loc.line,
                        column: statement.loc.column,
                        length: statement.span.end,
                    }),
                });
                exit(1);
            } else if !current_func_metadata.return_type.is_void_type() && statement.argument.is_none() {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Function '{}' must return a value of type '{}'.",
                        &current_func_metadata.func_decl.get_usable_name(),
                        current_func_metadata.return_type
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

            current_func_metadata.return_type.clone()
        };

        match statement.argument {
            Some(argument) => {
                let argument_expr = self.build_expr(Rc::clone(&scope), argument);
                let argument_basic_value = self.implicit_cast(
                    self.internal_value_as_rvalue(argument_expr, statement.loc.clone(), statement.span.end),
                    return_type.clone(),
                    statement.loc.clone(),
                    statement.span.end,
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
        &mut self,
        scope: ScopeRef<'ctx>,
        arguments: Vec<Expression>,
        params_metadata: FuncParamsMetadata<'ctx>,
        starting_point: usize,
        static_params_length: usize,
        func_name: String,
        loc: Location,
        span_end: usize,
    ) -> Vec<BasicMetadataValueEnum<'ctx>> {
        let mut final_arguments: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();

        for (idx, arg) in arguments[starting_point..static_params_length].iter().enumerate() {
            let lvalue = self.build_expr(Rc::clone(&scope), arg.clone());
            let rvalue = self.internal_value_as_rvalue(lvalue, loc.clone(), span_end);

            let param_internal_type = match params_metadata.param_types.get(idx) {
                Some(internal_type) => internal_type.clone(),
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Couldn't get param metadata from func metadata.".to_string()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
            };

            if !self.compatible_types(param_internal_type.clone(), rvalue.get_type()) {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Argument at index {} for function '{}' is not compatible with type '{}' for implicit casting.",
                        starting_point + idx,
                        func_name,
                        param_internal_type
                    )),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }

            final_arguments.push(
                self.implicit_cast(rvalue, param_internal_type, loc.clone(), span_end)
                    .into(),
            );
        }

        let vargs_count = arguments.len() - static_params_length + starting_point;

        if let Some((func_variadic_params, variadic_element_type)) = params_metadata.variadic_arguments {
            match func_variadic_params {
                FuncVariadicParams::Typed(..) => {
                    // add vargs_count before building the variadic arguments
                    final_arguments.push(BasicMetadataValueEnum::IntValue(
                        self.context
                            .i32_type()
                            .const_int(vargs_count.try_into().unwrap(), false),
                    ));

                    for (idx, arg) in arguments[static_params_length..].iter().enumerate() {
                        let lvalue = self.build_expr(Rc::clone(&scope), arg.clone());
                        let rvalue = self.internal_value_as_rvalue(lvalue, loc.clone(), span_end);

                        let argument_type = rvalue.get_type();
                        if !self.compatible_types(argument_type, variadic_element_type.clone()) {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom(format!(
                                    "Argument at index {} for function '{}' is not compatible with type '{}' for implicit casting.",
                                    static_params_length + idx,
                                    func_name,
                                    variadic_element_type
                                )),
                                location: Some(DiagLoc {
                                    file: self.file_path.clone(),
                                    line: loc.line,
                                    column: loc.column,
                                    length: span_end,
                                }),
                            });
                            exit(1);
                        }

                        final_arguments.push(
                            self.implicit_cast(rvalue, variadic_element_type.clone(), loc.clone(), span_end)
                                .into(),
                        );
                    }
                }
                FuncVariadicParams::UntypedCStyle => {
                    for arg in arguments[static_params_length..].iter().as_slice() {
                        let lvalue = self.build_expr(Rc::clone(&scope), arg.clone());
                        let rvalue = self.internal_value_as_rvalue(lvalue, loc.clone(), span_end);
                        final_arguments.push(self.internal_value_to_basic_metadata(rvalue));
                    }
                }
            }
        }

        final_arguments
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

    fn build_func_call_operand(&self, operand: Box<Expression>, loc: Location, span_end: usize) -> FuncMetadata<'ctx> {
        let resolve_local_func = |name: String| self.resolve_func_metadata(self.module_id, name);
        let resolve_imported_func = |mut module_import: ModuleImport| {
            debug_assert!(module_import.segments.len() > 0);

            if let Some(identifier) = module_import.as_identifier() {
                return resolve_local_func(identifier.name);
            }

            let func_name = module_import.segments.pop().unwrap().as_identifier().name;

            let imported_module_id = match self.get_imported_module(module_import.segments.clone()) {
                Some(imported_module) => imported_module.module_id,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::ModuleImportNotFound(module_segments_as_string(module_import.segments)),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
            };

            match self.resolve_func_metadata(imported_module_id, func_name) {
                Some(func_metadata) => Some(func_metadata),
                None => None,
            }
        };

        let (func_metadata_opt, resolving_name) = match *operand {
            Expression::Identifier(identifier) => (resolve_local_func(identifier.name.clone()), identifier.name),
            Expression::ModuleImport(module_import) => (
                resolve_imported_func(module_import.clone()),
                module_segments_as_string(module_import.segments),
            ),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::FuncCallInvalidOperand,
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        };

        let func_metadata = match func_metadata_opt {
            Some(func_metadata) => func_metadata,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::FuncNotFound(resolving_name),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        };

        func_metadata
    }

    pub(crate) fn build_func_call(&mut self, scope: ScopeRef<'ctx>, func_call: FuncCall) -> InternalValue<'ctx> {
        let func_metadata = self.build_func_call_operand(func_call.operand, func_call.loc.clone(), func_call.span.end);

        self.check_func_args_count_mismatch(
            func_metadata.func_decl.name.clone(),
            func_metadata.func_decl.clone(),
            func_call.arguments.len(),
            func_call.loc.clone(),
            func_call.span.end,
        );

        let static_params_length = func_metadata.params_metadata.param_types.len().clone();

        let arguments = &self.build_arguments(
            Rc::clone(&scope),
            func_call.arguments.clone(),
            func_metadata.params_metadata.clone(),
            0,
            static_params_length,
            func_metadata.func_decl.get_usable_name(),
            func_call.loc.clone(),
            func_call.span.end,
        );

        let func_value =
            self.get_or_declare_local_func_ir_value(func_metadata.local_ir_value_id, func_metadata.clone());
        let call_site_value = self.builder.build_call(func_value, arguments, "call").unwrap();
        let return_type = func_metadata.return_type.clone();

        if let Some(value) = call_site_value.try_as_basic_value().left() {
            self.new_internal_value(value, return_type)
        } else {
            InternalValue::PointerValue(self.build_null())
        }
    }
}
