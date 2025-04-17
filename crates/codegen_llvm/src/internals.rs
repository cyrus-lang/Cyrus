use crate::{AnyType, CodeGenLLVM, diag::*, funcs::FuncMetadata, scope::ScopeRef};
use ast::{
    ast::{Expression, FuncCall, FuncDecl, FuncParam, FuncParams, Identifier, IntegerLiteral, StringLiteral, VisType},
    token::{Location, Span, Token, TokenKind},
};
use inkwell::{types::BasicType, values::{AsValueRef, BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, IntValue}};
use rust_embed::Embed;
use std::{env, fs::File, io::Write, process::exit, rc::Rc};
use utils::generate_random_hex::generate_random_hex;

#[derive(Embed)]
#[folder = "../../internals/"]
struct Asset;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_internals(&mut self) {
        if cfg!(target_os = "linux") {
            let asset_name = "internal_linux.o";
            match Asset::get(asset_name) {
                Some(embedded_file) => {
                    let temp_file_path = format!("{}/{}.o", env::temp_dir().to_str().unwrap(), generate_random_hex());
                    let mut temp_file = File::create(temp_file_path.clone()).unwrap();
                    temp_file.write(&embedded_file.data).unwrap();
                    self.internal_object_modules.push(temp_file_path);
                }
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Failed to get internal object file.".to_string()),
                        location: None,
                    });
                    exit(1);
                }
            }
        }
    }

    pub(crate) fn load_internal_funcs(&mut self) {
        let internal_funcs: Vec<(&str, FuncMetadata)> = vec![
            ("println", {
                let func_decl = FuncDecl {
                    name: "internal_println".to_string(),
                    params: FuncParams {
                        list: vec![FuncParam {
                            identifier: Identifier {
                                name: "v".to_string(),
                                span: Span::default(),
                                loc: Location::default(),
                            },
                            ty: Some(TokenKind::String),
                            default_value: None,
                            span: Span::default(),
                            loc: Location::default(),
                        }],
                        variadic: Some(TokenKind::Dereference(Box::new(TokenKind::Void))),
                    },
                    return_type: None,
                    vis_type: VisType::Inline,
                    renamed_as: None,
                    span: Span::default(),
                    loc: Location::default(),
                };
                let ptr = self.build_func_decl(func_decl.clone());
                FuncMetadata {
                    ptr,
                    func_decl,
                    is_internal: true,
                }
            }),
            ("printf", {
                let func_decl = FuncDecl {
                    name: "internal_printf".to_string(),
                    params: FuncParams {
                        list: vec![FuncParam {
                            identifier: Identifier {
                                name: "v".to_string(),
                                span: Span::default(),
                                loc: Location::default(),
                            },
                            ty: Some(TokenKind::String),
                            default_value: None,
                            span: Span::default(),
                            loc: Location::default(),
                        }],
                        variadic: Some(TokenKind::Dereference(Box::new(TokenKind::Void))),
                    },
                    return_type: None,
                    vis_type: VisType::Inline,
                    renamed_as: None,
                    span: Span::default(),
                    loc: Location::default(),
                };
                let ptr = self.build_func_decl(func_decl.clone());
                FuncMetadata {
                    ptr,
                    func_decl,
                    is_internal: true,
                }
            }),
            ("eprintln", {
                let func_decl = FuncDecl {
                    name: "internal_eprintln".to_string(),
                    params: FuncParams {
                        list: vec![FuncParam {
                            identifier: Identifier {
                                name: "v".to_string(),
                                span: Span::default(),
                                loc: Location::default(),
                            },
                            ty: Some(TokenKind::String),
                            default_value: None,
                            span: Span::default(),
                            loc: Location::default(),
                        }],
                        variadic: Some(TokenKind::Dereference(Box::new(TokenKind::Void))),
                    },
                    return_type: None,
                    vis_type: VisType::Inline,
                    renamed_as: None,
                    span: Span::default(),
                    loc: Location::default(),
                };
                let ptr = self.build_func_decl(func_decl.clone());
                FuncMetadata {
                    ptr,
                    func_decl,
                    is_internal: true,
                }
            }),
            ("eprintf", {
                let func_decl = FuncDecl {
                    name: "internal_eprintf".to_string(),
                    params: FuncParams {
                        list: vec![FuncParam {
                            identifier: Identifier {
                                name: "v".to_string(),
                                span: Span::default(),
                                loc: Location::default(),
                            },
                            ty: Some(TokenKind::String),
                            default_value: None,
                            span: Span::default(),
                            loc: Location::default(),
                        }],
                        variadic: Some(TokenKind::Dereference(Box::new(TokenKind::Void))),
                    },
                    return_type: None,
                    vis_type: VisType::Inline,
                    renamed_as: None,
                    span: Span::default(),
                    loc: Location::default(),
                };
                let ptr = self.build_func_decl(func_decl.clone());
                FuncMetadata {
                    ptr,
                    func_decl,
                    is_internal: true,
                }
            }),
            ("exit", {
                let func_decl = FuncDecl {
                    name: "internal_exit".to_string(),
                    params: FuncParams {
                        list: vec![FuncParam {
                            identifier: Identifier {
                                name: "status".to_string(),
                                span: Span::default(),
                                loc: Location::default(),
                            },
                            ty: Some(TokenKind::I32),
                            default_value: None,
                            span: Span::default(),
                            loc: Location::default(),
                        }],
                        variadic: Some(TokenKind::Dereference(Box::new(TokenKind::Void))),
                    },
                    return_type: None,
                    vis_type: VisType::Inline,
                    renamed_as: None,
                    span: Span::default(),
                    loc: Location::default(),
                };
                let ptr = self.build_func_decl(func_decl.clone());
                FuncMetadata {
                    ptr,
                    func_decl,
                    is_internal: true,
                }
            }),
            ("panic", {
                let func_decl = FuncDecl {
                    name: "internal_panic".to_string(),
                    params: FuncParams {
                        list: vec![
                            FuncParam {
                                identifier: Identifier {
                                    name: "file_name".to_string(),
                                    span: Span::default(),
                                    loc: Location::default(),
                                },
                                ty: Some(TokenKind::String),
                                default_value: None,
                                span: Span::default(),
                                loc: Location::default(),
                            },
                            FuncParam {
                                identifier: Identifier {
                                    name: "line".to_string(),
                                    span: Span::default(),
                                    loc: Location::default(),
                                },
                                ty: Some(TokenKind::I32),
                                default_value: None,
                                span: Span::default(),
                                loc: Location::default(),
                            },
                        ],
                        variadic: Some(TokenKind::Dereference(Box::new(TokenKind::Void))),
                    },
                    return_type: None,
                    vis_type: VisType::Inline,
                    renamed_as: None,
                    span: Span::default(),
                    loc: Location::default(),
                };
                let ptr = self.build_func_decl(func_decl.clone());
                FuncMetadata {
                    ptr,
                    func_decl,
                    is_internal: true,
                }
            }),
            ("internal_len_string", {
                let func_decl = FuncDecl {
                    name: "internal_len_string".to_string(),
                    params: FuncParams {
                        list: vec![FuncParam {
                            identifier: Identifier {
                                name: "input".to_string(),
                                span: Span::default(),
                                loc: Location::default(),
                            },
                            ty: Some(TokenKind::String),
                            default_value: None,
                            span: Span::default(),
                            loc: Location::default(),
                        }],
                        variadic: None,
                    },
                    return_type: Some(Token {
                        kind: TokenKind::I32,
                        span: Span::default(),
                    }),
                    vis_type: VisType::Inline,
                    renamed_as: None,
                    span: Span::default(),
                    loc: Location::default(),
                };
                let ptr = self.build_func_decl(func_decl.clone());
                FuncMetadata {
                    ptr,
                    func_decl,
                    is_internal: true,
                }
            }),
        ];

        for (func_name, func_metadata) in internal_funcs {
            self.func_table.insert(
                func_name.to_string(),
                FuncMetadata {
                    ptr: func_metadata.ptr,
                    func_decl: func_metadata.func_decl,
                    is_internal: func_metadata.is_internal,
                },
            );
        }
    }

    pub(crate) fn build_internal_func_call(
        &self,
        func_call: FuncCall,
        func_metadata: FuncMetadata<'ctx>,
        mut arguments: Vec<BasicMetadataValueEnum<'ctx>>,
    ) -> CallSiteValue<'ctx> {
        let argc = self.build_integer_literal(IntegerLiteral::I32(arguments.len().try_into().unwrap()));
        let func_name = func_metadata.func_decl.name.as_str();

        match func_name {
            "internal_printf" => {
                self.check_func_args_count_mismatch(
                    "printf".to_string(),
                    func_metadata.func_decl.clone(),
                    func_call.clone(),
                );
            }
            "internal_println" => {
                self.check_func_args_count_mismatch(
                    "println".to_string(),
                    func_metadata.func_decl.clone(),
                    func_call.clone(),
                );
            }
            "internal_eprintln" => {
                self.check_func_args_count_mismatch(
                    "eprintln".to_string(),
                    func_metadata.func_decl.clone(),
                    func_call.clone(),
                );
            }
            "internal_eprintf" => {
                self.check_func_args_count_mismatch(
                    "eprintf".to_string(),
                    func_metadata.func_decl.clone(),
                    func_call.clone(),
                );
            }
            "internal_exit" => {
                self.check_func_args_count_mismatch(
                    "exit".to_string(),
                    func_metadata.func_decl.clone(),
                    func_call.clone(),
                );
            }
            "internal_panic" => {
                let file_name = self.build_string_literal(StringLiteral {
                    raw: self.file_path.clone(),
                    span: Span::default(),
                });
                let line = self.build_integer_literal(IntegerLiteral::I32(func_call.loc.line.try_into().unwrap()));
                arguments.insert(0, TryInto::<BasicValueEnum>::try_into(file_name).unwrap().into());
                arguments.insert(1, TryInto::<BasicValueEnum>::try_into(line).unwrap().into());
                arguments.insert(2, BasicMetadataValueEnum::IntValue(argc));
            }
            _ => {
                self.check_func_args_count_mismatch(
                    func_metadata.func_decl.name.clone(),
                    func_metadata.func_decl.clone(),
                    func_call.clone(),
                );
                return self.builder.build_call(func_metadata.ptr, &arguments, "call").unwrap();
            }
        };

        self.builder.build_call(func_metadata.ptr, &arguments, "call").unwrap()
    }

    pub(crate) fn build_call_internal_len(
        &self,
        func_call: FuncCall,
        arguments: Vec<BasicMetadataValueEnum<'ctx>>,
    ) -> CallSiteValue<'ctx> {
        if arguments.len() != 1 {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::FuncCallArgumentCountMismatch(
                    "len".to_string(),
                    1,
                    arguments.len().try_into().unwrap(),
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
        match arguments.first().unwrap() {
            BasicMetadataValueEnum::ArrayValue(array_value) => unsafe {
                let inner = self.builder.build_extract_value(*array_value, 0, "extract").unwrap();
                let len = self.build_integer_literal(IntegerLiteral::U32(inner.into_array_value().get_type().len()));
                CallSiteValue::new(len.as_value_ref())
            },
            BasicMetadataValueEnum::StructValue(struct_value) => {
                let string_type = self.string_type.struct_type;
                if string_type != struct_value.get_type() {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::LenCalledWithInvalidInput,
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: func_call.loc.line,
                            column: func_call.loc.column,
                            length: func_call.span.end,
                        }),
                    });
                    exit(1);
                }

                let len_func = self.func_table.get("internal_len_string").unwrap();
                return self.builder.build_call(len_func.ptr, &arguments, "call").unwrap();
            }
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::LenCalledWithInvalidInput,
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
    }

    pub(crate) fn builder_sizeof_internal(
        &self,
        data_type: AnyType<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> IntValue<'ctx> {
        match data_type {
            crate::AnyType::IntType(int_type) => int_type.size_of(),
            crate::AnyType::FloatType(float_type) => float_type.size_of(),
            crate::AnyType::ArrayType(array_type) => array_type.size_of().unwrap(),
            crate::AnyType::StructType(struct_type) => struct_type.size_of().unwrap(),
            crate::AnyType::VectorType(vector_type) => vector_type.size_of().unwrap(),
            crate::AnyType::StringType(string_type) => string_type.struct_type.size_of().unwrap(),
            crate::AnyType::VoidType(_) => self.build_integer_literal(IntegerLiteral::U64(0)),
            crate::AnyType::OpaquePointer(pointer_type) => pointer_type.size_of(),
            crate::AnyType::PointerType(typed_pointer_type) => {
                self.builder_sizeof_internal(typed_pointer_type.pointee_ty, loc, span_end)
            }
            crate::AnyType::ImportedModuleValue => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot get size of ImportedModuleValue.".to_string()),
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
    }

    pub(crate) fn build_call_internal_sizeof(&self, scope: ScopeRef<'ctx>, func_call: FuncCall) -> CallSiteValue<'ctx> {
        if func_call.arguments.len() != 1 {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::FuncCallArgumentCountMismatch(
                    "len".to_string(),
                    1,
                    func_call.arguments.len().try_into().unwrap(),
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

        let argument = func_call.arguments.first().unwrap();

        let value = {
            if let Expression::TypeToken(type_token) = argument {
                self.builder_sizeof_internal(
                    self.build_type(type_token.kind.clone(), func_call.loc.clone(), func_call.span.end),
                    func_call.loc.clone(),
                    func_call.span.end,
                )
                
            } else if let Expression::ModuleImport(module_import) = argument {
                // User defined type
                self.find_struct(scope, module_import.clone(), func_call.loc.clone(), func_call.span.end)
                    .struct_type
                    .size_of()
                    .unwrap()
            } else {
                self.builder_sizeof_internal(
                    self.build_expr(Rc::clone(&scope), argument.clone()).get_type(self.string_type.clone()),
                    func_call.loc.clone(),
                    func_call.span.end,
                )
            }
        };

        unsafe { CallSiteValue::new(value.as_value_ref()) }
    }
}
