use crate::{CodeGenLLVM, diag::*, funcs::FuncMetadata, scope::ScopeRef};
use ast::{
    ast::{FuncCall, FuncDecl, FuncParam, FuncParams, Identifier, IntegerLiteral, StringLiteral, VisType},
    token::{Location, Span, Token, TokenKind},
};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, CallSiteValue};
use rust_embed::Embed;
use std::{env, fs::File, io::Write, process::exit, rc::Rc};
use utils::generate_random_hex::generate_random_hex;

#[derive(Embed)]
#[folder = "../../internals/"]
struct Asset;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_internals(&mut self) {
        if cfg!(target_os = "linux") {
            let asset_name = "internals_linux.o";
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
            "len" => {
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
}
