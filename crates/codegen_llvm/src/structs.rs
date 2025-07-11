use crate::{
    CodeGenLLVM, InternalType, InternalValue,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    scope::ScopeRef,
    types::{DefinedType, InternalLvalueType, InternalStructType, InternalUnnamedStructType},
    values::Lvalue,
};
use ast::{
    ast::{
        Field, FieldAccess, FuncDecl, FuncDef, FuncParam, Identifier, MethodCall, ModuleImport, ModuleSegment,
        StorageClass, Struct, StructInit, TypeSpecifier, UnnamedStructType, UnnamedStructValue,
    },
    format::module_segments_as_string,
    token::{Location, Span, Token, TokenKind},
};
use inkwell::{
    AddressSpace,
    llvm_sys::prelude::LLVMTypeRef,
    types::{BasicTypeEnum, StructType},
    values::{AggregateValueEnum, BasicValueEnum, FunctionValue, PointerValue},
};
use std::{collections::HashMap, process::exit, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct StructMetadata<'a> {
    pub struct_name: ModuleImport,
    pub struct_type: StructType<'a>,
    pub inherits: Vec<Identifier>,
    pub fields: Vec<Field>,
    pub methods: Vec<(FuncDecl, FunctionValue<'a>)>,
    pub storage_class: StorageClass,
    pub packed: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnnamedStructTypeMetadata<'a> {
    pub struct_type: StructType<'a>,
    pub fields: Vec<(String, InternalType<'a>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnnamedStructValueMetadata<'a> {
    pub struct_type: StructType<'a>,
    pub fields: Vec<(String, InternalType<'a>)>,
    pub packed: bool,
}

pub type StructTable<'a> = HashMap<String, InternalStructType<'a>>;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn is_struct_using_uncompleted_type(
        &self,
        struct_name: String,
        field_type_specifier: TypeSpecifier,
    ) -> bool {
        match field_type_specifier.clone() {
            TypeSpecifier::Identifier(identifier) => identifier.name == struct_name,
            TypeSpecifier::Const(inner_type_specifier) => {
                self.is_struct_using_uncompleted_type(struct_name, *inner_type_specifier)
            }
            TypeSpecifier::Array(array_type_specifier) => {
                self.is_struct_using_uncompleted_type(struct_name, *array_type_specifier.element_type)
            }
            _ => false,
        }
    }

    pub(crate) fn build_struct_field_types(&self, struct_name: String, fields: Vec<Field>) -> Vec<BasicTypeEnum> {
        fields
            .iter()
            .map(|field| {
                if field.name == "this" {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("The field name 'this' is a reserved keyword and cannot be used. Please choose a different field name.".to_string()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: field.loc.line,
                            column: field.loc.column,
                            length: field.span.end,
                        }),
                    });
                    exit(1);
                }

                if self.is_struct_using_uncompleted_type(struct_name.clone(), field.ty.clone()) {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Field has incomplete type '{}'. Consider to use '{}*' instead",
                            struct_name, struct_name,
                        )),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: field.loc.line,
                            column: field.loc.column,
                            length: field.span.end,
                        }),
                    });
                    exit(1);
                }

                match self.build_type(field.ty.clone(), field.loc.clone(), field.span.end)
                    .to_basic_type(self.context.ptr_type(AddressSpace::default())) {
                        Ok(basic_type) => basic_type,
                        Err(err) => {
                            display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(err.to_string()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: field.loc.line,
                            column: field.loc.column,
                            length: field.span.end,
                        }),
                    });
                    exit(1);
                        },
                    }
            })
            .collect()
    }

    pub(crate) fn build_global_struct(&mut self, struct_statement: Struct) {
        if !matches!(
            struct_statement.storage_class,
            StorageClass::Public | StorageClass::Internal
        ) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Structs can only be defined public or internal.".to_string()),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: struct_statement.loc.line,
                    column: struct_statement.loc.column,
                    length: struct_statement.span.end,
                }),
            });
            exit(1);
        }

        let opaque_struct = self
            .context
            .opaque_struct_type(&format!("{}::{}", self.module_id, struct_statement.name));

        let struct_metadata = StructMetadata {
            struct_name: ModuleImport {
                segments: vec![ModuleSegment::SubModule(Identifier {
                    name: struct_statement.name.clone(),
                    span: struct_statement.span.clone(),
                    loc: struct_statement.loc.clone(),
                })],
                span: struct_statement.span.clone(),
                loc: struct_statement.loc.clone(),
            },
            struct_type: opaque_struct,
            fields: struct_statement.fields.clone(),
            methods: Vec::new(),
            inherits: struct_statement.inherits.clone(),
            storage_class: struct_statement.storage_class.clone(),
            packed: struct_statement.packed,
        };

        let internal_struct_type = InternalStructType {
            type_str: "".to_string(), // FIXME Struct formatter
            struct_metadata: struct_metadata.clone(),
        };

        self.struct_table
            .insert(struct_statement.name.clone(), internal_struct_type.clone());

        let field_types = self.build_struct_field_types(struct_statement.name.clone(), struct_statement.fields.clone());
        opaque_struct.set_body(&field_types, struct_statement.packed);

        let struct_methods = self.build_struct_methods(
            internal_struct_type,
            struct_statement.name.clone(),
            struct_statement.methods.clone(),
        );

        let internal_struct_type = self.struct_table.get_mut(&struct_statement.name.clone()).unwrap();
        internal_struct_type.struct_metadata.methods = struct_methods;
        internal_struct_type.struct_metadata.struct_type = opaque_struct;
    }

    pub(crate) fn build_struct_methods(
        &mut self,
        internal_struct_type: InternalStructType<'ctx>,
        struct_name: String,
        methods: Vec<FuncDef>,
    ) -> Vec<(FuncDecl, FunctionValue<'ctx>)> {
        let mut struct_methods: Vec<(FuncDecl, FunctionValue<'ctx>)> = Vec::new();

        for mut func_def in methods {
            let method_name = func_def.name.clone();
            let method_underlying_name =
                self.generate_abi_name(self.module_id.clone(), format!("{}_{}", struct_name.clone(), method_name));
            func_def.name = method_underlying_name.clone();

            // this_modifier
            let mut func_param_types: Vec<LLVMTypeRef> = self.build_func_params(
                func_def.name.clone(),
                func_def.loc.clone(),
                func_def.span.end,
                func_def.params.list.clone(),
                func_def.params.variadic.clone(),
            );

            func_param_types.insert(
                0,
                InternalType::Lvalue(Box::new(InternalLvalueType {
                    ptr_type: self.context.ptr_type(AddressSpace::default()),
                    pointee_ty: InternalType::StructType(InternalStructType {
                        type_str: internal_struct_type.type_str.clone(),
                        struct_metadata: internal_struct_type.struct_metadata.clone(),
                    }),
                }))
                .as_type_ref(),
            );

            let this_modifier = FuncParam {
                identifier: Identifier {
                    name: "this".to_string(),
                    loc: func_def.loc.clone(),
                    span: func_def.span.clone(),
                },
                ty: Some(TypeSpecifier::Dereference(Box::new(TypeSpecifier::Identifier(
                    Identifier {
                        name: struct_name.clone(),
                        loc: func_def.loc.clone(),
                        span: func_def.span.clone(),
                    },
                )))),
                default_value: None,
                loc: func_def.loc.clone(),
                span: func_def.span.clone(),
            };
            func_def.params.list.insert(0, this_modifier);

            let func_value = self.build_func_def(func_def.clone(), func_param_types, false);

            let func_decl = FuncDecl {
                name: method_underlying_name,
                params: func_def.params,
                return_type: func_def.return_type,
                storage_class: func_def.storage_class,
                renamed_as: Some(method_name),
                span: func_def.span.clone(),
                loc: func_def.loc.clone(),
            };

            struct_methods.push((func_decl, func_value))
        }

        struct_methods
    }

    pub(crate) fn build_struct_init(&mut self, scope: ScopeRef<'ctx>, struct_init: StructInit) -> InternalValue<'ctx> {
        let defined_type = self.find_defined_type(
            struct_init.struct_name.clone(),
            struct_init.loc.clone(),
            struct_init.span.end,
        );

        let internal_struct_type = match defined_type {
            DefinedType::Struct(internal_struct_type) => internal_struct_type,
        };

        if internal_struct_type.struct_metadata.fields.len() != struct_init.field_inits.len() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Struct '{}' has {} fields, but {} fields were provided.",
                    struct_init.struct_name.to_string(),
                    internal_struct_type.struct_metadata.fields.len(),
                    struct_init.field_inits.len()
                )),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: struct_init.loc.line,
                    column: struct_init.loc.column,
                    length: struct_init.span.end,
                }),
            });
            exit(1);
        }

        let mut struct_value = internal_struct_type.struct_metadata.struct_type.get_undef();

        for field_init in struct_init.field_inits {
            let field_idx = internal_struct_type
                .struct_metadata
                .fields
                .iter()
                .position(|field| field.name == field_init.name)
                .unwrap();

            let field = internal_struct_type.struct_metadata.fields.get(field_idx).unwrap();

            let field_type = self.build_type(field.ty.clone(), field.loc.clone(), field.span.end);

            let field_expr = self.build_expr(Rc::clone(&scope), field_init.value.clone());
            let field_rvalue = self.internal_value_as_rvalue(field_expr, field.loc.clone(), field.span.end);

            if !self.compatible_types(field_type.clone(), field_rvalue.get_type(self.string_type.clone())) {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Error: Field {} of struct '{}' expects a value of type '{}', but received '{}'.",
                        field_idx,
                        module_segments_as_string(struct_init.struct_name.segments),
                        field_type,
                        field_rvalue.get_type(self.string_type.clone())
                    )),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: struct_init.loc.line,
                        column: struct_init.loc.column,
                        length: struct_init.span.end,
                    }),
                });
                exit(1);
            }

            let field_rvalue =
                self.implicit_cast(field_rvalue, field_type, struct_init.loc.clone(), struct_init.span.end);

            struct_value = self
                .builder
                .build_insert_value(
                    AggregateValueEnum::StructValue(struct_value),
                    field_rvalue,
                    field_idx.try_into().unwrap(),
                    "insert_data",
                )
                .unwrap()
                .into_struct_value();
        }

        InternalValue::StructValue(struct_value, InternalType::StructType(internal_struct_type.clone()))
    }

    pub(crate) fn build_struct_field_access(
        &self,
        pointer: PointerValue<'ctx>,
        field_name: String,
        struct_internal_type: InternalType<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        match struct_internal_type {
            InternalType::StructType(internal_struct_type) => {
                match internal_struct_type
                    .struct_metadata
                    .fields
                    .iter()
                    .enumerate()
                    .find(|(_, f)| f.name == field_name)
                {
                    Some((field_idx, field)) => {
                        let field_ptr = self
                            .builder
                            .build_struct_gep(
                                internal_struct_type.struct_metadata.struct_type,
                                pointer,
                                field_idx.try_into().unwrap(),
                                "gep",
                            )
                            .unwrap();

                        InternalValue::Lvalue(Lvalue {
                            ptr: field_ptr,
                            pointee_ty: self.build_type(field.ty.clone(), field.loc.clone(), field.span.end),
                        })
                    }
                    None => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(format!(
                                "Struct '{}' has not field named '{}'.",
                                module_segments_as_string(internal_struct_type.struct_metadata.struct_name.segments),
                                field_name
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
                }
            }
            InternalType::UnnamedStruct(internal_unnamed_struct_type) => internal_unnamed_struct_type
                .unnamed_struct_metadata
                .fields
                .iter()
                .enumerate()
                .find(|(_, f)| f.0 == field_name)
                .map_or_else(
                    || {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(format!("Unnamed struct has not field named '{}'.", field_name)),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: loc.line,
                                column: loc.column,
                                length: span_end,
                            }),
                        });
                        exit(1);
                    },
                    |(field_idx, (_, field_type))| {
                        let field_ptr = self
                            .builder
                            .build_struct_gep(
                                internal_unnamed_struct_type.unnamed_struct_metadata.struct_type,
                                pointer,
                                field_idx.try_into().unwrap(),
                                "gep",
                            )
                            .unwrap();

                        InternalValue::Lvalue(Lvalue {
                            ptr: field_ptr,
                            pointee_ty: field_type.clone(),
                        })
                    },
                ),
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_method_call(&mut self, scope: ScopeRef<'ctx>, method_call: MethodCall) -> InternalValue<'ctx> {
        let mut operand = self.build_expr(Rc::clone(&scope), *method_call.operand.clone());
        let mut operand_rvalue =
            self.internal_value_as_rvalue(operand.clone(), method_call.loc.clone(), method_call.span.end);

        if method_call.is_fat_arrow {
            if !operand_rvalue.get_type(self.string_type.clone()).is_pointer_type() {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build fat arrow on non-pointer values.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: method_call.loc.line,
                        column: method_call.loc.column,
                        length: method_call.span.end,
                    }),
                });
                exit(1);
            }

            operand = self.build_deref_internal(operand_rvalue.clone(), method_call.loc.clone(), method_call.span.end);
            operand_rvalue =
                self.internal_value_as_rvalue(operand.clone(), method_call.loc.clone(), method_call.span.end);
        }

        let internal_struct_type = match operand_rvalue.clone() {
            InternalValue::StructValue(_, internal_type) => match internal_type {
                InternalType::StructType(internal_struct_type) => internal_struct_type,
                _ => unreachable!(),
            },
            InternalValue::PointerValue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build method call on pointer values. Consider to deference before calling the method or use fat arrow (->) instead.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: method_call.method_name.loc.line,
                        column: method_call.method_name.loc.column,
                        length: method_call.method_name.span.end,
                    }),
                });
                exit(1);
            }
            _ => {
                return self.build_internal_methods(
                    method_call.method_name.name,
                    operand_rvalue,
                    method_call.loc.clone(),
                    method_call.span.end,
                );
            }
        };

        // let's find the method from struct_metadata
        let method_metadata = internal_struct_type
            .struct_metadata
            .methods
            .iter()
            .find(|method| method.0.get_usable_name() == method_call.method_name.name);

        match method_metadata {
            Some((func_decl, func_value)) => {
                let func_basic_blocks = func_value.get_basic_blocks();
                let current_block =
                    self.get_current_block("method call", method_call.loc.clone(), method_call.span.end);

                if !func_basic_blocks.contains(&current_block) && func_decl.storage_class != StorageClass::Public {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Method '{}' is defined internally for the struct and cannot be called directly from outside. It is intended for internal use only.",
                            method_call.method_name.name.clone()
                        )),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: method_call.method_name.loc.line,
                            column: method_call.method_name.loc.column,
                            length: method_call.method_name.span.end,
                        }),
                    });
                    exit(1);
                }

                // set this_modifier value as first argument
                let mut arguments: Vec<inkwell::values::BasicMetadataValueEnum<'_>> = vec![operand.to_basic_metadata()];

                let mut func_params_without_this_modifier = func_decl.params.clone();
                func_params_without_this_modifier.list.remove(0);

                arguments.append(&mut self.build_arguments(
                    Rc::clone(&scope),
                    method_call.arguments.clone(),
                    func_params_without_this_modifier,
                    func_decl.get_usable_name(),
                    method_call.loc.clone(),
                    method_call.span.end,
                ));

                self.check_method_args_count_mismatch(
                    func_decl.name.clone(),
                    func_decl.clone(),
                    method_call.arguments.len() + 1,
                    method_call.loc.clone(),
                    method_call.span.end,
                );

                let call_site_value = self.builder.build_call(*func_value, &arguments, "call").unwrap();
                let return_type = self.build_type(
                    func_decl.return_type.clone().unwrap_or(TypeSpecifier::TypeToken(Token {
                        kind: TokenKind::Void,
                        span: Span::default(),
                        loc: Location::default(),
                    })),
                    method_call.loc.clone(),
                    method_call.span.end,
                );

                if let Some(value) = call_site_value.try_as_basic_value().left() {
                    self.new_internal_value(value, return_type)
                } else {
                    InternalValue::PointerValue(self.build_null())
                }
            }
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Method '{}' not defined for struct '{}'.",
                        method_call.method_name.name.clone(),
                        module_segments_as_string(internal_struct_type.struct_metadata.struct_name.segments)
                    )),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: method_call.method_name.loc.line,
                        column: method_call.method_name.loc.column,
                        length: method_call.method_name.span.end,
                    }),
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_field_access(
        &mut self,
        scope: ScopeRef<'ctx>,
        field_access: FieldAccess,
    ) -> InternalValue<'ctx> {
        let mut internal_value = self.build_expr(Rc::clone(&scope), *field_access.operand);

        if field_access.is_fat_arrow {
            let rvalue =
                self.internal_value_as_rvalue(internal_value.clone(), field_access.loc.clone(), field_access.span.end);

            if !rvalue.get_type(self.string_type.clone()).is_pointer_type() {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build fat arrow on non-pointer values.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: field_access.loc.line,
                        column: field_access.loc.column,
                        length: field_access.span.end,
                    }),
                });
                exit(1);
            }

            internal_value = self.build_deref_internal(rvalue, field_access.loc.clone(), field_access.span.end);
        }

        match self.internal_value_as_rvalue(internal_value.clone(), field_access.loc.clone(), field_access.span.end) {
            InternalValue::StructValue(_, struct_internal_type)
            | InternalValue::UnnamedStructValue(_, struct_internal_type) => {
                let pointer = match internal_value {
                    InternalValue::PointerValue(typed_pointer_value) => typed_pointer_value.ptr,
                    InternalValue::Lvalue(lvalue) => lvalue.ptr,
                    _ => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(format!(
                                "InternalValue::PointerValue or InternalValue::Lvalue for struct field access but got '{}'.",
                                internal_value.get_type(self.string_type.clone())
                            )),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: field_access.loc.line,
                                column: field_access.loc.column,
                                length: field_access.span.end,
                            }),
                        });
                        exit(1);
                    }
                };

                self.build_struct_field_access(
                    pointer,
                    field_access.field_name.name,
                    struct_internal_type,
                    field_access.loc.clone(),
                    field_access.span.end,
                )
            }
            InternalValue::ModuleValue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot access fields on a module_value.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: field_access.loc.line,
                        column: field_access.loc.column,
                        length: field_access.span.end,
                    }),
                });
                exit(1);
            }
            InternalValue::PointerValue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build field access on pointer values. Consider to deference before accessing the field or use fat arrow (->) instead.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: field_access.loc.line,
                        column: field_access.loc.column,
                        length: field_access.span.end,
                    }),
                });
                exit(1);
            }
            InternalValue::FunctionValue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot access fields on an function_value.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: field_access.loc.line,
                        column: field_access.loc.column,
                        length: field_access.span.end,
                    }),
                });
                exit(1);
            }
            InternalValue::Lvalue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot access fields on an lvalue.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: field_access.loc.line,
                        column: field_access.loc.column,
                        length: field_access.span.end,
                    }),
                });
                exit(1);
            }
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build field access for non-struct values.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: field_access.loc.line,
                        column: field_access.loc.column,
                        length: field_access.span.end,
                    }),
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_unnamed_struct_type(&self, unnamed_struct: UnnamedStructType) -> InternalType<'ctx> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let field_types: Vec<BasicTypeEnum<'ctx>> = unnamed_struct
            .fields
            .iter()
            .map(|unnamed_struct_type_field| {
                match self
                    .build_type(
                        unnamed_struct_type_field.field_type.clone(),
                        unnamed_struct_type_field.loc.clone(),
                        unnamed_struct_type_field.span.end,
                    )
                    .to_basic_type(ptr_type)
                {
                    Ok(basic_type) => basic_type,
                    Err(err) => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(err.to_string()),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: unnamed_struct_type_field.loc.line,
                                column: unnamed_struct_type_field.loc.column,
                                length: unnamed_struct_type_field.span.end,
                            }),
                        });
                        exit(1);
                    }
                }
            })
            .collect();

        let struct_type = self.context.struct_type(&field_types, unnamed_struct.packed);
        let unnamed_struct_metadata = UnnamedStructTypeMetadata {
            struct_type,
            fields: unnamed_struct
                .fields
                .iter()
                .map(|f| {
                    (
                        f.field_name.clone(),
                        self.build_type(f.field_type.clone(), f.loc.clone(), f.span.end),
                    )
                })
                .collect(),
        };
        InternalType::UnnamedStruct(InternalUnnamedStructType {
            type_str: "".to_string(), // FIXME
            unnamed_struct_metadata,
        })
    }

    pub(crate) fn build_unnamed_struct_value(
        &mut self,
        scope: ScopeRef<'ctx>,
        unnamed_struct_value: UnnamedStructValue,
    ) -> InternalValue<'ctx> {
        let mut field_values: Vec<(String, InternalValue<'ctx>)> = Vec::new();

        for (_, field) in unnamed_struct_value.fields.iter().enumerate() {
            let field_type: InternalType<'ctx>;

            let field_expr = self.build_expr(Rc::clone(&scope), *field.field_value.clone());
            let field_value = self.internal_value_as_rvalue(field_expr, field.loc.clone(), field.span.end);

            if let Some(field_type_specifier) = field.field_type.clone() {
                field_type = self.build_type(field_type_specifier, field.loc.clone(), field.span.end);
            } else {
                field_type = field_value.get_type(self.string_type.clone());
            }

            if !self.compatible_types(field_type.clone(), field_value.get_type(self.string_type.clone())) {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Rvalue with type '{}' is not compatible with field type '{}' for field '{}'.",
                        field_value.get_type(self.string_type.clone()),
                        field_type,
                        field.field_name
                    )),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: field.loc.line,
                        column: field.loc.column,
                        length: field.span.end,
                    }),
                });
                exit(1);
            }

            let field_value = self.new_internal_value(
                self.implicit_cast(field_value, field_type.clone(), field.loc.clone(), field.span.end),
                field_type.clone(),
            );

            field_values.push((field.field_name.name.clone(), field_value));
        }

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let field_types: Vec<BasicTypeEnum<'ctx>> = field_values
            .iter()
            .map(
                |f| match f.1.get_type(self.string_type.clone()).to_basic_type(ptr_type).clone() {
                    Ok(basic_type) => basic_type,
                    Err(err) => {
                        // FIXME Error location does not work here. You can make it better by refactoring the whole function.
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(err.to_string()),
                            location: None,
                        });
                        exit(1);
                    }
                },
            )
            .collect();

        let struct_type = self.context.struct_type(&field_types, unnamed_struct_value.packed);
        let struct_alloca = self.builder.build_alloca(struct_type, "alloca").unwrap();

        for (idx, (_, field_value)) in field_values.iter().enumerate() {
            let field_gep = self
                .builder
                .build_struct_gep(struct_type, struct_alloca, idx.try_into().unwrap(), "gep")
                .unwrap();

            let field_basic_value: BasicValueEnum<'ctx> = field_value.to_basic_metadata().try_into().unwrap();
            self.builder.build_store(field_gep, field_basic_value).unwrap();
        }

        let struct_value = self
            .builder
            .build_load(struct_type, struct_alloca, "load")
            .unwrap()
            .into_struct_value();

        let unnamed_struct_metadata = UnnamedStructTypeMetadata {
            struct_type,
            fields: field_values
                .iter()
                .map(|(field_name, field_value)| {
                    (
                        field_name.clone(),
                        field_value.get_type(self.string_type.clone()).clone(),
                    )
                })
                .collect(),
        };

        InternalValue::UnnamedStructValue(
            struct_value,
            InternalType::UnnamedStruct(InternalUnnamedStructType {
                type_str: "".to_string(), // FIXME
                unnamed_struct_metadata,
            }),
        )
    }
}
