use crate::{
    CodeGenLLVM, InternalType, InternalValue,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    funcs::{FuncMetadata, FuncParamsMetadata},
    modules::DefinitionLookupResult,
    scope::{Scope, ScopeRecord, ScopeRef},
    types::{DefinedType, InternalLvalueType, InternalStructType, InternalUnnamedStructType},
    values::Lvalue,
};
use ast::{
    ast::{
        AccessSpecifier, Expression, Field, FieldAccess, FuncDecl, FuncDef, FuncParamKind, FuncVariadicParams,
        Identifier, MethodCall, ModuleImport, ModuleSegment, SelfModifier, Struct, StructInit, TypeSpecifier,
        UnnamedStructType, UnnamedStructValue,
    },
    format::module_segments_as_string,
    token::{Location, Span, Token, TokenKind},
};
use inkwell::{
    AddressSpace,
    llvm_sys::{core::LLVMFunctionType, prelude::LLVMTypeRef},
    module::Linkage,
    types::{BasicTypeEnum, FunctionType, StructType},
    values::{AggregateValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
};
use std::{cell::RefCell, collections::HashMap, ops::DerefMut, process::exit, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct StructMetadata<'a> {
    pub struct_name: ModuleImport,
    pub struct_type: StructType<'a>,
    pub inherits: Vec<Identifier>,
    pub fields: Vec<Field>,
    pub methods: Vec<StructMethodMetadata<'a>>,
    pub access_specifier: AccessSpecifier,
    pub packed: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructMethodMetadata<'a> {
    pub method_decl: FuncDecl,
    pub method_value: FunctionValue<'a>,
    pub method_params_metadata: FuncParamsMetadata<'a>,
    pub is_static_method: bool,
    pub return_type: InternalType<'a>,
    pub self_modifier_type: Option<(InternalType<'a>, SelfModifier)>,
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
            struct_statement.access_specifier,
            AccessSpecifier::Public | AccessSpecifier::Internal
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
            .opaque_struct_type(&self.generate_abi_name(self.module_id.clone(), struct_statement.name.clone()));

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
            access_specifier: struct_statement.access_specifier.clone(),
            packed: struct_statement.packed,
        };

        let internal_struct_type = InternalStructType {
            type_str: struct_statement.name.clone(),
            struct_metadata: struct_metadata.clone(),
        };

        self.struct_table
            .insert(struct_statement.name.clone(), internal_struct_type.clone());

        let field_types = self.build_struct_field_types(struct_statement.name.clone(), struct_statement.fields.clone());
        opaque_struct.set_body(&field_types, struct_statement.packed);

        let completed_struct_type = opaque_struct.clone();

        {
            let internal_struct_type = self.struct_table.get_mut(&struct_statement.name.clone()).unwrap();
            internal_struct_type.struct_metadata.struct_type = completed_struct_type;
        }

        let struct_methods = self.build_struct_methods(struct_statement.name.clone(), struct_statement.methods.clone());

        let internal_struct_type = self.struct_table.get_mut(&struct_statement.name.clone()).unwrap();
        internal_struct_type.struct_metadata.methods = struct_methods;
    }

    fn build_self_modifier_local_alloca(
        &self,
        scope: ScopeRef<'ctx>,
        self_modifier_type_specifier: TypeSpecifier,
        self_modifier_type: BasicTypeEnum<'ctx>,
        func_first_param: BasicValueEnum<'ctx>,
        loc: Location,
        span_end: usize,
    ) {
        let alloca = self.builder.build_alloca(self_modifier_type, "alloca").unwrap();
        self.builder.build_store(alloca, func_first_param).unwrap();
        scope.borrow_mut().insert(
            "self".to_string(),
            ScopeRecord {
                ptr: alloca,
                ty: self.build_type(self_modifier_type_specifier, loc.clone(), span_end),
            },
        );
    }

    fn validate_method_storage_class(&self, func_def: FuncDef) {
        if func_def.access_specifier == AccessSpecifier::Extern {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(
                    "Extern storage class specifier is not permitted in method definitions.".to_string(),
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

    pub(crate) fn build_self_modifier_param(
        &self,
        self_modifier: &SelfModifier,
        struct_name: &str,
        internal_struct_type: &InternalStructType<'ctx>,
        func_loc: &Location,
        func_span: &Span,
    ) -> (InternalType<'ctx>, TypeSpecifier) {
        match self_modifier {
            SelfModifier::Copied => (
                InternalType::StructType(InternalStructType {
                    type_str: internal_struct_type.type_str.clone(),
                    struct_metadata: internal_struct_type.struct_metadata.clone(),
                }),
                TypeSpecifier::Identifier(Identifier {
                    name: struct_name.to_string(),
                    loc: func_loc.clone(),
                    span: func_span.clone(),
                }),
            ),
            SelfModifier::Referenced => (
                InternalType::Lvalue(Box::new(InternalLvalueType {
                    ptr_type: self.context.ptr_type(AddressSpace::default()),
                    pointee_ty: InternalType::StructType(InternalStructType {
                        type_str: internal_struct_type.type_str.clone(),
                        struct_metadata: internal_struct_type.struct_metadata.clone(),
                    }),
                })),
                TypeSpecifier::Dereference(Box::new(TypeSpecifier::Identifier(Identifier {
                    name: struct_name.to_string(),
                    loc: func_loc.clone(),
                    span: func_span.clone(),
                }))),
            ),
        }
    }

    pub(crate) fn build_method_def(
        &mut self,
        mut func_def: FuncDef,
        struct_name: String,
        internal_struct_type: InternalStructType<'ctx>,
    ) -> StructMethodMetadata<'ctx> {
        let scope: ScopeRef<'ctx> = Rc::new(RefCell::new(Scope::new()));
        self.validate_method_storage_class(func_def.clone());
        let func_decl = self.transform_to_func_decl(func_def.clone());
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

        let mut func_params_list = func_def.params.list.clone();
        let mut self_modifier_type: Option<InternalType<'ctx>> = None;
        let mut self_modifier_type_specifier: Option<TypeSpecifier> = None;
        let mut self_modifier_kind: Option<SelfModifier> = None;

        if let Some(params_list) = func_params_list.clone().first() {
            if let FuncParamKind::SelfModifier(self_modifier) = params_list {
                self_modifier_kind = Some(self_modifier.clone());
                func_params_list.remove(0);
                func_def.params.list.remove(0);

                let (ty, spec) = self.build_self_modifier_param(
                    self_modifier,
                    &struct_name,
                    &internal_struct_type,
                    &func_def.loc,
                    &func_def.span,
                );
                self_modifier_type = Some(ty);
                self_modifier_type_specifier = Some(spec);
            }
        }

        let mut params_metadata = self.build_func_params(
            func_def.name.clone(),
            func_def.loc.clone(),
            func_def.span.end,
            func_params_list,
            func_def.params.variadic.clone(),
            self_modifier_type.is_some(),
        );

        if let Some(self_modifier_type) = self_modifier_type.clone() {
            params_metadata.param_types.insert(0, self_modifier_type);
        }

        let fn_type = unsafe {
            FunctionType::new(LLVMFunctionType(
                return_type.as_type_ref(),
                params_metadata
                    .param_types
                    .iter()
                    .map(|p| p.as_type_ref())
                    .collect::<Vec<LLVMTypeRef>>()
                    .as_mut_ptr(),
                params_metadata.param_types.len() as u32,
                is_variadic as i32,
            ))
        };

        let method_name = func_def.name.clone();
        let method_underlying_name =
            self.generate_method_abi_name(self.module_id.clone(), struct_name.clone(), method_name);

        let func_linkage: Option<Linkage> = Some(self.build_func_linkage(func_def.access_specifier.clone()));

        let func_value =
            self.module
                .borrow_mut()
                .deref_mut()
                .add_function(&method_underlying_name, fn_type, func_linkage);

        self.current_func_ref = Some(FuncMetadata {
            ptr: func_value.clone(),
            func_decl: func_decl.clone(),
            return_type: return_type.clone(),
            imported_from: None,
            params_metadata: params_metadata.clone(),
            is_method: true,
        });

        let entry_block = self.context.append_basic_block(func_value, "entry");
        self.builder.position_at_end(entry_block);
        self.current_block_ref = Some(entry_block);

        if let Some(ref self_modifier_type) = self_modifier_type {
            let func_first_param = func_value.get_first_param().unwrap();
            let self_modifier_basic_type =
                match self_modifier_type.to_basic_type(self.context.ptr_type(AddressSpace::default())) {
                    Ok(basic_type) => basic_type,
                    Err(err) => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(err.to_string()),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: func_decl.loc.line,
                                column: func_decl.loc.column,
                                length: func_decl.span.end,
                            }),
                        });
                        exit(1);
                    }
                };

            self.build_self_modifier_local_alloca(
                Rc::clone(&scope),
                self_modifier_type_specifier.clone().unwrap(),
                self_modifier_basic_type,
                func_first_param,
                func_def.loc.clone(),
                func_def.span.end,
            );
        }

        self.build_func_define_local_params(
            Rc::clone(&scope),
            func_value,
            func_def.clone(),
            self_modifier_type.is_some(),
        );

        match func_def.params.variadic.clone() {
            Some(variadic_type) => match variadic_type {
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
                            "C-style variadic arguments not supported in method definition. Consider to add a type annotation for the variadic parameter in method '{}'.",
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
            },
            None => {}
        }

        self.build_statements(Rc::clone(&scope), func_def.body.exprs);

        let current_block = self.get_current_block("method_def statement", func_def.loc.clone(), func_def.span.end);

        if !self.is_block_terminated(current_block) && return_type.is_void_type() {
            self.builder.build_return(None).unwrap();
        } else if !self.is_block_terminated(current_block) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "The method '{}' is missing a return statement.",
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

        StructMethodMetadata {
            method_decl: func_decl,
            method_value: func_value,
            method_params_metadata: params_metadata,
            is_static_method: self_modifier_type.is_none(),
            return_type,
            self_modifier_type: if self_modifier_type.is_some() && self_modifier_kind.is_some() {
                Some((self_modifier_type.unwrap(), self_modifier_kind.unwrap()))
            } else {
                None
            },
        }
    }

    pub(crate) fn build_struct_methods(
        &mut self,
        struct_name: String,
        methods: Vec<FuncDef>,
    ) -> Vec<StructMethodMetadata<'ctx>> {
        let internal_struct_type = self.struct_table.get(&struct_name).cloned().unwrap();

        let mut struct_methods: Vec<StructMethodMetadata<'ctx>> = Vec::new();
        for func_def in methods {
            let method_metadata = self.build_method_def(func_def, struct_name.clone(), internal_struct_type.clone());
            struct_methods.push(method_metadata);
        }
        struct_methods
    }

    pub(crate) fn build_struct_init(&mut self, scope: ScopeRef<'ctx>, struct_init: StructInit) -> InternalValue<'ctx> {
        let defined_type = match self.find_defined_type(
            struct_init.struct_name.clone(),
            struct_init.loc.clone(),
            struct_init.span.end,
        ) {
            Some(defined_type) => defined_type,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::UndefinedDataType(module_segments_as_string(
                        struct_init.struct_name.segments.clone(),
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
        };

        let internal_struct_type = match defined_type {
            DefinedType::Struct(internal_struct_type) => internal_struct_type,
            DefinedType::Typedef(typedef_metadata) => match self.typedef_as_struct_type(typedef_metadata) {
                Some(internal_struct_type) => internal_struct_type,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Cannot build struct init from a non-struct type.".to_string()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: struct_init.loc.line,
                            column: struct_init.loc.column,
                            length: struct_init.span.end,
                        }),
                    });
                    exit(1);
                }
            },
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

    fn retrieve_method_params_metadata(
        &self,
        struct_methods: Vec<(FuncDecl, FunctionValue<'ctx>, FuncParamsMetadata<'ctx>, bool)>,
        method_name: String,
        loc: Location,
        span_end: usize,
    ) -> FuncParamsMetadata<'ctx> {
        match struct_methods.iter().find(|m| m.0.get_usable_name() == method_name) {
            Some((_, _, params_metadata, _)) => params_metadata.clone(),
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Method '{}' not found in struct methods.", method_name)),
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

    pub(crate) fn build_method_call(&mut self, scope: ScopeRef<'ctx>, method_call: MethodCall) -> InternalValue<'ctx> {
        match *method_call.operand.clone() {
            Expression::Identifier(identifier) => match self.lookup_definition(identifier.name.clone()) {
                Some(definition_lookup_result) => match definition_lookup_result {
                    DefinitionLookupResult::Struct(internal_struct_type) => {
                        self.build_static_method_call(scope, method_call, internal_struct_type)
                    }
                    DefinitionLookupResult::Typedef(typedef_metadata) => {
                        match self.typedef_as_struct_type(typedef_metadata) {
                            Some(internal_struct_type) => {
                                self.build_static_method_call(scope, method_call, internal_struct_type)
                            }
                            None => {
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::Custom(
                                        "Cannot build method call on a non-struct value.".to_string(),
                                    ),
                                    location: Some(DiagLoc {
                                        file: self.file_path.clone(),
                                        line: method_call.loc.line,
                                        column: method_call.loc.column,
                                        length: method_call.span.end,
                                    }),
                                });
                                exit(1);
                            }
                        }
                    }
                    DefinitionLookupResult::Func(_) => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom("Cannot build method call on a function value.".to_string()),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: identifier.loc.line,
                                column: identifier.loc.column,
                                length: identifier.span.end,
                            }),
                        });
                        exit(1);
                    }
                    DefinitionLookupResult::GlobalVariable(global_variable_metadata) => todo!(),
                },
                None => {
                    // FIXME
                    todo!();
                    // self.build_instance_method_call(Rc::clone(&scope), method_call)
                }
            },
            Expression::ModuleImport(module_import) => {
                match self.find_defined_type(module_import.clone(), method_call.loc.clone(), method_call.span.end) {
                    Some(defined_type) => match defined_type {
                        DefinedType::Struct(internal_struct_type) => {
                            self.build_static_method_call(scope, method_call, internal_struct_type)
                        }
                        DefinedType::Typedef(typedef_metadata) => match self.typedef_as_struct_type(typedef_metadata) {
                            Some(internal_struct_type) => {
                                self.build_static_method_call(scope, method_call, internal_struct_type)
                            }
                            None => {
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::Custom(
                                        "Cannot build method call on a non-struct value.".to_string(),
                                    ),
                                    location: Some(DiagLoc {
                                        file: self.file_path.clone(),
                                        line: method_call.loc.line,
                                        column: method_call.loc.column,
                                        length: method_call.span.end,
                                    }),
                                });
                                exit(1);
                            }
                        },
                    },
                    None => self.build_instance_method_call(Rc::clone(&scope), method_call),
                }
            }
            _ => {
                panic!();
                // self.build_instance_method_call(Rc::clone(&scope), method_call)
            }
        }
    }

    pub(crate) fn build_static_method_call(
        &mut self,
        scope: ScopeRef<'ctx>,
        method_call: MethodCall,
        internal_struct_type: InternalStructType<'ctx>,
    ) -> InternalValue<'ctx> {
        let method_metadata = internal_struct_type
            .struct_metadata
            .methods
            .iter()
            .find(|method| method.method_decl.get_usable_name() == method_call.method_name.name);

        match method_metadata {
            Some(struct_method_metadata) => {
                if !struct_method_metadata.is_static_method {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(
                            "Non-static method cannot be called without an instance of the object.".to_string(),
                        ),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: method_call.method_name.loc.line,
                            column: method_call.method_name.loc.column,
                            length: method_call.method_name.span.end,
                        }),
                    });
                    exit(1);
                }

                let func_basic_blocks = struct_method_metadata.method_value.get_basic_blocks();
                let current_block =
                    self.get_current_block("method call", method_call.loc.clone(), method_call.span.end);

                if !func_basic_blocks.contains(&current_block)
                    && struct_method_metadata.method_decl.access_specifier != AccessSpecifier::Public
                {
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

                // static methods args count checking are the same as the funcs
                self.check_func_args_count_mismatch(
                    struct_method_metadata.method_decl.name.clone(),
                    struct_method_metadata.method_decl.clone(),
                    method_call.arguments.len(),
                    method_call.loc.clone(),
                    method_call.span.end,
                );

                let static_params_length = struct_method_metadata.method_params_metadata.param_types.len();

                let arguments = self.build_arguments(
                    Rc::clone(&scope),
                    method_call.arguments.clone(),
                    struct_method_metadata.method_params_metadata.clone(),
                    0,
                    static_params_length,
                    struct_method_metadata.method_decl.get_usable_name(),
                    method_call.loc.clone(),
                    method_call.span.end,
                );

                let call_site_value = self
                    .builder
                    .build_call(struct_method_metadata.method_value, &arguments, "call")
                    .unwrap();
                let return_type = self.build_type(
                    struct_method_metadata
                        .method_decl
                        .return_type
                        .clone()
                        .unwrap_or(TypeSpecifier::TypeToken(Token {
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
                        "Static method '{}' not defined for struct '{}'.",
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

    pub(crate) fn check_instance_method_args_count_mismatch(
        &self,
        func_name: String,
        func_decl: FuncDecl,
        arguments_length: usize,
        loc: Location,
        span_end: usize,
    ) {
        // exclude self modifier from the params length
        let expected_count = func_decl.params.list.len() - 1;

        if func_decl.params.variadic.is_none() && expected_count != arguments_length {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::MethodCallArgumentCountMismatch(
                    func_name.clone(),
                    arguments_length.try_into().unwrap(),
                    expected_count.try_into().unwrap(),
                ),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: loc.line,
                    column: loc.column,
                    length: span_end,
                }),
            });
            exit(1);
        } else if func_decl.params.variadic.is_some() && arguments_length < expected_count {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::MethodCallArgumentCountMismatch(
                    func_name.clone(),
                    arguments_length.try_into().unwrap(),
                    expected_count.try_into().unwrap(),
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

    pub(crate) fn build_instance_method_call(
        &mut self,
        scope: ScopeRef<'ctx>,
        method_call: MethodCall,
    ) -> InternalValue<'ctx> {
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
        let method_metadata_opt = internal_struct_type
            .struct_metadata
            .methods
            .iter()
            .find(|method| method.method_decl.get_usable_name() == method_call.method_name.name);

        match method_metadata_opt.cloned() {
            Some(mut method_metadata) => {
                if method_metadata.is_static_method {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Cannot call static method '{}' from an object instance. Call it directly instead.",
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

                let func_basic_blocks = method_metadata.method_value.get_basic_blocks();
                let current_block =
                    self.get_current_block("method call", method_call.loc.clone(), method_call.span.end);

                if !func_basic_blocks.contains(&current_block)
                    && method_metadata.method_decl.access_specifier != AccessSpecifier::Public
                {
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

                let mut arguments: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();

                // pass self modifier value if exists
                if let Some(first_param) = method_metadata.method_decl.params.list.first() {
                    if let FuncParamKind::SelfModifier(self_modifier) = first_param {
                        match self_modifier {
                            SelfModifier::Copied => {
                                arguments.push(operand_rvalue.to_basic_metadata());
                            }
                            SelfModifier::Referenced => {
                                arguments.push(operand.to_basic_metadata());
                            }
                        }
                    }

                    method_metadata.method_params_metadata.param_types.remove(0); // remove self_modifier from normal params
                }

                self.check_instance_method_args_count_mismatch(
                    method_metadata.method_decl.name.clone(),
                    method_metadata.method_decl.clone(),
                    // include self modifier which is added manually from the arguments count
                    method_call.arguments.len(),
                    method_call.loc.clone(),
                    method_call.span.end,
                );

                let static_params_length = method_metadata.method_params_metadata.param_types.len() - 1;

                arguments.append(&mut self.build_arguments(
                    Rc::clone(&scope),
                    method_call.arguments.clone(),
                    method_metadata.method_params_metadata.clone(),
                    // exclude self modifier
                    static_params_length,
                    1,
                    method_metadata.method_decl.get_usable_name(),
                    method_call.loc.clone(),
                    method_call.span.end,
                ));

                let call_site_value = self
                    .builder
                    .build_call(method_metadata.method_value, &arguments, "call")
                    .unwrap();
                let return_type = self.build_type(
                    method_metadata
                        .method_decl
                        .return_type
                        .clone()
                        .unwrap_or(TypeSpecifier::TypeToken(Token {
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
