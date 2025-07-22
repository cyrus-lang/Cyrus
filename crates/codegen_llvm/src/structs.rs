use crate::{
    context::CodeGenLLVM,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    funcs::{FuncMetadata, FuncParamsMetadata},
    modules::{LocalIRValue, LocalIRValueID, ModuleID, generate_local_ir_value_id},
    scope::{Scope, ScopeRecord, ScopeRef},
    types::{InternalLvalueType, InternalStructType, InternalType, InternalUnnamedStructType},
    values::{InternalValue, Lvalue},
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
    values::{AggregateValueEnum, BasicMetadataValueEnum, BasicValueEnum, PointerValue},
};
use rand::Rng;
use std::{cell::RefCell, collections::HashMap, ops::DerefMut, process::exit, rc::Rc};

pub type StructID = u64;

#[derive(Debug, Clone, PartialEq)]
pub struct StructMetadata<'a> {
    pub struct_id: StructID,
    pub struct_name: ModuleImport,
    pub struct_type: StructType<'a>,
    pub fields: Vec<Field>,
    pub methods: Vec<StructMethodMetadata<'a>>,
    pub access_specifier: AccessSpecifier,
    pub imported_from: Option<ModuleID>,
    pub packed: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructMethodMetadata<'a> {
    pub local_ir_value_id: LocalIRValueID,
    pub method_decl: FuncDecl,
    pub method_params_metadata: FuncParamsMetadata<'a>,
    pub return_type: InternalType<'a>,
    pub self_modifier_type: Option<(InternalType<'a>, SelfModifier)>,
    pub is_static_method: bool,
}

impl<'a> StructMetadata<'a> {
    pub fn to_internal_struct_type(&self) -> InternalStructType<'a> {
        InternalStructType {
            struct_id: self.struct_id,
            type_str: module_segments_as_string(self.struct_name.segments.clone()),
            struct_name: self.struct_name.clone(),
            struct_type: self.struct_type.clone(),
            fields: self.fields.clone(),
            methods: self.methods.clone(),
        }
    }
}

fn generate_struct_id() -> StructID {
    let mut rng = rand::rng();
    rng.random::<u64>()
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

pub type StructTable<'a> = HashMap<String, StructMetadata<'a>>;

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
                kind: DiagKind::InvalidStructAccessSpecifier,
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: struct_statement.loc.line,
                    column: struct_statement.loc.column,
                    length: struct_statement.span.end,
                }),
            });
            exit(1);
        }

        let opaque_struct = self.context.opaque_struct_type(
            &self.generate_struct_abi_name(self.module_name.clone(), struct_statement.name.clone()),
        );

        let struct_id = generate_struct_id();
        let struct_metadata = StructMetadata {
            struct_id,
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
            access_specifier: struct_statement.access_specifier.clone(),
            packed: struct_statement.packed,
            imported_from: None,
        };

        let mut module_metadata = self.get_module_metadata_by_module_id(self.module_id).unwrap();
        module_metadata.insert_struct(struct_statement.name.clone(), struct_metadata);
        drop(module_metadata);

        let field_types = self.build_struct_field_types(struct_statement.name.clone(), struct_statement.fields.clone());
        opaque_struct.set_body(&field_types, struct_statement.packed);

        let completed_struct_type = opaque_struct.clone();

        let mut module_metadata = self.get_module_metadata_by_module_id(self.module_id).unwrap();
        module_metadata.get_and_update_struct_type(struct_id, completed_struct_type);
        drop(module_metadata);

        let struct_methods = self.build_struct_methods(struct_id, struct_statement.methods.clone());

        let mut module_metadata = self.get_module_metadata_by_module_id(self.module_id).unwrap();
        module_metadata.get_and_update_struct_methods(struct_id, struct_methods);
        drop(module_metadata);
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
        struct_metadata: &StructMetadata<'ctx>,
        func_loc: &Location,
        func_span: &Span,
    ) -> (InternalType<'ctx>, TypeSpecifier) {
        match self_modifier {
            SelfModifier::Copied => (
                InternalType::StructType(InternalStructType {
                    struct_id: struct_metadata.struct_id,
                    struct_name: struct_metadata.struct_name.clone(),
                    struct_type: struct_metadata.struct_type.clone(),
                    fields: struct_metadata.fields.clone(),
                    methods: struct_metadata.methods.clone(),
                    type_str: "self".to_string(),
                }),
                TypeSpecifier::Identifier(Identifier {
                    name: struct_metadata.struct_name.to_string(),
                    loc: func_loc.clone(),
                    span: func_span.clone(),
                }),
            ),
            SelfModifier::Referenced => (
                InternalType::Lvalue(Box::new(InternalLvalueType {
                    ptr_type: self.context.ptr_type(AddressSpace::default()),
                    pointee_ty: InternalType::StructType(InternalStructType {
                        struct_id: struct_metadata.struct_id,
                        struct_name: struct_metadata.struct_name.clone(),
                        struct_type: struct_metadata.struct_type.clone(),
                        fields: struct_metadata.fields.clone(),
                        methods: struct_metadata.methods.clone(),
                        type_str: "&self".to_string(),
                    }),
                })),
                TypeSpecifier::Dereference(Box::new(TypeSpecifier::Identifier(Identifier {
                    name: struct_metadata.struct_name.to_string(),
                    loc: func_loc.clone(),
                    span: func_span.clone(),
                }))),
            ),
        }
    }

    pub(crate) fn build_method_def(
        &mut self,
        mut func_def: FuncDef,
        struct_metadata: StructMetadata<'ctx>,
    ) -> StructMethodMetadata<'ctx> {
        let scope: ScopeRef<'ctx> = Rc::new(RefCell::new(Scope::new()));
        self.validate_method_storage_class(func_def.clone());
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

        let mut func_params_list = func_def.params.list.clone();
        let mut self_modifier_type: Option<InternalType<'ctx>> = None;
        let mut self_modifier_type_specifier: Option<TypeSpecifier> = None;
        let mut self_modifier_kind: Option<SelfModifier> = None;

        if let Some(params_list) = func_params_list.clone().first() {
            if let FuncParamKind::SelfModifier(self_modifier) = params_list {
                self_modifier_kind = Some(self_modifier.clone());
                func_params_list.remove(0);
                func_def.params.list.remove(0);

                let (ty, spec) =
                    self.build_self_modifier_param(self_modifier, &struct_metadata, &func_def.loc, &func_def.span);
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

        let local_ir_value_id = generate_local_ir_value_id();

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
        let method_abi_name = self.generate_method_abi_name(
            self.module_name.clone(),
            struct_metadata.struct_name.as_identifier().unwrap().name,
            method_name,
        );

        func_decl.renamed_as = Some(func_decl.name);
        func_decl.name = method_abi_name.clone();

        let func_linkage: Option<Linkage> = Some(self.build_func_linkage(func_def.access_specifier.clone()));

        let func_value = self
            .module
            .borrow_mut()
            .deref_mut()
            .add_function(&method_abi_name, fn_type, func_linkage);

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

        self.insert_local_ir_value(local_ir_value_id, LocalIRValue::Func(func_value));

        StructMethodMetadata {
            local_ir_value_id,
            method_decl: func_decl,
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
        struct_id: StructID,
        methods: Vec<FuncDef>,
    ) -> Vec<StructMethodMetadata<'ctx>> {
        let mut module_metadata = self.get_module_metadata_by_module_id(self.module_id).unwrap();
        let struct_metadata = module_metadata.get_struct_metadata_by_id(struct_id).clone();
        drop(module_metadata);

        let mut struct_methods: Vec<StructMethodMetadata<'ctx>> = Vec::new();
        for func_def in methods {
            let method_metadata = self.build_method_def(func_def, struct_metadata.clone());
            struct_methods.push(method_metadata);
        }
        struct_methods
    }

    pub(crate) fn build_struct_init(&mut self, scope: ScopeRef<'ctx>, struct_init: StructInit) -> InternalValue<'ctx> {
        let struct_metadata = match self.lookup_struct_by_name(
            struct_init.struct_name.clone(),
            struct_init.loc.clone(),
            struct_init.span.end,
        ) {
            Some(struct_metadata) => struct_metadata,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::IdentifierNotDefined(struct_init.struct_name.as_identifier().unwrap().name),
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

        if struct_metadata.fields.len() != struct_init.field_inits.len() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Struct '{}' has {} fields, but {} fields were provided.",
                    struct_init.struct_name.to_string(),
                    struct_metadata.fields.len(),
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

        let mut struct_value = struct_metadata.struct_type.get_undef();

        for field_init in struct_init.field_inits {
            let field_idx = match struct_metadata
                .fields
                .iter()
                .position(|field| field.name == field_init.name.clone())
            {
                Some(field_idx) => field_idx,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Field '{}' not found in struct '{}'.",
                            field_init.name,
                            module_segments_as_string(struct_init.struct_name.segments)
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

            let field = struct_metadata.fields.get(field_idx).unwrap();

            let field_type = self.build_type(field.ty.clone(), field.loc.clone(), field.span.end);

            let field_expr = self.build_expr(Rc::clone(&scope), field_init.value.clone());
            let field_rvalue = self.internal_value_as_rvalue(field_expr, field.loc.clone(), field.span.end);

            if !self.compatible_types(field_type.clone(), field_rvalue.get_type(self.context.i8_type())) {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Field {} of struct '{}' expects a value of type '{}', but received '{}'.",
                        field_idx,
                        module_segments_as_string(struct_init.struct_name.segments),
                        field_type,
                        field_rvalue.get_type(self.context.i8_type())
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

        InternalValue::StructValue(
            struct_value,
            InternalType::StructType(struct_metadata.to_internal_struct_type()),
        )
    }

    fn build_unnamed_struct_field_access(
        &self,
        internal_unnamed_struct_type: InternalUnnamedStructType<'ctx>,
        pointer: PointerValue<'ctx>,
        field_name: String,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        internal_unnamed_struct_type
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
            )
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
                let struct_metadata = match self.resolve_struct_metadata_with_struct_id(internal_struct_type.struct_id)
                {
                    Some(struct_metadata) => struct_metadata,
                    None => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(format!(
                                "Could'nt find any struct with struct name '{}' in the module metadata registry.",
                                module_segments_as_string(internal_struct_type.struct_name.segments.clone())
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
                };

                match struct_metadata
                    .fields
                    .iter()
                    .enumerate()
                    .find(|(_, f)| f.name == field_name)
                {
                    Some((field_idx, field)) => {
                        let field_ptr = self
                            .builder
                            .build_struct_gep(
                                struct_metadata.struct_type,
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
                                module_segments_as_string(struct_metadata.struct_name.segments),
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
            InternalType::UnnamedStruct(internal_unnamed_struct_type) => {
                self.build_unnamed_struct_field_access(internal_unnamed_struct_type, pointer, field_name, loc, span_end)
            }
            _ => unreachable!(),
        }
    }

    fn build_method_call_operand_as_rvalue(
        &mut self,
        scope: ScopeRef<'ctx>,
        method_call: MethodCall,
    ) -> Option<(StructMetadata<'ctx>, InternalValue<'ctx>, InternalValue<'ctx>)> {
        let lvalue = self.build_expr(Rc::clone(&scope), *method_call.operand.clone());
        let rvalue = self.internal_value_as_rvalue(lvalue.clone(), method_call.loc.clone(), method_call.span.end);
        let rvalue_type = rvalue.get_type(self.context.i8_type());

        let internal_struct_type = match rvalue_type {
            InternalType::StructType(internal_struct_type) => internal_struct_type,
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::MethodCallOnNonStructValue,
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: method_call.loc.line,
                        column: method_call.loc.column,
                        length: method_call.span.end,
                    }),
                });
                exit(1);
            }
        };

        let struct_id = internal_struct_type.struct_id;
        match self.resolve_struct_metadata_with_struct_id(struct_id) {
            Some(struct_metadata) => Some((struct_metadata, lvalue, rvalue)),
            None => None,
        }
    }

    pub(crate) fn build_method_call(&mut self, scope: ScopeRef<'ctx>, method_call: MethodCall) -> InternalValue<'ctx> {
        let lookup_with_identifier = |identifier: Identifier| -> Option<StructMetadata<'ctx>> {
            match self.lookup_struct_by_name(
                ModuleImport {
                    segments: vec![ModuleSegment::SubModule(identifier.clone())],
                    loc: identifier.loc.clone(),
                    span: identifier.span.clone(),
                },
                identifier.loc.clone(),
                identifier.span.end,
            ) {
                Some(struct_metadata) => Some(struct_metadata),
                None => None,
            }
        };

        let struct_metadata_opt = match *method_call.operand.clone() {
            Expression::Identifier(identifier) => lookup_with_identifier(identifier),
            Expression::ModuleImport(mut module_import) => {
                if let Some(identifier) = module_import.as_identifier() {
                    lookup_with_identifier(identifier)
                } else {
                    let struct_name = module_import.segments.pop().unwrap().as_identifier().name;

                    let module_id = match self.get_imported_module(module_import.segments.clone()) {
                        Some(imported_module) => imported_module.module_id,
                        None => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::ModuleNotFound(module_segments_as_string(module_import.segments)),
                                location: Some(DiagLoc {
                                    file: self.file_path.clone(),
                                    line: method_call.loc.line,
                                    column: method_call.loc.column,
                                    length: method_call.span.end,
                                }),
                            });
                            exit(1);
                        }
                    };

                    let mut module_metadata = self.get_module_metadata_by_module_id(module_id).unwrap();

                    let struct_metadata = match module_metadata.get_defined_type(struct_name.clone()) {
                        Some(internal_type) => match internal_type {
                            InternalType::StructType(internal_struct_type) => module_metadata
                                .get_struct_metadata_by_id(internal_struct_type.struct_id)
                                .clone(),
                            _ => {
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::SymbolIsNotAnStruct(
                                        struct_name,
                                        module_segments_as_string(module_import.segments),
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
                        None => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::SymbolNotFoundInModule(
                                    struct_name,
                                    module_segments_as_string(module_import.segments),
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
                    };

                    drop(module_metadata);
                    Some(struct_metadata)
                }
            }
            _ => None,
        };

        let get_method_metadata = |struct_metadata: StructMetadata<'ctx>, file_path: String| match struct_metadata
            .methods
            .iter()
            .find(|m| m.method_decl.get_usable_name() == method_call.method_name.name)
        {
            Some(method_metadata) => method_metadata.clone(),
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::MethodNotDefinedForStruct(
                        method_call.method_name.name.clone(),
                        module_segments_as_string(struct_metadata.struct_name.segments),
                    ),
                    location: Some(DiagLoc {
                        file: file_path,
                        line: method_call.loc.line,
                        column: method_call.loc.column,
                        length: method_call.span.end,
                    }),
                });
                exit(1);
            }
        };

        match struct_metadata_opt {
            Some(struct_metadata) => {
                // Static method call
                let method_metadata = get_method_metadata(struct_metadata, self.file_path.clone());

                if !method_metadata.is_static_method {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::MethodIsAnInstance(method_call.method_name.name.clone()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: method_call.loc.line,
                            column: method_call.loc.column,
                            length: method_call.span.end,
                        }),
                    });
                    exit(1);
                }

                self.build_static_method_call(Rc::clone(&scope), method_call.clone(), method_metadata)
            }
            None => match self.build_method_call_operand_as_rvalue(Rc::clone(&scope), method_call.clone()) {
                Some((struct_metadata, lvalue, rvalue)) => {
                    // Instance method call
                    let method_metadata = get_method_metadata(struct_metadata, self.file_path.clone());

                    if method_metadata.is_static_method {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::MethodIsStatic(method_call.method_name.name.clone()),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: method_call.loc.line,
                                column: method_call.loc.column,
                                length: method_call.span.end,
                            }),
                        });
                        exit(1);
                    }

                    self.build_instance_method_call(
                        Rc::clone(&scope),
                        lvalue,
                        rvalue,
                        method_metadata,
                        method_call.clone(),
                    )
                }
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::MethodCallOnNonStructValue,
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
        }
    }

    pub(crate) fn build_static_method_call(
        &mut self,
        scope: ScopeRef<'ctx>,
        method_call: MethodCall,
        method_metadata: StructMethodMetadata<'ctx>,
    ) -> InternalValue<'ctx> {
        if !method_metadata.is_static_method {
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

        let func_metadata = FuncMetadata {
            local_ir_value_id: method_metadata.local_ir_value_id,
            func_decl: method_metadata.method_decl.clone(),
            return_type: method_metadata.return_type.clone(),
            imported_from: None,
            params_metadata: method_metadata.method_params_metadata.clone(),
        };
        let method_value = self.get_or_declare_local_func_ir_value(method_metadata.local_ir_value_id, func_metadata);

        let func_basic_blocks = method_value.get_basic_blocks();
        let current_block = self.get_current_block("method call", method_call.loc.clone(), method_call.span.end);

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

        // static methods args count checking are the same as the funcs
        self.check_func_args_count_mismatch(
            method_metadata.method_decl.name.clone(),
            method_metadata.method_decl.clone(),
            method_call.arguments.len(),
            method_call.loc.clone(),
            method_call.span.end,
        );

        let static_params_length = method_metadata.method_params_metadata.param_types.len();

        let arguments = self.build_arguments(
            Rc::clone(&scope),
            method_call.arguments.clone(),
            method_metadata.method_params_metadata.clone(),
            0,
            static_params_length,
            method_metadata.method_decl.get_usable_name(),
            method_call.loc.clone(),
            method_call.span.end,
        );

        let call_site_value = self.builder.build_call(method_value, &arguments, "call").unwrap();

        if let Some(value) = call_site_value.try_as_basic_value().left() {
            self.new_internal_value(value, method_metadata.return_type.clone())
        } else {
            InternalValue::PointerValue(self.build_null())
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

    pub(crate) fn lookup_struct_by_name(
        &self,
        mut module_import: ModuleImport,
        loc: Location,
        span_end: usize,
    ) -> Option<StructMetadata<'ctx>> {
        if let Some(identifier) = module_import.as_identifier() {
            let module_metadata = self.get_module_metadata_by_module_id(self.module_id).unwrap();
            let struct_metadata = match module_metadata.get_struct_metadata_by_name(identifier.name.clone()) {
                Some(struct_metadata) => struct_metadata,
                None => {
                    return None;
                }
            };

            drop(module_metadata);
            Some(struct_metadata)
        } else {
            // lookup in imported structs
            let struct_name = module_import.segments.pop().unwrap().as_identifier().name;

            let module_id = match self.get_imported_module(module_import.segments.clone()) {
                Some(imported_module) => imported_module.module_id,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::ModuleNotFound(module_segments_as_string(module_import.segments.clone())),
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

            let module_metadata = self.get_module_metadata_by_module_id(module_id).unwrap();

            let internal_type = match module_metadata.get_defined_type(struct_name.clone()) {
                Some(internal_type) => internal_type,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::SymbolNotFoundInModule(
                            struct_name,
                            module_segments_as_string(module_import.segments.clone()),
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
            };

            drop(module_metadata);

            let struct_id = match internal_type {
                InternalType::StructType(internal_struct_type) => internal_struct_type.struct_id,
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::SymbolIsNotAnStruct(
                            struct_name,
                            module_segments_as_string(module_import.segments.clone()),
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
            };

            self.resolve_struct_metadata_with_struct_id(struct_id)
        }
    }

    pub(crate) fn build_instance_method_call(
        &mut self,
        scope: ScopeRef<'ctx>,
        mut operand: InternalValue<'ctx>,
        mut operand_rvalue: InternalValue<'ctx>,
        mut method_metadata: StructMethodMetadata<'ctx>,
        method_call: MethodCall,
    ) -> InternalValue<'ctx> {
        if method_call.is_fat_arrow {
            if !operand_rvalue.get_type(self.context.i8_type()).is_pointer_type() {
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

            // REVIEW
            operand = self.build_deref_internal(operand_rvalue.clone(), method_call.loc.clone(), method_call.span.end);
            operand_rvalue =
                self.internal_value_as_rvalue(operand.clone(), method_call.loc.clone(), method_call.span.end);
        }

        // Set ABI naming

        let func_metadata = FuncMetadata {
            local_ir_value_id: method_metadata.local_ir_value_id,
            func_decl: method_metadata.method_decl.clone(),
            return_type: method_metadata.return_type.clone(),
            imported_from: None,
            params_metadata: method_metadata.method_params_metadata.clone(),
        };
        let method_value = self.get_or_declare_local_func_ir_value(method_metadata.local_ir_value_id, func_metadata);

        let func_basic_blocks = method_value.get_basic_blocks();
        let current_block = self.get_current_block("method call", method_call.loc.clone(), method_call.span.end);

        if !func_basic_blocks.contains(&current_block)
            && method_metadata.method_decl.access_specifier != AccessSpecifier::Public
        {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Method '{}' is defined with internal access specifier for the struct and cannot be called directly from outside of the struct declaration.",
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

        // Pass self modifier value if exists
        if let Some(first_param) = method_metadata.method_decl.params.list.first() {
            if let FuncParamKind::SelfModifier(self_modifier) = first_param {
                match self_modifier {
                    SelfModifier::Copied => {
                        arguments.push(self.internal_value_to_basic_metadata(operand_rvalue));
                    }
                    SelfModifier::Referenced => {
                        arguments.push(self.internal_value_to_basic_metadata(operand));
                    }
                }
            }

            // Remove self modifier from normal params
            method_metadata.method_params_metadata.param_types.remove(0);
        }

        self.check_instance_method_args_count_mismatch(
            method_metadata.method_decl.get_usable_name().clone(),
            method_metadata.method_decl.clone(),
            method_call.arguments.len(),
            method_call.loc.clone(),
            method_call.span.end,
        );

        let static_params_length = method_metadata.method_params_metadata.param_types.len();

        if static_params_length >= 1 {
            arguments.append(&mut self.build_arguments(
                Rc::clone(&scope),
                method_call.arguments.clone(),
                method_metadata.method_params_metadata.clone(),
                // exclude self modifier
                0,
                static_params_length,
                method_metadata.method_decl.get_usable_name(),
                method_call.loc.clone(),
                method_call.span.end,
            ));
        }

        let call_site_value = self.builder.build_call(method_value, &arguments, "call").unwrap();

        if let Some(value) = call_site_value.try_as_basic_value().left() {
            self.new_internal_value(value, method_metadata.return_type.clone())
        } else {
            InternalValue::PointerValue(self.build_null())
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

            if !rvalue.get_type(self.context.i8_type()).is_pointer_type() {
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
            InternalValue::StructValue(_, internal_type) | InternalValue::UnnamedStructValue(_, internal_type) => {
                let pointer = match internal_value {
                    InternalValue::PointerValue(typed_pointer_value) => typed_pointer_value.ptr,
                    InternalValue::Lvalue(lvalue) => lvalue.ptr,
                    _ => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(format!(
                                "Invalid operand for field access. Expected a struct or reference/pointer, but got type '{}'.",
                                internal_value.get_type(self.context.i8_type())
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
                    internal_type,
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
                field_type = field_value.get_type(self.context.i8_type());
            }

            if !self.compatible_types(field_type.clone(), field_value.get_type(self.context.i8_type())) {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Rvalue with type '{}' is not compatible with field type '{}' for field '{}'.",
                        field_value.get_type(self.context.i8_type()),
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
            .map(|f| f.1.get_type(self.context.i8_type()).to_basic_type(ptr_type).unwrap())
            .collect();

        let struct_type = self.context.struct_type(&field_types, unnamed_struct_value.packed);
        let struct_alloca = self.builder.build_alloca(struct_type, "alloca").unwrap();

        for (idx, (_, field_value)) in field_values.iter().enumerate() {
            let field_gep = self
                .builder
                .build_struct_gep(struct_type, struct_alloca, idx.try_into().unwrap(), "gep")
                .unwrap();

            let field_basic_value: BasicValueEnum<'ctx> = self
                .internal_value_to_basic_metadata(field_value.clone())
                .try_into()
                .unwrap();
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
                    (field_name.clone(), field_value.get_type(self.context.i8_type()).clone())
                })
                .collect(),
        };

        InternalValue::UnnamedStructValue(
            struct_value,
            InternalType::UnnamedStruct(InternalUnnamedStructType {
                type_str: unnamed_struct_value.to_string(),
                unnamed_struct_metadata,
            }),
        )
    }
}

impl<'a> StructMetadata<'a> {
    pub fn as_internal_struct_type(&self) -> InternalStructType<'a> {
        InternalStructType {
            struct_id: self.struct_id,
            struct_name: self.struct_name.clone(),
            struct_type: self.struct_type.clone(),
            fields: self.fields.clone(),
            methods: self.methods.clone(),
            type_str: module_segments_as_string(self.struct_name.segments.clone()),
        }
    }
}
