use crate::{
    context::CodeGenLLVM,
    diag::*,
    modules::{LocalIRValue, LocalIRValueID, generate_local_ir_value_id},
    scope::ScopeRef,
    structs::UnnamedStructTypeMetadata,
    types::{InternalEnumType, InternalIntType, InternalType},
    values::InternalValue,
};
use ast::{
    ast::{AccessSpecifier, Enum, EnumField, FieldAccess, Identifier, MethodCall},
    token::{Location, TokenKind},
};
use inkwell::{
    AddressSpace,
    module::Linkage,
    types::{ArrayType, BasicTypeEnum},
    values::{ArrayValue, BasicValueEnum, StructValue},
};
use rand::Rng;
use std::{collections::HashMap, process::exit, rc::Rc};

pub type EnumID = u64;

#[derive(Debug, Clone, PartialEq)]
pub struct EnumMetadata<'a> {
    pub enum_id: EnumID,
    pub enum_name: String,
    pub variants: Vec<(Identifier, EnumVariantMetadata<'a>)>,
    pub internal_type: InternalType<'a>,
    pub access_specifier: AccessSpecifier,
    payload_type: EnumPayloadType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumIdentifierVariantMetadata {
    variant_number: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantFieldMetadata<'a> {
    variant_number: u32,
    unnamed_struct_type: UnnamedStructTypeMetadata<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumValuedVariantMetadata<'a> {
    variant_number: u32,
    pub value_type: InternalType<'a>,
    pub local_ir_value_id: LocalIRValueID,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantMetadata<'a> {
    Identifier(EnumIdentifierVariantMetadata),
    Valued(EnumValuedVariantMetadata<'a>),
    Variant(EnumVariantFieldMetadata<'a>),
}

pub type EnumTable<'a> = HashMap<String, EnumMetadata<'a>>;
pub type EnumPayloadType<'a> = ArrayType<'a>;
pub type EnumPayloadValue<'a> = ArrayValue<'a>;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn buld_enum_variant_internal_field(
        &self,
        field_name: String,
        struct_value: StructValue<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        match &*field_name {
            "index" => self.build_enum_extract_index(struct_value),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::FieldNotFoundForEnumVariant(field_name),
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

    pub(crate) fn buld_enum_variant_instance_internal_field(
        &self,
        field_name: String,
        struct_value: StructValue<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        match &*field_name {
            "index" => self.build_enum_extract_index(struct_value),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::FieldNotFoundForEnumVariant(field_name),
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

    pub(crate) fn build_enum_extract_index(&self, struct_value: StructValue<'ctx>) -> InternalValue<'ctx> {
        let index_value = self
            .builder
            .build_extract_value(struct_value, 0, "extract")
            .unwrap()
            .into_int_value();
        InternalValue::IntValue(
            index_value,
            InternalType::IntType(InternalIntType {
                type_str: "uint32".to_string(),
                int_kind: TokenKind::UInt32,
                int_type: index_value.get_type(),
            }),
        )
    }

    pub(crate) fn build_enum(&mut self, enum_statement: Enum) {
        if enum_statement.access_specifier == AccessSpecifier::Extern {
            self.build_c_enum(enum_statement);
            return;
        } else if !matches!(
            enum_statement.access_specifier,
            AccessSpecifier::Public | AccessSpecifier::Internal
        ) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::InvalidEnumAccessSpecifier,
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: enum_statement.loc.line,
                    column: enum_statement.loc.column,
                    length: enum_statement.span.end,
                }),
            });
            exit(1);
        }

        let enum_name = enum_statement.identifier.name.clone();
        let enum_opaque_struct = self
            .context
            .opaque_struct_type(&self.generate_enum_abi_name(self.module_name.clone(), enum_name.clone()));

        let mut enum_iota = 0;

        // Determine payload size
        let mut payload_size = 0;
        let mut variants: Vec<(Identifier, EnumVariantMetadata<'ctx>)> = Vec::new();

        enum_statement.variants.iter().for_each(|variant| {
            let (identifier, enum_variant_metadata) = match variant {
                EnumField::Identifier(identifier) => {
                    // Does not affect payload size.
                    (
                        identifier,
                        EnumVariantMetadata::Identifier(EnumIdentifierVariantMetadata {
                            variant_number: enum_iota,
                        }),
                    )
                }
                EnumField::Valued(identifier, expression) => {
                    match self.build_unscoped_expression(*expression.clone()) {
                        Some(internal_value) => {
                            let internal_type = internal_value.get_type();
                            let store_size = self.get_internal_type_store_size(internal_type.clone());

                            if store_size > payload_size {
                                payload_size = store_size;
                            }

                            let basic_value: BasicValueEnum<'ctx> = self
                                .internal_value_to_basic_metadata(internal_value)
                                .try_into()
                                .unwrap();

                            let basic_type =
                                match internal_type.to_basic_type(self.context.ptr_type(AddressSpace::default())) {
                                    Ok(basic_type) => basic_type,
                                    Err(err) => {
                                        display_single_diag(Diag {
                                            level: DiagLevel::Error,
                                            kind: DiagKind::Custom(err.to_string()),
                                            location: Some(DiagLoc {
                                                file: self.file_path.clone(),
                                                line: identifier.loc.line,
                                                column: identifier.loc.column,
                                                length: identifier.span.end,
                                            }),
                                        });
                                        exit(1);
                                    }
                                };

                            let local_ir_value_id = generate_local_ir_value_id();
                            let module = self.module.borrow_mut();
                            let global_value = module.add_global(
                                basic_type,
                                None,
                                &self.generate_enum_variant_abi_name(
                                    self.module_name.clone(),
                                    enum_name.clone(),
                                    identifier.name.clone(),
                                ),
                            );
                            global_value.set_linkage(Linkage::External);
                            global_value.set_initializer(&basic_value);
                            self.insert_local_ir_value(local_ir_value_id, LocalIRValue::GlobalValue(global_value));
                            drop(module);

                            (
                                identifier,
                                EnumVariantMetadata::Valued(EnumValuedVariantMetadata {
                                    variant_number: enum_iota,
                                    value_type: internal_type.clone(),
                                    local_ir_value_id,
                                }),
                            )
                        }
                        None => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::MustBeComptimeExpr,
                                location: Some(DiagLoc {
                                    file: self.file_path.clone(),
                                    line: identifier.loc.line,
                                    column: identifier.loc.column,
                                    length: identifier.span.end,
                                }),
                            });
                            exit(1);
                        }
                    }
                }
                EnumField::Variant(identifier, enum_valued_fields) => {
                    let mut fields_metadata: Vec<(String, InternalType<'ctx>)> = Vec::new();

                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let variant_fields: Vec<BasicTypeEnum<'ctx>> = enum_valued_fields
                        .iter()
                        .map(|enum_valued_field| {
                            let internal_type = self.build_type(
                                enum_valued_field.field_type.clone(),
                                enum_valued_field.identifier.loc.clone(),
                                enum_valued_field.identifier.span.end,
                            );

                            fields_metadata.push((enum_valued_field.identifier.name.clone(), internal_type.clone()));

                            match internal_type.to_basic_type(ptr_type) {
                                Ok(basic_type) => basic_type,
                                Err(err) => {
                                    display_single_diag(Diag {
                                        level: DiagLevel::Error,
                                        kind: DiagKind::Custom(err.to_string()),
                                        location: Some(DiagLoc {
                                            file: self.file_path.clone(),
                                            line: identifier.loc.line,
                                            column: identifier.loc.column,
                                            length: identifier.span.end,
                                        }),
                                    });
                                    exit(1);
                                }
                            }
                        })
                        .collect();

                    let struct_type = self.context.struct_type(&variant_fields, false);
                    let store_size = self.get_struct_store_size(struct_type);
                    if store_size > payload_size {
                        payload_size = store_size;
                    }

                    (
                        identifier,
                        EnumVariantMetadata::Variant(EnumVariantFieldMetadata {
                            unnamed_struct_type: UnnamedStructTypeMetadata {
                                struct_type,
                                fields: fields_metadata,
                            },
                            variant_number: enum_iota,
                        }),
                    )
                }
            };

            variants.push((identifier.clone(), enum_variant_metadata));
            enum_iota += 1;
        });

        let payload_type = self.context.i8_type().array_type(payload_size.try_into().unwrap());
        let field_types: &[BasicTypeEnum<'ctx>; 2] = &[
            BasicTypeEnum::IntType(self.context.i32_type()),
            BasicTypeEnum::ArrayType(payload_type),
        ];
        enum_opaque_struct.set_body(field_types, false);

        let enum_id = generate_enum_id();

        let enum_internal_type = InternalType::EnumType(InternalEnumType {
            enum_id,
            enum_type: enum_opaque_struct,
            type_str: enum_statement.identifier.name.clone(),
        });

        let enum_metadata = EnumMetadata {
            enum_id,
            variants,
            payload_type,
            enum_name: enum_name.clone(),
            internal_type: enum_internal_type,
            access_specifier: enum_statement.access_specifier,
        };

        let mut module_metadata = self.get_module_metadata_by_module_id(self.module_id).unwrap();
        module_metadata.insert_enum(enum_name, enum_metadata);
        drop(module_metadata);
    }

    fn build_c_enum(&self, enum_statement: Enum) {
        // Build C-ABI compatible enum statement.
        todo!();
    }

    pub(crate) fn build_construct_enum(
        &self,
        enum_metadata: EnumMetadata<'ctx>,
        variant_number: u32,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        let enum_variant_metadata = match enum_metadata.resolve_enum_variant_metadata(variant_number) {
            Some((_, enum_variant_metadata)) => enum_variant_metadata,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "The enum '{}' does not contain variant with number '{}'.",
                        enum_metadata.enum_name, variant_number
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

        let internal_enum_type = match enum_metadata.internal_type.clone() {
            InternalType::EnumType(internal_enum_type) => internal_enum_type,
            _ => unreachable!(),
        };

        let mut struct_value = internal_enum_type.enum_type.get_undef();
        let payload_value = match enum_variant_metadata.clone() {
            EnumVariantMetadata::Identifier(m) => m.get_payload(enum_metadata.payload_type),
            EnumVariantMetadata::Valued(m) => m.get_payload(enum_metadata.payload_type),
            EnumVariantMetadata::Variant(..) => unreachable!(),
        };
        let variant_number = self
            .context
            .i32_type()
            .const_int(enum_variant_metadata.get_variant_number().into(), false);

        struct_value = self
            .builder
            .build_insert_value(struct_value, variant_number, 0, "set")
            .unwrap()
            .into_struct_value();

        struct_value = self
            .builder
            .build_insert_value(struct_value, payload_value, 1, "set")
            .unwrap()
            .into_struct_value();

        InternalValue::EnumVariantValue(struct_value, enum_metadata.internal_type)
    }

    pub(crate) fn build_enum_variant_value(
        &mut self,
        scope: ScopeRef<'ctx>,
        enum_metadata: EnumMetadata<'ctx>,
        method_call: MethodCall,
    ) -> InternalValue<'ctx> {
        let enum_variant_metadata =
            match enum_metadata.resolve_enum_variant_metadata_with_name(method_call.method_name.name.clone()) {
                Some((_, enum_variant_metadata)) => enum_variant_metadata,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::EnumVariantNotDefined(enum_metadata.enum_name, method_call.method_name.name),
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

        let enum_variant_field_metadata = match enum_variant_metadata {
            EnumVariantMetadata::Variant(ref enum_variant_field_metadata) => enum_variant_field_metadata,
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "The enum variant '{}.{}' does not take any arguments. Values cannot be provided when accessing this variant.",
                        enum_metadata.enum_name, method_call.method_name.name
                    )),
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

        if enum_variant_field_metadata.unnamed_struct_type.fields.len() != method_call.arguments.len() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "The enum variant '{}.{}' expects {} arguments, but {} were provided.",
                    enum_metadata.enum_name,
                    method_call.method_name.name,
                    enum_variant_field_metadata.unnamed_struct_type.fields.len(),
                    method_call.arguments.len()
                )),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: method_call.loc.line,
                    column: method_call.loc.column,
                    length: method_call.span.end,
                }),
            });
            exit(1);
        }

        let mut basic_values: Vec<BasicValueEnum<'ctx>> = Vec::new();

        for (idx, expr) in method_call.arguments.iter().enumerate() {
            let lvalue = self.build_expr(Rc::clone(&scope), expr.clone());
            let rvalue = self.internal_value_as_rvalue(lvalue, method_call.loc.clone(), method_call.span.end);
            let rvalue_type = rvalue.get_type();

            let (field_name, field_type) = enum_variant_field_metadata
                .unnamed_struct_type
                .fields
                .get(idx)
                .unwrap()
                .clone();

            if !self.compatible_types(field_type.clone(), rvalue_type.clone()) {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Rvalue with type '{}' is not compatible with field type '{}' for field '{}'.",
                        rvalue_type, field_type, field_name
                    )),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: method_call.loc.line,
                        column: method_call.loc.column,
                        length: method_call.span.end,
                    }),
                });
                exit(1);
            }

            basic_values.push(self.internal_value_to_basic_metadata(rvalue).try_into().unwrap());
        }

        let internal_enum_type = match enum_metadata.internal_type.clone() {
            InternalType::EnumType(internal_enum_type) => internal_enum_type,
            _ => unreachable!(),
        };

        let mut enum_value = internal_enum_type.enum_type.get_undef();
        let variant_number = self
            .context
            .i32_type()
            .const_int(enum_variant_metadata.get_variant_number().into(), false);

        enum_value = self
            .builder
            .build_insert_value(enum_value, variant_number, 0, "set")
            .unwrap()
            .into_struct_value();

        // Copy the StructValue into buffer using by memcpy intrinsic function.
        let struct_alignment = self
            .target_machine
            .get_target_data()
            .get_abi_alignment(&enum_variant_field_metadata.unnamed_struct_type.struct_type);

        let payload_struct_value = enum_variant_field_metadata
            .unnamed_struct_type
            .struct_type
            .const_named_struct(&basic_values);

        let payload_alloca = self
            .builder
            .build_alloca(
                enum_variant_field_metadata.unnamed_struct_type.struct_type,
                "payload_alloca",
            )
            .unwrap();
        self.builder.build_store(payload_alloca, payload_struct_value).unwrap();

        let buffer_size = self.context.i64_type().const_int(
            self.get_struct_store_size(enum_variant_field_metadata.unnamed_struct_type.struct_type),
            false,
        );

        let payload_buffer = self
            .builder
            .build_alloca(
                self.context
                    .i8_type()
                    .array_type(buffer_size.get_zero_extended_constant().unwrap().try_into().unwrap()),
                "payload_buffer",
            )
            .unwrap();

        let src_ptr = self
            .builder
            .build_bit_cast(
                payload_alloca,
                self.context.ptr_type(AddressSpace::default()),
                "src_ptr",
            )
            .unwrap()
            .into_pointer_value();

        let dest_ptr = self
            .builder
            .build_bit_cast(
                payload_buffer,
                self.context.ptr_type(AddressSpace::default()),
                "dest_ptr",
            )
            .unwrap()
            .into_pointer_value();

        self.builder
            .build_memcpy(dest_ptr, struct_alignment, src_ptr, struct_alignment, buffer_size)
            .unwrap();

        let buffer_value = self
            .builder
            .build_load(
                self.context
                    .i8_type()
                    .array_type(buffer_size.get_zero_extended_constant().unwrap().try_into().unwrap()),
                payload_buffer,
                "load_buffer",
            )
            .unwrap()
            .into_array_value();

        enum_value = self
            .builder
            .build_insert_value(enum_value, buffer_value, 1, "insert_payload")
            .unwrap()
            .into_struct_value();

        InternalValue::EnumVariantValue(enum_value, enum_metadata.internal_type)
    }

    pub(crate) fn build_enum_value(
        &self,
        enum_metadata: EnumMetadata<'ctx>,
        field_access: FieldAccess,
    ) -> InternalValue<'ctx> {
        if field_access.is_fat_arrow {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(
                    "Cannot access enum value with '->'. Enum members must be accessed using '.' notation.".to_string(),
                ),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: field_access.loc.line,
                    column: field_access.loc.column,
                    length: field_access.span.end,
                }),
            });
            exit(1);
        }

        let variant_number = match enum_metadata
            .resolve_enum_variant_metadata_with_name(field_access.field_name.name.clone())
        {
            Some((_, enum_variant_metadata)) => match enum_variant_metadata {
                EnumVariantMetadata::Variant(_) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "The enum variant '{}.{}' is a constructor that requires values to be provided. It cannot be used as a direct value.",
                            enum_metadata.enum_name, field_access.field_name.name
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
                _ => enum_variant_metadata.get_variant_number(),
            },
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::EnumVariantNotDefined(enum_metadata.enum_name, field_access.field_name.name),
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

        self.build_construct_enum(
            enum_metadata,
            variant_number,
            field_access.loc.clone(),
            field_access.span.end,
        )
    }
}

impl EnumIdentifierVariantMetadata {
    pub fn get_payload<'a>(&self, payload_type: EnumPayloadType<'a>) -> EnumPayloadValue<'a> {
        payload_type.const_zero()
    }
}

impl<'a> EnumValuedVariantMetadata<'a> {
    pub fn get_payload(&self, payload_type: EnumPayloadType<'a>) -> EnumPayloadValue<'a> {
        payload_type.const_zero()
    }
}

impl<'a> EnumVariantMetadata<'a> {
    pub fn get_variant_number(&self) -> u32 {
        match self {
            EnumVariantMetadata::Identifier(m) => m.variant_number,
            EnumVariantMetadata::Valued(m) => m.variant_number,
            EnumVariantMetadata::Variant(m) => m.variant_number,
        }
    }
}

impl<'a> EnumMetadata<'a> {
    pub fn resolve_enum_variant_metadata(&self, variant_number: u32) -> Option<(Identifier, EnumVariantMetadata<'a>)> {
        self.variants
            .iter()
            .find(|v| match &v.1 {
                EnumVariantMetadata::Identifier(m) => m.variant_number == variant_number,
                EnumVariantMetadata::Valued(m) => m.variant_number == variant_number,
                EnumVariantMetadata::Variant(m) => m.variant_number == variant_number,
            })
            .cloned()
    }

    pub fn resolve_enum_variant_metadata_with_name(
        &self,
        name: String,
    ) -> Option<(Identifier, EnumVariantMetadata<'a>)> {
        self.variants.iter().find(|(v, ..)| v.name == name).cloned()
    }
}

fn generate_enum_id() -> EnumID {
    let mut rng = rand::rng();
    rng.random::<u64>()
}
