use crate::{
    context::CodeGenLLVM,
    diag::*,
    modules::{LocalIRValueID, generate_local_ir_value_id},
    structs::UnnamedStructTypeMetadata,
    types::{InternalEnumType, InternalType},
    values::InternalValue,
};
use ast::ast::{AccessSpecifier, Enum, EnumField, FieldAccess, Identifier};
use inkwell::{
    AddressSpace,
    types::{ArrayType, BasicTypeEnum},
    values::{ArrayValue, BasicValueEnum},
};
use rand::Rng;
use std::{collections::HashMap, process::exit};

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
pub struct EnumValuedVariantMetadata {
    variant_number: u32,
    local_ir_value_id: LocalIRValueID,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantMetadata<'a> {
    Identifier(EnumIdentifierVariantMetadata),
    Valued(EnumValuedVariantMetadata),
    Variant(EnumVariantFieldMetadata<'a>),
}

pub type EnumTable<'a> = HashMap<String, EnumMetadata<'a>>;
pub type EnumPayloadType<'a> = ArrayType<'a>;
pub type EnumPayloadValue<'a> = ArrayValue<'a>;

impl<'ctx> CodeGenLLVM<'ctx> {
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
                            let store_size = self.get_internal_type_store_size(internal_type);

                            if store_size > payload_size {
                                payload_size = store_size;
                            }

                            (
                                identifier,
                                EnumVariantMetadata::Valued(EnumValuedVariantMetadata {
                                    variant_number: enum_iota,
                                    local_ir_value_id: generate_local_ir_value_id(),
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
    ) -> InternalValue<'ctx> {
        let variant_metadata = enum_metadata.resolve_enum_variant_metadata(variant_number).unwrap().1;
        let payload_value = variant_metadata.get_payload(enum_metadata.payload_type);

        let internal_enum_type = match enum_metadata.internal_type.clone() {
            InternalType::EnumType(internal_enum_type) => internal_enum_type,
            _ => unreachable!(),
        };

        let struct_value = internal_enum_type.enum_type.const_named_struct(&[
            BasicValueEnum::IntValue(self.context.i32_type().const_int(variant_number.into(), false)),
            BasicValueEnum::ArrayValue(payload_value),
        ]);

        InternalValue::EnumVariantValue(struct_value, enum_metadata.internal_type.clone())
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

        let (identifier, enum_variant_metadata) =
            match enum_metadata.resolve_enum_variant_metadata_with_name(field_access.field_name.name.clone()) {
                Some(v) => v,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "The enum '{}' does not contain a variant named '{}'.",
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
            };

        let internal_enum_type = match enum_metadata.internal_type.clone() {
            InternalType::EnumType(internal_enum_type) => internal_enum_type,
            _ => unreachable!(),
        };

        let mut struct_value = internal_enum_type.enum_type.get_undef();
        let payload_value = enum_variant_metadata.get_payload(enum_metadata.payload_type);
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
}

impl EnumIdentifierVariantMetadata {
    pub fn get_payload<'a>(&self, payload_type: EnumPayloadType<'a>) -> EnumPayloadValue<'a> {
        payload_type.const_zero()
    }
}

impl EnumValuedVariantMetadata {
    pub fn get_payload<'a>(&self, payload_type: EnumPayloadType<'a>) -> EnumPayloadValue<'a> {
        todo!();
    }
}

impl<'a> EnumVariantFieldMetadata<'a> {
    pub fn get_payload(&self, payload_type: EnumPayloadType<'a>) -> EnumPayloadValue<'a> {
        todo!();
    }
}

impl<'a> EnumVariantMetadata<'a> {
    pub fn get_payload(&self, payload_type: EnumPayloadType<'a>) -> EnumPayloadValue<'a> {
        match self {
            EnumVariantMetadata::Identifier(m) => m.get_payload(payload_type),
            EnumVariantMetadata::Valued(m) => m.get_payload(payload_type),
            EnumVariantMetadata::Variant(m) => m.get_payload(payload_type),
        }
    }

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
