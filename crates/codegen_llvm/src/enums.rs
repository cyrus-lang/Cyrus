use crate::{
    context::CodeGenLLVM,
    diag::*,
    modules::{LocalIRValue, LocalIRValueID, generate_local_ir_value_id},
    structs::UnnamedStructTypeMetadata,
};
use ast::ast::{AccessSpecifier, Enum, EnumField, Identifier};
use inkwell::{
    module::{self, Linkage},
    types::IntType,
    values::IntValue,
};
use rand::Rng;
use std::{collections::HashMap, process::exit};

pub type EnumID = u64;

#[derive(Debug, Clone, PartialEq)]
pub struct EnumMetadata<'a> {
    pub enum_id: EnumID,
    pub variants: Vec<(Identifier, EnumVariantMetadata<'a>)>,
    pub access_specifier: AccessSpecifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumIdentifierVariantMetadata {
    local_ir_value_id: LocalIRValueID,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumValuedVariantMetadata {
    local_ir_value_id: LocalIRValueID,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantMetadata<'a> {
    Identifier(EnumIdentifierVariantMetadata),
    Valued(EnumValuedVariantMetadata),
    Variant(UnnamedStructTypeMetadata<'a>),
}

pub type EnumTable<'a> = HashMap<String, EnumMetadata<'a>>;

impl<'ctx> CodeGenLLVM<'ctx> {
    fn build_enum_variant_int_type(&self) -> IntType<'ctx> {
        self.context.i32_type()
    }

    fn build_enum_variant_int_value(&self, value: u64) -> IntValue<'ctx> {
        self.context.i32_type().const_int(value, false)
    }

    fn build_enum_linkage(&self, access_specifier: AccessSpecifier) -> Linkage {
        match access_specifier {
            AccessSpecifier::Public => Linkage::External,
            AccessSpecifier::Internal => Linkage::Private,
            _ => unreachable!(),
        }
    }

    pub(crate) fn build_enum(&self, enum_statement: Enum) {
        if !matches!(
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
        let module_ref = self.module.borrow_mut();
        let mut enum_iota = 0;

        let mut enum_variants: Vec<(Identifier, EnumVariantMetadata)> = Vec::new();

        enum_statement.variants.iter().for_each(|variant| {
            let (identifier, enum_variant_metadata) = match variant {
                EnumField::Identifier(identifier) => {
                    let global_value = module_ref.add_global(
                        self.build_enum_variant_int_type(),
                        None,
                        &self.generate_enum_variant_abi_name(
                            self.module_name.clone(),
                            enum_name.clone(),
                            identifier.name.clone(),
                        ),
                    );

                    let local_ir_value_id = generate_local_ir_value_id();
                    let variant_value = self.build_enum_variant_int_value(enum_iota);

                    global_value.set_initializer(&variant_value);
                    global_value.set_constant(true);
                    global_value.set_linkage(self.build_enum_linkage(enum_statement.access_specifier.clone()));
                    global_value.set_alignment(1);

                    self.insert_local_ir_value(local_ir_value_id, LocalIRValue::GlobalValue(global_value));

                    enum_iota += 1;

                    (
                        identifier,
                        EnumVariantMetadata::Identifier(EnumIdentifierVariantMetadata { local_ir_value_id }),
                    )
                }
                EnumField::Valued(identifier, expression) => todo!(),
                EnumField::Variant(identifier, enum_valued_fields) => todo!(),
            };

            enum_variants.push((identifier.clone(), enum_variant_metadata));
        });

        let mut module_metadata = self.get_module_metadata_by_module_id(self.module_id).unwrap();
        module_metadata.insert_enum(
            enum_name,
            EnumMetadata {
                enum_id: generate_enum_id(),
                variants: enum_variants,
                access_specifier: enum_statement.access_specifier,
            },
        );
        drop(module_metadata);
    }
}

fn generate_enum_id() -> EnumID {
    let mut rng = rand::rng();
    rng.random::<u64>()
}
