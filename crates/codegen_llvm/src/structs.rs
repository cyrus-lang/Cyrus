use crate::{
    InternalValue, CodeGenLLVM,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    scope::ScopeRef,
};
use ast::{
    ast::{Field, Identifier, ModuleImport, StorageClass, Struct, StructInit},
    token::Location,
};
use inkwell::{
    types::{BasicTypeEnum, StructType},
    values::{AggregateValueEnum, BasicValueEnum}, AddressSpace,
};
use std::{collections::HashMap, process::exit, rc::Rc};

#[derive(Debug, Clone)]
pub struct StructMetadata<'a> {
    pub struct_type: StructType<'a>,
    pub inherits: Vec<Identifier>,
    pub fields: Vec<Field>,
    pub storage_class: StorageClass,
}

pub type StructTable<'a> = HashMap<String, StructMetadata<'a>>;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_struct_fields(&self, fields: Vec<Field>) -> Vec<BasicTypeEnum> {
        fields
            .iter()
            .map(|field| {
                self.build_type(field.ty.clone(), field.loc.clone(), field.span.end)
                    .to_basic_type(self.context.ptr_type(AddressSpace::default()))
            })
            .collect()
    }

    pub(crate) fn build_struct(&self, struct_statement: Struct) -> StructType<'ctx> {
        let field_types = self.build_struct_fields(struct_statement.fields.clone());
        let struct_type = self.context.struct_type(&field_types, false);
        if !matches!(struct_statement.storage_class, StorageClass::Public | StorageClass::Internal) {
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
        struct_type
    }

    pub(crate) fn find_struct_by_type(
        &self,
        struct_type: StructType<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> (String, StructMetadata<'ctx>) {
        let mut record: Option<(String, StructMetadata<'ctx>)> = None;

        for (_, (name, metadata)) in self.struct_table.iter().enumerate() {
            if metadata.struct_type == struct_type {
                record = Some((name.clone(), metadata.clone()));
                break;
            }
        }

        if record.is_none() {
            for loaded_module in self.loaded_modules.clone() {
                for (_, (name, metadata)) in loaded_module.imported_structs.iter().enumerate() {
                    if metadata.struct_type == struct_type {
                        record = Some((
                            name.clone(),
                            StructMetadata {
                                struct_type,
                                inherits: metadata.inherits.clone(),
                                fields: metadata.fields.clone(),
                                storage_class: metadata.storage_class.clone(),
                            },
                        ));
                        break;
                    }
                }
            }
        }

        if let Some((struct_name, struct_metadata)) = record {
            (struct_name, struct_metadata)
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Unknown struct type.".to_string()),
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

    pub(crate) fn find_struct(
        &self,
        scope: ScopeRef<'ctx>,
        struct_name: ModuleImport,
        loc: Location,
        span_end: usize,
    ) -> StructMetadata<'ctx> {
        // FIXME
        todo!();

        // lookup in locals
        // match self
        //     .struct_table
        //     .get(&struct_name.segments.first().unwrap().to_string())
        // {
        //     Some(metadata) => metadata.clone(),
        //     None => {
        //         // lookup in loaded modules
        //         let (internal_value, _) = self.build_module_import(Rc::clone(&scope), struct_name.clone());
        //         match internal_value {
        //             InternalValue::ModuleValue(imported_module_value) => {
        //                 match struct_name.segments.last().unwrap() {
        //                     ast::ast::ModuleSegment::SubModule(identifier) => {
        //                         match imported_module_value.metadata.imported_structs.get(&identifier.name) {
        //                             Some(struct_metadata) => StructMetadata {
        //                                 struct_type: struct_metadata.struct_type.clone(),
        //                                 inherits: struct_metadata.inherits.clone(),
        //                                 fields: struct_metadata.fields.clone(),
        //                                 storage_class: struct_metadata.storage_class.clone(),
        //                             },
        //                             None => {
        //                                 display_single_diag(Diag {
        //                                     level: DiagLevel::Error,
        //                                     kind: DiagKind::Custom(format!(
        //                                         "Struct '{}' not defined.",
        //                                         struct_name.to_string()
        //                                     )),
        //                                     location: Some(DiagLoc {
        //                                         file: self.file_path.clone(),
        //                                         line: loc.line,
        //                                         column: loc.column,
        //                                         length: span_end,
        //                                     }),
        //                                 });
        //                                 exit(1);
        //                             }
        //                         }
        //                     }
        //                 }
        //             }
        //             _ => {
        //                 display_single_diag(Diag {
        //                     level: DiagLevel::Error,
        //                     kind: DiagKind::Custom(
        //                         "Expected ImportedModuleValue as result of build_module_import but something else."
        //                             .to_string(),
        //                     ),
        //                     location: Some(DiagLoc {
        //                         file: self.file_path.clone(),
        //                         line: loc.line,
        //                         column: loc.column,
        //                         length: span_end,
        //                     }),
        //                 });
        //                 exit(1);
        //             }
        //         }
        //     }
        // }
    }

    pub(crate) fn build_struct_init(&self, scope: ScopeRef<'ctx>, struct_init: StructInit) -> InternalValue<'ctx> {
        // FIXME
        todo!();

        // let struct_metadata = self.find_struct(
        //     Rc::clone(&scope),
        //     struct_init.struct_name.clone(),
        //     struct_init.loc.clone(),
        //     struct_init.span.end,
        // );

        // if struct_metadata.fields.len() != struct_init.field_inits.len() {
        //     display_single_diag(Diag {
        //         level: DiagLevel::Error,
        //         kind: DiagKind::Custom(format!(
        //             "Struct '{}' has {} fields, but {} fields were provided.",
        //             struct_init.struct_name.to_string(),
        //             struct_metadata.fields.len(),
        //             struct_init.field_inits.len()
        //         )),
        //         location: Some(DiagLoc {
        //             file: self.file_path.clone(),
        //             line: struct_init.loc.line,
        //             column: struct_init.loc.column,
        //             length: struct_init.span.end,
        //         }),
        //     });
        //     exit(1);
        // }

        // let mut struct_value = struct_metadata.struct_type.get_undef();

        // for field_init in struct_init.field_inits {
        //     let field_idx = struct_metadata
        //         .fields
        //         .iter()
        //         .position(|field| field.name == field_init.name)
        //         .unwrap();

        //     let field = struct_metadata.fields.get(field_idx).unwrap();

        //     let field_type = self
        //         .build_type(field.ty.clone(), field.loc.clone(), field.span.end)
        //         .to_basic_type(self.context.ptr_type(AddressSpace::default()));

        //     let field_any_value = self.build_expr(Rc::clone(&scope), field_init.value.clone());

        //     if field_any_value.get_type(self.string_type.clone()).to_basic_type(self.context.ptr_type(AddressSpace::default())) != field_type {
        //         display_single_diag(Diag {
        //             level: DiagLevel::Error,
        //             kind: DiagKind::Custom(format!(
        //                 "Expected type '{}' but got type '{}'.",
        //                 field_type.to_string(),
        //                 field_any_value.get_type(self.string_type.clone()).to_string()
        //             )),
        //             location: Some(DiagLoc {
        //                 file: self.file_path.clone(),
        //                 line: field_init.loc.line,
        //                 column: field_init.loc.column,
        //                 length: struct_init.span.end,
        //             }),
        //         });
        //         exit(1);
        //     }

        //     let field_value: BasicValueEnum<'ctx> = field_any_value.try_into().unwrap();

        //     struct_value = self
        //         .builder
        //         .build_insert_value(
        //             AggregateValueEnum::StructValue(struct_value),
        //             field_value,
        //             field_idx.try_into().unwrap(),
        //             "insert_data",
        //         )
        //         .unwrap()
        //         .into_struct_value();
        // }

        // InternalValue::StructValue(struct_value)
    }
}
