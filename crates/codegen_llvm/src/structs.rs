use crate::{
    AnyValue, CodeGenLLVM,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    scope::ScopeRef,
};
use ast::ast::{Field, Struct, StructInit, VisType};
use inkwell::{
    types::{BasicTypeEnum, StructType},
    values::{AsValueRef, StructValue},
};
use std::{collections::HashMap, process::exit, rc::Rc};

pub struct StructMetadata<'a> {
    pub struct_type: StructType<'a>,
    pub fields: Vec<Field>,
    pub vis_type: VisType,
}

pub type StructTable<'a> = HashMap<String, StructMetadata<'a>>;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_struct_fields(&self, fields: Vec<Field>) -> Vec<BasicTypeEnum> {
        fields
            .iter()
            .map(|field| {
                self.build_type(field.ty.clone(), field.loc.clone(), field.span.end)
                    .to_basic_type()
            })
            .collect()
    }

    pub(crate) fn build_struct(&mut self, struct_statement: Struct) {
        let field_types = self.build_struct_fields(struct_statement.fields.clone());
        let struct_type = self.context.struct_type(&field_types, false);
        if !matches!(struct_statement.vis_type, VisType::Pub | VisType::Internal) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Struct definition can only be public or internal".to_string()),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: struct_statement.loc.line,
                    column: struct_statement.loc.column,
                    length: struct_statement.span.end,
                }),
            });
            exit(1);
        }
        self.struct_table.insert(
            struct_statement.name,
            StructMetadata {
                struct_type,
                fields: struct_statement.fields,
                vis_type: struct_statement.vis_type,
            },
        );
    }

    // FIXME
    pub(crate) fn build_struct_init(&self, scope: ScopeRef<'ctx>, struct_init: StructInit) -> AnyValue<'ctx> {
        todo!()
        // if struct_init.struct_name.sub_modules.len() == 0 {
        //     if let Some(struct_def) = self.struct_table.get(&struct_init.struct_name.identifier.name) {
        //         if struct_def.fields.len() != struct_init.field_inits.len() {
        //             display_single_diag(Diag {
        //                 level: DiagLevel::Error,
        //                 kind: DiagKind::Custom(format!(
        //                     "Struct '{}' has {} fields, but {} fields were provided.",
        //                     struct_init.struct_name.to_string(),
        //                     struct_def.fields.len(),
        //                     struct_init.field_inits.len()
        //                 )),
        //                 location: Some(DiagLoc {
        //                     file: self.file_path.clone(),
        //                     line: struct_init.loc.line,
        //                     column: struct_init.loc.column,
        //                     length: struct_init.span.end,
        //                 }),
        //             });
        //             exit(1);
        //         }

        //         let struct_ptr = self
        //             .builder
        //             .build_alloca(struct_def.struct_type, "struct_init")
        //             .unwrap();

        //         for field_init in struct_init.field_inits {
        //             let field_idx = struct_def
        //                 .fields
        //                 .iter()
        //                 .position(|field| field.name == field_init.name)
        //                 .unwrap();

        //             let field = struct_def.fields.get(field_idx).unwrap();

        //             let field_type = self
        //                 .build_type(field.ty.clone(), field.loc.clone(), field.span.end)
        //                 .to_basic_type();

        //             let field_value = self.build_expr(Rc::clone(&scope), field_init.value.clone());

        //             if field_value.get_type(self.string_type.clone()).to_basic_type() != field_type {
        //                 display_single_diag(Diag {
        //                     level: DiagLevel::Error,
        //                     kind: DiagKind::Custom(format!(
        //                         "Expected type '{}' but got type '{}'.",
        //                         field_type.to_string(),
        //                         field_value.get_type(self.string_type.clone()).to_string()
        //                     )),
        //                     location: Some(DiagLoc {
        //                         file: self.file_path.clone(),
        //                         line: field_init.loc.line,
        //                         column: field_init.loc.column,
        //                         length: struct_init.span.end,
        //                     }),
        //                 });
        //                 exit(1);
        //             }

        //             let field_ptr = self
        //                 .builder
        //                 .build_struct_gep(
        //                     struct_def.struct_type,
        //                     struct_ptr,
        //                     field_idx.try_into().unwrap(),
        //                     "set_field",
        //                 )
        //                 .unwrap();

        //             self.build_store(field_ptr, field_value);
        //         }

        //         return AnyValue::StructValue(unsafe { StructValue::new(struct_ptr.as_value_ref()) });
        //     } else {
        //         display_single_diag(Diag {
        //             level: DiagLevel::Error,
        //             kind: DiagKind::Custom(format!("Struct '{}' not found.", struct_init.struct_name.to_string())),
        //             location: Some(DiagLoc {
        //                 file: self.file_path.clone(),
        //                 line: struct_init.loc.line,
        //                 column: struct_init.loc.column,
        //                 length: struct_init.span.end,
        //             }),
        //         });
        //         exit(1);
        //     }
        // } else {
        //     todo!();
        // }
    }
}
