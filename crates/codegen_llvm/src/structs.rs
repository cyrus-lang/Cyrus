use crate::{
    CodeGenLLVM, InternalType, InternalValue,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    scope::ScopeRef,
    types::DefinedType,
    values::Lvalue,
};
use ast::{
    ast::{Field, FieldAccess, Identifier, ModuleImport, StorageClass, Struct, StructInit},
    format::module_segments_as_string,
    token::Location,
};
use inkwell::{
    AddressSpace,
    llvm_sys::core::LLVMGetGEPSourceElementType,
    types::{BasicTypeEnum, StructType},
    values::{AggregateValueEnum, PointerValue, StructValue},
};
use std::{collections::HashMap, process::exit, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct StructMetadata<'a> {
    pub struct_name: ModuleImport,
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
        struct_type
    }

    pub(crate) fn build_struct_init(&self, scope: ScopeRef<'ctx>, struct_init: StructInit) -> InternalValue<'ctx> {
        let defined_type = self.find_defined_type(
            struct_init.struct_name.clone(),
            struct_init.loc.clone(),
            struct_init.span.end,
        );

        let struct_metadata = match defined_type {
            DefinedType::Struct(struct_metadata) => struct_metadata,
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
            let field_idx = struct_metadata
                .fields
                .iter()
                .position(|field| field.name == field_init.name)
                .unwrap();

            let field = struct_metadata.fields.get(field_idx).unwrap();

            let field_type = self.build_type(field.ty.clone(), field.loc.clone(), field.span.end);

            let field_rvalue =
                self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), field_init.value.clone()));

            if !self.compatible_types(field_type.clone(), field_rvalue.get_type(self.string_type.clone())) {
                // FIXME We need accurate type name tracking here
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Error: Field {} of struct '{}' expects a value of type '{:?}', but received '{:?}'.",
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

            let field_rvalue = self.implicit_cast(field_rvalue, field_type);

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

        InternalValue::StructValue(struct_value, InternalType::StructType(struct_metadata.clone()))
    }

    pub(crate) fn build_struct_field_access(
        &self,
        pointer: PointerValue<'ctx>,
        field_name: String,
        struct_internal_type: InternalType<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        let struct_metadata = match struct_internal_type {
            InternalType::StructType(struct_metadata) => struct_metadata,
            _ => unreachable!(),
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
    pub(crate) fn build_field_access(&self, scope: ScopeRef<'ctx>, field_access: FieldAccess) -> InternalValue<'ctx> {
        let internal_value = self.build_expr(Rc::clone(&scope), *field_access.operand);

        match self.internal_value_as_rvalue(internal_value.clone()) {
            InternalValue::StructValue(_, struct_internal_type) => {
                let pointer = match internal_value {
                    InternalValue::PointerValue(typed_pointer_value) => typed_pointer_value.ptr,
                    InternalValue::Lvalue(lvalue) => lvalue.ptr,
                    _ => {
                        panic!(
                            "InternalValue::PointerValue or InternalValue::Lvalue for struct field access but got {:?}",
                            internal_value
                        );
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
            // TODO Implement internal methods for primitive data types.
            InternalValue::BoolValue(int_value) => todo!(),
            InternalValue::IntValue(int_value, internal_type) => todo!(),
            InternalValue::FloatValue(float_value, internal_type) => todo!(),
            InternalValue::ArrayValue(array_value, internal_type) => todo!(),
            InternalValue::VectorValue(vector_value, internal_type) => todo!(),
            InternalValue::StrValue(pointer_value, internal_type) => todo!(),
            InternalValue::StringValue(string_value) => todo!(),
            InternalValue::ModuleValue(module_metadata) => {
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
            InternalValue::PointerValue(typed_pointer_value) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot access fields on a pointer_value.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: field_access.loc.line,
                        column: field_access.loc.column,
                        length: field_access.span.end,
                    }),
                });
                exit(1);
            }
            InternalValue::FunctionValue(func_metadata) => {
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
            InternalValue::Lvalue(lvalue) => {
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
        }
    }
}
