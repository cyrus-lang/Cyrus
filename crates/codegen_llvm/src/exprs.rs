use crate::{
    CodeGenLLVM, ScopeRef,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    types::{AnyType, TypedPointerType},
    values::{AnyValue, ImportedModuleValue, StringValue, TypedPointerValue},
};
use ast::{
    ast::*,
    token::{Location, TokenKind},
};
use either::Either;
use inkwell::{
    AddressSpace,
    llvm_sys::{core::LLVMBuildGEP2, prelude::LLVMValueRef},
    types::{BasicType, BasicTypeEnum},
    values::{ArrayValue, AsValueRef, BasicValueEnum, FloatValue, IntValue, PointerValue},
};
use std::{
    ffi::CString,
    ops::{Deref, DerefMut},
    process::exit,
    rc::Rc,
};

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_expr(&self, scope: ScopeRef<'ctx>, expr: Expression) -> AnyValue<'ctx> {
        match expr {
            Expression::Identifier(identifier) => {
                self.build_load_lvalue(
                    Rc::clone(&scope),
                    ModuleImport {
                        segments: vec![ModuleSegment::SubModule(identifier.clone())],
                        span: identifier.span,
                        loc: identifier.loc,
                    },
                )
                .0
            }
            Expression::Assignment(assignment) => {
                self.build_assignment(Rc::clone(&scope), assignment);
                AnyValue::PointerValue(self.build_null())
            }
            Expression::Literal(literal) => self.build_literal(literal),
            Expression::Prefix(unary_expression) => self.build_prefix_expr(Rc::clone(&scope), unary_expression),
            Expression::Infix(binary_expression) => self.build_infix_expr(Rc::clone(&scope), binary_expression),
            Expression::UnaryOperator(unary_operator) => self.build_unary_operator(Rc::clone(&scope), unary_operator),
            Expression::CastAs(cast_as) => self.build_cast_as(Rc::clone(&scope), cast_as),
            Expression::FieldAccessOrMethodCall(field_access_or_method_call) => {
                self.build_field_access_or_method_call(Rc::clone(&scope), field_access_or_method_call)
            }
            Expression::AddressOf(expr) => self.build_address_of(Rc::clone(&scope), *expr),
            Expression::Dereference(expression) => self.build_deref(Rc::clone(&scope), *expression),
            Expression::StructInit(struct_init) => self.build_struct_init(Rc::clone(&scope), struct_init),
            Expression::Array(array) => self.build_array(Rc::clone(&scope), array),
            Expression::ArrayIndex(array_index) => self.build_array_index(Rc::clone(&scope), array_index),
            Expression::ArrayIndexAssign(array_index_assign) => {
                self.build_array_index_assign(Rc::clone(&scope), *array_index_assign);
                AnyValue::PointerValue(self.build_null())
            }
            Expression::ModuleImport(module_import) => self.build_module_import(Rc::clone(&scope), module_import).0,
            Expression::FuncCall(func_call) => {
                let call_site_value = self.build_func_call(Rc::clone(&scope), func_call);
                if let Some(basic_value) = call_site_value.try_as_basic_value().left() {
                    AnyValue::try_from(basic_value).unwrap()
                } else {
                    AnyValue::PointerValue(self.build_null())
                }
            }
            Expression::TypeToken(_) => AnyValue::PointerValue(self.build_null()),
        }
    }

    pub(crate) fn build_address_of(&self, scope: ScopeRef<'ctx>, expr: Expression) -> AnyValue<'ctx> {
        let any_value = self.build_expr(Rc::clone(&scope), expr);

        match any_value {
            AnyValue::PointerValue(typed_pointer_value) => AnyValue::PointerValue(typed_pointer_value),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot take the address of an rvalue.".to_string()),
                    location: None,
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_deref(&self, scope: ScopeRef<'ctx>, expr: Expression) -> AnyValue<'ctx> {
        let any_value = self.build_expr(Rc::clone(&scope), expr);

        match any_value {
            AnyValue::PointerValue(pointer_value) => {
                let inner_ptr_value = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        pointer_value.ptr,
                        "deref",
                    )
                    .unwrap()
                    .into_pointer_value();

                AnyValue::PointerValue(TypedPointerValue {
                    ptr: inner_ptr_value,
                    pointee_ty: pointer_value.pointee_ty,
                })
            }
            AnyValue::StringValue(string_value) => self.build_load_string(string_value),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot dereference non-pointer value.".to_string()),
                    location: None,
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_module_import(
        &self,
        scope: ScopeRef<'ctx>,
        module_import: ModuleImport,
    ) -> (AnyValue<'ctx>, AnyType<'ctx>) {
        if module_import.segments.len() == 1 {
            return self.build_load_lvalue(Rc::clone(&scope), module_import);
        }

        let record: (PointerValue<'ctx>, AnyType<'ctx>);

        let first_segment = {
            match module_import.segments.first().unwrap() {
                ModuleSegment::SubModule(sub_module) => sub_module,
            }
        };

        let binding = {
            match scope.borrow().get(first_segment.name.clone()) {
                Some(record) => {
                    if module_import.segments.len() >= 2 {
                        let chains: Vec<Either<FuncCall, FieldAccess>> = module_import.segments
                            [1..module_import.segments.len()]
                            .iter()
                            .map(|s| match s {
                                ModuleSegment::SubModule(identifier) => Either::Right(FieldAccess {
                                    identifier: identifier.clone(),
                                    span: identifier.span.clone(),
                                    loc: identifier.loc.clone(),
                                }),
                            })
                            .collect();

                        let result = self.build_field_access_or_method_call(
                            Rc::clone(&scope),
                            FieldAccessOrMethodCall {
                                expr: Box::new(Expression::ModuleImport(ModuleImport {
                                    segments: vec![ModuleSegment::SubModule(first_segment.clone())],
                                    span: module_import.span.clone(),
                                    loc: module_import.loc.clone(),
                                })),
                                chains,
                            },
                        );

                        return (result.clone(), result.get_type(self.string_type.clone()));
                    } else {
                        record
                    }
                }
                None => {
                    let module_identifier = self.build_module_identifier(ModulePath {
                        alias: None,
                        segments: module_import.segments[0..module_import.segments.len() - 1].to_vec(),
                    });

                    match self.find_loaded_module(module_identifier.clone()) {
                        Some(metadata) => {
                            return (
                                AnyValue::ImportedModuleValue(ImportedModuleValue { metadata }),
                                AnyType::ImportedModuleValue,
                            );
                        }
                        None => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom(format!("Module '{}' not found.", module_identifier)),
                                location: None,
                            });
                            exit(1);
                        }
                    }
                }
            }
        };
        let scope_record = binding.borrow_mut().deref().clone();
        record = (scope_record.ptr.clone(), scope_record.ty.clone());

        self.build_load_internal(record.0, record.1)
    }

    pub(crate) fn build_load_internal(
        &self,
        ptr: PointerValue<'ctx>,
        pointee_ty: AnyType<'ctx>,
    ) -> (AnyValue<'ctx>, AnyType<'ctx>) {
        let value = self
            .builder
            .build_load(
                pointee_ty.to_basic_type(self.context.ptr_type(AddressSpace::default())),
                ptr,
                "load",
            )
            .unwrap();

        if value.get_type() == BasicTypeEnum::StructType(self.string_type.struct_type.clone()) {
            (
                AnyValue::StringValue(self.string_from_struct_value(ptr)),
                pointee_ty.clone(),
            )
        } else {
            (AnyValue::try_from(value).unwrap(), pointee_ty.clone())
        }
    }

    pub(crate) fn build_load_rvalue(
        &self,
        scope: ScopeRef<'ctx>,
        module_import: ModuleImport,
    ) -> (AnyValue<'ctx>, AnyType<'ctx>) {
        match module_import.segments.first().unwrap() {
            ModuleSegment::SubModule(identifier) => {
                let binding = {
                    match scope.borrow().get(identifier.name.clone()) {
                        Some(record) => record,
                        None => match self.find_loaded_module(identifier.name.clone()) {
                            Some(metadata) => {
                                return (
                                    AnyValue::ImportedModuleValue(ImportedModuleValue { metadata }),
                                    AnyType::ImportedModuleValue,
                                );
                            }
                            None => {
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::IdentifierNotDefined(identifier.name.clone()),
                                    location: None,
                                });
                                exit(1);
                            }
                        },
                    }
                };
                let record = binding.borrow();
                self.build_load_internal(record.ptr.clone(), record.ty.clone())
            }
        }
    }

    pub(crate) fn build_load_lvalue(
        &self,
        scope: ScopeRef<'ctx>,
        module_import: ModuleImport,
    ) -> (AnyValue<'ctx>, AnyType<'ctx>) {
        match module_import.segments.first().unwrap() {
            ModuleSegment::SubModule(identifier) => {
                let binding = {
                    match scope.borrow().get(identifier.name.clone()) {
                        Some(record) => record,
                        None => match self.find_loaded_module(identifier.name.clone()) {
                            Some(metadata) => {
                                return (
                                    AnyValue::ImportedModuleValue(ImportedModuleValue { metadata }),
                                    AnyType::ImportedModuleValue,
                                );
                            }
                            None => {
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::IdentifierNotDefined(identifier.name.clone()),
                                    location: None,
                                });
                                exit(1);
                            }
                        },
                    }
                };

                let record = binding.borrow();
                (
                    AnyValue::PointerValue(TypedPointerValue {
                        ptr: record.ptr.clone(),
                        pointee_ty: record.ty.clone(),
                    }),
                    record.ty.clone(),
                )
            }
        }
    }

    pub(crate) fn build_assignment(&self, scope: ScopeRef<'ctx>, assignment: Box<Assignment>) {
        let assign_to = self.build_expr(Rc::clone(&scope), assignment.assign_to);
        let value = self.build_expr(Rc::clone(&scope), assignment.expr);

        if let AnyValue::PointerValue(pointer_value) = assign_to {
            self.build_store(pointer_value.ptr, value);
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Cannot store value in non-pointer allocation.".to_string()),
                location: None,
            });
            exit(1);
        }
    }

    pub(crate) fn build_array(&self, scope: ScopeRef<'ctx>, array: Array) -> AnyValue<'ctx> {
        let elements: Vec<BasicValueEnum> = array
            .elements
            .iter()
            .map(|item| BasicValueEnum::from(self.build_expr(Rc::clone(&scope), item.clone())))
            .collect();

        let first_element_type = elements[0].get_type();
        let _ = elements.iter().map(|item| {
            if first_element_type != item.get_type() {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::InconsistentArrayItemTypes,
                    location: None,
                });
                exit(1);
            }
        });

        let array_type = first_element_type.array_type(elements.len().try_into().unwrap());
        let array_elements = unsafe { ArrayValue::new_const_array(&array_type, &elements) };
        AnyValue::ArrayValue(array_type.const_array(&[array_elements]))
    }

    pub(crate) fn build_ordered_indexes(
        &self,
        scope: ScopeRef<'ctx>,
        dimensions: Vec<Expression>,
    ) -> Vec<IntValue<'_>> {
        let mut ordered_indexes: Vec<IntValue> = Vec::new();
        ordered_indexes.push(self.context.i32_type().const_int(0, false));
        for item in dimensions {
            if let Expression::Array(array) = item {
                if array.elements.len() != 1 {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(
                            "Expected only one integer literal when accessing array but got something else."
                                .to_string(),
                        ),
                        location: None,
                    });
                    exit(1);
                }

                if let AnyValue::IntValue(index) = self.build_expr(Rc::clone(&scope), array.elements[0].clone()) {
                    ordered_indexes.push(index);
                } else {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Cannot build array indexing with a non-integer index.".to_string()),
                        location: None,
                    });
                    exit(1);
                }
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Expected an integer literal as index but got something else.".to_string()),
                    location: None,
                });
                exit(1);
            }
        }
        ordered_indexes
    }

    // FIXME
    pub(crate) fn build_array_index(&self, scope: ScopeRef<'ctx>, array_index: ArrayIndex) -> AnyValue<'ctx> {
        let (any_value, pointee_ty) = self.build_load_rvalue(Rc::clone(&scope), array_index.module_import);

        if let AnyValue::PointerValue(pointer_value) = any_value {
            let ordered_indexes = self.build_ordered_indexes(Rc::clone(&scope), array_index.dimensions);

            let index_ptr = unsafe {
                let name = CString::new("gep").unwrap();
                let mut indices: Vec<LLVMValueRef> = ordered_indexes.iter().map(|item| item.as_value_ref()).collect();
                PointerValue::new(LLVMBuildGEP2(
                    self.builder.as_mut_ptr(),
                    pointee_ty.as_type_ref(),
                    pointer_value.ptr.as_value_ref(),
                    indices.as_mut_ptr(),
                    ordered_indexes.len().try_into().unwrap(),
                    name.as_ptr(),
                ))
            };

            let index_value = self
                .builder
                .build_load(
                    pointee_ty.to_basic_type(self.context.ptr_type(AddressSpace::default())),
                    index_ptr,
                    "load",
                )
                .unwrap();

            AnyValue::try_from(index_value).unwrap()
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Cannot apply array indexing to a non-array pointer type.".to_string()),
                location: None,
            });
            exit(1);
        }
    }

    pub(crate) fn build_array_index_assign(&self, scope: ScopeRef<'ctx>, array_index_assign: ArrayIndexAssign) {
        let (any_value, pointee_ty) = self.build_load_lvalue(Rc::clone(&scope), array_index_assign.module_import);

        if let AnyValue::PointerValue(pointer_value) = any_value {
            let ordered_indexes = self.build_ordered_indexes(Rc::clone(&scope), array_index_assign.dimensions);

            let index_ptr = unsafe {
                let name = CString::new("gep").unwrap();
                let mut indices: Vec<LLVMValueRef> = ordered_indexes.iter().map(|item| item.as_value_ref()).collect();
                PointerValue::new(LLVMBuildGEP2(
                    self.builder.as_mut_ptr(),
                    pointee_ty.as_type_ref(),
                    pointer_value.ptr.as_value_ref(),
                    indices.as_mut_ptr(),
                    ordered_indexes.len().try_into().unwrap(),
                    name.as_ptr(),
                ))
            };

            let value: BasicValueEnum = self.build_expr(Rc::clone(&scope), array_index_assign.expr).into();
            self.builder.build_store(index_ptr, value).unwrap();
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Cannot apply array index assign to a non-array pointer type.".to_string()),
                location: None,
            });
            exit(1);
        }
    }

    pub(crate) fn build_literal(&self, literal: Literal) -> AnyValue<'ctx> {
        match literal {
            Literal::Integer(integer_literal) => AnyValue::IntValue(self.build_integer_literal(integer_literal)),
            Literal::Float(float_literal) => AnyValue::FloatValue(self.build_float_literal(float_literal)),
            Literal::Bool(bool_literal) => AnyValue::IntValue(self.build_bool_literal(bool_literal)),
            Literal::String(string_literal) => self.build_string_literal(string_literal),
            Literal::Char(char_literal) => AnyValue::IntValue(self.build_char_literal(char_literal)),
            Literal::Null => AnyValue::PointerValue(self.build_null()),
        }
    }

    pub(crate) fn build_integer_literal(&self, integer_literal: IntegerLiteral) -> IntValue<'ctx> {
        match integer_literal {
            IntegerLiteral::I8(val) => self.context.i8_type().const_int(val.try_into().unwrap(), true),
            IntegerLiteral::I16(val) => self.context.i16_type().const_int(val.try_into().unwrap(), true),
            IntegerLiteral::I32(val) => self.context.i32_type().const_int(val.try_into().unwrap(), true),
            IntegerLiteral::I64(val) => self.context.i64_type().const_int(val.try_into().unwrap(), true),
            IntegerLiteral::I128(val) => self.context.i128_type().const_int(val.try_into().unwrap(), true),
            IntegerLiteral::U8(val) => self.context.i8_type().const_int(val.try_into().unwrap(), false),
            IntegerLiteral::U16(val) => self.context.i16_type().const_int(val.try_into().unwrap(), false),
            IntegerLiteral::U32(val) => self.context.i32_type().const_int(val.try_into().unwrap(), false),
            IntegerLiteral::U64(val) => self.context.i64_type().const_int(val.try_into().unwrap(), false),
            IntegerLiteral::U128(val) => self.context.i128_type().const_int(val.try_into().unwrap(), false),
            IntegerLiteral::SizeT(val) => {
                let data_layout = self.target_machine.get_target_data();
                self.context
                    .ptr_sized_int_type(&data_layout, None)
                    .const_int(val.try_into().unwrap(), false)
            }
        }
    }

    pub(crate) fn build_float_literal(&self, float_literal: FloatLiteral) -> FloatValue<'ctx> {
        match float_literal {
            FloatLiteral::Float(val) => self.context.f32_type().const_float(val.into()),
            FloatLiteral::Double(val) => self.context.f64_type().const_float(val),
        }
    }

    pub(crate) fn build_string_literal(&self, string_literal: StringLiteral) -> AnyValue<'ctx> {
        let mut bytes = self.unescape_string(&string_literal.raw).into_bytes();
        bytes.push(0); // null terminator

        let i8_array_type = self.context.i8_type().array_type(bytes.len() as u32);

        let string_global =
            self.module
                .borrow_mut()
                .deref_mut()
                .add_global(i8_array_type, Some(AddressSpace::default()), ".str");

        let const_string = self.context.const_string(&bytes, false);
        string_global.set_initializer(&const_string);
        string_global.set_constant(true);
        string_global.set_linkage(inkwell::module::Linkage::Private);

        AnyValue::StringValue(StringValue {
            struct_value: self.string_type.struct_type.const_named_struct(&[
                BasicValueEnum::PointerValue(string_global.as_pointer_value()),
                BasicValueEnum::IntValue(
                    self.context
                        .i64_type()
                        .const_int(bytes.len().try_into().unwrap(), false),
                ),
            ]),
        })
    }

    pub(crate) fn build_char_literal(&self, char_literal: CharLiteral) -> IntValue<'ctx> {
        self.context.i8_type().const_int(char_literal.raw as u8 as u64, false)
    }

    pub(crate) fn build_null(&self) -> TypedPointerValue<'ctx> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        TypedPointerValue {
            ptr: ptr_type.const_null(),
            pointee_ty: AnyType::PointerType(Box::new(TypedPointerType {
                ptr_type,
                pointee_ty: AnyType::IntType(self.context.i32_type()),
            })),
        }
    }

    pub(crate) fn build_bool_literal(&self, bool_literal: BoolLiteral) -> IntValue<'ctx> {
        self.context
            .bool_type()
            .const_int(if bool_literal.raw { 1 } else { 0 }, false)
    }

    pub(crate) fn build_cast_as(&self, scope: ScopeRef<'ctx>, cast_as: CastAs) -> AnyValue<'ctx> {
        let expr = self.build_expr(Rc::clone(&scope), *cast_as.expr.clone());
        let target = self.build_type(cast_as.type_token, cast_as.loc.clone(), cast_as.span.end);

        match expr {
            AnyValue::IntValue(int_value) => match target {
                AnyType::IntType(int_type) => {
                    AnyValue::IntValue(self.builder.build_int_cast(int_value, int_type, "cast").unwrap())
                }
                AnyType::FloatType(float_type) => AnyValue::FloatValue(
                    self.builder
                        .build_signed_int_to_float(int_value, float_type, "cast")
                        .unwrap(),
                ),
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from("Cannot cast non-basic value.")),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: cast_as.loc.line,
                            column: cast_as.loc.column,
                            length: cast_as.span.end,
                        }),
                    });
                    exit(1);
                }
            },
            AnyValue::FloatValue(float_value) => match target {
                AnyType::IntType(int_type) => AnyValue::IntValue(
                    self.builder
                        .build_float_to_signed_int(float_value, int_type, "cast")
                        .unwrap(),
                ),
                AnyType::FloatType(float_type) => {
                    AnyValue::FloatValue(self.builder.build_float_cast(float_value, float_type, "cast").unwrap())
                }
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from("Cannot cast non-basic value.")),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: cast_as.loc.line,
                            column: cast_as.loc.column,
                            length: cast_as.span.end,
                        }),
                    });
                    exit(1);
                }
            },
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(String::from("Cannot cast non-basic value.")),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: cast_as.loc.line,
                        column: cast_as.loc.column,
                        length: cast_as.span.end,
                    }),
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_prefix_expr(&self, scope: ScopeRef<'ctx>, unary_expression: UnaryExpression) -> AnyValue<'ctx> {
        let operand = self.build_expr(Rc::clone(&scope), *unary_expression.operand.clone());
        match unary_expression.operator.kind {
            TokenKind::Minus => match operand {
                AnyValue::IntValue(int_value) => {
                    AnyValue::IntValue(self.builder.build_int_neg(int_value, "prefix_iminus").unwrap())
                }
                AnyValue::FloatValue(float_value) => {
                    AnyValue::FloatValue(self.builder.build_float_neg(float_value, "prefix_fminus").unwrap())
                }
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from("Cannot build minus for non-basic value.")),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: unary_expression.loc.line,
                            column: unary_expression.loc.column,
                            length: unary_expression.span.end,
                        }),
                    });
                    exit(1);
                }
            },
            TokenKind::Bang => match operand {
                AnyValue::IntValue(int_value) => {
                    let zero = int_value.get_type().const_int(0, false);
                    let one = int_value.get_type().const_int(1, false);

                    let is_zero = self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, int_value, zero, "prefix_ineg")
                        .unwrap();

                    if is_zero.get_zero_extended_constant().unwrap() == 0 {
                        AnyValue::IntValue(zero)
                    } else {
                        AnyValue::IntValue(one)
                    }
                }
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from("Cannot build neg for non-integer value.")),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: unary_expression.loc.line,
                            column: unary_expression.loc.column,
                            length: unary_expression.span.end,
                        }),
                    });
                    exit(1);
                }
            },
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(String::from("Invalid prefix token.")),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: unary_expression.loc.line,
                        column: unary_expression.loc.column,
                        length: unary_expression.span.end,
                    }),
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_unary_operator(&self, scope: ScopeRef<'ctx>, unary_operator: UnaryOperator) -> AnyValue<'ctx> {
        let int_one = self.context.i32_type().const_int(1, false);
        let value = self.any_value_as_rvalue(self
            .build_module_import(Rc::clone(&scope), unary_operator.module_import)
            .0);
        
        match value {
            AnyValue::IntValue(int_value) => match unary_operator.ty {
                UnaryOperatorType::PreIncrement => {
                    return AnyValue::IntValue(self.builder.build_int_add(int_value, int_one, "unaryop").unwrap());
                }
                UnaryOperatorType::PreDecrement => {
                    return AnyValue::IntValue(self.builder.build_int_sub(int_value, int_one, "unaryop").unwrap());
                }
                UnaryOperatorType::PostIncrement => {
                    let clone = value.clone();
                    self.builder.build_int_add(int_value, int_one, "unaryop").unwrap();
                    return clone;
                }
                UnaryOperatorType::PostDecrement => {
                    let clone = value.clone();
                    self.builder.build_int_sub(int_value, int_one, "unaryop").unwrap();
                    return clone;
                }
            },
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(String::from("Cannot build unary operator for non-integer value.")),
                    location: None,
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_infix_expr(
        &self,
        scope: ScopeRef<'ctx>,
        binary_expression: BinaryExpression,
    ) -> AnyValue<'ctx> {
        let left = self.any_value_as_rvalue(self.build_expr(Rc::clone(&scope), *binary_expression.left));
        let right = self.any_value_as_rvalue(self.build_expr(Rc::clone(&scope), *binary_expression.right));

        let result = match binary_expression.operator.kind {
            TokenKind::Plus => self.bin_op_add(left, right),
            TokenKind::Minus => self.bin_op_sub(left, right),
            TokenKind::Asterisk => self.bin_op_mul(left, right),
            TokenKind::Slash => self.bin_op_div(left, right),
            TokenKind::Percent => self.bin_op_rem(left, right),
            TokenKind::LessThan => self.bin_op_lt(left, right),
            TokenKind::LessEqual => self.bin_op_le(left, right),
            TokenKind::GreaterThan => self.bin_op_gt(left, right),
            TokenKind::GreaterEqual => self.bin_op_ge(left, right),
            TokenKind::Equal => self.bin_op_eq(left, right),
            TokenKind::NotEqual => self.bin_op_neq(left, right),
            TokenKind::Or => self.bin_op_or(left, right),
            TokenKind::And => self.bin_op_and(left, right),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(String::from("Invalid infix token.")),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: binary_expression.loc.line,
                        column: binary_expression.loc.column,
                        length: binary_expression.span.end,
                    }),
                });
                exit(1);
            }
        };

        result.unwrap_or_else(|| {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::InfixNonBasic,
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: binary_expression.loc.line,
                    column: binary_expression.loc.column,
                    length: binary_expression.span.end,
                }),
            });
            exit(1);
        })
    }

    pub(crate) fn build_cond(
        &self,
        scope: ScopeRef<'ctx>,
        expr: Expression,
        loc: Location,
        span_end: usize,
    ) -> IntValue<'ctx> {
        if let AnyValue::IntValue(value) = self.any_value_as_rvalue(self.build_expr(scope, expr)) {
            value
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Condition result must be an integer value.".to_string()),
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
