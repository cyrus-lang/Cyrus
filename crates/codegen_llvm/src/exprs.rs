use crate::{
    CodeGenLLVM, ScopeRef,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    types::{InternalType, TypedPointerType},
    values::{InternalValue, Lvalue, StringValue, TypedPointerValue},
};
use ast::{
    ast::*,
    token::{Location, TokenKind},
};
use core::panic;
use inkwell::{
    AddressSpace,
    types::BasicType,
    values::{AnyValue, ArrayValue, BasicValueEnum, FloatValue, IntValue, PointerValue},
};
use std::{
    ops::{Deref, DerefMut},
    process::exit,
    rc::Rc,
};
use utils::purify_string::unescape_string;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_expr(&self, scope: ScopeRef<'ctx>, expr: Expression) -> InternalValue<'ctx> {
        match expr {
            Expression::FieldAccess(field_access) => todo!(),
            Expression::MethodCall(method_call) => todo!(),
            Expression::Identifier(identifier) => self.build_load_lvalue(
                Rc::clone(&scope),
                ModuleImport {
                    segments: vec![ModuleSegment::SubModule(identifier.clone())],
                    span: identifier.span,
                    loc: identifier.loc,
                },
            ),
            Expression::Assignment(assignment) => {
                self.build_assignment(Rc::clone(&scope), assignment);
                InternalValue::PointerValue(self.build_null())
            }
            Expression::Literal(literal) => self.build_literal(literal),
            Expression::Prefix(unary_expression) => self.build_prefix_expr(Rc::clone(&scope), unary_expression),
            Expression::Infix(binary_expression) => self.build_infix_expr(Rc::clone(&scope), binary_expression),
            Expression::UnaryOperator(unary_operator) => self.build_unary_operator(Rc::clone(&scope), unary_operator),
            Expression::Cast(cast_as) => self.build_cast_expression(Rc::clone(&scope), cast_as),
            Expression::AddressOf(expr) => self.build_address_of(Rc::clone(&scope), *expr),
            Expression::Dereference(expression) => self.build_deref(Rc::clone(&scope), *expression),
            Expression::StructInit(struct_init) => self.build_struct_init(Rc::clone(&scope), struct_init),
            Expression::Array(array) => self.build_array(Rc::clone(&scope), array),
            Expression::ArrayIndex(array_index) => self.build_array_index(Rc::clone(&scope), array_index),
            Expression::ModuleImport(module_import) => self.build_module_import(Rc::clone(&scope), module_import),
            Expression::FuncCall(func_call) => {
                let (call_site_value, return_type) = self.build_func_call(Rc::clone(&scope), func_call);
                if let Some(value) = call_site_value.try_as_basic_value().left() {
                    self.new_internal_value_from_basic_value_enum(value, return_type)
                } else {
                    InternalValue::PointerValue(self.build_null())
                }
            }
            Expression::TypeSpecifier(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build type specifier here.".to_string()),
                    location: None,
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_address_of(&self, scope: ScopeRef<'ctx>, expr: Expression) -> InternalValue<'ctx> {
        let internal_value = self.build_expr(Rc::clone(&scope), expr);

        if let InternalValue::Lvalue(lvalue) = internal_value {
            return InternalValue::PointerValue(TypedPointerValue {
                ptr: lvalue.ptr,
                pointee_ty: lvalue.pointee_ty,
            });
        }

        display_single_diag(Diag {
            level: DiagLevel::Error,
            kind: DiagKind::Custom("Cannot take the address of an rvalue.".to_string()),
            location: None,
        });
        exit(1);
    }

    pub(crate) fn build_deref(&self, scope: ScopeRef<'ctx>, expr: Expression) -> InternalValue<'ctx> {
        let internal_value = self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), expr));

        match internal_value {
            InternalValue::PointerValue(pointer_value) => InternalValue::Lvalue(Lvalue {
                ptr: pointer_value.ptr,
                pointee_ty: pointer_value.pointee_ty,
            }),
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
    ) -> InternalValue<'ctx> {
        if module_import.segments.len() == 1 {
            return self.build_load_lvalue(Rc::clone(&scope), module_import);
        }

        let record: (PointerValue<'ctx>, InternalType<'ctx>);

        let first_segment = {
            match module_import.segments.first().unwrap() {
                ModuleSegment::SubModule(sub_module) => sub_module,
            }
        };

        let binding = {
            match scope.borrow().get(first_segment.name.clone()) {
                Some(record) => {
                    if module_import.segments.len() >= 2 {
                        // FIXME
                        // let chains: Vec<Either<FuncCall, FieldAccess>> = module_import.segments
                        //     [1..module_import.segments.len()]
                        //     .iter()
                        //     .map(|s| match s {
                        //         ModuleSegment::SubModule(identifier) => Either::Right(FieldAccess {
                        //             identifier: identifier.clone(),
                        //             span: identifier.span.clone(),
                        //             loc: identifier.loc.clone(),
                        //         }),
                        //     })
                        //     .collect();

                        // let result = self.build_field_access_or_method_call(
                        //     Rc::clone(&scope),
                        //     FieldAccessOrMethodCall {
                        //         expr: Box::new(Expression::ModuleImport(ModuleImport {
                        //             segments: vec![ModuleSegment::SubModule(first_segment.clone())],
                        //             span: module_import.span.clone(),
                        //             loc: module_import.loc.clone(),
                        //         })),
                        //         chains,
                        //     },
                        // );

                        // return (result.clone(), result.get_type(self.string_type.clone()));

                        todo!();
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
                            return InternalValue::ModuleValue(metadata);
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
        pointee_ty: InternalType<'ctx>,
    ) -> InternalValue<'ctx> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let loaded_value = self
            .builder
            .build_load(pointee_ty.to_basic_type(ptr_type), ptr, "load")
            .unwrap();
        self.new_internal_value_from_basic_value_enum(loaded_value, pointee_ty)
    }

    pub(crate) fn build_load_lvalue(&self, scope: ScopeRef<'ctx>, module_import: ModuleImport) -> InternalValue<'ctx> {
        let first_segment = module_import.segments.first().unwrap();
        let ModuleSegment::SubModule(identifier) = first_segment;

        // If the module import has only one segment, we try to find the identifier in the current scope.
        // If not found, we check if it's an imported module.
        // If neither is found, we report an error.
        if module_import.segments.len() == 1 {
            // local variable
            if let Some(record) = scope.borrow().get(identifier.name.clone()) {
                let record = record.borrow();
                return InternalValue::Lvalue(Lvalue {
                    ptr: record.ptr.clone(),
                    pointee_ty: record.ty.clone(),
                });
            }
            // local function
            else if let Some(func_metadata) = self.func_table.get(&identifier.name.clone()) {
                return InternalValue::FunctionValue(func_metadata.clone());
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::IdentifierNotDefined(identifier.name.clone()),
                    location: None,
                });
                exit(1);
            }
        } else {
            self.build_module_import(Rc::clone(&scope), module_import)
        }
    }

    pub(crate) fn build_assignment(&self, scope: ScopeRef<'ctx>, assignment: Box<Assignment>) {
        let assign_to = self.build_expr(Rc::clone(&scope), assignment.assign_to);
        let rvalue = self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), assignment.expr));

        match assign_to.clone() {
            InternalValue::PointerValue(typed_pointer_value) => {
                if let InternalType::ConstType(_) = typed_pointer_value.pointee_ty {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Cannot assign to a constant lvalue.".to_string()),
                        location: None,
                    });
                    exit(1);
                }

                if !self.compatible_types(
                    typed_pointer_value.pointee_ty.clone(),
                    rvalue.get_type(self.string_type.clone()),
                ) {
                    // FIXME We need accurate type name tracking here
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Cannot assign value of type '{}' to lvalue of type '{}'.",
                            rvalue
                                .get_type(self.string_type.clone())
                                .to_basic_type(self.context.ptr_type(AddressSpace::default())),
                            typed_pointer_value
                                .pointee_ty
                                .to_basic_type(self.context.ptr_type(AddressSpace::default())),
                        )),
                        location: None,
                    });
                    exit(1);
                };
                let final_rvalue = self.implicit_cast(rvalue, typed_pointer_value.pointee_ty);
                self.builder.build_store(typed_pointer_value.ptr, final_rvalue).unwrap();
            }
            InternalValue::Lvalue(lvalue) => {
                if let InternalType::ConstType(_) = lvalue.pointee_ty {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Cannot assign to a constant lvalue.".to_string()),
                        location: None,
                    });
                    exit(1);
                }

                if !self.compatible_types(lvalue.pointee_ty.clone(), rvalue.get_type(self.string_type.clone())) {
                    // FIXME We need accurate type name tracking here
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Cannot assign value of type {} to lvalue of type {}.",
                            rvalue
                                .get_type(self.string_type.clone())
                                .to_basic_type(self.context.ptr_type(AddressSpace::default())),
                            lvalue
                                .pointee_ty
                                .clone()
                                .to_basic_type(self.context.ptr_type(AddressSpace::default())),
                        )),
                        location: None,
                    });
                    exit(1);
                };
                let final_rvalue = self.implicit_cast(rvalue, lvalue.pointee_ty);
                self.builder.build_store(lvalue.ptr, final_rvalue).unwrap();
            }
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot assign to a non-pointer value.".to_string()),
                    location: None,
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_array(&self, scope: ScopeRef<'ctx>, array: Array) -> InternalValue<'ctx> {
        match array.data_type {
            TypeSpecifier::Array(element_type_specifier, dimensions) => {
                let array_type = self.build_array_type(*element_type_specifier, dimensions, array.loc, array.span.end);

                dbg!(array.elements.clone());

                todo!();
            }
            _ => unreachable!(),
        }
    }

    // pub(crate) fn build_array(&self, scope: ScopeRef<'ctx>, array: Array) -> InternalValue<'ctx> {
    //     match array.data_type {
    //         TypeSpecifier::Array(element_type_specifier, dimensions) => {
    //             let mut array_value_vec: Vec<ArrayValue<'ctx>> = Vec::new();
    //             let ptr_type = self.context.ptr_type(AddressSpace::default());
    //             let element_type = self.build_type(*element_type_specifier.clone(), array.loc.clone(), array.span.end);
    //             let dimensions_len = dimensions.len();

    //             dbg!(dimensions.iter().rev());

    //             for array_capacity in dimensions.iter().rev() {
    //                 match array_capacity {
    //                     ArrayCapacity::Static(token_kind) => {
    //                         let array_size: u32 = match token_kind {
    //                             TokenKind::Literal(literal) => match literal {
    //                                 Literal::Integer(value) => (*value).try_into().unwrap(),
    //                                 _ => {
    //                                     display_single_diag(Diag {
    //                                         level: DiagLevel::Error,
    //                                         kind: DiagKind::Custom(
    //                                             "Cannot build array type with non-integer value as it's capacity."
    //                                                 .to_string(),
    //                                         ),
    //                                         location: None,
    //                                     });
    //                                     exit(1);
    //                                 }
    //                             },
    //                             _ => {
    //                                 display_single_diag(Diag {
    //                                     level: DiagLevel::Error,
    //                                     kind: DiagKind::Custom(
    //                                         "Cannot build array type with non-integer value as it's capacity."
    //                                             .to_string(),
    //                                     ),
    //                                     location: None,
    //                                 });
    //                                 exit(1);
    //                             }
    //                         };

    //                         let mut element_values: Vec<BasicValueEnum<'ctx>> =
    //                             Vec::with_capacity(array_size.try_into().unwrap());

    //                         // ensure type compatibility of the items
    //                         for (idx, element) in array.elements.iter().enumerate() {
    //                             let element_value = self.build_expr(Rc::clone(&scope), element.clone());

    //                             if !self.compatible_types(
    //                                 element_type.clone(),
    //                                 element_value.get_type(self.string_type.clone()),
    //                             ) {
    //                                 display_single_diag(Diag {
    //                                     level: DiagLevel::Error,
    //                                     kind: DiagKind::Custom(format!(
    //                                         "Incompatible item in array construction at index {}.",
    //                                         idx
    //                                     )),
    //                                     location: Some(DiagLoc {
    //                                         file: self.file_path.clone(),
    //                                         line: array.loc.line,
    //                                         column: array.loc.column,
    //                                         length: array.span.end,
    //                                     }),
    //                                 });
    //                                 exit(1);
    //                             }

    //                             element_values.push(
    //                                 element_value
    //                                     .to_basic_metadata()
    //                                     .as_any_value_enum()
    //                                     .try_into()
    //                                     .unwrap(),
    //                             );
    //                         }

    //                         let array_value = unsafe {
    //                             ArrayValue::new_const_array(&element_type.to_basic_type(ptr_type), &element_values)
    //                         };
    //                         array_value_vec.push(array_value);
    //                     }
    //                     ArrayCapacity::Dynamic => {
    //                         display_single_diag(Diag {
    //                             level: DiagLevel::Error,
    //                             kind: DiagKind::Custom(
    //                                 "Cannot build array with dynamic memory management. Consider to checkout `std::vec` module."
    //                                     .to_string(),
    //                             ),
    //                             location: None,
    //                         });
    //                         exit(1);
    //                     }
    //                 }
    //             }

    //             let outer_array_type = element_type
    //                 .to_basic_type(ptr_type)
    //                 .array_type(dimensions_len.try_into().unwrap());
    //             dbg!(array_value_vec.clone());

    //             todo!();
    //         }
    //         _ => unreachable!(),
    //     }
    // }

    pub(crate) fn build_ordered_indexes(
        &self,
        scope: ScopeRef<'ctx>,
        dimensions: Vec<Expression>,
    ) -> Vec<IntValue<'_>> {
        let mut ordered_indexes: Vec<IntValue> = Vec::new();
        ordered_indexes.push(self.context.i32_type().const_int(0, false));
        for index_expr in dimensions {
            if let InternalValue::IntValue(index, _) = self.build_expr(Rc::clone(&scope), index_expr.clone()) {
                ordered_indexes.push(index);
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build array indexing with a non-integer index.".to_string()),
                    location: None,
                });
                exit(1);
            }
        }
        ordered_indexes
    }

    // FIXME
    pub(crate) fn build_array_index(&self, scope: ScopeRef<'ctx>, array_index: ArrayIndex) -> InternalValue<'ctx> {
        todo!();

        // let internal_value = self.build_expr(Rc::clone(&scope), *array_index.expr);

        // if let InternalValue::PointerValue(pointer_value) = internal_value {
        //     if let InternalType::ArrayType(_) = pointer_value.pointee_ty {
        //         let ordered_indexes = self.build_ordered_indexes(Rc::clone(&scope), array_index.dimensions);

        //         let index_ptr = unsafe {
        //             let name = CString::new("gep").unwrap();
        //             let mut indices: Vec<LLVMValueRef> =
        //                 ordered_indexes.iter().map(|item| item.as_value_ref()).collect();

        //             PointerValue::new(LLVMBuildGEP2(
        //                 self.builder.as_mut_ptr(),
        //                 pointer_value.pointee_ty.as_type_ref(),
        //                 pointer_value.ptr.as_value_ref(),
        //                 indices.as_mut_ptr(),
        //                 ordered_indexes.len().try_into().unwrap(),
        //                 name.as_ptr(),
        //             ))
        //         };

        //         let ptr_type = self.context.ptr_type(AddressSpace::default());
        //         let array_element_type = pointer_value
        //             .pointee_ty
        //             .to_basic_type(ptr_type)
        //             .into_array_type()
        //             .get_element_type();

        //         let index_value = self.builder.build_load(array_element_type, index_ptr, "load").unwrap();
        //         InternalValue::try_from(index_value).unwrap()
        //     } else {
        //         display_single_diag(Diag {
        //             level: DiagLevel::Error,
        //             kind: DiagKind::Custom(
        //                 "Cannot build array indexing to a pointer with non-array pointee type.".to_string(),
        //             ),
        //             location: None,
        //         });
        //         exit(1);
        //     }
        // } else {
        //     display_single_diag(Diag {
        //         level: DiagLevel::Error,
        //         kind: DiagKind::Custom("Cannot apply array indexing to a non-array pointer type.".to_string()),
        //         location: None,
        //     });
        //     exit(1);
        // }
    }

    pub(crate) fn build_literal(&self, literal: Literal) -> InternalValue<'ctx> {
        match literal {
            Literal::Integer(integer_literal) => {
                InternalValue::IntValue(self.build_integer_literal(integer_literal), self.context.i32_type())
            }
            Literal::Float(float_literal) => {
                InternalValue::FloatValue(self.build_float_literal(float_literal), self.context.f32_type())
            }
            Literal::Bool(bool_literal) => {
                InternalValue::IntValue(self.build_bool_literal(bool_literal), self.context.bool_type())
            }
            Literal::Char(char_literal) => {
                InternalValue::IntValue(self.build_char_literal(char_literal), self.context.i8_type())
            }
            Literal::String(string_literal) => self.build_string_literal(string_literal),
            Literal::Null => InternalValue::PointerValue(self.build_null()),
        }
    }

    pub(crate) fn build_integer_literal(&self, value: i64) -> IntValue<'ctx> {
        let data_layout = self.target_machine.get_target_data();
        self.context
            .ptr_sized_int_type(&data_layout, None)
            .const_int(value.try_into().unwrap(), false)
    }

    pub(crate) fn build_float_literal(&self, value: f64) -> FloatValue<'ctx> {
        self.context.f64_type().const_float(value)
    }

    pub(crate) fn build_string_literal(&self, value: String) -> InternalValue<'ctx> {
        let mut bytes = unescape_string(value).into_bytes();
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

        InternalValue::StrValue(string_global.as_any_value_enum().into_pointer_value(), i8_array_type)
    }

    pub(crate) fn build_char_literal(&self, value: char) -> IntValue<'ctx> {
        self.context.i8_type().const_int(value as u64, false)
    }

    pub(crate) fn build_null(&self) -> TypedPointerValue<'ctx> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        TypedPointerValue {
            ptr: ptr_type.const_null(),
            pointee_ty: InternalType::PointerType(Box::new(TypedPointerType {
                ptr_type,
                pointee_ty: InternalType::IntType(self.context.i32_type()),
            })),
        }
    }

    pub(crate) fn build_bool_literal(&self, value: bool) -> IntValue<'ctx> {
        self.context.bool_type().const_int(if value { 1 } else { 0 }, false)
    }

    pub(crate) fn build_cast_expression_internal(
        &self,
        internal_value: InternalValue<'ctx>,
        target_type: InternalType<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        match internal_value.clone() {
            InternalValue::IntValue(int_value, _) => match target_type {
                InternalType::IntType(int_type) => InternalValue::IntValue(
                    self.builder.build_int_cast(int_value, int_type, "cast").unwrap(),
                    int_type,
                ),
                InternalType::FloatType(float_type) => InternalValue::FloatValue(
                    self.builder
                        .build_signed_int_to_float(int_value, float_type, "cast")
                        .unwrap(),
                    float_type,
                ),
                InternalType::ConstType(inner_type) => {
                    self.build_cast_expression_internal(internal_value, *inner_type, loc, span_end)
                }
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from("Cannot cast non-basic value.")),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
            },
            InternalValue::FloatValue(float_value, _) => match target_type {
                InternalType::IntType(int_type) => InternalValue::IntValue(
                    self.builder
                        .build_float_to_signed_int(float_value, int_type, "cast")
                        .unwrap(),
                    int_type,
                ),
                InternalType::FloatType(float_type) => InternalValue::FloatValue(
                    self.builder.build_float_cast(float_value, float_type, "cast").unwrap(),
                    float_type,
                ),
                InternalType::ConstType(inner_type) => {
                    self.build_cast_expression_internal(internal_value, *inner_type, loc, span_end)
                }
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from("Cannot cast non-basic value.")),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
            },
            InternalValue::PointerValue(pointer_value) => match target_type {
                InternalType::StringType(_) => {
                    match pointer_value
                        .pointee_ty
                        .to_basic_type(self.context.ptr_type(AddressSpace::default()))
                    {
                        inkwell::types::BasicTypeEnum::ArrayType(array_type) => {
                            let buffer_size = self.context.i64_type().const_int(array_type.len().into(), false);
                            self.build_construct_string_value(pointer_value.ptr, buffer_size)
                        }
                        inkwell::types::BasicTypeEnum::IntType(_) => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom(String::from("Cannot cast char* to string.")),
                                location: Some(DiagLoc {
                                    file: self.file_path.clone(),
                                    line: loc.line,
                                    column: loc.column,
                                    length: span_end,
                                }),
                            });
                            exit(1);
                        }
                        _ => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom(String::from("Cannot build invalid cast for constant string.")),
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
                InternalType::PointerType(typed_pointer_type) => {
                    let casted_ptr = self
                        .builder
                        .build_pointer_cast(pointer_value.ptr, typed_pointer_type.ptr_type, "cast_ptr")
                        .unwrap();

                    InternalValue::PointerValue(TypedPointerValue {
                        ptr: casted_ptr,
                        pointee_ty: typed_pointer_type.pointee_ty,
                    })
                }
                InternalType::ConstType(inner_type) => {
                    self.build_cast_expression_internal(internal_value, *inner_type, loc, span_end)
                }
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from("Cannot cast pointer to non-pointer value.")),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
            },
            InternalValue::StringValue(string_value) => match target_type {
                InternalType::PointerType(typed_pointer_type) => match &typed_pointer_type.pointee_ty {
                    InternalType::IntType(_) => self.build_load_string(string_value.clone()),
                    InternalType::ConstType(inner_type) => {
                        self.build_cast_expression_internal(internal_value, *inner_type.clone(), loc, span_end)
                    }
                    _ => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(String::from("String type can only be casted to char* type.")),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: loc.line,
                                column: loc.column,
                                length: span_end,
                            }),
                        });
                        exit(1);
                    }
                },
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from("Cannot cast string value to non-pointer type.")),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
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
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_cast_expression(&self, scope: ScopeRef<'ctx>, cast: Cast) -> InternalValue<'ctx> {
        let internal_value = self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), *cast.expr.clone()));
        let target_type = self.build_type(cast.target_type, cast.loc.clone(), cast.span.end);

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if !self.compatible_types(target_type.clone(), internal_value.get_type(self.string_type.clone())) {
            // FIXME We need accurate type name tracking here
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Cannot assign value of type '{}' to lvalue of type '{}'.",
                    internal_value
                        .get_type(self.string_type.clone())
                        .to_basic_type(ptr_type),
                    target_type.to_basic_type(ptr_type)
                )),
                location: None,
            });
            exit(1);
        };

        self.build_cast_expression_internal(internal_value, target_type, cast.loc.clone(), cast.span.end)
    }

    pub(crate) fn build_prefix_expr(
        &self,
        scope: ScopeRef<'ctx>,
        unary_expression: UnaryExpression,
    ) -> InternalValue<'ctx> {
        let operand = self.build_expr(Rc::clone(&scope), *unary_expression.operand.clone());
        match unary_expression.operator.kind {
            TokenKind::Minus => match operand {
                InternalValue::IntValue(int_value, _) => InternalValue::IntValue(
                    self.builder.build_int_neg(int_value, "prefix_iminus").unwrap(),
                    int_value.get_type(),
                ),
                InternalValue::FloatValue(float_value, _) => InternalValue::FloatValue(
                    self.builder.build_float_neg(float_value, "prefix_fminus").unwrap(),
                    float_value.get_type(),
                ),
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
                InternalValue::IntValue(int_value, _) => {
                    let zero = int_value.get_type().const_int(0, false);
                    let one = int_value.get_type().const_int(1, false);

                    let is_zero = self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, int_value, zero, "prefix_ineg")
                        .unwrap();

                    if is_zero.get_zero_extended_constant().unwrap() == 0 {
                        InternalValue::IntValue(zero, int_value.get_type())
                    } else {
                        InternalValue::IntValue(one, int_value.get_type())
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

    pub(crate) fn build_unary_operator(
        &self,
        scope: ScopeRef<'ctx>,
        unary_operator: UnaryOperator,
    ) -> InternalValue<'ctx> {
        let int_one = self.context.i32_type().const_int(1, false);
        let value =
            self.internal_value_as_rvalue(self.build_module_import(Rc::clone(&scope), unary_operator.module_import));

        match value {
            InternalValue::IntValue(int_value, _) => match unary_operator.ty {
                UnaryOperatorType::PreIncrement => {
                    return InternalValue::IntValue(
                        self.builder.build_int_add(int_value, int_one, "unaryop").unwrap(),
                        int_value.get_type(),
                    );
                }
                UnaryOperatorType::PreDecrement => {
                    return InternalValue::IntValue(
                        self.builder.build_int_sub(int_value, int_one, "unaryop").unwrap(),
                        int_value.get_type(),
                    );
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
    ) -> InternalValue<'ctx> {
        let left = self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), *binary_expression.left));
        let right = self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), *binary_expression.right));

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
        if let InternalValue::IntValue(int_value, _) = self.internal_value_as_rvalue(self.build_expr(scope, expr)) {
            int_value
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
