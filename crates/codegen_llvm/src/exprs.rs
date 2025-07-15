use crate::{
    CodeGenLLVM, ScopeRef,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    types::{
        InternalArrayType, InternalBoolType, InternalFloatType, InternalIntType, InternalLvalueType,
        InternalPointerType, InternalType, InternalVoidType,
    },
    values::{InternalValue, Lvalue, TypedPointerValue},
};
use ast::{
    ast::*,
    token::{Location, Token, TokenKind},
};
use inkwell::{
    AddressSpace,
    types::{BasicType, BasicTypeEnum},
    values::{ArrayValue, BasicValue, BasicValueEnum, FloatValue, IntValue},
};
use std::{process::exit, rc::Rc};

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_expr(&mut self, scope: ScopeRef<'ctx>, expr: Expression) -> InternalValue<'ctx> {
        match expr {
            Expression::FieldAccess(field_access) => self.build_field_access(Rc::clone(&scope), field_access),
            Expression::MethodCall(method_call) => self.build_method_call(Rc::clone(&scope), method_call),
            Expression::Identifier(identifier) => self.build_lvalue(
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
            Expression::AddressOf(address_of) => self.build_address_of(Rc::clone(&scope), address_of),
            Expression::Dereference(dereference) => self.build_deref(Rc::clone(&scope), dereference),
            Expression::StructInit(struct_init) => self.build_struct_init(Rc::clone(&scope), struct_init),
            Expression::Array(array) => self.build_array(Rc::clone(&scope), array),
            Expression::ArrayIndex(array_index) => self.build_array_index(Rc::clone(&scope), array_index),
            Expression::ModuleImport(module_import) => self.build_module_import(Rc::clone(&scope), module_import),
            Expression::FuncCall(func_call) => self.build_func_call(Rc::clone(&scope), func_call),
            Expression::TypeSpecifier(_) => InternalValue::PointerValue(self.build_null()),
            Expression::UnnamedStructValue(unnamed_struct_value) => {
                self.build_unnamed_struct_value(Rc::clone(&scope), unnamed_struct_value)
            }
        }
    }

    pub(crate) fn build_address_of(&mut self, scope: ScopeRef<'ctx>, address_of: AddressOf) -> InternalValue<'ctx> {
        let internal_value = self.build_expr(Rc::clone(&scope), *address_of.expr);

        if let InternalValue::Lvalue(lvalue) = internal_value {
            return InternalValue::PointerValue(TypedPointerValue {
                type_str: format!("&{}", lvalue.pointee_ty),
                ptr: lvalue.ptr,
                pointee_ty: lvalue.pointee_ty,
            });
        }

        display_single_diag(Diag {
            level: DiagLevel::Error,
            kind: DiagKind::Custom("Cannot take the address of an rvalue.".to_string()),
            location: Some(DiagLoc {
                file: self.file_path.clone(),
                line: address_of.loc.line,
                column: address_of.loc.column,
                length: address_of.span.end,
            }),
        });
        exit(1);
    }

    pub(crate) fn build_deref_internal(
        &self,
        internal_value: InternalValue<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        match internal_value {
            InternalValue::PointerValue(pointer_value) => InternalValue::Lvalue(Lvalue {
                ptr: pointer_value.ptr,
                pointee_ty: pointer_value.pointee_ty,
            }),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot dereference non-pointer value.".to_string()),
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

    pub(crate) fn build_deref(&mut self, scope: ScopeRef<'ctx>, dereference: Dereference) -> InternalValue<'ctx> {
        let internal_value = self.build_expr(Rc::clone(&scope), *dereference.expr);
        let internal_value =
            self.internal_value_as_rvalue(internal_value, dereference.loc.clone(), dereference.span.end);

        self.build_deref_internal(internal_value, dereference.loc, dereference.span.end)
    }

    pub(crate) fn build_module_import(
        &self,
        scope: ScopeRef<'ctx>,
        module_import: ModuleImport,
    ) -> InternalValue<'ctx> {
        if module_import.segments.len() == 1 {
            return self.build_lvalue(Rc::clone(&scope), module_import);
        }

        let full_module_id = self.build_module_id(ModulePath {
            alias: None,
            segments: module_import.segments.clone(),
            loc: module_import.loc.clone(),
            span: module_import.span.clone(),
        });

        let last_segment = {
            match module_import.segments.last().unwrap() {
                ModuleSegment::SubModule(sub_module) => sub_module,
                ModuleSegment::Single(_) => unreachable!(),
            }
        };

        match match self.find_imported_module(full_module_id.clone()) {
            Some(module_metadata) => {
                return InternalValue::ModuleValue(module_metadata.metadata.clone());
            }
            None => {
                let module_id = self.build_module_id(ModulePath {
                    alias: None,
                    segments: module_import.segments[..(module_import.segments.len() - 1)].to_vec(),
                    loc: module_import.loc.clone(),
                    span: module_import.span.clone(),
                });

                match self.find_imported_module(module_id.clone()) {
                    Some(module_metadata) => {
                        match module_metadata.metadata.global_variables_table.get(&last_segment.name) {
                            Some(global_variable_metadata) => {
                                let global_value_ptr = global_variable_metadata.global_value.as_pointer_value();
                                let global_value_ptr_type = global_value_ptr.get_type();
                                return InternalValue::Lvalue(Lvalue {
                                    ptr: global_value_ptr,
                                    pointee_ty: InternalType::Lvalue(Box::new(InternalLvalueType {
                                        ptr_type: global_value_ptr_type,
                                        pointee_ty: global_variable_metadata.variable_type.clone(),
                                    })),
                                });
                            }
                            None => {
                                return InternalValue::ModuleValue(module_metadata.metadata.clone());
                            }
                        }
                    }
                    None => None,
                }
            }
        } {
            Some(internal_value) => internal_value,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::ModuleNotFound(full_module_id),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: module_import.loc.line,
                        column: module_import.loc.column,
                        length: module_import.span.end,
                    }),
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_lvalue(&self, scope: ScopeRef<'ctx>, module_import: ModuleImport) -> InternalValue<'ctx> {
        let identifier = match module_import.segments.first().unwrap() {
            ModuleSegment::SubModule(identifier) => identifier.clone(),
            ModuleSegment::Single(_) => unreachable!(),
        };

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
            } else if let Some(global_variable_metadata) = self.global_variables_table.get(&identifier.name.clone()) {
                let global_value_ptr = global_variable_metadata.global_value.as_pointer_value();
                let global_value_ptr_type = global_value_ptr.get_type();
                return InternalValue::Lvalue(Lvalue {
                    ptr: global_value_ptr,
                    pointee_ty: InternalType::Lvalue(Box::new(InternalLvalueType {
                        ptr_type: global_value_ptr_type,
                        pointee_ty: global_variable_metadata.variable_type.clone(),
                    })),
                });
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::IdentifierNotDefined(identifier.name.clone()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: module_import.loc.line,
                        column: module_import.loc.column,
                        length: module_import.span.end,
                    }),
                });
                exit(1);
            }
        } else {
            self.build_module_import(Rc::clone(&scope), module_import)
        }
    }

    pub(crate) fn build_assignment(&mut self, scope: ScopeRef<'ctx>, assignment: Box<Assignment>) {
        let assign_to = self.build_expr(Rc::clone(&scope), assignment.assign_to);
        let assign_expr = self.build_expr(Rc::clone(&scope), assignment.expr);

        match assign_to.clone() {
            InternalValue::PointerValue(typed_pointer_value) => {
                let rvalue = self.internal_value_as_rvalue(assign_expr, assignment.loc.clone(), assignment.span.end);

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
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Cannot assign value of type '{}' to lvalue of type '{}'.",
                            rvalue.get_type(self.string_type.clone()),
                            typed_pointer_value.pointee_ty
                        )),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: assignment.loc.line,
                            column: assignment.loc.column,
                            length: assignment.span.end,
                        }),
                    });
                    exit(1);
                };

                let final_rvalue = self.implicit_cast(
                    rvalue,
                    typed_pointer_value.pointee_ty,
                    assignment.loc.clone(),
                    assignment.span.end,
                );

                self.builder.build_store(typed_pointer_value.ptr, final_rvalue).unwrap();
            }
            InternalValue::Lvalue(lvalue) => {
                let rvalue = self.internal_value_as_rvalue(assign_expr, assignment.loc.clone(), assignment.span.end);

                if let InternalType::ConstType(_) = lvalue.pointee_ty {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Cannot assign to a constant lvalue.".to_string()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: assignment.loc.line,
                            column: assignment.loc.column,
                            length: assignment.span.end,
                        }),
                    });
                    exit(1);
                }

                if !self.compatible_types(lvalue.pointee_ty.clone(), rvalue.get_type(self.string_type.clone())) {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Cannot assign value of type '{}' to lvalue of type '{}'.",
                            rvalue.get_type(self.string_type.clone()),
                            lvalue.pointee_ty
                        )),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: assignment.loc.line,
                            column: assignment.loc.column,
                            length: assignment.span.end,
                        }),
                    });
                    exit(1);
                };
                let final_rvalue =
                    self.implicit_cast(rvalue, lvalue.pointee_ty, assignment.loc.clone(), assignment.span.end);
                self.builder.build_store(lvalue.ptr, final_rvalue).unwrap();
            }
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot assign to a non-pointer value.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: assignment.loc.line,
                        column: assignment.loc.column,
                        length: assignment.span.end,
                    }),
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_array(&mut self, scope: ScopeRef<'ctx>, array: Array) -> InternalValue<'ctx> {
        let mut is_const_array = true;
        let mut array_elements: Vec<InternalValue<'ctx>> = array
            .elements
            .iter()
            .map(|expr| {
                let internal_value = self.build_expr(Rc::clone(&scope), expr.clone());

                if !internal_value.is_const() {
                    is_const_array = false;
                }

                self.internal_value_as_rvalue(internal_value, array.loc.clone(), array.span.end)
            })
            .collect();

        if let TypeSpecifier::Array(array_internal_type) = array.data_type {
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let element_type = self.build_type(*array_internal_type.element_type, array.loc.clone(), array.span.end);
            let array_size = self.build_array_capacity(array_internal_type.size, array.loc.clone(), array.span.end);

            if array_elements.len() > array_size as usize {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Array construction has more elements than specified capacity that is {}.",
                        array_size
                    )),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: array.loc.line,
                        column: array.loc.column,
                        length: array.span.end,
                    }),
                });
                exit(1);
            } else if array_elements.len() != array_size as usize {
                // zeroinit for unknown fields of the array!
                for _ in array_elements.len()..(array_size as usize) {
                    let zeroinit_value = self.build_zero_initialized_internal_value(
                        element_type.clone(),
                        array.loc.clone(),
                        array.span.end,
                    );
                    array_elements.push(zeroinit_value);
                }
            }

            let array_element_values: Vec<BasicValueEnum<'ctx>> = array_elements
                .iter()
                .enumerate()
                .map(|(idx, v)| {
                    if !self.compatible_types(v.get_type(self.string_type.clone()), element_type.clone()) {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(format!(
                                "Incompatible item in array construction at index {}. Expected item with type '{}' but got '{}'.",
                                idx,
                                element_type,
                                v.get_type(self.string_type.clone())
                            )),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: array.loc.line,
                                column: array.loc.column,
                                length: array.span.end,
                            }),
                        });
                        exit(1);
                    }

                    self.implicit_cast(v.clone(), element_type.clone(), array.loc.clone(), array.span.end).as_basic_value_enum()
                })
                .collect();

            let array_value = {
                if is_const_array {
                    let basic_type = match element_type.to_basic_type(ptr_type) {
                        Ok(basic_type) => basic_type,
                        Err(err) => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom(err.to_string()),
                                location: Some(DiagLoc {
                                    file: self.file_path.clone(),
                                    line: array.loc.line,
                                    column: array.loc.column,
                                    length: array.span.end,
                                }),
                            });
                            exit(1);
                        }
                    };
                    self.build_const_array(basic_type, array_element_values)
                } else {
                    let basic_type = match element_type
                        .into_array_type(array_size.try_into().unwrap(), element_type.to_string())
                        .unwrap()
                        .to_basic_type(ptr_type)
                    {
                        Ok(basic_type) => basic_type,
                        Err(err) => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom(err.to_string()),
                                location: Some(DiagLoc {
                                    file: self.file_path.clone(),
                                    line: array.loc.line,
                                    column: array.loc.column,
                                    length: array.span.end,
                                }),
                            });
                            exit(1);
                        }
                    };
                    self.build_runtime_array(basic_type, array_element_values)
                }
            };

            let element_basic_type = match element_type.to_basic_type(ptr_type) {
                Ok(basic_type) => basic_type,
                Err(err) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(err.to_string()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: array.loc.line,
                            column: array.loc.column,
                            length: array.span.end,
                        }),
                    });
                    exit(1);
                }
            };
            return InternalValue::ArrayValue(
                array_value,
                InternalType::ArrayType(InternalArrayType {
                    type_str: element_type.to_string(),
                    inner_type: Box::new(element_type.clone()),
                    array_type: element_basic_type.array_type(array_size.try_into().unwrap()),
                }),
            );
        }

        unreachable!();
    }

    pub(crate) fn build_runtime_array(
        &self,
        array_type: BasicTypeEnum<'ctx>,
        values: Vec<BasicValueEnum<'ctx>>,
    ) -> ArrayValue<'ctx> {
        let zero_array_value = array_type.into_array_type().const_zero();
        let array_alloca = self.builder.build_alloca(array_type.clone(), "array").unwrap();
        self.builder.build_store(array_alloca, zero_array_value).unwrap();

        for (idx, value) in values.iter().enumerate() {
            let mut ordered_indexes: Vec<IntValue<'ctx>> = Vec::new();
            ordered_indexes.push(self.build_index_value(0)); // first index is always 0
            ordered_indexes.push(self.build_index_value(idx)); // add item index
            let element_pointer = unsafe {
                self.builder
                    .build_in_bounds_gep(array_type, array_alloca, &ordered_indexes, "gep")
                    .unwrap()
            };
            self.builder.build_store(element_pointer, value.clone()).unwrap();
        }

        let array_value = self.builder.build_load(array_type, array_alloca, "load").unwrap();
        array_value.into_array_value()
    }

    pub(crate) fn build_const_array(
        &self,
        element_type: BasicTypeEnum<'ctx>,
        values: Vec<BasicValueEnum<'ctx>>,
    ) -> ArrayValue<'ctx> {
        unsafe { ArrayValue::new_const_array(&element_type, &values) }
    }

    pub(crate) fn build_array_index(&mut self, scope: ScopeRef<'ctx>, array_index: ArrayIndex) -> InternalValue<'ctx> {
        let (pointer, pointee_ty) = match self.build_expr(Rc::clone(&scope), *array_index.expr) {
            InternalValue::PointerValue(typed_pointer_value) => {
                (typed_pointer_value.ptr, typed_pointer_value.pointee_ty)
            }
            InternalValue::Lvalue(lvalue) => (lvalue.ptr, lvalue.pointee_ty),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build array index to a non-array value.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: array_index.loc.line,
                        column: array_index.loc.column,
                        length: array_index.span.end,
                    }),
                });
                exit(1);
            }
        };

        if !pointee_ty.is_array_type() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Cannot build array index to a non-array value.".to_string()),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: array_index.loc.line,
                    column: array_index.loc.column,
                    length: array_index.span.end,
                }),
            });
            exit(1);
        }

        if let InternalType::ArrayType(internal_array_type) = pointee_ty.clone() {
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let mut ordered_indexes: Vec<IntValue> = Vec::new();
            ordered_indexes.push(self.build_index_value(0)); // first index is always 0

            let index_expr = self.build_expr(Rc::clone(&scope), *array_index.index);
            if let InternalValue::IntValue(index_int_value, index_int_value_type) =
                self.internal_value_as_rvalue(index_expr, array_index.loc.clone(), array_index.span.end)
            {
                if index_int_value.is_const()
                    && internal_array_type.array_type.len() - 1
                        < index_int_value.get_zero_extended_constant().unwrap() as u32
                {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Index {} is out of bounds for array of size {}.",
                            index_int_value.get_zero_extended_constant().unwrap(),
                            internal_array_type.array_type.len(),
                        )),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: array_index.loc.line,
                            column: array_index.loc.column,
                            length: array_index.span.end,
                        }),
                    });
                    exit(1);
                } else if !index_int_value.is_const() {
                    return self.build_runtime_inbounds_check(
                        pointer,
                        pointee_ty,
                        InternalValue::IntValue(index_int_value, index_int_value_type),
                        internal_array_type.array_type.len(),
                        array_index.loc.clone(),
                        array_index.span.end,
                    );
                }

                ordered_indexes.push(index_int_value);
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build array indexing with a non-integer index.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: array_index.loc.line,
                        column: array_index.loc.column,
                        length: array_index.span.end,
                    }),
                });
                exit(1);
            }

            let element_basic_type = match pointee_ty.to_basic_type(ptr_type) {
                Ok(basic_type) => basic_type,
                Err(err) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(err.to_string()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: array_index.loc.line,
                            column: array_index.loc.column,
                            length: array_index.span.end,
                        }),
                    });
                    exit(1);
                }
            };

            let element_pointer = unsafe {
                self.builder
                    .build_in_bounds_gep(element_basic_type, pointer, &ordered_indexes, "gep")
                    .unwrap()
            };

            InternalValue::Lvalue(Lvalue {
                ptr: element_pointer,
                pointee_ty: *internal_array_type.inner_type,
            })
        } else {
            unreachable!();
        }
    }

    pub(crate) fn build_literal(&self, literal: Literal) -> InternalValue<'ctx> {
        match literal.kind {
            LiteralKind::Integer(integer_literal) => self.build_integer_literal(
                integer_literal,
                Token {
                    kind: TokenKind::Int32,
                    span: literal.span.clone(),
                    loc: literal.loc.clone(),
                },
            ),
            LiteralKind::Float(float_literal) => {
                let value = self.build_float_literal(float_literal);
                InternalValue::FloatValue(
                    value,
                    InternalType::FloatType(InternalFloatType {
                        type_str: "float32".to_string(),
                        float_type: value.get_type(),
                    }),
                )
            }
            LiteralKind::Bool(bool_literal) => {
                let value = self.build_bool_literal(bool_literal);
                InternalValue::BoolValue(
                    value,
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: value.get_type(),
                    }),
                )
            }
            LiteralKind::Char(char_literal) => {
                let value = self.build_char_literal(char_literal);
                InternalValue::IntValue(
                    value,
                    InternalType::IntType(InternalIntType {
                        type_str: "char".to_string(),
                        int_kind: TokenKind::Char,
                        int_type: value.get_type(),
                    }),
                )
            }
            LiteralKind::String(string_literal, prefix) => match prefix {
                Some(prefix) => match prefix {
                    StringPrefix::C => self.build_c_style_string(string_literal, Location::default(), 0),
                    StringPrefix::B => self.build_byte_string(string_literal, Location::default(), 0),
                },
                None => self.build_string_literal(string_literal, Location::default(), 0),
            },
            LiteralKind::Null => InternalValue::PointerValue(self.build_null()),
        }
    }

    pub(crate) fn build_index_value(&self, index_value: usize) -> IntValue<'ctx> {
        let data_layout = self.target_machine.get_target_data();
        let ptr_sized_int_type = self.context.ptr_sized_int_type(&data_layout, None);
        ptr_sized_int_type.const_int(index_value.try_into().unwrap(), false)
    }

    pub(crate) fn is_integer_signed(&self, token_kind: TokenKind) -> bool {
        match token_kind {
            TokenKind::IntPtr
            | TokenKind::Int
            | TokenKind::Int8
            | TokenKind::Int16
            | TokenKind::Int32
            | TokenKind::Int64
            | TokenKind::Int128 => return true,
            TokenKind::UIntPtr
            | TokenKind::SizeT
            | TokenKind::UInt
            | TokenKind::UInt8
            | TokenKind::UInt16
            | TokenKind::UInt32
            | TokenKind::UInt64
            | TokenKind::UInt128 => return false,
            _ => {
                panic!("Given TypeToken is not a type of integer kind.");
            }
        }
    }

    pub(crate) fn build_integer_literal(&self, value: i64, token: Token) -> InternalValue<'ctx> {
        let sign_extend = self.is_integer_signed(token.kind.clone());
        let internal_type = self.build_type_token(token.clone(), token.loc);
        InternalValue::IntValue(
            internal_type
                .to_basic_type(self.context.ptr_type(AddressSpace::default()))
                .unwrap()
                .into_int_type()
                .const_int(value.try_into().unwrap(), sign_extend),
            internal_type,
        )
    }

    pub(crate) fn build_float_literal(&self, value: f64) -> FloatValue<'ctx> {
        self.context.f32_type().const_float(value)
    }

    pub(crate) fn build_char_literal(&self, value: char) -> IntValue<'ctx> {
        self.context.i8_type().const_int(value as u64, false)
    }

    pub(crate) fn build_null(&self) -> TypedPointerValue<'ctx> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        TypedPointerValue {
            type_str: "null".to_string(),
            ptr: ptr_type.const_null(),
            pointee_ty: InternalType::PointerType(Box::new(InternalPointerType {
                ptr_type,
                pointee_ty: InternalType::IntType(InternalIntType {
                    type_str: "null".to_string(),
                    int_kind: TokenKind::Int32,
                    int_type: self.context.i32_type(),
                }),
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
                InternalType::PointerType(internal_pointer_type) => {
                    let pointer = self
                        .builder
                        .build_int_to_ptr(int_value, internal_pointer_type.ptr_type, "inttoptr")
                        .unwrap();

                    InternalValue::PointerValue(TypedPointerValue {
                        type_str: format!("{}*", internal_pointer_type.pointee_ty.to_string()),
                        ptr: pointer,
                        pointee_ty: internal_pointer_type.pointee_ty,
                    })
                }
                InternalType::IntType(internal_int_type) => {
                    let is_signed = self.is_integer_signed(internal_int_type.int_kind.clone());

                    InternalValue::IntValue(
                        self.builder
                            .build_int_cast_sign_flag(int_value, internal_int_type.int_type, is_signed, "cast")
                            .unwrap(),
                        InternalType::IntType(internal_int_type),
                    )
                }
                InternalType::FloatType(internal_float_type) => InternalValue::FloatValue(
                    self.builder
                        .build_signed_int_to_float(int_value, internal_float_type.float_type, "cast")
                        .unwrap(),
                    InternalType::FloatType(internal_float_type),
                ),
                InternalType::ConstType(internal_const_type) => {
                    self.build_cast_expression_internal(internal_value, *internal_const_type.inner_type, loc, span_end)
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
                InternalType::IntType(internal_int_type) => InternalValue::IntValue(
                    self.builder
                        .build_float_to_signed_int(float_value, internal_int_type.int_type, "cast")
                        .unwrap(),
                    InternalType::IntType(internal_int_type),
                ),
                InternalType::FloatType(internal_float_type) => InternalValue::FloatValue(
                    self.builder
                        .build_float_cast(float_value, internal_float_type.float_type, "cast")
                        .unwrap(),
                    InternalType::FloatType(internal_float_type),
                ),
                InternalType::ConstType(internal_const_type) => {
                    self.build_cast_expression_internal(internal_value, *internal_const_type.inner_type, loc, span_end)
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
                InternalType::IntType(internal_int_type) => {
                    let intptr = self
                        .builder
                        .build_ptr_to_int(pointer_value.ptr, internal_int_type.int_type, "ptrtoint")
                        .unwrap();
                    InternalValue::IntValue(intptr, InternalType::IntType(internal_int_type))
                }
                InternalType::StringType(_) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from(
                            "Cannot construct string from a 'char*' value. Consider to use '.to_string()' method on 'char*' value.",
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
                InternalType::PointerType(internal_pointer_type) => {
                    let casted_ptr = self
                        .builder
                        .build_pointer_cast(pointer_value.ptr, internal_pointer_type.ptr_type, "cast_ptr")
                        .unwrap();

                    InternalValue::PointerValue(TypedPointerValue {
                        type_str: format!("{}*", internal_pointer_type.pointee_ty.to_string()),
                        ptr: casted_ptr,
                        pointee_ty: internal_pointer_type.pointee_ty,
                    })
                }
                InternalType::ConstType(internal_const_type) => {
                    self.build_cast_expression_internal(internal_value, *internal_const_type.inner_type, loc, span_end)
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
                InternalType::PointerType(internal_pointer_type) => match &internal_pointer_type.pointee_ty {
                    InternalType::IntType(_) => self.build_load_string(string_value.clone()),
                    InternalType::ConstType(internal_const_type) => match *internal_const_type.inner_type.clone() {
                        InternalType::IntType(_) => match self.build_load_string(string_value.clone()) {
                            InternalValue::PointerValue(typed_pointer_value) => {
                                InternalValue::PointerValue(TypedPointerValue {
                                    type_str: "const char*".to_string(),
                                    ptr: typed_pointer_value.ptr,
                                    pointee_ty: InternalType::VoidType(InternalVoidType {
                                        type_str: "const char".to_string(),
                                        void_type: self.context.void_type(),
                                    }),
                                })
                            }
                            _ => unreachable!(),
                        },
                        _ => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom(String::from(
                                    "String type cannot be casted to any type except 'const char*'.",
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
                    },
                    _ => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(String::from(
                                "String type cannot be casted to any type except 'char*'.",
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
                },
                InternalType::ConstType(_) => internal_value,
                InternalType::StringType(_) => internal_value,
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
            _ => internal_value,
        }
    }

    pub(crate) fn build_cast_expression(&mut self, scope: ScopeRef<'ctx>, cast: Cast) -> InternalValue<'ctx> {
        let cast_expr = self.build_expr(Rc::clone(&scope), *cast.expr.clone());
        let internal_value = self.internal_value_as_rvalue(cast_expr, cast.loc.clone(), cast.span.end);
        let target_type = self.build_type(cast.target_type, cast.loc.clone(), cast.span.end);

        if !self.compatible_types(target_type.clone(), internal_value.get_type(self.string_type.clone())) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Cannot cast value of type '{}' to lvalue of type '{}'.",
                    internal_value.get_type(self.string_type.clone()),
                    target_type
                )),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: cast.loc.line,
                    column: cast.loc.column,
                    length: cast.span.end,
                }),
            });
            exit(1);
        };

        if matches!(
            target_type,
            InternalType::StructType(..)
                | InternalType::VectorType(..)
                | InternalType::VoidType(..)
                | InternalType::Lvalue(..),
        ) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!("Cannot cast value of type '{}'.", target_type)),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: cast.loc.line,
                    column: cast.loc.column,
                    length: cast.span.end,
                }),
            });
            exit(1);
        }

        self.build_cast_expression_internal(internal_value, target_type, cast.loc.clone(), cast.span.end)
    }

    pub(crate) fn build_sizeof_operator_internal(
        &self,
        internal_type: InternalType<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> IntValue<'ctx> {
        match internal_type {
            InternalType::BoolType(internal_bool_type) => internal_bool_type.bool_type.size_of(),
            InternalType::IntType(internal_int_type) => internal_int_type.int_type.size_of(),
            InternalType::FloatType(internal_float_type) => internal_float_type.float_type.size_of(),
            InternalType::ArrayPtrType(internal_array_ptr_type) => internal_array_ptr_type.ptr_type.size_of(),
            InternalType::StructType(internal_struct_type) => {
                match internal_struct_type.struct_metadata.struct_type.size_of() {
                    Some(size) => size,
                    None => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::SizeOfOperatorOnUnsizedObject,
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
                match internal_unnamed_struct_type
                    .unnamed_struct_metadata
                    .struct_type
                    .size_of()
                {
                    Some(size) => size,
                    None => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::SizeOfOperatorOnUnsizedObject,
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
            InternalType::VectorType(internal_vector_type) => match internal_vector_type.vector_type.size_of() {
                Some(size) => size,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::SizeOfOperatorOnUnsizedObject,
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
            InternalType::StringType(string_type) => string_type.struct_type.size_of().unwrap(),
            InternalType::VoidType(_) => self.build_index_value(0),
            InternalType::PointerType(internal_pointer_type) => internal_pointer_type.ptr_type.size_of(),
            InternalType::ConstType(internal_const_type) => {
                self.build_sizeof_operator_internal(*internal_const_type.inner_type, loc, span_end)
            }
            InternalType::Lvalue(lvalue_type) => lvalue_type.ptr_type.size_of(),
            InternalType::ArrayType(internal_array_type) => match internal_array_type.array_type.size_of() {
                Some(size) => size,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::SizeOfOperatorOnUnsizedObject,
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
        }
    }

    pub(crate) fn build_sizeof_operator(
        &mut self,
        scope: ScopeRef<'ctx>,
        expr: Expression,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        let expr = self.build_expr(scope, expr);
        let internal_value = self.internal_value_as_rvalue(expr, loc.clone(), span_end);
        let internal_type = internal_value.get_type(self.string_type.clone());
        let size_int_value = self.build_sizeof_operator_internal(internal_type, loc, span_end);
        InternalValue::IntValue(
            size_int_value,
            InternalType::IntType(InternalIntType {
                type_str: "size_t".to_string(),
                int_kind: TokenKind::SizeT,
                int_type: size_int_value.get_type(),
            }),
        )
    }

    pub(crate) fn build_prefix_expr(
        &mut self,
        scope: ScopeRef<'ctx>,
        unary_expression: UnaryExpression,
    ) -> InternalValue<'ctx> {
        let expr = self.build_expr(Rc::clone(&scope), *unary_expression.operand.clone());
        let operand_internal_value =
            self.internal_value_as_rvalue(expr, unary_expression.loc.clone(), unary_expression.span.end);

        match unary_expression.operator.kind {
            TokenKind::SizeOf => self.build_sizeof_operator(
                Rc::clone(&scope),
                *unary_expression.operand,
                unary_expression.loc.clone(),
                unary_expression.span.end,
            ),
            TokenKind::Minus => match operand_internal_value {
                InternalValue::IntValue(int_value, _) => {
                    let operand_internal_type = operand_internal_value.get_type(self.string_type.clone());
                    InternalValue::IntValue(
                        self.builder.build_int_neg(int_value, "prefix_iminus").unwrap(),
                        InternalType::IntType(InternalIntType {
                            type_str: operand_internal_type.to_string(),
                            int_kind: match operand_internal_type {
                                InternalType::IntType(internal_int_type) => internal_int_type.int_kind,
                                _ => unreachable!(),
                            },
                            int_type: int_value.get_type(),
                        }),
                    )
                }
                InternalValue::FloatValue(float_value, _) => InternalValue::FloatValue(
                    self.builder.build_float_neg(float_value, "prefix_fminus").unwrap(),
                    InternalType::FloatType(InternalFloatType {
                        type_str: operand_internal_value.get_type(self.string_type.clone()).to_string(),
                        float_type: float_value.get_type(),
                    }),
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
            TokenKind::Bang => match operand_internal_value {
                InternalValue::IntValue(int_value, _) => {
                    let zero = int_value.get_type().const_int(0, false);
                    let one = int_value.get_type().const_int(1, false);

                    let is_zero = self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, int_value, zero, "prefix_ineg")
                        .unwrap();

                    if is_zero.get_zero_extended_constant().unwrap() == 0 {
                        InternalValue::IntValue(
                            zero,
                            InternalType::BoolType(InternalBoolType {
                                type_str: "bool".to_string(),
                                bool_type: int_value.get_type(),
                            }),
                        )
                    } else {
                        InternalValue::IntValue(
                            one,
                            InternalType::BoolType(InternalBoolType {
                                type_str: "bool".to_string(),
                                bool_type: int_value.get_type(),
                            }),
                        )
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
        let operand_internal_value = self.build_module_import(Rc::clone(&scope), unary_operator.module_import);

        let int_one_value = self.build_index_value(1);
        let int_one = self
            .implicit_cast(
                InternalValue::IntValue(
                    int_one_value,
                    InternalType::IntType(InternalIntType {
                        type_str: operand_internal_value.get_type(self.string_type.clone()).to_string(),
                        int_kind: TokenKind::Int32,
                        int_type: int_one_value.get_type(),
                    }),
                ),
                self.internal_value_as_rvalue(
                    operand_internal_value.clone(),
                    unary_operator.loc.clone(),
                    unary_operator.span.end,
                )
                .get_type(self.string_type.clone()),
                unary_operator.loc.clone(),
                unary_operator.span.end,
            )
            .into_int_value();

        let (ptr, _) = match operand_internal_value.clone() {
            InternalValue::Lvalue(lvalue) => (lvalue.ptr, lvalue.pointee_ty),
            InternalValue::PointerValue(typed_pointer_value) => {
                (typed_pointer_value.ptr, typed_pointer_value.pointee_ty)
            }
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot apply unary operator to a non-lvalue.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: unary_operator.loc.line,
                        column: unary_operator.loc.column,
                        length: unary_operator.span.end,
                    }),
                });
                exit(1);
            }
        };

        match self.internal_value_as_rvalue(
            operand_internal_value.clone(),
            unary_operator.loc.clone(),
            unary_operator.span.end,
        ) {
            InternalValue::IntValue(int_value, _) => match unary_operator.ty {
                UnaryOperatorType::PreIncrement => {
                    let rvalue = self.builder.build_int_add(int_value, int_one, "unaryop").unwrap();
                    self.build_store(
                        ptr,
                        InternalValue::IntValue(
                            rvalue.clone(),
                            InternalType::IntType(InternalIntType {
                                type_str: "int32".to_string(),
                                int_kind: TokenKind::Int32,
                                int_type: rvalue.get_type(),
                            }),
                        ),
                    );
                    InternalValue::IntValue(
                        rvalue,
                        InternalType::IntType(InternalIntType {
                            type_str: "int32".to_string(),
                            int_kind: TokenKind::Int32,
                            int_type: rvalue.get_type(),
                        }),
                    )
                }
                UnaryOperatorType::PreDecrement => {
                    let rvalue = self.builder.build_int_sub(int_value, int_one, "unaryop").unwrap();
                    self.build_store(
                        ptr,
                        InternalValue::IntValue(
                            rvalue.clone(),
                            InternalType::IntType(InternalIntType {
                                type_str: "int32".to_string(),
                                int_kind: TokenKind::Int32,
                                int_type: rvalue.get_type(),
                            }),
                        ),
                    );
                    InternalValue::IntValue(
                        rvalue,
                        InternalType::IntType(InternalIntType {
                            type_str: "int32".to_string(),
                            int_kind: TokenKind::Int32,
                            int_type: rvalue.get_type(),
                        }),
                    )
                }
                UnaryOperatorType::PostIncrement => {
                    let rvalue = self
                        .builder
                        .build_int_add(int_value.clone(), int_one, "unaryop")
                        .unwrap();
                    self.build_store(
                        ptr,
                        InternalValue::IntValue(
                            rvalue.clone(),
                            InternalType::IntType(InternalIntType {
                                type_str: "int32".to_string(),
                                int_kind: TokenKind::Int32,
                                int_type: rvalue.get_type(),
                            }),
                        ),
                    );
                    return InternalValue::IntValue(
                        int_value,
                        InternalType::IntType(InternalIntType {
                            type_str: "int32".to_string(),
                            int_kind: TokenKind::Int32,
                            int_type: rvalue.get_type(),
                        }),
                    );
                }
                UnaryOperatorType::PostDecrement => {
                    let rvalue = self
                        .builder
                        .build_int_sub(int_value.clone(), int_one, "unaryop")
                        .unwrap();
                    self.build_store(
                        ptr,
                        InternalValue::IntValue(
                            rvalue.clone(),
                            InternalType::IntType(InternalIntType {
                                type_str: "int32".to_string(),
                                int_kind: TokenKind::Int32,
                                int_type: rvalue.get_type(),
                            }),
                        ),
                    );
                    return InternalValue::IntValue(
                        int_value,
                        InternalType::IntType(InternalIntType {
                            type_str: "int32".to_string(),
                            int_kind: TokenKind::Int32,
                            int_type: rvalue.get_type(),
                        }),
                    );
                }
            },
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(String::from("Cannot build unary operator for non-integer value.")),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: unary_operator.loc.line,
                        column: unary_operator.loc.column,
                        length: unary_operator.span.end,
                    }),
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_infix_expr(
        &mut self,
        scope: ScopeRef<'ctx>,
        binary_expression: BinaryExpression,
    ) -> InternalValue<'ctx> {
        let left_expr = self.build_expr(Rc::clone(&scope), *binary_expression.left);
        let left = self.internal_value_as_rvalue(left_expr, binary_expression.loc.clone(), binary_expression.span.end);

        let right_expr = self.build_expr(Rc::clone(&scope), *binary_expression.right);
        let mut right =
            self.internal_value_as_rvalue(right_expr, binary_expression.loc.clone(), binary_expression.span.end);

        if !self.compatible_types(
            left.get_type(self.string_type.clone()),
            right.get_type(self.string_type.clone()),
        ) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Incompatible types for binary operation: '{}' and '{}'.",
                    left.get_type(self.string_type.clone()),
                    right.get_type(self.string_type.clone())
                )),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: binary_expression.loc.line,
                    column: binary_expression.loc.column,
                    length: binary_expression.span.end,
                }),
            });
            exit(1);
        }

        right = self.new_internal_value(
            self.implicit_cast(
                right,
                left.get_type(self.string_type.clone()),
                binary_expression.loc.clone(),
                binary_expression.span.end,
            ),
            left.get_type(self.string_type.clone()),
        );

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
            TokenKind::Or => self.bin_op_logical_or(left, right),
            TokenKind::And => self.bin_op_logical_and(left, right),
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

        match result {
            Ok(value) => value,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(err),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: binary_expression.loc.line,
                        column: binary_expression.loc.column,
                        length: binary_expression.span.end,
                    }),
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_cond(
        &mut self,
        scope: ScopeRef<'ctx>,
        expr: Expression,
        loc: Location,
        span_end: usize,
    ) -> IntValue<'ctx> {
        let expr = self.build_expr(scope, expr);
        let condition = self.internal_value_as_rvalue(expr, loc.clone(), span_end);

        if !condition.get_type(self.string_type.clone()).is_bool_type() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Condition result must be an bool value.".to_string()),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: loc.line,
                    column: loc.column,
                    length: span_end,
                }),
            });
            exit(1);
        }

        match condition {
            InternalValue::BoolValue(int_value, ..) => int_value,
            InternalValue::IntValue(int_value, ..) => int_value,
            _ => {
                unreachable!()
            }
        }
    }
}
