use crate::{
    CodeGenLLVM, ScopeRef,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
};
use ast::{ast::*, token::TokenKind};
use inkwell::{
    AddressSpace,
    llvm_sys::{
        core::{LLVMBuildGEP2, LLVMBuildPointerCast, LLVMGetElementType},
        prelude::LLVMValueRef,
    },
    types::{AnyTypeEnum, AsTypeRef, BasicType, BasicTypeEnum},
    values::{AnyValueEnum, ArrayValue, AsValueRef, BasicValueEnum, FloatValue, IntValue, PointerValue},
};
use std::{ffi::CString, process::exit, rc::Rc};

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_expr(&self, scope: ScopeRef, expr: Expression) -> AnyValueEnum {
        match expr {
            Expression::Identifier(identifier) => self.build_load_value(Rc::clone(&scope), identifier).0,
            Expression::Assignment(assignment) => {
                self.build_assignment(Rc::clone(&scope), assignment);
                AnyValueEnum::PointerValue(self.build_null_literal())
            }
            Expression::Literal(literal) => self.build_literal(literal),
            Expression::Prefix(unary_expression) => self.build_prefix_expr(Rc::clone(&scope), unary_expression),
            Expression::Infix(binary_expression) => self.build_infix_expr(Rc::clone(&scope), binary_expression),
            Expression::UnaryOperator(unary_operator) => self.build_unary_operator(Rc::clone(&scope), unary_operator),
            Expression::CastAs(cast_as) => self.build_cast_as(Rc::clone(&scope), cast_as),
            Expression::FieldAccessOrMethodCall(field_access_or_method_calls) => {
                self.build_field_access_or_method_call(Rc::clone(&scope), field_access_or_method_calls)
            }
            Expression::AddressOf(expr) => self.build_address_of(Rc::clone(&scope), *expr),
            Expression::Dereference(expression) => self.build_deref(Rc::clone(&scope), *expression),
            Expression::StructInit(struct_init) => todo!(),
            Expression::Array(array) => self.build_array(Rc::clone(&scope), array),
            Expression::ArrayIndex(array_index) => self.build_array_index(Rc::clone(&scope), array_index),
            Expression::ArrayIndexAssign(array_index_assign) => {
                self.build_array_index_assign(Rc::clone(&scope), *array_index_assign);
                AnyValueEnum::PointerValue(self.build_null_literal())
            }
            Expression::ModuleImport(module_import) => self.build_module_import(Rc::clone(&scope), module_import).0,
        }
    }

    pub(crate) fn build_address_of(&self, scope: ScopeRef, expr: Expression) -> AnyValueEnum {
        let any_value = self.build_expr(Rc::clone(&scope), expr);
        let ptr = self
            .builder
            .build_alloca(self.context.ptr_type(AddressSpace::default()), "addr_of")
            .unwrap();
        self.build_store(ptr, any_value);
        AnyValueEnum::PointerValue(ptr)
    }

    pub(crate) fn build_deref(&self, scope: ScopeRef, expr: Expression) -> AnyValueEnum {
        let any_value = self.build_expr(Rc::clone(&scope), expr);
        match any_value {
            AnyValueEnum::PointerValue(pointer_value) => {
                let pointee_ty = self.context.ptr_type(AddressSpace::default());
                self.builder
                    .build_load(pointee_ty, pointer_value, "deref")
                    .unwrap()
                    .into()
            }
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

    pub(crate) fn build_unary_operator(&self, scope: ScopeRef, unary_operator: UnaryOperator) -> AnyValueEnum {
        let int_one = self.context.i32_type().const_int(1, false);
        let value = self
            .build_module_import(Rc::clone(&scope), unary_operator.module_import)
            .0;
        match value {
            AnyValueEnum::IntValue(int_value) => match unary_operator.ty {
                UnaryOperatorType::PreIncrement => {
                    return AnyValueEnum::IntValue(self.builder.build_int_add(int_value, int_one, "unaryop").unwrap());
                }
                UnaryOperatorType::PreDecrement => {
                    return AnyValueEnum::IntValue(self.builder.build_int_sub(int_value, int_one, "unaryop").unwrap());
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

    pub(crate) fn build_module_import(
        &self,
        scope: ScopeRef,
        module_import: ModuleImport,
    ) -> (AnyValueEnum, BasicTypeEnum) {
        if module_import.sub_modules.is_empty() {
            return self.build_load_value(Rc::clone(&scope), module_import.identifier);
        }

        todo!();
    }

    pub(crate) fn build_load_ptr(&self, scope: ScopeRef, module_import: ModuleImport) -> (AnyValueEnum, BasicTypeEnum) {
        let binding = {
            match scope.borrow_mut().get(module_import.identifier.name.clone()) {
                Some(record) => record,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::IdentifierNotDefined(module_import.identifier.name),
                        location: None,
                    });
                    exit(1);
                }
            }
        };
        let record = binding.borrow_mut();

        (unsafe { AnyValueEnum::new(record.ptr) }, unsafe {
            BasicTypeEnum::new(record.ty)
        })
    }

    pub(crate) fn build_load_value(&self, scope: ScopeRef, identifier: Identifier) -> (AnyValueEnum, BasicTypeEnum) {
        let binding: Rc<std::cell::RefCell<crate::scope::ScopeRecord>> = {
            match scope.borrow_mut().get(identifier.name.clone()) {
                Some(record) => record,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::IdentifierNotDefined(identifier.name),
                        location: None,
                    });
                    exit(1);
                }
            }
        };
        let record = binding.borrow_mut();

        let ptr = unsafe { PointerValue::new(record.ptr) };
        let pointee_ty = self.context.ptr_type(AddressSpace::default());
        let value = self.builder.build_load(pointee_ty, ptr, "load").unwrap();
        (value.into(), unsafe { BasicTypeEnum::new(record.ty) })
    }

    pub(crate) fn build_assignment(&self, scope: ScopeRef, assignment: Box<Assignment>) {
        let (ptr, _) = self.build_load_ptr(Rc::clone(&scope), assignment.module_import);
        let value = self.build_expr(Rc::clone(&scope), assignment.expr);
        if let AnyValueEnum::PointerValue(ptr) = ptr {
            self.build_store(ptr, value);
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Cannot store value in non-pointer allocation.".to_string()),
                location: None,
            });
            exit(1);
        }
    }

    pub(crate) fn build_array(&self, scope: ScopeRef, array: Array) -> AnyValueEnum {
        let elements: Vec<BasicValueEnum> = array
            .elements
            .iter()
            .map(|item| unsafe { BasicValueEnum::new(self.build_expr(Rc::clone(&scope), item.clone()).as_value_ref()) })
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
        AnyValueEnum::ArrayValue(array_type.const_array(&[array_elements]))
    }

    pub(crate) fn build_ordered_indexes(&self, scope: ScopeRef, dimensions: Vec<Expression>) -> Vec<IntValue<'_>> {
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

                if let AnyValueEnum::IntValue(index) = self.build_expr(Rc::clone(&scope), array.elements[0].clone()) {
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

    pub(crate) fn build_array_index(&self, scope: ScopeRef, array_index: ArrayIndex) -> AnyValueEnum {
        let (any_value, pointee_ty) = self.build_load_ptr(Rc::clone(&scope), array_index.module_import);

        if let AnyValueEnum::PointerValue(array_ptr) = any_value {
            let ordered_indexes = self.build_ordered_indexes(Rc::clone(&scope), array_index.dimensions);

            let index_ptr = unsafe {
                let name = CString::new("gep").unwrap();
                let mut indices: Vec<LLVMValueRef> = ordered_indexes.iter().map(|item| item.as_value_ref()).collect();
                PointerValue::new(LLVMBuildGEP2(
                    self.builder.as_mut_ptr(),
                    pointee_ty.as_type_ref(),
                    array_ptr.as_value_ref(),
                    indices.as_mut_ptr(),
                    ordered_indexes.len().try_into().unwrap(),
                    name.as_ptr(),
                ))
            };

            let element_type = unsafe { LLVMGetElementType(pointee_ty.as_type_ref()) };

            let index_value = self
                .builder
                .build_load(unsafe { BasicTypeEnum::new(element_type) }, index_ptr, "load")
                .unwrap();

            index_value.into()
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Cannot apply array indexing to a non-array pointer type.".to_string()),
                location: None,
            });
            exit(1);
        }
    }

    pub(crate) fn build_array_index_assign(&self, scope: ScopeRef, array_index_assign: ArrayIndexAssign) {
        todo!();
    }

    pub(crate) fn build_literal(&self, literal: Literal) -> AnyValueEnum {
        match literal {
            Literal::Integer(integer_literal) => AnyValueEnum::IntValue(self.build_integer_literal(integer_literal)),
            Literal::Float(float_literal) => AnyValueEnum::FloatValue(self.build_float_literal(float_literal)),
            Literal::Bool(bool_literal) => AnyValueEnum::IntValue(self.build_bool_literal(bool_literal)),
            Literal::String(string_literal) => AnyValueEnum::PointerValue(self.build_string_literal(string_literal)),
            Literal::Char(char_literal) => AnyValueEnum::IntValue(self.build_char_literal(char_literal)),
            Literal::Null => AnyValueEnum::PointerValue(self.build_null_literal()),
        }
    }

    pub(crate) fn build_integer_literal(&self, integer_literal: IntegerLiteral) -> IntValue {
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
            IntegerLiteral::SizeT(val) => todo!(),
        }
    }

    pub(crate) fn build_float_literal(&self, float_literal: FloatLiteral) -> FloatValue {
        match float_literal {
            FloatLiteral::Float(val) => self.context.f32_type().const_float(val.into()),
            FloatLiteral::Double(val) => self.context.f64_type().const_float(val),
        }
    }

    pub(crate) fn build_string_literal(&self, string_literal: StringLiteral) -> PointerValue {
        let mut bytes = self.unescape_string(&string_literal.raw).into_bytes();
        bytes.push(0); // null terminator

        let i8_array_type = self.context.i8_type().array_type(bytes.len() as u32);

        let string_global = self
            .module
            .add_global(i8_array_type, Some(AddressSpace::default()), ".str");

        let const_string = self.context.const_string(&bytes, false);
        string_global.set_initializer(&const_string);
        string_global.set_constant(true);
        string_global.set_linkage(inkwell::module::Linkage::Private);

        string_global.as_pointer_value()
    }

    pub(crate) fn build_char_literal(&self, char_literal: CharLiteral) -> IntValue {
        self.context.i8_type().const_int(char_literal.raw as u8 as u64, false)
    }

    pub(crate) fn build_null_literal(&self) -> PointerValue {
        self.context.ptr_type(AddressSpace::default()).const_null()
    }

    pub(crate) fn build_bool_literal(&self, bool_literal: BoolLiteral) -> IntValue {
        self.context
            .bool_type()
            .const_int(if bool_literal.raw { 1 } else { 0 }, false)
    }

    pub(crate) fn build_cast_as(&self, scope: ScopeRef, cast_as: CastAs) -> AnyValueEnum {
        let expr = self.build_expr(Rc::clone(&scope), *cast_as.expr.clone());
        let target = self.build_type(cast_as.type_token, cast_as.loc.clone(), cast_as.span.end);

        match expr {
            AnyValueEnum::IntValue(int_value) => match target {
                AnyTypeEnum::IntType(int_type) => {
                    AnyValueEnum::IntValue(self.builder.build_int_cast(int_value, int_type, "cast").unwrap())
                }
                AnyTypeEnum::FloatType(float_type) => AnyValueEnum::FloatValue(
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
            AnyValueEnum::FloatValue(float_value) => match target {
                AnyTypeEnum::IntType(int_type) => AnyValueEnum::IntValue(
                    self.builder
                        .build_float_to_signed_int(float_value, int_type, "cast")
                        .unwrap(),
                ),
                AnyTypeEnum::FloatType(float_type) => {
                    AnyValueEnum::FloatValue(self.builder.build_float_cast(float_value, float_type, "cast").unwrap())
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
            AnyValueEnum::PointerValue(pointer_value) => {
                let name = CString::new("cast_ptr").unwrap();
                let ptr = unsafe {
                    LLVMBuildPointerCast(
                        self.builder.as_mut_ptr(),
                        pointer_value.as_value_ref(),
                        target.as_type_ref(),
                        name.as_ptr(),
                    )
                };
                AnyValueEnum::PointerValue(unsafe { PointerValue::new(ptr) })
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
        }
    }

    pub(crate) fn build_prefix_expr(&self, scope: ScopeRef, unary_expression: UnaryExpression) -> AnyValueEnum {
        let operand = self.build_expr(Rc::clone(&scope), *unary_expression.operand.clone());
        match unary_expression.operator.kind {
            TokenKind::Minus => match operand {
                AnyValueEnum::IntValue(int_value) => {
                    AnyValueEnum::IntValue(self.builder.build_int_neg(int_value, "prefix_iminus").unwrap())
                }
                AnyValueEnum::FloatValue(float_value) => {
                    AnyValueEnum::FloatValue(self.builder.build_float_neg(float_value, "prefix_fminus").unwrap())
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
                AnyValueEnum::IntValue(int_value) => {
                    let zero = int_value.get_type().const_int(0, false);
                    let one = int_value.get_type().const_int(1, false);

                    let is_zero = self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, int_value, zero, "prefix_ineg")
                        .unwrap();

                    if is_zero.get_zero_extended_constant().unwrap() == 0 {
                        AnyValueEnum::IntValue(zero)
                    } else {
                        AnyValueEnum::IntValue(one)
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

    pub(crate) fn build_infix_expr(&self, scope: ScopeRef, binary_expression: BinaryExpression) -> AnyValueEnum {
        let left = self.build_expr(Rc::clone(&scope), *binary_expression.left);
        let right = self.build_expr(Rc::clone(&scope), *binary_expression.right);

        match binary_expression.operator.kind {
            TokenKind::Plus => {
                if let AnyValueEnum::IntValue(left) = left {
                    if let AnyValueEnum::IntValue(right) = right {
                        AnyValueEnum::IntValue(self.builder.build_int_add(left, right, "infix_iadd").unwrap())
                    } else if let AnyValueEnum::FloatValue(right) = right {
                        let left_float = {
                            self.builder
                                .build_signed_int_to_float(left, right.get_type(), "cast")
                                .unwrap()
                        };
                        AnyValueEnum::FloatValue(self.builder.build_float_add(left_float, right, "infix_fadd").unwrap())
                    } else {
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
                    }
                } else if let AnyValueEnum::FloatValue(left) = left {
                    if let AnyValueEnum::FloatValue(right) = right {
                        AnyValueEnum::FloatValue(self.builder.build_float_add(left, right, "infix_fadd").unwrap())
                    } else if let AnyValueEnum::IntValue(right) = right {
                        let right_float = {
                            self.builder
                                .build_signed_int_to_float(right, left.get_type(), "cast")
                                .unwrap()
                        };
                        AnyValueEnum::FloatValue(self.builder.build_float_add(left, right_float, "infix_fadd").unwrap())
                    } else {
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
                    }
                } else {
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
                }
            }
            TokenKind::Minus => {
                if let AnyValueEnum::IntValue(left) = left {
                    if let AnyValueEnum::IntValue(right) = right {
                        AnyValueEnum::IntValue(self.builder.build_int_sub(left, right, "infix_iadd").unwrap())
                    } else if let AnyValueEnum::FloatValue(right) = right {
                        let left_float = {
                            self.builder
                                .build_signed_int_to_float(left, right.get_type(), "cast")
                                .unwrap()
                        };
                        AnyValueEnum::FloatValue(self.builder.build_float_sub(left_float, right, "infix_fadd").unwrap())
                    } else {
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
                    }
                } else if let AnyValueEnum::FloatValue(left) = left {
                    if let AnyValueEnum::FloatValue(right) = right {
                        AnyValueEnum::FloatValue(self.builder.build_float_sub(left, right, "infix_fadd").unwrap())
                    } else if let AnyValueEnum::IntValue(right) = right {
                        let right_float = {
                            self.builder
                                .build_signed_int_to_float(right, left.get_type(), "cast")
                                .unwrap()
                        };
                        AnyValueEnum::FloatValue(self.builder.build_float_sub(left, right_float, "infix_fadd").unwrap())
                    } else {
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
                    }
                } else {
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
                }
            }
            TokenKind::Asterisk => {
                if let AnyValueEnum::IntValue(left) = left {
                    if let AnyValueEnum::IntValue(right) = right {
                        AnyValueEnum::IntValue(self.builder.build_int_mul(left, right, "infix_iadd").unwrap())
                    } else if let AnyValueEnum::FloatValue(right) = right {
                        let left_float = {
                            self.builder
                                .build_signed_int_to_float(left, right.get_type(), "cast")
                                .unwrap()
                        };
                        AnyValueEnum::FloatValue(self.builder.build_float_mul(left_float, right, "infix_fadd").unwrap())
                    } else {
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
                    }
                } else if let AnyValueEnum::FloatValue(left) = left {
                    if let AnyValueEnum::FloatValue(right) = right {
                        AnyValueEnum::FloatValue(self.builder.build_float_mul(left, right, "infix_fadd").unwrap())
                    } else if let AnyValueEnum::IntValue(right) = right {
                        let right_float = {
                            self.builder
                                .build_signed_int_to_float(right, left.get_type(), "cast")
                                .unwrap()
                        };
                        AnyValueEnum::FloatValue(self.builder.build_float_mul(left, right_float, "infix_fadd").unwrap())
                    } else {
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
                    }
                } else {
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
                }
            }
            TokenKind::Slash => {
                if let AnyValueEnum::IntValue(left) = left {
                    if let AnyValueEnum::IntValue(right) = right {
                        AnyValueEnum::IntValue(self.builder.build_int_signed_div(left, right, "infix_iadd").unwrap())
                    } else if let AnyValueEnum::FloatValue(right) = right {
                        let left_float = {
                            self.builder
                                .build_signed_int_to_float(left, right.get_type(), "cast")
                                .unwrap()
                        };
                        AnyValueEnum::FloatValue(self.builder.build_float_div(left_float, right, "infix_fadd").unwrap())
                    } else {
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
                    }
                } else if let AnyValueEnum::FloatValue(left) = left {
                    if let AnyValueEnum::FloatValue(right) = right {
                        AnyValueEnum::FloatValue(self.builder.build_float_div(left, right, "infix_fadd").unwrap())
                    } else if let AnyValueEnum::IntValue(right) = right {
                        let right_float = {
                            self.builder
                                .build_signed_int_to_float(right, left.get_type(), "cast")
                                .unwrap()
                        };
                        AnyValueEnum::FloatValue(self.builder.build_float_div(left, right_float, "infix_fadd").unwrap())
                    } else {
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
                    }
                } else {
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
                }
            }
            TokenKind::Percent => {
                if let AnyValueEnum::IntValue(left) = left {
                    if let AnyValueEnum::IntValue(right) = right {
                        AnyValueEnum::IntValue(self.builder.build_int_signed_rem(left, right, "infix_iadd").unwrap())
                    } else if let AnyValueEnum::FloatValue(right) = right {
                        let left_float = {
                            self.builder
                                .build_signed_int_to_float(left, right.get_type(), "cast")
                                .unwrap()
                        };
                        AnyValueEnum::FloatValue(self.builder.build_float_rem(left_float, right, "infix_fadd").unwrap())
                    } else {
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
                    }
                } else if let AnyValueEnum::FloatValue(left) = left {
                    if let AnyValueEnum::FloatValue(right) = right {
                        AnyValueEnum::FloatValue(self.builder.build_float_rem(left, right, "infix_fadd").unwrap())
                    } else if let AnyValueEnum::IntValue(right) = right {
                        let right_float = {
                            self.builder
                                .build_signed_int_to_float(right, left.get_type(), "cast")
                                .unwrap()
                        };
                        AnyValueEnum::FloatValue(self.builder.build_float_rem(left, right_float, "infix_fadd").unwrap())
                    } else {
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
                    }
                } else {
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
                }
            }
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
        }
    }
}
