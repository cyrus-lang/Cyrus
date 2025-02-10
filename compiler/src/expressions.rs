use std::ffi::CString;
use std::ptr::null_mut;
use std::rc::Rc;

use ast::ast::*;
use ast::token::*;
use gccjit_sys::*;
use utils::compiler_error;

use crate::scope::ScopeRef;
use crate::Compiler;

impl Compiler {
    fn access_identifier_values(
        &mut self,
        scope: ScopeRef,
        identifier: Identifier,
    ) -> (*mut gcc_jit_lvalue, *mut gcc_jit_rvalue) {
        if let Some(metadata) = scope.borrow_mut().get(identifier.name.clone()) {
            let lvalue = metadata.borrow_mut().lvalue;
            let rvalue = unsafe { gcc_jit_lvalue_as_rvalue(lvalue) };

            return (lvalue, rvalue);
        }

        if let Some(values) = self.access_current_func_param(identifier.clone()) {
            return values.clone();
        }

        compiler_error!(format!("'{}' is not defined in this scope.", identifier.name))
    }

    fn compile_identifier(&mut self, scope: ScopeRef, identifier: Identifier) -> *mut gcc_jit_rvalue {
        self.access_identifier_values(scope, identifier).1
    }

    pub(crate) fn compile_expression(&mut self, scope: ScopeRef, expr: Expression) -> *mut gcc_jit_rvalue {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifier(identifier) => self.compile_identifier(scope, identifier),
            Expression::Prefix(unary_expression) => self.compile_prefix_expression(scope, unary_expression),
            Expression::Infix(binary_expression) => self.compile_infix_expression(scope, binary_expression),
            Expression::FuncCall(func_call) => self.compile_func_call(scope, func_call),
            Expression::UnaryOperator(unary_operator) => self.compile_unary_operator(scope, unary_operator),
            Expression::Array(array) => self.compile_array(Rc::clone(&scope), array, null_mut()),
            Expression::ArrayIndex(array_index) => self.compile_array_index(Rc::clone(&scope), array_index),
            Expression::Assignment(assignment) => self.compile_assignment(scope, *assignment),
            Expression::ArrayIndexAssign(array_index_assign) => {
                self.compile_array_index_assign(Rc::clone(&scope), *array_index_assign)
            }
            Expression::AddressOf(expression) => self.compile_address_of(Rc::clone(&scope), expression),
            Expression::Dereference(expression) => self.compile_dereference(Rc::clone(&scope), expression),
            Expression::StructInit(struct_init) => self.compile_struct_init(scope, struct_init),
            Expression::StructFieldAccess(struct_field_access) => {
                self.compile_struct_field_access(scope, *struct_field_access)
            }
            Expression::CastAs(cast_as) => self.compile_cast_as(Rc::clone(&scope), cast_as),
        }
    }

    fn compile_cast_as(&mut self, scope: ScopeRef, cast_as: CastAs) -> *mut gcc_jit_rvalue {
        let rvalue = self.compile_expression(scope, *cast_as.expr);
        let target_type = self.token_as_data_type(self.context, cast_as.cast_as);

        unsafe { gcc_jit_context_new_cast(self.context, self.gccjit_location(cast_as.loc), rvalue, target_type) }
    }

    fn compile_dereference(&mut self, scope: ScopeRef, expression: Box<Expression>) -> *mut gcc_jit_rvalue {
        let rvalue = self.compile_expression(scope, *expression.clone());

        unsafe { gcc_jit_lvalue_as_rvalue(gcc_jit_rvalue_dereference(rvalue, null_mut())) }
    }

    fn compile_address_of(&mut self, scope: ScopeRef, expression: Box<Expression>) -> *mut gcc_jit_rvalue {
        match *expression {
            Expression::Identifier(identifier) => {
                let lvalue = self.access_identifier_values(Rc::clone(&scope), identifier).0;
                unsafe { gcc_jit_lvalue_get_address(lvalue, null_mut()) }
            }
            _ => self.compile_expression(scope, *expression),
        }
    }

    fn compile_array_index_assign(
        &mut self,
        scope: ScopeRef,
        array_index_assign: ArrayIndexAssign,
    ) -> *mut gcc_jit_rvalue {
        match scope.borrow_mut().get(array_index_assign.identifier.name.clone()) {
            Some(variable) => {
                let lvalue = self.array_dimension_as_lvalue(
                    Rc::clone(&scope),
                    variable.borrow_mut().lvalue,
                    array_index_assign.dimensions,
                );

                let block_func = self.block_func_ref.lock().unwrap();
                if let Some(block) = block_func.block {
                    drop(block_func);

                    let loc = self.gccjit_location(array_index_assign.loc.clone());
                    match array_index_assign.expr.clone() {
                        Expression::Array(Array { elements, .. }) => {
                            for (idx, item) in elements.iter().enumerate() {
                                let expr = self.compile_expression(Rc::clone(&scope), item.clone());
                                let array_item_ptr = unsafe {
                                    gcc_jit_context_new_array_access(
                                        self.context,
                                        loc.clone(),
                                        gcc_jit_lvalue_as_rvalue(lvalue),
                                        gcc_jit_context_new_rvalue_from_int(
                                            self.context,
                                            Compiler::i32_type(self.context),
                                            idx.try_into().unwrap(),
                                        ),
                                    )
                                };

                                unsafe { gcc_jit_block_add_assignment(block, loc, array_item_ptr, expr) };
                            }

                            // TODO 
                            // Make a new construction to return the assigned array
                            return null_mut(); 
                        }
                        _ => {
                            let rvalue = self.compile_expression(Rc::clone(&scope), array_index_assign.expr);

                            let casted_rvalue = unsafe {
                                gcc_jit_context_new_cast(
                                    self.context,
                                    self.gccjit_location(array_index_assign.loc.clone()),
                                    rvalue,
                                    gcc_jit_rvalue_get_type(gcc_jit_lvalue_as_rvalue(lvalue)),
                                )
                            };

                            unsafe {
                                gcc_jit_block_add_assignment(
                                    block,
                                    self.gccjit_location(array_index_assign.loc.clone()),
                                    lvalue,
                                    casted_rvalue,
                                )
                            }
                            return rvalue;
                        }
                    };
                } else {
                    compiler_error!("Array index assignment in invalid block.");
                }
            }
            None => compiler_error!(format!(
                "'{}' is not defined in this scope.",
                array_index_assign.identifier.name
            )),
        }
    }

    fn array_dimension_as_lvalue(
        &mut self,
        scope: ScopeRef,
        variable: *mut gcc_jit_lvalue,
        dimensions: Vec<Expression>,
    ) -> *mut gcc_jit_lvalue {
        let mut result: *mut gcc_jit_lvalue = variable;

        for dim in dimensions {
            if let Expression::Array(index_expr) = dim {
                if let Expression::Array(value) = index_expr.elements[0].clone() {
                    let idx = self.compile_expression(Rc::clone(&scope), value.elements[0].clone());

                    let lvalue = unsafe {
                        gcc_jit_context_new_array_access(
                            self.context,
                            null_mut(),
                            gcc_jit_lvalue_as_rvalue(result),
                            idx,
                        )
                    };

                    result = lvalue;
                }
            }
        }

        result
    }

    fn compile_array_index(&mut self, scope: ScopeRef, array_index: ArrayIndex) -> *mut gcc_jit_rvalue {
        match scope.borrow_mut().get(array_index.identifier.name.clone()) {
            Some(variable) => {
                let lvalue = self.array_dimension_as_lvalue(
                    Rc::clone(&scope),
                    variable.borrow_mut().lvalue,
                    array_index.dimensions,
                );

                unsafe { gcc_jit_lvalue_as_rvalue(lvalue) }
            }
            None => compiler_error!(format!(
                "'{}' is not defined in this scope.",
                array_index.identifier.name
            )),
        }
    }

    pub(crate) fn compile_array(
        &mut self,
        scope: ScopeRef,
        array: Array,
        mut array_type: *mut gcc_jit_type,
    ) -> *mut gcc_jit_rvalue {
        let mut array_elements: Vec<*mut gcc_jit_rvalue> = Vec::new();

        if array.elements.len() == 0 {
            return unsafe {
                gcc_jit_context_new_array_constructor(
                    self.context,
                    self.gccjit_location(array.loc),
                    array_type,
                    array_elements.len() as i32,
                    [].as_mut_ptr(),
                )
            };
        }

        for expr in array.elements {
            array_elements.push(self.compile_expression(Rc::clone(&scope), expr));
        }

        let element_type = unsafe { gcc_jit_rvalue_get_type(array_elements[0]) };

        if array_type.is_null() {
            array_type = unsafe {
                gcc_jit_context_new_array_type(
                    self.context,
                    self.gccjit_location(array.loc.clone()),
                    element_type,
                    array_elements.len().try_into().unwrap(),
                )
            }
        }

        unsafe {
            gcc_jit_context_new_array_constructor(
                self.context,
                self.gccjit_location(array.loc),
                array_type,
                array_elements.len() as i32,
                array_elements.as_mut_ptr(),
            )
        }
    }

    fn compile_assignment(&mut self, scope: ScopeRef, assignment: Assignment) -> *mut gcc_jit_rvalue {
        let (lvalue, rvalue) = self.access_identifier_values(Rc::clone(&scope), assignment.identifier);

        let block_func = self.block_func_ref.lock().unwrap();
        if let Some(block) = block_func.block {
            drop(block_func);

            let target_type = unsafe { gcc_jit_rvalue_get_type(rvalue) };
            let new_rvalue = self.compile_expression(scope, assignment.expr);
            let casted_rvalue = unsafe {
                gcc_jit_context_new_cast(
                    self.context,
                    self.gccjit_location(assignment.loc.clone()),
                    new_rvalue,
                    target_type,
                )
            };

            unsafe {
                gcc_jit_block_add_assignment(block, self.gccjit_location(assignment.loc.clone()), lvalue, casted_rvalue);
            };

            return rvalue;
        } else {
            compiler_error!("Incorrect usage of the assignment. Assignments must be performed inside a valid block.");
        }
    }

    fn compile_unary_operator(&mut self, scope: ScopeRef, unary_operator: UnaryOperator) -> *mut gcc_jit_rvalue {
        let loc = self.gccjit_location(unary_operator.loc.clone());

        match scope.borrow_mut().get(unary_operator.identifier.name.clone()) {
            Some(lvalue) => {
                let rvalue = unsafe { gcc_jit_lvalue_as_rvalue(lvalue.borrow_mut().lvalue) };
                let rvalue_type = unsafe { gcc_jit_rvalue_get_type(rvalue) };

                if !self.is_int_data_type(rvalue_type) {
                    compiler_error!("Unary operations are only valid for integer types.");
                }

                let fixed_number = unsafe { gcc_jit_context_new_rvalue_from_int(self.context, rvalue_type, 1) };

                let bin_op = match unary_operator.ty.clone() {
                    UnaryOperatorType::PostIncrement | UnaryOperatorType::PreIncrement => {
                        gcc_jit_binary_op::GCC_JIT_BINARY_OP_PLUS
                    }
                    UnaryOperatorType::PostDecrement | UnaryOperatorType::PreDecrement => {
                        gcc_jit_binary_op::GCC_JIT_BINARY_OP_MINUS
                    }
                };

                let guard = self.block_func_ref.lock().unwrap();

                let tmp_local: *mut gcc_jit_lvalue;
                if let (Some(block), Some(func)) = (guard.block, guard.func) {
                    let tmp_local_name = CString::new("temp").unwrap();

                    tmp_local = unsafe { gcc_jit_function_new_local(func, loc, rvalue_type, tmp_local_name.as_ptr()) };

                    if !self.block_is_terminated(block) {
                        unsafe { gcc_jit_block_add_assignment(block, loc, tmp_local, rvalue) };
                    }
                } else {
                    compiler_error!("Unary operators (++, --, etc.) are only allowed inside functions.");
                }

                let tmp_rvalue = unsafe { gcc_jit_lvalue_as_rvalue(tmp_local) };

                // Assign incremented/decremented value in the variable
                if let Some(block) = guard.block {
                    if !self.block_is_terminated(block) {
                        unsafe {
                            gcc_jit_block_add_assignment_op(
                                block,
                                loc,
                                lvalue.borrow_mut().lvalue,
                                bin_op,
                                gcc_jit_context_new_cast(self.context, loc, fixed_number, rvalue_type),
                            )
                        };
                    }
                }

                let result = rvalue.clone();

                let result = match unary_operator.ty {
                    UnaryOperatorType::PreIncrement => result,
                    UnaryOperatorType::PostIncrement => tmp_rvalue,
                    UnaryOperatorType::PreDecrement => result,
                    UnaryOperatorType::PostDecrement => tmp_rvalue,
                };

                result
            }
            None => {
                compiler_error!(format!(
                    "'{}' is not defined in this scope.",
                    unary_operator.identifier.name
                ))
            }
        }
    }

    fn compile_prefix_expression(&mut self, scope: ScopeRef, unary_expression: UnaryExpression) -> *mut gcc_jit_rvalue {
        let op = match unary_expression.operator.kind {
            TokenKind::Minus => gcc_jit_unary_op::GCC_JIT_UNARY_OP_MINUS,
            TokenKind::Bang => gcc_jit_unary_op::GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
            _ => compiler_error!("Invalid operator given for the prefix expression."),
        };

        let expr = self.compile_expression(scope, *unary_expression.operand);
        let ty = unsafe { gcc_jit_rvalue_get_type(expr) };

        unsafe { gcc_jit_context_new_unary_op(self.context, self.gccjit_location(unary_expression.loc), op, ty, expr) }
    }

    fn compile_infix_expression(
        &mut self,
        scope: ScopeRef,
        binary_expression: BinaryExpression,
    ) -> *mut gcc_jit_rvalue {
        let left = self.compile_expression(Rc::clone(&scope), *binary_expression.left);
        let right = self.compile_expression(Rc::clone(&scope), *binary_expression.right);
        let left_type = unsafe { gcc_jit_rvalue_get_type(left) };
        let right_type = unsafe { gcc_jit_rvalue_get_type(right) };

        let widest_data_type = self.widest_data_type(left_type, right_type);

        let casted_left = unsafe {
            gcc_jit_context_new_cast(
                self.context,
                self.gccjit_location(binary_expression.loc.clone()),
                left,
                widest_data_type,
            )
        };
        let casted_right = unsafe {
            gcc_jit_context_new_cast(
                self.context,
                self.gccjit_location(binary_expression.loc.clone()),
                right,
                widest_data_type,
            )
        };

        match binary_expression.operator.kind {
            bin_op @ TokenKind::Plus
            | bin_op @ TokenKind::Minus
            | bin_op @ TokenKind::Slash
            | bin_op @ TokenKind::Asterisk
            | bin_op @ TokenKind::Percent => self.compile_binary_operation(
                bin_op,
                widest_data_type,
                casted_left,
                casted_right,
                binary_expression.loc,
            ),
            bin_op @ TokenKind::LessThan
            | bin_op @ TokenKind::LessEqual
            | bin_op @ TokenKind::GreaterThan
            | bin_op @ TokenKind::GreaterEqual
            | bin_op @ TokenKind::Equal
            | bin_op @ TokenKind::NotEqual => {
                self.compile_comparison_operation(bin_op, casted_left, casted_right, binary_expression.loc)
            }
            _ => compiler_error!("Invalid operator given for the infix expression."),
        }
    }

    fn compile_comparison_operation(
        &mut self,
        bin_op: TokenKind,
        left: *mut gcc_jit_rvalue,
        right: *mut gcc_jit_rvalue,
        loc: Location,
    ) -> *mut gcc_jit_rvalue {
        let op = match bin_op {
            TokenKind::LessThan => gcc_jit_comparison::GCC_JIT_COMPARISON_LT,
            TokenKind::LessEqual => gcc_jit_comparison::GCC_JIT_COMPARISON_LE,
            TokenKind::GreaterThan => gcc_jit_comparison::GCC_JIT_COMPARISON_GT,
            TokenKind::GreaterEqual => gcc_jit_comparison::GCC_JIT_COMPARISON_GE,
            TokenKind::Equal => gcc_jit_comparison::GCC_JIT_COMPARISON_EQ,
            TokenKind::NotEqual => gcc_jit_comparison::GCC_JIT_COMPARISON_NE,
            _ => panic!(),
        };

        unsafe { gcc_jit_context_new_comparison(self.context, self.gccjit_location(loc), op, left, right) }
    }

    fn compile_binary_operation(
        &mut self,
        bin_op: TokenKind,
        data_type: *mut gcc_jit_type,
        left: *mut gcc_jit_rvalue,
        right: *mut gcc_jit_rvalue,
        loc: Location,
    ) -> *mut gcc_jit_rvalue {
        let op = match bin_op {
            TokenKind::Plus => gcc_jit_binary_op::GCC_JIT_BINARY_OP_PLUS,
            TokenKind::Minus => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MINUS,
            TokenKind::Slash => gcc_jit_binary_op::GCC_JIT_BINARY_OP_DIVIDE,
            TokenKind::Asterisk => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MULT,
            TokenKind::Percent => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MODULO,
            _ => panic!(),
        };

        unsafe { gcc_jit_context_new_binary_op(self.context, self.gccjit_location(loc), op, data_type, left, right) }
    }

    fn compile_literal(&mut self, literal: Literal) -> *mut gcc_jit_rvalue {
        match literal {
            Literal::Integer(integer_literal) => match integer_literal {
                IntegerLiteral::I8(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i8_type(self.context), value as i32)
                },
                IntegerLiteral::I16(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i16_type(self.context), value as i32)
                },
                IntegerLiteral::I32(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i32_type(self.context), value as i32)
                },
                IntegerLiteral::I64(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i64_type(self.context), value as i32)
                },
                IntegerLiteral::I128(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i128_type(self.context), value as i32)
                },
                IntegerLiteral::U8(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::u8_type(self.context), value as i32)
                },
                IntegerLiteral::U16(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::u16_type(self.context), value as i32)
                },
                IntegerLiteral::U32(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::u32_type(self.context), value as i32)
                },
                IntegerLiteral::U64(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::u64_type(self.context), value as i32)
                },
                IntegerLiteral::U128(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::u128_type(self.context), value as i32)
                },
                IntegerLiteral::SizeT(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::size_t_type(self.context), value as i32)
                },
            },
            Literal::Float(float_literal) => match float_literal {
                FloatLiteral::Float(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_double(self.context, Compiler::f32_type(self.context), value as f64)
                },
                FloatLiteral::Double(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_double(self.context, Compiler::f64_type(self.context), value as f64)
                },
            },
            Literal::Bool(bool_literal) => {
                let value = if bool_literal.raw { 1 } else { 0 };
                unsafe { gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i8_type(self.context), value) }
            }
            Literal::String(string_literal) => unsafe {
                let value = CString::new(self.purify_string(string_literal.raw)).unwrap();
                gcc_jit_context_new_string_literal(self.context, value.as_ptr())
            },
            Literal::Char(char_literal) => unsafe {
                gcc_jit_context_new_rvalue_from_int(
                    self.context,
                    Compiler::char_type(self.context),
                    char_literal.raw as i32,
                )
            },
            Literal::Null => unsafe { gcc_jit_context_null(self.context, Compiler::void_ptr_type(self.context)) },
        }
    }
}
