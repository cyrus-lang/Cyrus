use std::ffi::CString;
use std::ptr::null_mut;
use std::rc::Rc;

use ast::ast::*;
use ast::token::*;
use gccjit_sys::*;
use utils::compiler_error;
use utils::compile_time_errors::errors::*;
use utils::purify_string::purify_string;

use crate::scope::ScopeRef;
use crate::Compiler;

impl Compiler {
    pub(crate) fn access_identifier_values(
        &mut self,
        scope: ScopeRef,
        from_package: FromPackage,
    ) -> (*mut gcc_jit_lvalue, *mut gcc_jit_rvalue) {
        if from_package.sub_packages.len() == 0 {
            // local defined identifier
            if let Some(metadata) = scope.borrow_mut().get(from_package.identifier.name.clone()) {
                let lvalue = metadata.borrow_mut().lvalue;
                let rvalue = unsafe { gcc_jit_lvalue_as_rvalue(lvalue) };

                return (lvalue, rvalue);
            }

            if let Some(values) = self.access_current_func_param(from_package.identifier.clone()) {
                return values.clone();
            }
        }

        compiler_error!(format!("'{}' is not defined in this scope.", from_package.to_string()), self.file_path.clone())
    }

    fn compile_identifier(&mut self, scope: ScopeRef, identifier: Identifier) -> *mut gcc_jit_rvalue {
        self.access_identifier_values(
            scope,
            FromPackage {
                sub_packages: vec![],
                identifier: identifier.clone(),
                span: identifier.span.clone(),
                loc: identifier.loc,
            },
        )
        .1
    }

    fn compile_from_package(&mut self, scope: ScopeRef, from_package: FromPackage) -> *mut gcc_jit_rvalue {
        self.access_identifier_values(Rc::clone(&scope), from_package).1
    }

    pub(crate) fn compile_expression(&mut self, scope: ScopeRef, expr: Expression) -> *mut gcc_jit_rvalue {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifier(identifier) => self.compile_identifier(scope, identifier),
            Expression::Prefix(unary_expression) => self.compile_prefix_expression(scope, unary_expression),
            Expression::Infix(binary_expression) => self.compile_infix_expression(scope, binary_expression),
            Expression::FieldAccessOrMethodCall(mut chains) => {
                let chains_copy = chains.clone();
                let rvalue = self.eval_first_item_of_chains(Rc::clone(&scope), chains.clone());
                chains.remove(0);
                let result = self.field_access_or_method_call(Rc::clone(&scope), rvalue, chains.clone());

                let last_chain = chains_copy.last().unwrap();
                if let Some(method_call) = last_chain.method_call.clone() {
                    self.eval_func_call(result, method_call.loc);
                    null_mut()
                } else {
                    result
                }
            }
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
                let item = struct_field_access.chains[struct_field_access.chains.len() - 1].clone();
                if let Some(method_call) = item.method_call {
                    let rvalue = self.compile_struct_field_access(scope, *struct_field_access.clone());
                    self.eval_func_call(rvalue, method_call.loc.clone());
                }
                null_mut()
            }
            Expression::CastAs(cast_as) => self.compile_cast_as(Rc::clone(&scope), cast_as),
            Expression::FromPackage(from_package) => self.compile_from_package(Rc::clone(&scope), from_package),
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
            Expression::FromPackage(from_package) => {
                let lvalue = self.access_identifier_values(Rc::clone(&scope), from_package).0;
                unsafe { gcc_jit_lvalue_get_address(lvalue, null_mut()) }
            }
            Expression::Identifier(identifier) => {
                let lvalue = self
                    .access_identifier_values(
                        Rc::clone(&scope),
                        FromPackage {
                            sub_packages: vec![],
                            identifier: identifier.clone(),
                            span: identifier.span.clone(),
                            loc: identifier.loc,
                        },
                    )
                    .0;
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
        let (lvalue, _) = self.access_identifier_values(Rc::clone(&scope), array_index_assign.from_package);
        let array_index_lvalue =
            self.array_dimension_as_lvalue(Rc::clone(&scope), lvalue, array_index_assign.dimensions);

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
                                gcc_jit_lvalue_as_rvalue(array_index_lvalue),
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
                    let rvalue_type = unsafe { gcc_jit_rvalue_get_type(gcc_jit_lvalue_as_rvalue(array_index_lvalue)) };
                    let expr = array_index_assign.expr.clone();
                    return self.safe_assign_lvalue(
                        Rc::clone(&scope),
                        array_index_lvalue,
                        rvalue_type,
                        expr,
                        array_index_assign.loc,
                    );
                }
            };
        } else {
            compiler_error!("Array index assignment in invalid block.", self.file_path.clone());
        }
    }

    fn array_dimension_as_lvalue(
        &mut self,
        scope: ScopeRef,
        variable: *mut gcc_jit_lvalue,
        dimensions: Vec<Expression>,
    ) -> *mut gcc_jit_lvalue {
        if dimensions.len() == 0 {
            compiler_error!("You are trying to access an array item lvalue with empty dimension.", self.file_path.clone())
        }

        let mut result: *mut gcc_jit_lvalue = variable;

        for dim in dimensions {
            if let Expression::Array(index_expr) = dim {
                if let Expression::Literal(Literal::Integer(integer_literal)) = index_expr.elements[0].clone() {
                    let idx = self.compile_expression(
                        Rc::clone(&scope),
                        Expression::Literal(Literal::Integer(integer_literal)),
                    );

                    let lvalue = unsafe {
                        gcc_jit_context_new_array_access(
                            self.context,
                            null_mut(),
                            gcc_jit_lvalue_as_rvalue(result),
                            idx,
                        )
                    };

                    result = lvalue;
                } else {
                    compiler_error!("Unable to access array lvalue through a non-integer literal.", self.file_path.clone())
                }
            }
        }

        if result == variable {
            compiler_error!("Unexpected behavior when trying to compile array_dimension_as_lvalue.", self.file_path.clone())
        }

        result
    }

    fn compile_array_index(&mut self, scope: ScopeRef, array_index: ArrayIndex) -> *mut gcc_jit_rvalue {
        let (lvalue, _) = self.access_identifier_values(Rc::clone(&scope), array_index.from_package);
        let array_index_lvalue = self.array_dimension_as_lvalue(Rc::clone(&scope), lvalue, array_index.dimensions);
        unsafe { gcc_jit_lvalue_as_rvalue(array_index_lvalue) }
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

    fn safe_assign_lvalue(
        &mut self,
        scope: ScopeRef,
        lvalue: *mut gcc_jit_lvalue,
        rvalue_type: *mut gcc_jit_type,
        expr: Expression,
        loc: Location,
    ) -> *mut gcc_jit_rvalue {
        let block_func = self.block_func_ref.lock().unwrap();
        if let Some(block) = block_func.block {
            drop(block_func);

            let new_rvalue = match expr.clone() {
                Expression::FieldAccessOrMethodCall(mut chains) => {
                    let rvalue = self.eval_first_item_of_chains(Rc::clone(&scope), chains.clone());
                    chains.remove(0);
                    return self.field_access_or_method_call(Rc::clone(&scope), rvalue, chains);
                }
                Expression::StructFieldAccess(struct_field_access) => {
                    self.compile_struct_field_access(Rc::clone(&scope), *struct_field_access.clone())
                }
                _ => self.compile_expression(scope, expr),
            };
            let casted_rvalue = unsafe {
                gcc_jit_context_new_cast(self.context, self.gccjit_location(loc.clone()), new_rvalue, rvalue_type)
            };

            unsafe {
                gcc_jit_block_add_assignment(block, self.gccjit_location(loc.clone()), lvalue, casted_rvalue);
            };

            return casted_rvalue;
        } else {
            compiler_error!("Incorrect usage of the assignment. Assignments must be performed inside a valid block.", self.file_path.clone());
        }
    }

    fn compile_assignment(&mut self, scope: ScopeRef, assignment: Assignment) -> *mut gcc_jit_rvalue {
        let (lvalue, rvalue) = self.access_identifier_values(Rc::clone(&scope), assignment.identifier);
        let rvalue_type = unsafe { gcc_jit_rvalue_get_type(rvalue) };
        self.safe_assign_lvalue(Rc::clone(&scope), lvalue, rvalue_type, assignment.expr, assignment.loc)
    }

    fn compile_unary_operator(&mut self, scope: ScopeRef, unary_operator: UnaryOperator) -> *mut gcc_jit_rvalue {
        let loc = self.gccjit_location(unary_operator.loc.clone());

        let (lvalue, rvalue) = self.access_identifier_values(Rc::clone(&scope), unary_operator.identifier);

        let rvalue_type = unsafe { gcc_jit_rvalue_get_type(rvalue) };

        if !self.is_int_data_type(rvalue_type) {
            compiler_error!("Unary operations are only valid for integer types.", self.file_path.clone());
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
            compiler_error!("Unary operators (++, --, etc.) are only allowed inside functions.", self.file_path.clone());
        }

        let tmp_rvalue = unsafe { gcc_jit_lvalue_as_rvalue(tmp_local) };

        // Assign incremented/decremented value in the variable
        if let Some(block) = guard.block {
            if !self.block_is_terminated(block) {
                unsafe {
                    gcc_jit_block_add_assignment_op(
                        block,
                        loc,
                        lvalue,
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

    fn compile_prefix_expression(&mut self, scope: ScopeRef, unary_expression: UnaryExpression) -> *mut gcc_jit_rvalue {
        let op = match unary_expression.operator.kind {
            TokenKind::Minus => gcc_jit_unary_op::GCC_JIT_UNARY_OP_MINUS,
            TokenKind::Bang => gcc_jit_unary_op::GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
            _ => compiler_error!("Invalid operator given for the prefix expression.", self.file_path.clone()),
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
            _ => compiler_error!("Invalid operator given for the infix expression.", self.file_path.clone()),
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
                    gcc_jit_context_new_rvalue_from_double(
                        self.context,
                        Compiler::float_type(self.context),
                        value as f64,
                    )
                },
                FloatLiteral::Double(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_double(
                        self.context,
                        Compiler::double_type(self.context),
                        value as f64,
                    )
                },
            },
            Literal::Bool(bool_literal) => {
                let value = if bool_literal.raw { 1 } else { 0 };
                unsafe { gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i8_type(self.context), value) }
            }
            Literal::String(string_literal) => unsafe {
                let value = CString::new(purify_string(string_literal.raw)).unwrap();
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
