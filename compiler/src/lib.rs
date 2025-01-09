use ast::{ast::*, token::TokenKind};
use builtins::macros::retrieve_builtin_func;
use gccjit_sys::*;
use scope::{Scope, ScopeRef};
use std::{
    cell::RefCell,
    collections::HashMap,
    ffi::CString,
    ops::Deref,
    ptr::null_mut,
    rc::Rc,
    sync::{Arc, Mutex},
};
use utils::compiler_error;

mod builtins;
mod output;
mod scope;
mod types;

// Tracks the current GCC JIT block and function being compiled.
struct BlockFuncPair {
    block: Option<*mut gcc_jit_block>,
    func: Option<*mut gcc_jit_function>,
}

struct FuncParamRecord {
    param_index: i32,
    param_name: String,
}

type FuncParamsRecords = Vec<FuncParamRecord>;

pub struct Compiler {
    program: Program,
    context: *mut gcc_jit_context,
    func_table: RefCell<HashMap<String, *mut gcc_jit_function>>,
    global_vars_table: RefCell<HashMap<String, *mut gcc_jit_lvalue>>,
    param_table: RefCell<HashMap<*mut gcc_jit_function, FuncParamsRecords>>,
    block_func_ref: Arc<Mutex<Box<BlockFuncPair>>>,
}

impl Compiler {
    pub fn new(program: Program) -> Self {
        let context = unsafe { gcc_jit_context_acquire() };
        Self {
            program,
            context,
            func_table: RefCell::new(HashMap::new()),
            global_vars_table: RefCell::new(HashMap::new()),
            param_table: RefCell::new(HashMap::new()),
            block_func_ref: Arc::new(Mutex::new(Box::new(BlockFuncPair {
                block: None,
                func: None,
            }))),
        }
    }

    pub fn compile(&mut self) {
        let scope = Rc::new(RefCell::new(Scope::new()));
        self.compile_statements(scope, self.program.body.clone());
    }

    pub fn compile_statements(&mut self, scope: ScopeRef, stmts: Vec<Statement>) {
        for stmt in stmts {
            self.compile_statement(Rc::clone(&scope), stmt.clone());
        }
    }

    pub fn compile_statement(&mut self, scope: ScopeRef, stmt: Statement) {
        match stmt {
            Statement::Variable(variable) => self.compile_variable(scope, variable.clone()),
            Statement::Expression(expr) => {
                self.compile_expression(scope, expr.clone());
            }
            Statement::FuncDef(function) => self.compile_func_def(scope, function.clone()),
            Statement::If(statement) => self.compile_if_statement(scope, statement),
            Statement::For(statement) => self.compile_for_statement(scope, statement),
            Statement::Match(_) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Package(statement) => todo!(),
            Statement::Import(statement) => todo!(),
            Statement::Return(statement) => self.compile_return(scope, statement),
            Statement::BlockStatement(statement) => self.compile_statements(
                Rc::new(RefCell::new(scope.borrow_mut().clone_immutable())),
                statement.body,
            ),
            _ => compiler_error!(format!("Invalid statement: {:?}", stmt)),
        }
    }

    // FIXME
    // Not works fine in chained situation
    // TODO
    // Impl break and continue after fixing chaining
    fn compile_for_statement(&mut self, scope: ScopeRef, statement: For) {
        let guard = self.block_func_ref.lock().unwrap();

        if let (Some(block), Some(func)) = (guard.block, guard.func) {
            drop(guard);

            if let Some(initializer) = statement.initializer {
                let init_value = self.compile_expression(Rc::clone(&scope), initializer.expr);
                let init_type = unsafe { gcc_jit_rvalue_get_type(init_value) };
                let init_name = CString::new(initializer.name.clone()).unwrap();
                let init = unsafe { gcc_jit_function_new_local(func, null_mut(), init_type, init_name.as_ptr()) };
                unsafe { gcc_jit_block_add_assignment(block, null_mut(), init, init_value) };

                Rc::clone(&scope).borrow_mut().insert(initializer.name, init);
            }

            let mut cond =
                unsafe { gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i32_type(self.context), 1) };
            if let Some(expr) = statement.condition.clone() {
                cond = self.compile_expression(Rc::clone(&scope), expr);
            }

            let body_block_name = CString::new("for_body").unwrap();
            let body_block = unsafe { gcc_jit_function_new_block(func, body_block_name.as_ptr()) };
            let mut guard = self.block_func_ref.lock().unwrap();
            guard.block = Some(body_block);
            drop(guard);

            let after_for_name = CString::new("after_for").unwrap();
            let after_for_block = unsafe { gcc_jit_function_new_block(func, after_for_name.as_ptr()) };

            let mut for_breaked = false;
            // Custom statement compilation because of existence of the continue and break keyword.
            for body_statement in statement.body.body {
                match body_statement {
                    Statement::Break => {
                        unsafe {
                            gcc_jit_block_end_with_jump(body_block, null_mut(), after_for_block);
                        }
                        for_breaked = true;
                        break;
                    }
                    Statement::Continue => todo!(),
                    _ => self.compile_statement(Rc::clone(&scope), body_statement),
                }
            }

            if let Some(increment) = statement.increment {
                self.compile_expression(Rc::clone(&scope), increment);
            }

            if let Some(_) = statement.condition {
                unsafe {
                    gcc_jit_block_end_with_conditional(body_block, null_mut(), cond, body_block, after_for_block)
                };
            } else {
                unsafe {
                    gcc_jit_block_end_with_jump(body_block, null_mut(), after_for_block);
                }
            }

            let mut guard = self.block_func_ref.lock().unwrap();
            guard.block = Some(after_for_block);
            drop(guard);

            unsafe { gcc_jit_block_end_with_conditional(block, null_mut(), cond, body_block, after_for_block) };
        }
    }

    fn compile_return(&mut self, scope: ScopeRef, ret_stmt: Return) {
        let guard = self.block_func_ref.lock().unwrap();

        if let Some(block) = guard.block {
            drop(guard);

            let ret_value = self.compile_expression(scope, ret_stmt.argument);

            unsafe { gcc_jit_block_end_with_return(block, std::ptr::null_mut(), ret_value) };
        } else {
            compiler_error!("Incorrect usage of the return statement. It must be used inside a function declaration.");
        }
    }

    fn compile_variable(&mut self, scope: ScopeRef, variable: Variable) {
        let guard = self.block_func_ref.lock().unwrap();

        if let (Some(block), Some(func)) = (guard.block, guard.func) {
            drop(guard);

            let rvalue = self.compile_expression(Rc::clone(&scope), variable.expr);

            let var_ty: *mut gcc_jit_type;

            if let Some(token) = variable.ty {
                var_ty = Compiler::token_as_data_type(self.context, token);
            } else {
                var_ty = unsafe { gcc_jit_rvalue_get_type(rvalue) };
            }

            let name = CString::new(variable.name.clone()).unwrap();
            let lvalue = unsafe { gcc_jit_function_new_local(func, null_mut(), var_ty, name.as_ptr()) };

            let auto_casted = unsafe { gcc_jit_context_new_cast(self.context, null_mut(), rvalue, var_ty) };

            unsafe { gcc_jit_block_add_assignment(block, null_mut(), lvalue, auto_casted) };

            scope.borrow_mut().insert(variable.name, lvalue);
        } else {
            compiler_error!("Invalid usage of local variable.");
        }
    }

    fn compile_func_def(&mut self, scope: ScopeRef, func_def: FuncDef) {
        let func_type = match func_def.vis_type {
            FuncVisType::Extern => gcc_jit_function_kind::GCC_JIT_FUNCTION_EXPORTED,
            FuncVisType::Pub => gcc_jit_function_kind::GCC_JIT_FUNCTION_EXPORTED,
            FuncVisType::Internal => gcc_jit_function_kind::GCC_JIT_FUNCTION_INTERNAL,
            FuncVisType::Inline => gcc_jit_function_kind::GCC_JIT_FUNCTION_ALWAYS_INLINE,
        };

        let mut ret_type: *mut gcc_jit_type = Compiler::void_type(self.context);

        if let Some(token) = func_def.return_type {
            ret_type = Compiler::token_as_data_type(self.context, token.kind);
        }

        let mut params: Vec<*mut gcc_jit_param> = Vec::new();
        let mut func_param_records = FuncParamsRecords::new();

        for (idx, func_def_param) in func_def.params.iter().enumerate() {
            let name = CString::new(func_def_param.identifier.name.clone()).unwrap();

            let ty_token = if let Some(user_def) = &func_def_param.ty {
                user_def
            } else {
                &TokenKind::Void
            };

            let ty = Compiler::token_as_data_type(self.context, ty_token.clone());

            let param = unsafe { gcc_jit_context_new_param(self.context, null_mut(), ty, name.as_ptr()) };

            params.push(param);

            func_param_records.push(FuncParamRecord {
                param_index: idx as i32,
                param_name: func_def_param.identifier.name.clone(),
            });
        }

        let func_name = CString::new(func_def.name.clone()).unwrap();
        let func = unsafe {
            gcc_jit_context_new_function(
                self.context,
                null_mut(),
                func_type,
                ret_type,
                func_name.as_ptr(),
                params.len().try_into().unwrap(),
                params.as_mut_ptr(),
                0,
            )
        };

        self.param_table.borrow_mut().insert(func, func_param_records);

        // Build func block
        let name = CString::new("def").unwrap();
        let block = unsafe { gcc_jit_function_new_block(func, name.as_ptr()) };
        let mut return_compiled = false;

        let mut guard = self.block_func_ref.lock().unwrap();
        guard.block = Some(block);
        guard.func = Some(func);
        drop(guard);

        for item in func_def.body.body {
            if let Statement::Return(_) = item.clone() {
                return_compiled = true;
            }

            self.compile_statement(Rc::clone(&scope), item);
        }

        if !return_compiled {
            compiler_error!(format!(
                "Explicit return statement required for the function '{}'.",
                func_def.name
            ));
        }

        self.func_table.borrow_mut().insert(func_def.name, func);
    }

    pub fn load_lvalue_rvalue(
        &mut self,
        scope: ScopeRef,
        identifier: Identifier,
    ) -> (*mut gcc_jit_lvalue, *mut gcc_jit_rvalue) {
        match scope.borrow_mut().get(identifier.name.clone()) {
            Some(lvalue) => {
                let rvalue = unsafe { gcc_jit_lvalue_as_rvalue(*lvalue.borrow_mut()) };
                return (*lvalue.borrow_mut(), rvalue);
            }
            None => {
                let guard = self.block_func_ref.lock().unwrap();

                if let Some(func) = guard.func {
                    if let Some(func_param_records) = self.param_table.borrow_mut().get(&func) {
                        for param in func_param_records {
                            if param.param_name == identifier.name {
                                let param = unsafe { gcc_jit_function_get_param(func, param.param_index) };
                                let rvalue = unsafe { gcc_jit_param_as_rvalue(param) };
                                let lvalue = unsafe { gcc_jit_param_as_lvalue(param) };
                                return (lvalue, rvalue);
                            }
                        }
                    }
                }

                compiler_error!(format!("'{}' is not defined in this scope.", identifier.name))
            }
        }
    }

    pub fn compile_identifier(&mut self, scope: ScopeRef, identifier: Identifier) -> *mut gcc_jit_rvalue {
        self.load_lvalue_rvalue(scope, identifier).1
    }

    fn compile_expression(&mut self, scope: ScopeRef, expr: Expression) -> *mut gcc_jit_rvalue {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifier(identifier) => self.compile_identifier(scope, identifier),
            Expression::Prefix(unary_expression) => self.compile_prefix_expression(scope, unary_expression),
            Expression::Infix(binary_expression) => self.compile_infix_expression(scope, binary_expression),
            Expression::FunctionCall(func_call) => self.compile_func_call(scope, func_call),
            Expression::UnaryOperator(unary_operator) => self.compile_unary_operator(scope, unary_operator),
            Expression::Array(array) => todo!(),
            Expression::ArrayIndex(array_index) => todo!(),
            Expression::Assignment(assignment) => self.compile_assignment(scope, *assignment),
        }
    }

    pub fn compile_assignment(&mut self, scope: ScopeRef, assignment: Assignment) -> *mut gcc_jit_rvalue {
        let (lvalue, _) = self.load_lvalue_rvalue(Rc::clone(&scope), assignment.identifier);

        let block_func = self.block_func_ref.lock().unwrap();
        if let Some(block) = block_func.block {
            drop(block_func);

            let new_rvalue = self.compile_expression(scope, assignment.expr);

            unsafe {
                gcc_jit_block_add_assignment(block, null_mut(), lvalue, new_rvalue);
            };

            return new_rvalue;
        } else {
            compiler_error!("Incorrect usage of the assignment. Assignments must be performed inside a valid block.");
        }
    }

    pub fn compile_func_call(&mut self, scope: ScopeRef, func_call: FunctionCall) -> *mut gcc_jit_rvalue {
        let guard = self.block_func_ref.lock().unwrap();

        if let Some(block) = guard.block {
            drop(guard);

            let mut args: Vec<*mut gcc_jit_rvalue> = Vec::new();

            for arg_expr in func_call.arguments {
                let arg_rvalue = self.compile_expression(Rc::clone(&scope), arg_expr);
                args.push(arg_rvalue);
            }

            match self.func_table.borrow_mut().get(&func_call.function_name.name) {
                Some(func) => unsafe {
                    let rvalue = gcc_jit_context_new_call(
                        self.context,
                        null_mut(),
                        *func,
                        args.len().try_into().unwrap(),
                        args.as_mut_ptr(),
                    );
                    gcc_jit_block_add_eval(block, std::ptr::null_mut(), rvalue);
                    rvalue
                },
                None => match retrieve_builtin_func(func_call.function_name.name.clone()) {
                    Some(func_def) => func_def(self.context, block, &mut args),
                    None => compiler_error!(format!(
                        "Function '{}' not defined at this module.",
                        func_call.function_name.name
                    )),
                },
            }
        } else {
            compiler_error!("Calling any function at top-level nodes isn't allowed.");
        }
    }

    pub fn compile_unary_operator(&mut self, scope: ScopeRef, unary_operator: UnaryOperator) -> *mut gcc_jit_rvalue {
        match scope.borrow_mut().get(unary_operator.identifer.name.clone()) {
            Some(lvalue) => {
                let rvalue = unsafe { gcc_jit_lvalue_as_rvalue(*lvalue.borrow_mut()) };
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
                    tmp_local =
                        unsafe { gcc_jit_function_new_local(func, null_mut(), rvalue_type, tmp_local_name.as_ptr()) };
                    unsafe { gcc_jit_block_add_assignment(block, null_mut(), tmp_local, rvalue) };
                } else {
                    compiler_error!("Unary operators (++, --, etc.) are only allowed inside functions.");
                }

                let tmp_rvalue = unsafe { gcc_jit_lvalue_as_rvalue(tmp_local) };

                // Assign incremented/decremented value in the variable
                if let Some(block) = guard.block {
                    unsafe {
                        gcc_jit_block_add_assignment_op(
                            block,
                            null_mut(),
                            *lvalue.borrow_mut(),
                            bin_op,
                            gcc_jit_context_new_cast(self.context, null_mut(), fixed_number, rvalue_type),
                        )
                    };
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
                    unary_operator.identifer.name
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

        unsafe { gcc_jit_context_new_unary_op(self.context, null_mut(), op, ty, expr) }
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

        let casted_left = unsafe { gcc_jit_context_new_cast(self.context, null_mut(), left, widest_data_type) };
        let casted_right = unsafe { gcc_jit_context_new_cast(self.context, null_mut(), right, widest_data_type) };

        match binary_expression.operator.kind {
            bin_op @ TokenKind::Plus
            | bin_op @ TokenKind::Minus
            | bin_op @ TokenKind::Slash
            | bin_op @ TokenKind::Asterisk
            | bin_op @ TokenKind::Percent => {
                self.compile_binary_operation(bin_op, widest_data_type, casted_left, casted_right)
            }
            bin_op @ TokenKind::LessThan
            | bin_op @ TokenKind::LessEqual
            | bin_op @ TokenKind::GreaterThan
            | bin_op @ TokenKind::GreaterEqual
            | bin_op @ TokenKind::Equal
            | bin_op @ TokenKind::NotEqual => self.compile_comparison_operation(bin_op, casted_left, casted_right),
            _ => compiler_error!("Invalid operator given for the infix expression."),
        }
    }

    fn compile_comparison_operation(
        &mut self,
        bin_op: TokenKind,
        left: *mut gcc_jit_rvalue,
        right: *mut gcc_jit_rvalue,
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

        unsafe { gcc_jit_context_new_comparison(self.context, null_mut(), op, left, right) }
    }

    fn compile_binary_operation(
        &mut self,
        bin_op: TokenKind,
        data_type: *mut gcc_jit_type,
        left: *mut gcc_jit_rvalue,
        right: *mut gcc_jit_rvalue,
    ) -> *mut gcc_jit_rvalue {
        let op = match bin_op {
            TokenKind::Plus => gcc_jit_binary_op::GCC_JIT_BINARY_OP_PLUS,
            TokenKind::Minus => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MINUS,
            TokenKind::Slash => gcc_jit_binary_op::GCC_JIT_BINARY_OP_DIVIDE,
            TokenKind::Asterisk => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MULT,
            TokenKind::Percent => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MODULO,
            _ => panic!(),
        };

        unsafe { gcc_jit_context_new_binary_op(self.context, null_mut(), op, data_type, left, right) }
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
            },
            Literal::Float(float_literal) => match float_literal {
                FloatLiteral::F32(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_double(self.context, Compiler::f32_type(self.context), value as f64)
                },
                FloatLiteral::F64(value) => unsafe {
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
            Literal::Char(char_literal) => todo!(),
        }
    }

    // FIXME
    // Not works fine in chained situation
    fn compile_if_statement(&mut self, scope: ScopeRef, statement: If) {
        let guard = self.block_func_ref.lock().unwrap();

        unsafe {
            if let (Some(current_block), Some(func)) = (guard.block, guard.func) {
                drop(guard);

                // Handle ending block
                let if_end_name = CString::new("if_end").unwrap();
                let end_block = gcc_jit_function_new_block(func, if_end_name.as_ptr());

                let else_block_name = CString::new("if_else").unwrap();
                let mut else_block = null_mut();
                let mut next_block;

                if let Some(_) = statement.alternate {
                    else_block = gcc_jit_function_new_block(func, else_block_name.as_ptr());
                    next_block = else_block.clone();
                } else {
                    next_block = end_block.clone();
                }

                // Handle the initial 'if'
                let initial_cond = self.compile_expression(Rc::clone(&scope), statement.condition);
                let initial_then_name = CString::new("if_then").unwrap();
                let initial_then_block = gcc_jit_function_new_block(func, initial_then_name.as_ptr());

                let mut guard = self.block_func_ref.lock().unwrap();
                guard.block = Some(initial_then_block);
                drop(guard);

                self.compile_statements(scope, statement.consequent.body);
                gcc_jit_block_end_with_jump(initial_then_block, std::ptr::null_mut(), end_block);

                gcc_jit_block_end_with_conditional(
                    current_block,
                    std::ptr::null_mut(),
                    initial_cond,
                    initial_then_block,
                    next_block,
                );

                // Handle the final 'else' statement
                if !else_block.is_null() {
                    if let Some(alternate) = statement.alternate {
                        let mut guard = self.block_func_ref.lock().unwrap();
                        guard.block = Some(else_block);
                        drop(guard);

                        // FIXME
                        // self.compile_statements(Rc::clone(scope.clone()), alternate.body);
                        gcc_jit_block_end_with_jump(else_block, std::ptr::null_mut(), end_block);
                    }
                }

                let mut guard = self.block_func_ref.lock().unwrap();
                guard.block = Some(end_block);
            }
        }
    }

    // for branch in statement.branches.iter() {
    //     let branch_cond = self.compile_expression(branch.condition.clone());
    //     let elseif_then_name = CString::new("branch_then").unwrap();
    //     let elseif_else_name = CString::new("branch_else").unwrap();
    //     let branch_then_block = gcc_jit_function_new_block(func, elseif_then_name.as_ptr());
    //     let branch_else_block = gcc_jit_function_new_block(func, elseif_else_name.as_ptr());

    //     let mut guard = self.block_func_ref.lock().unwrap();
    //     guard.block = Some(branch_then_block);
    //     drop(guard);

    //     self.compile_statements(branch.consequent.body.clone());
    //     gcc_jit_block_end_with_jump(branch_then_block, std::ptr::null_mut(), end_block);

    //     gcc_jit_block_end_with_conditional(
    //         branch_else_block,
    //         std::ptr::null_mut(),
    //         branch_cond,
    //         branch_then_block,
    //         next_block,
    //     );

    //     next_block = branch_else_block; // Chain the else blocks
    // }

    // FIXME
    // Not works fine in chained situation
    // fn compile_if_statement(&mut self, statement: If) {
    //     let guard = self.block_func_ref.lock().unwrap();
    //     unsafe {
    //         if let (Some(current_block), Some(func)) = (guard.block, guard.func) {
    //             drop(guard);

    //             // Handle ending block
    //             let if_end_name = CString::new(format!("if_end_{}", self.new_block_name())).unwrap();

    //             let end_block: *mut gcc_jit_block;
    //             if let Some(block) = final_block {
    //                 dbg!("first final_block set as end_block");
    //                 end_block = block;
    //             } else {
    //                 end_block = gcc_jit_function_new_block(func, if_end_name.as_ptr());
    //                 dbg!("new end block created");
    //             }

    //             let else_block_name = CString::new(format!("if_else_{}", self.new_block_name())).unwrap();
    //             let mut else_block = null_mut();
    //             let next_block;

    //             if let Some(_) = statement.alternate {
    //                 else_block = gcc_jit_function_new_block(func, else_block_name.as_ptr());
    //                 next_block = else_block.clone();
    //             } else {
    //                 next_block = end_block.clone();
    //             }

    //             // for branch in statement.branches.iter() {
    //             //     dbg!("branch compilation");

    //             //     let branch_cond = self.compile_expression(branch.condition.clone());
    //             //     let elseif_then_name = CString::new(format!("branch_then_{}", self.new_block_name())).unwrap();
    //             //     let elseif_else_name = CString::new(format!("branch_else_{}", self.new_block_name())).unwrap();
    //             //     let branch_then_block = gcc_jit_function_new_block(func, elseif_then_name.as_ptr());
    //             //     let branch_else_block = gcc_jit_function_new_block(func, elseif_else_name.as_ptr());

    //             //     let mut guard = self.block_func_ref.lock().unwrap();
    //             //     guard.block = Some(branch_then_block);
    //             //     drop(guard);

    //             //     self.compile_statements(branch.consequent.body.clone());
    //             //     gcc_jit_block_end_with_jump(branch_then_block, std::ptr::null_mut(), end_block);

    //             //     gcc_jit_block_end_with_conditional(
    //             //         branch_else_block,
    //             //         std::ptr::null_mut(),
    //             //         branch_cond,
    //             //         branch_then_block,
    //             //         next_block,
    //             //     );

    //             //     next_block = branch_else_block; // Chain the else blocks
    //             // }

    //             // Handle the initial 'if'
    //             let initial_cond = self.compile_expression(statement.condition);
    //             let initial_then_name = CString::new(format!("if_then_{}", self.new_block_name())).unwrap();
    //             let initial_then_block = gcc_jit_function_new_block(func, initial_then_name.as_ptr());

    //             let mut guard = self.block_func_ref.lock().unwrap();
    //             guard.block = Some(initial_then_block);
    //             drop(guard);

    //             for item in statement.consequent.body {
    //                 match item {
    //                     Statement::If(chain) => {
    //                         self.compile_if_statement(Some(end_block), chain);
    //                     }
    //                     _ => self.compile_statement(item),
    //                 }
    //             }
    //             gcc_jit_block_end_with_jump(initial_then_block, std::ptr::null_mut(), end_block);

    //             gcc_jit_block_end_with_conditional(
    //                 current_block,
    //                 std::ptr::null_mut(),
    //                 initial_cond,
    //                 initial_then_block,
    //                 next_block,
    //             );

    //             // Handle the final 'else' statement
    //             if !else_block.is_null() {
    //                 if let Some(alternate) = statement.alternate {
    //                     let mut guard = self.block_func_ref.lock().unwrap();
    //                     guard.block = Some(else_block);
    //                     drop(guard);
    //                     self.compile_statements(alternate.body);
    //                     gcc_jit_block_end_with_jump(else_block, std::ptr::null_mut(), end_block);
    //                 }
    //             }

    //             let mut guard = self.block_func_ref.lock().unwrap();
    //             guard.block = Some(end_block);
    //         }
    //     }
    // }
}

impl Drop for Compiler {
    fn drop(&mut self) {
        unsafe { gcc_jit_context_release(self.context) };
    }
}
