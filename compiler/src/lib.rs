use gccjit_sys::*;
use utils::compiler_error;
use ast::{ast::*, token::TokenKind};
use builtins::macros::retrieve_builtin_func;
use std::{cell::RefCell, collections::HashMap, ffi::CString, ptr::null_mut};

mod builtins;
mod output;
mod types;

type BlockFuncRef = (Option<*mut gcc_jit_block>, Option<*mut gcc_jit_function>);

struct FuncParamRecord {
    param_index: i32,
    param_name: String,
    param_type: *mut gcc_jit_type,
}

type FuncParamsRecords = Vec<FuncParamRecord>;

pub struct Compiler {
    program: Program,
    context: *mut gcc_jit_context,
    func_table: RefCell<HashMap<String, *mut gcc_jit_function>>,
    var_table: RefCell<HashMap<String, *mut gcc_jit_lvalue>>,
    param_table: RefCell<HashMap<*mut gcc_jit_function, FuncParamsRecords>>,
}

impl Compiler {
    pub fn new(program: Program) -> Self {
        let context = unsafe { gcc_jit_context_acquire() };
        Self {
            program,
            context,
            func_table: RefCell::new(HashMap::new()),
            var_table: RefCell::new(HashMap::new()),
            param_table: RefCell::new(HashMap::new()),
        }
    }

    pub fn compile(&mut self) {
        for stmt in self.program.body.clone() {
            self.compile_statement(stmt.clone(), (None, None));
        }
    }

    pub fn compile_statement(&mut self, stmt: Statement, block_func_ref: BlockFuncRef) {
        match stmt {
            Statement::Variable(variable) => self.compile_variable(block_func_ref, variable.clone()),
            Statement::Expression(expr) => {
                self.compile_expression(block_func_ref, expr.clone());
            }
            Statement::FuncDef(function) => self.compile_func_def(function.clone()),
            Statement::If(if_stmt) => todo!(),
            Statement::For(_) => todo!(),
            Statement::Match(_) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Package(package) => todo!(),
            Statement::Import(import) => todo!(),
            Statement::Return(ret_stmt) => self.compile_return(block_func_ref, ret_stmt),
        }
    }

    fn compile_return(&mut self, block_func_ref: BlockFuncRef, ret_stmt: Return) {
        if let Some(block) = block_func_ref.0 {
            let ret_value = self.compile_expression(block_func_ref, ret_stmt.argument);

            unsafe { gcc_jit_block_end_with_return(block, std::ptr::null_mut(), ret_value) };
        } else {
            compiler_error!("Incorrect usage of the return statement. It must be used inside a function declaration.");
        }
    }

    fn compile_variable(&mut self, block_func_ref: BlockFuncRef, variable: Variable) {
        if let Some(func) = block_func_ref.1 {
            if let Some(block) = block_func_ref.0 {
                let rvalue = self.compile_expression(block_func_ref, variable.expr);

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

                self.var_table.borrow_mut().insert(variable.name, lvalue);
            } else {
                compiler_error!("gcc_jit_block required to make a variable declaration but it's null.");
            }
        } else {
            compiler_error!("Local variable declarations must be performed inside a function.");
        }
    }

    fn compile_func_def(&mut self, func_def: FuncDef) {
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
                param_type: ty,
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

        for item in func_def.body.body {
            if let Statement::Return(_) = item.clone() {
                return_compiled = true;
            }

            self.compile_statement(item, (Some(block), Some(func)));
        }

        if !return_compiled {
            compiler_error!(format!(
                "Explicit return statement required for the function '{}'.",
                func_def.name
            ));
        }

        self.func_table.borrow_mut().insert(func_def.name, func);
    }

    pub fn compile_identifier(&mut self, block_func_ref: BlockFuncRef, identifier: Identifier) -> *mut gcc_jit_rvalue {
        match self.var_table.borrow_mut().get(&identifier.name) {
            Some(lvalue) => {
                let rvalue = unsafe { gcc_jit_lvalue_as_rvalue(lvalue.clone()) };
                return rvalue;
            }
            None => {
                if let Some(func) = block_func_ref.1 {
                    if let Some(func_param_records) = self.param_table.borrow_mut().get(&func) {
                        for param in func_param_records {
                            if param.param_name == identifier.name {
                                let param = unsafe { gcc_jit_function_get_param(func, param.param_index) };
                                let rvalue = unsafe { gcc_jit_param_as_rvalue(param) };
                                return rvalue;
                            }
                        }
                    }
                }

                compiler_error!(format!("'{}' is not defined in this scope.", identifier.name))
            }
        }
    }

    fn compile_expression(&mut self, block_func_ref: BlockFuncRef, expr: Expression) -> *mut gcc_jit_rvalue {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifier(identifier) => self.compile_identifier(block_func_ref, identifier),
            Expression::Prefix(unary_expression) => self.compile_prefix_expression(block_func_ref, unary_expression),
            Expression::Infix(binary_expression) => self.compile_infix_expression(block_func_ref, binary_expression),
            Expression::FunctionCall(func_call) => self.compile_func_call(block_func_ref, func_call),
            Expression::UnaryOperator(unary_operator) => self.compile_unary_operator(unary_operator),
            Expression::Array(array) => todo!(),
            Expression::ArrayIndex(array_index) => todo!(),
        }
    }

    pub fn compile_func_call(&mut self, block_func_ref: BlockFuncRef, func_call: FunctionCall) -> *mut gcc_jit_rvalue {
        if let Some(block) = block_func_ref.0 {
            let mut args: Vec<*mut gcc_jit_rvalue> = Vec::new();

            for arg_expr in func_call.arguments {
                let arg_rvalue = self.compile_expression(block_func_ref, arg_expr);
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
                    Some(func_def) => {
                        func_def(self.context, block, &mut args)
                    }
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

    pub fn compile_unary_operator(&mut self, unary_operator: UnaryOperator) -> *mut gcc_jit_rvalue {
        todo!()
    }

    fn compile_prefix_expression(
        &mut self,
        block_func_ref: BlockFuncRef,
        unary_expression: UnaryExpression,
    ) -> *mut gcc_jit_rvalue {
        let op = match unary_expression.operator.kind {
            TokenKind::Minus => gcc_jit_unary_op::GCC_JIT_UNARY_OP_MINUS,
            TokenKind::Bang => gcc_jit_unary_op::GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
            _ => compiler_error!("Invalid operator given for the prefix expression."),
        };

        let expr = self.compile_expression(block_func_ref, *unary_expression.operand);
        let ty = unsafe { gcc_jit_rvalue_get_type(expr) };

        unsafe { gcc_jit_context_new_unary_op(self.context, null_mut(), op, ty, expr) }
    }

    fn compile_infix_expression(
        &mut self,
        block_func_ref: BlockFuncRef,
        binary_expression: BinaryExpression,
    ) -> *mut gcc_jit_rvalue {
        let op = match binary_expression.operator.kind {
            TokenKind::Plus => gcc_jit_binary_op::GCC_JIT_BINARY_OP_PLUS,
            TokenKind::Minus => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MINUS,
            TokenKind::Slash => gcc_jit_binary_op::GCC_JIT_BINARY_OP_DIVIDE,
            TokenKind::Asterisk => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MULT,
            TokenKind::Percent => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MODULO,
            _ => compiler_error!("Invalid operator given for the infix expression."),
        };

        let left = self.compile_expression(block_func_ref, *binary_expression.left);
        let right = self.compile_expression(block_func_ref, *binary_expression.right);
        let left_type = unsafe { gcc_jit_rvalue_get_type(left) };
        let right_type = unsafe { gcc_jit_rvalue_get_type(right) };

        let widest_data_type = self.widest_data_type(left_type, right_type);

        let casted_left = unsafe { gcc_jit_context_new_cast(self.context, null_mut(), left, widest_data_type) };
        let casted_right = unsafe { gcc_jit_context_new_cast(self.context, null_mut(), right, widest_data_type) };

        unsafe {
            gcc_jit_context_new_binary_op(
                self.context,
                null_mut(),
                op,
                widest_data_type,
                casted_left,
                casted_right,
            )
        }
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
                dbg!(bool_literal.raw.clone());

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

    fn compile_if_stmt(&mut self, if_stmt: If) {}
}

impl Drop for Compiler {
    fn drop(&mut self) {
        unsafe { gcc_jit_context_release(self.context) };
    }
}
