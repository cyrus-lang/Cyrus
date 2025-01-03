use ast::{ast::*, token::TokenKind};
use builtin_builder::retrieve_builtin_func;
use gccjit_sys::*;
use std::{cell::RefCell, collections::HashMap, ffi::CString, ptr::null_mut, slice::from_raw_parts};
use utils::compiler_error;

mod builtin_builder;
mod builtin_funcs;
mod output;
mod types;

pub struct Compiler {
    program: Program,
    context: *mut gcc_jit_context,
    func_table: RefCell<HashMap<String, *mut gcc_jit_function>>,
}

impl Compiler {
    pub fn new(program: Program) -> Self {
        let context = unsafe { gcc_jit_context_acquire() };
        Self {
            program,
            context,
            func_table: RefCell::new(HashMap::new()),
        }
    }

    pub fn compile(&mut self) {
        for stmt in self.program.body.clone() {
            self.compile_statement(stmt.clone(), None, None);
        }
    }

    pub fn compile_statement(
        &mut self,
        stmt: Statement,
        block: Option<*mut gcc_jit_block>,
        func: Option<*mut gcc_jit_function>,
    ) {
        match stmt {
            Statement::Variable(variable) => self.compile_variable(variable.clone(), block, func),
            Statement::Expression(expr) => {
                self.compile_expression(block, expr.clone());
            }
            Statement::FuncDef(function) => self.compile_func_def(function.clone()),
            Statement::If(if_stmt) => todo!(),
            Statement::For(_) => todo!(),
            Statement::Match(_) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Package(package) => todo!(),
            Statement::Import(import) => todo!(),
            Statement::Return(ret_stmt) => self.compile_return(ret_stmt, block),
        }
    }

    fn compile_return(&mut self, ret_stmt: Return, block: Option<*mut gcc_jit_block>) {
        if let Some(block) = block {
            let ret_value = self.compile_expression(Some(block), ret_stmt.argument);

            unsafe { gcc_jit_block_end_with_return(block, std::ptr::null_mut(), ret_value) };
        } else {
            compiler_error!("Incorrect usage of the return statement. It must be used inside a function declaration.");
        }
    }

    fn compile_variable(
        &mut self,
        variable: Variable,
        block: Option<*mut gcc_jit_block>,
        func: Option<*mut gcc_jit_function>,
    ) {
        if let Some(func) = func {
            if let Some(block) = block {
                let var_ty: *mut gcc_jit_type;

                if let Some(token) = variable.ty {
                    var_ty = self.as_type(token);
                } else {
                    var_ty = self.void_type();
                }

                let name = CString::new(variable.name).unwrap();
                let lvalue = unsafe { gcc_jit_function_new_local(func, null_mut(), var_ty, name.as_ptr()) };
                let rvalue = self.compile_expression(Some(block), variable.expr);

                unsafe { gcc_jit_block_add_assignment(block, null_mut(), lvalue, rvalue) };
            } else {
                compiler_error!("gcc_jit_block required to make a variable declaration but it's null.");
            }
        } else {
            compiler_error!("Local variable declarations must be performed inside a function.");
        }
    }

    fn compile_func_def(&mut self, function: FuncDef) {
        let func_type = match function.vis_type {
            FuncVisType::Extern => gcc_jit_function_kind::GCC_JIT_FUNCTION_EXPORTED,
            FuncVisType::Pub => gcc_jit_function_kind::GCC_JIT_FUNCTION_EXPORTED,
            FuncVisType::Internal => gcc_jit_function_kind::GCC_JIT_FUNCTION_INTERNAL,
            FuncVisType::Inline => gcc_jit_function_kind::GCC_JIT_FUNCTION_ALWAYS_INLINE,
        };

        let mut ret_type: *mut gcc_jit_type = self.void_type();

        if let Some(token) = function.return_type {
            ret_type = self.as_type(token.kind);
        }

        // TODO - Eval params
        let params: *mut *mut gcc_jit_param = null_mut();

        let func_name = CString::new(function.name.clone()).unwrap();
        let func = unsafe {
            gcc_jit_context_new_function(
                self.context,
                null_mut(),
                func_type,
                ret_type,
                func_name.as_ptr(),
                0,
                params,
                0,
            )
        };

        // Build func block
        let name = CString::new("def").unwrap();
        let block = unsafe { gcc_jit_function_new_block(func, name.as_ptr()) };
        let mut return_compiled = false;

        for item in function.body.body {
            if let Statement::Return(_) = item.clone() {
                return_compiled = true;
            }

            self.compile_statement(item, Some(block), Some(func));
        }

        if !return_compiled {
            compiler_error!(format!(
                "Explicit return statement required for the function '{}'.",
                function.name
            ));
        }

        self.func_table.borrow_mut().insert(function.name, func);
    }

    fn compile_expression(&mut self, block: Option<*mut gcc_jit_block>, expr: Expression) -> *mut gcc_jit_rvalue {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifier(identifier) => todo!(),
            Expression::Prefix(unary_expression) => self.compile_prefix_expression(block, unary_expression),
            Expression::Infix(binary_expression) => self.compile_infix_expression(block, binary_expression),
            Expression::FunctionCall(func_call) => self.compile_function_call(block, func_call),
            Expression::UnaryOperator(unary_operator) => self.compile_unary_operator(unary_operator),
            Expression::Array(array) => todo!(),
            Expression::ArrayIndex(array_index) => todo!(),
        }
    }

    pub fn compile_function_call(
        &mut self,
        block: Option<*mut gcc_jit_block>,
        func_call: FunctionCall,
    ) -> *mut gcc_jit_rvalue {
        if let Some(cur_block) = block {
            let mut args: Vec<*mut gcc_jit_rvalue> = Vec::new();

            for expr in func_call.arguments {
                args.push(self.compile_expression(block, expr));
            }

            match self.func_table.borrow_mut().get(&func_call.function_name.name) {
                Some(func) => unsafe {
                    gcc_jit_context_new_call(self.context, null_mut(), *func, 0, args.as_mut_ptr())
                },
                None => match retrieve_builtin_func(func_call.function_name.name.clone()) {
                    Some(func_def) => func_def(self.context, cur_block, args),
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
        block: Option<*mut gcc_jit_block>,
        unary_expression: UnaryExpression,
    ) -> *mut gcc_jit_rvalue {
        let op = match unary_expression.operator.kind {
            TokenKind::Minus => gcc_jit_unary_op::GCC_JIT_UNARY_OP_MINUS,
            TokenKind::Bang => gcc_jit_unary_op::GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
            _ => compiler_error!("Invalid operator given for the prefix expression."),
        };

        let expr = self.compile_expression(block, *unary_expression.operand);
        let ty = unsafe { gcc_jit_rvalue_get_type(expr) };

        unsafe { gcc_jit_context_new_unary_op(self.context, null_mut(), op, ty, expr) }
    }

    fn compile_infix_expression(
        &mut self,
        block: Option<*mut gcc_jit_block>,
        binary_expression: BinaryExpression,
    ) -> *mut gcc_jit_rvalue {
        let op = match binary_expression.operator.kind {
            TokenKind::Plus => gcc_jit_binary_op::GCC_JIT_BINARY_OP_PLUS,
            TokenKind::Minus => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MINUS,
            TokenKind::Slash => gcc_jit_binary_op::GCC_JIT_BINARY_OP_DIVIDE,
            TokenKind::Asterisk => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MULT,
            TokenKind::Percent => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MULT,
            _ => compiler_error!("Invalid operator given for the infix expression."),
        };
        let mut is_float = false;

        let left = self.compile_expression(block, *binary_expression.left);
        let right = self.compile_expression(block, *binary_expression.right);

        unsafe {
            if gcc_jit_rvalue_get_type(left) == self.f32_type() || gcc_jit_rvalue_get_type(right) == self.f32_type() {
                is_float = true
            }
        }

        let op_type = if is_float { self.f32_type() } else { self.i32_type() };

        unsafe { gcc_jit_context_new_binary_op(self.context, null_mut(), op, op_type, left, right) }
    }

    fn compile_literal(&mut self, literal: Literal) -> *mut gcc_jit_rvalue {
        match literal {
            Literal::Integer(integer_literal) => match integer_literal {
                // IntegerLiteral::I32(value) => self.context.new_rvalue_from_int(self.i32_type(), value as i32),
                IntegerLiteral::I32(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, self.i32_type(), value as i32)
                },
                IntegerLiteral::I64(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, self.i64_type(), value as i32)
                },
                IntegerLiteral::U32(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, self.u32_type(), value as i32)
                },
                IntegerLiteral::U64(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, self.u64_type(), value as i32)
                },
            },
            Literal::Float(float_literal) => match float_literal {
                FloatLiteral::F32(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_double(self.context, self.f32_type(), value as f64)
                },
                FloatLiteral::F64(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_double(self.context, self.f64_type(), value as f64)
                },
            },
            Literal::Boolean(boolean_literal) => {
                let value = if boolean_literal.raw { 1 } else { 0 };
                unsafe { gcc_jit_context_new_rvalue_from_int(self.context, self.i8_type(), value) }
            }
            Literal::String(string_literal) => unsafe {
                let value = CString::new(string_literal.raw).unwrap();

                gcc_jit_context_new_string_literal(self.context, value.as_ptr())
            },
        }
    }

    fn compile_if_stmt(&mut self, if_stmt: If) {}
}

impl Drop for Compiler {
    fn drop(&mut self) {
        unsafe { gcc_jit_context_release(self.context) };
    }
}
