use std::rc::Rc;

use ast::{ast::*, token::TokenKind};
use gccjit::*;
use utils::compiler_error;

mod output;
mod types;

pub struct Compiler<'a> {
    program: Program,
    context: Rc<Context<'a>>,
}

impl<'a> Compiler<'a> {
    pub fn new(program: Program) -> Self {
        let context = Rc::new(Context::default());
        Self { program, context }
    }

    pub fn compile(&self) {
        for stmt in self.program.body.clone() {
            self.compile_statement(stmt.clone());
        }
    }

    pub fn compile_statement(&self, stmt: Statement) {
        match stmt {
            Statement::Variable(variable) => self.compile_variable(variable.clone()),
            Statement::Expression(expr) => {
                self.compile_expression(expr.clone());
            }
            Statement::FuncDef(function) => self.compile_func_def(function.clone()),
            Statement::If(if_stmt) => todo!(),
            Statement::For(_) => todo!(),
            Statement::Match(_) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Package(package) => todo!(),
            Statement::Import(import) => todo!(),
            Statement::Return(_) => compiler_error!("Incorrect usage of the return statement. It must be used inside a functino declaration."),
        }
    }

    fn compile_func_def(&self, function: FuncDef) {
        // TODO - Add func type keywords
        let func_type = gccjit::FunctionType::Exported;
        let func_return_type: Type;
        let mut is_return_type_void = false;

        if let Some(token) = function.return_type {
            func_return_type = self.token_to_type(token);
        } else {
            func_return_type = self.void_type();
            is_return_type_void = true;
        }

        // TODO - Eval params
        let params: &[Parameter<'a>] = &[];

        let func = self
            .context
            .new_function(None, func_type, func_return_type, params, function.name.clone(), false);

        // Build func block
        let mut block = func.new_block("def");
        let mut return_compiled = false;

        for item in function.body.body {
            match item {
                Statement::Return(ret_stmt) => {
                    let ret_value = self.compile_expression(ret_stmt.argument);
                    block.end_with_return(None, ret_value);
                    return_compiled = true;
                }
                _ => {
                    self.compile_statement(item);
                }
            }
        }

        if !return_compiled && !is_return_type_void {
            compiler_error!(format!("Explicit return statement required for the function '{}'.", function.name));
        } else if is_return_type_void && !return_compiled {
            block.end_with_void_return(None);
        }
    }

    fn compile_expression(&self, expr: Expression) -> RValue {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifier(identifier) => todo!(),
            Expression::Prefix(unary_expression) => self.compile_prefix_expression(unary_expression),
            Expression::Infix(binary_expression) => self.compile_infix_expression(binary_expression),
            Expression::FunctionCall(function_call) => todo!(),
            Expression::UnaryOperator(unary_operator) => self.compile_unary_operator(unary_operator),
        }
    }

    pub fn compile_unary_operator(&self, unary_operator: UnaryOperator) -> RValue {
        todo!()
    }

    fn compile_prefix_expression(&self, unary_expression: UnaryExpression) -> RValue {
        let op = match unary_expression.operator.kind {
            TokenKind::Minus => UnaryOp::Minus,
            TokenKind::Bang => UnaryOp::LogicalNegate,
            _ => compiler_error!("Invalid operator given for the prefix expression."),
        };

        let expr = self.compile_expression(*unary_expression.operand);

        self.context.new_unary_op(None, op, expr.get_type(), expr)
    }

    fn compile_infix_expression(&self, binary_expression: BinaryExpression) -> RValue {
        let op = match binary_expression.operator.kind {
            TokenKind::Plus => BinaryOp::Plus,
            TokenKind::Minus => BinaryOp::Minus,
            TokenKind::Slash => BinaryOp::Divide,
            TokenKind::Asterisk => BinaryOp::Mult,
            TokenKind::Percent => BinaryOp::Modulo,
            _ => compiler_error!("Invalid operator given for the infix expression."),
        };
        let mut is_float = false;

        let left = self.compile_expression(*binary_expression.left);
        let right = self.compile_expression(*binary_expression.right);

        if left.get_type() == self.f32_type() || right.get_type() == self.f32_type() {
            is_float = true
        }

        let op_type = if is_float { self.f32_type() } else { self.i32_type() };

        let result = self.context.new_binary_op(None, op, op_type, left, right);
        return result;
    }

    fn compile_literal(&self, literal: Literal) -> RValue {
        match literal {
            Literal::Integer(integer_literal) => match integer_literal {
                IntegerLiteral::I32(value) => self.context.new_rvalue_from_int(self.i32_type(), value as i32),
                IntegerLiteral::I64(value) => self.context.new_rvalue_from_int(self.i64_type(), value as i32),
                IntegerLiteral::U32(value) => self.context.new_rvalue_from_int(self.u32_type(), value as i32),
                IntegerLiteral::U64(value) => self.context.new_rvalue_from_int(self.u64_type(), value as i32),
            },
            Literal::Float(float_literal) => match float_literal {
                FloatLiteral::F32(value) => self.context.new_rvalue_from_double(self.f32_type(), value as f64),
                FloatLiteral::F64(value) => self.context.new_rvalue_from_double(self.f64_type(), value as f64),
            },
            Literal::Boolean(boolean_literal) => {
                if boolean_literal.raw {
                    self.context.new_rvalue_one(self.context.new_int_type(1, false))
                } else {
                    self.context.new_rvalue_zero(self.context.new_int_type(1, false))
                }
            }
            Literal::String(string_literal) => self.context.new_string_literal(string_literal.raw),
        }
    }

    fn compile_variable(&self, variable: Variable) {}
    fn compile_if_stmt(&self, if_stmt: If) {}
}
