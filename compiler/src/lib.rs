use ast::ast::*;
use gccjit::{Context, RValue, Type};
use utils::compiler_error;

pub struct Compiler<'a> {
    program: Program,
    context: Context<'a>,
}

impl<'a> Compiler<'a> {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            context: Context::default(),
        }
    }

    pub fn compile(&mut self) {
        for statement in &self.program.body {
            match statement {
                Statement::Variable(variable) => self.compile_variable(variable),
                Statement::Expression(expression) => todo!(),
                Statement::If(if_stmt) => todo!(),
                Statement::Return(_) => todo!(),
                Statement::Function(function) => todo!(),
                Statement::For(_) => todo!(),
                Statement::Match(_) => todo!(),
                Statement::Struct(_) => todo!(),
                Statement::Package(package) => todo!(),
                Statement::Import(import) => todo!(),
            }
        }
    }

    fn compile_expression(&self, expr: Expression) -> RValue {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifier(identifier) => todo!(),
            Expression::Prefix(unary_expression) => todo!(),
            Expression::Infix(binary_expression) => todo!(),
            Expression::FunctionCall(function_call) => todo!(),
            Expression::UnaryOperator(unary_operator) => todo!(),
        }
    }

    fn compile_literal(&self, literal: Literal) -> RValue {
        match literal {
            Literal::Integer(integer_literal) => match integer_literal {
                IntegerLiteral::I32(value) => self.context.new_rvalue_from_int(self.context.new_int_type(32, true), value),
                IntegerLiteral::I64(value) => self.context.new_rvalue_from_int(self.context.new_int_type(64, true), value as i32),
                IntegerLiteral::U32(value) => self.context.new_rvalue_from_int(self.context.new_int_type(32, false), value as i32),
                IntegerLiteral::U64(value) => self.context.new_rvalue_from_int(self.context.new_int_type(64, false), value as i32),
            },
            Literal::Float(float_literal) => match float_literal {
                FloatLiteral::F32(value) => self.context.new_rvalue_from_double(self.context.new_int_type(32, true), value as f64),
                FloatLiteral::F64(value) => self.context.new_rvalue_from_double(self.context.new_int_type(64, true), value),
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

    fn compile_variable(&self, variable: &Variable) {}
    fn compile_if_stmt(&self, if_stmt: If) {}
    fn compile_return_stmt(&self, return_stmt: Return) {}

    // Built-in types
    fn void_type(&self) -> Type {
        self.context.new_type::<()>()
    }

    // Get output
    pub fn execute(&self) {
        let result = self.context.compile();
        let main = result.get_function("main");
        if main.is_null() {
            compiler_error!("Executable requires a 'main' function as the entry point.");
        }

        unsafe {
            let main_fn: extern "C" fn() = std::mem::transmute(main);
            main_fn();
        }
    }

    pub fn make_executable_file(&self, file_path: String) {
        self.context.compile_to_file(gccjit::OutputKind::Executable, file_path);
    }

    pub fn make_object_file(&self, file_path: String) {
        self.context.compile_to_file(gccjit::OutputKind::ObjectFile, file_path);
    }

    pub fn set_debug_info(&self, is_debug_mode: bool) {
        self.context.set_debug_info(is_debug_mode);
    }

    pub fn make_dump_file(&self, file_path: String) {
        self.context.dump_to_file(file_path, true);
    }
}
