use crate::diagnostics::AnalyzerDiagKind;
use ast::LiteralKind;
use diagcentral::{Diag, DiagLevel, DiagLoc, reporter::DiagReporter};
use resolver::Resolver;
use typed_ast::{
    SymbolID, TypedBlockStatement, TypedExpression, TypedFuncDef, TypedProgramTree, TypedStatement, TypedVariable,
    format::format_concrete_type,
    types::{BasicConcreteType, ConcreteType, TypedArrayCapacity, TypedArrayType},
};

pub struct AnalysisContext<'a> {
    pub ast: &'a TypedProgramTree,
    pub resolver: &'a Resolver,
    pub reporter: DiagReporter<AnalyzerDiagKind>,
}

impl<'a> AnalysisContext<'a> {
    pub fn new(resolver: &'a Resolver, ast: &'a TypedProgramTree) -> Self {
        Self {
            ast,
            resolver,
            reporter: DiagReporter::new(),
        }
    }

    // Traverse TypedAST
    pub fn analyze(&mut self) {
        for typed_stmt in &self.ast.body {
            match typed_stmt {
                TypedStatement::FuncDef(typed_func_def) => self.analyze_block_statement(&typed_func_def.body),
                TypedStatement::Typedef(typed_typedef) => todo!(),
                TypedStatement::Import(typed_import) => todo!(),
                TypedStatement::GlobalVariable(typed_global_variable) => todo!(),
                TypedStatement::Variable(typed_variable) => todo!(),
                TypedStatement::FuncDecl(typed_func_decl) => todo!(),
                TypedStatement::BlockStatement(typed_block_statement) => todo!(),
                TypedStatement::If(typed_if) => todo!(),
                TypedStatement::Return(typed_return) => todo!(),
                TypedStatement::Break(typed_break) => todo!(),
                TypedStatement::Continue(typed_continue) => todo!(),
                TypedStatement::For(typed_for) => todo!(),
                TypedStatement::Foreach(typed_foreach) => todo!(),
                TypedStatement::Switch(typed_switch) => todo!(),
                TypedStatement::Struct(typed_struct) => todo!(),
                TypedStatement::Enum(typed_enum) => todo!(),
                TypedStatement::Interface(typed_interface) => todo!(),
                TypedStatement::Expression(typed_expr) => self.analyze_expr(typed_expr),
            }
        }
    }

    pub fn analyze_expr(&mut self, typed_expr: &TypedExpression) {
        todo!();
    }

    fn analyze_block_statement(&mut self, block_statement: &TypedBlockStatement) {
        for typed_stmt in &block_statement.exprs {
            match typed_stmt {
                TypedStatement::Variable(typed_variable) => self.analyze_variable(typed_variable),
                TypedStatement::Typedef(typed_typedef) => todo!(),
                TypedStatement::BlockStatement(typed_block_statement) => {
                    self.analyze_block_statement(typed_block_statement)
                }
                TypedStatement::If(typed_if) => todo!(),
                TypedStatement::Return(typed_return) => todo!(),
                TypedStatement::Break(typed_break) => todo!(),
                TypedStatement::Continue(typed_continue) => todo!(),
                TypedStatement::For(typed_for) => todo!(),
                TypedStatement::Foreach(typed_foreach) => todo!(),
                TypedStatement::Switch(typed_switch) => todo!(),
                TypedStatement::Struct(typed_struct) => todo!(),
                TypedStatement::Enum(typed_enum) => todo!(),
                TypedStatement::Interface(typed_interface) => todo!(),
                TypedStatement::Expression(typed_expression) => todo!(),
                // Invalid statements.
                TypedStatement::FuncDef(_) => unreachable!(),
                TypedStatement::FuncDecl(_) => unreachable!(),
                TypedStatement::Import(_) => unreachable!(),
                TypedStatement::GlobalVariable(_) => unreachable!(),
            }
        }
    }

    fn analyze_variable(&mut self, typed_variable: &TypedVariable) {
        if typed_variable.ty.is_none() || typed_variable.rhs.is_none() {
            return;
        }

        let target_type = typed_variable.ty.clone().unwrap();
        let value_type = self.get_typed_expr_type(&typed_variable.rhs.clone().unwrap());

        if !self.check_type_mismatch(value_type.clone(), target_type.clone()) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::AssignmentTypeMismatch {
                    lhs_type: format_concrete_type(target_type, self.get_symbol_formatter()),
                    rhs_type: format_concrete_type(value_type, self.get_symbol_formatter()),
                },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    typed_variable.loc.clone(),
                    0,
                )),
                hint: None,
            });
        }
    }

    fn check_type_mismatch(&self, value_type: ConcreteType, target_type: ConcreteType) -> bool {
        match (value_type, target_type) {
            (ConcreteType::BasicType(basic_concrete_type1), ConcreteType::BasicType(basic_concrete_type2)) => {
                self.check_basic_type_mismatch(basic_concrete_type1, basic_concrete_type2)
            }
            (ConcreteType::Const(inner_concrete_type1), ConcreteType::Const(inner_concrete_type2)) => {
                self.check_type_mismatch(*inner_concrete_type1, *inner_concrete_type2)
            }
            (ConcreteType::Const(inner_concrete_type1), concrete_type2) => {
                self.check_type_mismatch(*inner_concrete_type1, concrete_type2)
            }
            (concrete_type1, ConcreteType::Const(inner_concrete_type2)) => {
                self.check_type_mismatch(concrete_type1, *inner_concrete_type2)
            }
            _ => false,
        }
    }

    fn get_symbol_formatter(&self) -> Box<&dyn Fn(SymbolID) -> String> {
        Box::new(&move |symbol_id: SymbolID| -> String {
            todo!();
        })
    }

    fn get_typed_expr_type(&self, typed_expr: &TypedExpression) -> ConcreteType {
        match typed_expr {
            TypedExpression::Symbol(_) => todo!(),
            TypedExpression::Literal(literal) => match &literal.kind {
                LiteralKind::Integer(_) => ConcreteType::BasicType(BasicConcreteType::Int),
                LiteralKind::Float(_) => ConcreteType::BasicType(BasicConcreteType::Float32),
                LiteralKind::Bool(_) => ConcreteType::BasicType(BasicConcreteType::Bool),
                LiteralKind::String(string_value, string_prefix) => {
                    if let Some(string_prefix) = string_prefix {
                        match string_prefix {
                            ast::StringPrefix::B => {
                                let len = string_value.len() + 1;
                                return ConcreteType::Array(TypedArrayType {
                                    element_type: Box::new(ConcreteType::BasicType(BasicConcreteType::Char)),
                                    capacity: TypedArrayCapacity::Fixed(len.try_into().unwrap()),
                                    loc: literal.loc.clone(),
                                });
                            }
                            ast::StringPrefix::C => {}
                        }
                    }
                    ConcreteType::Pointer(Box::new(ConcreteType::BasicType(BasicConcreteType::Char)))
                }
                LiteralKind::Char(_) => ConcreteType::BasicType(BasicConcreteType::Char),
                LiteralKind::Null => ConcreteType::BasicType(BasicConcreteType::Null),
            },
            TypedExpression::Prefix(typed_prefix_expression) => todo!(),
            TypedExpression::Infix(typed_infix_expression) => todo!(),
            TypedExpression::Unary(typed_unary_expression) => todo!(),
            TypedExpression::Assignment(typed_assignment) => todo!(),
            TypedExpression::Cast(typed_cast) => todo!(),
            TypedExpression::Array(typed_array) => todo!(),
            TypedExpression::ArrayIndex(typed_array_index) => todo!(),
            TypedExpression::AddressOf(typed_address_of) => todo!(),
            TypedExpression::Dereference(typed_dereference) => todo!(),
            TypedExpression::StructInit(typed_struct_init) => todo!(),
            TypedExpression::FuncCall(typed_func_call) => todo!(),
            TypedExpression::FieldAccess(typed_field_access) => todo!(),
            TypedExpression::MethodCall(typed_method_call) => todo!(),
            TypedExpression::UnnamedStructValue(typed_unnamed_struct_value) => todo!(),
        }
    }
}
