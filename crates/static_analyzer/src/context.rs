use crate::diagnostics::AnalyzerDiagKind;
use ast::{LiteralKind, operators::PrefixOperator};
use diagcentral::{Diag, DiagLevel, DiagLoc, reporter::DiagReporter};
use resolver::Resolver;
use typed_ast::{
    format::format_concrete_type,
    types::{BasicConcreteType, ConcreteType, TypedArrayCapacity, TypedArrayType},
    *,
};

pub struct AnalysisContext<'a> {
    pub ast: &'a TypedProgramTree,
    pub resolver: &'a Resolver,
    pub reporter: DiagReporter<AnalyzerDiagKind>,
    pub module_id: ModuleID,
}

impl<'a> AnalysisContext<'a> {
    pub fn new(resolver: &'a Resolver, module_id: ModuleID, ast: &'a TypedProgramTree) -> Self {
        Self {
            ast,
            resolver,
            reporter: DiagReporter::new(),
            module_id,
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
                TypedStatement::Expression(typed_expression) => {
                    self.get_typed_expr_type(typed_expression);
                }
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
        let value_type = match self.get_typed_expr_type(&typed_variable.rhs.clone().unwrap()) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

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

    fn get_infix_expr_type(&mut self, infix_expr: &TypedInfixExpression) -> Option<ConcreteType> {
        let lhs_type = match self.get_typed_expr_type(&infix_expr.lhs) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let rhs_type = match self.get_typed_expr_type(&infix_expr.rhs) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let valid_concrete_type = match (lhs_type.clone(), rhs_type.clone()) {
            (ConcreteType::BasicType(basic_concrete_type1), ConcreteType::BasicType(basic_concrete_type2)) => {
                if (self.is_basic_concrete_type_integer(basic_concrete_type1.clone())
                    && self.is_basic_concrete_type_integer(basic_concrete_type2.clone()))
                    || (self.is_basic_concrete_type_float(basic_concrete_type1.clone())
                        && self.is_basic_concrete_type_integer(basic_concrete_type2.clone()))
                {
                    Some(BasicConcreteType::bigger_type(basic_concrete_type1, basic_concrete_type2).unwrap())
                } else {
                    None
                }
            }
            _ => None,
        };

        match valid_concrete_type {
            Some(concrete_type) => Some(ConcreteType::BasicType(concrete_type)),
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidInfix {
                        lhs_type: format_concrete_type(lhs_type, self.get_symbol_formatter()),
                        rhs_type: format_concrete_type(rhs_type, self.get_symbol_formatter()),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        infix_expr.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        }
    }

    fn get_prefix_expr_type(&mut self, prefix_expr: &TypedPrefixExpression) -> Option<ConcreteType> {
        let operand_type = match self.get_typed_expr_type(&prefix_expr.operand) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        match prefix_expr.op {
            PrefixOperator::SizeOf => Some(ConcreteType::BasicType(BasicConcreteType::SizeT)),
            PrefixOperator::Bang => {
                let valid_concrete_type = match &operand_type {
                    ConcreteType::BasicType(basic_concrete_type) => match basic_concrete_type {
                        BasicConcreteType::Bool => Some(ConcreteType::BasicType(basic_concrete_type.clone())),
                        _ => None,
                    },
                    _ => None,
                };

                match valid_concrete_type {
                    Some(concrete_type) => Some(concrete_type),
                    None => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::PrefixBangOnNonBool {
                                operand_type: format_concrete_type(operand_type, self.get_symbol_formatter()),
                            },
                            location: Some(DiagLoc::new(
                                self.resolver.get_current_module_file_path(),
                                prefix_expr.loc.clone(),
                                0,
                            )),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
            PrefixOperator::Minus => {
                let valid_concrete_type = match &operand_type {
                    ConcreteType::BasicType(basic_concrete_type) => match basic_concrete_type {
                        BasicConcreteType::UIntPtr
                        | BasicConcreteType::IntPtr
                        | BasicConcreteType::SizeT
                        | BasicConcreteType::Int
                        | BasicConcreteType::Int8
                        | BasicConcreteType::Int16
                        | BasicConcreteType::Int32
                        | BasicConcreteType::Int64
                        | BasicConcreteType::Int128
                        | BasicConcreteType::UInt
                        | BasicConcreteType::UInt8
                        | BasicConcreteType::UInt16
                        | BasicConcreteType::UInt32
                        | BasicConcreteType::UInt64
                        | BasicConcreteType::UInt128
                        | BasicConcreteType::Float16
                        | BasicConcreteType::Float32
                        | BasicConcreteType::Float64
                        | BasicConcreteType::Float128 => Some(ConcreteType::BasicType(basic_concrete_type.clone())),
                        _ => None,
                    },
                    _ => None,
                };

                match valid_concrete_type {
                    Some(concrete_type) => Some(concrete_type),
                    None => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::PrefixMinusOnNonInteger {
                                operand_type: format_concrete_type(operand_type, self.get_symbol_formatter()),
                            },
                            location: Some(DiagLoc::new(
                                self.resolver.get_current_module_file_path(),
                                prefix_expr.loc.clone(),
                                0,
                            )),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
        }
    }

    fn get_unary_expr_type(&mut self, unary_expr: &TypedUnaryExpression) -> Option<ConcreteType> {
        let operand_type = match self.get_typed_expr_type(&unary_expr.operand) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let valid_operand_type = match &operand_type {
            ConcreteType::BasicType(basic_concrete_type) => {
                if self.is_basic_concrete_type_integer(basic_concrete_type.clone()) {
                    Some(basic_concrete_type)
                } else {
                    None
                }
            }
            _ => None,
        };

        match valid_operand_type {
            Some(concrete_type) => Some(ConcreteType::BasicType(concrete_type.clone())),
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidUnary {
                        operand_type: format_concrete_type(operand_type, self.get_symbol_formatter()),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        unary_expr.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        }
    }

    fn get_typed_expr_type(&mut self, typed_expr: &TypedExpression) -> Option<ConcreteType> {
        match typed_expr {
            TypedExpression::Symbol(symbol_id) => {
                let symbol_entry = self
                    .resolver
                    .lookup_symbol_entry_with_id(self.module_id, *symbol_id)
                    .unwrap();

                dbg!(symbol_entry.clone());

                todo!();
            }
            TypedExpression::Literal(literal) => match &literal.kind {
                LiteralKind::Integer(_) => Some(ConcreteType::BasicType(BasicConcreteType::Int)),
                LiteralKind::Float(_) => Some(ConcreteType::BasicType(BasicConcreteType::Float32)),
                LiteralKind::Bool(_) => Some(ConcreteType::BasicType(BasicConcreteType::Bool)),
                LiteralKind::String(string_value, string_prefix) => {
                    if let Some(string_prefix) = string_prefix {
                        match string_prefix {
                            ast::StringPrefix::B => {
                                let len = string_value.len() + 1;
                                return Some(ConcreteType::Array(TypedArrayType {
                                    element_type: Box::new(ConcreteType::BasicType(BasicConcreteType::Char)),
                                    capacity: TypedArrayCapacity::Fixed(len.try_into().unwrap()),
                                    loc: literal.loc.clone(),
                                }));
                            }
                            ast::StringPrefix::C => {}
                        }
                    }
                    Some(ConcreteType::Pointer(Box::new(ConcreteType::BasicType(
                        BasicConcreteType::Char,
                    ))))
                }
                LiteralKind::Char(_) => Some(ConcreteType::BasicType(BasicConcreteType::Char)),
                LiteralKind::Null => Some(ConcreteType::BasicType(BasicConcreteType::Null)),
            },
            TypedExpression::Prefix(typed_prefix_expr) => self.get_prefix_expr_type(typed_prefix_expr),
            TypedExpression::Infix(typed_infix_expr) => self.get_infix_expr_type(typed_infix_expr),
            TypedExpression::Unary(typed_unary_expr) => self.get_unary_expr_type(typed_unary_expr),
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
