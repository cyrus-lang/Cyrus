use crate::diagnostics::AnalyzerDiagKind;
use ast::{LiteralKind, operators::PrefixOperator};
use diagcentral::{Diag, DiagLevel, DiagLoc, reporter::DiagReporter};
use resolver::{
    Resolver,
    scope::{LocalOrGlobalSymbol, LocalSymbol, SymbolEntry},
};
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
                TypedStatement::Import(typed_import) => todo!(),
                TypedStatement::GlobalVariable(typed_global_variable) => todo!(),
                TypedStatement::Variable(typed_variable) => todo!(),
                TypedStatement::FuncDecl(typed_func_decl) => todo!(),
                TypedStatement::BlockStatement(typed_block_statement) => todo!(),
                TypedStatement::Struct(typed_struct) => todo!(),
                TypedStatement::Enum(typed_enum) => todo!(),
                TypedStatement::Interface(typed_interface) => self.analyze_interface(typed_interface),
                // Not analyzed
                TypedStatement::Typedef(_) => continue,
                // Invalid top-level statements
                TypedStatement::If(_) => unreachable!(),
                TypedStatement::Return(_) => unreachable!(),
                TypedStatement::Break(_) => unreachable!(),
                TypedStatement::Continue(_) => unreachable!(),
                TypedStatement::For(_) => unreachable!(),
                TypedStatement::Foreach(_) => unreachable!(),
                TypedStatement::Switch(_) => unreachable!(),
                TypedStatement::Expression(_) => {
                    unreachable!()
                }
            }
        }
    }

    fn analyze_interface(&mut self, typed_interface: &TypedInterface) {
        let mut name_list: Vec<String> = Vec::new();

        let resolved_interface = self
            .resolver
            .lookup_symbol_entry_with_id(self.module_id, typed_interface.symbol_id)
            .unwrap();

        let interface_name = resolved_interface.as_interface().unwrap().interface_sig.name.clone();

        for method in &typed_interface.methods {
            if name_list.contains(&method.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InterfaceDuplicateMethod {
                        interface_name: interface_name.clone(),
                        method_name: method.name.clone(),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        method.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
            }

            name_list.push(method.name.clone());
        }
    }

    fn analyze_block_statement(&mut self, block_stmt: &TypedBlockStatement) {
        for typed_stmt in &block_stmt.exprs {
            match typed_stmt {
                TypedStatement::Variable(typed_variable) => {
                    self.analyze_variable(Some(block_stmt.scope_id), typed_variable)
                }
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
                    self.get_typed_expr_type(Some(block_stmt.scope_id), typed_expression);
                }
                // Invalid statements
                TypedStatement::FuncDef(_) => unreachable!(),
                TypedStatement::FuncDecl(_) => unreachable!(),
                TypedStatement::Import(_) => unreachable!(),
                TypedStatement::GlobalVariable(_) => unreachable!(),
            }
        }
    }

    fn analyze_variable(&mut self, scope_id_opt: Option<ScopeID>, typed_variable: &TypedVariable) {
        if typed_variable.ty.is_none() && typed_variable.rhs.is_none() {
            return;
        }

        let value_type = match self.get_typed_expr_type(scope_id_opt, &typed_variable.rhs.clone().unwrap()) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

        if let Some(var_type) = &typed_variable.ty {
            if !self.check_type_mismatch(value_type.clone(), var_type.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::AssignmentTypeMismatch {
                        lhs_type: format_concrete_type(var_type.clone(), self.get_symbol_formatter()),
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
    }

    pub(crate) fn analyze_assignment(&mut self, scope_id_opt: Option<ScopeID>, typed_assignment: &TypedAssignment) {
        let lhs_type = match self.get_typed_expr_type(scope_id_opt, &typed_assignment.lhs) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

        let rhs_type = match self.get_typed_expr_type(scope_id_opt, &typed_assignment.rhs) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

        if !self.check_type_mismatch(rhs_type.clone(), lhs_type.clone()) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::AssignmentTypeMismatch {
                    lhs_type: format_concrete_type(lhs_type, self.get_symbol_formatter()),
                    rhs_type: format_concrete_type(rhs_type, self.get_symbol_formatter()),
                },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    typed_assignment.loc.clone(),
                    0,
                )),
                hint: None,
            });
        }
    }
}
