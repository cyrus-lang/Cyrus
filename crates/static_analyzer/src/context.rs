use crate::diagnostics::AnalyzerDiagKind;
use diagcentral::{Diag, DiagLevel, DiagLoc, reporter::DiagReporter};
use resolver::{
    Resolver,
    scope::{LocalOrGlobalSymbol, LocalSymbol, SymbolEntry},
};
use typed_ast::{format::format_concrete_type, *};

pub struct AnalysisContext<'a> {
    pub ast: &'a TypedProgramTree,
    pub resolver: &'a Resolver,
    pub reporter: DiagReporter<AnalyzerDiagKind>,
    pub module_id: ModuleID,
    pub symbol_formatter: Box<dyn Fn(Option<ScopeID>) -> Box<dyn Fn(SymbolID) -> String + 'a> + 'a>,
}

impl<'a> AnalysisContext<'a> {
    pub fn new(resolver: &'a Resolver, module_id: ModuleID, ast: &'a TypedProgramTree) -> Self {
        let symbol_formatter = Box::new(
            move |scope_id_opt: Option<ScopeID>| -> Box<dyn Fn(SymbolID) -> String> {
                Box::new(move |symbol_id: SymbolID| -> String {
                    let local_scope_opt =
                        scope_id_opt.and_then(|scope_id| resolver.get_scope_ref(module_id, scope_id).clone());

                    let local_or_global_symbol = resolver
                        .resolve_local_or_global_symbol(module_id, local_scope_opt.clone(), symbol_id)
                        .unwrap();

                    match local_or_global_symbol {
                        LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match local_symbol {
                            LocalSymbol::Variable(resolved_variable) => resolved_variable.typed_variable.name.clone(),
                            LocalSymbol::Struct(resolved_struct) => resolved_struct.struct_sig.name.clone(),
                            LocalSymbol::Enum(resolved_enum) => resolved_enum.enum_sig.name.clone(),
                            LocalSymbol::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.name.clone(),
                            LocalSymbol::Interface(resolved_interface) => resolved_interface.interface_sig.name.clone(),
                        },
                        LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match symbol_entry {
                            SymbolEntry::Method(resolved_method) => resolved_method.func_sig.name.clone(),
                            SymbolEntry::Func(resolved_function) => resolved_function.func_sig.name.clone(),
                            SymbolEntry::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.name.clone(),
                            SymbolEntry::GlobalVar(resolved_global_var) => {
                                resolved_global_var.global_var_sig.name.clone()
                            }
                            SymbolEntry::Struct(resolved_struct) => resolved_struct.struct_sig.name.clone(),
                            SymbolEntry::Enum(resolved_enum) => resolved_enum.enum_sig.name.clone(),
                            SymbolEntry::Interface(resolved_interface) => resolved_interface.interface_sig.name.clone(),
                        },
                    }
                })
            },
        );

        Self {
            ast,
            resolver,
            reporter: DiagReporter::new(),
            module_id,
            symbol_formatter,
        }
    }

    // Traverse TypedAST
    pub fn analyze(&mut self) {
        for typed_stmt in &self.ast.body {
            match typed_stmt {
                TypedStatement::FuncDef(typed_func_def) => self.analyze_block_statement(&typed_func_def.body),
                TypedStatement::FuncDecl(typed_func_decl) => self.analyze_func_decl(typed_func_decl),
                TypedStatement::Interface(typed_interface) => self.analyze_interface(typed_interface),
                TypedStatement::Import(typed_import) => todo!(),
                TypedStatement::Struct(typed_struct) => self.analyze_struct(typed_struct),
                TypedStatement::Enum(typed_enum) => self.analyze_enum(typed_enum),
                TypedStatement::GlobalVariable(_) => {}
                // Not analyzed
                TypedStatement::Typedef(_) => continue,
                // Invalid top-level statements
                TypedStatement::Variable(_) => todo!(),
                TypedStatement::BlockStatement(_) => unreachable!(),
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

    pub(crate) fn analyze_struct(&mut self, typed_struct: &TypedStruct) {
        let mut field_names: Vec<String> = Vec::new();

        for field in &typed_struct.fields {
            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DuplicateFieldName {
                        struct_name: typed_struct.name.clone(),
                        field_name: field.name.clone(),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        field.loc.clone(),
                        0,
                    )),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
                continue;
            }

            field_names.push(field.name.clone());
        }
    }

    pub(crate) fn analyze_enum(&mut self, typed_enum: &TypedEnum) {
        let mut variant_names: Vec<String> = Vec::new();

        for variant in &typed_enum.variants {
            let variant_identifier = match variant {
                TypedEnumVariant::Identifier(identifier) => identifier,
                TypedEnumVariant::Valued(identifier, _) => identifier,
                TypedEnumVariant::Variant(identifier, typed_enum_valued_fields) => {
                    let mut field_names: Vec<String> = Vec::new();

                    for field in typed_enum_valued_fields {
                        if field_names.contains(&field.name) {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: AnalyzerDiagKind::DuplicateEnumFieldName {
                                    enum_name: typed_enum.name.clone(),
                                    field_name: field.name.clone(),
                                    variant_name: identifier.name.clone()
                                },
                                location: Some(DiagLoc::new(
                                    self.resolver.get_current_module_file_path(),
                                    field.loc.clone(),
                                    0,
                                )),
                                hint: Some("Consider to rename the field to a different name.".to_string()),
                            });
                            continue;
                        }

                        field_names.push(field.name.clone());
                    }
                    identifier
                }
            };

            if variant_names.contains(&variant_identifier.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DuplicateEnumVariantName {
                        enum_name: typed_enum.name.clone(),
                        variant_name: variant_identifier.name.clone(),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        variant_identifier.loc.clone(),
                        0,
                    )),
                    hint: Some("Consider to rename the variant to a different name.".to_string()),
                });
                continue;
            }

            variant_names.push(variant_identifier.name.clone());
        }
    }

    pub(crate) fn check_duplicate_param_names(
        &mut self,
        params: &[TypedFuncParamKind],
        variadic: Option<&TypedFuncVariadicParams>,
        location: DiagLoc,
    ) {
        let mut param_names: Vec<String> = Vec::new();

        for (param_idx, param) in params.iter().enumerate() {
            match param {
                TypedFuncParamKind::FuncParam(typed_func_param) => {
                    if param_names.contains(&typed_func_param.name) {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::DuplicateFuncParameter {
                                param_name: typed_func_param.name.clone(),
                                param_idx: param_idx.try_into().unwrap(),
                            },
                            location: Some(location.clone()),
                            hint: Some("Consider to rename the parameter to a different name.".to_string()),
                        });
                        continue;
                    }

                    param_names.push(typed_func_param.name.clone());
                }
                TypedFuncParamKind::SelfModifier(_) => {
                    param_names.push("self".to_string());
                }
            }
        }

        if let Some(variadic_param) = variadic {
            match variadic_param {
                TypedFuncVariadicParams::Typed(identifier, _) => {
                    if param_names.contains(identifier) {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::DuplicateFuncVariadicParameter {
                                param_name: identifier.clone(),
                            },
                            location: Some(location.clone()),
                            hint: Some("Consider to rename the parameter to a different name.".to_string()),
                        });
                    }
                }
                TypedFuncVariadicParams::UntypedCStyle => {}
            }
        }
    }

    fn analyze_func_decl(&mut self, typed_func_decl: &TypedFuncDecl) {
        self.check_duplicate_param_names(
            &typed_func_decl.params.list,
            typed_func_decl.params.variadic.as_ref(),
            DiagLoc::new(
                self.resolver.get_current_module_file_path(),
                typed_func_decl.loc.clone(),
                0,
            ),
        );
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
                continue;
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
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        if typed_variable.ty.is_none() && typed_variable.rhs.is_none() {
            return;
        }

        let value_type = match self.get_typed_expr_type(scope_id_opt, &typed_variable.rhs.clone().unwrap()) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

        if let Some(var_type) = &typed_variable.ty {
            let lhs_type = format_concrete_type(var_type.clone(), &formatter_closure);
            let rhs_type = format_concrete_type(value_type.clone(), &formatter_closure);

            if !self.check_type_mismatch(value_type.clone(), var_type.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type },
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
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let lhs_type = match self.get_typed_expr_type(scope_id_opt, &typed_assignment.lhs) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

        let rhs_type = match self.get_typed_expr_type(scope_id_opt, &typed_assignment.rhs) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

        if !self.check_type_mismatch(rhs_type.clone(), lhs_type.clone()) {
            let lhs_type = format_concrete_type(lhs_type, &formatter_closure);
            let rhs_type = format_concrete_type(rhs_type, &formatter_closure);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type },
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
