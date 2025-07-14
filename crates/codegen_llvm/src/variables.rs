use ast::{
    ast::{AccessSpecifier, Expression, GlobalVariable, TypeSpecifier, Variable},
    token::TokenKind,
};
use inkwell::{
    AddressSpace,
    module::Linkage,
    values::{AnyValue, BasicValueEnum, GlobalValue},
};

use crate::{
    CodeGenLLVM,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    scope::{Scope, ScopeRecord, ScopeRef},
    types::InternalType,
};
use std::{cell::RefCell, collections::HashMap, process::exit, rc::Rc};

pub type GlobalVariablesTable<'a> = HashMap<String, GlobalVariableMetadata<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVariableMetadata<'a> {
    pub name: String,
    pub variable_type: InternalType<'a>,
    pub global_value: GlobalValue<'a>,
    pub access_specifier: AccessSpecifier,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_global_variable_linkage(&self, access_specifier: AccessSpecifier) -> Linkage {
        match access_specifier {
            AccessSpecifier::PublicExtern => Linkage::Common,
            AccessSpecifier::Extern => Linkage::Common,
            AccessSpecifier::Public => Linkage::External,
            AccessSpecifier::Internal => Linkage::Private,
            AccessSpecifier::Inline => unreachable!(),
            AccessSpecifier::PublicInline => unreachable!(),
        }
    }

    pub(crate) fn build_global_variable(&mut self, global_variable: GlobalVariable) {
        let initializer_value = match global_variable.expr {
            Some(expression) => {
                match expression {
                    // Expression::Cast(cast) => self.build_cast_expression(scope, cast),
                    // Expression::Identifier(identifier) => todo!(), // Distinct runtime and comptime values
                    expr @ Expression::Literal(..)
                    | expr @ Expression::Prefix(..)
                    | expr @ Expression::Infix(..)
                    | expr @ Expression::UnaryOperator(..)
                    | expr @ Expression::ModuleImport(..)
                    | expr @ Expression::Array(..) => {
                        let fake_scope: ScopeRef<'ctx> = Rc::new(RefCell::new(Scope::new()));
                        self.build_expr(fake_scope, expr)
                    }

                    _ => {
                        display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(
                        "Invalid initializer for global variable. Global variables must be initialized with a compile-time constant expression."
                            .to_string(),
                    ),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: global_variable.loc.line,
                        column: global_variable.loc.column,
                        length: global_variable.span.end,
                    }),
                });
                        exit(1);
                    }
                }
            }
            None => {
                let internal_type = self.build_type(
                    global_variable.type_specifier.clone().unwrap(),
                    global_variable.loc.clone(),
                    global_variable.span.end,
                );
                self.build_zero_initialized_internal_value(
                    internal_type,
                    global_variable.loc.clone(),
                    global_variable.span.end,
                )
            }
        };

        let variable_type: InternalType<'ctx>;
        if let Some(type_specifier) = global_variable.type_specifier {
            variable_type = self.build_type(type_specifier, global_variable.loc.clone(), global_variable.span.end);
        } else {
            variable_type = initializer_value.clone().get_type(self.string_type.clone())
        }

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let module = self.module.borrow_mut();

        let variable_basic_type = match variable_type.to_basic_type(ptr_type) {
            Ok(basic_type) => basic_type,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(err.to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: global_variable.loc.line,
                        column: global_variable.loc.column,
                        length: global_variable.span.end,
                    }),
                });
                exit(1);
            }
        };

        if matches!(
            global_variable.access_specifier,
            AccessSpecifier::Inline | AccessSpecifier::PublicInline
        ) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Cannot declare an inline global variable.".to_string()),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: global_variable.loc.line,
                    column: global_variable.loc.column,
                    length: global_variable.span.end,
                }),
            });
            exit(1);
        }

        let linkage = self.build_global_variable_linkage(global_variable.access_specifier.clone());

        let initialzier_basic_value: BasicValueEnum<'ctx> = initializer_value
            .to_basic_metadata()
            .as_any_value_enum()
            .try_into()
            .unwrap();

        let global_value = module.add_global(variable_basic_type, None, &global_variable.identifier.name);
        global_value.set_linkage(linkage);
        global_value.set_initializer(&initialzier_basic_value);

        self.global_variables_table.insert(
            global_variable.identifier.name.clone(),
            GlobalVariableMetadata {
                name: global_variable.identifier.name,
                variable_type,
                global_value,
                access_specifier: global_variable.access_specifier,
            },
        );
    }

    pub(crate) fn build_variable(&mut self, scope: ScopeRef<'ctx>, variable: Variable) {
        match variable.ty {
            Some(type_specifier) => {
                if let TypeSpecifier::TypeToken(type_token) = type_specifier.clone() {
                    if type_token.kind == TokenKind::Void {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom("Cannot declare a variable with 'void' type.".to_string()),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: variable.loc.line,
                                column: variable.loc.column,
                                length: variable.span.end,
                            }),
                        });
                        exit(1);
                    }
                }

                let (ptr, var_internal_type) = self.build_alloca(
                    type_specifier.clone(),
                    variable.name.clone(),
                    variable.loc.clone(),
                    variable.span.end,
                );

                if let Some(expr) = variable.expr {
                    let expr = self.build_expr(Rc::clone(&scope), expr);
                    let rvalue = self.internal_value_as_rvalue(expr, variable.loc.clone(), variable.span.end);

                    if !self.compatible_types(var_internal_type.clone(), rvalue.get_type(self.string_type.clone())) {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(format!(
                                "Cannot assign value of type '{}' to lvalue of type '{}'.",
                                rvalue.get_type(self.string_type.clone()),
                                var_internal_type,
                            )),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: variable.loc.line,
                                column: variable.loc.column,
                                length: variable.span.end,
                            }),
                        });
                        exit(1);
                    };

                    let final_rvalue = self.implicit_cast(
                        rvalue,
                        self.build_type(type_specifier, variable.loc.clone(), variable.span.end),
                        variable.loc.clone(),
                        variable.span.end,
                    );

                    self.builder.build_store(ptr, final_rvalue).unwrap();
                } else if var_internal_type.is_const_type() && variable.expr.is_none() {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Variable '{}' is declared as constant but has no initializer.",
                            variable.name
                        )),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: variable.loc.line,
                            column: variable.loc.column,
                            length: variable.span.end,
                        }),
                    });
                    exit(1);
                } else {
                    let zero_init = self.build_zero_initialized_internal_value(
                        var_internal_type.clone(),
                        variable.loc.clone(),
                        variable.span.end,
                    );
                    let final_rvalue: BasicValueEnum =
                        zero_init.to_basic_metadata().as_any_value_enum().try_into().unwrap();
                    self.builder.build_store(ptr, final_rvalue).unwrap();
                }

                let mut scope_borrow = scope.borrow_mut();

                if scope_borrow.get(variable.name.clone()).is_some() {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Variable '{}' would shadow a previous declaration.",
                            variable.name
                        )),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: variable.loc.line,
                            column: variable.loc.column,
                            length: variable.span.end,
                        }),
                    });
                    exit(1);
                }

                scope_borrow.insert(
                    variable.name.clone(),
                    ScopeRecord {
                        ptr,
                        ty: var_internal_type,
                    },
                );
            }
            None => {
                if let Some(expr) = variable.expr {
                    let expr = self.build_expr(Rc::clone(&scope), expr);
                    let rvalue = self.internal_value_as_rvalue(expr, variable.loc.clone(), variable.span.end);
                    let var_internal_type = rvalue.get_type(self.string_type.clone());

                    let var_basic_type =
                        match var_internal_type.to_basic_type(self.context.ptr_type(AddressSpace::default())) {
                            Ok(basic_type) => basic_type,
                            Err(err) => {
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::Custom(err.to_string()),
                                    location: Some(DiagLoc {
                                        file: self.file_path.clone(),
                                        line: variable.loc.line,
                                        column: variable.loc.column,
                                        length: variable.span.end,
                                    }),
                                });
                                exit(1);
                            }
                        };

                    let ptr = self.builder.build_alloca(var_basic_type, &variable.name).unwrap();

                    self.build_store(ptr, rvalue);

                    scope.borrow_mut().insert(
                        variable.name,
                        ScopeRecord {
                            ptr,
                            ty: var_internal_type,
                        },
                    );
                } else {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::TypeAnnotationRequired,
                        location: None,
                    });
                    exit(1);
                }
            }
        }
    }
}
