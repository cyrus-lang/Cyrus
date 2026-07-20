// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::{InterfaceDecl, MethodDecls},
    format::format_sema_type,
    stmts::{TypedImplementInterface, TypedInterfaceStmt, TypedTypeArgs},
    substitute::instantiate_method_decl,
    types::{NamedType, SemaType, TypeDeclID},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_interface(&mut self, interface: &TypedInterfaceStmt) {
        let mut is_interface_dynamically_dispatchable = true;

        let interface_name = &interface.name;

        self.nameconv_check_interface_name(&interface_name, interface.loc);

        let mut methods: Vec<String> = Vec::new();

        for (_, method_decl_id) in interface.methods.iter() {
            let mut method_decl = self.decl_tables.method_decl(*method_decl_id);

            self.analyze_method_decl(TypeDeclID::Interface(interface.interface_decl_id), &mut method_decl);

            self.decl_tables.with_method_decl_mut(*method_decl_id, |_method_decl| {
                *_method_decl = method_decl.clone();
            });

            let func_decl = method_decl.func_decl;

            let params_start = if let Some(self_modifier) = func_decl.params.get_self_modifier() {
                if !self_modifier.kind.is_referenced() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InterfaceMethodsMustUseReferencedSelf),
                        loc: Some(self_modifier.loc),
                        hint: None,
                    });
                }
                1
            } else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InterfaceMethodsMustHaveSelfModifier),
                    loc: Some(func_decl.loc),
                    hint: None,
                });
                0
            };

            // skip the self param (index 0)
            let params = &func_decl.params.list[params_start..];

            for param_kind in params {
                let ty = param_kind.param_type();

                if ty.contains_self_type() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InterfaceMethodParamContainsSelf {
                            interface_name: interface_name.clone(),
                            method_name: func_decl.name.clone(),
                        }),
                        loc: Some(param_kind.loc()),
                        hint: None,
                    });
                }
            }

            if func_decl.is_generic() {
                is_interface_dynamically_dispatchable = false;
            }

            if func_decl.ret_type.contains_self_type() {
                is_interface_dynamically_dispatchable = false;
            }

            if methods.contains(&func_decl.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InterfaceDuplicateMethod {
                        interface_name: interface_name.clone(),
                        method_name: func_decl.name.clone(),
                    }),
                    loc: Some(func_decl.loc),
                    hint: None,
                });
                continue;
            }

            methods.push(func_decl.name.clone());
        }

        self.decl_tables
            .with_interface_decl_mut(interface.interface_decl_id, |interface_decl| {
                interface_decl.is_interface_dynamically_dispatchable = is_interface_dynamically_dispatchable;
            })
    }

    /// Validates that an object correctly implements its declared interfaces.
    ///
    /// Ensures the referenced symbols are valid interfaces, checks visibility
    /// rules, resolves generic interface instantiations, and verifies that all
    /// required interface methods are implemented with matching signatures.
    pub(crate) fn analyze_object_implements_interfaces(
        &mut self,
        object_name: &String,
        is_object_generic: bool,
        impls: &Vec<TypedImplementInterface>,
        method_decls: &MethodDecls,
    ) {
        for implement_interface in impls {
            let Some(normalized_type) =
                self.normalize_and_check_type_formation(implement_interface.ty.clone(), implement_interface.loc, 0)
            else {
                continue;
            };

            let sema_type = self.expand_sema_type(normalized_type, implement_interface.loc);

            let interface_decl_id = sema_type.as_named_interface();

            let interface_decl = match interface_decl_id {
                Some(id) => self.decl_tables.interface_decl(id),
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::NonInterfaceSymbol {
                            symbol_name: format_sema_type(sema_type, self.formatter),
                        }),
                        loc: Some(implement_interface.loc),
                        hint: None,
                    });
                    continue;
                }
            };

            let interface_type_args = &sema_type.as_named_type().unwrap().type_args;

            for (_, method_decl_id) in interface_decl.methods.iter() {
                let interface_method_decl = self.decl_tables.method_decl(*method_decl_id);

                let func_decl = instantiate_method_decl(
                    &interface_method_decl,
                    &interface_decl.generic_params,
                    interface_type_args,
                )
                .func_decl;

                if !method_decls.contains(&func_decl.name) {
                    // method missing
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::MissingInterfaceMethodImpl {
                            object_name: object_name.clone(),
                            method_name: func_decl.name.clone(),
                            interface_name: interface_decl.name.clone(),
                        }),
                        loc: Some(implement_interface.loc),
                        hint: None,
                    });
                    continue;
                }

                let method_decl_id = method_decls.get(&func_decl.name).unwrap();
                let method_decl = self.decl_tables.method_decl(method_decl_id);

                if !is_object_generic && !method_decl.func_decl.is_generic() {
                    // check method declaration mismatch
                    if method_decl.func_decl != func_decl {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::InterfaceMethodTypeMismatch {
                                object_name: object_name.clone(),
                                method_name: func_decl.name.clone(),
                                interface_name: interface_decl.name.clone(),
                            }),
                            loc: Some(func_decl.loc),
                            hint: None,
                        });
                    }
                }
            }

            if !is_object_generic
                && self
                    .check_type_correctness(sema_type.clone(), implement_interface.loc)
                    .is_none()
            {
                continue;
            }
        }
    }

    pub(crate) fn analyze_object_implements_monomorphized_interfaces(
        &mut self,
        interface_decl: &InterfaceDecl,
        interface_type_args: TypedTypeArgs,
        object_name: &String,
        object_type: &SemaType,
        impls: &Vec<TypedImplementInterface>,
        method_decls: &MethodDecls,
        loc: Loc,
    ) -> Option<()> {
        let generic_params = interface_decl.generic_params.clone();
        let generic_env =
            self.create_inference_generic_env(&interface_decl.name, generic_params, &interface_type_args, loc)?;

        self.with_generic_env(generic_env, |this| {
            for implement_interface in impls {
                let Some(normalized_type) =
                    this.normalize_sema_type(implement_interface.ty.clone(), implement_interface.loc, 0)
                else {
                    continue;
                };

                let sema_type = this.expand_sema_type(normalized_type, implement_interface.loc);

                let interface_decl_id = sema_type.as_named_interface();

                let interface_decl = match interface_decl_id {
                    Some(id) => this.decl_tables.interface_decl(id),
                    None => {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::NonInterfaceSymbol {
                                symbol_name: format_sema_type(sema_type, this.formatter),
                            }),
                            loc: Some(implement_interface.loc),
                            hint: None,
                        });
                        continue;
                    }
                };

                for (_, method_decl_id) in interface_decl.methods.iter() {
                    // interface method
                    let interface_method_decl = &mut this.decl_tables.method_decl(*method_decl_id).func_decl;

                    let self_modifier = interface_method_decl.params.get_self_modifier_mut().unwrap();
                    self_modifier.ty = this.substitute_self_type(self_modifier.ty.clone(), object_type);

                    interface_method_decl.params = this.substitute_func_params(interface_method_decl.params.clone());
                    interface_method_decl.ret_type = this.substitute_type(&interface_method_decl.ret_type);

                    // object method
                    let Some(method_decl_id) = method_decls.get(&interface_method_decl.name) else {
                        continue;
                    };

                    let method_decl = &mut this.decl_tables.method_decl(method_decl_id).func_decl;

                    if method_decl.is_generic() {
                        // SKIP comparison for generic methods
                        // generic methods will be validated during monomorphization
                        continue;
                    }

                    let self_modifier = method_decl.params.get_self_modifier_mut().unwrap();
                    self_modifier.ty = this.substitute_self_type(self_modifier.ty.clone(), object_type);

                    method_decl.params = this.substitute_func_params(method_decl.params.clone());
                    method_decl.ret_type = this.substitute_type(&method_decl.ret_type);

                    // check method declaration mismatch
                    if method_decl != interface_method_decl {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::InterfaceMethodTypeMismatch {
                                object_name: object_name.clone(),
                                method_name: interface_method_decl.name.clone(),
                                interface_name: interface_decl.name.clone(),
                            }),
                            loc: Some(interface_method_decl.loc),
                            hint: None,
                        });
                    }
                }

                if this
                    .check_type_correctness(sema_type.clone(), implement_interface.loc)
                    .is_none()
                {
                    continue;
                }
            }

            Some(())
        })
    }

    pub(crate) fn implement_interfaces_of_named_type(
        &self,
        named_type: &NamedType,
    ) -> Option<Vec<TypedImplementInterface>> {
        match named_type.type_decl_id {
            TypeDeclID::Struct(struct_decl_id) => {
                let struct_decl = self.decl_tables.struct_decl(struct_decl_id);

                Some(struct_decl.impls)
            }
            TypeDeclID::Enum(enum_decl_id) => {
                let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

                Some(enum_decl.impls)
            }
            TypeDeclID::Union(union_decl_id) => {
                let union_decl = self.decl_tables.union_decl(union_decl_id);

                Some(union_decl.impls)
            }
            TypeDeclID::Interface(_) => None,
            TypeDeclID::Typedef(_) => None,
        }
    }
}
