/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

use crate::{context::AnalysisContext, infer::InferCtx};
use cyrusc_typed_ast::{
    decls::{MethodDecl, MethodDeclID, MethodDecls},
    stmts::{TypedFuncParamKind, TypedTypeArgs},
    types::{NamedType, SemaType, TypeDeclID},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_object_methods(&mut self, object_type_decl_id: TypeDeclID, method_decls: &MethodDecls) {
        // pass 1: declarations
        for (_, method_decl_id) in method_decls.iter() {
            let mut method_decl = self.decl_tables.method_decl(*method_decl_id);

            self.analyze_method_decl(object_type_decl_id, &mut method_decl);

            self.decl_tables.with_method_decl_mut(*method_decl_id, |_method_decl| {
                *_method_decl = method_decl;
            });
        }

        // pass 2: bodies
        for (_, method_decl_id) in method_decls.iter() {
            let mut method_decl = self.decl_tables.method_decl(*method_decl_id);

            if !method_decl.func_decl.is_generic() {
                self.analyze_method_body(*method_decl_id, &mut method_decl);
            } else {
                // generic methods are being analyzed in call-site
            }
        }
    }

    pub(crate) fn analyze_method_decl(&mut self, object_type_decl_id: TypeDeclID, method_decl: &mut MethodDecl) {
        // If the method is non-generic, eagerly lower all SelfType occurrences
        // to the concrete object type. Generic methods defer SelfType substitution
        // to the call-site via the instantiated generic environment.
        if !method_decl.func_decl.is_generic() {
            let concrete_self_type = SemaType::Named(NamedType {
                type_decl_id: object_type_decl_id,
                type_args: TypedTypeArgs::new(),
            });

            self.apply_self_type_in_method_decl_and_variable(method_decl, &concrete_self_type);
        }

        self.analyze_func_decl(&mut method_decl.func_decl);

        self.nameconv_check_method_name(&method_decl.func_decl.name, method_decl.func_decl.loc);
    }

    pub(crate) fn analyze_method_body(&mut self, method_decl_id: MethodDeclID, method_decl: &mut MethodDecl) {
        let func_type = method_decl.func_decl.as_func_type();

        let method_env = self.create_method_env(method_decl_id, func_type.clone(), Some(InferCtx::new()));

        let body_id = method_decl.body.unwrap();

        self.with_func_env(method_env, |this| {
            let mut body = this.decl_tables.body(body_id);

            this.analyze_func_body(&mut body, &method_decl.func_decl.ret_type);

            this.decl_tables.with_body_mut(body_id, |_body| {
                *_body = body;
            });
        });
    }

    pub(crate) fn apply_self_type_in_method_decl_and_variable(
        &mut self,
        method_decl: &mut MethodDecl,
        concrete_self_type: &SemaType,
    ) {
        for param_kind in method_decl.func_decl.params.list.iter_mut() {
            match param_kind {
                TypedFuncParamKind::FuncParam(func_param) => {
                    func_param.ty = self.substitute_self_type(func_param.ty.clone(), concrete_self_type);

                    if let Some(infer) = &self.func_env.infer {
                        func_param.ty = infer.resolve(&func_param.ty);
                    }
                }
                TypedFuncParamKind::SelfModifier(self_modifier) => {
                    self_modifier.ty = self.substitute_self_type(self_modifier.ty.clone(), concrete_self_type);

                    if let Some(infer) = &self.func_env.infer {
                        self_modifier.ty = infer.resolve(&self_modifier.ty);
                    }

                    self.decl_tables
                        .with_var_decl_mut(self_modifier.var_decl_id.unwrap(), |_var_decl| {
                            _var_decl.ty = Some(self_modifier.ty.clone());
                        });
                }
            }
        }

        let mut substituted_ret_type =
            self.substitute_self_type(method_decl.func_decl.ret_type.clone(), concrete_self_type);

        if let Some(infer) = &self.func_env.infer {
            substituted_ret_type = infer.resolve(&substituted_ret_type);
        }

        method_decl.func_decl.ret_type = substituted_ret_type;
    }
}
