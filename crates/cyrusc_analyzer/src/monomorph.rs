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

use crate::context::AnalysisContext;
use cyrusc_internal::monomorph::MonomorphizableTemplateID;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    builtins::TypedBuiltin,
    decls::{DeclID, FuncDecl, FuncDeclID, MethodDecl, MethodDeclID, MonomorphID, VarDecl, VarDeclID},
    exprs::{
        TypedEnumInitArgs, TypedExpr, TypedExprKind, TypedFuncCall, TypedFuncCallDispatch, TypedSymbolExpr,
        TypedUnnamedEnumValueKind,
    },
    format::format_loc,
    stmts::{
        TypedBlockStmt, TypedFuncParamKind, TypedFuncParams, TypedFuncVariadicParam, TypedIfStmt, TypedStmt,
        TypedSwitchCasePattern, TypedSwitchCasePatternKind, TypedTupleExportPattern, TypedTupleExportPatternKind,
        TypedTypeArgs, TypedVarStmt,
    },
    types::SemaType,
};
use fx_hash::FxHashMap;

type DeclMap = FxHashMap<VarDeclID, VarDeclID>;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn monomorphize_generic_method_call(
        &mut self,
        operand_type: SemaType,
        method_decl_id: MethodDeclID,
        mut method_decl: MethodDecl,
        is_generic_interface_method_call: bool,
        loc: Loc,
    ) -> Option<(MonomorphID, TypedTypeArgs)> {
        let pure_operand_type = operand_type.const_inner().pointer_inner().clone();

        if !is_generic_interface_method_call {
            // substitute Self inside params, ret_type, variables
            self.apply_self_type_in_method_decl_and_variable(&mut method_decl, &pure_operand_type);
        }

        method_decl.func_decl.params = self.substitute_func_params(method_decl.func_decl.params.clone());
        method_decl.func_decl.ret_type = self.substitute_type(&method_decl.func_decl.ret_type);

        self.check_type_arity(method_decl.func_decl.ret_type.clone(), loc)?;

        let final_type_args = self.collect_instantiated_type_args(method_decl.func_decl.generic_params.clone());

        // get or create monomorph instance
        let monomorph_id = self.monomorph_registry.get_or_create(
            MonomorphizableTemplateID::Method(method_decl_id),
            final_type_args.clone(),
            method_decl.func_decl.params.clone(),
            method_decl.func_decl.ret_type.clone(),
        );

        let monomorph_instance = self.monomorph_registry.get(monomorph_id);

        // analyze monomorphized body if not analyzed yet
        if !is_generic_interface_method_call && !monomorph_instance.analyzed {
            let body_id = method_decl.body.unwrap();
            let template_body = self.decl_tables.body(body_id);

            let mut specialized_body = self.specialize_func_body(&template_body, &mut method_decl.func_decl.params);

            let diag_len = self.reporter.len();

            self.analyze_func_body(&mut specialized_body, &method_decl.func_decl.ret_type);

            let monomorph_body_id = self.monomorph_registry.insert_monomorph_body(specialized_body);

            let diag_origin = format_loc(&self.source_map, loc);

            if self.reporter.len() > diag_len {
                self.apply_error_originated_from_on_diag_range(diag_len..=diag_len, |diag| {
                    diag.hint = Some(format!("Error originates from self method call at {}.", diag_origin));
                });
            }

            self.monomorph_registry.update(monomorph_id, |inst| {
                inst.analyzed = true;
                inst.body = Some(monomorph_body_id);

                // ensure unique var_decl_id remapping per monomorph instance
                inst.params = method_decl.func_decl.params.clone();
            });
        }

        Some((monomorph_id, final_type_args))
    }

    pub(crate) fn monomorphize_generic_func_call(
        &mut self,
        operand_type: &SemaType,
        func_call: &mut TypedFuncCall,
        func_decl_id: FuncDeclID,
        func_decl: &mut FuncDecl,
        final_type_args: TypedTypeArgs,
    ) -> Option<SemaType> {
        func_call.operand.ty = Some(self.substitute_type(&operand_type));
        func_decl.params = self.substitute_func_params(func_decl.params.clone());
        func_decl.ret_type = self.substitute_type(&func_decl.ret_type);

        // get or get monomorph instance
        let monomorph_id = self.monomorph_registry.get_or_create(
            MonomorphizableTemplateID::Func(func_decl_id),
            final_type_args.clone(),
            func_decl.params.clone(),   // specialized params
            func_decl.ret_type.clone(), // specialized ret
        );

        let monomorph_instance = self.monomorph_registry.get(monomorph_id);

        // analyze monomorphized body if not analyzed yet
        if !monomorph_instance.analyzed {
            // IMPORTANT: to prevent infinite-recursion for recursive-generic-funcs
            self.monomorph_registry.update(monomorph_id, |_monomorph_instance| {
                _monomorph_instance.analyzed = true;
            });

            let body_id = func_decl.body.unwrap();
            let template_body = self.decl_tables.body(body_id);

            let mut specialized_body = self.specialize_func_body(&template_body, &mut func_decl.params);

            let diag_len = self.reporter.len();

            self.analyze_func_body(&mut specialized_body, &func_decl.ret_type);

            let monomorph_body_id = self.monomorph_registry.insert_monomorph_body(specialized_body);

            let diag_origin = format_loc(&self.source_map, func_call.loc);

            if self.reporter.len() > diag_len {
                self.apply_error_originated_from_on_diag_range(diag_len..=diag_len, |diag| {
                    diag.hint = Some(format!("Error originates from self function call at {}.", diag_origin));
                });
            }

            self.monomorph_registry.update(monomorph_id, |_monomorph_instance| {
                _monomorph_instance.analyzed = true;
                _monomorph_instance.body = Some(monomorph_body_id);

                // ensure unique var_decl_id remapping per monomorph instance
                _monomorph_instance.params = func_decl.params.clone();
            });
        }

        func_call.dispatch = TypedFuncCallDispatch::Monomorph { monomorph_id };

        Some(func_decl.ret_type.clone())
    }
}

impl<'a> AnalysisContext<'a> {
    /// Specialize a function body's local declarations for a monomorph.
    ///
    /// Clones the template body, creates fresh `VarDecl`s for all locals,
    /// builds a mapping from old to new `VarDeclID`s, and rewrites all
    /// references in the cloned body.
    ///
    /// Does not modify the template body.
    pub(crate) fn specialize_func_body(
        &self,
        template_body: &TypedBlockStmt,
        params: &mut TypedFuncParams,
    ) -> TypedBlockStmt {
        let mut specialize_body = template_body.clone();
        let mut decl_map = FxHashMap::default();

        // assign fresh decls for parameters (first-class locals)
        self.collect_and_instantiate_and_specialize_func_params(params, &mut decl_map);

        // first pass: create fresh vars and fill decl_map
        for stmt in &mut specialize_body.stmts {
            self.collect_and_instantiate_fresh_var_decls(stmt, &mut decl_map);
        }

        for defer in &mut specialize_body.defers {
            self.collect_and_instantiate_fresh_var_decls(&mut defer.operand, &mut decl_map);
        }

        // second pass: rewrite refs using decl_map
        self.specialize_var_decls_in_body(&mut specialize_body, &decl_map);

        specialize_body
    }

    fn collect_and_instantiate_and_specialize_func_params(&self, params: &mut TypedFuncParams, decl_map: &mut DeclMap) {
        for param_kind in &mut params.list {
            match param_kind {
                TypedFuncParamKind::FuncParam(func_param) => {
                    if let Some(var_decl_id) = func_param.var_decl_id {
                        let mut var_decl = self.decl_tables.var_decl(var_decl_id);

                        var_decl.ty = Some(func_param.ty.clone());

                        self.instantiate_fresh_var_decl(var_decl_id, &var_decl, decl_map);

                        let new_var_decl_id = decl_map.get(&var_decl_id).copied().unwrap();

                        func_param.var_decl_id = Some(new_var_decl_id);
                    }
                }
                TypedFuncParamKind::SelfModifier(self_modifier) => {
                    if let Some(var_decl_id) = self_modifier.var_decl_id {
                        let mut var_decl = self.decl_tables.var_decl(var_decl_id);

                        var_decl.ty = Some(self_modifier.ty.clone());

                        self.instantiate_fresh_var_decl(var_decl_id, &var_decl, decl_map);

                        let new_var_decl_id = decl_map.get(&var_decl_id).copied().unwrap();

                        self_modifier.var_decl_id = Some(new_var_decl_id);
                    }
                }
            }
        }
    }

    /// Create a fresh `VarDecl` for a `var` statement and update its
    /// `VarDeclID` using the specialization mapping.
    ///
    /// Only used during the declaration-collection pass.
    fn instantiate_fresh_var_stmt(&self, var_stmt: &TypedVarStmt, decl_map: &mut DeclMap) {
        let var_decl = VarDecl {
            name: var_stmt.name.clone(),
            ty: var_stmt.ty.clone(),
            rhs: var_stmt.rhs.clone(),
            is_const: var_stmt.is_const,
            analyzed: false,
            loc: var_stmt.loc,
        };

        self.instantiate_fresh_var_decl(var_stmt.var_decl_id, &var_decl, decl_map);
    }

    /// Instantiate a fresh `VarDecl` for a template-local variable.
    ///
    /// Reuses an existing mapping if this variable was already instantiated.
    /// Ensures each template-local variable produces exactly one new
    /// `VarDeclID` per monomorph.
    #[inline]
    fn instantiate_fresh_var_decl(&self, var_decl_id: VarDeclID, template_var_decl: &VarDecl, decl_map: &mut DeclMap) {
        // already instantiated earlier in traversal
        if decl_map.get(&var_decl_id).is_some() {
            return;
        }

        let mut var_decl = template_var_decl.clone();

        if var_decl.ty.is_some() {
            var_decl.ty = Some(self.substitute_type(&var_decl.ty.unwrap()));

            if let Some(infer) = &self.func_env.infer {
                var_decl.ty = Some(infer.resolve(&var_decl.ty.unwrap()));
            }
        }

        let new_var_decl_id = self.decl_tables.insert_var(template_var_decl.clone());

        let old_var_decl_id = var_decl_id;

        decl_map.insert(old_var_decl_id, new_var_decl_id);
    }
}

impl<'a> AnalysisContext<'a> {
    fn specialize_var_decls_in_body(&self, body: &mut TypedBlockStmt, decl_map: &DeclMap) {
        for stmt in &mut body.stmts {
            self.specialize_stmt(stmt, decl_map);
        }
    }

    fn specialize_var_stmt(&self, var_stmt: &mut TypedVarStmt, decl_map: &DeclMap) {
        var_stmt.var_decl_id = decl_map.get(&var_stmt.var_decl_id).copied().unwrap();

        if let Some(init) = &mut var_stmt.rhs {
            self.specialize_expr(init, decl_map);
        }
    }

    fn specialize_if_stmt(&self, if_stmt: &mut TypedIfStmt, decl_map: &DeclMap) {
        self.specialize_expr(&mut if_stmt.cond, decl_map);

        self.specialize_block_stmt(&mut if_stmt.then_block, decl_map);

        if let Some(else_block) = &mut if_stmt.else_block {
            self.specialize_block_stmt(else_block, decl_map);
        }

        for branch in &mut if_stmt.branches {
            self.specialize_if_stmt(branch, decl_map);
        }
    }

    fn specialize_block_stmt(&self, block_stmt: &mut TypedBlockStmt, decl_map: &DeclMap) {
        for stmt in &mut block_stmt.stmts {
            self.specialize_stmt(stmt, decl_map);
        }

        for defer in &mut block_stmt.defers {
            self.specialize_stmt(&mut defer.operand, decl_map);
        }
    }

    fn specialize_stmt(&self, stmt: &mut TypedStmt, decl_map: &DeclMap) {
        match stmt {
            TypedStmt::Expr(expr) => {
                self.specialize_expr(expr, decl_map);
            }
            TypedStmt::Variable(var_stmt) => {
                self.specialize_var_stmt(var_stmt, decl_map);
            }
            TypedStmt::TupleExport(tuple_export) => {
                self.specialize_tuple_export_pattern(&mut tuple_export.pattern, decl_map);

                if let Some(expr) = &mut tuple_export.rhs {
                    self.specialize_expr(expr, decl_map);
                }
            }

            TypedStmt::BlockStmt(block_stmt) => {
                self.specialize_block_stmt(block_stmt, decl_map);
            }
            TypedStmt::If(if_stmt) => {
                self.specialize_if_stmt(if_stmt, decl_map);
            }
            TypedStmt::For(for_stmt) => {
                if let Some(var_stmt) = &mut for_stmt.initializer {
                    self.specialize_var_stmt(var_stmt, decl_map);
                }

                if let Some(expr) = &mut for_stmt.cond {
                    self.specialize_expr(expr, decl_map);
                }

                if let Some(increment) = &mut for_stmt.increment {
                    self.specialize_expr(increment, decl_map);
                }

                self.specialize_block_stmt(&mut for_stmt.body, decl_map);
            }
            TypedStmt::While(while_stmt) => {
                self.specialize_expr(&mut while_stmt.cond, decl_map);

                self.specialize_block_stmt(&mut while_stmt.body, decl_map);
            }
            TypedStmt::Switch(switch_stmt) => {
                self.specialize_expr(&mut switch_stmt.operand, decl_map);

                for case in &mut switch_stmt.cases {
                    self.specialize_switch_patterns(&mut case.patterns, decl_map);

                    self.specialize_block_stmt(&mut case.body, decl_map);
                }
            }
            TypedStmt::Return(return_stmt) => {
                if let Some(arg) = &mut return_stmt.arg {
                    self.specialize_expr(arg, decl_map);
                }
            }

            TypedStmt::Continue(_) | TypedStmt::Break(_) => {}

            // TODO: Recursively remap if it defines a compound block..
            TypedStmt::Builtin(_) => todo!(),

            TypedStmt::FuncDef(_)
            | TypedStmt::FuncDecl(_)
            | TypedStmt::Typedef(_)
            | TypedStmt::GlobalVar(_)
            | TypedStmt::Struct(_)
            | TypedStmt::Enum(_)
            | TypedStmt::Union(_)
            | TypedStmt::Interface(_)
            | TypedStmt::Defer(_)
            | TypedStmt::Label(_)
            | TypedStmt::Goto(_) => {}
        }
    }

    fn specialize_switch_pattern(&self, pattern: &mut TypedSwitchCasePattern, decl_map: &DeclMap) {
        match &mut pattern.kind {
            TypedSwitchCasePatternKind::Wildcard | TypedSwitchCasePatternKind::EnumUnit(_) => {
                // nothing to specialize
            }

            TypedSwitchCasePatternKind::Binding { var_decl_id, .. } => {
                let new_var_decl_id = decl_map.get(var_decl_id).copied().unwrap();

                *var_decl_id = new_var_decl_id
            }

            TypedSwitchCasePatternKind::Range(range) => {
                self.specialize_expr(&mut range.lower, decl_map);
                self.specialize_expr(&mut range.upper, decl_map);
            }

            TypedSwitchCasePatternKind::Expr(expr) => {
                self.specialize_expr(expr, decl_map);
            }

            TypedSwitchCasePatternKind::EnumTupleVariant { items, .. } => {
                for item in items {
                    self.specialize_switch_pattern(item, decl_map);
                }
            }

            TypedSwitchCasePatternKind::EnumStructVariant { items, .. } => {
                for item in items {
                    self.specialize_switch_pattern(&mut item.pattern, decl_map);
                }
            }
        }
    }

    fn specialize_switch_patterns(&self, patterns: &mut Vec<TypedSwitchCasePattern>, decl_map: &DeclMap) {
        for pattern in patterns {
            self.specialize_switch_pattern(pattern, decl_map);
        }
    }

    fn specialize_expr(&self, expr: &mut TypedExpr, decl_map: &DeclMap) {
        match &mut expr.kind {
            TypedExprKind::Symbol(symbol_expr) => {
                let decl_id = self.analyze_symbol_expr(symbol_expr).unwrap();

                if let Some(var_decl_id) = decl_id.as_var() {
                    let new_decl_id = DeclID::Var(decl_map.get(&var_decl_id).copied().unwrap());

                    *symbol_expr = TypedSymbolExpr::Resolved {
                        decl_id: new_decl_id,
                        loc: symbol_expr.loc(),
                    };
                }
            }

            TypedExprKind::Prefix(prefix) => {
                self.specialize_expr(&mut prefix.operand, decl_map);
            }
            TypedExprKind::Infix(infix) => {
                self.specialize_expr(&mut infix.lhs, decl_map);
                self.specialize_expr(&mut infix.rhs, decl_map);
            }
            TypedExprKind::Unary(unary) => {
                self.specialize_expr(&mut unary.operand, decl_map);
            }
            TypedExprKind::Assign(assign) => {
                self.specialize_expr(&mut assign.lhs, decl_map);
                self.specialize_expr(&mut assign.rhs, decl_map);
            }
            TypedExprKind::AddrOf(addr_of) => {
                self.specialize_expr(&mut addr_of.operand, decl_map);
            }
            TypedExprKind::Deref(deref) => {
                self.specialize_expr(&mut deref.operand, decl_map);
            }
            TypedExprKind::Array(array) => {
                for element in &mut array.elements {
                    self.specialize_expr(element, decl_map);
                }
            }
            TypedExprKind::ArrayIndex(array_index) => {
                self.specialize_expr(&mut array_index.index, decl_map);
                self.specialize_expr(&mut array_index.operand, decl_map);
            }
            TypedExprKind::UnnamedStructValue(struct_value) => {
                for field in &mut struct_value.fields {
                    self.specialize_expr(&mut field.value, decl_map);
                }
            }
            TypedExprKind::UnnamedUnionValue(union_value) => {
                debug_assert!(union_value.fields.len() == 1);

                let field = union_value.fields.first_mut().unwrap();

                self.specialize_expr(&mut field.value, decl_map);
            }
            TypedExprKind::UnnamedEnumValue(enum_value) => match &mut enum_value.kind {
                TypedUnnamedEnumValueKind::Unit => {}
                TypedUnnamedEnumValueKind::Tuple(elements) => {
                    for element in elements {
                        self.specialize_expr(element, decl_map);
                    }
                }
                TypedUnnamedEnumValueKind::Struct(field_inits) => {
                    for field_init in field_inits {
                        self.specialize_expr(&mut field_init.value, decl_map);
                    }
                }
            },
            TypedExprKind::EnumStructVariantInit(struct_variant_init) => {
                self.specialize_expr(&mut struct_variant_init.operand, decl_map);

                for field_init in &mut struct_variant_init.field_inits {
                    self.specialize_expr(&mut field_init.value, decl_map);
                }
            }
            TypedExprKind::EnumInit(enum_init) => match &mut enum_init.arg {
                TypedEnumInitArgs::Unit => todo!(),
                TypedEnumInitArgs::Tuple(elements) => {
                    for element in elements {
                        self.specialize_expr(element, decl_map);
                    }
                }
                TypedEnumInitArgs::Struct(field_inits) => {
                    for field_init in field_inits {
                        self.specialize_expr(&mut field_init.value, decl_map);
                    }
                }
            },
            TypedExprKind::StructInit(struct_init) => {
                for field in &mut struct_init.fields {
                    self.specialize_expr(&mut field.value, decl_map);
                }
            }
            TypedExprKind::UnionInit(union_init) => {
                self.specialize_expr(&mut union_init.field.value, decl_map);
            }
            TypedExprKind::FuncCall(func_call) => {
                self.specialize_expr(&mut func_call.operand, decl_map);

                for arg in &mut func_call.args {
                    self.specialize_expr(arg, decl_map);
                }
            }
            TypedExprKind::MethodCall(method_call) => {
                self.specialize_expr(&mut method_call.operand, decl_map);

                for arg in &mut method_call.args {
                    self.specialize_expr(arg, decl_map);
                }
            }
            TypedExprKind::FieldAccess(field_access) => {
                self.specialize_expr(&mut field_access.operand, decl_map);
            }
            TypedExprKind::Lambda(lambda) => {
                for param_kind in &mut lambda.params.list {
                    match param_kind {
                        TypedFuncParamKind::FuncParam(func_param) => {
                            if let Some(var_decl_id) = func_param.var_decl_id {
                                func_param.var_decl_id = Some(decl_map.get(&var_decl_id).copied().unwrap());
                            }
                        }
                        TypedFuncParamKind::SelfModifier(self_modifier) => {
                            if let Some(var_decl_id) = self_modifier.var_decl_id {
                                self_modifier.var_decl_id = Some(decl_map.get(&var_decl_id).copied().unwrap());
                            }
                        }
                    }
                }

                if let Some(variadic) = &mut lambda.params.variadic {
                    match variadic {
                        TypedFuncVariadicParam::UntypedCStyle => {}
                        TypedFuncVariadicParam::Typed { var_decl_id, .. } => {
                            *var_decl_id = decl_map.get(&var_decl_id).copied().unwrap();
                        }
                    }
                }

                self.specialize_block_stmt(&mut lambda.body, decl_map);
            }
            TypedExprKind::Tuple(tuple) => {
                for element in &mut tuple.elements {
                    self.specialize_expr(element, decl_map);
                }
            }
            TypedExprKind::TupleAccess(tuple_access) => {
                self.specialize_expr(&mut tuple_access.operand, decl_map);
            }
            TypedExprKind::Dynamic(dynamic) => {
                self.specialize_expr(&mut dynamic.operand, decl_map);
            }

            TypedExprKind::Builtin(builtin) => match builtin {
                TypedBuiltin::BuiltinFunc(builtin_func) => {
                    for arg in &mut builtin_func.args {
                        self.specialize_expr(arg, decl_map);
                    }
                }
                TypedBuiltin::BuiltinBlock(builtin_block) => {
                    for arg in &mut builtin_block.args {
                        self.specialize_expr(arg, decl_map);
                    }

                    self.specialize_block_stmt(&mut builtin_block.block, decl_map);
                }
            },

            TypedExprKind::Poisoned | TypedExprKind::Literal(_) | TypedExprKind::SemaType { .. } => {}
        }
    }

    fn specialize_tuple_export_pattern(&self, pattern: &mut TypedTupleExportPattern, decl_map: &DeclMap) {
        match &mut pattern.kind {
            TypedTupleExportPatternKind::Ident(var_decl_id) => {
                *var_decl_id = decl_map.get(&var_decl_id).copied().unwrap();
            }
            TypedTupleExportPatternKind::Tuple(patterns) => {
                for pattern in patterns {
                    self.specialize_tuple_export_pattern(pattern, decl_map);
                }
            }
            TypedTupleExportPatternKind::Ignore => {}
        }
    }
}

impl<'a> AnalysisContext<'a> {
    fn collect_and_instantiate_fresh_var_decls(&self, stmt: &TypedStmt, decl_map: &mut DeclMap) {
        match stmt {
            TypedStmt::Variable(var_stmt) => {
                self.instantiate_fresh_var_stmt(var_stmt, decl_map);
            }
            TypedStmt::BlockStmt(block) => {
                for stmt in &block.stmts {
                    self.collect_and_instantiate_fresh_var_decls(stmt, decl_map);
                }

                for defer in &block.defers {
                    self.collect_and_instantiate_fresh_var_decls(&defer.operand, decl_map);
                }
            }
            TypedStmt::For(for_stmt) => {
                if let Some(initializer) = &for_stmt.initializer {
                    self.instantiate_fresh_var_stmt(initializer, decl_map);
                }

                for stmt in &for_stmt.body.stmts {
                    self.collect_and_instantiate_fresh_var_decls(stmt, decl_map);
                }
            }
            TypedStmt::While(while_stmt) => {
                for stmt in &while_stmt.body.stmts {
                    self.collect_and_instantiate_fresh_var_decls(stmt, decl_map);
                }
            }
            TypedStmt::Switch(switch_stmt) => {
                for case in &switch_stmt.cases {
                    for pattern in &case.patterns {
                        self.collect_and_instantiate_fresh_var_decls_of_switch_pattern(pattern, decl_map);
                    }

                    for stmt in &case.body.stmts {
                        self.collect_and_instantiate_fresh_var_decls(stmt, decl_map);
                    }

                    for defer in &case.body.defers {
                        self.collect_and_instantiate_fresh_var_decls(&defer.operand, decl_map);
                    }
                }
            }
            TypedStmt::TupleExport(tuple_export) => {
                self.collect_and_instantiate_fresh_var_decls_of_tuple_pattern(&tuple_export.pattern, decl_map);
            }
            TypedStmt::Expr(expr) => match &expr.kind {
                TypedExprKind::Lambda(lambda) => {
                    for param_kind in &lambda.params.list {
                        match param_kind {
                            TypedFuncParamKind::FuncParam(func_param) => {
                                if let Some(var_decl_id) = func_param.var_decl_id {
                                    let var_decl = self.decl_tables.var_decl(var_decl_id);

                                    self.instantiate_fresh_var_decl(var_decl_id, &var_decl, decl_map);
                                }
                            }
                            TypedFuncParamKind::SelfModifier(self_modifier) => {
                                if let Some(var_decl_id) = self_modifier.var_decl_id {
                                    let var_decl = self.decl_tables.var_decl(var_decl_id);

                                    self.instantiate_fresh_var_decl(var_decl_id, &var_decl, decl_map);
                                }
                            }
                        }
                    }

                    if let Some(variadic) = &lambda.params.variadic {
                        match variadic {
                            TypedFuncVariadicParam::UntypedCStyle => {}
                            TypedFuncVariadicParam::Typed { var_decl_id, .. } => {
                                let var_decl = self.decl_tables.var_decl(*var_decl_id);

                                self.instantiate_fresh_var_decl(*var_decl_id, &var_decl, decl_map);
                            }
                        }
                    }
                }

                _ => {}
            },

            // TODO: Collect recursively if it defines a compound block..
            TypedStmt::Builtin(_builtin) => todo!(),

            _ => {}
        }
    }

    fn collect_and_instantiate_fresh_var_decls_of_tuple_pattern(
        &self,
        pattern: &TypedTupleExportPattern,
        decl_map: &mut DeclMap,
    ) {
        match &pattern.kind {
            TypedTupleExportPatternKind::Ident(var_decl_id) => {
                let var_decl = self.decl_tables.var_decl(*var_decl_id);

                self.instantiate_fresh_var_decl(*var_decl_id, &var_decl, decl_map);
            }
            TypedTupleExportPatternKind::Tuple(patterns) => {
                for pattern in patterns {
                    self.collect_and_instantiate_fresh_var_decls_of_tuple_pattern(pattern, decl_map);
                }
            }
            TypedTupleExportPatternKind::Ignore => {}
        }
    }

    fn collect_and_instantiate_fresh_var_decls_of_switch_pattern(
        &self,
        pattern: &TypedSwitchCasePattern,
        decl_map: &mut DeclMap,
    ) {
        match &pattern.kind {
            TypedSwitchCasePatternKind::Wildcard
            | TypedSwitchCasePatternKind::Range(_)
            | TypedSwitchCasePatternKind::Expr(_)
            | TypedSwitchCasePatternKind::EnumUnit(_) => {
                // nothing to do
            }

            TypedSwitchCasePatternKind::Binding { var_decl_id, .. } => {
                let var_decl = self.decl_tables.var_decl(*var_decl_id);

                self.instantiate_fresh_var_decl(*var_decl_id, &var_decl, decl_map);
            }

            TypedSwitchCasePatternKind::EnumTupleVariant { items, .. } => {
                for item in items {
                    self.collect_and_instantiate_fresh_var_decls_of_switch_pattern(item, decl_map);
                }
            }

            TypedSwitchCasePatternKind::EnumStructVariant { items, .. } => {
                for item in items {
                    self.collect_and_instantiate_fresh_var_decls_of_switch_pattern(&item.pattern, decl_map);
                }
            }
        }
    }
}
