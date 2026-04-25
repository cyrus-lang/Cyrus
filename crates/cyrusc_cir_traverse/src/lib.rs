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

use cyrusc_ast::abi::CallConv;
use cyrusc_ast::modifiers::GlobalVarModifiers;
use cyrusc_internal::abi::mangler::*;
use cyrusc_internal::abi::target::ABITarget;
use cyrusc_internal::cir::cir::*;
use cyrusc_internal::cir::types::*;
use cyrusc_internal::monomorph::*;
use cyrusc_internal::symbols::SymbolQuery;
use cyrusc_internal::vtable::VTableRegistry;
use cyrusc_source_loc::FileID;
use cyrusc_source_loc::Loc;
use cyrusc_source_loc::SourceMap;
use cyrusc_tokens::literals::*;
use cyrusc_typed_ast::TypedProgramTree;
use cyrusc_typed_ast::VTableID;
use cyrusc_typed_ast::decls::table::DeclTablesRegistry;
use cyrusc_typed_ast::decls::*;
use cyrusc_typed_ast::exprs::*;
use cyrusc_typed_ast::format::Formatter;
use cyrusc_typed_ast::format::*;
use cyrusc_typed_ast::stmts::*;
use cyrusc_typed_ast::substitute::*;
use cyrusc_typed_ast::types::*;
use fx_hash::FxHashMap;
use std::sync::Arc;

struct CIRTraverse<'a> {
    program_tree: Box<TypedProgramTree>,
    decl_tables: Arc<DeclTablesRegistry>,
    formatter: &'a dyn Formatter,
    target: &'a ABITarget,
    module_name: String,

    vtable_registry: Arc<VTableRegistry>,
    monomorph_registry: Arc<MonomorphRegistry>,

    next_irv_id: IRValueID,
    decl_to_ir_value_map: FxHashMap<DeclID, IRValueID>,
    monomorph_to_ir_value_map: FxHashMap<MonomorphID, IRValueID>,
    vtable_to_ir_value_map: FxHashMap<VTableID, IRValueID>,
    func_decls: FxHashMap<IRValueID, CIRFuncDeclStmt>,
    global_var_decls: FxHashMap<IRValueID, CIRGlobalVarStmt>,
}

impl<'a> CIRTraverse<'a> {
    pub fn new(
        program_tree: Box<TypedProgramTree>,
        module_name: String,
        decl_tables: Arc<DeclTablesRegistry>,
        formatter: &'a dyn Formatter,
        vtable_registry: Arc<VTableRegistry>,
        monomorph_registry: Arc<MonomorphRegistry>,
        target: &'a ABITarget,
    ) -> Self {
        Self {
            program_tree,
            module_name,
            decl_tables,
            formatter,
            vtable_registry,
            monomorph_registry,
            target,
            next_irv_id: IRValueID(0),
            decl_to_ir_value_map: FxHashMap::default(),
            global_var_decls: FxHashMap::default(),
            func_decls: FxHashMap::default(),
            monomorph_to_ir_value_map: FxHashMap::default(),
            vtable_to_ir_value_map: FxHashMap::default(),
        }
    }

    pub fn lower(&mut self, file_path: String) -> CIRModule {
        let stmts = std::mem::take(&mut self.program_tree.body);

        let lowered_stmts = self.lower_stmts(&stmts);

        let global_var_decls = std::mem::take(&mut self.global_var_decls);
        let func_decls = std::mem::take(&mut self.func_decls);

        CIRModule {
            stmts: lowered_stmts,
            file_path,
            module_name: self.module_name.clone(),
            global_var_decls,
            func_decls,
            vtable_registry: self.vtable_registry.clone(),
            vtable_to_ir_value_map: self.vtable_to_ir_value_map.clone(),
            monomorph_to_ir_value_map: self.monomorph_to_ir_value_map.clone(),
        }
    }

    fn lower_stmt(&mut self, stmt: &TypedStmt, lowered_stmts: &mut Vec<CIRStmt>) {
        match stmt {
            TypedStmt::FuncDef(func_def) => {
                if func_def.is_generic() {
                    lowered_stmts.extend(self.lower_monomorphized_func_def(func_def.func_decl_id.unwrap()));
                } else {
                    let irv_id = self.new_ir_value_id();

                    lowered_stmts.push(CIRStmt::FuncDef(self.lower_func_def(irv_id, func_def, true)))
                }
            }
            TypedStmt::FuncDecl(func_decl) => {
                lowered_stmts.push(CIRStmt::FuncDecl(self.lower_func_decl_stmt(func_decl, true)));
            }
            TypedStmt::Switch(switch_stmt) => {
                lowered_stmts.push(self.lower_switch(switch_stmt));
            }
            TypedStmt::Variable(var) => {
                lowered_stmts.push(CIRStmt::Variable(self.lower_var(var)));
            }
            TypedStmt::GlobalVar(global_var) => {
                lowered_stmts.push(self.lower_global_var_stmt(&mut global_var.clone()));
            }
            TypedStmt::BlockStmt(block) => {
                lowered_stmts.push(CIRStmt::Block(self.lower_body(block)));
            }
            TypedStmt::If(if_stmt) => {
                lowered_stmts.push(self.lower_if(if_stmt));
            }
            TypedStmt::Return(return_stmt) => {
                lowered_stmts.push(self.lower_return(return_stmt));
            }
            TypedStmt::Break(break_stmt) => {
                lowered_stmts.push(self.lower_break(break_stmt));
            }
            TypedStmt::Continue(continue_stmt) => {
                lowered_stmts.push(self.lower_continue(continue_stmt));
            }
            TypedStmt::For(for_stmt) => {
                lowered_stmts.push(self.lower_for(for_stmt));
            }
            TypedStmt::While(while_stmt) => {
                lowered_stmts.push(self.lower_while(while_stmt));
            }
            TypedStmt::TupleExport(export_tuple_stmt) => {
                self.lower_export_tuple_to_vars(export_tuple_stmt)
                    .iter()
                    .for_each(|var| {
                        lowered_stmts.push(CIRStmt::Variable(var.clone()));
                    });
            }
            TypedStmt::Label(label) => {
                lowered_stmts.push(self.lower_label(label));
            }
            TypedStmt::Goto(goto) => {
                lowered_stmts.push(self.lower_goto(goto));
            }
            TypedStmt::Expr(expr) => {
                if expr.is_rvalue() {
                    lowered_stmts.push(CIRStmt::Expr(self.lower_expr(expr)));
                } else {
                    // ignore
                }
            }
            TypedStmt::Struct(struct_stmt) => {
                if !struct_stmt.is_generic() {
                    let stmts = self.lower_non_generic_methods(&struct_stmt.name, &struct_stmt.methods);
                    lowered_stmts.extend(stmts);
                } else {
                    self.lower_generic_methods(&struct_stmt.methods, lowered_stmts);
                }
            }
            TypedStmt::Enum(enum_stmt) => {
                if !enum_stmt.is_generic() {
                    let stmts = self.lower_non_generic_methods(&enum_stmt.name, &enum_stmt.methods);
                    lowered_stmts.extend(stmts);
                } else {
                    self.lower_generic_methods(&enum_stmt.methods, lowered_stmts);
                }
            }
            TypedStmt::Union(union_stmt) => {
                if !union_stmt.is_generic() {
                    let stmts = self.lower_non_generic_methods(&union_stmt.name, &union_stmt.methods);
                    lowered_stmts.extend(stmts);
                } else {
                    self.lower_generic_methods(&union_stmt.methods, lowered_stmts);
                }
            }
            TypedStmt::Defer(_) | TypedStmt::Interface(..) | TypedStmt::Typedef(..) => {}

            // TODO
            TypedStmt::Builtin(_builtin) => todo!(),
        }
    }

    fn lower_stmts(&mut self, stmts: &Vec<TypedStmt>) -> Vec<CIRStmt> {
        let mut lowered_stmts = Vec::new();

        for stmt in stmts {
            self.lower_stmt(stmt, &mut lowered_stmts);
        }

        lowered_stmts
    }

    fn lower_defer(&mut self, defer: &TypedDeferStmt) -> CIRStmt {
        let mut lowered_stmts = Vec::new();
        self.lower_stmt(&defer.operand, &mut lowered_stmts);
        assert_eq!(lowered_stmts.len(), 1);

        let operand = lowered_stmts.first().unwrap();
        operand.clone()
    }

    fn lower_export_tuple_to_vars(&mut self, export_tuple: &TypedTupleExportStmt) -> Vec<CIRVarStmt> {
        let mut vars = Vec::new();

        let tuple_expr = self.lower_expr(&export_tuple.rhs.as_ref().unwrap());

        // temp to hold RHS
        let expr_var = self.new_ir_value_id();
        let ty = tuple_expr.ty.clone();

        vars.push(CIRVarStmt {
            irv_id: expr_var,
            name: "__tuple_expr".to_string(),
            ty,
            expr: Some(tuple_expr),
            loc: export_tuple.loc,
        });

        // start recursion (root has no index)
        self.lower_export_pattern_recursive(&export_tuple.pattern, expr_var, None, &mut vars);

        vars
    }

    fn lower_export_pattern_recursive(
        &mut self,
        pattern: &TypedTupleExportPattern,
        base_irv_id: IRValueID,
        index: Option<usize>,
        vars: &mut Vec<CIRVarStmt>,
    ) {
        match &pattern.kind {
            TypedTupleExportPatternKind::Ident(var_decl_id) => {
                let var_decl = self.decl_tables.var_decl(*var_decl_id);

                let var_name = var_decl.name.clone();
                let var_ty = self.lower_sema_type(&var_decl.ty.as_ref().unwrap());

                let irv_id = self.new_ir_value_id();

                self.decl_to_ir_value_map.insert(DeclID::Var(*var_decl_id), irv_id);

                let tuple_index = index.expect("tuple element must have index");

                let expr = CIRExpr {
                    kind: CIRExprKind::TupleAccess(CIRTupleAccessExpr {
                        operand: Box::new(CIRExpr {
                            kind: CIRExprKind::Load(CIRValue {
                                irv_id: base_irv_id,
                                kind: CIRValueKind::LocalVariable,
                            }),
                            ty: var_ty.clone(),
                            loc: var_decl.loc,
                        }),
                        index: tuple_index,
                    }),
                    ty: var_ty.clone(),
                    loc: var_decl.loc,
                };

                vars.push(CIRVarStmt {
                    irv_id,
                    name: var_name,
                    ty: var_ty,
                    expr: Some(expr),
                    loc: var_decl.loc,
                });
            }

            TypedTupleExportPatternKind::Tuple(children) => {
                for (i, child) in children.iter().enumerate() {
                    self.lower_export_pattern_recursive(child, base_irv_id, Some(i), vars);
                }
            }

            TypedTupleExportPatternKind::Ignore => {}
        }
    }

    fn lower_if(&mut self, if_stmt: &TypedIfStmt) -> CIRStmt {
        let cond = self.lower_expr(&if_stmt.cond);
        let then_block = Box::new(self.lower_body(&if_stmt.then_block));

        let mut else_block = if_stmt
            .else_block
            .as_ref()
            .map(|block| Box::new(self.lower_body(block)));

        for branch in if_stmt.branches.iter().rev() {
            let branch_cond = self.lower_expr(&branch.cond);
            let branch_then = Box::new(self.lower_body(&branch.then_block));

            let nested_if = CIRStmt::If(CIRIfStmt {
                cond: branch_cond,
                then_block: branch_then,
                else_block: else_block.take(),
                loc: branch.loc,
            });

            else_block = Some(Box::new(CIRBlockStmt {
                stmts: vec![nested_if],
                defers: Vec::new(),
                loc: branch.loc,
            }));
        }

        CIRStmt::If(CIRIfStmt {
            cond,
            then_block,
            else_block,
            loc: if_stmt.loc,
        })
    }

    // FIXME
    fn lower_switch(&mut self, switch_stmt: &TypedSwitchStmt) -> CIRStmt {
        todo!();
    }

    fn lower_while(&mut self, while_stmt: &TypedWhileStmt) -> CIRStmt {
        let cond = Box::new(self.lower_expr(&while_stmt.cond));
        let body = Box::new(self.lower_body(&while_stmt.body));
        CIRStmt::While(CIRWhileStmt {
            cond,
            body,
            loc: while_stmt.loc,
        })
    }

    fn lower_for(&mut self, for_stmt: &TypedForStmt) -> CIRStmt {
        let initializer = for_stmt.initializer.clone().and_then(|var| Some(self.lower_var(&var)));

        let cond = for_stmt.cond.clone().and_then(|cond| Some(self.lower_expr(&cond)));

        let increment = for_stmt
            .increment
            .clone()
            .and_then(|increment| Some(self.lower_expr(&increment)));

        let body = Box::new(self.lower_body(&for_stmt.body));

        CIRStmt::For(CIRForStmt {
            initializer,
            cond,
            increment,
            body,
            loc: for_stmt.loc,
        })
    }

    fn lower_break(&self, break_stmt: &TypedBreakStmt) -> CIRStmt {
        CIRStmt::Break(CIRBreakStmt { loc: break_stmt.loc })
    }

    fn lower_continue(&self, continue_stmt: &TypedContinueStmt) -> CIRStmt {
        CIRStmt::Continue(CIRContinueStmt { loc: continue_stmt.loc })
    }

    fn lower_return(&mut self, return_stmt: &TypedReturnStmt) -> CIRStmt {
        let arg = return_stmt.arg.clone().and_then(|arg| Some(self.lower_expr(&arg)));

        CIRStmt::Return(CIRReturnStmt {
            arg,
            loc: return_stmt.loc,
        })
    }

    fn lower_global_var_stmt(&mut self, global_var: &mut TypedGlobalVarStmt) -> CIRStmt {
        let ty = global_var
            .ty
            .as_ref()
            .or_else(|| global_var.expr.as_ref().and_then(|expr| expr.ty.as_ref()))
            .map(|sema_type| self.lower_sema_type(sema_type))
            .unwrap_or_else(|| {
                panic!(
                    "Global var '{}' has neither explicit type nor valid initializer type.",
                    global_var.name
                )
            });

        let expr = global_var.expr.clone().and_then(|expr| Some(self.lower_expr(&expr)));

        let mangled_name = mangle_global_var(&global_var.modifiers, &self.module_name, &global_var.name);

        let irv_id = self.new_ir_value_id();

        let global_var_stmt = CIRGlobalVarStmt {
            irv_id,
            name: mangled_name,
            ty,
            expr,
            modifiers: global_var.modifiers.clone(),
            loc: global_var.loc,
        };

        self.decl_to_ir_value_map
            .insert(DeclID::GlobalVar(global_var.global_var_decl_id), irv_id);

        self.global_var_decls.insert(irv_id, global_var_stmt.clone());

        CIRStmt::GlobalVar(global_var_stmt)
    }

    fn lower_global_var_decl(&mut self, irv_id: IRValueID, global_var_decl: &GlobalVarDecl) -> CIRGlobalVarStmt {
        let ty = global_var_decl.ty.as_ref().map(|ty| self.lower_sema_type(ty)).unwrap();

        let mangled_name = mangle_global_var(&global_var_decl.modifiers, &self.module_name, &global_var_decl.name);

        CIRGlobalVarStmt {
            irv_id,
            name: mangled_name,
            ty,
            expr: None,
            modifiers: global_var_decl.modifiers.clone(),
            loc: global_var_decl.loc,
        }
    }

    fn lower_var(&mut self, var: &TypedVarStmt) -> CIRVarStmt {
        let ty = var
            .ty
            .as_ref()
            .or_else(|| var.rhs.as_ref().and_then(|rhs| rhs.ty.as_ref()))
            .map(|ty| self.lower_sema_type(ty))
            .unwrap_or_else(|| {
                panic!(
                    "Variable '{}' has neither explicit type nor RHS type ({}:{})",
                    var.name, var.loc.line, var.loc.column
                )
            });

        let expr = var.rhs.clone().and_then(|expr| Some(self.lower_expr(&expr)));

        let irv_id = self.new_ir_value_id();

        self.decl_to_ir_value_map.insert(DeclID::Var(var.var_decl_id), irv_id);

        CIRVarStmt {
            irv_id,
            name: var.name.clone(),
            ty,
            expr,
            loc: var.loc,
        }
    }

    fn lower_func_type_params(&mut self, func_type_params: &TypedFuncTypeParams) -> Vec<CIRType> {
        func_type_params
            .list
            .iter()
            .map(|sema_type| self.lower_sema_type(sema_type))
            .collect()
    }

    fn lower_func_params(&mut self, func_params: &TypedFuncParams, is_decl: bool) -> CIRFuncParams {
        let mut cir_params = Vec::new();

        for param_kind in &func_params.list {
            let name = Some(param_kind.name());
            let loc = param_kind.loc();

            match param_kind {
                TypedFuncParamKind::FuncParam(func_param) => {
                    let irv_id = if !is_decl {
                        let irv_id = self.new_ir_value_id();

                        if let Some(var_decl_id) = func_param.var_decl_id {
                            self.decl_to_ir_value_map.insert(DeclID::Var(var_decl_id), irv_id);

                            Some(irv_id)
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    cir_params.push(CIRFuncParam {
                        name,
                        irv_id,
                        ty: self.lower_sema_type(&func_param.ty),
                        loc,
                    });
                }
                TypedFuncParamKind::SelfModifier(self_modifier) => {
                    let irv_id = if !is_decl {
                        let irv_id = self.new_ir_value_id();

                        if let Some(var_decl_id) = self_modifier.var_decl_id {
                            self.decl_to_ir_value_map.insert(DeclID::Var(var_decl_id), irv_id);

                            Some(irv_id)
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    cir_params.push(CIRFuncParam {
                        name,
                        irv_id,
                        ty: self.lower_sema_type(&self_modifier.ty),
                        loc,
                    });
                }
            }
        }

        CIRFuncParams {
            list: cir_params,
            is_var: func_params.variadic.is_some(),
        }
    }

    fn lower_monomorphized_func_def(&mut self, func_decl_id: FuncDeclID) -> Vec<CIRStmt> {
        let mut stmts = Vec::new();

        let func_decl = self.decl_tables.func_decl(func_decl_id);

        let monomorph_ids = self
            .monomorph_registry
            .get_func_monomorphs(MonomorphizableTemplateID::Func(func_decl_id));

        for monomorph_id in monomorph_ids {
            let monomorph_instance = self.monomorph_registry.get(monomorph_id);

            let body_id = monomorph_instance.body.unwrap();

            let body = self.monomorph_registry.get_monomorph_body(body_id).unwrap();

            let irv_id = self.get_or_declare_func_ir_value(func_decl_id);

            let mangled_name = mangle_monomorphized_func(
                &func_decl.modifiers,
                &self.module_name,
                &func_decl.name,
                &monomorph_instance.type_args,
            );

            let func_def_stmt = TypedFuncDefStmt {
                func_decl_id: Some(func_decl_id),
                name: mangled_name,
                generic_params: func_decl.generic_params.clone(),
                params: monomorph_instance.params.clone(),
                ret_type: monomorph_instance.ret_type.clone(),
                body: Box::new(body),
                modifiers: func_decl.modifiers.clone(),
                loc: func_decl.loc,
            };

            let cir_func_def = self.lower_func_def(irv_id, &func_def_stmt, false);
            let cir_func_decl = cir_func_def_as_decl(&cir_func_def);

            stmts.push(CIRStmt::FuncDef(cir_func_def));

            self.func_decls.insert(irv_id, cir_func_decl);

            self.monomorph_to_ir_value_map.insert(monomorph_id, irv_id);
        }

        stmts
    }

    fn lower_monomorphized_methods_def(&mut self, method_decl_id: MethodDeclID) -> Vec<CIRStmt> {
        let mut stmts = Vec::new();

        let method_decl = self.decl_tables.method_decl(method_decl_id);

        let monomorph_ids = self
            .monomorph_registry
            .get_func_monomorphs(MonomorphizableTemplateID::Method(method_decl_id));

        for monomorph_id in monomorph_ids {
            let monomorph_instance = self.monomorph_registry.get(monomorph_id);

            let body_id = monomorph_instance.body.unwrap();

            let body = self.monomorph_registry.get_monomorph_body(body_id).unwrap();

            let (irv_id, _, mangled_name) = self.get_or_declare_monomorph_method_ir_value(monomorph_id);

            let func_def_stmt = TypedFuncDefStmt {
                func_decl_id: None,
                name: mangled_name,
                generic_params: method_decl.func_decl.generic_params.clone(),
                params: monomorph_instance.params.clone(),
                ret_type: monomorph_instance.ret_type.clone(),
                body: Box::new(body),
                modifiers: method_decl.func_decl.modifiers.clone(),
                loc: method_decl.func_decl.loc,
            };

            let cir_func_def = self.lower_func_def(irv_id, &func_def_stmt, false);
            let cir_func_decl = cir_func_def_as_decl(&cir_func_def);

            stmts.push(CIRStmt::FuncDef(cir_func_def));

            self.func_decls.insert(irv_id, cir_func_decl);

            self.monomorph_to_ir_value_map.insert(monomorph_id, irv_id);
        }

        stmts
    }

    fn lower_func_def(&mut self, irv_id: IRValueID, func_def: &TypedFuncDefStmt, mangle_name: bool) -> CIRFuncDefStmt {
        let params = self.lower_func_params(&func_def.params, false);

        let body = self.lower_body(&func_def.body);

        let ret_type = self.lower_sema_type(&func_def.ret_type);

        if let Some(func_decl_id) = func_def.func_decl_id {
            self.decl_to_ir_value_map.insert(DeclID::Func(func_decl_id), irv_id);
        }

        let mut cir_func_def = CIRFuncDefStmt {
            irv_id,
            name: func_def.name.clone(),
            body: Box::new(body),
            params,
            ret_type,
            modifiers: func_def.modifiers.clone(),
            abi_func_info: None,
            loc: func_def.loc,
        };

        let cir_func_type = cir_func_decl_as_func_type(&cir_func_def_as_decl(&cir_func_def));
        cir_func_def.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());

        if mangle_name {
            let mangled_name = mangle_func(&cir_func_def.modifiers, &self.module_name, &cir_func_def.name);
            cir_func_def.name = mangled_name.clone();

            if let Some(func_decl_id) = func_def.func_decl_id {
                self.decl_tables.with_func_decl_mut(func_decl_id, |_func_decl| {
                    _func_decl.name = mangled_name.clone();
                });
            }
        }

        cir_func_def
    }

    fn lower_func_decl(
        &mut self,
        func_decl_id_opt: Option<FuncDeclID>,
        func_decl: &FuncDecl,
        mangle_name: bool,
    ) -> CIRFuncDeclStmt {
        let params = self.lower_func_params(&func_decl.params, true);

        let ret_type = self.lower_sema_type(&func_decl.ret_type);

        let func_name = if mangle_name {
            mangle_func(&func_decl.modifiers, &self.module_name, &func_decl.name)
        } else {
            func_decl.name.clone()
        };

        let irv_id = self.new_ir_value_id();

        if let Some(func_decl_id) = func_decl_id_opt {
            self.decl_to_ir_value_map.insert(DeclID::Func(func_decl_id), irv_id);

            self.decl_tables.with_func_decl_mut(func_decl_id, |_func_decl| {
                _func_decl.name = func_name.clone();
            });
        }

        let mut cir_func_decl = CIRFuncDeclStmt {
            irv_id,
            name: func_name,
            params,
            ret_type,
            modifiers: func_decl.modifiers.clone(),
            abi_func_info: None,
            loc: func_decl.loc,
        };

        let cir_func_type = cir_func_decl_as_func_type(&cir_func_decl);
        cir_func_decl.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());

        self.func_decls.insert(irv_id, cir_func_decl.clone());

        cir_func_decl
    }

    #[inline]
    fn lower_func_decl_stmt(&mut self, func_decl_stmt: &TypedFuncDeclStmt, mangle_name: bool) -> CIRFuncDeclStmt {
        let func_decl = func_decl_stmt.as_func_decl();

        self.lower_func_decl(Some(func_decl_stmt.func_decl_id), &func_decl, mangle_name)
    }

    fn lower_body(&mut self, block: &TypedBlockStmt) -> CIRBlockStmt {
        let stmts = self.lower_stmts(&block.stmts);
        let defers = block.defers.iter().map(|defer| self.lower_defer(defer)).collect();

        CIRBlockStmt {
            stmts,
            defers,
            loc: block.loc,
        }
    }

    fn lower_struct_init(&mut self, struct_init: &TypedStructInitExpr) -> CIRExprKind {
        let named_type = struct_init.operand.as_named_type().unwrap();
        let type_args = &named_type.type_args;
        let struct_decl_id = named_type.type_decl_id.as_struct().unwrap();

        let struct_decl = self.decl_tables.struct_decl(struct_decl_id);

        let inst_struct_decl = instantiate_struct_decl_with_type_args(&struct_decl, type_args);

        let struct_name = format_struct_decl(&inst_struct_decl, self.formatter);

        let fields = inst_struct_decl
            .fields
            .iter()
            .map(|field| self.lower_sema_type(&field.ty))
            .collect();

        let fields_info = inst_struct_decl
            .fields
            .iter()
            .map(|field| (field.name.clone(), field.loc))
            .collect();

        let struct_type = CIRStructType {
            name: Some(struct_name),
            fields,
            fields_info,
            align: inst_struct_decl.align.clone(),
            repr_attr: inst_struct_decl.modifiers.repr_attr.clone(),
            loc: inst_struct_decl.loc,
        };

        let mut lowered_fields = FxHashMap::default();

        for field_init in &struct_init.fields {
            let lowered = self.lower_expr(&field_init.value);
            lowered_fields.insert(field_init.name.clone(), lowered);
        }

        // emit exprs in declaration order
        let fields: Vec<CIRExpr> = inst_struct_decl
            .fields
            .iter()
            .map(|decl_field| lowered_fields.get(&decl_field.name).unwrap().clone())
            .collect();

        CIRExprKind::StructInit(CIRStructInitExpr {
            ty: struct_type,
            fields,
        })
    }

    fn lower_union_init(&mut self, union_init: &TypedUnionInitExpr) -> CIRExprKind {
        let named_type = union_init.operand.as_named_type().unwrap();
        let type_args = &named_type.type_args;
        let union_decl_id = named_type.type_decl_id.as_union().unwrap();

        let union_decl = self.decl_tables.union_decl(union_decl_id);

        let inst_union_decl = instantiate_union_decl_with_type_args(&union_decl, type_args);

        let union_name = format_union_decl(&inst_union_decl, self.formatter);

        let fields = inst_union_decl
            .fields
            .iter()
            .map(|field| self.lower_sema_type(&field.ty))
            .collect();

        let fields_info = inst_union_decl
            .fields
            .iter()
            .map(|field| (field.name.clone(), field.loc))
            .collect();

        let union_type = CIRUnionType {
            name: Some(union_name),
            fields,
            fields_info,
            repr_attr: inst_union_decl.modifiers.repr_attr.clone(),
            align: inst_union_decl.align.clone(),
            loc: inst_union_decl.loc,
        };

        let lowered_expr = Box::new(self.lower_expr(&union_init.field.value));

        CIRExprKind::UnionInit(CIRUnionInitExpr {
            ty: CIRType::Union(union_type),
            expr: lowered_expr,
        })
    }

    fn lower_enum_init(&mut self, enum_init: &TypedEnumInit) -> CIRExprKind {
        let named_type = enum_init.operand.as_named_type().unwrap();
        let type_args = &named_type.type_args;
        let enum_decl_id = named_type.type_decl_id.as_enum().unwrap();

        let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

        let inst_enum_decl = instantiate_enum_decl_with_type_args(&enum_decl, type_args);

        let variant = match &enum_init.arg {
            TypedEnumInitArgs::Unit => CIREnumInitVariant::Unit,
            TypedEnumInitArgs::Tuple(exprs) => {
                let lowered_exprs = exprs.iter().map(|expr| self.lower_expr(expr)).collect();

                CIREnumInitVariant::Payload(lowered_exprs)
            }
            TypedEnumInitArgs::Struct(field_inits) => {
                let lowered_exprs = field_inits
                    .iter()
                    .map(|field_init| self.lower_expr(&field_init.value))
                    .collect();

                CIREnumInitVariant::Payload(lowered_exprs)
            }
        };

        let enum_type = self.lower_enum_decl(&inst_enum_decl);

        let tag = enum_type.compute_variant_tag(&enum_init.name).unwrap() as usize;

        CIRExprKind::EnumInit(CIREnumInitExpr {
            tag,
            variant,
            enum_type,
        })
    }

    fn lower_tuple(&mut self, tuple: &TypedTupleExpr) -> CIRExprKind {
        let elements: Vec<CIRExpr> = tuple.elements.iter().map(|expr| self.lower_expr(expr)).collect();

        CIRExprKind::Tuple(CIRTupleExpr {
            elements,
            loc: tuple.loc,
        })
    }

    fn lower_lambda(&mut self, lambda: &TypedLambdaExpr) -> CIRExprKind {
        let params = self.lower_func_params(&lambda.params, false);
        let body = Box::new(self.lower_body(&lambda.body));
        let ret = self.lower_sema_type(&lambda.ret_type);

        let cir_func_type = CIRFuncType {
            params: params.list.iter().map(|param| param.ty.clone()).collect(),
            is_var: params.is_var,
            ret_type: Box::new(ret.clone()),
            callconv: CallConv::default(),
            abi_func_info: None,
        };

        let abi_func_info = self.target.target_abi.classify_func(&cir_func_type).unwrap();

        let irv_id = self.new_ir_value_id();

        CIRExprKind::Lambda(CIRLambda {
            irv_id,
            params,
            inline: lambda.inline,
            ret,
            body,
            abi_func_info,
            loc: lambda.loc,
        })
    }

    fn lower_generic_methods(&mut self, methods: &MethodDecls, lowered_stmts: &mut Vec<CIRStmt>) {
        for (_, &method_decl_id) in methods.iter() {
            let stmts = self.lower_monomorphized_methods_def(method_decl_id);

            lowered_stmts.extend(stmts);
        }
    }

    fn lower_non_generic_methods(&mut self, object_name: &String, methods: &MethodDecls) -> Vec<CIRStmt> {
        let mut stmts: Vec<CIRStmt> = Vec::new();

        for (_, &method_decl_id) in methods.iter() {
            let method_decl = self.decl_tables.method_decl(method_decl_id);

            let method_def = self.lower_method(method_decl_id, &method_decl, &object_name).unwrap();

            stmts.push(method_def);
        }

        stmts
    }

    fn lower_method(
        &mut self,
        method_decl_id: MethodDeclID,
        method_decl: &MethodDecl,
        object_name: &String,
    ) -> Option<CIRStmt> {
        let body_id = method_decl.body.unwrap();

        let params = self.lower_func_params(&method_decl.func_decl.params, false);

        let body = {
            // FIXME: Handle monomorphized body if method is generic.
            let method_body = self.decl_tables.body(body_id);

            self.lower_body(&method_body)
        };

        let ret_type = self.lower_sema_type(&method_decl.func_decl.ret_type);

        let irv_id = self.new_ir_value_id();

        self.decl_to_ir_value_map.insert(DeclID::Method(method_decl_id), irv_id);

        let mangled_name = CYRUS_ABI.method_name(&self.module_name, object_name, &method_decl.func_decl.name);

        self.decl_tables.with_method_decl_mut(method_decl_id, |_method_decl| {
            _method_decl.func_decl.name = mangled_name.clone();
        });

        let mut cir_func_def = CIRFuncDefStmt {
            irv_id,
            name: mangled_name,
            body: Box::new(body),
            params,
            ret_type,
            modifiers: method_decl.func_decl.modifiers.clone(),
            abi_func_info: None,
            loc: method_decl.func_decl.loc,
        };

        let cir_func_type = cir_func_decl_as_func_type(&cir_func_def_as_decl(&cir_func_def));
        cir_func_def.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());

        Some(CIRStmt::FuncDef(cir_func_def))
    }

    fn lower_load_symbol(&mut self, decl_id: DeclID) -> CIRExprKind {
        if let Some(func_decl_id) = decl_id.as_func() {
            let irv_id = self.get_or_declare_func_ir_value(func_decl_id);

            CIRExprKind::Load(CIRValue {
                irv_id,
                kind: CIRValueKind::Func,
            })
        } else if let Some(global_var_decl_id) = decl_id.as_global_var() {
            let irv_id = self.get_or_declare_global_var_ir_value(global_var_decl_id);

            CIRExprKind::Load(CIRValue {
                irv_id,
                kind: CIRValueKind::GlobalVar,
            })
        } else if let Some(var_decl_id) = decl_id.as_var() {
            let irv_id = self
                .decl_to_ir_value_map
                .get(&DeclID::Var(var_decl_id))
                .copied()
                .unwrap();

            CIRExprKind::Load(CIRValue {
                irv_id,
                kind: CIRValueKind::LocalVariable,
            })
        } else {
            unreachable!("unexpected symbol kind when lowering load symbol")
        }
    }

    fn lower_array_index(&mut self, array_index: &TypedArrayIndexExpr) -> CIRExprKind {
        CIRExprKind::ArrayIndex(CIRArrayIndexExpr {
            operand: Box::new(self.lower_expr(&array_index.operand)),
            index: Box::new(self.lower_expr(&array_index.index)),
        })
    }

    fn lower_expr(&mut self, expr: &TypedExpr) -> CIRExpr {
        if cfg!(debug_assertions) {
            if expr.ty.is_none() {
                dbg!(expr.clone());
            }
            debug_assert!(expr.ty.is_some());
        }

        let ty = self.lower_sema_type(&expr.ty.clone().unwrap());

        let kind = match &expr.kind {
            TypedExprKind::Symbol(symbol_expr) => self.lower_load_symbol(symbol_expr.decl_id),
            TypedExprKind::Literal(literal) => self.lower_literal(literal),
            TypedExprKind::Prefix(prefix) => self.lower_prefix(prefix),
            TypedExprKind::Infix(infix) => self.lower_infix(infix),
            TypedExprKind::Unary(unary) => self.lower_unary(unary),
            TypedExprKind::Assign(assign) => self.lower_assign(assign),
            TypedExprKind::AddrOf(addr_of) => self.lower_addr_of(addr_of),
            TypedExprKind::Deref(deref) => self.lower_deref(deref),
            TypedExprKind::Array(array) => self.lower_array(array),
            TypedExprKind::ArrayIndex(array_index) => self.lower_array_index(array_index),
            TypedExprKind::FuncCall(func_call) => self.lower_func_call(func_call),
            TypedExprKind::MethodCall(method_call) => self.lower_method_call(method_call),
            TypedExprKind::FieldAccess(field_access) => self.lower_field_access(field_access.clone()),
            TypedExprKind::Lambda(lambda) => self.lower_lambda(lambda),
            TypedExprKind::Tuple(expr) => self.lower_tuple(expr),
            TypedExprKind::TupleAccess(tuple_access) => self.lower_tuple_access(tuple_access),
            TypedExprKind::Dynamic(dynamic) => self.lower_dynamic(dynamic),
            TypedExprKind::StructInit(struct_init) => self.lower_struct_init(struct_init),
            TypedExprKind::UnionInit(union_init) => self.lower_union_init(union_init),
            TypedExprKind::EnumInit(enum_init) => self.lower_enum_init(enum_init),

            TypedExprKind::Builtin(_builtin) => todo!(),

            TypedExprKind::UnnamedStructValue(_)
            | TypedExprKind::UnnamedEnumValue(_)
            | TypedExprKind::UnnamedUnionValue(_) => unreachable!("unexpected unnamed constructor expression"),
            TypedExprKind::EnumStructVariantInit(_) => unreachable!("unexpected enum struct variant init expression"),
            TypedExprKind::SemaType(..) => unreachable!("unexpected semantic type as expression"),
            TypedExprKind::Poisoned => unreachable!("unexpected poisoned expression"),
        };

        CIRExpr {
            kind,
            ty,
            loc: expr.loc,
        }
    }

    fn lower_method_call(&mut self, method_call: &TypedMethodCall) -> CIRExprKind {
        let args: Vec<CIRExpr> = method_call.args.iter().map(|arg| self.lower_expr(arg)).collect();

        match &method_call.dispatch {
            TypedMethodCallDispatch::Normal { method_decl_id } => {
                let irv_id = self.get_or_declare_method_ir_value(*method_decl_id);

                let method_decl = self.decl_tables.method_decl(*method_decl_id);

                let self_meta = {
                    if method_decl.is_instance_method() {
                        let self_modifier = method_decl.func_decl.params.get_self_modifier().unwrap();
                        let operand = self.lower_expr(&method_call.operand);

                        Some(CIRCallMethodSelfMetadata {
                            operand: Box::new(operand),
                            is_referenced: self_modifier.kind.is_referenced(),
                        })
                    } else {
                        None
                    }
                };

                let ret_type = self.lower_sema_type(&method_decl.func_decl.ret_type);

                let func_type = self.lower_func_type(&method_decl.func_decl.as_func_type());

                CIRExprKind::Call(CIRCall {
                    args,
                    ret_type,
                    dispatch: CIRCallDispatch::Method {
                        irv_id,
                        func_type,
                        abi_name: method_decl.func_decl.name.clone(),
                        self_meta,
                    },
                })
            }
            TypedMethodCallDispatch::Interface {
                interface_decl_id,
                index,
                dispatch,
            } => {
                match dispatch {
                    TypedInterfaceCallDispatch::Dynamic => {
                        let operand = self.lower_expr(&method_call.operand);

                        let interface_decl = self.decl_tables.interface_decl(*interface_decl_id);
                        let interface_generic_params = &interface_decl.generic_params;

                        let method_decl_id = interface_decl.get_method(&method_call.name).unwrap();

                        let method_decl = self.decl_tables.method_decl(method_decl_id);

                        let mut inst_method_decl =
                            instantiate_method_decl(&method_decl, interface_generic_params, &method_call.type_args);

                        // substitute method self type
                        inst_method_decl
                            .func_decl
                            .params
                            .get_self_modifier_mut()
                            .as_mut()
                            .unwrap()
                            .ty = SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Void))); // void*

                        let cir_func_type = self.lower_func_type(&inst_method_decl.func_decl.as_func_type());

                        let ret_type = self.lower_sema_type(&inst_method_decl.func_decl.ret_type);

                        CIRExprKind::Call(CIRCall {
                            args,
                            ret_type,
                            dispatch: CIRCallDispatch::Interface {
                                operand: Box::new(operand),
                                index: *index,
                                func_type: cir_func_type,
                            },
                        })
                    }
                    TypedInterfaceCallDispatch::Static { vtable_id } => {
                        // let vtable_irv = self.get_or_declare_vtable_ir_value(&interface_decl.name, *vtable_id);
                        
                        // TODO
                        todo!()
                    }
                }
                // let concrete_type = method_self_type.const_inner().pointer_inner();

                // let named_type = &concrete_type.as_named_type().unwrap();

                // let object_methods = self.decl_tables.methods_decl_of_named_type(*named_type).unwrap();

                // let object_generic_params = self.decl_tables.type_decl_generic_params(named_type.type_decl_id);

                // let type_args = &named_type.type_args;
            }
            TypedMethodCallDispatch::Monomorph {
                monomorph_id,
                is_instance_method,
            } => {
                let (irv_id, cir_func_type, abi_name) = self.get_or_declare_monomorph_method_ir_value(*monomorph_id);

                let monomorph_instance = self.monomorph_registry.get(*monomorph_id);

                let self_meta = {
                    if *is_instance_method {
                        let self_modifier = monomorph_instance.params.get_self_modifier().unwrap();
                        let operand = self.lower_expr(&method_call.operand);

                        Some(CIRCallMethodSelfMetadata {
                            operand: Box::new(operand),
                            is_referenced: self_modifier.kind.is_referenced(),
                        })
                    } else {
                        None
                    }
                };

                CIRExprKind::Call(CIRCall {
                    args,
                    ret_type: *cir_func_type.ret_type.clone(),
                    dispatch: CIRCallDispatch::Method {
                        irv_id,
                        func_type: cir_func_type,
                        abi_name,
                        self_meta,
                    },
                })
            }

            TypedMethodCallDispatch::Unresolved => unreachable!(),
        }
    }

    fn lower_tuple_access(&mut self, tuple_access: &TypedTupleAccessExpr) -> CIRExprKind {
        let operand = Box::new(self.lower_expr(&tuple_access.operand));

        CIRExprKind::TupleAccess(CIRTupleAccessExpr {
            operand,
            index: tuple_access.index,
        })
    }

    fn lower_field_access(&mut self, mut field_access: TypedFieldAccess) -> CIRExprKind {
        if field_access.is_thin_arrow {
            field_access.operand = Box::new(TypedExpr {
                kind: TypedExprKind::Deref(TypedDerefExpr {
                    operand: field_access.operand.clone(),
                    loc: field_access.loc,
                }),
                ty: Some(field_access.operand.ty.clone().unwrap().pointer_inner().clone()),
                val_cat: ValueCategory::LValue,
                loc: field_access.loc,
            })
        }

        let operand = self.lower_expr(&field_access.operand);

        let field_type = self.lower_sema_type(&field_access.ty.unwrap());

        match &field_access.dispatch {
            TypedFieldAccessDispatch::Unresolved => unreachable!(),

            TypedFieldAccessDispatch::Struct { index, .. } => CIRExprKind::FieldAccess(CIRFieldAccessExpr {
                operand: Box::new(operand),
                kind: CIRFieldAccessKind::Struct {
                    index: *index,
                    field_type: field_type,
                },
            }),
            TypedFieldAccessDispatch::Union { .. } => CIRExprKind::FieldAccess(CIRFieldAccessExpr {
                operand: Box::new(operand),
                kind: CIRFieldAccessKind::Union { field_type },
            }),
        }
    }

    fn lower_dynamic(&mut self, dynamic: &TypedDynamicExpr) -> CIRExprKind {
        let operand_sema_type = dynamic.operand.ty.clone().unwrap();
        let operand = self.lower_expr(&dynamic.operand);

        let named_type = &dynamic.interface_object_type.as_ref().unwrap().interface_type;

        let interface_decl_id = named_type.type_decl_id.as_interface().unwrap();
        let interface_name = self.formatter.format_decl(DeclID::Interface(interface_decl_id));
        let interface_type_args = named_type.type_args.clone();

        let vtable_id = self
            .vtable_registry
            .get(&operand_sema_type, (interface_decl_id, interface_type_args));

        let vtable_abi_name = DEFAULT_ABI.vtable_name(&interface_name, &vtable_id.0.to_string());

        let vtable_irv_id = self.lower_vtable_global(vtable_id, &vtable_abi_name, dynamic.loc);

        CIRExprKind::Dynamic(CIRDynamicExpr {
            data_expr: Box::new(operand),
            vtable_id,
            vtable_irv_id,
            loc: dynamic.loc,
        })
    }

    fn lower_vtable_global(&mut self, vtable_id: VTableID, vtable_abi_name: &str, loc: Loc) -> IRValueID {
        if let Some(existing_irv_id) = self.vtable_to_ir_value_map.get(&vtable_id) {
            return *existing_irv_id;
        }

        let irv_id = self.new_ir_value_id();

        let vtable_type = cir_fat_ptr_type(loc);

        let cir_global = CIRGlobalVarStmt {
            irv_id,
            name: vtable_abi_name.to_string(),
            ty: vtable_type,
            expr: None, // no initializer
            modifiers: GlobalVarModifiers {
                linkage: None,
                ..Default::default()
            },
            loc,
        };

        self.global_var_decls.insert(irv_id, cir_global);

        self.vtable_to_ir_value_map.insert(vtable_id, irv_id);

        let vtable_info = self.vtable_registry.info(vtable_id);

        if vtable_info.cir_method_decls.is_none() {
            let lowered_methods = vtable_info
                .method_decls
                .iter()
                .enumerate()
                .map(|(idx, (_, method_decl_id))| {
                    if vtable_info.is_interface_generic {
                        if let Some(monomorph_id) = vtable_info.monomorphized_methods.get(idx).unwrap() {
                            // both interface and method are generics,
                            // that's why we need to monomorphize
                            let (irv_id, _, _) = self.get_or_declare_monomorph_method_ir_value(*monomorph_id);

                            irv_id
                        } else {
                            // interface is generic, but method is concrete (non-generic)
                            self.decl_to_ir_value_map
                                .get(&DeclID::Method(*method_decl_id))
                                .copied()
                                .unwrap()
                        }
                    } else {
                        self.decl_to_ir_value_map
                            .get(&DeclID::Method(*method_decl_id))
                            .copied()
                            .unwrap()
                    }
                })
                .collect();

            self.vtable_registry.with_vtable_info_mut(vtable_id, |_vtable_info| {
                _vtable_info.abi_name = Some(vtable_abi_name.to_string());
                _vtable_info.vtable_irv_id = Some(irv_id);
                _vtable_info.cir_method_decls = Some(lowered_methods);
            });
        }

        irv_id
    }

    fn lower_func_call(&mut self, func_call: &TypedFuncCall) -> CIRExprKind {
        let args: Vec<CIRExpr> = func_call.args.iter().map(|arg| self.lower_expr(arg)).collect();

        match &func_call.dispatch {
            TypedFuncCallDispatch::Normal { func_decl_id } => {
                let irv_id = self.get_or_declare_func_ir_value(*func_decl_id);

                let func_decl = self.decl_tables.func_decl(*func_decl_id);

                let ret_type = self.lower_sema_type(&func_decl.ret_type);

                let func_type = self.lower_func_type(&func_decl.as_func_type());

                CIRExprKind::Call(CIRCall {
                    args,
                    ret_type,
                    dispatch: CIRCallDispatch::Normal {
                        irv_id,
                        func_type,
                        abi_name: func_decl.name.clone(),
                    },
                })
            }
            TypedFuncCallDispatch::Monomorph { monomorph_id } => {
                let (irv_id, cir_fuc_type, abi_name) = self.get_or_declare_monomorph_func_ir_value(*monomorph_id);

                CIRExprKind::Call(CIRCall {
                    args,
                    ret_type: *cir_fuc_type.ret_type.clone(),
                    dispatch: CIRCallDispatch::Normal {
                        irv_id,
                        func_type: cir_fuc_type,
                        abi_name,
                    },
                })
            }
            TypedFuncCallDispatch::FunctionPointer { func_type } => {
                let operand = Box::new(self.lower_expr(&func_call.operand));

                let ret_type = self.lower_sema_type(&func_type.ret_type);

                CIRExprKind::Call(CIRCall {
                    args,
                    ret_type,
                    dispatch: CIRCallDispatch::FunctionPointer { operand },
                })
            }
            TypedFuncCallDispatch::Unresolved => unreachable!(),
        }
    }

    fn lower_array(&mut self, array: &TypedArrayExpr) -> CIRExprKind {
        let elements: Vec<CIRExpr> = array.elements.iter().map(|elm| self.lower_expr(elm)).collect();

        CIRExprKind::Array(CIRArrayExpr {
            ty: self.lower_sema_type(&array.ty.as_ref().unwrap()),
            elements,
            loc: array.loc,
        })
    }

    fn lower_deref(&mut self, deref: &TypedDerefExpr) -> CIRExprKind {
        CIRExprKind::Deref(CIRDerefExpr {
            operand: Box::new(self.lower_expr(&deref.operand)),
        })
    }

    fn lower_addr_of(&mut self, addr_of: &TypedAddrOfExpr) -> CIRExprKind {
        CIRExprKind::AddrOf(CIRAddrOfExpr {
            operand: Box::new(self.lower_expr(&addr_of.operand)),
        })
    }

    fn lower_assign(&mut self, assign: &TypedAssignExpr) -> CIRExprKind {
        CIRExprKind::Assign(CIRAssignExpr {
            lhs: Box::new(self.lower_expr(&assign.lhs)),
            rhs: Box::new(self.lower_expr(&assign.rhs)),
        })
    }

    fn lower_unary(&mut self, unary: &TypedUnaryExpr) -> CIRExprKind {
        CIRExprKind::Unary(CIRUnaryExpr {
            op: unary.op.clone(),
            operand: Box::new(self.lower_expr(&unary.operand)),
        })
    }

    fn lower_infix(&mut self, infix: &TypedInfixExpr) -> CIRExprKind {
        CIRExprKind::Infix(CIRInfixExpr {
            op: infix.op.clone(),
            lhs: Box::new(self.lower_expr(&infix.lhs)),
            rhs: Box::new(self.lower_expr(&infix.rhs)),
        })
    }

    fn lower_prefix(&mut self, prefix: &TypedPrefixExpr) -> CIRExprKind {
        CIRExprKind::Prefix(CIRPrefixExpr {
            op: prefix.op.clone(),
            operand: Box::new(self.lower_expr(&prefix.operand)),
        })
    }

    fn lower_literal(&mut self, literal: &TypedLiteralExpr) -> CIRExprKind {
        let kind = match &literal.kind {
            LiteralKind::Integer(value, ..) => {
                let is_signed = literal.ty.clone().unwrap().as_plain_type().unwrap().is_signed();
                CIRLiteralKind::Integer(*value, is_signed)
            }
            LiteralKind::Float(value, ..) => CIRLiteralKind::Float(*value),
            LiteralKind::Bool(value) => CIRLiteralKind::Bool(*value),
            LiteralKind::Char(value) => CIRLiteralKind::Char(*value),
            LiteralKind::Null => CIRLiteralKind::Null,
            LiteralKind::String(value, prefix_opt) => match prefix_opt.clone().unwrap_or(StringPrefix::C) {
                StringPrefix::C => CIRLiteralKind::CString(value.clone()),
                StringPrefix::B => CIRLiteralKind::ByteString(value.clone()),
            },
        };

        let ty = self.lower_sema_type(&literal.ty.clone().unwrap());
        CIRExprKind::Literal(CIRLiteral { kind, ty })
    }

    #[inline]
    fn lower_label(&self, label: &TypedLabelStmt) -> CIRStmt {
        CIRStmt::Label(CIRLabelStmt {
            name: label.name.clone(),
            label_id: label.label_id,
            loc: label.loc,
        })
    }

    #[inline]
    fn lower_goto(&self, goto: &TypedGotoStmt) -> CIRStmt {
        CIRStmt::Goto(CIRGotoStmt {
            label_id: goto.label_id.unwrap(),
            loc: goto.loc,
        })
    }
}

// Types.
impl<'a> CIRTraverse<'a> {
    fn lower_sema_type(&mut self, ty: &SemaType) -> CIRType {
        match ty {
            SemaType::Plain(plain_type) => CIRType::Plain(plain_type.clone()),
            SemaType::Named(named_type) => self.lower_named_type(named_type),
            SemaType::InterfaceObject(interface_object) => cir_fat_ptr_type(interface_object.loc),
            SemaType::Array(array_type) => {
                let element_type = self.lower_sema_type(&array_type.element_type);
                let len = match &array_type.capacity {
                    TypedArrayCapacity::Fixed(expr) => expr.literal_const_int_value().unwrap(),
                    TypedArrayCapacity::Dynamic => todo!(),
                };

                CIRType::Array(CIRArrayType {
                    element_type: Box::new(element_type),
                    len: len.try_into().unwrap(),
                })
            }
            SemaType::Const(sema_type) => CIRType::Const(Box::new(self.lower_sema_type(&*sema_type))),
            SemaType::Pointer(sema_type) => CIRType::Pointer(Box::new(self.lower_sema_type(&*sema_type))),
            SemaType::FuncType(func_type) => CIRType::FuncType(self.lower_func_type(func_type)),
            SemaType::Tuple(tuple_type) => {
                let elements: Vec<CIRType> = tuple_type
                    .elements
                    .iter()
                    .map(|sema_type| self.lower_sema_type(sema_type))
                    .collect();

                CIRType::Tuple(CIRTupleType {
                    elements,
                    loc: tuple_type.loc,
                })
            }
            SemaType::Unresolved(_) => unreachable!("unexpected unresolved type"),
            SemaType::GenericParam(_) => unreachable!("Unexpected generic param"),
            SemaType::SelfType(_) => unreachable!("unexpected self type"),
            SemaType::InferVar(_) => unreachable!("unexpected infer var type"),
            SemaType::Placeholder => unreachable!("unexpected placeholder type"),
            SemaType::Err(_) => unreachable!("unexpected error type"),
        }
        .const_inner()
        .clone()
    }

    fn lower_func_type(&mut self, func_type: &TypedFuncType) -> CIRFuncType {
        let ret = Box::new(self.lower_sema_type(&func_type.ret_type));
        let params = self.lower_func_type_params(&func_type.params);

        let mut cir_type = CIRFuncType {
            params: params,
            is_var: func_type.params.variadic.is_some(),
            ret_type: ret,
            callconv: CallConv::default(),
            abi_func_info: None,
        };

        cir_type.abi_func_info = Some(self.target.target_abi.classify_func(&cir_type).unwrap());
        cir_type
    }

    fn lower_struct_decl(&mut self, struct_decl: &StructDecl) -> CIRStructType {
        let fields = struct_decl
            .fields
            .iter()
            .map(|field| self.lower_sema_type(&field.ty))
            .collect();

        let fields_info = struct_decl
            .fields
            .iter()
            .map(|field| (field.name.clone(), field.loc))
            .collect();

        CIRStructType {
            name: struct_decl.name.clone(),
            fields,
            fields_info,
            repr_attr: struct_decl.modifiers.repr_attr.clone(),
            align: struct_decl.align.clone(),
            loc: struct_decl.loc,
        }
    }

    fn lower_enum_variant(&mut self, variant: &TypedEnumVariant) -> CIREnumVariant {
        match variant {
            TypedEnumVariant::Unit(ident) => CIREnumVariant::Unit(ident.as_string()),
            TypedEnumVariant::Valued { ident, value } => {
                CIREnumVariant::Valued(ident.as_string(), Box::new(self.lower_expr(value)))
            }
            TypedEnumVariant::Tuple { ident, fields } => {
                let fields = fields.iter().map(|field| self.lower_sema_type(&field.ty)).collect();

                CIREnumVariant::Payload(ident.as_string(), fields)
            }
            TypedEnumVariant::Struct { ident, fields } => {
                let fields = fields
                    .iter()
                    .map(|struct_variant_field| self.lower_sema_type(&struct_variant_field.ty))
                    .collect();

                CIREnumVariant::Payload(ident.as_string(), fields)
            }
        }
    }

    fn lower_enum_decl(&mut self, enum_decl: &EnumDecl) -> CIREnumType {
        let variants: Vec<CIREnumVariant> = enum_decl
            .variants
            .iter()
            .map(|variant| self.lower_enum_variant(variant))
            .collect();

        let tag_type = enum_decl
            .tag_type
            .clone()
            .map(|sema_type| Box::new(self.lower_sema_type(&sema_type)));

        CIREnumType {
            name: enum_decl.name.clone(),
            variants,
            tag_type,
            repr_attr: enum_decl.modifiers.repr_attr.clone(),
            align: enum_decl.align.clone(),
            loc: enum_decl.loc,
        }
    }

    fn lower_union_decl(&mut self, union_decl: &UnionDecl) -> CIRUnionType {
        let fields = union_decl
            .fields
            .iter()
            .map(|field| self.lower_sema_type(&field.ty))
            .collect();

        let fields_info = union_decl
            .fields
            .iter()
            .map(|field| (field.name.clone(), field.loc))
            .collect();

        CIRUnionType {
            name: union_decl.name.clone(),
            fields,
            fields_info,
            repr_attr: union_decl.modifiers.repr_attr.clone(),
            align: union_decl.align.clone(),
            loc: union_decl.loc,
        }
    }

    fn lower_named_type(&mut self, named_type: &NamedType) -> CIRType {
        match named_type.type_decl_id {
            TypeDeclID::Struct(struct_decl_id) => {
                let struct_decl = self.decl_tables.struct_decl(struct_decl_id);

                let inst_struct_decl = instantiate_struct_decl_with_type_args(&struct_decl, &named_type.type_args);

                CIRType::Struct(self.lower_struct_decl(&inst_struct_decl))
            }
            TypeDeclID::Union(union_decl_id) => {
                let union_decl = self.decl_tables.union_decl(union_decl_id);

                let inst_union_decl = instantiate_union_decl_with_type_args(&union_decl, &named_type.type_args);

                CIRType::Union(self.lower_union_decl(&inst_union_decl))
            }
            TypeDeclID::Enum(enum_decl_id) => {
                let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

                let inst_enum_decl = instantiate_enum_decl_with_type_args(&enum_decl, &named_type.type_args);

                CIRType::Enum(self.lower_enum_decl(&inst_enum_decl))
            }
            TypeDeclID::Interface(interface_decl_id) => {
                let interface_decl = self.decl_tables.interface_decl(interface_decl_id);

                cir_fat_ptr_type(interface_decl.loc)
            }
            TypeDeclID::Typedef(_) => unreachable!("unexpected unexpanded typedef"),
        }
    }
}

// Helpers.
impl<'a> CIRTraverse<'a> {
    fn get_or_declare_method_ir_value(&mut self, method_decl_id: MethodDeclID) -> IRValueID {
        if let Some(irv_id) = self.decl_to_ir_value_map.get(&DeclID::Method(method_decl_id)).copied() {
            return irv_id;
        }

        let method_decl = self.decl_tables.method_decl(method_decl_id);

        let cir_func_decl_stmt = self.lower_func_decl(None, &method_decl.func_decl, false);

        let irv_id = cir_func_decl_stmt.irv_id;

        self.func_decls.insert(irv_id, cir_func_decl_stmt);

        self.decl_to_ir_value_map.insert(DeclID::Method(method_decl_id), irv_id);

        irv_id
    }

    fn get_or_declare_monomorph_method_ir_value(
        &mut self,
        monomorph_id: MonomorphID,
    ) -> (IRValueID, CIRFuncType, String) {
        if let Some(irv_id) = self.monomorph_to_ir_value_map.get(&monomorph_id).copied() {
            let func_decl = self.func_decls.get(&irv_id).unwrap();

            return (irv_id, cir_func_decl_as_func_type(func_decl), func_decl.name.clone());
        }

        let monomorph_instance = self.monomorph_registry.get(monomorph_id);

        let method_decl_id = monomorph_instance.template_id.as_method().unwrap();
        let method_decl = self.decl_tables.method_decl(method_decl_id);

        let mangled_name = mangle_monomorphized_method(
            &self.module_name,
            method_decl_id.0 as usize,
            &method_decl.func_decl.name,
            &monomorph_instance.type_args,
        );

        let ret_type = self.lower_sema_type(&monomorph_instance.ret_type);

        let params = self.lower_func_params(&monomorph_instance.params, true);

        let irv_id = self.new_ir_value_id();

        let mut cir_func_decl = CIRFuncDeclStmt {
            irv_id,
            name: mangled_name.clone(),
            params,
            ret_type,
            abi_func_info: None,
            modifiers: method_decl.func_decl.modifiers.clone(),
            loc: method_decl.func_decl.loc,
        };

        let mut cir_func_type = cir_func_decl_as_func_type(&cir_func_decl);
        let abi_func_info = self.target.target_abi.classify_func(&cir_func_type).unwrap();
        cir_func_decl.abi_func_info = Some(abi_func_info.clone());
        cir_func_type.abi_func_info = Some(abi_func_info);

        self.func_decls.insert(irv_id, cir_func_decl);

        self.monomorph_to_ir_value_map.insert(monomorph_id, irv_id);

        (irv_id, cir_func_type, mangled_name)
    }

    fn get_or_declare_func_ir_value(&mut self, func_decl_id: FuncDeclID) -> IRValueID {
        if let Some(irv_id) = self.decl_to_ir_value_map.get(&DeclID::Func(func_decl_id)).copied() {
            return irv_id;
        }

        let func_decl = self.decl_tables.func_decl(func_decl_id);

        let cir_func_decl_stmt = self.lower_func_decl(Some(func_decl_id), &func_decl, !func_decl.is_func_decl);

        let irv_id = cir_func_decl_stmt.irv_id;

        self.func_decls.insert(irv_id, cir_func_decl_stmt);

        self.decl_to_ir_value_map.insert(DeclID::Func(func_decl_id), irv_id);

        irv_id
    }

    fn get_or_declare_monomorph_func_ir_value(
        &mut self,
        monomorph_id: MonomorphID,
    ) -> (IRValueID, CIRFuncType, String) {
        if let Some(irv_id) = self.monomorph_to_ir_value_map.get(&monomorph_id).copied() {
            let func_decl = self.func_decls.get(&irv_id).unwrap();

            return (irv_id, cir_func_decl_as_func_type(func_decl), func_decl.name.clone());
        }

        let monomorph_instance = self.monomorph_registry.get(monomorph_id);

        let func_decl_id = monomorph_instance.template_id.as_func().unwrap();
        let func_decl = self.decl_tables.func_decl(func_decl_id);

        let mangled_name = mangle_monomorphized_func(
            &func_decl.modifiers,
            &self.module_name,
            &func_decl.name,
            &monomorph_instance.type_args,
        );

        let ret_type = self.lower_sema_type(&monomorph_instance.ret_type);

        let params = self.lower_func_params(&monomorph_instance.params, true);

        let irv_id = self.new_ir_value_id();

        let mut cir_func_decl = CIRFuncDeclStmt {
            irv_id,
            name: mangled_name.clone(),
            params,
            ret_type,
            abi_func_info: None,
            modifiers: func_decl.modifiers.clone(),
            loc: func_decl.loc,
        };

        let mut cir_func_type = cir_func_decl_as_func_type(&cir_func_decl);
        let abi_func_info = self.target.target_abi.classify_func(&cir_func_type).unwrap();
        cir_func_decl.abi_func_info = Some(abi_func_info.clone());
        cir_func_type.abi_func_info = Some(abi_func_info);

        self.func_decls.insert(irv_id, cir_func_decl);

        self.monomorph_to_ir_value_map.insert(monomorph_id, irv_id);

        (irv_id, cir_func_type, mangled_name)
    }

    fn get_or_declare_global_var_ir_value(&mut self, global_var_decl_id: GlobalVarDeclID) -> IRValueID {
        if let Some(irv_id) = self
            .decl_to_ir_value_map
            .get(&DeclID::GlobalVar(global_var_decl_id))
            .copied()
        {
            return irv_id;
        }

        let global_var_decl = self.decl_tables.global_var_decl(global_var_decl_id);

        let irv_id = self.new_ir_value_id();

        let global_var_stmt = self.lower_global_var_decl(irv_id, &global_var_decl);

        self.global_var_decls.insert(irv_id, global_var_stmt);

        self.decl_to_ir_value_map
            .insert(DeclID::GlobalVar(global_var_decl_id), irv_id);

        irv_id
    }

    #[inline(always)]
    fn new_ir_value_id(&mut self) -> IRValueID {
        let id = self.next_irv_id.0;
        self.next_irv_id.0 += 1;
        IRValueID(id)
    }
}

#[inline(never)]
pub fn walk_program_trees_in_parallel(
    threads: Option<usize>,
    program_trees: Vec<Box<TypedProgramTree>>,
    query: &dyn SymbolQuery,
    formatter: &dyn Formatter,
    source_map: Arc<SourceMap>,
    decl_tables: Arc<DeclTablesRegistry>,
    vtable_registries: &FxHashMap<FileID, Arc<VTableRegistry>>,
    monomorph_registry: Arc<MonomorphRegistry>,
    target: &ABITarget,
) -> Vec<Box<CIRModule>> {
    use rayon::prelude::*;

    let num_threads = threads.unwrap_or_else(|| num_cpus::get().max(1));

    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(num_threads)
        .build()
        .expect("Failed to build thread pool.");

    pool.install(|| {
        program_trees
            .into_par_iter()
            .map(|program_tree| {
                let vtable_registry = vtable_registries.get(&program_tree.file_id).cloned().unwrap();

                let module_name = query.lookup_module_name(program_tree.file_id).unwrap();

                let mut cir_walk = CIRTraverse::new(
                    program_tree.clone(),
                    module_name,
                    decl_tables.clone(),
                    formatter,
                    vtable_registry,
                    monomorph_registry.clone(),
                    target,
                );

                let file_path = source_map.get_file(program_tree.file_id).unwrap().file_path.clone();

                Box::new(cir_walk.lower(file_path.to_string_lossy().to_string()))
            })
            .collect()
    })
}
