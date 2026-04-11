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
use cyrusc_internal::abi::mangler::mangle_func;
use cyrusc_internal::abi::mangler::mangle_global_var;
use cyrusc_internal::abi::target::ABITarget;
use cyrusc_internal::cir::cir::*;
use cyrusc_internal::cir::instances::CIRInstanceRegistry;
use cyrusc_internal::cir::types::*;
use cyrusc_internal::symbols::SymbolQuery;
use cyrusc_internal::vtable::VTableRegistry;
use cyrusc_source_loc::SourceMap;
use cyrusc_tokens::literals::*;
use cyrusc_typed_ast::TypedProgramTree;
use cyrusc_typed_ast::decls::table::DeclTablesRegistry;
use cyrusc_typed_ast::decls::*;
use cyrusc_typed_ast::stmts::*;
use cyrusc_typed_ast::types::*;
use cyrusc_typed_ast::{SymbolID, exprs::*};
use fx_hash::FxHashMap;
use std::sync::{Arc, Mutex};

pub(crate) struct CIRTraverse<'a> {
    program_tree: Box<TypedProgramTree>,
    query: &'a dyn SymbolQuery,
    decl_tables: Arc<DeclTablesRegistry>,
    cir_instance_registry: Arc<Mutex<CIRInstanceRegistry>>,
    vtable_registry: Arc<Mutex<VTableRegistry>>,
    target: &'a ABITarget,
    module_name: String,

    next_irv_id: IRValueID,
    decl_to_ir_value_map: FxHashMap<DeclID, IRValueID>,
    func_decls: FxHashMap<IRValueID, CIRFuncDeclStmt>,
    global_var_decls: FxHashMap<IRValueID, CIRGlobalVarStmt>,
}

impl<'resolver> CIRTraverse<'resolver> {
    pub fn new(
        program_tree: Box<TypedProgramTree>,
        query: &'resolver dyn SymbolQuery,
        decl_tables: Arc<DeclTablesRegistry>,
        cir_instance_registry: Arc<Mutex<CIRInstanceRegistry>>,
        vtable_registry: Arc<Mutex<VTableRegistry>>,
        target: &'resolver ABITarget,
    ) -> Self {
        let module_name = query.lookup_module_name(program_tree.file_id).unwrap();

        Self {
            program_tree,
            query,
            decl_tables,
            cir_instance_registry,
            vtable_registry,
            target,
            module_name,
            next_irv_id: IRValueID(0),
            decl_to_ir_value_map: FxHashMap::default(),
            global_var_decls: FxHashMap::default(),
            func_decls: FxHashMap::default(),
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
        }
    }

    fn lower_stmt(&mut self, stmt: &TypedStmt, lowered_stmts: &mut Vec<CIRStmt>) {
        match stmt {
            TypedStmt::FuncDef(func_def) => {
                if func_def.is_generic() {
                    return; // skip lowering at this point
                }
                lowered_stmts.push(self.lower_func_def(func_def, true));
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
            TypedStmt::ExportTuple(export_tuple_stmt) => {
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
                lowered_stmts.push(CIRStmt::Expr(self.lower_expr(expr)));
            }
            TypedStmt::Struct(struct_stmt) => {
                if struct_stmt.is_generic() {
                    let stmts = self.lower_non_generic_methods(&struct_stmt.name, &struct_stmt.methods);
                    lowered_stmts.extend(stmts);
                }
            }
            TypedStmt::Enum(enum_stmt) => {
                if enum_stmt.is_generic() {
                    let stmts = self.lower_non_generic_methods(&enum_stmt.name, &enum_stmt.methods);
                    lowered_stmts.extend(stmts);
                }
            }
            TypedStmt::Union(union_stmt) => {
                if union_stmt.is_generic() {
                    let stmts = self.lower_non_generic_methods(&union_stmt.name, &union_stmt.methods);
                    lowered_stmts.extend(stmts);
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

    fn lower_label(&self, label: &TypedLabelStmt) -> CIRStmt {
        CIRStmt::Label(CIRLabelStmt {
            name: label.name.clone(),
            label_id: label.label_id,
            loc: label.loc,
        })
    }

    fn lower_defer(&mut self, defer: &TypedDeferStmt) -> CIRStmt {
        let mut lowered_stmts = Vec::new();
        self.lower_stmt(&defer.operand, &mut lowered_stmts);
        assert_eq!(lowered_stmts.len(), 1);

        let operand = lowered_stmts.first().unwrap();
        operand.clone()
    }

    #[inline]
    fn lower_goto(&self, goto: &TypedGotoStmt) -> CIRStmt {
        CIRStmt::Goto(CIRGotoStmt {
            label_id: goto.label_id.unwrap(),
            loc: goto.loc,
        })
    }

    pub fn lower_export_tuple_to_vars(&mut self, export_tuple: &TypedExportTupleStmt) -> Vec<CIRVarStmt> {
        let mut vars = Vec::new();
        self.lower_export_pattern_recursive(&export_tuple.pattern, &mut vars);
        vars
    }

    fn lower_export_pattern_recursive(&mut self, pattern: &TypedExportPattern, vars: &mut Vec<CIRVarStmt>) {
        match &pattern.kind {
            TypedExportPatternKind::Ident(symbol_id) => {
                let var_decl_id = self.query.get_var(*symbol_id).unwrap();
                let var_decl = self.decl_tables.var_decl(var_decl_id);

                let var_name = var_decl.name.clone();
                let var_ty = self.lower_sema_type(&var_decl.ty.as_ref().unwrap());
                let var_rhs = self.lower_expr(&var_decl.rhs.as_ref().unwrap());

                let irv_id = self.next_irv_id();

                self.decl_to_ir_value_map.insert(DeclID::Var(var_decl_id), irv_id);

                vars.push(CIRVarStmt {
                    irv_id,
                    name: var_name,
                    ty: var_ty,
                    expr: Some(var_rhs),
                    loc: var_decl.loc,
                });
            }
            TypedExportPatternKind::Tuple(export_patterns) => {
                for pattern in export_patterns {
                    self.lower_export_pattern_recursive(pattern, vars);
                }
            }
            TypedExportPatternKind::Ignore => { /* skip */ }
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
            .or_else(|| global_var.expr.as_ref().and_then(|expr| expr.sema_type.as_ref()))
            .map(|sema_type| self.lower_sema_type(sema_type))
            .unwrap_or_else(|| {
                panic!(
                    "Global var '{}' has neither explicit type nor valid initializer type.",
                    global_var.name
                )
            });

        let expr = global_var.expr.clone().and_then(|expr| Some(self.lower_expr(&expr)));

        let mangled_name = mangle_global_var(&global_var.modifiers, &self.module_name, &global_var.name);

        let irv_id = self.next_irv_id();

        let global_var_decl_id = self.query.get_global_var(global_var.symbol_id).unwrap();

        let global_var_stmt = CIRGlobalVarStmt {
            irv_id,
            name: mangled_name,
            ty,
            expr,
            modifiers: global_var.modifiers.clone(),
            loc: global_var.loc,
        };

        self.decl_to_ir_value_map
            .insert(DeclID::GlobalVar(global_var_decl_id), irv_id);

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
            .or_else(|| var.rhs.as_ref().and_then(|rhs| rhs.sema_type.as_ref()))
            .map(|ty| self.lower_sema_type(ty))
            .unwrap_or_else(|| {
                panic!(
                    "Variable '{}' has neither explicit type nor RHS type ({}:{})",
                    var.name, var.loc.line, var.loc.column
                )
            });

        let expr = var.rhs.clone().and_then(|expr| Some(self.lower_expr(&expr)));

        let irv_id = self.next_irv_id();

        let var_decl_id = self.query.get_var(var.symbol_id).unwrap();

        self.decl_to_ir_value_map.insert(DeclID::Var(var_decl_id), irv_id);

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
        CIRFuncParams {
            list: func_params
                .list
                .iter()
                .map(|param_kind| {
                    let name = Some(param_kind.name());
                    let loc = param_kind.loc();

                    match param_kind {
                        TypedFuncParamKind::FuncParam(func_param) => {
                            let irv_id = { if !is_decl { Some(self.next_irv_id()) } else { None } };

                            CIRFuncParam {
                                name,
                                irv_id,
                                ty: self.lower_sema_type(&func_param.ty),
                                loc,
                            }
                        }
                        TypedFuncParamKind::SelfModifier(self_modifier) => {
                            let irv_id = self.next_irv_id();

                            CIRFuncParam {
                                name,
                                irv_id: Some(irv_id),
                                ty: self.lower_sema_type(&self_modifier.ty.as_ref().unwrap()),
                                loc,
                            }
                        }
                    }
                })
                .collect(),

            is_var: func_params.variadic.is_some(),
        }
    }

    fn lower_func_def(&mut self, func_def: &TypedFuncDefStmt, mangle_name: bool) -> CIRStmt {
        let params = self.lower_func_params(&func_def.params, false);

        let body = self.lower_body(&func_def.body);
        let ret = self.lower_sema_type(&func_def.ret_type);

        let irv_id = self.next_irv_id();

        let func_decl_id = self.query.get_func(func_def.symbol_id).unwrap();

        self.decl_to_ir_value_map.insert(DeclID::Func(func_decl_id), irv_id);

        let mut cir_func_def = CIRFuncDefStmt {
            irv_id,
            name: func_def.name.clone(),
            params,
            body: Box::new(body),
            ret,
            modifiers: func_def.modifiers.clone(),
            abi_func_info: None,
            loc: func_def.loc,
        };

        let cir_func_type = cir_func_decl_as_func_ty(&cir_func_def_as_decl(&cir_func_def));
        cir_func_def.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());

        if mangle_name {
            let mangled_name = mangle_func(&cir_func_def.modifiers, &self.module_name, &cir_func_def.name);
            cir_func_def.name = mangled_name;
        }

        CIRStmt::FuncDef(cir_func_def)
    }

    fn lower_func_decl(&mut self, func_decl: &FuncDecl, mangle_name: bool) -> CIRFuncDeclStmt {
        let params = self.lower_func_params(&func_decl.params, true);

        let ret_type = self.lower_sema_type(&func_decl.ret_type);

        let func_name = if mangle_name {
            mangle_func(&func_decl.modifiers, &self.module_name, &func_decl.name)
        } else {
            func_decl.name.clone()
        };

        let irv_id = self.next_irv_id();

        let func_decl_id = self.query.get_func(func_decl.symbol_id.unwrap()).unwrap();

        self.decl_to_ir_value_map.insert(DeclID::Func(func_decl_id), irv_id);

        let mut cir_func_decl = CIRFuncDeclStmt {
            irv_id,
            name: func_name,
            params,
            ret: ret_type,
            modifiers: func_decl.modifiers.clone(),
            abi_func_info: None,
            loc: func_decl.loc,
        };

        let cir_func_type = cir_func_decl_as_func_ty(&cir_func_decl);
        cir_func_decl.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());

        self.func_decls.insert(irv_id, cir_func_decl.clone());

        cir_func_decl
    }

    fn lower_func_decl_stmt(&mut self, func_decl_stmt: &TypedFuncDeclStmt, mangle_name: bool) -> CIRFuncDeclStmt {
        let func_decl = func_decl_stmt.as_func_decl();
        self.lower_func_decl(&func_decl, mangle_name)
    }

    pub(crate) fn lower_body(&mut self, block: &TypedBlockStmt) -> CIRBlockStmt {
        let stmts = self.lower_stmts(&block.stmts);
        let defers = block.defers.iter().map(|defer| self.lower_defer(defer)).collect();

        CIRBlockStmt {
            stmts,
            defers,
            loc: block.loc,
        }
    }

    fn lower_expr(&mut self, expr: &TypedExprStmt) -> CIRExpr {
        if cfg!(debug_assertions) {
            if expr.sema_type.is_none() {
                dbg!(expr.clone());
            }
            debug_assert!(expr.sema_type.is_some());
        }

        let ty = self.lower_sema_type(&expr.sema_type.clone().unwrap());

        let kind = match &expr.kind {
            TypedExprKind::Symbol(symbol_expr) => self.lower_load_symbol(symbol_expr.symbol_id),
            TypedExprKind::Literal(literal_expr) => self.lower_literal(literal_expr),
            TypedExprKind::Prefix(prefix_expr) => self.lower_prefix(prefix_expr),
            TypedExprKind::Infix(infix_expr) => self.lower_infix(infix_expr),
            TypedExprKind::Unary(unary_expr) => self.lower_unary(unary_expr),
            TypedExprKind::Assign(assign_expr) => self.lower_assign(assign_expr),
            TypedExprKind::AddrOf(addr_of_expr) => self.lower_addr_of(addr_of_expr),
            TypedExprKind::Deref(deref_expr) => self.lower_deref(deref_expr),
            TypedExprKind::Array(array_expr) => self.lower_array(array_expr),
            TypedExprKind::ArrayIndex(array_index_expr) => self.lower_array_index(array_index_expr),
            TypedExprKind::FuncCall(func_call) => self.lower_func_call(func_call),
            TypedExprKind::MethodCall(method_call) => self.lower_method_call(method_call),
            TypedExprKind::FieldAccess(field_access) => self.lower_field_access(field_access.clone()),
            TypedExprKind::Lambda(lambda_expr) => self.lower_lambda(lambda_expr),
            TypedExprKind::Tuple(tuple_expr) => self.lower_tuple(tuple_expr),
            TypedExprKind::TupleAccess(tuple_access_expr) => self.lower_tuple_access(tuple_access_expr),
            TypedExprKind::Dynamic(dynamic) => self.lower_dynamic(dynamic),
            TypedExprKind::StructInit(struct_init_expr) => self.lower_struct_init(struct_init_expr),
            TypedExprKind::EnumInit(enum_init) => todo!(),
            TypedExprKind::UnionInit(union_init) => todo!(),

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

    pub(crate) fn lower_load_symbol(&mut self, symbol_id: SymbolID) -> CIRExprKind {
        let symbol_entry = self.query.lookup_symbol_entry(symbol_id).unwrap();

        if let Some(func_decl_id) = symbol_entry.as_func() {
            let irv_id = self
                .decl_to_ir_value_map
                .get(&DeclID::Func(func_decl_id))
                .copied()
                .unwrap();

            CIRExprKind::Load(CIRValue {
                irv_id,
                kind: CIRValueKind::Func,
            })
        } else if let Some(global_var_decl_id) = symbol_entry.as_global_var() {
            let irv_id = self
                .decl_to_ir_value_map
                .get(&DeclID::GlobalVar(global_var_decl_id))
                .copied()
                .unwrap();

            CIRExprKind::Load(CIRValue {
                irv_id,
                kind: CIRValueKind::GlobalVar,
            })
        } else if let Some(var_decl_id) = symbol_entry.as_var() {
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

    // FIXME
    pub(crate) fn lower_struct_init(&mut self, struct_init_expr: &TypedStructInitExpr) -> CIRExprKind {
        todo!();

        // let symbol_entry = self.query.lookup_symbol_entry(struct_init_expr.symbol_id).unwrap();

        // if let Some(resolved_struct) = symbol_entry.as_struct() {
        //     let fields = struct_init_expr
        //         .fields
        //         .iter()
        //         .map(|field| self.lower_sema_ty(&field.value.sema_type.clone().unwrap()))
        //         .collect();

        //     let fields_info = struct_init_expr
        //         .fields
        //         .iter()
        //         .map(|field| (field.name.clone(), field.loc))
        //         .collect();

        //     let struct_ty = CIRStructType {
        //         name: Some(resolved_struct.struct_decl.name.clone()),
        //         repr_attr: resolved_struct.struct_decl.modifiers.repr_attr.clone(),
        //         align: resolved_struct.struct_decl.align.clone(),
        //         fields,
        //         fields_info,
        //         loc: resolved_struct.struct_decl.loc,
        //     };

        //     let fields: Vec<CIRExpr> = struct_init_expr
        //         .fields
        //         .iter()
        //         .map(|field| self.lower_expr(&field.value))
        //         .collect();

        //     CIRExprKind::StructInit(CIRStructInitExpr { ty: struct_ty, fields })
        // } else if let Some(resolved_union) = symbol_entry.as_union() {
        //     let fields = struct_init_expr
        //         .fields
        //         .iter()
        //         .map(|field| self.lower_sema_ty(&field.value.sema_type.clone().unwrap()))
        //         .collect();

        //     let fields_info = struct_init_expr
        //         .fields
        //         .iter()
        //         .map(|field| (field.name.clone(), field.loc))
        //         .collect();

        //     let union_ty = CIRType::Union(CIRUnionTy {
        //         name: Some(resolved_union.union_decl.name.clone()),
        //         fields,
        //         fields_info,
        //         repr_attr: resolved_union.union_decl.modifiers.repr_attr.clone(),
        //         align: resolved_union.union_decl.align.clone(),
        //         loc: resolved_union.union_decl.loc,
        //     });

        //     let struct_field_init = struct_init_expr.fields.first().unwrap();
        //     let expr = Box::new(self.lower_expr(&struct_field_init.value));

        //     CIRExprKind::UnionInit(CIRUnionInitExpr { expr, ty: union_ty })
        // } else {
        //     unreachable!()
        // }
    }

    fn lower_tuple_access(&mut self, tuple_access: &TypedTupleAccessExpr) -> CIRExprKind {
        let operand = Box::new(self.lower_expr(&tuple_access.operand));
        CIRExprKind::TupleAccess(CIRTupleAccessExpr {
            operand,
            index: tuple_access.index,
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
            ret: Box::new(ret.clone()),
            callconv: CallConv::default(),
            abi_func_info: None,
        };

        let abi_func_info = self.target.target_abi.classify_func(&cir_func_type).unwrap();

        let irv_id = self.next_irv_id();

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

    // FIXME
    fn lower_dynamic(&mut self, dynamic: &TypedDynamicExpr) -> CIRExprKind {
        todo!();

        // let operand = self.lower_expr(&dynamic.operand);
        // let data_ptr = CIRExpr {
        //     kind: CIRExprKind::AddrOf(CIRAddrOfExpr {
        //         operand: Box::new(operand),
        //     }),
        //     ty: CIRType::Pointer(Box::new(CIRType::PlainType(PlainType::Void))),
        //     loc: dynamic.loc,
        // };

        // let vtable_info = {
        //     let vtable_registry = self.vtable_registry.lock().unwrap();
        //     vtable_registry.info(dynamic.vtable_id.unwrap()).clone()
        // };

        // let method_decls: Vec<CIRFuncDeclStmt> = vtable_info
        //     .methods
        //     .iter()
        //     .cloned()
        //     .map(|mut func_decl| {
        //         let mangled_name = mangle_method(
        //             &self.query.lookup_module_name(func_decl.module_id).unwrap(),
        //             &dynamic.object_name.as_ref().unwrap(),
        //             &func_decl.name,
        //         );

        //         func_decl.name = mangled_name;
        //         self.lower_func_sig(func_decl.symbol_id.unwrap().0, &func_decl)
        //     })
        //     .collect();

        // let vtable_abi_name = DEFAULT_ABI.vtable_name(
        //     &dynamic.object_name.clone().unwrap(),
        //     &dynamic.vtable_id.unwrap().to_string(),
        // );

        // CIRExprKind::Dynamic(CIRDynamicExpr {
        //     global_var_id: vtable_info.vtable_id.0,
        //     data_expr: Box::new(data_ptr),
        //     method_decls,
        //     vtable_abi_name,
        //     loc: dynamic.loc,
        // })
    }

    // FIXME
    fn lower_method_call(&mut self, method_call: &TypedMethodCall) -> CIRExprKind {
        todo!();
    }

    // FIXME
    fn lower_field_access(&mut self, mut field_access: TypedFieldAccess) -> CIRExprKind {
        todo!();

        // if field_access.is_fat_arrow {
        //     field_access.operand = Box::new(TypedExprStmt {
        //         kind: TypedExprKind::Deref(TypedDerefExpr {
        //             operand: field_access.operand.clone(),
        //             loc: field_access.loc,
        //         }),
        //         sema_type: Some(field_access.operand.sema_type.clone().unwrap().pointer_inner().clone()),
        //         mloc: MemoryLocation::LValue,
        //         loc: field_access.loc,
        //     })
        // }

        // if let Some(sema_type) = &field_access.operand.sema_type {
        //     if sema_type.as_unnamed_struct().is_some() {
        //         return CIRExprKind::StructFieldAccess(CIRStructFieldAccessExpr {
        //             operand: Box::new(self.lower_expr(&field_access.operand)),
        //             field_idx: field_access.field_index.unwrap(),
        //             field_ty: self.lower_sema_ty(&field_access.field_ty.as_ref().unwrap()),
        //         });
        //     }

        //     if sema_type.as_unnamed_union().is_some() {
        //         return CIRExprKind::UnionFieldAccess(CIRUnionFieldAccessExpr {
        //             operand: Box::new(self.lower_expr(&field_access.operand)),
        //             field_ty: self.lower_sema_ty(&field_access.field_ty.as_ref().unwrap()),
        //         });
        //     }
        // }

        // let symbol_entry = self
        //     .query
        //     .lookup_symbol_entry(field_access.object_symbol_id.unwrap())
        //     .unwrap();

        // if symbol_entry.as_struct().is_some() {
        //     CIRExprKind::StructFieldAccess(CIRStructFieldAccessExpr {
        //         operand: Box::new(self.lower_expr(&field_access.operand)),
        //         field_idx: field_access.field_index.unwrap(),
        //         field_ty: self.lower_sema_ty(&field_access.field_ty.as_ref().unwrap()),
        //     })
        // } else if symbol_entry.as_union().is_some() {
        //     CIRExprKind::UnionFieldAccess(CIRUnionFieldAccessExpr {
        //         operand: Box::new(self.lower_expr(&field_access.operand)),
        //         field_ty: self.lower_sema_ty(&field_access.field_ty.as_ref().unwrap()),
        //     })
        // } else if let Some(mut resolved_enum) = symbol_entry.as_enum().cloned() {
        //     let sema_type = field_access.operand.sema_type.as_ref().unwrap();

        //     let variant: CIREnumInitVariant;
        //     let enum_ty: CIREnumTy;
        //     if let Some(generic_type) = sema_type.as_generic_type() {
        //         resolved_enum.enum_decl = substitute_enum_sig(
        //             self.mapping_ctx_arena.clone(),
        //             &resolved_enum.enum_decl,
        //             generic_type.mapping_ctx.clone(),
        //         )
        //         .unwrap()
        //     }

        //     let typed_variant = resolved_enum
        //         .enum_decl
        //         .variants
        //         .get(field_access.field_index.unwrap())
        //         .unwrap();

        //     variant = match typed_variant {
        //         TypedEnumVariant::Ident(..) => CIREnumInitVariant::Ident,
        //         TypedEnumVariant::Valued(_, expr) => CIREnumInitVariant::Valued(Box::new(self.lower_expr(expr))),
        //         TypedEnumVariant::Variant(..) => unreachable!(),
        //     };

        //     enum_ty = self.lower_enum_sig_as_enum_ty(&resolved_enum.enum_decl);

        //     let tag = enum_ty.compute_variant_tag(&field_access.field_name).unwrap();

        //     CIRExprKind::EnumInit(CIREnumInitExpr {
        //         tag: tag.try_into().unwrap(),
        //         variant,
        //         enum_ty,
        //     })
        // } else {
        //     unreachable!()
        // }
    }

    // FIXME
    fn lower_non_generic_methods(&mut self, object_name: &String, methods: &MethodDecls) -> Vec<CIRStmt> {
        todo!();

        // let mut stmts: Vec<CIRStmt> = Vec::new();

        // for method_id in methods.values().cloned() {
        //     let method_decl_id = self.query.get_method(method_id).unwrap();
        //     let method_decl = self.decl_tables.method_decl(method_decl_id);

        //     let lowered_method = self.lower_method(&method_decl, object_name).unwrap();
        //     stmts.push(lowered_method);
        // }

        // stmts
    }

    // FIXME
    fn lower_method(&mut self, method_decl: &MethodDecl, object_name: &String) -> Option<CIRStmt> {
        todo!();

        // let mangled_name = DEFAULT_ABI.method_name(&self.module_name, object_name, &resolved_method.func_decl.name);

        // // skip if has no body
        // let func_body = resolved_method.func_body.clone()?;

        // let func_def = TypedFuncDefStmt {
        //     symbol_id: resolved_method.symbol_id,
        //     name: mangled_name,
        //     params: resolved_method.func_decl.params.clone(),
        //     generic_params: resolved_method.func_decl.generic_params.clone(),
        //     body: func_body,
        //     ret_type: resolved_method.func_decl.ret_type.clone(),
        //     modifiers: resolved_method.func_decl.modifiers.clone(),
        //     loc: resolved_method.func_decl.loc,
        // };

        // Some(self.lower_func_def(&func_def, false))
    }

    fn lower_func_call(&mut self, func_call: &TypedFuncCall) -> CIRExprKind {
        let args: Vec<CIRExpr> = func_call.args.iter().map(|arg| self.lower_expr(arg)).collect();

        match &func_call.dispatch {
            TypedFuncCallDispatch::Unresolved => unreachable!(),
            TypedFuncCallDispatch::Normal { func_decl_id } => {
                let func_decl = self.decl_tables.func_decl(*func_decl_id);

                let irv_id = self
                    .decl_to_ir_value_map
                    .get(&DeclID::Func(*func_decl_id))
                    .copied()
                    .unwrap();

                let ret_type = self.lower_sema_type(&func_decl.ret_type);

                let func_type = self.lower_func_type(&func_decl.as_func_type());

                CIRExprKind::Call(CIRCall {
                    args,
                    ret_type,
                    dispatch: CIRCallDispatch::Normal { irv_id, func_type },
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
            TypedFuncCallDispatch::Monomorph {
                func_decl_id,
                monomorph_id,
            } => todo!(),
        }
    }

    fn lower_array_index(&mut self, array_index: &TypedArrayIndexExpr) -> CIRExprKind {
        CIRExprKind::ArrayIndex(CIRArrayIndexExpr {
            operand: Box::new(self.lower_expr(&array_index.operand)),
            index: Box::new(self.lower_expr(&array_index.index)),
        })
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

    fn lower_func_type(&mut self, func_type: &TypedFuncType) -> CIRFuncType {
        let ret = Box::new(self.lower_sema_type(&func_type.ret_type));
        let params = self.lower_func_type_params(&func_type.params);

        let mut cir_type = CIRFuncType {
            params: params,
            is_var: func_type.params.variadic.is_some(),
            ret,
            callconv: CallConv::default(),
            abi_func_info: None,
        };

        cir_type.abi_func_info = Some(self.target.target_abi.classify_func(&cir_type).unwrap());
        cir_type
    }

    fn lower_sema_type(&mut self, sema_type: &SemaType) -> CIRType {
        match sema_type {
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
            SemaType::InterfaceType(interface_type) => CIRType::Struct(CIRStructType {
                name: None,
                fields: vec![
                    CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void))),
                    CIRType::Pointer(Box::new(CIRType::Plain(PlainType::Void))),
                ],
                fields_info: vec![
                    ("data_ptr".to_string(), interface_type.loc),
                    ("vtable_ptr".to_string(), interface_type.loc),
                ],
                repr_attr: None,
                align: None,
                loc: interface_type.loc,
            }),
            SemaType::Named(named_type) => self.lower_named_type(named_type),
            SemaType::Plain(plain_type) => CIRType::Plain(plain_type.clone()),
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
                let tuple_fields = fields.iter().map(|field| self.lower_sema_type(&field.ty)).collect();

                CIREnumVariant::Tuple(ident.as_string(), tuple_fields)
            }
            TypedEnumVariant::Struct { ident, fields } => todo!(), // TODO
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
        match named_type.decl_id {
            TypeDeclID::Struct(struct_decl_id) => {
                let struct_decl = self.decl_tables.struct_decl(struct_decl_id);
                CIRType::Struct(self.lower_struct_decl(&struct_decl))
            }
            TypeDeclID::Enum(enum_decl_id) => {
                let enum_decl = self.decl_tables.enum_decl(enum_decl_id);
                CIRType::Enum(self.lower_enum_decl(&enum_decl))
            }
            TypeDeclID::Union(union_decl_id) => {
                let union_decl = self.decl_tables.union_decl(union_decl_id);
                CIRType::Union(self.lower_union_decl(&union_decl))
            }
            TypeDeclID::Interface(interface_decl_id) => {
                // let interface_decl = self.decl_tables.interface_decl(interface_decl_id);
                // CIRType::Dynamic(self.lower_interface(&interface_decl))

                // FIXME
                todo!();
            }
            TypeDeclID::Typedef(_) => unreachable!("unexpected unexpanded typedef"),
        }
    }

    #[inline(always)]
    fn next_irv_id(&mut self) -> IRValueID {
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
    source_map: Arc<SourceMap>,
    decl_tables: Arc<DeclTablesRegistry>,
    cir_instance_registry: Arc<Mutex<CIRInstanceRegistry>>,
    vtable_registries: &Vec<Arc<Mutex<VTableRegistry>>>,
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
            .enumerate()
            .map(|(i, program_tree)| {
                let vtable_registry = vtable_registries[i].clone();

                let mut cir_walk = CIRTraverse::new(
                    program_tree.clone(),
                    query,
                    decl_tables.clone(),
                    cir_instance_registry.clone(),
                    vtable_registry,
                    target,
                );

                let file_path = source_map.get_file(program_tree.file_id).unwrap().file_path.clone();

                Box::new(cir_walk.lower(file_path.to_string_lossy().to_string()))
            })
            .collect()
    })
}
