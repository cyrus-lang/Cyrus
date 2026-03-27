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

use crate::abi::mangler::*;
use crate::abi::target::ABITarget;
use crate::cir::cir::*;
use crate::cir::monomorph::CIRMonomorphRegistry;
use crate::cir::types::*;
use crate::symbols::symbols::ResolvedMethod;
use crate::symbols::table::Query;
use crate::vtable::VTableRegistry;
use cyrusc_ast::abi::CallConv;
use cyrusc_ast::abi::Linkage;
use cyrusc_ast::operators::*;
use cyrusc_source_loc::SourceMap;
use cyrusc_tokens::literals::*;
use cyrusc_typed_ast::TypedProgramTree;
use cyrusc_typed_ast::format::SymbolFormatterFn;
use cyrusc_typed_ast::generics::generic_type::GenericType;
use cyrusc_typed_ast::generics::mapping_ctx_arena::GenericMappingCtxArena;
use cyrusc_typed_ast::generics::substitute::*;
use cyrusc_typed_ast::sigs::typed_func_decl_from_func_sig;
use cyrusc_typed_ast::sigs::*;
use cyrusc_typed_ast::stmts::*;
use cyrusc_typed_ast::types::*;
use cyrusc_typed_ast::{SymbolID, exprs::*};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

pub(crate) struct CIRTraverse<'q> {
    program_tree: Box<TypedProgramTree>,
    current_self_ty: Option<CIRTy>,
    pub(crate) query: &'q dyn Query,
    pub(crate) cir_monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
    pub(crate) current_obj_operand_ty: Option<SemanticType>,
    pub(crate) mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    pub(crate) vtable_registry: Arc<Mutex<VTableRegistry>>,
    pub(crate) target: &'q ABITarget,

    module_name: String,
    lambda_id: u32,
}

impl<'resolver> CIRTraverse<'resolver> {
    pub fn new(
        program_tree: Box<TypedProgramTree>,
        query: &'resolver dyn Query,
        cir_monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
        mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
        vtable_registry: Arc<Mutex<VTableRegistry>>,
        target: &'resolver ABITarget,
    ) -> Self {
        let module_name = query.lookup_module_name(program_tree.file_id).unwrap();

        Self {
            program_tree,
            query,
            cir_monomorph_registry,
            current_self_ty: None,
            current_obj_operand_ty: None,
            mapping_ctx_arena,
            vtable_registry,
            target,
            module_name,
            lambda_id: 0,
        }
    }

    pub fn run_pass(&mut self, file_path: String) -> CIRProgramTree {
        let stmts = std::mem::take(&mut self.program_tree.body);
        let lowered_stmts = self.lower_stmts(&stmts);

        CIRProgramTree {
            body: lowered_stmts,
            file_path,
            module_name: self.module_name.clone(),
        }
    }

    fn lower_stmt(&mut self, stmt: &TypedStmt, lowered_stmts: &mut Vec<CIRStmt>) {
        match stmt {
            TypedStmt::FuncDef(func_def) => {
                if func_def.generic_params.is_some() {
                    return; // skip lowering at this point
                }
                lowered_stmts.push(self.lower_func_def(func_def, true));
            }
            TypedStmt::FuncDecl(func_decl) => {
                lowered_stmts.push(CIRStmt::FuncDecl(self.lower_func_decl(func_decl, true)));
            }
            TypedStmt::Switch(switch_stmt) => {
                lowered_stmts.push(self.lower_switch(switch_stmt));
            }
            TypedStmt::Variable(var) => {
                lowered_stmts.push(CIRStmt::Variable(self.lower_var(var)));
            }
            TypedStmt::GlobalVar(global_var) => {
                lowered_stmts.push(self.lower_global_var(&mut global_var.clone()));
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
                if struct_stmt.generic_params.is_none() {
                    let stmts = self.lower_non_generic_methods(&struct_stmt.name, &struct_stmt.methods);
                    lowered_stmts.extend(stmts);
                }
            }
            TypedStmt::Enum(enum_stmt) => {
                if enum_stmt.generic_params.is_none() {
                    let stmts = self.lower_non_generic_methods(&enum_stmt.name, &enum_stmt.methods);
                    lowered_stmts.extend(stmts);
                }
            }
            TypedStmt::Union(union_stmt) => {
                if union_stmt.generic_params.is_none() {
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

    fn lower_non_generic_methods(&mut self, object_name: &String, methods: &HashMap<String, SymbolID>) -> Vec<CIRStmt> {
        let mut stmts: Vec<CIRStmt> = Vec::new();

        for method_id in methods.values().cloned() {
            let resolved_method = self.query.get_method(method_id).unwrap();
            let lowered_method = self.lower_method(&resolved_method, object_name).unwrap();
            stmts.push(lowered_method);
        }

        stmts
    }

    fn lower_method(&mut self, resolved_method: &ResolvedMethod, object_name: &String) -> Option<CIRStmt> {
        let mangled_name = DEFAULT_ABI.method_name(&self.module_name, object_name, &resolved_method.func_sig.name);

        // skip if has no body
        let func_body = resolved_method.func_body.clone()?;

        let func_def = TypedFuncDefStmt {
            symbol_id: resolved_method.symbol_id,
            name: mangled_name,
            params: resolved_method.func_sig.params.clone(),
            generic_params: resolved_method.func_sig.generic_params.clone(),
            body: func_body,
            ret_type: resolved_method.func_sig.ret_type.clone(),
            modifiers: resolved_method.func_sig.modifiers.clone(),
            loc: resolved_method.func_sig.loc,
        };

        Some(self.lower_func_def(&func_def, false))
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

    // FIXME
    fn lower_goto(&self, goto: &TypedGotoStmt) -> CIRStmt {
        todo!();
        // let scope_rc = self
        //     .query
        //     .resolve_local_scope(self.module_id, scope_id_opt.unwrap())
        //     .unwrap();

        // {
        //     let scope_ref = scope_rc.borrow();
        //     let label_id = scope_ref.resolve_label(&goto.name).unwrap();
        //     CIRStmt::Goto(CIRGotoStmt {
        //         label_id,
        //         loc: goto.loc,
        //     })
        // }
    }

    pub fn lower_export_tuple_to_vars(&mut self, export_tuple: &TypedExportTupleStmt) -> Vec<CIRVarStmt> {
        let mut vars = Vec::new();
        self.lower_export_pattern_recursive(&export_tuple.pattern, &mut vars);
        vars
    }

    fn lower_export_pattern_recursive(&mut self, pattern: &TypedExportPattern, vars: &mut Vec<CIRVarStmt>) {
        match pattern {
            TypedExportPattern::Ident(symbol_id) => {
                let var = &self.query.get_var(*symbol_id).unwrap().variable;

                let var_name = var.name.clone();
                let var_ty = self.lower_sema_ty(&var.ty.as_ref().unwrap());
                let var_rhs = self.lower_expr(&var.rhs.as_ref().unwrap());

                vars.push(CIRVarStmt {
                    irv_id: symbol_id.0,
                    name: format!("tuple.{}", var_name),
                    ty: var_ty,
                    expr: Some(var_rhs),
                    loc: var.loc,
                });
            }
            TypedExportPattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.lower_export_pattern_recursive(pattern, vars);
                }
            }
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

    fn lower_switch(&mut self, switch_stmt: &TypedSwitchStmt) -> CIRStmt {
        let operand = self.lower_expr(&switch_stmt.operand);
        let operand_type = switch_stmt.operand.sema_type.as_ref().unwrap().const_inner();

        let unnamed_enum_type_opt = operand_type
            .as_enum_symbol_id()
            .and_then(|symbol_id| {
                self.query
                    .get_enum(symbol_id)
                    .map(|resolved_enum| enum_sig_as_unnamed_enum_type(&resolved_enum.enum_sig, switch_stmt.loc))
            })
            .or(operand_type.as_generic_type().and_then(|generic_type| {
                Some(
                    self.query
                        .get_enum(generic_type.base)
                        .map(|resolved_enum| {
                            let enum_sig = substitute_enum_sig(
                                self.mapping_ctx_arena.clone(),
                                &resolved_enum.enum_sig,
                                generic_type.mapping_ctx.clone(),
                            )
                            .unwrap();

                            enum_sig_as_unnamed_enum_type(&enum_sig, switch_stmt.loc)
                        })
                        .unwrap(),
                )
            }))
            .or(operand_type.as_unnamed_enum());

        let default = switch_stmt
            .default_case
            .as_ref()
            .and_then(|default_case| Some(self.lower_body(&default_case)));

        if let Some(unnamed_enum_type) = &unnamed_enum_type_opt {
            if unnamed_enum_type.is_repr_c() || !unnamed_enum_type.includes_payload() {
                self.lower_switch_on_scalar_enum(&operand, &unnamed_enum_type, &default, switch_stmt)
            } else {
                self.lower_switch_on_enum(&unnamed_enum_type, &operand, &default, switch_stmt)
            }
        } else {
            if switch_stmt.includes_any_range() || !switch_stmt.includes_only_integer() {
                return self.lower_switch_as_chained_if(&operand, &default, switch_stmt);
            }

            self.lower_pure_switch(&operand, &default, switch_stmt)
        }
    }

    fn lower_switch_as_chained_if(
        &mut self,

        operand: &CIRExpr,
        default: &Option<CIRBlockStmt>,
        switch_stmt: &TypedSwitchStmt,
    ) -> CIRStmt {
        let mut current: Option<CIRIfStmt> = None;

        // build chain bottom-up
        for case in switch_stmt.cases.iter().rev() {
            let mut cond_expr = CIRExpr {
                kind: CIRExprKind::Literal(CIRLiteral {
                    kind: CIRLiteralKind::Bool(false),
                    ty: CIRTy::PlainType(PlainType::Bool),
                }),
                ty: CIRTy::PlainType(PlainType::Bool),
                loc: case.loc,
            };
            for pattern in &case.patterns {
                let new_cond = match &pattern {
                    TypedSwitchCasePattern::Expr(expr) => {
                        let lowered_case_expr = self.lower_expr(expr);

                        CIRExpr {
                            kind: CIRExprKind::Infix(CIRInfixExpr {
                                op: InfixOperator::Equal,
                                lhs: Box::new(operand.clone()),
                                rhs: Box::new(lowered_case_expr),
                            }),
                            ty: CIRTy::PlainType(PlainType::Bool),
                            loc: expr.loc,
                        }
                    }
                    TypedSwitchCasePattern::Range(range) => {
                        let lower = self.lower_expr(&range.lower);
                        let upper = self.lower_expr(&range.upper);

                        let ge = CIRExpr {
                            kind: CIRExprKind::Infix(CIRInfixExpr {
                                op: InfixOperator::GreaterEqual,
                                lhs: Box::new(operand.clone()),
                                rhs: Box::new(lower),
                            }),
                            ty: CIRTy::PlainType(PlainType::Bool),
                            loc: range.loc,
                        };

                        // lhs <= upper (inclusive)
                        // lhs < upper (exclusive)
                        let upper_op = if range.inclusive_upper {
                            InfixOperator::LessEqual
                        } else {
                            InfixOperator::LessThan
                        };

                        let upper_cmp = CIRExpr {
                            kind: CIRExprKind::Infix(CIRInfixExpr {
                                op: upper_op,
                                lhs: Box::new(operand.clone()),
                                rhs: Box::new(upper),
                            }),
                            ty: CIRTy::PlainType(PlainType::Bool),
                            loc: range.loc,
                        };

                        CIRExpr {
                            kind: CIRExprKind::Infix(CIRInfixExpr {
                                op: InfixOperator::And,
                                lhs: Box::new(ge),
                                rhs: Box::new(upper_cmp),
                            }),
                            ty: CIRTy::PlainType(PlainType::Bool),
                            loc: range.loc,
                        }
                    }
                    _ => unreachable!("Unexpected switch pattern for if-chain lowering"),
                };

                let loc = new_cond.loc;

                cond_expr = CIRExpr {
                    kind: CIRExprKind::Infix(CIRInfixExpr {
                        op: InfixOperator::Or,
                        lhs: Box::new(cond_expr),
                        rhs: Box::new(new_cond),
                    }),
                    ty: CIRTy::PlainType(PlainType::Bool),
                    loc,
                };
            }

            let then_block = Box::new(self.lower_body(&case.body));

            let new_if = CIRIfStmt {
                cond: cond_expr,
                then_block,
                else_block: match current {
                    Some(inner) => {
                        let loc = inner.loc;

                        Some(Box::new(CIRBlockStmt {
                            stmts: vec![CIRStmt::If(inner)],
                            defers: Vec::new(),
                            loc,
                        }))
                    }
                    None => None,
                },
                loc: switch_stmt.loc,
            };

            current = Some(new_if);
        }

        let root_if = match current {
            Some(mut if_stmt) => {
                if let Some(default_block) = default.clone() {
                    let loc = default_block.loc;

                    if_stmt.else_block = Some(Box::new(CIRBlockStmt {
                        stmts: vec![CIRStmt::Block(default_block)],
                        defers: Vec::new(),
                        loc,
                    }));
                }
                if_stmt
            }
            None => {
                return match default {
                    Some(block) => CIRStmt::Block(block.clone()),
                    None => unreachable!("Switch statement has no any case."),
                };
            }
        };

        CIRStmt::If(root_if)
    }

    fn lower_switch_on_scalar_enum(
        &mut self,

        operand: &CIRExpr,
        unnamed_enum_type: &TypedUnnamedEnumType,
        default: &Option<CIRBlockStmt>,
        switch_stmt: &TypedSwitchStmt,
    ) -> CIRStmt {
        let enum_ty = self.lower_unnamed_enum_type_as_cir_enum_ty(unnamed_enum_type);

        let cases: Vec<CIRSwitchCase> = switch_stmt
            .cases
            .iter()
            .map(|case| {
                let patterns: Vec<CIRExpr> = case
                    .patterns
                    .iter()
                    .map(|pattern| {
                        let (case_ident, case_loc) = match &pattern {
                            TypedSwitchCasePattern::Ident(ident) => (ident, ident.loc),
                            _ => unreachable!("Unexpected enum variant pattern when lowering switch on integer enum."),
                        };

                        let tag = enum_ty.compute_variant_tag(&case_ident.value).unwrap();
                        let tag_type = enum_ty.tag_type_or_infer_or_default();

                        CIRExpr {
                            kind: CIRExprKind::Literal(CIRLiteral {
                                kind: CIRLiteralKind::Integer(tag.try_into().unwrap(), tag_type.is_signed_integer()),
                                ty: *tag_type.clone(),
                            }),
                            ty: *tag_type,
                            loc: case_loc.clone(),
                        }
                    })
                    .collect();

                let body = self.lower_body(&case.body);

                CIRSwitchCase { patterns, body }
            })
            .collect();

        let explicit_all_cases_return = switch_stmt.cases.len() == enum_ty.variants.len();

        CIRStmt::Switch(CIRSwitchStmt {
            value: operand.clone(),
            cases,
            default: default.clone(),
            all_cases_covered: explicit_all_cases_return,
            loc: switch_stmt.loc,
        })
    }

    fn lower_pure_switch(
        &mut self,

        operand: &CIRExpr,
        default: &Option<CIRBlockStmt>,
        switch_stmt: &TypedSwitchStmt,
    ) -> CIRStmt {
        let cases: Vec<CIRSwitchCase> = switch_stmt
            .cases
            .iter()
            .map(|case| {
                let patterns: Vec<CIRExpr> = case
                    .patterns
                    .iter()
                    .map(|pattern| {
                        let case_expr = match &pattern {
                            TypedSwitchCasePattern::Expr(expr) => expr,
                            TypedSwitchCasePattern::Range(..) => {
                                unreachable!("Unexpected range when lowering pure switch.")
                            }
                            _ => unreachable!("Unexpected enum variant pattern when lowering pure switch."),
                        };

                        self.lower_expr(case_expr)
                    })
                    .collect();

                let body = self.lower_body(&case.body);

                CIRSwitchCase { patterns, body }
            })
            .collect();

        CIRStmt::Switch(CIRSwitchStmt {
            value: operand.clone(),
            cases,
            default: default.clone(),
            all_cases_covered: false,
            loc: switch_stmt.loc,
        })
    }

    fn lower_switch_on_enum(
        &mut self,

        unnamed_enum_type: &TypedUnnamedEnumType,
        operand: &CIRExpr,
        default: &Option<CIRBlockStmt>,
        switch_stmt: &TypedSwitchStmt,
    ) -> CIRStmt {
        let mut lowered_cases: Vec<CIRSwitchOnEnumCase> = Vec::new();

        for case in &switch_stmt.cases {
            let mut lowered_patterns: Vec<CIRSwitchOnEnumPattern> = Vec::new();

            for pattern in &case.patterns {
                match pattern {
                    TypedSwitchCasePattern::Ident(ident) => {
                        let variant_idx = unnamed_enum_type
                            .variants
                            .iter()
                            .position(|variant| variant.ident().value == ident.value)
                            .unwrap();

                        lowered_patterns.push(CIRSwitchOnEnumPattern::Ident(ident.as_string(), variant_idx));
                    }
                    TypedSwitchCasePattern::EnumVariant(ident, exported_fields, _) => {
                        let variant_idx = unnamed_enum_type
                            .variants
                            .iter()
                            .position(|variant| variant.ident().value == ident.value)
                            .unwrap();

                        let variant = &unnamed_enum_type.variants[variant_idx];

                        match variant {
                            TypedUnnamedEnumVariant::Valued(_, expr) => {
                                let exported_field = exported_fields.first().unwrap();
                                let lowered_expr = self.lower_expr(&expr);

                                lowered_patterns.push(CIRSwitchOnEnumPattern::Valued(
                                    ident.as_string(),
                                    variant_idx,
                                    (exported_field.clone(), lowered_expr),
                                ));
                            }
                            TypedUnnamedEnumVariant::Variant(_, valued_fields) => {
                                let exported_fields: Vec<(TypedIdent, CIRTy)> = exported_fields
                                    .iter()
                                    .enumerate()
                                    .map(|(i, ident)| {
                                        let field_ty = &valued_fields.get(i).as_ref().unwrap().ty;
                                        (ident.clone(), self.lower_sema_ty(field_ty))
                                    })
                                    .collect();

                                lowered_patterns.push(CIRSwitchOnEnumPattern::ExportFields(
                                    ident.as_string(),
                                    variant_idx,
                                    exported_fields,
                                ));
                            }
                            TypedUnnamedEnumVariant::Ident(_) => unreachable!(),
                        }
                    }
                    _ => unreachable!("Unexpected non-enum-variant pattern when lowering switch as switch_on_enum."),
                }
            }

            let body = self.lower_body(&case.body);
            lowered_cases.push(CIRSwitchOnEnumCase {
                patterns: lowered_patterns,
                body,
            });
        }

        CIRStmt::SwitchOnEnum(CIRSwitchOnEnumStmt {
            value: operand.clone(),
            cases: lowered_cases,
            default: default.clone(),
            loc: switch_stmt.loc,
        })
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
        CIRStmt::Break(break_stmt.loc)
    }

    fn lower_continue(&self, continue_stmt: &TypedContinueStmt) -> CIRStmt {
        CIRStmt::Continue(continue_stmt.loc)
    }

    fn lower_return(&mut self, return_stmt: &TypedReturnStmt) -> CIRStmt {
        let arg = return_stmt.arg.clone().and_then(|arg| Some(self.lower_expr(&arg)));

        CIRStmt::Return(CIRReturnStmt {
            arg,
            loc: return_stmt.loc,
        })
    }

    fn lower_global_var(&mut self, global_var: &mut TypedGlobalVarStmt) -> CIRStmt {
        let ty = global_var
            .ty
            .as_ref()
            .or_else(|| global_var.expr.as_ref().and_then(|expr| expr.sema_type.as_ref()))
            .map(|sema_type| self.lower_sema_ty(sema_type))
            .unwrap_or_else(|| {
                panic!(
                    "Global var '{}' has neither explicit type nor valid initializer type.",
                    global_var.name
                )
            });

        let expr = global_var.expr.clone().and_then(|expr| Some(self.lower_expr(&expr)));

        let mangled_name = mangle_global_var(&global_var.modifiers, &self.module_name, &global_var.name);

        CIRStmt::GlobalVar(CIRGlobalVarStmt {
            irv_id: global_var.symbol_id.0,
            name: mangled_name,
            ty,
            expr,
            modifiers: global_var.modifiers.clone(),
            loc: global_var.loc,
        })
    }

    fn lower_global_var_sig(&mut self, irv_id: IRValueID, global_var_sig: &GlobalVarSig) -> CIRGlobalVarStmt {
        let ty = global_var_sig
            .ty
            .as_ref()
            .or_else(|| global_var_sig.rhs.as_ref().and_then(|expr| expr.sema_type.as_ref()))
            .map(|sema_type| self.lower_sema_ty(sema_type))
            .unwrap_or_else(|| {
                panic!(
                    "Global var '{}' has neither explicit type nor valid initializer type.",
                    global_var_sig.name
                )
            });

        CIRGlobalVarStmt {
            irv_id,
            name: global_var_sig.name.clone(),
            ty,
            expr: None,
            modifiers: global_var_sig.modifiers.clone(),
            loc: global_var_sig.loc,
        }
    }

    fn lower_var(&mut self, var: &TypedVarStmt) -> CIRVarStmt {
        let ty = var
            .ty
            .as_ref()
            .or_else(|| var.rhs.as_ref().and_then(|rhs| rhs.sema_type.as_ref()))
            .map(|ty| self.lower_sema_ty(ty))
            .unwrap_or_else(|| {
                panic!(
                    "Variable '{}' has neither explicit type nor RHS type ({}:{})",
                    var.name, var.loc.line, var.loc.column
                )
            });

        let expr = var.rhs.clone().and_then(|expr| Some(self.lower_expr(&expr)));

        CIRVarStmt {
            irv_id: var.symbol_id.0,
            name: var.name.clone(),
            ty,
            expr,
            loc: var.loc,
        }
    }

    fn lower_func_type_params(&mut self, func_type_params: &TypedFuncTypeParams) -> Vec<CIRTy> {
        func_type_params
            .list
            .iter()
            .map(|sema_type| self.lower_sema_ty(sema_type))
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
                            let irv_id = {
                                if !is_decl {
                                    Some(func_param.symbol_id.unwrap().0)
                                } else {
                                    None
                                }
                            };

                            CIRFuncParam {
                                name,
                                irv_id,
                                ty: self.lower_sema_ty(&func_param.ty),
                                loc,
                            }
                        }
                        TypedFuncParamKind::SelfModifier(self_modifier) => CIRFuncParam {
                            name,
                            irv_id: Some(self_modifier.self_id.unwrap().0),
                            ty: self.lower_sema_ty(&self_modifier.ty.as_ref().unwrap()),
                            loc,
                        },
                    }
                })
                .collect(),
            is_var: func_params.variadic.is_some(),
        }
    }

    fn lower_func_def(&mut self, func_def: &TypedFuncDefStmt, mangle_name: bool) -> CIRStmt {
        let params = self.lower_func_params(&func_def.params, false);

        let body = self.lower_body(&func_def.body);
        let ret = self.lower_sema_ty(&func_def.ret_type);

        let mut cir_func_def = CIRFuncDefStmt {
            irv_id: func_def.symbol_id.0,
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

    fn lower_func_decl(&mut self, func_decl: &TypedFuncDeclStmt, mangle_name: bool) -> CIRFuncDeclStmt {
        let params = self.lower_func_params(&func_decl.params, true);
        let ret = self.lower_sema_ty(&func_decl.ret_type);

        let mut func_name = func_decl.renamed_as.as_ref().unwrap_or(&func_decl.name).clone();

        if mangle_name {
            func_name = mangle_func(&func_decl.modifiers, &self.module_name, &func_name);
        }

        let mut cir_func_decl = CIRFuncDeclStmt {
            irv_id: func_decl.symbol_id.0,
            name: func_name,
            params,
            ret,
            modifiers: func_decl.modifiers.clone(),
            abi_func_info: None,
            loc: func_decl.loc,
        };

        let cir_func_type = cir_func_decl_as_func_ty(&cir_func_decl);
        cir_func_decl.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());
        cir_func_decl
    }

    pub(crate) fn lower_func_sig(&mut self, irv_id: IRValueID, func_sig: &FuncSig) -> CIRFuncDeclStmt {
        let params = self.lower_func_params(&func_sig.params, func_sig.is_func_decl);
        let ret = self.lower_sema_ty(&func_sig.ret_type);

        let mut cir_func_decl = CIRFuncDeclStmt {
            irv_id,
            name: func_sig.name.clone(),
            params,
            ret,
            modifiers: func_sig.modifiers.clone(),
            abi_func_info: None,
            loc: func_sig.loc,
        };

        let cir_func_type = cir_func_decl_as_func_ty(&cir_func_decl);
        cir_func_decl.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());
        cir_func_decl
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

        let ty = self.lower_sema_ty(&expr.sema_type.clone().unwrap());

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
            TypedExprKind::UnnamedStructValue(unnamed_struct_value) => {
                self.lower_unnamed_struct_value(unnamed_struct_value)
            }
            TypedExprKind::UnnamedEnumValue(unnamed_enum_value) => self.lower_unnamed_enum_value(unnamed_enum_value),
            TypedExprKind::UnnamedUnionValue(unnamed_union_value) => {
                self.lower_unnamed_union_value(unnamed_union_value)
            }
            TypedExprKind::FuncCall(func_call) => self.lower_func_call(func_call),
            TypedExprKind::MethodCall(method_call) => self.lower_method_call(method_call),
            TypedExprKind::FieldAccess(field_access) => self.lower_field_access(field_access.clone()),
            TypedExprKind::StructInit(struct_init_expr) => self.lower_struct_init(struct_init_expr),
            TypedExprKind::Lambda(lambda_expr) => self.lower_lambda(lambda_expr),
            TypedExprKind::Tuple(tuple_expr) => self.lower_tuple(tuple_expr),
            TypedExprKind::TupleAccess(tuple_access_expr) => self.lower_tuple_access(tuple_access_expr),
            TypedExprKind::Dynamic(dynamic) => self.lower_dynamic_expr(dynamic),
            TypedExprKind::SemanticType(..) => unreachable!(),

            TypedExprKind::Builtin(_builtin) => todo!(),
        };

        CIRExpr {
            kind,
            ty,
            loc: expr.loc,
        }
    }

    fn lower_unnamed_union_value(&mut self, unnamed_union_value: &TypedUnnamedUnionValue) -> CIRExprKind {
        let ty = CIRTy::Union(
            self.lower_unnamed_union_type_as_cir_union_ty(&unnamed_union_value.union_ty.as_ref().unwrap()),
        );

        let expr = self.lower_expr(&unnamed_union_value.field_value);

        CIRExprKind::UnionInit(CIRUnionInitExpr {
            expr: Box::new(expr),
            ty,
        })
    }

    fn lower_unnamed_enum_value(&mut self, unnamed_enum_value: &TypedUnnamedEnumValue) -> CIRExprKind {
        let unnamed_enum_value_type = unnamed_enum_value.enum_ty.clone().unwrap();

        match unnamed_enum_value_type {
            TypedUnnamedEnumValueTy::EnumSig(enum_sig) => {
                let enum_ty = self.lower_enum_sig_as_enum_ty(&enum_sig);

                let tag = enum_ty.compute_variant_tag(&unnamed_enum_value.ident.value).unwrap();

                let variant_idx = enum_sig
                    .variants
                    .iter()
                    .position(|variant| *variant.ident() == unnamed_enum_value.ident)
                    .unwrap();

                let variant = enum_sig.variants.get(variant_idx).unwrap();

                let cir_enum_variant = match variant {
                    TypedEnumVariant::Ident(_) => CIREnumInitVariant::Ident,
                    TypedEnumVariant::Valued(_, value) => {
                        let expr = self.lower_expr(value);
                        CIREnumInitVariant::Valued(Box::new(expr))
                    }
                    TypedEnumVariant::Variant(_, _) => {
                        let values = unnamed_enum_value.kind.as_fielded().unwrap();
                        let exprs = values.iter().map(|expr| self.lower_expr(expr)).collect();
                        CIREnumInitVariant::Fielded(exprs)
                    }
                };

                CIRExprKind::EnumInit(CIREnumInitExpr {
                    tag: tag.try_into().unwrap(),
                    variant: cir_enum_variant,
                    enum_ty,
                })
            }
            TypedUnnamedEnumValueTy::UnnamedEnum(unnamed_enum_type) => {
                let enum_ty = self.lower_unnamed_enum_type_as_cir_enum_ty(&unnamed_enum_type);

                let tag = enum_ty.compute_variant_tag(&unnamed_enum_value.ident.value).unwrap();

                let variant_idx = unnamed_enum_type
                    .variants
                    .iter()
                    .position(|variant| *variant.ident() == unnamed_enum_value.ident)
                    .unwrap();

                let variant = unnamed_enum_type.variants.get(variant_idx).unwrap();

                let cir_enum_variant = match variant {
                    TypedUnnamedEnumVariant::Ident(_) => CIREnumInitVariant::Ident,
                    TypedUnnamedEnumVariant::Valued(_, value) => {
                        let expr = self.lower_expr(value);
                        CIREnumInitVariant::Valued(Box::new(expr))
                    }
                    TypedUnnamedEnumVariant::Variant(_, _) => {
                        let values = unnamed_enum_value.kind.as_fielded().unwrap();
                        let exprs = values.iter().map(|expr| self.lower_expr(expr)).collect();
                        CIREnumInitVariant::Fielded(exprs)
                    }
                };

                CIRExprKind::EnumInit(CIREnumInitExpr {
                    tag: tag.try_into().unwrap(),
                    variant: cir_enum_variant,
                    enum_ty,
                })
            }
        }
    }

    // FIXME
    fn lower_dynamic_expr(&mut self, dynamic: &TypedDynamicExpr) -> CIRExprKind {
        todo!();

        // let operand = self.lower_expr(&dynamic.operand);
        // let data_ptr = CIRExpr {
        //     kind: CIRExprKind::AddrOf(CIRAddrOfExpr {
        //         operand: Box::new(operand),
        //     }),
        //     ty: CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))),
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
        //     .map(|mut func_sig| {
        //         let mangled_name = mangle_method(
        //             &self.query.lookup_module_name(func_sig.module_id).unwrap(),
        //             &dynamic.object_name.as_ref().unwrap(),
        //             &func_sig.name,
        //         );

        //         func_sig.name = mangled_name;
        //         self.lower_func_sig(func_sig.symbol_id.unwrap().0, &func_sig)
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
    pub(crate) fn lower_load_symbol(&mut self, symbol_id: SymbolID) -> CIRExprKind {
        todo!();

        // let symbol_entry = self.query.lookup_global_symbol(symbol_id).unwrap();

        // if let Some(resolved_func) = symbol_entry.as_func() {
        //     let mut cir_func_decl = self.lower_func_sig(resolved_func.symbol_id.0, &resolved_func.func_sig);

        //     let cir_func_type = cir_func_decl_as_func_ty(&cir_func_decl);
        //     cir_func_decl.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());

        //     let mangled_name = mangle_func(
        //         &cir_func_decl.modifiers,
        //         &self.query.lookup_module_name(resolved_func.module_id).unwrap(),
        //         &resolved_func.func_sig.name,
        //     );

        //     cir_func_decl.name = mangled_name;

        //     CIRExprKind::Load(CIRValue {
        //         irv_id: resolved_func.symbol_id.0,
        //         kind: CIRValueKind::Func(Box::new(cir_func_decl)),
        //     })
        // } else if let Some(resolved_global_var) = symbol_entry.as_global_var() {
        //     let mut global_var_stmt =
        //         self.lower_global_var_sig(resolved_global_var.symbol_id.0, &resolved_global_var.global_var_sig);

        //     if global_var_stmt.modifiers.vis.is_public() {
        //         if global_var_stmt.modifiers.linkage.is_none() {
        //             global_var_stmt.modifiers.linkage = Some(Linkage::Extern(None));
        //         }
        //     }

        //     let mangled_name = mangle_global_var(
        //         &resolved_global_var.global_var_sig.modifiers,
        //         &self.query.lookup_module_name(resolved_global_var.module_id).unwrap(),
        //         &resolved_global_var.global_var_sig.name,
        //     );

        //     global_var_stmt.name = mangled_name;

        //     CIRExprKind::Load(CIRValue {
        //         irv_id: resolved_global_var.symbol_id.0,
        //         kind: CIRValueKind::GlobalVar(Box::new(global_var_stmt)),
        //     })
        // } else if let Some(resolved_variable) = symbol_entry.as_var() {
        //     CIRExprKind::Load(CIRValue {
        //         irv_id: resolved_variable.symbol_id.0,
        //         kind: CIRValueKind::LocalVariable,
        //     })
        // } else {
        //     unreachable!("Unexpected symbol kind when lowering load symbol.")
        // }
    }

    pub(crate) fn lower_struct_init(&mut self, struct_init_expr: &TypedStructInitExpr) -> CIRExprKind {
        let symbol_entry = self.query.get_symbol(struct_init_expr.symbol_id).unwrap();

        if let Some(resolved_struct) = symbol_entry.as_struct() {
            let fields = struct_init_expr
                .fields
                .iter()
                .map(|field| self.lower_sema_ty(&field.value.sema_type.clone().unwrap()))
                .collect();

            let fields_info = struct_init_expr
                .fields
                .iter()
                .map(|field| (field.name.clone(), field.loc))
                .collect();

            let struct_ty = CIRStructTy {
                name: Some(resolved_struct.struct_sig.name.clone()),
                repr_attr: resolved_struct.struct_sig.modifiers.repr_attr.clone(),
                align: resolved_struct.struct_sig.align.clone(),
                fields,
                fields_info,
                loc: resolved_struct.struct_sig.loc,
            };

            let fields: Vec<CIRExpr> = struct_init_expr
                .fields
                .iter()
                .map(|field| self.lower_expr(&field.value))
                .collect();

            CIRExprKind::StructInit(CIRStructInitExpr { ty: struct_ty, fields })
        } else if let Some(resolved_union) = symbol_entry.as_union() {
            let fields = struct_init_expr
                .fields
                .iter()
                .map(|field| self.lower_sema_ty(&field.value.sema_type.clone().unwrap()))
                .collect();

            let fields_info = struct_init_expr
                .fields
                .iter()
                .map(|field| (field.name.clone(), field.loc))
                .collect();

            let union_ty = CIRTy::Union(CIRUnionTy {
                name: Some(resolved_union.union_sig.name.clone()),
                fields,
                fields_info,
                repr_attr: resolved_union.union_sig.modifiers.repr_attr.clone(),
                align: resolved_union.union_sig.align.clone(),
                loc: resolved_union.union_sig.loc,
            });

            let struct_field_init = struct_init_expr.fields.first().unwrap();
            let expr = Box::new(self.lower_expr(&struct_field_init.value));

            CIRExprKind::UnionInit(CIRUnionInitExpr { expr, ty: union_ty })
        } else {
            unreachable!()
        }
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
        let ret = self.lower_sema_ty(&lambda.ret_type);

        let cir_func_type = CIRFuncTy {
            params: params.list.iter().map(|param| param.ty.clone()).collect(),
            is_var: params.is_var,
            ret: Box::new(ret.clone()),
            callconv: CallConv::default(),
            abi_func_info: None,
        };

        let abi_func_info = self.target.target_abi.classify_func(&cir_func_type).unwrap();

        let irv_id = self.lambda_id;
        self.lambda_id += 1;

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

    fn lower_interface_method_call(&mut self, method_call: &TypedMethodCall) -> CIRExprKind {
        let operand = self.lower_expr(&method_call.operand);

        let args = method_call
            .args
            .iter()
            .map(|arg| self.lower_expr(arg))
            .collect::<Vec<CIRExpr>>();

        let ret_ty = self.lower_sema_ty(&method_call.func_sig.as_ref().unwrap().ret_type.clone());

        let metadata = method_call.method_call_on_interface.as_ref().unwrap();

        let cir_func_decl = self.lower_func_sig(metadata.method_sig.symbol_id.unwrap().0, &metadata.method_sig);

        CIRExprKind::InterfaceMethodCall(CIRInterfaceMethodCall {
            operand: Box::new(operand),
            args,
            ret_ty,
            func_type: cir_func_decl_as_func_ty(&cir_func_decl),
            method_idx: metadata.method_idx,
            methods_len: metadata.methods_len,
        })
    }

    // FIXME
    fn lower_regular_method_call(&mut self, method_call: &TypedMethodCall) -> CIRExprKind {
        todo!();

        // if method_call.method_call_on_interface.is_some() {
        //     return self.lower_interface_method_call(method_call);
        // }

        // self.current_obj_operand_ty = Some(method_call.operand.sema_type.clone().unwrap());
        // self.current_self_ty = method_call
        //     .self_ty
        //     .clone()
        //     .map(|sema_type| self.lower_sema_ty(&sema_type));

        // let mut func_sig = method_call.func_sig.as_ref().unwrap().clone();

        // let mangled_name = mangle_method(
        //     &self.query.lookup_module_name(func_sig.module_id).unwrap(),
        //     &method_call.object_name.as_ref().unwrap(),
        //     &func_sig.name,
        // );

        // func_sig.name = mangled_name;

        // let args = method_call
        //     .args
        //     .iter()
        //     .map(|arg| self.lower_expr(arg))
        //     .collect::<Vec<CIRExpr>>();

        // let ret_ty = self.lower_sema_ty(&func_sig.ret_type.clone());

        // let cir_expr_kind;
        // if let Some(monomorph_id) = method_call.monomorph_id {
        //     self.insert_monomorph_func_instance(monomorph_id, &func_sig);

        //     cir_expr_kind = CIRExprKind::MonomorphFuncInstanceCall(CIRMonomorphFuncInstanceCall {
        //         monomorph_id,
        //         args,
        //         ret_ty,
        //     })
        // } else {
        //     let func_decl = typed_func_decl_from_func_sig(&func_sig);
        //     let cir_func_decl = self.lower_func_decl(&func_decl, false);

        //     let operand = Box::new(CIRExpr {
        //         kind: CIRExprKind::Load(CIRValue {
        //             irv_id: method_call.func_sig.as_ref().unwrap().symbol_id.unwrap().0,
        //             kind: CIRValueKind::Func(Box::new(cir_func_decl)),
        //         }),
        //         ty: ret_ty.clone(),
        //         loc: func_decl.loc,
        //     });

        //     cir_expr_kind = CIRExprKind::FuncCall(CIRFuncCall { operand, args, ret_ty })
        // }

        // self.current_self_ty = None;
        // cir_expr_kind
    }

    fn lower_enum_init(&mut self, enum_sig: &EnumSig, method_call: &TypedMethodCall) -> CIRExprKind {
        let sema_type = method_call.operand.sema_type.as_ref().unwrap();

        let variant_idx_opt = enum_sig
            .variants
            .iter()
            .position(|variant| variant.ident().as_string() == method_call.method_name);

        // it's not a enum variant construction, so try again as a regular method call
        let variant_idx = if let Some(i) = variant_idx_opt {
            i
        } else {
            return self.lower_regular_method_call(method_call);
        };

        let typed_variant = enum_sig.variants.get(variant_idx).unwrap();

        let variant: CIREnumInitVariant;
        let enum_ty: CIREnumTy;
        if let Some(generic_type) = sema_type.as_generic_type() {
            let enum_sig = substitute_enum_sig(
                self.mapping_ctx_arena.clone(),
                &enum_sig,
                generic_type.mapping_ctx.clone(),
            )
            .unwrap();

            enum_ty = self.lower_enum_sig_as_enum_ty(&enum_sig);
        } else {
            enum_ty = self.lower_enum_sig_as_enum_ty(&enum_sig);
        }

        let tag = enum_ty.compute_variant_tag(&method_call.method_name).unwrap();

        variant = match typed_variant {
            TypedEnumVariant::Variant(..) => {
                let values: Vec<CIRExpr> = method_call.args.iter().map(|arg| self.lower_expr(arg)).collect();
                CIREnumInitVariant::Fielded(values)
            }
            _ => unreachable!(),
        };

        return CIRExprKind::EnumInit(CIREnumInitExpr {
            tag: tag.try_into().unwrap(),
            variant,
            enum_ty,
        });
    }

    fn lower_method_call(&mut self, method_call: &TypedMethodCall) -> CIRExprKind {
        if let Some(enum_id) = method_call.enum_constructor {
            let resolved_enum = self.query.get_enum(enum_id).unwrap();

            return self.lower_enum_init(&resolved_enum.enum_sig, method_call);
        }

        self.lower_regular_method_call(method_call)
    }

    fn lower_field_access(&mut self, mut field_access: TypedFieldAccess) -> CIRExprKind {
        if field_access.is_fat_arrow {
            field_access.operand = Box::new(TypedExprStmt {
                kind: TypedExprKind::Deref(TypedDerefExpr {
                    operand: field_access.operand.clone(),
                    loc: field_access.loc,
                }),
                sema_type: Some(field_access.operand.sema_type.clone().unwrap().pointer_inner().clone()),
                mloc: MemoryLocation::LValue,
                loc: field_access.loc,
            })
        }

        if let Some(sema_type) = &field_access.operand.sema_type {
            if sema_type.as_unnamed_struct().is_some() {
                return CIRExprKind::StructFieldAccess(CIRStructFieldAccessExpr {
                    operand: Box::new(self.lower_expr(&field_access.operand)),
                    field_idx: field_access.field_index.unwrap(),
                    field_ty: self.lower_sema_ty(&field_access.field_ty.as_ref().unwrap()),
                });
            }

            if sema_type.as_unnamed_union().is_some() {
                return CIRExprKind::UnionFieldAccess(CIRUnionFieldAccessExpr {
                    operand: Box::new(self.lower_expr(&field_access.operand)),
                    field_ty: self.lower_sema_ty(&field_access.field_ty.as_ref().unwrap()),
                });
            }
        }

        let symbol_entry = self
            .query
            .get_symbol(field_access.object_symbol_id.unwrap())
            .unwrap();

        if symbol_entry.as_struct().is_some() {
            CIRExprKind::StructFieldAccess(CIRStructFieldAccessExpr {
                operand: Box::new(self.lower_expr(&field_access.operand)),
                field_idx: field_access.field_index.unwrap(),
                field_ty: self.lower_sema_ty(&field_access.field_ty.as_ref().unwrap()),
            })
        } else if symbol_entry.as_union().is_some() {
            CIRExprKind::UnionFieldAccess(CIRUnionFieldAccessExpr {
                operand: Box::new(self.lower_expr(&field_access.operand)),
                field_ty: self.lower_sema_ty(&field_access.field_ty.as_ref().unwrap()),
            })
        } else if let Some(mut resolved_enum) = symbol_entry.as_enum().cloned() {
            let sema_type = field_access.operand.sema_type.as_ref().unwrap();

            let variant: CIREnumInitVariant;
            let enum_ty: CIREnumTy;
            if let Some(generic_type) = sema_type.as_generic_type() {
                resolved_enum.enum_sig = substitute_enum_sig(
                    self.mapping_ctx_arena.clone(),
                    &resolved_enum.enum_sig,
                    generic_type.mapping_ctx.clone(),
                )
                .unwrap()
            }

            let typed_variant = resolved_enum
                .enum_sig
                .variants
                .get(field_access.field_index.unwrap())
                .unwrap();

            variant = match typed_variant {
                TypedEnumVariant::Ident(..) => CIREnumInitVariant::Ident,
                TypedEnumVariant::Valued(_, expr) => CIREnumInitVariant::Valued(Box::new(self.lower_expr(expr))),
                TypedEnumVariant::Variant(..) => unreachable!(),
            };

            enum_ty = self.lower_enum_sig_as_enum_ty(&resolved_enum.enum_sig);

            let tag = enum_ty.compute_variant_tag(&field_access.field_name).unwrap();

            CIRExprKind::EnumInit(CIREnumInitExpr {
                tag: tag.try_into().unwrap(),
                variant,
                enum_ty,
            })
        } else {
            unreachable!()
        }
    }

    fn lower_func_call(&mut self, func_call: &TypedFuncCall) -> CIRExprKind {
        let args: Vec<CIRExpr> = func_call.args.iter().map(|arg| self.lower_expr(arg)).collect();

        let ret_ty = self.lower_sema_ty(&func_call.ret_type.clone().unwrap());

        if let Some(monomorph_id) = func_call.monomorph_id {
            let monomorph_func_entry = self.query.lookup_monomorph_func(monomorph_id).unwrap();

            let symbol_entry = self
                .query
                .get_symbol(monomorph_func_entry.base_symbol)
                .unwrap();

            let mut func_sig = symbol_entry
                .as_func()
                .map(|resolved_func| resolved_func.func_sig.clone())
                .or(symbol_entry
                    .as_method()
                    .map(|resolved_method| resolved_method.func_sig.clone()))
                .expect("Monomorphizaton not supported for the symbol.");

            func_sig = substitute_func_sig(
                self.mapping_ctx_arena.clone(),
                &func_sig,
                Rc::new(RefCell::new(monomorph_func_entry.mapping_ctx.clone())),
            )
            .unwrap();

            self.insert_monomorph_func_instance(monomorph_id, &func_sig);

            CIRExprKind::MonomorphFuncInstanceCall(CIRMonomorphFuncInstanceCall {
                monomorph_id,
                args,
                ret_ty,
            })
        } else {
            let operand = Box::new(self.lower_expr(&func_call.operand));
            CIRExprKind::FuncCall(CIRFuncCall { operand, args, ret_ty })
        }
    }

    fn lower_unnamed_struct_value(&mut self, unnamed_struct_value: &TypedUnnamedStructValue) -> CIRExprKind {
        let unnamed_struct_type = unnamed_struct_value.as_unnamed_struct_type();

        let fields: Vec<CIRExpr> = unnamed_struct_value
            .fields
            .iter()
            .map(|field| self.lower_expr(&field.field_value))
            .collect();

        let field_types = unnamed_struct_type
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(&field.ty))
            .collect();

        let fields_info = unnamed_struct_type
            .fields
            .iter()
            .map(|field| (field.name.clone(), field.loc))
            .collect();

        let struct_ty = CIRStructTy {
            name: None,
            fields: field_types,
            fields_info,
            repr_attr: unnamed_struct_value.repr_attr.clone(),
            align: unnamed_struct_value.align.clone(),
            loc: unnamed_struct_type.loc,
        };

        CIRExprKind::StructInit(CIRStructInitExpr { ty: struct_ty, fields })
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
            ty: self.lower_sema_ty(&array.ty.as_ref().unwrap()),
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
                let is_signed = literal.ty.clone().unwrap().as_basic_type().unwrap().is_signed();
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

        let ty = self.lower_sema_ty(&literal.ty.clone().unwrap());
        CIRExprKind::Literal(CIRLiteral { kind, ty })
    }

    fn lower_sema_ty(&mut self, sema_type: &SemanticType) -> CIRTy {
        match sema_type {
            SemanticType::ResolvedSymbol(resolved_symbol) => self.lower_resolved_symbol(resolved_symbol),
            SemanticType::PlainType(basic_type) => CIRTy::PlainType(basic_type.clone()),
            SemanticType::Array(array_type) => {
                let ty = self.lower_sema_ty(&array_type.element_type);
                let len = match &array_type.capacity {
                    TypedArrayCapacity::Fixed(expr) => expr.literal_const_int_value().unwrap(),
                    TypedArrayCapacity::Dynamic => todo!(),
                };

                CIRTy::Array(CIRArrayTy {
                    ty: Box::new(ty),
                    len: len.try_into().unwrap(),
                })
            }
            SemanticType::Const(sema_type) => CIRTy::Const(Box::new(self.lower_sema_ty(&*sema_type))),
            SemanticType::Pointer(sema_type) => CIRTy::Pointer(Box::new(self.lower_sema_ty(&*sema_type))),
            SemanticType::UnnamedStruct(unnamed_struct_type) => {
                let fields = unnamed_struct_type
                    .fields
                    .iter()
                    .map(|field| self.lower_sema_ty(&field.ty))
                    .collect();

                let fields_info = unnamed_struct_type
                    .fields
                    .iter()
                    .map(|field| (field.name.clone(), field.loc))
                    .collect();

                CIRTy::Struct(CIRStructTy {
                    name: None,
                    fields,
                    fields_info,
                    repr_attr: unnamed_struct_type.repr_attr.clone(),
                    align: unnamed_struct_type.align.clone(),
                    loc: unnamed_struct_type.loc,
                })
            }
            SemanticType::FuncType(func_type) => {
                let ret = Box::new(self.lower_sema_ty(&func_type.ret_type));
                let params = self.lower_func_type_params(&func_type.params);

                let mut cir_type = CIRFuncTy {
                    params: params,
                    is_var: func_type.params.variadic.is_some(),
                    ret,
                    callconv: CallConv::default(),
                    abi_func_info: None,
                };

                cir_type.abi_func_info = Some(self.target.target_abi.classify_func(&cir_type).unwrap());
                CIRTy::FuncType(cir_type)
            }
            SemanticType::Tuple(tuple_type) => {
                let elements: Vec<CIRTy> = tuple_type
                    .elements
                    .iter()
                    .map(|sema_type| self.lower_sema_ty(sema_type))
                    .collect();

                CIRTy::Tuple(CIRTupleTy {
                    elements,
                    loc: tuple_type.loc,
                })
            }
            SemanticType::GenericType(generic_type) => self.lower_generic_type(generic_type.clone()),
            SemanticType::UnresolvedSymbol(_) => unreachable!("Unexpected unresolved symbol."),
            SemanticType::SelfType(_) => {
                if let Some(cir_ty) = &self.current_self_ty {
                    cir_ty.clone()
                } else {
                    unreachable!("Unexpected self type which is not resolved.")
                }
            }
            SemanticType::GenericParam(generic_param) => {
                if let Some(sema_type) = self.current_obj_operand_ty.clone() {
                    if let Some(generic_type) = sema_type.as_generic_type() {
                        {
                            let mapping_ctx = generic_type.mapping_ctx.borrow();

                            let cir_ty = mapping_ctx
                                .resolve_with_name(self.mapping_ctx_arena.clone(), &generic_param.name.value)
                                .map(|sema_type| self.lower_sema_ty(&sema_type))
                                .unwrap();

                            return cir_ty;
                        }
                    }
                }

                unreachable!("Unexpected generic param which is not resolved: {:#?}", generic_param)
            }
            SemanticType::DynamicType(dynamic_type) => CIRTy::Dynamic(CIRDynamicTy {
                vtable_id: dynamic_type.vtable_id,
            }),
            SemanticType::Interface(interface_type) => CIRTy::Struct(CIRStructTy {
                name: None,
                fields: vec![
                    CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))),
                    CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))),
                ],
                fields_info: vec![
                    ("data_ptr".to_string(), interface_type.loc),
                    ("vtable_ptr".to_string(), interface_type.loc),
                ],
                repr_attr: None,
                align: None,
                loc: interface_type.loc,
            }),
            SemanticType::UnnamedUnion(unnamed_union_type) => {
                CIRTy::Union(self.lower_unnamed_union_type_as_cir_union_ty(unnamed_union_type))
            }
            SemanticType::UnnamedEnum(unnamed_enum_type) => {
                CIRTy::Enum(self.lower_unnamed_enum_type_as_cir_enum_ty(unnamed_enum_type))
            }
        }
        .const_inner()
        .clone()
    }

    fn lower_unnamed_union_type_as_cir_union_ty(&mut self, unnamed_union_type: &TypedUnnamedUnionType) -> CIRUnionTy {
        let fields = unnamed_union_type
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(&field.ty))
            .collect();

        let fields_info = unnamed_union_type
            .fields
            .iter()
            .map(|field| (field.name.clone(), field.loc))
            .collect();

        CIRUnionTy {
            name: None,
            fields,
            fields_info,
            repr_attr: unnamed_union_type.repr_attr.clone(),
            align: unnamed_union_type.align.clone(),
            loc: unnamed_union_type.loc,
        }
    }

    fn lower_unnamed_enum_type_as_cir_enum_ty(&mut self, unnamed_enum_type: &TypedUnnamedEnumType) -> CIREnumTy {
        let variants: Vec<CIREnumTyVariant> = unnamed_enum_type
            .variants
            .iter()
            .map(|variant| self.lower_unnamed_enum_ty_variant(variant))
            .collect();

        let tag_type = unnamed_enum_type
            .tag_type
            .clone()
            .map(|sema_type| Box::new(self.lower_sema_ty(&sema_type)));

        CIREnumTy {
            name: None,
            variants,
            tag_type,
            repr_attr: unnamed_enum_type.repr_attr.clone(),
            align: unnamed_enum_type.align.clone(),
            loc: unnamed_enum_type.loc,
        }
    }

    fn lower_unnamed_enum_ty_variant(&mut self, variant: &TypedUnnamedEnumVariant) -> CIREnumTyVariant {
        match variant {
            TypedUnnamedEnumVariant::Ident(ident) => CIREnumTyVariant::Ident(ident.as_string()),
            TypedUnnamedEnumVariant::Valued(ident, expr) => {
                CIREnumTyVariant::Valued(ident.as_string(), Box::new(self.lower_expr(expr)))
            }
            TypedUnnamedEnumVariant::Variant(ident, fields) => {
                let fields: Vec<CIRTy> = fields.iter().map(|field| self.lower_sema_ty(&field.ty)).collect();
                CIREnumTyVariant::Fielded(ident.as_string(), fields)
            }
        }
    }

    fn lower_generic_type(&mut self, mut generic_type: GenericType) -> CIRTy {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let symbol_entry = self.query.get_symbol(generic_type.base).unwrap();

        if let Err(err) = generic_type.init(self.mapping_ctx_arena.clone(), fmt_symbol) {
            eprintln!("Failed to init generic type: {:?}.", err.kind.to_string())
        }

        if let Some(resolved_struct) = symbol_entry.as_struct() {
            let struct_sig = substitute_struct_sig(
                self.mapping_ctx_arena.clone(),
                &resolved_struct.struct_sig,
                generic_type.mapping_ctx,
            )
            .unwrap();

            let cir_struct_ty = self.lower_struct_sig_as_struct_ty(&struct_sig);
            CIRTy::Struct(cir_struct_ty)
        } else if let Some(resolved_enum) = symbol_entry.as_enum() {
            let enum_sig = substitute_enum_sig(
                self.mapping_ctx_arena.clone(),
                &resolved_enum.enum_sig,
                generic_type.mapping_ctx,
            )
            .unwrap();

            let cir_enum_ty = self.lower_enum_sig_as_enum_ty(&enum_sig);
            CIRTy::Enum(cir_enum_ty)
        } else if let Some(resolved_union) = symbol_entry.as_union() {
            let union_sig = substitute_union_sig(
                self.mapping_ctx_arena.clone(),
                &resolved_union.union_sig,
                generic_type.mapping_ctx,
            )
            .unwrap();

            let cir_union_ty = self.lower_union_sig_as_union_ty(&union_sig);
            CIRTy::Union(cir_union_ty)
        } else {
            unreachable!("Object does not support generic type at CIR walk.")
        }
    }

    fn lower_struct_sig_as_struct_ty(&mut self, struct_sig: &StructSig) -> CIRStructTy {
        let fields = struct_sig
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(&field.ty))
            .collect();

        let fields_info = struct_sig
            .fields
            .iter()
            .map(|field| (field.name.clone(), field.loc))
            .collect();

        CIRStructTy {
            name: Some(struct_sig.name.clone()),
            fields,
            fields_info,
            repr_attr: struct_sig.modifiers.repr_attr.clone(),
            align: struct_sig.align.clone(),
            loc: struct_sig.loc,
        }
    }

    fn lower_enum_ty_variant(&mut self, variant: &TypedEnumVariant) -> CIREnumTyVariant {
        match variant {
            TypedEnumVariant::Ident(ident) => CIREnumTyVariant::Ident(ident.as_string()),
            TypedEnumVariant::Valued(ident, expr) => {
                CIREnumTyVariant::Valued(ident.as_string(), Box::new(self.lower_expr(expr)))
            }
            TypedEnumVariant::Variant(ident, fields) => {
                let fields: Vec<CIRTy> = fields.iter().map(|field| self.lower_sema_ty(&field.ty)).collect();
                CIREnumTyVariant::Fielded(ident.as_string(), fields)
            }
        }
    }

    fn lower_enum_sig_as_enum_ty(&mut self, enum_sig: &EnumSig) -> CIREnumTy {
        let variants: Vec<CIREnumTyVariant> = enum_sig
            .variants
            .iter()
            .map(|variant| self.lower_enum_ty_variant(variant))
            .collect();

        let tag_type = enum_sig
            .tag_type
            .clone()
            .map(|sema_type| Box::new(self.lower_sema_ty(&sema_type)));

        CIREnumTy {
            name: Some(enum_sig.name.clone()),
            variants,
            tag_type,
            repr_attr: enum_sig.modifiers.repr_attr.clone(),
            align: enum_sig.align.clone(),
            loc: enum_sig.loc,
        }
    }

    fn lower_union_sig_as_union_ty(&mut self, union_sig: &UnionSig) -> CIRUnionTy {
        let fields = union_sig
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(&field.ty))
            .collect();

        let fields_info = union_sig
            .fields
            .iter()
            .map(|field| (field.name.clone(), field.loc))
            .collect();

        CIRUnionTy {
            name: Some(union_sig.name.clone()),
            fields,
            fields_info,
            repr_attr: union_sig.modifiers.repr_attr.clone(),
            align: union_sig.align.clone(),
            loc: union_sig.loc,
        }
    }

    fn lower_resolved_symbol(&mut self, resolved_symbol: &ResolvedSymbol) -> CIRTy {
        let symbol_entry = self.query.get_symbol(resolved_symbol.symbol_id()).unwrap();

        if let Some(resolved_struct) = symbol_entry.as_struct() {
            CIRTy::Struct(self.lower_struct_sig_as_struct_ty(&resolved_struct.struct_sig))
        } else if let Some(resolved_union) = symbol_entry.as_union() {
            CIRTy::Union(self.lower_union_sig_as_union_ty(&resolved_union.union_sig))
        } else if let Some(resolved_enum) = symbol_entry.as_enum() {
            CIRTy::Enum(self.lower_enum_sig_as_enum_ty(&resolved_enum.enum_sig))
        } else {
            unreachable!()
        }
    }
}

#[inline(never)]
pub fn walk_program_trees_in_parallel(
    threads: Option<usize>,
    program_trees: Vec<Box<TypedProgramTree>>,
    query: &dyn Query,
    source_map: Arc<SourceMap>,
    cir_monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    vtable_registries: &Vec<Arc<Mutex<VTableRegistry>>>,
    target: &ABITarget,
) -> Vec<Box<CIRProgramTree>> {
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
                    cir_monomorph_registry.clone(),
                    mapping_ctx_arena.clone(),
                    vtable_registry,
                    target,
                );

                let file_path = source_map.get_file(program_tree.file_id).unwrap().file_path.clone();
                let cir_program_tree = cir_walk.run_pass(file_path.to_string_lossy().to_string());
                Box::new(cir_program_tree)
            })
            .collect()
    })
}
