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

use crate::abi::mangler::{ABINameMangler, DEFAULT_ABI, mangle_func, mangle_global_var, mangle_method};
use crate::abi::target::ABITarget;
use crate::cir::cir::*;
use crate::cir::monomorph::CIRMonomorphRegistry;
use crate::cir::types::*;
use cyrusc_ast::abi::{CallConv, Linkage};
use cyrusc_ast::operators::InfixOperator;
use cyrusc_diagcentral::source_loc::SourceLoc;
use cyrusc_resolver::Resolver;
use cyrusc_resolver::symbols::{LocalScopeRef, ResolvedMethod, generate_symbol_id};
use cyrusc_tast::generics::generic_type::GenericType;
use cyrusc_tast::generics::mapping_ctx_arena::GenericMappingCtxArena;
use cyrusc_tast::generics::substitute::{
    substitute_enum_sig, substitute_func_sig, substitute_struct_sig, substitute_union_sig,
};
use cyrusc_tast::sigs::{EnumSig, FuncSig, GlobalVarSig, StructSig, UnionSig, typed_func_decl_from_func_sig};
use cyrusc_tast::types::{
    PlainType, ResolvedSymbol, TypedUnnamedEnumType, TypedUnnamedEnumVariant, TypedUnnamedUnionType,
    enum_sig_as_unnamed_enum_ty,
};
use cyrusc_tast::{ModuleID, ScopeID, SymbolID};
use cyrusc_tast::{
    TypedProgramTree,
    exprs::*,
    stmts::*,
    types::{SemanticType, TypedArrayCapacity, TypedArrayFixedCapacityValue},
};
use cyrusc_tokens::literals::{LiteralKind, StringPrefix};
use cyrusc_vtable_registry::VTableRegistry;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

pub(crate) struct CIRWalk<'resolver> {
    program_tree: Box<TypedProgramTree>,
    current_self_ty: Option<CIRTy>,
    pub(crate) module_id: ModuleID,
    pub(crate) resolver: &'resolver Resolver,
    pub(crate) cir_monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
    pub(crate) current_obj_operand_ty: Option<SemanticType>,
    pub(crate) symbol_formatter: Box<dyn Fn(SymbolID) -> String + 'resolver>,
    pub(crate) mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    pub(crate) vtable_registry: Arc<Mutex<VTableRegistry>>,
    pub(crate) target: &'resolver ABITarget,
}

impl<'resolver> CIRWalk<'resolver> {
    pub fn new(
        program: Box<TypedProgramTree>,
        resolver: &'resolver Resolver,
        module_id: ModuleID,
        cir_monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
        mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
        vtable_registry: Arc<Mutex<VTableRegistry>>,
        target: &'resolver ABITarget,
    ) -> Self {
        Self {
            program_tree: program,
            resolver,
            module_id,
            cir_monomorph_registry,
            current_self_ty: None,
            current_obj_operand_ty: None,
            symbol_formatter: build_panic_symbol_formatter(),
            mapping_ctx_arena,
            vtable_registry,
            target,
        }
    }

    pub fn run_pass(&mut self, file_path: String) -> CIRProgramTree {
        let stmts = std::mem::take(&mut self.program_tree.body);
        let lowered_stmts = self.lower_stmts(None, &stmts);
        CIRProgramTree {
            body: lowered_stmts,
            file_path,
            module_name: self.program_tree.module_name.clone(),
        }
    }

    fn lower_stmts(&mut self, scope_id_opt: Option<ScopeID>, stmts: &Vec<TypedStmt>) -> Vec<CIRStmt> {
        let mut lowered_stmts: Vec<CIRStmt> = Vec::new();

        for stmt in stmts {
            let lowered_stmt = match stmt {
                TypedStmt::FuncDef(func_def_stmt) => {
                    if func_def_stmt.generic_params.is_some() {
                        continue; // skip lowering at this point
                    }
                    self.lower_func_def(scope_id_opt, func_def_stmt, true)
                }
                TypedStmt::FuncDecl(func_decl_stmt) => {
                    CIRStmt::FuncDecl(self.lower_func_decl(scope_id_opt, func_decl_stmt, true))
                }
                TypedStmt::Switch(switch_stmt) => self.lower_switch(scope_id_opt, switch_stmt),
                TypedStmt::Variable(var_stmt) => CIRStmt::Variable(self.lower_var(scope_id_opt, var_stmt)),
                TypedStmt::GlobalVar(global_var_stmt) => {
                    self.lower_global_var(scope_id_opt, &mut global_var_stmt.clone())
                }
                TypedStmt::BlockStmt(block_stmt) => CIRStmt::Block(self.lower_body(block_stmt)),
                TypedStmt::If(if_stmt) => self.lower_if(scope_id_opt, if_stmt),
                TypedStmt::Return(return_stmt) => self.lower_return(scope_id_opt, return_stmt),
                TypedStmt::Break(break_stmt) => self.lower_break(break_stmt),
                TypedStmt::Continue(continue_stmt) => self.lower_continue(continue_stmt),
                TypedStmt::For(for_stmt) => self.lower_for(scope_id_opt, for_stmt),
                TypedStmt::While(while_stmt) => self.lower_while(scope_id_opt, while_stmt),
                TypedStmt::ExportTuple(export_tuple_stmt) => {
                    self.lower_export_tuple_to_vars(scope_id_opt, export_tuple_stmt)
                        .iter()
                        .for_each(|var| {
                            lowered_stmts.push(CIRStmt::Variable(var.clone()));
                        });
                    continue;
                }
                TypedStmt::Label(label) => self.lower_label(label),
                TypedStmt::Goto(goto) => self.lower_goto(scope_id_opt, goto),
                TypedStmt::Expr(expr) => CIRStmt::Expr(self.lower_expr(scope_id_opt, expr)),
                // lowered only when used
                TypedStmt::Struct(struct_stmt) => {
                    if struct_stmt.generic_params.is_none() {
                        // non generic
                        let stmts =
                            self.lower_non_generic_methods(scope_id_opt, &struct_stmt.name, &struct_stmt.methods);
                        lowered_stmts.extend(stmts);
                    }
                    continue;
                }
                TypedStmt::Enum(enum_stmt) => {
                    if enum_stmt.generic_params.is_none() {
                        // non generic
                        let stmts = self.lower_non_generic_methods(scope_id_opt, &enum_stmt.name, &enum_stmt.methods);
                        lowered_stmts.extend(stmts);
                    }
                    continue;
                }
                TypedStmt::Union(union_stmt) => {
                    if union_stmt.generic_params.is_none() {
                        // non generic
                        let stmts = self.lower_non_generic_methods(scope_id_opt, &union_stmt.name, &union_stmt.methods);
                        lowered_stmts.extend(stmts);
                    }
                    continue;
                }
                // skipped
                TypedStmt::Interface(..) | TypedStmt::Defer(..) | TypedStmt::Typedef(..) => continue,
            };

            lowered_stmts.push(lowered_stmt);
        }

        lowered_stmts
    }

    fn lower_non_generic_methods(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        object_name: &String,
        methods: &HashMap<String, SymbolID>,
    ) -> Vec<CIRStmt> {
        let mut stmts: Vec<CIRStmt> = Vec::new();
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        for module_id in methods.values() {
            let sym = self
                .resolver
                .resolve_local_or_global_symbol(scope_opt.clone(), *module_id)
                .unwrap();
            let resolved_method = sym.as_method().unwrap();

            let lowered_method = self.lower_method(scope_id_opt, resolved_method, object_name);

            stmts.push(match lowered_method {
                Some(stmt) => stmt,
                None => continue,
            });
        }

        stmts
    }

    fn lower_method(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        resolved_method: &ResolvedMethod,
        object_name: &String,
    ) -> Option<CIRStmt> {
        let mangled_name = DEFAULT_ABI.method_name(
            &self.program_tree.module_name,
            object_name,
            &resolved_method.func_sig.name,
        );

        // skip if has no body
        let func_body = resolved_method.func_body.clone()?;

        let func_def = TypedFuncDefStmt {
            module_id: resolved_method.module_id,
            symbol_id: resolved_method.symbol_id,
            name: mangled_name,
            params: resolved_method.func_sig.params.clone(),
            generic_params: resolved_method.func_sig.generic_params.clone(),
            body: func_body,
            return_type: resolved_method.func_sig.return_type.clone(),
            modifiers: resolved_method.func_sig.modifiers.clone(),
            loc: resolved_method.func_sig.loc.clone(),
        };

        Some(self.lower_func_def(scope_id_opt, &func_def, false))
    }

    fn lower_label(&self, label: &TypedLabelStmt) -> CIRStmt {
        CIRStmt::Label(CIRLabelStmt {
            name: label.name.clone(),
            label_id: label.label_id,
        })
    }

    fn lower_goto(&self, scope_id_opt: Option<ScopeID>, goto: &TypedGotoStmt) -> CIRStmt {
        let scope_rc = self
            .resolver
            .resolve_local_scope(self.module_id, scope_id_opt.unwrap())
            .unwrap();

        {
            let scope_ref = scope_rc.borrow();
            let label_id = scope_ref.resolve_label(&goto.name).unwrap();
            CIRStmt::Goto(CIRGotoStmt { label_id })
        }
    }

    pub fn lower_export_tuple_to_vars(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        export_tuple: &TypedExportTupleStmt,
    ) -> Vec<CIRVarStmt> {
        let scope = self
            .resolver
            .resolve_local_scope(self.module_id, scope_id_opt.unwrap())
            .unwrap();

        let mut vars = Vec::new();
        self.lower_export_pattern_recursive(scope_id_opt, &scope, &export_tuple.pattern, &mut vars);
        vars
    }

    fn lower_export_pattern_recursive(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        scope_rc: &LocalScopeRef,
        pattern: &TypedExportPattern,
        vars: &mut Vec<CIRVarStmt>,
    ) {
        match pattern {
            TypedExportPattern::Ident(symbol_id) => {
                let scope = scope_rc.borrow();
                let resolved_variable = scope
                    .with_symbol_id(*symbol_id, |local_symbol| local_symbol.as_variable().cloned().unwrap())
                    .unwrap();

                let var_name = resolved_variable.typed_variable.name.clone();
                let var_ty = self.lower_sema_ty(scope_id_opt, &resolved_variable.typed_variable.ty.as_ref().unwrap());
                let var_rhs = self.lower_expr(scope_id_opt, &resolved_variable.typed_variable.rhs.as_ref().unwrap());
                drop(scope);

                vars.push(CIRVarStmt {
                    irv_id: *symbol_id,
                    name: format!("tuple.{}", var_name),
                    ty: var_ty,
                    expr: Some(var_rhs),
                });
            }
            TypedExportPattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.lower_export_pattern_recursive(scope_id_opt, scope_rc, pattern, vars);
                }
            }
        }
    }

    fn lower_if(&mut self, scope_id_opt: Option<ScopeID>, if_stmt: &TypedIfStmt) -> CIRStmt {
        let cond = self.lower_expr(scope_id_opt, &if_stmt.cond);
        let then_block = Box::new(self.lower_body(&if_stmt.then_block));

        let mut else_block = if_stmt.else_block.as_ref().map(|b| Box::new(self.lower_body(b)));

        for branch in if_stmt.branches.iter().rev() {
            let branch_cond = self.lower_expr(scope_id_opt, &branch.cond);
            let branch_then = Box::new(self.lower_body(&branch.then_block));

            let nested_if = CIRStmt::If(CIRIfStmt {
                cond: branch_cond,
                then_block: branch_then,
                else_block: else_block.take(),
            });

            else_block = Some(Box::new(CIRBlockStmt { stmts: vec![nested_if] }));
        }

        CIRStmt::If(CIRIfStmt {
            cond,
            then_block,
            else_block,
        })
    }

    fn lower_switch(&mut self, scope_id_opt: Option<ScopeID>, switch_stmt: &TypedSwitchStmt) -> CIRStmt {
        let operand = self.lower_expr(scope_id_opt, &switch_stmt.operand);
        let operand_ty = switch_stmt.operand.sema_ty.as_ref().unwrap().const_inner();

        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        let unnamed_enum_type_opt = operand_ty
            .as_enum_symbol_id()
            .and_then(|symbol_id| {
                self.resolver
                    .resolve_local_or_global_symbol(scope_opt.clone(), symbol_id)
                    .unwrap()
                    .as_enum()
                    .cloned()
                    .map(|resolved_enum| enum_sig_as_unnamed_enum_ty(&resolved_enum.enum_sig, switch_stmt.loc.clone()))
            })
            .or(operand_ty.as_generic_type().and_then(|generic_type| {
                Some(
                    self.resolver
                        .resolve_local_or_global_symbol(scope_opt.clone(), generic_type.base)
                        .unwrap()
                        .as_enum()
                        .cloned()
                        .map(|resolved_enum| {
                            let enum_sig = substitute_enum_sig(
                                self.mapping_ctx_arena.clone(),
                                &resolved_enum.enum_sig,
                                generic_type.mapping_ctx.clone(),
                            )
                            .unwrap();

                            enum_sig_as_unnamed_enum_ty(&enum_sig, switch_stmt.loc.clone())
                        })
                        .unwrap(),
                )
            }))
            .or(operand_ty.as_unnamed_enum());

        let default = switch_stmt
            .default_case
            .as_ref()
            .and_then(|default_case| Some(self.lower_body(&default_case)));

        if let Some(unnamed_enum_type) = &unnamed_enum_type_opt {
            if unnamed_enum_type.is_repr_c() || !unnamed_enum_type.includes_payload() {
                self.lower_switch_on_scalar_enum(scope_id_opt, &operand, &unnamed_enum_type, &default, switch_stmt)
            } else {
                self.lower_switch_on_enum(scope_id_opt, &unnamed_enum_type, &operand, &default, switch_stmt)
            }
        } else {
            if switch_stmt.includes_any_range() || !switch_stmt.includes_only_integer() {
                return self.lower_switch_as_chained_if(scope_id_opt, &operand, &default, switch_stmt);
            }

            self.lower_pure_switch(scope_id_opt, &operand, &default, switch_stmt)
        }
    }

    fn lower_switch_as_chained_if(
        &mut self,
        scope_id_opt: Option<ScopeID>,
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
            };
            for pattern in &case.patterns {
                let new_cond = match &pattern {
                    TypedSwitchCasePattern::Expr(expr, _) => {
                        let lowered_case_expr = self.lower_expr(scope_id_opt, expr);

                        CIRExpr {
                            kind: CIRExprKind::Infix(CIRInfixExpr {
                                op: InfixOperator::Equal,
                                lhs: Box::new(operand.clone()),
                                rhs: Box::new(lowered_case_expr),
                            }),
                            ty: CIRTy::PlainType(PlainType::Bool),
                        }
                    }
                    TypedSwitchCasePattern::Range(range) => {
                        let lower = self.lower_expr(scope_id_opt, &range.lower);
                        let upper = self.lower_expr(scope_id_opt, &range.upper);

                        let ge = CIRExpr {
                            kind: CIRExprKind::Infix(CIRInfixExpr {
                                op: InfixOperator::GreaterEqual,
                                lhs: Box::new(operand.clone()),
                                rhs: Box::new(lower),
                            }),
                            ty: CIRTy::PlainType(PlainType::Bool),
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
                        };

                        CIRExpr {
                            kind: CIRExprKind::Infix(CIRInfixExpr {
                                op: InfixOperator::And,
                                lhs: Box::new(ge),
                                rhs: Box::new(upper_cmp),
                            }),
                            ty: CIRTy::PlainType(PlainType::Bool),
                        }
                    }
                    _ => unreachable!("Unexpected switch pattern for if-chain lowering"),
                };

                cond_expr = CIRExpr {
                    kind: CIRExprKind::Infix(CIRInfixExpr {
                        op: InfixOperator::Or,
                        lhs: Box::new(cond_expr),
                        rhs: Box::new(new_cond),
                    }),
                    ty: CIRTy::PlainType(PlainType::Bool),
                };
            }

            let then_block = Box::new(self.lower_body(&case.body));

            let new_if = CIRIfStmt {
                cond: cond_expr,
                then_block,
                else_block: match current {
                    Some(inner) => Some(Box::new(CIRBlockStmt {
                        stmts: vec![CIRStmt::If(inner)],
                    })),
                    None => None,
                },
            };

            current = Some(new_if);
        }

        let root_if = match current {
            Some(mut if_stmt) => {
                if let Some(default_block) = default.clone() {
                    if_stmt.else_block = Some(Box::new(CIRBlockStmt {
                        stmts: vec![CIRStmt::Block(default_block)],
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
        scope_id_opt: Option<ScopeID>,
        operand: &CIRExpr,
        unnamed_enum_type: &TypedUnnamedEnumType,
        default: &Option<CIRBlockStmt>,
        switch_stmt: &TypedSwitchStmt,
    ) -> CIRStmt {
        let enum_ty = self.lower_unnamed_enum_type_as_cir_enum_ty(scope_id_opt, unnamed_enum_type);

        let cases: Vec<CIRSwitchCase> = switch_stmt
            .cases
            .iter()
            .map(|case| {
                let patterns: Vec<CIRExpr> = case
                    .patterns
                    .iter()
                    .map(|pattern| {
                        let case_ident = match &pattern {
                            TypedSwitchCasePattern::Ident(ident, _) => ident,
                            _ => unreachable!("Unexpected enum variant pattern when lowering switch on integer enum."),
                        };

                        let tag = enum_ty.compute_variant_tag(case_ident).unwrap();
                        let tag_type = enum_ty.tag_type_or_infer_or_default();

                        CIRExpr {
                            kind: CIRExprKind::Literal(CIRLiteral {
                                kind: CIRLiteralKind::Integer(tag.try_into().unwrap(), tag_type.is_signed_integer()),
                                ty: *tag_type.clone(),
                            }),
                            ty: *tag_type,
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
        })
    }

    fn lower_pure_switch(
        &mut self,
        scope_id_opt: Option<ScopeID>,
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
                            TypedSwitchCasePattern::Expr(expr, _) => expr,
                            TypedSwitchCasePattern::Range(..) => {
                                unreachable!("Unexpected range when lowering pure switch.")
                            }
                            _ => unreachable!("Unexpected enum variant pattern when lowering pure switch."),
                        };

                        self.lower_expr(scope_id_opt, case_expr)
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
            all_cases_covered: false
        })
    }

    fn lower_switch_on_enum(
        &mut self,
        scope_id_opt: Option<ScopeID>,
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
                    TypedSwitchCasePattern::Ident(ident, _) => {
                        let variant_idx = unnamed_enum_type
                            .variants
                            .iter()
                            .position(|variant| variant.ident().as_string() == *ident)
                            .unwrap();

                        lowered_patterns.push(CIRSwitchOnEnumPattern::Ident(ident.clone(), variant_idx));
                    }
                    TypedSwitchCasePattern::EnumVariant(ident, exported_fields, _) => {
                        let variant_idx = unnamed_enum_type
                            .variants
                            .iter()
                            .position(|variant| variant.ident().as_string() == *ident)
                            .unwrap();

                        let variant = &unnamed_enum_type.variants[variant_idx];

                        match variant {
                            TypedUnnamedEnumVariant::Valued(_, expr) => {
                                let exported_field = exported_fields.first().unwrap();
                                let lowered_expr = self.lower_expr(scope_id_opt, &expr);

                                lowered_patterns.push(CIRSwitchOnEnumPattern::Valued(
                                    ident.clone(),
                                    variant_idx,
                                    (exported_field.clone(), lowered_expr),
                                ));
                            }
                            TypedUnnamedEnumVariant::Variant(_, valued_fields) => {
                                let exported_fields: Vec<(TypedIdentifier, CIRTy)> = exported_fields
                                    .iter()
                                    .enumerate()
                                    .map(|(i, ident)| {
                                        let field_ty = &valued_fields.get(i).as_ref().unwrap().ty;
                                        (ident.clone(), self.lower_sema_ty(scope_id_opt, field_ty))
                                    })
                                    .collect();

                                lowered_patterns.push(CIRSwitchOnEnumPattern::ExportFields(
                                    ident.clone(),
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
        })
    }

    fn lower_while(&mut self, scope_id_opt: Option<ScopeID>, while_stmt: &TypedWhileStmt) -> CIRStmt {
        let cond = Box::new(self.lower_expr(scope_id_opt, &while_stmt.cond));
        let body = Box::new(self.lower_body(&while_stmt.body));
        CIRStmt::While(CIRWhileStmt { cond, body })
    }

    fn lower_for(&mut self, scope_id_opt: Option<ScopeID>, for_stmt: &TypedForStmt) -> CIRStmt {
        let initializer = for_stmt
            .initializer
            .clone()
            .and_then(|var| Some(self.lower_var(scope_id_opt, &var)));
        let cond = for_stmt
            .cond
            .clone()
            .and_then(|cond| Some(self.lower_expr(scope_id_opt, &cond)));
        let increment = for_stmt
            .increment
            .clone()
            .and_then(|increment| Some(self.lower_expr(scope_id_opt, &increment)));

        let body = Box::new(self.lower_body(&for_stmt.body));

        CIRStmt::For(CIRForStmt {
            initializer,
            cond,
            increment,
            body,
        })
    }

    fn lower_break(&self, _: &TypedBreakStmt) -> CIRStmt {
        CIRStmt::Break
    }

    fn lower_continue(&self, _: &TypedContinueStmt) -> CIRStmt {
        CIRStmt::Continue
    }

    fn lower_return(&mut self, scope_id_opt: Option<ScopeID>, ret: &TypedReturnStmt) -> CIRStmt {
        let arg = ret
            .arg
            .clone()
            .and_then(|arg| Some(self.lower_expr(scope_id_opt, &arg)));
        CIRStmt::Return(CIRReturnStmt { arg })
    }

    fn lower_global_var(&mut self, scope_id_opt: Option<ScopeID>, global_var: &mut TypedGlobalVarStmt) -> CIRStmt {
        let ty = global_var
            .ty
            .as_ref()
            .or_else(|| global_var.expr.as_ref().and_then(|expr| expr.sema_ty.as_ref()))
            .map(|sema_ty| self.lower_sema_ty(scope_id_opt, sema_ty))
            .unwrap_or_else(|| {
                panic!(
                    "Global var '{}' has neither explicit type nor valid initializer type.",
                    global_var.name
                )
            });

        let expr = global_var
            .expr
            .clone()
            .and_then(|expr| Some(self.lower_expr(scope_id_opt, &expr)));

        let mangled_name = mangle_global_var(
            &global_var.modifiers,
            &self.module_name_by_module_id(global_var.module_id).unwrap(),
            &global_var.name,
        );

        CIRStmt::GlobalVar(CIRGlobalVarStmt {
            irv_id: global_var.symbol_id,
            name: mangled_name,
            ty,
            expr,
            modifiers: global_var.modifiers.clone(),
        })
    }

    fn lower_global_var_sig(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        irv_id: IRValueID,
        global_var_sig: &GlobalVarSig,
    ) -> CIRGlobalVarStmt {
        let ty = global_var_sig
            .ty
            .as_ref()
            .or_else(|| global_var_sig.rhs.as_ref().and_then(|expr| expr.sema_ty.as_ref()))
            .map(|sema_ty| self.lower_sema_ty(scope_id_opt, sema_ty))
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
        }
    }

    fn lower_var(&mut self, scope_id_opt: Option<ScopeID>, var: &TypedVarStmt) -> CIRVarStmt {
        let ty = var
            .ty
            .as_ref()
            .or_else(|| var.rhs.as_ref().and_then(|rhs| rhs.sema_ty.as_ref()))
            .map(|ty| self.lower_sema_ty(scope_id_opt, ty))
            .unwrap_or_else(|| {
                panic!(
                    "Variable '{}' has neither explicit type nor RHS type ({}:{})",
                    var.name, var.loc.file_path, var.loc.line
                )
            });

        let expr = var
            .rhs
            .clone()
            .and_then(|expr| Some(self.lower_expr(scope_id_opt, &expr)));

        CIRVarStmt {
            irv_id: var.symbol_id,
            name: var.name.clone(),
            ty,
            expr,
        }
    }

    fn lower_func_type_params(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_type_params: &TypedFuncTypeParams,
    ) -> Vec<CIRTy> {
        func_type_params
            .list
            .iter()
            .map(|sema_ty| self.lower_sema_ty(scope_id_opt, sema_ty))
            .collect()
    }

    fn lower_func_params(&mut self, scope_id_opt: Option<ScopeID>, func_params: &TypedFuncParams) -> CIRFuncParams {
        CIRFuncParams {
            list: func_params
                .list
                .iter()
                .map(|param| match param {
                    TypedFuncParamKind::FuncParam(func_param) => CIRFuncParam {
                        irv_id: func_param.symbol_id,
                        ty: self.lower_sema_ty(scope_id_opt, &func_param.ty),
                    },
                    TypedFuncParamKind::SelfModifier(self_modifier) => CIRFuncParam {
                        irv_id: self_modifier.self_symbol_id.unwrap(),
                        ty: self.lower_sema_ty(scope_id_opt, &self_modifier.ty.as_ref().unwrap()),
                    },
                })
                .collect(),
            is_var: func_params.variadic.is_some(),
        }
    }

    fn lower_func_def(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_def: &TypedFuncDefStmt,
        mangle_name: bool,
    ) -> CIRStmt {
        let params = self.lower_func_params(scope_id_opt, &func_def.params);

        let body = self.lower_body(&func_def.body);
        let ret = self.lower_sema_ty(scope_id_opt, &func_def.return_type);

        let mut cir_func_def = CIRFuncDefStmt {
            irv_id: func_def.symbol_id,
            name: func_def.name.clone(),
            params,
            body: Box::new(body),
            ret,
            modifiers: func_def.modifiers.clone(),
            abi_func_info: None,
        };

        let cir_func_type = cir_func_decl_as_func_ty(&cir_func_def_as_decl(&cir_func_def));
        cir_func_def.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());

        if mangle_name {
            let mangled_name = mangle_func(
                &cir_func_def.modifiers,
                &self.program_tree.module_name,
                &cir_func_def.name,
            );
            cir_func_def.name = mangled_name;
        }

        CIRStmt::FuncDef(cir_func_def)
    }

    fn lower_func_decl(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_decl: &TypedFuncDeclStmt,
        mangle_name: bool,
    ) -> CIRFuncDeclStmt {
        let params = self.lower_func_params(scope_id_opt, &func_decl.params);
        let ret = self.lower_sema_ty(scope_id_opt, &func_decl.return_type);

        let mut func_name = func_decl.renamed_as.as_ref().unwrap_or(&func_decl.name).clone();

        if mangle_name {
            func_name = mangle_func(&func_decl.modifiers, &self.program_tree.module_name, &func_name);
        }

        let mut cir_func_decl = CIRFuncDeclStmt {
            irv_id: func_decl.symbol_id,
            name: func_name,
            params,
            ret,
            modifiers: func_decl.modifiers.clone(),
            abi_func_info: None,
        };

        let cir_func_type = cir_func_decl_as_func_ty(&cir_func_decl);
        cir_func_decl.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());
        cir_func_decl
    }

    pub(crate) fn lower_func_sig(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        irv_id: IRValueID,
        func_sig: &FuncSig,
    ) -> CIRFuncDeclStmt {
        let params = self.lower_func_params(scope_id_opt, &func_sig.params);
        let ret = self.lower_sema_ty(scope_id_opt, &func_sig.return_type);

        let mut cir_func_decl = CIRFuncDeclStmt {
            irv_id,
            name: func_sig.name.clone(),
            params,
            ret,
            modifiers: func_sig.modifiers.clone(),
            abi_func_info: None,
        };

        let cir_func_type = cir_func_decl_as_func_ty(&cir_func_decl);
        cir_func_decl.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());
        cir_func_decl
    }

    pub(crate) fn lower_body(&mut self, block: &TypedBlockStmt) -> CIRBlockStmt {
        let mut stmts = self.lower_stmts(Some(block.scope_id), &block.stmts);
        let defer_stmts: Vec<TypedStmt> = block.defers.iter().map(|defer| *defer.operand.clone()).collect();
        stmts.extend(self.lower_stmts(Some(block.scope_id), &defer_stmts));
        CIRBlockStmt { stmts }
    }

    // exprs

    fn lower_expr(&mut self, scope_id_opt: Option<ScopeID>, expr: &TypedExprStmt) -> CIRExpr {
        if cfg!(debug_assertions) {
            if expr.sema_ty.is_none() {
                dbg!(expr.clone());
            }
            debug_assert!(expr.sema_ty.is_some());
        }

        let ty = self.lower_sema_ty(scope_id_opt, &expr.sema_ty.clone().unwrap());

        let kind = match &expr.kind {
            TypedExprKind::Symbol(symbol_id, ..) => self.lower_load_symbol(scope_id_opt, *symbol_id),
            TypedExprKind::Literal(literal_expr) => self.lower_literal(scope_id_opt, literal_expr),
            TypedExprKind::Prefix(prefix_expr) => self.lower_prefix(scope_id_opt, prefix_expr),
            TypedExprKind::Infix(infix_expr) => self.lower_infix(scope_id_opt, infix_expr),
            TypedExprKind::Unary(unary_expr) => self.lower_unary(scope_id_opt, unary_expr),
            TypedExprKind::Assign(assign_expr) => self.lower_assign(scope_id_opt, assign_expr),
            TypedExprKind::Cast(cast_expr) => self.lower_cast(scope_id_opt, cast_expr),
            TypedExprKind::AddrOf(addr_of_expr) => self.lower_addr_of(scope_id_opt, addr_of_expr),
            TypedExprKind::Deref(deref_expr) => self.lower_deref(scope_id_opt, deref_expr),
            TypedExprKind::Array(array_expr) => self.lower_array(scope_id_opt, array_expr),
            TypedExprKind::ArrayIndex(array_index_expr) => self.lower_array_index(scope_id_opt, array_index_expr),
            TypedExprKind::UnnamedStructValue(unnamed_struct_value) => {
                self.lower_unnamed_struct_value(scope_id_opt, unnamed_struct_value)
            }
            TypedExprKind::UnnamedEnumValue(unnamed_enum_value) => {
                self.lower_unnamed_enum_value(scope_id_opt, unnamed_enum_value)
            }
            TypedExprKind::UnnamedUnionValue(unnamed_union_value) => {
                self.lower_unnamed_union_value(scope_id_opt, unnamed_union_value)
            }
            TypedExprKind::FuncCall(func_call) => self.lower_func_call(scope_id_opt, func_call),
            TypedExprKind::MethodCall(method_call) => self.lower_method_call(scope_id_opt, method_call),
            TypedExprKind::FieldAccess(field_access) => self.lower_field_access(scope_id_opt, field_access.clone()),
            TypedExprKind::StructInit(struct_init_expr) => self.lower_struct_init(scope_id_opt, struct_init_expr),
            TypedExprKind::SizeOf(size_of_expr) => self.lower_size_of(scope_id_opt, size_of_expr),
            TypedExprKind::Lambda(lambda_expr) => self.lower_lambda(scope_id_opt, lambda_expr),
            TypedExprKind::Tuple(tuple_expr) => self.lower_tuple(scope_id_opt, tuple_expr),
            TypedExprKind::TupleAccess(tuple_access_expr) => self.lower_tuple_access(scope_id_opt, tuple_access_expr),
            TypedExprKind::Dynamic(dynamic_expr) => self.lower_dynamic_expr(scope_id_opt, dynamic_expr),
            TypedExprKind::SemanticType(..) => unreachable!(),
        };

        CIRExpr { kind, ty }
    }

    fn lower_unnamed_union_value(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_union_value: &TypedUnnamedUnionValue,
    ) -> CIRExprKind {
        let ty =
            CIRTy::Union(self.lower_unnamed_union_type_as_cir_union_ty(
                scope_id_opt,
                &unnamed_union_value.union_ty.as_ref().unwrap(),
            ));

        let expr = self.lower_expr(scope_id_opt, &unnamed_union_value.field_value);

        CIRExprKind::UnionInit(CIRUnionInitExpr {
            expr: Box::new(expr),
            ty,
        })
    }

    fn lower_unnamed_enum_value(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_enum_value: &TypedUnnamedEnumValue,
    ) -> CIRExprKind {
        let unnamed_enum_value_type = unnamed_enum_value.enum_ty.clone().unwrap();

        match unnamed_enum_value_type {
            TypedUnnamedEnumValueTy::EnumSig(enum_sig) => {
                let enum_ty = self.lower_enum_sig_as_enum_ty(scope_id_opt, &enum_sig);

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
                        let expr = self.lower_expr(scope_id_opt, value);
                        CIREnumInitVariant::Valued(Box::new(expr))
                    }
                    TypedEnumVariant::Variant(_, _) => {
                        let values = unnamed_enum_value.kind.as_fielded().unwrap();
                        let exprs = values.iter().map(|expr| self.lower_expr(scope_id_opt, expr)).collect();
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
                let enum_ty = self.lower_unnamed_enum_type_as_cir_enum_ty(scope_id_opt, &unnamed_enum_type);

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
                        let expr = self.lower_expr(scope_id_opt, value);
                        CIREnumInitVariant::Valued(Box::new(expr))
                    }
                    TypedUnnamedEnumVariant::Variant(_, _) => {
                        let values = unnamed_enum_value.kind.as_fielded().unwrap();
                        let exprs = values.iter().map(|expr| self.lower_expr(scope_id_opt, expr)).collect();
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

    fn lower_dynamic_expr(&mut self, scope_id_opt: Option<ScopeID>, dynamic_expr: &TypedDynamicExpr) -> CIRExprKind {
        let operand = self.lower_expr(scope_id_opt, &dynamic_expr.operand);
        let data_ptr = CIRExpr {
            kind: CIRExprKind::AddrOf(CIRAddrOfExpr {
                operand: Box::new(operand),
            }),
            ty: CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))),
        };

        let vtable_info = {
            let vtable_registry = self.vtable_registry.lock().unwrap();
            vtable_registry.info(dynamic_expr.vtable_id.unwrap()).clone()
        };

        let method_decls: Vec<CIRFuncDeclStmt> = vtable_info
            .methods
            .iter()
            .cloned()
            .map(|mut func_sig| {
                let mangled_name = mangle_method(
                    &self.module_name_by_module_id(func_sig.module_id).unwrap(),
                    &dynamic_expr.object_name.as_ref().unwrap(),
                    &func_sig.name,
                );

                func_sig.name = mangled_name;
                self.lower_func_sig(scope_id_opt, func_sig.symbol_id.unwrap(), &func_sig)
            })
            .collect();

        let vtable_abi_name = DEFAULT_ABI.vtable_name(
            &dynamic_expr.object_name.clone().unwrap(),
            &dynamic_expr.vtable_id.unwrap().to_string(),
        );

        CIRExprKind::Dynamic(CIRDynamicExpr {
            data_expr: Box::new(data_ptr),
            method_decls,
            global_var_id: vtable_info.global_var_id,
            vtable_abi_name,
        })
    }

    pub(crate) fn lower_load_symbol(&mut self, scope_id_opt: Option<ScopeID>, symbol_id: SymbolID) -> CIRExprKind {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(scope_opt, symbol_id)
            .unwrap();

        if let Some(resolved_func) = sym.as_func() {
            let mut cir_func_decl = self.lower_func_sig(scope_id_opt, resolved_func.symbol_id, &resolved_func.func_sig);

            let cir_func_type = cir_func_decl_as_func_ty(&cir_func_decl);
            cir_func_decl.abi_func_info = Some(self.target.target_abi.classify_func(&cir_func_type).unwrap());

            let mangled_name = mangle_func(
                &cir_func_decl.modifiers,
                &self.module_name_by_module_id(resolved_func.module_id).unwrap(),
                &resolved_func.func_sig.name,
            );

            cir_func_decl.name = mangled_name;

            CIRExprKind::Load(CIRValue {
                irv_id: resolved_func.symbol_id,
                kind: CIRValueKind::Func(Box::new(cir_func_decl)),
            })
        } else if let Some(resolved_global_var) = sym.as_global_var() {
            let mut global_var_stmt = self.lower_global_var_sig(
                scope_id_opt,
                resolved_global_var.symbol_id,
                &resolved_global_var.global_var_sig,
            );

            if global_var_stmt.modifiers.vis.is_public() {
                if global_var_stmt.modifiers.linkage.is_none() {
                    global_var_stmt.modifiers.linkage = Some(Linkage::Extern(None));
                }
            }

            let mangled_name = mangle_global_var(
                &resolved_global_var.global_var_sig.modifiers,
                &self.module_name_by_module_id(resolved_global_var.module_id).unwrap(),
                &resolved_global_var.global_var_sig.name,
            );

            global_var_stmt.name = mangled_name;

            CIRExprKind::Load(CIRValue {
                irv_id: resolved_global_var.symbol_id,
                kind: CIRValueKind::GlobalVar(Box::new(global_var_stmt)),
            })
        } else if let Some(resolved_variable) = sym.as_variable() {
            CIRExprKind::Load(CIRValue {
                irv_id: resolved_variable.symbol_id,
                kind: CIRValueKind::LocalVariable,
            })
        } else {
            unreachable!("Unexpected symbol kind when lowering load symbol.")
        }
    }

    fn module_name_by_module_id(&self, module_id: ModuleID) -> Option<String> {
        let program_trees = self.resolver.program_trees.lock().unwrap();
        let opt = program_trees
            .iter()
            .find(|entry| entry.module_id == module_id)
            .map(|entry| entry.module_name.clone());
        drop(program_trees);
        opt
    }

    pub(crate) fn lower_struct_init(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        struct_init_expr: &TypedStructInitExpr,
    ) -> CIRExprKind {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(scope_opt, struct_init_expr.symbol_id)
            .unwrap();

        if let Some(resolved_struct) = sym.as_struct() {
            let fields_tys: Vec<CIRTy> = struct_init_expr
                .fields
                .iter()
                .map(|field| self.lower_sema_ty(scope_id_opt, &field.value.sema_ty.clone().unwrap()))
                .collect();

            let struct_ty = CIRStructTy {
                repr_attr: resolved_struct.struct_sig.modifiers.repr_attr.clone(),
                align: resolved_struct.struct_sig.align.clone(),
                fields: fields_tys,
            };

            let fields: Vec<CIRExpr> = struct_init_expr
                .fields
                .iter()
                .map(|field| self.lower_expr(scope_id_opt, &field.value))
                .collect();

            CIRExprKind::StructInit(CIRStructInitExpr { ty: struct_ty, fields })
        } else if let Some(resolved_union) = sym.as_union() {
            let fields_tys: Vec<CIRTy> = struct_init_expr
                .fields
                .iter()
                .map(|field| self.lower_sema_ty(scope_id_opt, &field.value.sema_ty.clone().unwrap()))
                .collect();

            let union_ty = CIRTy::Union(CIRUnionTy {
                repr_attr: resolved_union.union_sig.modifiers.repr_attr.clone(),
                align: resolved_union.union_sig.align.clone(),
                fields: fields_tys,
            });

            let struct_field_init = struct_init_expr.fields.first().unwrap();
            let expr = Box::new(self.lower_expr(scope_id_opt, &struct_field_init.value));

            CIRExprKind::UnionInit(CIRUnionInitExpr { expr, ty: union_ty })
        } else {
            unreachable!()
        }
    }

    fn lower_tuple_access(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        tuple_access: &TypedTupleAccessExpr,
    ) -> CIRExprKind {
        let operand = Box::new(self.lower_expr(scope_id_opt, &tuple_access.operand));
        CIRExprKind::TupleAccess(CIRTupleAccessExpr {
            operand,
            index: tuple_access.index,
        })
    }

    fn lower_tuple(&mut self, scope_id_opt: Option<ScopeID>, tuple: &TypedTupleExpr) -> CIRExprKind {
        let elms: Vec<CIRExpr> = tuple
            .expr_list
            .iter()
            .map(|expr| self.lower_expr(scope_id_opt, expr))
            .collect();
        CIRExprKind::Tuple(CIRTupleExpr { elms })
    }

    fn lower_lambda(&mut self, scope_id_opt: Option<ScopeID>, lambda: &TypedLambdaExpr) -> CIRExprKind {
        let params = self.lower_func_params(scope_id_opt, &lambda.params);
        let body = Box::new(self.lower_body(&lambda.body));
        let ret = self.lower_sema_ty(scope_id_opt, &lambda.return_type);

        let cir_func_type = CIRFuncTy {
            params: params.list.iter().map(|param| param.ty.clone()).collect(),
            is_var: params.is_var,
            ret: Box::new(ret.clone()),
            callconv: CallConv::default(),
            abi_func_info: None,
        };

        let abi_func_info = self.target.target_abi.classify_func(&cir_func_type).unwrap();

        CIRExprKind::Lambda(CIRLambda {
            irv_id: generate_symbol_id(),
            params,
            inline: lambda.inline,
            ret,
            body,
            abi_func_info,
        })
    }

    fn lower_interface_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        method_call: &TypedMethodCall,
    ) -> CIRExprKind {
        let operand = self.lower_expr(scope_id_opt, &method_call.operand);

        let args = method_call
            .args
            .iter()
            .map(|arg| self.lower_expr(scope_id_opt, arg))
            .collect::<Vec<CIRExpr>>();

        let ret_ty = self.lower_sema_ty(
            scope_id_opt,
            &method_call.func_sig.as_ref().unwrap().return_type.clone(),
        );

        let interface_method_call_metadata = method_call.method_call_on_interface.as_ref().unwrap();

        let cir_func_decl = self.lower_func_sig(
            scope_id_opt,
            interface_method_call_metadata.method_sig.symbol_id.unwrap(),
            &interface_method_call_metadata.method_sig,
        );

        CIRExprKind::InterfaceMethodCall(CIRInterfaceMethodCall {
            operand: Box::new(operand),
            args,
            ret_ty,
            func_type: cir_func_decl_as_func_ty(&cir_func_decl),
            method_idx: interface_method_call_metadata.method_idx,
            methods_len: interface_method_call_metadata.methods_len,
        })
    }

    fn lower_regular_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        method_call: &TypedMethodCall,
    ) -> CIRExprKind {
        if method_call.method_call_on_interface.is_some() {
            return self.lower_interface_method_call(scope_id_opt, method_call);
        }

        self.current_obj_operand_ty = Some(method_call.operand.sema_ty.clone().unwrap());
        self.current_self_ty = method_call
            .self_ty
            .clone()
            .map(|sema_ty| self.lower_sema_ty(scope_id_opt, &sema_ty));

        let mut func_sig = method_call.func_sig.as_ref().unwrap().clone();

        let mangled_name = mangle_method(
            &self.module_name_by_module_id(func_sig.module_id).unwrap(),
            &method_call.object_name.as_ref().unwrap(),
            &func_sig.name,
        );

        func_sig.name = mangled_name;

        let args = method_call
            .args
            .iter()
            .map(|arg| self.lower_expr(scope_id_opt, arg))
            .collect::<Vec<CIRExpr>>();

        let ret_ty = self.lower_sema_ty(scope_id_opt, &func_sig.return_type.clone());

        let cir_expr_kind;
        if let Some(monomorph_key) = &method_call.monomorph_key {
            self.insert_monomorph_func_instance(scope_id_opt, monomorph_key, &func_sig);

            cir_expr_kind = CIRExprKind::MonomorphFuncInstanceCall(CIRMonomorphFuncInstanceCall {
                monomorph_key: monomorph_key.clone(),
                args,
                ret_ty,
            })
        } else {
            let func_decl = typed_func_decl_from_func_sig(&func_sig);
            let cir_func_decl = self.lower_func_decl(scope_id_opt, &func_decl, false);

            let operand = Box::new(CIRExpr {
                kind: CIRExprKind::Load(CIRValue {
                    irv_id: method_call.func_sig.as_ref().unwrap().symbol_id.unwrap(),
                    kind: CIRValueKind::Func(Box::new(cir_func_decl)),
                }),
                ty: ret_ty.clone(),
            });

            cir_expr_kind = CIRExprKind::FuncCall(CIRFuncCall { operand, args, ret_ty })
        }

        self.current_self_ty = None;
        cir_expr_kind
    }

    fn lower_enum_init(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        enum_sig: &EnumSig,
        method_call: &TypedMethodCall,
    ) -> CIRExprKind {
        let sema_ty = method_call.operand.sema_ty.as_ref().unwrap();

        let variant_idx_opt = enum_sig
            .variants
            .iter()
            .position(|variant| variant.ident().as_string() == method_call.method_name);

        // it's not a enum variant construction, so try again as a regular method call
        let variant_idx = if let Some(i) = variant_idx_opt {
            i
        } else {
            return self.lower_regular_method_call(scope_id_opt, method_call);
        };

        let typed_variant = enum_sig.variants.get(variant_idx).unwrap();

        let variant: CIREnumInitVariant;
        let enum_ty: CIREnumTy;
        if let Some(generic_type) = sema_ty.as_generic_type() {
            let enum_sig = substitute_enum_sig(
                self.mapping_ctx_arena.clone(),
                &enum_sig,
                generic_type.mapping_ctx.clone(),
            )
            .unwrap();

            enum_ty = self.lower_enum_sig_as_enum_ty(scope_id_opt, &enum_sig);
        } else {
            enum_ty = self.lower_enum_sig_as_enum_ty(scope_id_opt, &enum_sig);
        }

        let tag = enum_ty.compute_variant_tag(&method_call.method_name).unwrap();

        variant = match typed_variant {
            TypedEnumVariant::Variant(..) => {
                let values: Vec<CIRExpr> = method_call
                    .args
                    .iter()
                    .map(|arg| self.lower_expr(scope_id_opt, arg))
                    .collect();
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

    fn lower_method_call(&mut self, scope_id_opt: Option<ScopeID>, method_call: &TypedMethodCall) -> CIRExprKind {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        if let Some(enum_symbol_id) = method_call.enum_const {
            let sym = self
                .resolver
                .resolve_local_or_global_symbol(scope_opt.clone(), enum_symbol_id)
                .unwrap();

            if let Some(resolved_enum) = sym.as_enum() {
                return self.lower_enum_init(scope_id_opt, &resolved_enum.enum_sig, method_call);
            }
        }

        self.lower_regular_method_call(scope_id_opt, method_call)
    }

    fn lower_field_access(&mut self, scope_id_opt: Option<ScopeID>, mut field_access: TypedFieldAccess) -> CIRExprKind {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        if field_access.is_fat_arrow {
            field_access.operand = Box::new(TypedExprStmt {
                kind: TypedExprKind::Deref(TypedDerefExpr {
                    operand: field_access.operand.clone(),
                    loc: SourceLoc::default(),
                }),
                sema_ty: Some(field_access.operand.sema_ty.clone().unwrap().pointer_inner().clone()),
                mloc: MemoryLocation::LValue,
                loc: SourceLoc::default(),
            })
        }

        if let Some(sema_ty) = &field_access.operand.sema_ty {
            if sema_ty.as_unnamed_struct().is_some() {
                return CIRExprKind::StructFieldAccess(CIRStructFieldAccessExpr {
                    operand: Box::new(self.lower_expr(scope_id_opt, &field_access.operand)),
                    field_idx: field_access.field_index.unwrap(),
                    field_ty: self.lower_sema_ty(scope_id_opt, &field_access.field_ty.as_ref().unwrap()),
                });
            }

            if sema_ty.as_unnamed_union().is_some() {
                return CIRExprKind::UnionFieldAccess(CIRUnionFieldAccessExpr {
                    operand: Box::new(self.lower_expr(scope_id_opt, &field_access.operand)),
                    field_ty: self.lower_sema_ty(scope_id_opt, &field_access.field_ty.as_ref().unwrap()),
                });
            }
        }

        let sym = self
            .resolver
            .resolve_local_or_global_symbol(scope_opt, field_access.object_symbol_id.unwrap())
            .unwrap();

        if sym.as_struct().is_some() {
            CIRExprKind::StructFieldAccess(CIRStructFieldAccessExpr {
                operand: Box::new(self.lower_expr(scope_id_opt, &field_access.operand)),
                field_idx: field_access.field_index.unwrap(),
                field_ty: self.lower_sema_ty(scope_id_opt, &field_access.field_ty.as_ref().unwrap()),
            })
        } else if sym.as_union().is_some() {
            CIRExprKind::UnionFieldAccess(CIRUnionFieldAccessExpr {
                operand: Box::new(self.lower_expr(scope_id_opt, &field_access.operand)),
                field_ty: self.lower_sema_ty(scope_id_opt, &field_access.field_ty.as_ref().unwrap()),
            })
        } else if let Some(mut resolved_enum) = sym.as_enum().cloned() {
            let sema_ty = field_access.operand.sema_ty.as_ref().unwrap();

            let variant: CIREnumInitVariant;
            let enum_ty: CIREnumTy;
            if let Some(generic_type) = sema_ty.as_generic_type() {
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
                TypedEnumVariant::Valued(_, expr) => {
                    CIREnumInitVariant::Valued(Box::new(self.lower_expr(scope_id_opt, expr)))
                }
                TypedEnumVariant::Variant(..) => unreachable!(),
            };

            enum_ty = self.lower_enum_sig_as_enum_ty(scope_id_opt, &resolved_enum.enum_sig);

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

    fn lower_func_call(&mut self, scope_id_opt: Option<ScopeID>, func_call: &TypedFuncCall) -> CIRExprKind {
        let args: Vec<CIRExpr> = func_call
            .args
            .iter()
            .map(|arg| self.lower_expr(scope_id_opt, arg))
            .collect();

        let ret_ty = self.lower_sema_ty(scope_id_opt, &func_call.return_type.clone().unwrap());

        if let Some(monomorph_key) = &func_call.monomorph_key {
            let monomorph_func_entry = self.resolve_monomorph_func_entry(monomorph_key).unwrap();

            let scope_opt =
                scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

            let sym = self
                .resolver
                .resolve_local_or_global_symbol(scope_opt, monomorph_func_entry.base_symbol)
                .unwrap();

            let mut func_sig = sym
                .as_func()
                .map(|resolved_func| resolved_func.func_sig.clone())
                .or(sym.as_method().map(|resolved_method| resolved_method.func_sig.clone()))
                .expect("Monomorphizaton not supported for the symbol.");

            func_sig = substitute_func_sig(
                self.mapping_ctx_arena.clone(),
                &func_sig,
                Rc::new(RefCell::new(monomorph_func_entry.mapping_ctx.clone())),
            )
            .unwrap();

            self.insert_monomorph_func_instance(scope_id_opt, monomorph_key, &func_sig);

            CIRExprKind::MonomorphFuncInstanceCall(CIRMonomorphFuncInstanceCall {
                monomorph_key: monomorph_key.clone(),
                args,
                ret_ty,
            })
        } else {
            let operand = Box::new(self.lower_expr(scope_id_opt, &func_call.operand));
            CIRExprKind::FuncCall(CIRFuncCall { operand, args, ret_ty })
        }
    }

    fn lower_unnamed_struct_value(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_struct_value: &TypedUnnamedStructValue,
    ) -> CIRExprKind {
        let unnamed_struct_type = unnamed_struct_value.as_unnamed_struct_type();

        let fields: Vec<CIRExpr> = unnamed_struct_value
            .fields
            .iter()
            .map(|field| self.lower_expr(scope_id_opt, &field.field_value))
            .collect();

        let field_types = unnamed_struct_type
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
            .collect();

        let struct_ty = CIRStructTy {
            fields: field_types,
            repr_attr: unnamed_struct_value.repr_attr.clone(),
            align: unnamed_struct_value.align.clone(),
        };

        CIRExprKind::StructInit(CIRStructInitExpr { ty: struct_ty, fields })
    }

    fn lower_array_index(&mut self, scope_id_opt: Option<ScopeID>, array_index: &TypedArrayIndexExpr) -> CIRExprKind {
        CIRExprKind::ArrayIndex(CIRArrayIndexExpr {
            operand: Box::new(self.lower_expr(scope_id_opt, &array_index.operand)),
            index: Box::new(self.lower_expr(scope_id_opt, &array_index.index)),
        })
    }

    fn lower_array(&mut self, scope_id_opt: Option<ScopeID>, array: &TypedArrayExpr) -> CIRExprKind {
        let elms: Vec<CIRExpr> = array
            .elements
            .iter()
            .map(|elm| self.lower_expr(scope_id_opt, elm))
            .collect();

        CIRExprKind::Array(CIRArrayExpr {
            ty: self.lower_sema_ty(scope_id_opt, &array.array_type.as_ref().unwrap()),
            elms,
        })
    }

    fn lower_deref(&mut self, scope_id_opt: Option<ScopeID>, deref: &TypedDerefExpr) -> CIRExprKind {
        CIRExprKind::Deref(CIRDerefExpr {
            operand: Box::new(self.lower_expr(scope_id_opt, &deref.operand)),
        })
    }

    fn lower_addr_of(&mut self, scope_id_opt: Option<ScopeID>, addr_of: &TypedAddrOfExpr) -> CIRExprKind {
        CIRExprKind::AddrOf(CIRAddrOfExpr {
            operand: Box::new(self.lower_expr(scope_id_opt, &addr_of.operand)),
        })
    }

    fn lower_size_of(&mut self, scope_id_opt: Option<ScopeID>, sizeof: &TypedSizeOfExpr) -> CIRExprKind {
        let sema_ty = match &sizeof.operand.kind {
            TypedExprKind::SemanticType(sema_ty) => sema_ty.clone(),
            _ => sizeof.operand.sema_ty.clone().unwrap(),
        };

        let ty = self.lower_sema_ty(scope_id_opt, &sema_ty);
        CIRExprKind::SizeOf(CIRSizeOfExpr { ty })
    }

    fn lower_cast(&mut self, scope_id_opt: Option<ScopeID>, cast: &TypedCastExpr) -> CIRExprKind {
        CIRExprKind::Cast(CIRCastExpr {
            operand: Box::new(self.lower_expr(scope_id_opt, &cast.operand)),
            ty: Box::new(self.lower_sema_ty(scope_id_opt, &cast.target_type)),
        })
    }

    fn lower_assign(&mut self, scope_id_opt: Option<ScopeID>, assign: &TypedAssignExpr) -> CIRExprKind {
        CIRExprKind::Assign(CIRAssignExpr {
            lhs: Box::new(self.lower_expr(scope_id_opt, &assign.lhs)),
            rhs: Box::new(self.lower_expr(scope_id_opt, &assign.rhs)),
        })
    }

    fn lower_unary(&mut self, scope_id_opt: Option<ScopeID>, unary: &TypedUnaryExpr) -> CIRExprKind {
        CIRExprKind::Unary(CIRUnaryExpr {
            op: unary.op.clone(),
            operand: Box::new(self.lower_expr(scope_id_opt, &unary.operand)),
        })
    }

    fn lower_infix(&mut self, scope_id_opt: Option<ScopeID>, infix: &TypedInfixExpr) -> CIRExprKind {
        CIRExprKind::Infix(CIRInfixExpr {
            op: infix.op.clone(),
            lhs: Box::new(self.lower_expr(scope_id_opt, &infix.lhs)),
            rhs: Box::new(self.lower_expr(scope_id_opt, &infix.rhs)),
        })
    }

    fn lower_prefix(&mut self, scope_id_opt: Option<ScopeID>, prefix: &TypedPrefixExpr) -> CIRExprKind {
        CIRExprKind::Prefix(CIRPrefixExpr {
            op: prefix.op.clone(),
            operand: Box::new(self.lower_expr(scope_id_opt, &prefix.operand)),
        })
    }

    fn lower_literal(&mut self, scope_id_opt: Option<ScopeID>, lit: &TypedLiteralExpr) -> CIRExprKind {
        let kind = match &lit.kind {
            LiteralKind::Integer(value, ..) => {
                let is_signed = lit.ty.clone().unwrap().as_basic_type().unwrap().is_signed();
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

        let ty = self.lower_sema_ty(scope_id_opt, &lit.ty.clone().unwrap());
        CIRExprKind::Literal(CIRLiteral { kind, ty })
    }

    // types

    fn lower_sema_ty(&mut self, scope_id_opt: Option<ScopeID>, sema_ty: &SemanticType) -> CIRTy {
        match sema_ty {
            SemanticType::ResolvedSymbol(resolved_symbol) => self.lower_resolved_symbol(scope_id_opt, resolved_symbol),
            SemanticType::PlainType(basic_type) => CIRTy::PlainType(basic_type.clone()),
            SemanticType::Array(array_type) => {
                let ty = self.lower_sema_ty(scope_id_opt, &array_type.element_type);
                let len = match &array_type.capacity {
                    TypedArrayCapacity::Fixed(fixed_cap) => match fixed_cap {
                        TypedArrayFixedCapacityValue::Value(value) => *value,
                        TypedArrayFixedCapacityValue::Expr(_) => unreachable!(),
                    },
                    TypedArrayCapacity::Dynamic => todo!(),
                };
                CIRTy::Array(CIRArrayTy {
                    ty: Box::new(ty),
                    len: len.try_into().unwrap(),
                })
            }
            SemanticType::Const(sema_ty) => CIRTy::Const(Box::new(self.lower_sema_ty(scope_id_opt, &*sema_ty))),
            SemanticType::Pointer(sema_ty) => CIRTy::Pointer(Box::new(self.lower_sema_ty(scope_id_opt, &*sema_ty))),
            SemanticType::UnnamedStruct(unnamed_struct_type) => {
                let field_tys: Vec<CIRTy> = unnamed_struct_type
                    .fields
                    .iter()
                    .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
                    .collect();

                CIRTy::Struct(CIRStructTy {
                    fields: field_tys,
                    repr_attr: unnamed_struct_type.repr_attr.clone(),
                    align: unnamed_struct_type.align.clone(),
                })
            }
            SemanticType::FuncType(func_type) => {
                let ret = Box::new(self.lower_sema_ty(scope_id_opt, &func_type.return_type));
                let params = self.lower_func_type_params(scope_id_opt, &func_type.params);

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
                let items: Vec<CIRTy> = tuple_type
                    .type_list
                    .iter()
                    .map(|sema_ty| self.lower_sema_ty(scope_id_opt, sema_ty))
                    .collect();

                CIRTy::Tuple(CIRTupleTy { elements: items })
            }
            SemanticType::GenericType(generic_type) => self.lower_generic_type(scope_id_opt, generic_type.clone()),
            SemanticType::UnresolvedSymbol(_) => unreachable!("Unexpected unresolved symbol."),
            SemanticType::SelfType(_) => {
                if let Some(cir_ty) = &self.current_self_ty {
                    cir_ty.clone()
                } else {
                    unreachable!("Unexpected self type which is not resolved.")
                }
            }
            SemanticType::GenericParam(generic_param) => {
                if let Some(sema_ty) = self.current_obj_operand_ty.clone() {
                    if let Some(generic_type) = sema_ty.as_generic_type() {
                        {
                            let mapping_ctx = generic_type.mapping_ctx.borrow();

                            let cir_ty = mapping_ctx
                                .resolve_with_name(self.mapping_ctx_arena.clone(), &generic_param.param_name.name)
                                .map(|sema_ty| self.lower_sema_ty(scope_id_opt, &sema_ty))
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
            SemanticType::Interface(_) => {
                CIRTy::Struct(CIRStructTy {
                    fields: vec![
                        CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))), // data_ptr
                        CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))), // vtable_ptr
                    ],
                    repr_attr: None,
                    align: None,
                })
            }
            SemanticType::UnnamedUnion(unnamed_union_type) => {
                CIRTy::Union(self.lower_unnamed_union_type_as_cir_union_ty(scope_id_opt, unnamed_union_type))
            }
            SemanticType::UnnamedEnum(unnamed_enum_type) => {
                CIRTy::Enum(self.lower_unnamed_enum_type_as_cir_enum_ty(scope_id_opt, unnamed_enum_type))
            }
        }
        .const_inner()
        .clone()
    }

    fn lower_unnamed_union_type_as_cir_union_ty(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_union_type: &TypedUnnamedUnionType,
    ) -> CIRUnionTy {
        CIRUnionTy {
            fields: unnamed_union_type
                .fields
                .iter()
                .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
                .collect(),
            repr_attr: unnamed_union_type.repr_attr.clone(),
            align: unnamed_union_type.align.clone(),
        }
    }

    fn lower_unnamed_enum_type_as_cir_enum_ty(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_enum_type: &TypedUnnamedEnumType,
    ) -> CIREnumTy {
        let variants: Vec<CIREnumTyVariant> = unnamed_enum_type
            .variants
            .iter()
            .map(|variant| self.lower_unnamed_enum_ty_variant(scope_id_opt, variant))
            .collect();

        let tag_type = unnamed_enum_type
            .tag_type
            .clone()
            .map(|sema_ty| Box::new(self.lower_sema_ty(scope_id_opt, &sema_ty)));

        CIREnumTy {
            variants,
            tag_type,
            repr_attr: unnamed_enum_type.repr_attr.clone(),
            align: unnamed_enum_type.align.clone(),
        }
    }

    fn lower_unnamed_enum_ty_variant(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        variant: &TypedUnnamedEnumVariant,
    ) -> CIREnumTyVariant {
        match variant {
            TypedUnnamedEnumVariant::Ident(ident) => CIREnumTyVariant::Ident(ident.as_string()),
            TypedUnnamedEnumVariant::Valued(ident, expr) => {
                CIREnumTyVariant::Valued(ident.as_string(), Box::new(self.lower_expr(scope_id_opt, expr)))
            }
            TypedUnnamedEnumVariant::Variant(ident, fields) => {
                let fields: Vec<CIRTy> = fields
                    .iter()
                    .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
                    .collect();
                CIREnumTyVariant::Fielded(ident.as_string(), fields)
            }
        }
    }

    fn lower_generic_type(&mut self, scope_id_opt: Option<ScopeID>, mut generic_type: GenericType) -> CIRTy {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(scope_opt, generic_type.base)
            .unwrap();

        if let Err(err) = generic_type.init(self.mapping_ctx_arena.clone(), &self.symbol_formatter) {
            eprintln!("Failed to init generic type: {:?}.", err.kind.to_string())
        }

        if let Some(resolved_struct) = sym.as_struct() {
            let struct_sig = substitute_struct_sig(
                self.mapping_ctx_arena.clone(),
                &resolved_struct.struct_sig,
                generic_type.mapping_ctx,
            )
            .unwrap();

            let cir_struct_ty = self.lower_struct_sig_as_struct_ty(scope_id_opt, &struct_sig);
            CIRTy::Struct(cir_struct_ty)
        } else if let Some(resolved_enum) = sym.as_enum() {
            let enum_sig = substitute_enum_sig(
                self.mapping_ctx_arena.clone(),
                &resolved_enum.enum_sig,
                generic_type.mapping_ctx,
            )
            .unwrap();

            let cir_enum_ty = self.lower_enum_sig_as_enum_ty(scope_id_opt, &enum_sig);
            CIRTy::Enum(cir_enum_ty)
        } else if let Some(resolved_union) = sym.as_union() {
            let union_sig = substitute_union_sig(
                self.mapping_ctx_arena.clone(),
                &resolved_union.union_sig,
                generic_type.mapping_ctx,
            )
            .unwrap();

            let cir_union_ty = self.lower_union_sig_as_union_ty(scope_id_opt, &union_sig);
            CIRTy::Union(cir_union_ty)
        } else {
            unreachable!("Object does not support generic type at CIR walk.")
        }
    }

    fn lower_struct_sig_as_struct_ty(&mut self, scope_id_opt: Option<ScopeID>, struct_sig: &StructSig) -> CIRStructTy {
        let fields: Vec<CIRTy> = struct_sig
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
            .collect();

        CIRStructTy {
            fields,
            repr_attr: struct_sig.modifiers.repr_attr.clone(),
            align: struct_sig.align.clone(),
        }
    }

    fn lower_enum_ty_variant(&mut self, scope_id_opt: Option<ScopeID>, variant: &TypedEnumVariant) -> CIREnumTyVariant {
        match variant {
            TypedEnumVariant::Ident(ident) => CIREnumTyVariant::Ident(ident.as_string()),
            TypedEnumVariant::Valued(ident, expr) => {
                CIREnumTyVariant::Valued(ident.as_string(), Box::new(self.lower_expr(scope_id_opt, expr)))
            }
            TypedEnumVariant::Variant(ident, fields) => {
                let fields: Vec<CIRTy> = fields
                    .iter()
                    .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
                    .collect();
                CIREnumTyVariant::Fielded(ident.as_string(), fields)
            }
        }
    }

    fn lower_enum_sig_as_enum_ty(&mut self, scope_id_opt: Option<ScopeID>, enum_sig: &EnumSig) -> CIREnumTy {
        let variants: Vec<CIREnumTyVariant> = enum_sig
            .variants
            .iter()
            .map(|variant| self.lower_enum_ty_variant(scope_id_opt, variant))
            .collect();

        let tag_type = enum_sig
            .tag_type
            .clone()
            .map(|sema_ty| Box::new(self.lower_sema_ty(scope_id_opt, &sema_ty)));

        CIREnumTy {
            variants,
            tag_type,
            repr_attr: enum_sig.modifiers.repr_attr.clone(),
            align: enum_sig.align.clone(),
        }
    }

    fn lower_union_sig_as_union_ty(&mut self, scope_id_opt: Option<ScopeID>, union_sig: &UnionSig) -> CIRUnionTy {
        let fields: Vec<CIRTy> = union_sig
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
            .collect();

        CIRUnionTy {
            fields,
            repr_attr: union_sig.modifiers.repr_attr.clone(),
            align: union_sig.align.clone(),
        }
    }

    fn lower_resolved_symbol(&mut self, scope_id_opt: Option<ScopeID>, resolved_symbol: &ResolvedSymbol) -> CIRTy {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(scope_opt, resolved_symbol.symbol_id())
            .unwrap();

        if let Some(resolved_struct) = sym.as_struct() {
            CIRTy::Struct(self.lower_struct_sig_as_struct_ty(scope_id_opt, &resolved_struct.struct_sig))
        } else if let Some(resolved_union) = sym.as_union() {
            CIRTy::Union(self.lower_union_sig_as_union_ty(scope_id_opt, &resolved_union.union_sig))
        } else if let Some(resolved_enum) = sym.as_enum() {
            CIRTy::Enum(self.lower_enum_sig_as_enum_ty(scope_id_opt, &resolved_enum.enum_sig))
        } else {
            unreachable!()
        }
    }
}

#[inline(never)]
pub fn walk_program_trees_in_parallel(
    threads: Option<usize>,
    program_trees: Vec<Box<TypedProgramTree>>,
    resolver: &Resolver,
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

                let mut cir_walk = CIRWalk::new(
                    program_tree.clone(),
                    resolver,
                    program_tree.module_id,
                    cir_monomorph_registry.clone(),
                    mapping_ctx_arena.clone(),
                    vtable_registry,
                    target,
                );
                let cir_program_tree = cir_walk.run_pass(program_tree.file_path.to_string_lossy().to_string());
                Box::new(cir_program_tree)
            })
            .collect()
    })
}

pub(crate) fn build_panic_symbol_formatter<'a>() -> Box<dyn Fn(SymbolID) -> String + 'a> {
    Box::new(move |symbol_id: SymbolID| -> String {
        panic!("Panic symbol formatter called for symbol id {}", symbol_id)
    }) as Box<dyn Fn(SymbolID) -> String + 'a>
}
