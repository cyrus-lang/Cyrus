use crate::monomorph::CIRMonomorphRegistry;
use crate::types::{CIRArrayTy, CIRFuncTy, CIRStructTy, CIRTupleTy, CIRTy};
use crate::*;
use cyrusc_abi::mangling::ABINameMangling;
use cyrusc_ast::{LiteralKind, SelfModifierKind};
use cyrusc_resolver::symbols::{LocalScopeRef, ResolvedMethod, generate_symbol_id};
use cyrusc_resolver::{Resolver, typed_func_decl_from_func_sig};
use cyrusc_tast::generics::generic_type::GenericType;
use cyrusc_tast::generics::substitute::{substitute_enum_sig, substitute_struct_sig, substitute_union_sig};
use cyrusc_tast::sigs::{EnumSig, FuncSig, GlobalVarSig, UnionSig};
use cyrusc_tast::types::{PlainType, ResolvedSymbol};
use cyrusc_tast::{ModuleID, ScopeID, SymbolID};
use cyrusc_tast::{
    TypedProgramTree,
    exprs::*,
    stmts::*,
    types::{SemanticType, TypedArrayCapacity, TypedArrayFixedCapacityValue},
};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

pub(crate) struct CIRWalk<'resolver> {
    program: Box<TypedProgramTree>,
    mangling: &'resolver dyn ABINameMangling,
    pub(crate) module_id: ModuleID,
    pub(crate) resolver: &'resolver Resolver,
    pub(crate) cir_monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
}

impl<'resolver> CIRWalk<'resolver> {
    pub fn new(
        program: Box<TypedProgramTree>,
        resolver: &'resolver Resolver,
        module_id: ModuleID,
        cir_monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
        mangling: &'resolver dyn ABINameMangling,
    ) -> Self {
        Self {
            program,
            resolver,
            module_id,
            cir_monomorph_registry,
            mangling,
        }
    }

    pub fn run_pass(&mut self, file_path: String) -> CIRProgramTree {
        let stmts = std::mem::take(&mut self.program.body);
        let lowered_stmts = self.lower_stmts(None, &stmts);
        CIRProgramTree {
            body: lowered_stmts,
            file_path,
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
                    self.lower_func_def(scope_id_opt, func_def_stmt)
                }
                TypedStmt::FuncDecl(func_decl_stmt) => {
                    CIRStmt::FuncDecl(self.lower_func_decl(scope_id_opt, func_decl_stmt))
                }
                TypedStmt::Switch(switch_stmt) => self.lower_switch(scope_id_opt, switch_stmt),
                TypedStmt::Variable(var_stmt) => CIRStmt::Variable(self.lower_var(scope_id_opt, var_stmt)),
                TypedStmt::GlobalVar(global_var_stmt) => self.lower_global_var(scope_id_opt, global_var_stmt),
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
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        for module_id in methods.values() {
            let sym = self
                .resolver
                .resolve_local_or_global_symbol(local_scope_opt.clone(), *module_id)
                .unwrap();
            let resolved_method = sym.as_method().unwrap();

            let lowered_method = self.lower_method(scope_id_opt, resolved_method, object_name);

            stmts.push(lowered_method);
        }

        stmts
    }

    fn lower_method(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        resolved_method: &ResolvedMethod,
        object_name: &String,
    ) -> CIRStmt {
        let mangled_name = self
            .mangling
            .method_name(&"", object_name, &resolved_method.func_sig.name);

        let func_def = TypedFuncDefStmt {
            module_id: resolved_method.module_id,
            symbol_id: resolved_method.symbol_id,
            name: mangled_name,
            params: resolved_method.func_sig.params.clone(),
            generic_params: resolved_method.func_sig.generic_params.clone(),
            body: resolved_method.func_body.clone().unwrap(),
            return_type: resolved_method.func_sig.return_type.clone(),
            modifiers: resolved_method.func_sig.modifiers.clone(),
            loc: resolved_method.func_sig.loc.clone(),
        };

        self.lower_func_def(scope_id_opt, &func_def)
    }

    fn lower_label(&self, label: &TypedLabelStmt) -> CIRStmt {
        CIRStmt::Label(CIRLabelStmt {
            name: label.name.clone(),
            label_id: label.label_id,
        })
    }

    fn lower_goto(&self, scope_id_opt: Option<ScopeID>, goto: &TypedGotoStmt) -> CIRStmt {
        let local_scope_rc = self
            .resolver
            .get_scope_ref(self.module_id, scope_id_opt.unwrap())
            .unwrap();
        let local_scope_ref = local_scope_rc.borrow();
        let label_id = local_scope_ref.resolve_label(&goto.name).unwrap();
        drop(local_scope_ref);

        CIRStmt::Goto(CIRGotoStmt { label_id })
    }

    pub fn lower_export_tuple_to_vars(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        export_tuple: &TypedExportTupleStmt,
    ) -> Vec<CIRVarStmt> {
        let local_scope = self
            .resolver
            .get_scope_ref(self.module_id, scope_id_opt.unwrap())
            .unwrap();

        let mut vars = Vec::new();
        self.lower_export_pattern_recursive(scope_id_opt, &local_scope, &export_tuple.pattern, &mut vars);
        vars
    }

    fn lower_export_pattern_recursive(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_rc: &LocalScopeRef,
        pattern: &TypedExportPattern,
        vars: &mut Vec<CIRVarStmt>,
    ) {
        match pattern {
            TypedExportPattern::Identifier(symbol_id) => {
                let local_scope = local_scope_rc.borrow();
                let resolved_variable = local_scope
                    .with_symbol_id(*symbol_id, |local_symbol| local_symbol.as_variable().cloned().unwrap())
                    .unwrap();

                let var_name = resolved_variable.typed_variable.name.clone();
                let var_ty = self.lower_sema_ty(scope_id_opt, &resolved_variable.typed_variable.ty.as_ref().unwrap());
                let var_rhs = self.lower_expr(scope_id_opt, &resolved_variable.typed_variable.rhs.as_ref().unwrap());
                drop(local_scope);

                vars.push(CIRVarStmt {
                    irv_id: *symbol_id,
                    name: format!("tuple.{}", var_name),
                    ty: var_ty,
                    expr: Some(var_rhs),
                });
            }
            TypedExportPattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.lower_export_pattern_recursive(scope_id_opt, local_scope_rc, pattern, vars);
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
        let operand_ty = switch_stmt.operand.sema_ty.as_ref().unwrap().get_const_inner();

        let default = switch_stmt
            .default_case
            .as_ref()
            .and_then(|default_case| Some(self.lower_body(&default_case)));

        if operand_ty.as_generic_type().is_some() {
            return self.lower_switch_on_enum(scope_id_opt, &operand, &default, switch_stmt);
        }

        if operand_ty.is_enum() {
            self.lower_switch_on_enum(scope_id_opt, &operand, &default, switch_stmt)
        } else {
            if switch_stmt.includes_any_range() || !switch_stmt.includes_only_integer() {
                self.lower_switch_as_chained_if(scope_id_opt, &operand, &default, switch_stmt)
            } else {
                self.lower_pure_switch(scope_id_opt, &operand, &default, switch_stmt)
            }
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
        })
    }

    fn lower_switch_on_enum(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        operand: &CIRExpr,
        default: &Option<CIRBlockStmt>,
        switch_stmt: &TypedSwitchStmt,
    ) -> CIRStmt {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));
        let operand_ty = switch_stmt.operand.sema_ty.as_ref().unwrap().get_const_inner();

        let enum_sig = operand_ty
            .as_enum_symbol_id()
            .and_then(|symbol_id| {
                self.resolver
                    .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)
                    .unwrap()
                    .as_enum()
                    .cloned()
                    .map(|resolved_enum| resolved_enum.enum_sig)
            })
            .or(operand_ty.as_generic_type().and_then(|generic_type| {
                Some(
                    self.resolver
                        .resolve_local_or_global_symbol(local_scope_opt.clone(), generic_type.base)
                        .unwrap()
                        .as_enum()
                        .cloned()
                        .map(|resolved_enum| {
                            substitute_enum_sig(&resolved_enum.enum_sig, generic_type.mapping_ctx.clone()).unwrap()
                        })
                        .unwrap(),
                )
            }))
            .unwrap();

        let mut lowered_cases: Vec<CIRSwitchOnEnumCase> = Vec::new();

        for case in &switch_stmt.cases {
            let mut lowered_patterns: Vec<CIRSwitchOnEnumPattern> = Vec::new();

            for pattern in &case.patterns {
                match pattern {
                    TypedSwitchCasePattern::Identifier(identifier, _) => {
                        let variant_idx = enum_sig
                            .variants
                            .iter()
                            .position(|variant| variant.get_identifier().as_string() == *identifier)
                            .unwrap();
                        lowered_patterns.push(CIRSwitchOnEnumPattern::Identifier(variant_idx));
                    }
                    TypedSwitchCasePattern::EnumVariant(identifier, exported_fields, _) => {
                        let variant_idx = enum_sig
                            .variants
                            .iter()
                            .position(|variant| variant.get_identifier().as_string() == *identifier)
                            .unwrap();

                        let variant = &enum_sig.variants[variant_idx];

                        match variant {
                            TypedEnumVariant::Valued(_, expr) => {
                                let exported_field = exported_fields.first().unwrap();

                                let lowered_expr = self.lower_expr(scope_id_opt, &expr);
                                lowered_patterns.push(CIRSwitchOnEnumPattern::Valued(
                                    variant_idx,
                                    (exported_field.clone(), lowered_expr),
                                ));
                            }
                            TypedEnumVariant::Variant(_, valued_fields) => {
                                let typed_exported_fields: Vec<(TypedIdentifier, CIRTy)> = exported_fields
                                    .iter()
                                    .enumerate()
                                    .map(|(idx, identifier)| {
                                        let field_ty = &valued_fields.get(idx).as_ref().unwrap().ty;
                                        (identifier.clone(), self.lower_sema_ty(scope_id_opt, field_ty))
                                    })
                                    .collect();

                                lowered_patterns
                                    .push(CIRSwitchOnEnumPattern::ExportFields(variant_idx, typed_exported_fields));
                            }
                            TypedEnumVariant::Identifier(_) => unreachable!(),
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

    fn lower_global_var(&mut self, scope_id_opt: Option<ScopeID>, global_var: &TypedGlobalVarStmt) -> CIRStmt {
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

        let mangled_name = self
            .mangling
            .global_var_name(&self.program.module_name, &global_var.name);

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
                    TypedFuncParamKind::FuncParam(typed_func_param) => CIRFuncParam {
                        irv_id: typed_func_param.symbol_id,
                        ty: self.lower_sema_ty(scope_id_opt, &typed_func_param.ty),
                    },
                    TypedFuncParamKind::SelfModifier(typed_self_modifier) => CIRFuncParam {
                        irv_id: typed_self_modifier.self_symbol_id.unwrap(),
                        ty: self.lower_sema_ty(scope_id_opt, &typed_self_modifier.ty.as_ref().unwrap()),
                    },
                })
                .collect(),
            is_var: func_params.variadic.is_some(),
        }
    }

    fn lower_func_def(&mut self, scope_id_opt: Option<ScopeID>, func_def: &TypedFuncDefStmt) -> CIRStmt {
        let params = self.lower_func_params(scope_id_opt, &func_def.params);

        let body = self.lower_body(&func_def.body);
        let ret = self.lower_sema_ty(scope_id_opt, &func_def.return_type);

        let mangled_name = self
            .mangling
            .func_name(&self.program.module_name, &func_def.name, false);

        CIRStmt::FuncDef(CIRFuncDefStmt {
            irv_id: func_def.symbol_id,
            name: mangled_name,
            params,
            body: Box::new(body),
            ret,
            modifiers: func_def.modifiers.clone(),
        })
    }

    fn lower_func_decl(&mut self, scope_id_opt: Option<ScopeID>, func_decl: &TypedFuncDeclStmt) -> CIRFuncDeclStmt {
        let params = self.lower_func_params(scope_id_opt, &func_decl.params);
        let ret = self.lower_sema_ty(scope_id_opt, &func_decl.return_type);

        let func_name = func_decl.renamed_as.as_ref().unwrap_or(&func_decl.name);
        let mangled_name = self.mangling.func_name(&self.program.module_name, &func_name, true);

        CIRFuncDeclStmt {
            irv_id: func_decl.symbol_id,
            name: mangled_name,
            params,
            ret,
            modifiers: func_decl.modifiers.clone(),
        }
    }

    pub(crate) fn lower_func_sig(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        irv_id: IRValueID,
        func_sig: &FuncSig,
    ) -> CIRFuncDeclStmt {
        let params = self.lower_func_params(scope_id_opt, &func_sig.params);
        let ret = self.lower_sema_ty(scope_id_opt, &func_sig.return_type);

        CIRFuncDeclStmt {
            irv_id,
            name: func_sig.name.clone(),
            params,
            ret,
            modifiers: func_sig.modifiers.clone(),
        }
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
            TypedExprKind::UStructValue(ustruct_value) => self.lower_ustruct_value(scope_id_opt, ustruct_value),
            TypedExprKind::FuncCall(func_call) => self.lower_func_call(scope_id_opt, func_call),
            TypedExprKind::MethodCall(method_call) => self.lower_method_call(scope_id_opt, method_call),
            TypedExprKind::FieldAccess(field_access) => self.lower_field_access(scope_id_opt, field_access),
            TypedExprKind::StructInit(struct_init_expr) => self.lower_struct_init(scope_id_opt, struct_init_expr),
            TypedExprKind::SizeOf(size_of_expr) => self.lower_size_of(scope_id_opt, size_of_expr),
            TypedExprKind::Lambda(lambda_expr) => self.lower_lambda(scope_id_opt, lambda_expr),
            TypedExprKind::Tuple(tuple_expr) => self.lower_tuple(scope_id_opt, tuple_expr),
            TypedExprKind::TupleAccess(tuple_access_expr) => self.lower_tuple_access(scope_id_opt, tuple_access_expr),
            // skipped
            TypedExprKind::SemanticType(..) => unreachable!(),
        };

        CIRExpr { kind, ty }
    }

    pub(crate) fn lower_load_symbol(&mut self, scope_id_opt: Option<ScopeID>, symbol_id: SymbolID) -> CIRExprKind {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .unwrap();

        if let Some(resolved_func) = sym.as_func() {
            let func_decl = self.lower_func_sig(scope_id_opt, resolved_func.symbol_id, &resolved_func.func_sig);
            CIRExprKind::Load(CIRValue {
                irv_id: resolved_func.symbol_id,
                kind: CIRValueKind::Func(Box::new(func_decl)),
            })
        } else if let Some(resolved_global_var) = sym.as_global_var() {
            let global_var_stmt = self.lower_global_var_sig(
                scope_id_opt,
                resolved_global_var.symbol_id,
                &resolved_global_var.global_var_sig,
            );
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

    pub(crate) fn lower_struct_init(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        struct_init_expr: &TypedStructInitExpr,
    ) -> CIRExprKind {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, struct_init_expr.symbol_id)
            .unwrap();

        if let Some(resolved_struct) = sym.as_struct() {
            let fields_tys: Vec<CIRTy> = struct_init_expr
                .fields
                .iter()
                .map(|field| self.lower_sema_ty(scope_id_opt, &field.value.sema_ty.clone().unwrap()))
                .collect();

            let struct_ty = CIRStructTy {
                fields: fields_tys,
                is_packed: resolved_struct.struct_sig.is_packed,
            };

            let fields: Vec<CIRExpr> = struct_init_expr
                .fields
                .iter()
                .map(|field| self.lower_expr(scope_id_opt, &field.value))
                .collect();

            CIRExprKind::StructInit(CIRStructInitExpr { ty: struct_ty, fields })
        } else if let Some(_) = sym.as_union() {
            let struct_field_init = struct_init_expr.fields.first().unwrap();
            let expr = Box::new(self.lower_expr(scope_id_opt, &struct_field_init.value));

            CIRExprKind::UnionInit(CIRUnionInitExpr { expr })
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

        CIRExprKind::Lambda(CIRLambda {
            irv_id: generate_symbol_id(),
            params,
            ret,
            body,
        })
    }

    fn lower_method_call(&mut self, scope_id_opt: Option<ScopeID>, method_call: &TypedMethodCall) -> CIRExprKind {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), method_call.object_symbol_id.unwrap())
            .unwrap();

        if let Some(resolved_enum) = sym.as_enum() {
            let sema_ty = method_call.operand.sema_ty.as_ref().unwrap();

            let typed_variant_idx = resolved_enum
                .enum_sig
                .variants
                .iter()
                .position(|variant| variant.get_identifier().as_string() == method_call.method_name)
                .unwrap();
            let typed_variant = resolved_enum.enum_sig.variants.get(typed_variant_idx).unwrap();

            let variant: CIREnumInitVariant;
            let enum_ty: CIREnumTy;
            if let Some(generic_type) = sema_ty.as_generic_type() {
                let enum_sig = substitute_enum_sig(&resolved_enum.enum_sig, generic_type.mapping_ctx.clone()).unwrap();
                enum_ty = self.lower_enum_sig_as_enum_ty(scope_id_opt, &enum_sig);
            } else {
                enum_ty = self.lower_enum_sig_as_enum_ty(scope_id_opt, &resolved_enum.enum_sig);
            }

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

            CIRExprKind::EnumInit(CIREnumInitExpr {
                tag: typed_variant_idx,
                variant,
                enum_ty,
            })
        } else {
            let method_symbol_id = sym.get_method_symbol_id_by_name(&method_call.method_name).unwrap();
            let sym = self
                .resolver
                .resolve_local_or_global_symbol(local_scope_opt, method_symbol_id)
                .unwrap();
            let resolved_method = sym.as_method().unwrap();

            let mut args: Vec<CIRExpr> = method_call
                .args
                .iter()
                .map(|arg| self.lower_expr(scope_id_opt, arg))
                .collect();

            if resolved_method.is_instance_method() {
                let first_param = resolved_method.func_sig.params.list.first().unwrap();
                args.insert(
                    0,
                    self.lower_method_self_as_argument(
                        scope_id_opt,
                        &method_call.operand,
                        method_call.is_fat_arrow,
                        first_param,
                    ),
                );
            }

            let ret_ty = self.lower_sema_ty(scope_id_opt, &method_call.return_type.clone().unwrap());

            let func_decl = typed_func_decl_from_func_sig(&resolved_method.func_sig);
            let cir_func_decl = self.lower_func_decl(scope_id_opt, &func_decl);

            if let Some(monomorph_key) = &method_call.monomorph_key {
                self.insert_monomorph_func_instance(scope_id_opt, monomorph_key);

                CIRExprKind::MonomorphFuncInstanceCall(CIRMonomorphFuncInstanceCall {
                    monomorph_key: monomorph_key.clone(),
                    args,
                    ret_ty,
                })
            } else {
                let operand = Box::new(CIRExpr {
                    kind: CIRExprKind::Load(CIRValue {
                        irv_id: method_call.method_symbol_id.unwrap(),
                        kind: CIRValueKind::Func(Box::new(cir_func_decl)),
                    }),
                    ty: ret_ty.clone(),
                });

                CIRExprKind::FuncCall(CIRFuncCall { operand, args, ret_ty })
            }
        }
    }

    fn lower_method_self_as_argument(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        operand: &TypedExprStmt,
        is_fat_arrow: bool,
        first_param: &TypedFuncParamKind,
    ) -> CIRExpr {
        let expr = self.lower_expr(scope_id_opt, operand);
        let self_modifier = first_param.as_self_modifier().unwrap();

        match self_modifier.kind {
            SelfModifierKind::Copied => expr,
            SelfModifierKind::Referenced => {
                // only take address if not a fat arrow
                if is_fat_arrow {
                    expr
                } else {
                    let expr_ty = expr.ty.clone();
                    CIRExpr {
                        kind: CIRExprKind::AddrOf(CIRAddrOfExpr {
                            operand: Box::new(expr),
                        }),
                        ty: CIRTy::Pointer(Box::new(expr_ty)),
                    }
                }
            }
        }
    }

    fn lower_field_access(&mut self, scope_id_opt: Option<ScopeID>, field_access: &TypedFieldAccess) -> CIRExprKind {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        if field_access
            .operand
            .sema_ty
            .as_ref()
            .unwrap()
            .as_unnamed_struct()
            .is_some()
        {
            return CIRExprKind::StructFieldAccess(CIRStructFieldAccessExpr {
                operand: Box::new(self.lower_expr(scope_id_opt, &field_access.operand)),
                field_idx: field_access.field_index.unwrap(),
                field_ty: self.lower_sema_ty(scope_id_opt, &field_access.field_ty.as_ref().unwrap()),
            });
        }

        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, field_access.object_symbol_id.unwrap())
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
                resolved_enum.enum_sig =
                    substitute_enum_sig(&resolved_enum.enum_sig, generic_type.mapping_ctx.clone()).unwrap();
            }

            let typed_variant = resolved_enum
                .enum_sig
                .variants
                .get(field_access.field_index.unwrap())
                .unwrap();

            variant = match typed_variant {
                TypedEnumVariant::Identifier(..) => CIREnumInitVariant::Identifier,
                TypedEnumVariant::Valued(_, expr) => {
                    CIREnumInitVariant::Valued(Box::new(self.lower_expr(scope_id_opt, expr)))
                }
                TypedEnumVariant::Variant(..) => unreachable!(),
            };

            enum_ty = self.lower_enum_sig_as_enum_ty(scope_id_opt, &resolved_enum.enum_sig);

            CIRExprKind::EnumInit(CIREnumInitExpr {
                tag: field_access.field_index.unwrap(),
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
            self.insert_monomorph_func_instance(scope_id_opt, monomorph_key);

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

    fn lower_ustruct_value(&mut self, scope_id_opt: Option<ScopeID>, ustruct_value: &TypedUStructValue) -> CIRExprKind {
        let fields: Vec<CIRExpr> = ustruct_value
            .fields
            .iter()
            .map(|field| self.lower_expr(scope_id_opt, &field.field_value))
            .collect();

        let field_tys = ustruct_value
            .unnamed_struct_type
            .clone()
            .unwrap()
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(scope_id_opt, &field.field_ty))
            .collect();

        let struct_ty = CIRStructTy {
            fields: field_tys,
            is_packed: ustruct_value.is_packed,
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
            ty: self.lower_sema_ty(scope_id_opt, &array.array_type),
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
            SemanticType::UnnamedStruct(ustruct_ty) => {
                let field_tys: Vec<CIRTy> = ustruct_ty
                    .fields
                    .iter()
                    .map(|field| self.lower_sema_ty(scope_id_opt, &field.field_ty))
                    .collect();
                CIRTy::Struct(CIRStructTy {
                    fields: field_tys,
                    is_packed: ustruct_ty.is_packed,
                })
            }
            SemanticType::FuncType(func_type) => {
                let ret = Box::new(self.lower_sema_ty(scope_id_opt, &func_type.return_type));
                let params = self.lower_func_type_params(scope_id_opt, &func_type.params);

                CIRTy::FuncType(CIRFuncTy {
                    params: params,
                    is_var: func_type.params.variadic.is_some(),
                    ret,
                })
            }
            SemanticType::Tuple(tuple_type) => {
                let items: Vec<CIRTy> = tuple_type
                    .type_list
                    .iter()
                    .map(|sema_ty| self.lower_sema_ty(scope_id_opt, sema_ty))
                    .collect();

                CIRTy::Tuple(CIRTupleTy { items })
            }
            SemanticType::GenericType(generic_type) => self.lower_generic_type(scope_id_opt, generic_type.clone()),
            SemanticType::GenericParam(_) => unreachable!("Unexpected generic param which is not resolved."),
            SemanticType::UnresolvedSymbol(_) => unreachable!("Unexpected unresolved symbol."),
        }
    }

    fn lower_generic_type(&mut self, scope_id_opt: Option<ScopeID>, mut generic_type: GenericType) -> CIRTy {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, generic_type.base)
            .unwrap();

        let generic_params = sym.get_generic_params().unwrap();
        match generic_type.init(generic_params) {
            Ok(val) => val,
            Err(e) => {
                eprintln!("Failed to init generic type: {:?}.", e.kind.to_string())
            }
        }

        if let Some(resolved_struct) = sym.as_struct() {
            let struct_sig = substitute_struct_sig(&resolved_struct.struct_sig, generic_type.mapping_ctx).unwrap();
            let cir_struct_ty = self.lower_struct_sig_as_struct_ty(scope_id_opt, &struct_sig);
            CIRTy::Struct(cir_struct_ty)
        } else if let Some(resolved_enum) = sym.as_enum() {
            let enum_sig = substitute_enum_sig(&resolved_enum.enum_sig, generic_type.mapping_ctx).unwrap();
            let cir_enum_ty = self.lower_enum_sig_as_enum_ty(scope_id_opt, &enum_sig);
            CIRTy::Enum(cir_enum_ty)
        } else if let Some(resolved_union) = sym.as_union() {
            let union_sig = substitute_union_sig(&resolved_union.union_sig, generic_type.mapping_ctx).unwrap();
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
            is_packed: struct_sig.is_packed,
        }
    }

    fn lower_enum_ty_variant(&mut self, scope_id_opt: Option<ScopeID>, variant: &TypedEnumVariant) -> CIREnumTyVariant {
        match variant {
            TypedEnumVariant::Identifier(..) => CIREnumTyVariant::Identifier,
            TypedEnumVariant::Valued(_, expr) => {
                CIREnumTyVariant::Valued(Box::new(self.lower_expr(scope_id_opt, expr)))
            }
            TypedEnumVariant::Variant(_, fields) => {
                let fields: Vec<CIRTy> = fields
                    .iter()
                    .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
                    .collect();
                CIREnumTyVariant::Fielded(fields)
            }
        }
    }

    fn lower_enum_sig_as_enum_ty(&mut self, scope_id_opt: Option<ScopeID>, enum_sig: &EnumSig) -> CIREnumTy {
        let variants: Vec<CIREnumTyVariant> = enum_sig
            .variants
            .iter()
            .map(|variant| self.lower_enum_ty_variant(scope_id_opt, variant))
            .collect();

        CIREnumTy { variants }
    }

    fn lower_union_sig_as_union_ty(&mut self, scope_id_opt: Option<ScopeID>, union_sig: &UnionSig) -> CIRUnionTy {
        let fields: Vec<CIRTy> = union_sig
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
            .collect();

        CIRUnionTy { fields }
    }

    fn lower_resolved_symbol(&mut self, scope_id_opt: Option<ScopeID>, resolved_symbol: &ResolvedSymbol) -> CIRTy {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, resolved_symbol.get_symbol_id())
            .unwrap();

        if let Some(resolved_struct) = sym.as_struct() {
            CIRTy::Struct(self.lower_struct_sig_as_struct_ty(scope_id_opt, &resolved_struct.struct_sig))
        } else if let Some(resolved_union) = sym.as_union() {
            CIRTy::Union(self.lower_union_sig_as_union_ty(scope_id_opt, &resolved_union.union_sig))
        } else if let Some(resolved_enum) = sym.as_enum() {
            let variants = resolved_enum
                .enum_sig
                .variants
                .iter()
                .map(|variant| self.lower_enum_ty_variant(scope_id_opt, variant))
                .collect();

            CIRTy::Enum(CIREnumTy { variants })
        } else {
            unreachable!()
        }
    }
}

pub fn walk_program_trees_in_parallel(
    threads: Option<usize>,
    program_trees: Vec<Box<TypedProgramTree>>,
    resolver: &Resolver,
    cir_monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
    mangling: &dyn ABINameMangling,
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
            .map(|program_tree| {
                let mut cir_walk = CIRWalk::new(
                    program_tree.clone(),
                    resolver,
                    program_tree.module_id,
                    cir_monomorph_registry.clone(),
                    mangling,
                );
                let cir_program_tree = cir_walk.run_pass(program_tree.file_path);
                Box::new(cir_program_tree)
            })
            .collect()
    })
}
