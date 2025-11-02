use crate::types::{CIRArrayTy, CIRFuncTy, CIRStructTy, CIRTupleTy, CIRTy};
use crate::*;
use cyrusc_ast::LiteralKind;
use cyrusc_resolver::{Resolver, typed_func_params_as_func_type_params};
use cyrusc_tast::generics::generic_type::GenericType;
use cyrusc_tast::generics::substitute::{substitute_enum_sig, substitute_struct_sig, substitute_union_sig};
use cyrusc_tast::sigs::{EnumSig, UnionSig};
use cyrusc_tast::types::ResolvedSymbol;
use cyrusc_tast::{ModuleID, ScopeID};
use cyrusc_tast::{
    TypedProgramTree,
    exprs::*,
    stmts::*,
    types::{SemanticType, TypedArrayCapacity, TypedArrayFixedCapacityValue},
};

struct CIRWalk<'resolver> {
    program: Box<TypedProgramTree>,
    resolver: &'resolver Resolver,
    module_id: ModuleID,
}

impl<'resolver> CIRWalk<'resolver> {
    pub fn new(program: Box<TypedProgramTree>, resolver: &'resolver Resolver, module_id: ModuleID) -> Self {
        Self {
            program,
            resolver,
            module_id,
        }
    }

    pub fn run_pass(&self, file_path: String) -> CIRProgramTree {
        let stmts = self.lower_stmts(None, &self.program.body);
        CIRProgramTree { body: stmts, file_path }
    }

    fn lower_stmts(&self, scope_id_opt: Option<ScopeID>, stmts: &Vec<TypedStmt>) -> Vec<CIRStmt> {
        let mut lowered_stmts: Vec<CIRStmt> = Vec::new();

        for stmt in stmts {
            let lowered_stmt = match stmt {
                TypedStmt::FuncDef(func_def_stmt) => self.lower_func_def(scope_id_opt, func_def_stmt),
                TypedStmt::FuncDecl(func_decl_stmt) => self.lower_func_decl(scope_id_opt, func_decl_stmt),
                TypedStmt::Switch(switch_stmt) => self.lower_switch(scope_id_opt, switch_stmt),
                TypedStmt::Variable(var_stmt) => CIRStmt::Variable(self.lower_var(scope_id_opt, var_stmt)),
                TypedStmt::GlobalVariable(global_var_stmt) => self.lower_global_var(scope_id_opt, global_var_stmt),
                TypedStmt::BlockStatement(block_stmt) => CIRStmt::Block(self.lower_block(block_stmt)),
                TypedStmt::If(if_stmt) => self.lower_if(scope_id_opt, if_stmt),
                TypedStmt::Return(return_stmt) => self.lower_return(scope_id_opt, return_stmt),
                TypedStmt::Break(break_stmt) => self.lower_break(break_stmt),
                TypedStmt::Continue(continue_stmt) => self.lower_continue(continue_stmt),
                TypedStmt::For(for_stmt) => self.lower_for(scope_id_opt, for_stmt),
                TypedStmt::While(while_stmt) => self.lower_while(scope_id_opt, while_stmt),
                TypedStmt::ExportTuple(export_tuple_stmt) => self.lower_export_tuple(export_tuple_stmt),
                // lowered only when used
                TypedStmt::Struct(..) | TypedStmt::Enum(..) | TypedStmt::Union(..) => {
                    continue;
                }
                // skipped
                TypedStmt::Interface(..)
                | TypedStmt::Expression(..)
                | TypedStmt::Defer(..)
                | TypedStmt::Typedef(..) => continue,
            };

            lowered_stmts.push(lowered_stmt);
        }

        lowered_stmts
    }

    fn lower_export_tuple(&self, export_tuple: &TypedExportTupleStmt) -> CIRStmt {
        todo!();
        // CIRStmt::ExportTuple(CIRExportTupleStmt {
        // })
    }

    fn lower_union(&self, scope_id_opt: Option<ScopeID>, union_stmt: &TypedUnionStmt) -> CIRStmt {
        let fields: Vec<CIRTy> = union_stmt
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
            .collect();

        CIRStmt::Union(CIRUnionStmt {
            name: union_stmt.name.clone(),
            fields,
            vis: union_stmt.vis.clone(),
        })
    }

    fn lower_struct(&self, scope_id_opt: Option<ScopeID>, struct_stmt: &TypedStructStmt) -> CIRStmt {
        let fields: Vec<CIRTy> = struct_stmt
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
            .collect();

        CIRStmt::Struct(CIRStructStmt {
            name: struct_stmt.name.clone(),
            is_packed: struct_stmt.is_packed,
            fields,
            vis: struct_stmt.vis.clone(),
        })
    }

    fn lower_enum_variant(&self, scope_id_opt: Option<ScopeID>, variant: &TypedEnumVariant) -> CIREnumVariant {
        match variant {
            TypedEnumVariant::Identifier(..) => CIREnumVariant::Ident,
            TypedEnumVariant::Valued(_, expr) => CIREnumVariant::Valued(Box::new(self.lower_expr(scope_id_opt, expr))),
            TypedEnumVariant::Variant(_, fielded) => {
                let fields: Vec<CIRTy> = fielded
                    .iter()
                    .map(|field| self.lower_sema_ty(scope_id_opt, &field.field_ty))
                    .collect();
                CIREnumVariant::Fielded(fields)
            }
        }
    }

    fn lower_enum(&self, scope_id_opt: Option<ScopeID>, enum_stmt: &TypedEnumStmt) -> CIRStmt {
        let variants: Vec<CIREnumVariant> = enum_stmt
            .variants
            .iter()
            .map(|variant| self.lower_enum_variant(scope_id_opt, variant))
            .collect();

        CIRStmt::Enum(CIREnumStmt {
            name: enum_stmt.name.clone(),
            variants,
            vis: enum_stmt.vis.clone(),
        })
    }

    fn lower_switch(&self, scope_id_opt: Option<ScopeID>, switch_stmt: &TypedSwitchStmt) -> CIRStmt {
        todo!();
    }

    fn lower_while(&self, scope_id_opt: Option<ScopeID>, while_stmt: &TypedWhileStmt) -> CIRStmt {
        let cond = Box::new(self.lower_expr(scope_id_opt, &while_stmt.cond));
        let body = Box::new(self.lower_block(&while_stmt.body));

        todo!();
    }

    fn lower_for(&self, scope_id_opt: Option<ScopeID>, for_stmt: &TypedForStmt) -> CIRStmt {
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

        let body = Box::new(self.lower_block(&for_stmt.body));

        // CIRStmt::For(CIRForStmt {
        //     initializer,
        //     cond,
        //     increment,
        //     body,
        // })
        todo!();
    }

    fn lower_break(&self, _: &TypedBreakStmt) -> CIRStmt {
        todo!();
    }

    fn lower_continue(&self, _: &TypedContinueStmt) -> CIRStmt {
        todo!();
    }

    fn lower_return(&self, scope_id_opt: Option<ScopeID>, ret: &TypedReturnStmt) -> CIRStmt {
        let arg = ret
            .arg
            .clone()
            .and_then(|arg| Some(self.lower_expr(scope_id_opt, &arg)));
        CIRStmt::Return(CIRReturnStmt { arg })
    }

    fn lower_if(&self, scope_id_opt: Option<ScopeID>, if_stmt: &TypedIfStmt) -> CIRStmt {
        let cond = self.lower_expr(scope_id_opt, &if_stmt.cond);
        let then_block = Box::new(self.lower_block(&if_stmt.then_block));
        let else_block = if_stmt
            .else_block
            .clone()
            .and_then(|else_block| Some(Box::new(self.lower_block(&else_block))));
        // let branches: Vec<CIRIfStmt> = if_stmt
        //     .branches
        //     .iter()
        //     .map(|branch| self.lower_if(scope_id_opt, branch))
        //     .collect();

        
        todo!();
    }

    fn lower_global_var(&self, scope_id_opt: Option<ScopeID>, global_var: &TypedGlobalVarStmt) -> CIRStmt {
        let ty = self.lower_sema_ty(
            scope_id_opt,
            &global_var
                .ty
                .clone()
                .unwrap_or(global_var.expr.clone().unwrap().sema_ty.unwrap()),
        );

        let expr = global_var
            .expr
            .clone()
            .and_then(|expr| Some(self.lower_expr(scope_id_opt, &expr)));

        CIRStmt::GlobalVar(CIRGlobalVarStmt {
            irv_id: global_var.symbol_id,
            name: global_var.name.clone(),
            ty,
            expr,
            vis: global_var.vis.clone(),
        })
    }

    fn lower_var(&self, scope_id_opt: Option<ScopeID>, var: &TypedVarStmt) -> CIRVarStmt {
        let ty = var
            .ty
            .as_ref()
            .or_else(|| var.rhs.as_ref()?.sema_ty.as_ref())
            .map(|ty| self.lower_sema_ty(scope_id_opt, ty))
            .unwrap_or_else(|| {
                panic!(
                    "variable '{}' has neither explicit type nor rhs type ({}:{})",
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

    fn lower_func_params(
        &self,
        scope_id_opt: Option<ScopeID>,
        func_params: &TypedFuncTypeParams,
    ) -> (Vec<CIRTy>, bool) {
        let mut params: Vec<CIRTy> = Vec::new();

        func_params
            .list
            .iter()
            .for_each(|sema_ty| params.push(self.lower_sema_ty(scope_id_opt, &sema_ty)));

        let mut is_var = false;
        if let Some(variadic) = &func_params.variadic {
            is_var = true;

            match **variadic {
                TypedFuncTypeVariadicParams::UntypedCStyle => {}
                TypedFuncTypeVariadicParams::Typed(_) => todo!(), // TODO
            }
        }

        (params, is_var)
    }

    fn lower_func_def(&self, scope_id_opt: Option<ScopeID>, func_def: &TypedFuncDefStmt) -> CIRStmt {
        let func_params = typed_func_params_as_func_type_params(&func_def.params);
        let (params, is_var) = self.lower_func_params(scope_id_opt, &func_params);

        let body = self.lower_block(&func_def.body);
        let ret = self.lower_sema_ty(scope_id_opt, &func_def.return_type);

        CIRStmt::FuncDef(CIRFuncDefStmt {
            irv_id: func_def.symbol_id,
            name: func_def.name.clone(),
            params,
            is_var,
            body: Box::new(body),
            ret,
            vis: func_def.vis.clone(),
        })
    }

    fn lower_func_decl(&self, scope_id_opt: Option<ScopeID>, func_decl: &TypedFuncDeclStmt) -> CIRStmt {
        let func_params = typed_func_params_as_func_type_params(&func_decl.params);
        let (params, is_var) = self.lower_func_params(scope_id_opt, &func_params);
        let ret = self.lower_sema_ty(scope_id_opt, &func_decl.return_type);

        CIRStmt::FuncDecl(CIRFuncDeclStmt {
            irv_id: func_decl.symbol_id,
            name: func_decl.name.clone(),
            params,
            is_var,
            ret,
            vis: func_decl.vis.clone(),
        })
    }

    fn lower_block(&self, block: &TypedBlockStmt) -> CIRBlockStmt {
        let mut stmts = self.lower_stmts(Some(block.scope_id), &block.stmts);
        let defer_stmts: Vec<TypedStmt> = block.defers.iter().map(|defer| *defer.operand.clone()).collect();
        stmts.extend(self.lower_stmts(Some(block.scope_id), &defer_stmts));
        CIRBlockStmt { stmts }
    }

    // exprs

    fn lower_expr(&self, scope_id_opt: Option<ScopeID>, expr: &TypedExprStmt) -> CIRExpr {
        let ty = self.lower_sema_ty(scope_id_opt, &expr.sema_ty.clone().unwrap());

        let kind = match &expr.kind {
            TypedExprKind::Symbol(symbol_id, ..) => CIRExprKind::Load(CIRValueRef { irv_id: *symbol_id }),
            TypedExprKind::Literal(literal_expr) => self.lower_literal(literal_expr),
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
            TypedExprKind::FieldAccess(field_access) => todo!(),
            TypedExprKind::StructInit(struct_init_expr) => todo!(),
            TypedExprKind::SizeOf(size_of_expr) => self.lower_size_of(scope_id_opt, size_of_expr),
            TypedExprKind::Lambda(lambda_expr) => self.lower_lambda(lambda_expr),
            TypedExprKind::Tuple(tuple_expr) => self.lower_tuple(scope_id_opt, tuple_expr),
            TypedExprKind::TupleAccess(tuple_access_expr) => self.lower_tuple_access(scope_id_opt, tuple_access_expr),
            // skipped
            TypedExprKind::SemanticType(..) => unreachable!(),
        };

        CIRExpr { kind, ty }
    }

    fn lower_tuple_access(&self, scope_id_opt: Option<ScopeID>, tuple_access: &TypedTupleAccessExpr) -> CIRExprKind {
        let operand = Box::new(self.lower_expr(scope_id_opt, &tuple_access.operand));
        CIRExprKind::TupleAccess(CIRTupleAccessExpr {
            operand,
            index: tuple_access.index,
        })
    }

    fn lower_tuple(&self, scope_id_opt: Option<ScopeID>, tuple: &TypedTupleExpr) -> CIRExprKind {
        let elms: Vec<CIRExpr> = tuple
            .expr_list
            .iter()
            .map(|expr| self.lower_expr(scope_id_opt, expr))
            .collect();
        CIRExprKind::Tuple(CIRTupleExpr { elms })
    }

    fn lower_lambda(&self, lambda: &TypedLambdaExpr) -> CIRExprKind {
        todo!();
    }

    fn lower_method_call(&self, scope_id_opt: Option<ScopeID>, method_call: &TypedMethodCall) -> CIRExprKind {
        let args: Vec<CIRExpr> = method_call
            .args
            .iter()
            .map(|arg| self.lower_expr(scope_id_opt, arg))
            .collect();

        CIRExprKind::FuncCall(CIRFuncCall {
            operand: Box::new(self.lower_expr(scope_id_opt, &method_call.operand)),
            args,
        })
    }

    fn lower_field_access(&self, field_access: &TypedFieldAccess) -> CIRExprKind {
        // TODO Separate struct_field access from union and enum!
        todo!()

        // CIRExprKind::StructFieldAccess(CIRStructFieldAccessExpr {
        //     operand: todo!(),
        //     field_idx: todo!(),
        //     field_ty: todo!(),
        // })
    }

    fn lower_func_call(&self, scope_id_opt: Option<ScopeID>, func_call: &TypedFuncCall) -> CIRExprKind {
        let args: Vec<CIRExpr> = func_call
            .args
            .iter()
            .map(|arg| self.lower_expr(scope_id_opt, arg))
            .collect();

        CIRExprKind::FuncCall(CIRFuncCall {
            operand: Box::new(self.lower_expr(scope_id_opt, &func_call.operand)),
            args,
        })
    }

    fn lower_ustruct_value(&self, scope_id_opt: Option<ScopeID>, ustruct_value: &TypedUStructValue) -> CIRExprKind {
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

    fn lower_array_index(&self, scope_id_opt: Option<ScopeID>, array_index: &TypedArrayIndexExpr) -> CIRExprKind {
        CIRExprKind::ArrayIndex(CIRArrayIndexExpr {
            operand: Box::new(self.lower_expr(scope_id_opt, &array_index.operand)),
            index: Box::new(self.lower_expr(scope_id_opt, &array_index.index)),
        })
    }

    fn lower_array(&self, scope_id_opt: Option<ScopeID>, array: &TypedArrayExpr) -> CIRExprKind {
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

    fn lower_deref(&self, scope_id_opt: Option<ScopeID>, deref: &TypedDerefExpr) -> CIRExprKind {
        CIRExprKind::Deref(CIRDerefExpr {
            operand: Box::new(self.lower_expr(scope_id_opt, &deref.operand)),
        })
    }

    fn lower_addr_of(&self, scope_id_opt: Option<ScopeID>, addr_of: &TypedAddrOfExpr) -> CIRExprKind {
        CIRExprKind::AddrOf(CIRAddrOfExpr {
            operand: Box::new(self.lower_expr(scope_id_opt, &addr_of.operand)),
        })
    }

    fn lower_size_of(&self, scope_id_opt: Option<ScopeID>, sizeof: &TypedSizeOfExpr) -> CIRExprKind {
        CIRExprKind::SizeOf(CIRSizeOfExpr {
            operand: Box::new(self.lower_expr(scope_id_opt, &sizeof.operand)),
        })
    }

    fn lower_cast(&self, scope_id_opt: Option<ScopeID>, cast: &TypedCastExpr) -> CIRExprKind {
        CIRExprKind::Cast(CIRCastExpr {
            operand: Box::new(self.lower_expr(scope_id_opt, &cast.operand)),
            ty: Box::new(self.lower_sema_ty(scope_id_opt, &cast.target_type)),
        })
    }

    fn lower_assign(&self, scope_id_opt: Option<ScopeID>, assign: &TypedAssignExpr) -> CIRExprKind {
        CIRExprKind::Assign(CIRAssignExpr {
            lhs: Box::new(self.lower_expr(scope_id_opt, &assign.lhs)),
            rhs: Box::new(self.lower_expr(scope_id_opt, &assign.rhs)),
        })
    }

    fn lower_unary(&self, scope_id_opt: Option<ScopeID>, unary: &TypedUnaryExpr) -> CIRExprKind {
        CIRExprKind::Unary(CIRUnaryExpr {
            op: unary.op.clone(),
            operand: Box::new(self.lower_expr(scope_id_opt, &unary.operand)),
        })
    }

    fn lower_infix(&self, scope_id_opt: Option<ScopeID>, infix: &TypedInfixExpr) -> CIRExprKind {
        CIRExprKind::Infix(CIRInfixExpr {
            op: infix.op.clone(),
            lhs: Box::new(self.lower_expr(scope_id_opt, &infix.lhs)),
            rhs: Box::new(self.lower_expr(scope_id_opt, &infix.rhs)),
        })
    }

    fn lower_prefix(&self, scope_id_opt: Option<ScopeID>, prefix: &TypedPrefixExpr) -> CIRExprKind {
        CIRExprKind::Prefix(CIRPrefixExpr {
            op: prefix.op.clone(),
            operand: Box::new(self.lower_expr(scope_id_opt, &prefix.operand)),
        })
    }

    fn lower_literal(&self, literal: &TypedLiteralExpr) -> CIRExprKind {
        let literal = match &literal.kind {
            LiteralKind::Integer(value, ..) => CIRLiteral::Integer(*value),
            LiteralKind::Float(value, ..) => CIRLiteral::Float(*value),
            LiteralKind::Bool(value) => CIRLiteral::Bool(*value),
            LiteralKind::Char(value) => CIRLiteral::Char(*value),
            LiteralKind::Null => CIRLiteral::Null,
            LiteralKind::String(value, prefix_opt) => CIRLiteral::String(value.clone(), prefix_opt.clone()),
        };

        CIRExprKind::Literal(literal)
    }

    // types

    fn lower_sema_ty(&self, scope_id_opt: Option<ScopeID>, sema_ty: &SemanticType) -> CIRTy {
        match sema_ty {
            SemanticType::ResolvedSymbol(resolved_symbol) => self.lower_resolved_symbol(resolved_symbol),
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
                CIRTy::Array(CIRArrayTy { ty: Box::new(ty), len })
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
                let (params, is_var) = self.lower_func_params(scope_id_opt, &func_type.params);

                CIRTy::FuncType(CIRFuncTy { params, is_var, ret })
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
            SemanticType::UnresolvedSymbol(_) => {
                unreachable!()
            }
            SemanticType::GenericParam(typed_identifier) => {
                dbg!(typed_identifier.clone());
                todo!();
            }
        }
    }

    fn lower_generic_type(&self, scope_id_opt: Option<ScopeID>, mut generic_type: GenericType) -> CIRTy {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, generic_type.base)
            .unwrap();
        let generic_params = sym.get_generic_params().unwrap();
        generic_type.init(generic_params).unwrap();

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

    fn lower_struct_sig_as_struct_ty(&self, scope_id_opt: Option<ScopeID>, struct_sig: &StructSig) -> CIRStructTy {
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

    fn lower_enum_sig_as_enum_ty(&self, scope_id_opt: Option<ScopeID>, enum_sig: &EnumSig) -> CIREnumTy {
        let variants: Vec<CIREnumVariant> = enum_sig
            .variants
            .iter()
            .map(|variant| self.lower_enum_variant(scope_id_opt, variant))
            .collect();

        CIREnumTy { variants }
    }

    fn lower_union_sig_as_union_ty(&self, scope_id_opt: Option<ScopeID>, union_sig: &UnionSig) -> CIRUnionTy {
        let fields: Vec<CIRTy> = union_sig
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(scope_id_opt, &field.ty))
            .collect();

        CIRUnionTy { fields }
    }

    fn lower_resolved_symbol(&self, resolved_symbol: &ResolvedSymbol) -> CIRTy {
        // resolved_symbol.get_symbol_id()

        todo!();
    }
}

pub fn walk_program_trees_in_parallel(
    threads: Option<usize>,
    program_trees: Vec<Box<TypedProgramTree>>,
    resolver: &Resolver,
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
                let cir_walk = CIRWalk::new(program_tree.clone(), resolver, program_tree.module_id);
                let cir_program_tree = cir_walk.run_pass(program_tree.file_path);
                Box::new(cir_program_tree)
            })
            .collect()
    })
}
