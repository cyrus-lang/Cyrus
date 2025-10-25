use crate::{
    CIRAddrOfExpr, CIRArrayExpr, CIRArrayIndexExpr, CIRAssignExpr, CIRBlockStmt, CIRBreakStmt, CIRCastExpr,
    CIRContinueStmt, CIRDerefExpr, CIREnumStmt, CIREnumVariant, CIRExpr, CIRExprKind, CIRForStmt, CIRFuncCall,
    CIRFuncDeclStmt, CIRFuncDefStmt, CIRGlobalVarStmt, CIRIfStmt, CIRInfixExpr, CIRLiteral, CIRPrefixExpr,
    CIRProgramTree, CIRReturnStmt, CIRSizeOfExpr, CIRStmt, CIRStructInitExpr, CIRStructStmt, CIRTupleAccessExpr,
    CIRTupleExpr, CIRUnaryExpr, CIRUnionStmt, CIRValueRef, CIRVarStmt, CIRWhileStmt,
    concrete_type::{CIRArrayTy, CIRFuncTy, CIRStructTy, CIRTupleTy, CIRTy},
};
use ast::LiteralKind;
use resolver::typed_func_params_as_func_type_params;
use tast::{
    TypedAddrOfExpr, TypedArrayExpr, TypedArrayIndexExpr, TypedAssignExpr, TypedBlockStmt, TypedBreakStmt,
    TypedCastExpr, TypedContinueStmt, TypedDerefExpr, TypedEnumStmt, TypedEnumVariant, TypedExportTupleStmt,
    TypedExprKind, TypedExprStmt, TypedFieldAccess, TypedForStmt, TypedFuncCall, TypedFuncDeclStmt, TypedFuncDefStmt,
    TypedFuncTypeParams, TypedFuncTypeVariadicParams, TypedGlobalVarStmt, TypedIfStmt, TypedInfixExpr, TypedLambdaExpr,
    TypedLiteralExpr, TypedMethodCall, TypedPrefixExpr, TypedProgramTree, TypedReturnStmt, TypedSizeOfExpr, TypedStmt,
    TypedStructStmt, TypedTupleAccessExpr, TypedTupleExpr, TypedUStructValue, TypedUnaryExpr, TypedUnionStmt,
    TypedVarStmt, TypedWhileStmt,
    types::{SemanticType, TypedArrayCapacity, TypedArrayFixedCapacityValue},
};

#[derive(Debug)]
pub struct CIRWalk {
    program: TypedProgramTree,
}

impl CIRWalk {
    pub fn run_pass(&self) -> CIRProgramTree {
        let stmts = self.lower_stmts(&self.program.body);
        CIRProgramTree { body: stmts }
    }

    fn lower_stmts(&self, stmts: &Vec<TypedStmt>) -> Vec<CIRStmt> {
        let mut lowered_stmts: Vec<CIRStmt> = Vec::new();

        for stmt in stmts {
            let lowered_stmt = match stmt {
                TypedStmt::Variable(var_stmt) => CIRStmt::Variable(self.lower_var(var_stmt)),
                TypedStmt::GlobalVariable(global_var_stmt) => self.lower_global_var(global_var_stmt),
                TypedStmt::FuncDef(func_def_stmt) => self.lower_func_def(func_def_stmt),
                TypedStmt::FuncDecl(func_decl_stmt) => self.lower_func_decl(func_decl_stmt),
                TypedStmt::BlockStatement(block_stmt) => CIRStmt::Block(self.lower_block(block_stmt)),
                TypedStmt::If(if_stmt) => CIRStmt::If(self.lower_if(if_stmt)),
                TypedStmt::Return(return_stmt) => self.lower_return(return_stmt),
                TypedStmt::Break(break_stmt) => self.lower_break(break_stmt),
                TypedStmt::Continue(continue_stmt) => self.lower_continue(continue_stmt),
                TypedStmt::For(for_stmt) => self.lower_for(for_stmt),
                TypedStmt::While(while_stmt) => self.lower_while(while_stmt),
                TypedStmt::Switch(switch_stmt) => todo!(),
                TypedStmt::Struct(struct_stmt) => self.lower_struct(struct_stmt),
                TypedStmt::Enum(enum_stmt) => self.lower_enum(enum_stmt),
                TypedStmt::Union(union_stmt) => self.lower_union(union_stmt),
                TypedStmt::ExportTuple(export_tuple_stmt) => self.lower_export_tuple(export_tuple_stmt),
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

    // TODO
    fn lower_export_tuple(&self, export_tuple: &TypedExportTupleStmt) -> CIRStmt {
        todo!();
        // CIRStmt::ExportTuple(CIRExportTupleStmt {
        // })
    }

    fn lower_union(&self, union_stmt: &TypedUnionStmt) -> CIRStmt {
        let fields: Vec<CIRTy> = union_stmt
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(&field.ty))
            .collect();

        CIRStmt::Union(CIRUnionStmt {
            name: union_stmt.name.clone(),
            fields,
            vis: union_stmt.vis.clone(),
        })
    }

    fn lower_struct(&self, struct_stmt: &TypedStructStmt) -> CIRStmt {
        let fields: Vec<CIRTy> = struct_stmt
            .fields
            .iter()
            .map(|field| self.lower_sema_ty(&field.ty))
            .collect();

        CIRStmt::Struct(CIRStructStmt {
            name: struct_stmt.name.clone(),
            packed: struct_stmt.packed,
            fields,
            vis: struct_stmt.vis.clone(),
        })
    }

    fn lower_enum(&self, enum_stmt: &TypedEnumStmt) -> CIRStmt {
        let variants: Vec<CIREnumVariant> = enum_stmt
            .variants
            .iter()
            .map(|enum_variant| match enum_variant {
                TypedEnumVariant::Identifier(..) => CIREnumVariant::Ident,
                TypedEnumVariant::Valued(_, expr) => CIREnumVariant::Valued(Box::new(self.lower_expr(expr))),
                TypedEnumVariant::Variant(_, fielded) => {
                    let fields: Vec<CIRTy> = fielded
                        .iter()
                        .map(|field| self.lower_sema_ty(&field.field_type))
                        .collect();
                    CIREnumVariant::Fielded(fields)
                }
            })
            .collect();

        CIRStmt::Enum(CIREnumStmt {
            name: enum_stmt.name.clone(),
            variants,
            vis: enum_stmt.vis.clone(),
        })
    }

    fn lower_while(&self, while_stmt: &TypedWhileStmt) -> CIRStmt {
        let cond = Box::new(self.lower_expr(&while_stmt.cond));
        let body = Box::new(self.lower_block(&while_stmt.body));

        CIRStmt::While(CIRWhileStmt { cond, body })
    }

    fn lower_for(&self, for_stmt: &TypedForStmt) -> CIRStmt {
        let initializer = for_stmt.initializer.clone().and_then(|var| Some(self.lower_var(&var)));
        let cond = for_stmt.cond.clone().and_then(|cond| Some(self.lower_expr(&cond)));
        let increment = for_stmt
            .increment
            .clone()
            .and_then(|increment| Some(self.lower_expr(&increment)));

        let body = Box::new(self.lower_block(&for_stmt.body));

        CIRStmt::For(CIRForStmt {
            initializer,
            cond,
            increment,
            body,
        })
    }

    fn lower_break(&self, _: &TypedBreakStmt) -> CIRStmt {
        CIRStmt::Break(CIRBreakStmt {})
    }

    fn lower_continue(&self, _: &TypedContinueStmt) -> CIRStmt {
        CIRStmt::Continue(CIRContinueStmt {})
    }

    fn lower_return(&self, ret: &TypedReturnStmt) -> CIRStmt {
        let arg = ret.arg.clone().and_then(|arg| Some(self.lower_expr(&arg)));
        CIRStmt::Return(CIRReturnStmt { arg })
    }

    fn lower_if(&self, if_stmt: &TypedIfStmt) -> CIRIfStmt {
        let cond = self.lower_expr(&if_stmt.cond);
        let then_block = Box::new(self.lower_block(&if_stmt.then_block));
        let else_block = if_stmt
            .else_block
            .clone()
            .and_then(|else_block| Some(Box::new(self.lower_block(&else_block))));
        let branches: Vec<CIRIfStmt> = if_stmt.branches.iter().map(|branch| self.lower_if(branch)).collect();

        CIRIfStmt {
            cond,
            then_block,
            branches,
            else_block,
        }
    }

    fn lower_global_var(&self, global_var: &TypedGlobalVarStmt) -> CIRStmt {
        let ty = self.lower_sema_ty(
            &global_var
                .ty
                .clone()
                .unwrap_or(global_var.expr.clone().unwrap().sema_ty.unwrap()),
        );

        let expr = global_var.expr.clone().and_then(|expr| Some(self.lower_expr(&expr)));

        CIRStmt::GlobalVar(CIRGlobalVarStmt {
            name: global_var.name.clone(),
            ty,
            expr,
            vis: global_var.vis.clone(),
        })
    }

    fn lower_var(&self, var: &TypedVarStmt) -> CIRVarStmt {
        let ty = self.lower_sema_ty(&var.ty.clone().unwrap_or(var.rhs.clone().unwrap().sema_ty.unwrap()));
        let expr = var.rhs.clone().and_then(|expr| Some(self.lower_expr(&expr)));

        CIRVarStmt {
            name: var.name.clone(),
            ty,
            expr,
        }
    }

    fn lower_func_params(&self, func_params: &TypedFuncTypeParams) -> (Vec<CIRTy>, bool) {
        let mut params: Vec<CIRTy> = Vec::new();

        func_params
            .list
            .iter()
            .for_each(|sema_ty| params.push(self.lower_sema_ty(&sema_ty)));

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

    fn lower_func_def(&self, func_def: &TypedFuncDefStmt) -> CIRStmt {
        let func_params = typed_func_params_as_func_type_params(&func_def.params);
        let (params, is_var) = self.lower_func_params(&func_params);

        let body = self.lower_block(&func_def.body);
        let ret = self.lower_sema_ty(&func_def.return_type);

        CIRStmt::FuncDef(CIRFuncDefStmt {
            name: func_def.name.clone(),
            params,
            is_var,
            body: Box::new(body),
            ret,
            vis: func_def.vis.clone(),
        })
    }

    fn lower_func_decl(&self, func_decl: &TypedFuncDeclStmt) -> CIRStmt {
        let func_params = typed_func_params_as_func_type_params(&func_decl.params);
        let (params, is_var) = self.lower_func_params(&func_params);
        let ret = self.lower_sema_ty(&func_decl.return_type);

        CIRStmt::FuncDecl(CIRFuncDeclStmt {
            name: func_decl.name.clone(),
            params,
            is_var,
            ret,
            vis: func_decl.vis.clone(),
        })
    }

    fn lower_block(&self, block: &TypedBlockStmt) -> CIRBlockStmt {
        let mut stmts = self.lower_stmts(&block.stmts);
        let defer_stmts: Vec<TypedStmt> = block.defers.iter().map(|defer| *defer.operand.clone()).collect();
        stmts.extend(self.lower_stmts(&defer_stmts));
        CIRBlockStmt { stmts }
    }

    // exprs

    fn lower_expr(&self, expr: &TypedExprStmt) -> CIRExpr {
        let ty = self.lower_sema_ty(&expr.sema_ty.clone().unwrap());

        let kind = match &expr.kind {
            TypedExprKind::Symbol(symbol_id, ..) => CIRExprKind::Load(CIRValueRef { irv_id: *symbol_id }),
            TypedExprKind::Literal(literal_expr) => self.lower_literal(literal_expr),
            TypedExprKind::Prefix(prefix_expr) => self.lower_prefix(prefix_expr),
            TypedExprKind::Infix(infix_expr) => self.lower_infix(infix_expr),
            TypedExprKind::Unary(unary_expr) => self.lower_unary(unary_expr),
            TypedExprKind::Assign(assign_expr) => self.lower_assign(assign_expr),
            TypedExprKind::Cast(cast_expr) => self.lower_cast(cast_expr),
            TypedExprKind::AddrOf(addr_of_expr) => self.lower_addr_of(addr_of_expr),
            TypedExprKind::Deref(deref_expr) => self.lower_deref(deref_expr),
            TypedExprKind::Array(array_expr) => self.lower_array(array_expr),
            TypedExprKind::ArrayIndex(array_index_expr) => self.lower_array_index(array_index_expr),
            TypedExprKind::UStructValue(ustruct_value) => self.lower_ustruct_value(ustruct_value),
            TypedExprKind::FuncCall(func_call) => self.lower_func_call(func_call),
            TypedExprKind::MethodCall(method_call) => self.lower_method_call(method_call),
            TypedExprKind::FieldAccess(field_access) => todo!(),
            TypedExprKind::StructInit(struct_init_expr) => todo!(),
            TypedExprKind::SizeOf(size_of_expr) => self.lower_size_of(size_of_expr),
            TypedExprKind::Lambda(lambda_expr) => self.lower_lambda(lambda_expr),
            TypedExprKind::Tuple(tuple_expr) => self.lower_tuple(tuple_expr),
            TypedExprKind::TupleAccess(tuple_access_expr) => self.lower_tuple_access(tuple_access_expr),
            // skipped
            TypedExprKind::SemanticType(..) => unreachable!(),
        };

        CIRExpr { kind, ty }
    }

    fn lower_tuple_access(&self, tuple_access: &TypedTupleAccessExpr) -> CIRExprKind {
        let operand = Box::new(self.lower_expr(&tuple_access.operand));
        CIRExprKind::TupleAccess(CIRTupleAccessExpr {
            operand,
            index: tuple_access.index,
        })
    }

    fn lower_tuple(&self, tuple: &TypedTupleExpr) -> CIRExprKind {
        let elms: Vec<CIRExpr> = tuple.expr_list.iter().map(|expr| self.lower_expr(expr)).collect();
        CIRExprKind::Tuple(CIRTupleExpr { elms })
    }

    // TODO
    fn lower_lambda(&self, lambda: &TypedLambdaExpr) -> CIRExprKind {
        todo!();
    }

    fn lower_method_call(&self, method_call: &TypedMethodCall) -> CIRExprKind {
        let args: Vec<CIRExpr> = method_call.args.iter().map(|arg| self.lower_expr(arg)).collect();

        CIRExprKind::FuncCall(CIRFuncCall {
            operand: Box::new(self.lower_expr(&method_call.operand)),
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

    fn lower_func_call(&self, func_call: &TypedFuncCall) -> CIRExprKind {
        let args: Vec<CIRExpr> = func_call.args.iter().map(|arg| self.lower_expr(arg)).collect();

        CIRExprKind::FuncCall(CIRFuncCall {
            operand: Box::new(self.lower_expr(&func_call.operand)),
            args,
        })
    }

    fn lower_ustruct_value(&self, ustruct_value: &TypedUStructValue) -> CIRExprKind {
        let fields: Vec<CIRExpr> = ustruct_value
            .fields
            .iter()
            .map(|field| self.lower_expr(&field.field_value))
            .collect();

        let struct_ty = CIRStructTy {
            tys: ustruct_value
                .unnamed_struct_type
                .clone()
                .unwrap()
                .fields
                .iter()
                .map(|field| self.lower_sema_ty(&field.field_type))
                .collect(),
        };

        CIRExprKind::StructInit(CIRStructInitExpr { ty: struct_ty, fields })
    }

    fn lower_array_index(&self, array_index: &TypedArrayIndexExpr) -> CIRExprKind {
        CIRExprKind::ArrayIndex(CIRArrayIndexExpr {
            operand: Box::new(self.lower_expr(&array_index.operand)),
            index: Box::new(self.lower_expr(&array_index.index)),
        })
    }

    fn lower_array(&self, array: &TypedArrayExpr) -> CIRExprKind {
        let elms: Vec<CIRExpr> = array.elements.iter().map(|elm| self.lower_expr(elm)).collect();

        CIRExprKind::Array(CIRArrayExpr {
            ty: self.lower_sema_ty(&array.array_type),
            elms,
        })
    }

    fn lower_deref(&self, deref: &TypedDerefExpr) -> CIRExprKind {
        CIRExprKind::Deref(CIRDerefExpr {
            operand: Box::new(self.lower_expr(&deref.operand)),
        })
    }

    fn lower_addr_of(&self, addr_of: &TypedAddrOfExpr) -> CIRExprKind {
        CIRExprKind::AddrOf(CIRAddrOfExpr {
            operand: Box::new(self.lower_expr(&addr_of.operand)),
        })
    }

    fn lower_size_of(&self, sizeof: &TypedSizeOfExpr) -> CIRExprKind {
        CIRExprKind::SizeOf(CIRSizeOfExpr {
            operand: Box::new(self.lower_expr(&sizeof.operand)),
        })
    }

    fn lower_cast(&self, cast: &TypedCastExpr) -> CIRExprKind {
        CIRExprKind::Cast(CIRCastExpr {
            operand: Box::new(self.lower_expr(&cast.operand)),
            ty: Box::new(self.lower_sema_ty(&cast.target_type)),
        })
    }

    fn lower_assign(&self, assign: &TypedAssignExpr) -> CIRExprKind {
        CIRExprKind::Assign(CIRAssignExpr {
            lhs: Box::new(self.lower_expr(&assign.lhs)),
            rhs: Box::new(self.lower_expr(&assign.rhs)),
        })
    }

    fn lower_unary(&self, unary: &TypedUnaryExpr) -> CIRExprKind {
        CIRExprKind::Unary(CIRUnaryExpr {
            op: unary.op.clone(),
            operand: Box::new(self.lower_expr(&unary.operand)),
        })
    }

    fn lower_infix(&self, infix: &TypedInfixExpr) -> CIRExprKind {
        CIRExprKind::Infix(CIRInfixExpr {
            op: infix.op.clone(),
            lhs: Box::new(self.lower_expr(&infix.lhs)),
            rhs: Box::new(self.lower_expr(&infix.rhs)),
        })
    }

    fn lower_prefix(&self, prefix: &TypedPrefixExpr) -> CIRExprKind {
        CIRExprKind::Prefix(CIRPrefixExpr {
            op: prefix.op.clone(),
            operand: Box::new(self.lower_expr(&prefix.operand)),
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

    fn lower_sema_ty(&self, sema_ty: &SemanticType) -> CIRTy {
        match sema_ty {
            SemanticType::BasicType(basic_type) => CIRTy::BasicType(basic_type.clone()),
            SemanticType::ResolvedSymbol(..) => {
                // FIXME
                todo!();
            }
            SemanticType::Array(array_type) => {
                let ty = self.lower_sema_ty(&array_type.element_type);
                let len = match &array_type.capacity {
                    TypedArrayCapacity::Fixed(fixed_cap) => match fixed_cap {
                        TypedArrayFixedCapacityValue::Value(value) => *value,
                        TypedArrayFixedCapacityValue::Expr(_) => unreachable!(),
                    },
                    TypedArrayCapacity::Dynamic => todo!(),
                };
                CIRTy::Array(CIRArrayTy { ty: Box::new(ty), len })
            }
            SemanticType::Const(sema_ty) => CIRTy::Const(Box::new(self.lower_sema_ty(&*sema_ty))),
            SemanticType::Pointer(sema_ty) => CIRTy::Pointer(Box::new(self.lower_sema_ty(&*sema_ty))),
            SemanticType::UnnamedStruct(ustruct_ty) => {
                let tys: Vec<CIRTy> = ustruct_ty
                    .fields
                    .iter()
                    .map(|field| self.lower_sema_ty(&field.field_type))
                    .collect();
                CIRTy::Struct(CIRStructTy { tys })
            }
            SemanticType::FuncType(func_type) => {
                let ret = Box::new(self.lower_sema_ty(&func_type.return_type));
                let (params, is_var) = self.lower_func_params(&func_type.params);

                CIRTy::FuncType(CIRFuncTy { params, is_var, ret })
            }
            SemanticType::Tuple(tuple_type) => {
                let tys: Vec<CIRTy> = tuple_type
                    .type_list
                    .iter()
                    .map(|sema_ty| self.lower_sema_ty(sema_ty))
                    .collect();

                CIRTy::Tuple(CIRTupleTy { tys })
            }
            SemanticType::GenericType(_) | SemanticType::GenericParam(_) | SemanticType::UnresolvedSymbol(_) => {
                unreachable!()
            }
        }
    }
}
