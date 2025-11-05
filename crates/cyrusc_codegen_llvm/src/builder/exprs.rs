use crate::{
    builder::{
        builder::IRBuilderCtx,
        irreg::LocalIRValue,
        values::{InternalValue, InternalValueKind},
    },
    llvm::constness::is_basic_value_constant,
};
use cyrusc_cir::{
    CIRExpr, CIRExprKind, CIRFuncCall, CIRLiteral, CIRLiteralKind, CIRStructFieldAccessExpr, CIRStructInitExpr,
    CIRTupleAccessExpr, CIRTupleExpr, CIRValueRef,
    types::{CIRStructTy, CIRTupleTy, CIRTy},
};
use inkwell::{
    AddressSpace, Either,
    module::Linkage,
    types::BasicTypeEnum,
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, StructValue},
};

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_expr(&mut self, expr: &CIRExpr) -> InternalValue<'ll> {
        match &expr.kind {
            CIRExprKind::Load(value_ref) => self.emit_load(value_ref),
            CIRExprKind::Literal(literal) => self.emit_literal(literal),
            CIRExprKind::Prefix(prefix_expr) => todo!(),
            CIRExprKind::Infix(infix_expr) => todo!(),
            CIRExprKind::Unary(unary_expr) => todo!(),
            CIRExprKind::SizeOf(size_of_expr) => todo!(),
            CIRExprKind::Assign(assign_expr) => todo!(),
            CIRExprKind::Cast(cast_expr) => todo!(),
            CIRExprKind::AddrOf(addr_of_expr) => todo!(),
            CIRExprKind::Deref(deref_expr) => todo!(),
            CIRExprKind::Array(array_expr) => todo!(),
            CIRExprKind::ArrayIndex(array_index_expr) => todo!(),
            CIRExprKind::Tuple(tuple_expr) => self.emit_tuple(tuple_expr),
            CIRExprKind::TupleAccess(tuple_access) => self.emit_tuple_access(tuple_access),
            CIRExprKind::StructInit(struct_init_expr) => self.emit_struct_init(struct_init_expr),
            CIRExprKind::StructFieldAccess(struct_field_access_expr) => {
                self.emit_struct_field_access(struct_field_access_expr)
            }
            CIRExprKind::UnionFieldAccess(union_field_access_expr) => todo!(),
            CIRExprKind::FuncCall(func_call) => self.emit_func_call(func_call),
            CIRExprKind::Lambda(lambda) => self.emit_lambda(lambda),
        }
    }

    pub(crate) fn emit_struct_field_access(&mut self, field_access: &CIRStructFieldAccessExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&field_access.operand);

        let ptr = lvalue.as_basic_value().into_pointer_value();
        let pointee_ty: BasicTypeEnum<'ll> = self.emit_ty(field_access.operand.ty.clone()).try_into().unwrap();
        let field_value: BasicValueEnum<'ll> = self
            .llvmbuilder
            .build_struct_gep(
                pointee_ty,
                ptr,
                field_access.field_idx.try_into().unwrap(),
                "struct_gep",
            )
            .unwrap()
            .into();

        InternalValue::new(
            field_access.field_ty.clone(),
            InternalValueKind::LValue(field_value.into_pointer_value()),
        )
    }

    pub(crate) fn emit_tuple_access(&mut self, tuple_access: &CIRTupleAccessExpr) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&tuple_access.operand);
        let rvalue = self.load_rvalue(lvalue);
        let struct_value = rvalue.as_basic_value().into_struct_value();

        let extracted_value = self
            .llvmbuilder
            .build_extract_value(struct_value, tuple_access.index.try_into().unwrap(), "extractvalue")
            .unwrap();

        InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(extracted_value))
    }

    pub(crate) fn emit_tuple(&mut self, tuple: &CIRTupleExpr) -> InternalValue<'ll> {
        let tys: Vec<CIRTy> = tuple.elms.iter().map(|elm| elm.ty.clone()).collect();

        let struct_value = self
            .emit_struct_init(&CIRStructInitExpr {
                ty: CIRStructTy {
                    fields: tys.clone(),
                    is_packed: false,
                },
                fields: tuple.elms.clone(),
            })
            .as_basic_value()
            .into_struct_value();

        InternalValue::new(
            CIRTy::Tuple(CIRTupleTy { items: tys }),
            InternalValueKind::RValue(struct_value.into()),
        )
    }

    pub(crate) fn emit_struct_init(&mut self, struct_init: &CIRStructInitExpr) -> InternalValue<'ll> {
        let field_types: Vec<BasicTypeEnum<'ll>> = struct_init
            .ty
            .fields
            .iter()
            .map(|ty| self.emit_ty(ty.clone()).try_into().unwrap())
            .collect();

        let struct_type = self.llvmctx.struct_type(&field_types, struct_init.ty.is_packed);

        let mut all_const = true;

        let values: Vec<BasicValueEnum<'ll>> = struct_init
            .fields
            .iter()
            .map(|expr| {
                let lvalue = self.emit_expr(expr);
                let rvalue = self.load_rvalue(lvalue);
                let basic_value = rvalue.as_basic_value();
                if !is_basic_value_constant(basic_value) {
                    all_const = false;
                }
                basic_value
            })
            .collect();

        let struct_value: StructValue<'ll>;

        if all_const {
            struct_value = struct_type.const_named_struct(&values);
        } else {
            struct_value = struct_type.get_undef();

            values.iter().enumerate().for_each(|(index, rvalue)| {
                self.llvmbuilder
                    .build_insert_value(struct_value, *rvalue, index.try_into().unwrap(), "insert")
                    .unwrap()
                    .into_struct_value();
            });
        }

        InternalValue::new(
            CIRTy::Struct(struct_init.ty.clone()),
            InternalValueKind::RValue(struct_value.into()),
        )
    }

    pub(crate) fn emit_func_call(&mut self, func_call: &CIRFuncCall) -> InternalValue<'ll> {
        let lvalue = self.emit_expr(&func_call.operand);
        let rvalue = self.load_rvalue(lvalue);

        // Check if it's a direct or indirect call
        if let Some(fn_value) = rvalue.as_func() {
            self.emit_direct_call(func_call, fn_value)
        } else if rvalue.as_basic_value().is_pointer_value() {
            self.emit_indirect_call(func_call)
        } else {
            panic!("Expected a function or pointer to function in call expression.")
        }
    }

    pub(crate) fn emit_direct_call(
        &mut self,
        func_call: &CIRFuncCall,
        fn_value: &FunctionValue<'ll>,
    ) -> InternalValue<'ll> {
        let args: Vec<BasicMetadataValueEnum<'ll>> = func_call
            .args
            .iter()
            .map(|expr| {
                let lvalue = self.emit_expr(expr);
                self.load_rvalue(lvalue).as_basic_value().into()
            })
            .collect();

        let call_site = self.llvmbuilder.build_call(*fn_value, &args, "call").unwrap();

        match call_site.try_as_basic_value() {
            Either::Left(bv) => InternalValue::new(func_call.ret_ty.clone(), InternalValueKind::RValue(bv)),
            Either::Right(_) => self.emit_null(func_call.ret_ty.clone()),
        }
    }

    pub(crate) fn emit_indirect_call(&mut self, func_call: &CIRFuncCall) -> InternalValue<'ll> {
        let operand = self.emit_expr(&func_call.operand);

        let fn_ty = self.emit_func_ty(operand.ty.as_fn_ty().unwrap());
        let fn_ptr = operand.as_basic_value().into_pointer_value();

        let args: Vec<BasicMetadataValueEnum<'ll>> = func_call
            .args
            .iter()
            .map(|expr| {
                let lvalue = self.emit_expr(expr);
                self.load_rvalue(lvalue).as_basic_value().into()
            })
            .collect();

        let call_site = self
            .llvmbuilder
            .build_indirect_call(fn_ty, fn_ptr, &args, "indirect_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            Either::Left(bv) => InternalValue::new(func_call.ret_ty.clone(), InternalValueKind::RValue(bv)),
            Either::Right(_) => self.emit_null(func_call.ret_ty.clone()),
        }
    }

    pub(crate) fn emit_load(&self, value_ref: &CIRValueRef) -> InternalValue<'ll> {
        let irreg = self.irreg.borrow();
        let internal_value = match irreg.get(value_ref.irv_id).unwrap() {
            LocalIRValue::Func(fn_value, ty) => InternalValue::new(ty, InternalValueKind::FuncValue(fn_value)),
            LocalIRValue::Global(global_value, ty) => {
                InternalValue::new(ty, InternalValueKind::LValue(global_value.as_pointer_value()))
            }
            LocalIRValue::LValue(pointer_value, ty) => InternalValue::new(ty, InternalValueKind::LValue(pointer_value)),
            LocalIRValue::RValue(basic_value_enum, ty) => {
                InternalValue::new(ty, InternalValueKind::RValue(basic_value_enum))
            }
            _ => unreachable!(),
        };
        drop(irreg);
        internal_value
    }

    pub(crate) fn emit_literal(&self, lit: &CIRLiteral) -> InternalValue<'ll> {
        let ty: BasicTypeEnum<'ll> = self.emit_ty(lit.ty.clone()).try_into().unwrap();

        let basic_value = match &lit.kind {
            CIRLiteralKind::Bool(value) => {
                BasicValueEnum::IntValue(self.llvmctx.bool_type().const_int(*value as u64, false))
            }
            CIRLiteralKind::Integer(value, is_signed) => {
                BasicValueEnum::IntValue(ty.into_int_type().const_int((*value).try_into().unwrap(), *is_signed))
            }
            CIRLiteralKind::Float(value) => BasicValueEnum::FloatValue(ty.into_float_type().const_float(*value)),
            CIRLiteralKind::Char(value) => {
                BasicValueEnum::IntValue(self.llvmctx.i8_type().const_int(*value as u64, false))
            }
            CIRLiteralKind::Null => {
                BasicValueEnum::PointerValue(self.llvmctx.ptr_type(AddressSpace::default()).const_null())
            }
            CIRLiteralKind::CString(value) => self.emit_cstring(value.clone()),
            CIRLiteralKind::ByteString(value) => self.emit_bytestring(value.clone()),
        };

        InternalValue::new(lit.ty.clone(), InternalValueKind::RValue(basic_value))
    }

    pub(crate) fn emit_null(&self, ty: CIRTy) -> InternalValue<'ll> {
        let basic_value = BasicValueEnum::PointerValue(self.llvmctx.ptr_type(AddressSpace::default()).const_null());
        InternalValue::new(ty, InternalValueKind::RValue(basic_value))
    }

    pub(crate) fn emit_cstring(&self, value: String) -> BasicValueEnum<'ll> {
        let const_str = self.llvmctx.const_string(value.as_bytes(), true);

        let llvmmodule = self.llvmmodule.borrow();
        let global_str = llvmmodule.add_global(const_str.get_type(), None, ".cstring");
        global_str.set_initializer(&const_str);
        global_str.set_constant(true);
        global_str.set_unnamed_addr(true);
        global_str.set_linkage(Linkage::Private);
        global_str.set_alignment(1);
        drop(llvmmodule);

        global_str.as_pointer_value().into()
    }

    pub(crate) fn emit_bytestring(&self, value: String) -> BasicValueEnum<'ll> {
        self.llvmctx.const_string(value.as_bytes(), true).into()
    }
}
