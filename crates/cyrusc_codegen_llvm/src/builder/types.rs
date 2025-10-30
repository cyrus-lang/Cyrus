use crate::builder::builder::IRBuilderCtx;
use cyrusc_cir::types::{CIRArrayTy, CIREnumTy, CIREnumVariantTy, CIRStructTy, CIRTupleTy, CIRTy, CIRUnionTy};
use cyrusc_tast::types::PlainType;
use inkwell::{
    AddressSpace,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType},
};
use std::rc::Rc;

pub(crate) fn emit_struct_ty<'ll>(ctx: &'ll Rc<IRBuilderCtx>, struct_ty: CIRStructTy) -> StructType<'ll> {
    let field_types = emit_tys(ctx, &struct_ty.fields)
        .iter()
        .map(|ty| (*ty).try_into().unwrap())
        .collect::<Vec<BasicTypeEnum<'ll>>>();
    ctx.llvmctx.struct_type(&field_types, struct_ty.is_packed)
}

pub(crate) fn emit_ty<'ll>(ctx: &'ll Rc<IRBuilderCtx>, ty: CIRTy) -> AnyTypeEnum<'ll> {
    match ty {
        CIRTy::PlainType(plain_ty) => emit_plain_ty(ctx, plain_ty),
        CIRTy::Const(ty) => emit_ty(ctx, *ty),
        CIRTy::Pointer(ty) => ctx.llvmctx.ptr_type(AddressSpace::default()).as_any_type_enum(),
        CIRTy::Struct(struct_ty) => emit_struct_ty(ctx, struct_ty).as_any_type_enum(),
        CIRTy::Enum(enum_ty) => emit_enum_ty(ctx, enum_ty).as_any_type_enum(),
        CIRTy::Union(union_ty) => emit_union_ty(ctx, union_ty).as_any_type_enum(),
        CIRTy::Tuple(tuple_ty) => emit_tuple_ty(ctx, tuple_ty).as_any_type_enum(),
        CIRTy::Array(array_ty) => emit_array_ty(ctx, array_ty).as_any_type_enum(),
        CIRTy::FuncType(func_ty) => todo!(),
    }
}

pub(crate) fn emit_plain_ty<'ll>(ctx: &'ll Rc<IRBuilderCtx>, plain_ty: PlainType) -> AnyTypeEnum<'ll> {
    let llvmctx = &ctx.llvmctx;

    match plain_ty {
        PlainType::UIntPtr | PlainType::IntPtr | PlainType::SizeT => llvmctx
            .ptr_sized_int_type(&ctx.llvmtm.get_target_data(), None)
            .as_any_type_enum(),
        PlainType::Int8 => llvmctx.i8_type().as_any_type_enum(),
        PlainType::Int16 => llvmctx.i16_type().as_any_type_enum(),
        PlainType::Int32 => llvmctx.i32_type().as_any_type_enum(),
        PlainType::Int64 => llvmctx.i64_type().as_any_type_enum(),
        PlainType::Int128 => llvmctx.i128_type().as_any_type_enum(),
        PlainType::UInt8 => llvmctx.i8_type().as_any_type_enum(),
        PlainType::UInt16 => llvmctx.i16_type().as_any_type_enum(),
        PlainType::UInt32 => llvmctx.i32_type().as_any_type_enum(),
        PlainType::UInt64 => llvmctx.i64_type().as_any_type_enum(),
        PlainType::UInt128 => llvmctx.i128_type().as_any_type_enum(),
        PlainType::Int => llvmctx.i32_type().as_any_type_enum(),
        PlainType::UInt => llvmctx.i32_type().as_any_type_enum(),
        PlainType::Float16 => llvmctx.f16_type().as_any_type_enum(),
        PlainType::Float32 => llvmctx.f32_type().as_any_type_enum(),
        PlainType::Float64 => llvmctx.f64_type().as_any_type_enum(),
        PlainType::Float128 => llvmctx.f128_type().as_any_type_enum(),
        PlainType::Char => llvmctx.i8_type().as_any_type_enum(),
        PlainType::Bool => llvmctx.bool_type().as_any_type_enum(),
        PlainType::Null => llvmctx.ptr_type(AddressSpace::default()).as_any_type_enum(),
        PlainType::Void => unreachable!(),
    }
}

pub(crate) fn emit_tys<'ll>(ctx: &'ll Rc<IRBuilderCtx>, tys: &[CIRTy]) -> Vec<AnyTypeEnum<'ll>> {
    tys.iter().map(|ty| emit_ty(ctx, ty.clone())).collect()
}

pub(crate) fn emit_enum_ty<'ll>(ctx: &'ll Rc<IRBuilderCtx>, enum_ty: CIREnumTy) -> StructType<'ll> {
    let llvmctx = &ctx.llvmctx;
    let target_data = ctx.llvmtm.get_target_data();
    let tag_type = llvmctx.i32_type();

    let mut largest_payload_ty: Option<BasicTypeEnum<'ll>> = None;
    let mut max_payload_size = 0u64;

    for variant in enum_ty.variants {
        match variant {
            CIREnumVariantTy::Ident(_) => {} // no payload
            CIREnumVariantTy::Valued(_, expr) => {
                let llvm_ty = emit_ty(ctx, expr.ty.clone())
                    .try_into()
                    .expect("Enum value payload must be a valid llvm type.");
                let size = target_data.get_store_size(&llvm_ty);
                if size > max_payload_size {
                    max_payload_size = size;
                    largest_payload_ty = Some(llvm_ty);
                }
            }
            CIREnumVariantTy::Fielded(_, field_tys) => {
                let llvm_fields = emit_tys(ctx, &field_tys)
                    .iter()
                    .map(|ty| (*ty).try_into().unwrap())
                    .collect::<Vec<BasicTypeEnum<'ll>>>();
                let struct_ty = llvmctx.struct_type(&llvm_fields, false);
                let size = target_data.get_store_size(&struct_ty);
                if size > max_payload_size {
                    max_payload_size = size;
                    largest_payload_ty = Some(struct_ty.as_basic_type_enum());
                }
            }
        }
    }

    let payload_ty = largest_payload_ty.unwrap_or_else(|| llvmctx.i8_type().as_basic_type_enum());

    llvmctx.struct_type(&[tag_type.as_basic_type_enum(), payload_ty], false)
}

pub(crate) fn emit_union_ty<'ll>(ctx: &'ll Rc<IRBuilderCtx>, union_ty: CIRUnionTy) -> StructType<'ll> {
    let mut largest: Option<BasicTypeEnum<'ll>> = None;
    let mut max_size = 0u64;

    let target_data = ctx.llvmtm.get_target_data();

    for field_ty in union_ty.fields {
        let llvm_ty = emit_ty(ctx, field_ty.clone())
            .try_into()
            .expect("Union variant must be a valid basic type");

        let size = target_data.get_store_size(&llvm_ty);
        if size > max_size {
            max_size = size;
            largest = Some(llvm_ty);
        }
    }

    let largest_ty = largest.expect("Union must have at least one field");
    ctx.llvmctx.struct_type(&[largest_ty], false)
}

pub(crate) fn emit_tuple_ty<'ll>(ctx: &'ll Rc<IRBuilderCtx>, tuple_ty: CIRTupleTy) -> StructType<'ll> {
    let element_types = emit_tys(ctx, &tuple_ty.items)
        .iter()
        .map(|ty| (*ty).try_into().unwrap())
        .collect::<Vec<BasicTypeEnum<'ll>>>();
    ctx.llvmctx.struct_type(&element_types, false)
}

pub(crate) fn emit_array_ty<'ll>(ctx: &'ll Rc<IRBuilderCtx>, array_ty: CIRArrayTy) -> AnyTypeEnum<'ll> {
    let elm_ty: BasicTypeEnum<'ll> = emit_ty(ctx, *array_ty.ty)
        .try_into()
        .expect("Array element must be a valid llvm type.");
    elm_ty.array_type(array_ty.len as u32).as_any_type_enum()
}

pub(crate) fn emit_func_ty<'ll>(
    ctx: &'ll Rc<IRBuilderCtx>,
    func_ty: cyrusc_cir::types::CIRFuncTy,
) -> FunctionType<'ll> {
    let ret_ty: BasicTypeEnum<'ll> = emit_ty(ctx, *func_ty.ret)
        .try_into()
        .expect("Function return type must be a valid llvm type.");

    let param_tys = emit_tys(ctx, &func_ty.params)
        .iter()
        .map(|ty| (*ty).try_into().unwrap())
        .collect::<Vec<BasicMetadataTypeEnum<'ll>>>();

    ret_ty.fn_type(&param_tys, func_ty.is_var)
}
