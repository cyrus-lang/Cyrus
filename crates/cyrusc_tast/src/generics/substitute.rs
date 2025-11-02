use crate::{
    generics::mapping_ctx::GenericMappingCtx,
    sigs::{EnumSig, StructSig, UnionSig},
    stmts::{TypedEnumValuedField, TypedEnumVariant, TypedFuncTypeParams},
    types::{
        SemanticType, TypedArrayType, TypedFuncType, TypedTupleType, TypedUStructType, TypedUnnamedStructTypeField,
    },
};
use std::cell::RefCell;
use std::rc::Rc;

pub fn substitute_type(sema_ty: SemanticType, ctx: Rc<RefCell<GenericMappingCtx>>) -> Option<SemanticType> {
    fn sub<F>(inner: SemanticType, f: &F) -> Option<SemanticType>
    where
        F: Fn(SemanticType) -> Option<SemanticType>,
    {
        f(inner)
    }

    let ctx_ref = ctx.borrow();

    match sema_ty {
        SemanticType::GenericParam(param) => ctx_ref.get_with_symbol_id(param.symbol_id),
        SemanticType::Pointer(inner) => {
            sub(*inner, &|t| substitute_type(t, ctx.clone())).map(|t| SemanticType::Pointer(Box::new(t)))
        }
        SemanticType::Array(inner) => sub(*inner.element_type, &|t| substitute_type(t, ctx.clone())).map(|elem_ty| {
            SemanticType::Array(TypedArrayType {
                element_type: Box::new(elem_ty),
                capacity: inner.capacity,
                loc: inner.loc.clone(),
            })
        }),
        SemanticType::Const(inner) => {
            sub(*inner, &|t| substitute_type(t, ctx.clone())).map(|t| SemanticType::Const(Box::new(t)))
        }
        SemanticType::Tuple(tuple) => {
            let list = tuple
                .type_list
                .into_iter()
                .map(|t| substitute_type(t, ctx.clone()))
                .collect::<Option<Vec<_>>>()?;
            Some(SemanticType::Tuple(TypedTupleType {
                type_list: list,
                loc: tuple.loc,
            }))
        }
        SemanticType::FuncType(func) => {
            let params = func
                .params
                .list
                .into_iter()
                .map(|p| substitute_type(p, ctx.clone()))
                .collect::<Option<Vec<_>>>()?;
            let ret_ty = Box::new(substitute_type(*func.return_type, ctx.clone())?);
            Some(SemanticType::FuncType(TypedFuncType {
                def_module_id: func.def_module_id,
                params: TypedFuncTypeParams {
                    list: params,
                    variadic: func.params.variadic,
                },
                return_type: ret_ty,
                vis_opt: func.vis_opt,
                loc: func.loc,
            }))
        }
        SemanticType::UnnamedStruct(s) => {
            let fields = s
                .fields
                .iter()
                .map(|f| {
                    let inner = substitute_type(*f.field_ty.clone(), ctx.clone())?;
                    Some(TypedUnnamedStructTypeField {
                        field_name: f.field_name.clone(),
                        field_ty: Box::new(inner),
                        loc: f.loc.clone(),
                    })
                })
                .collect::<Option<Vec<_>>>()?;
            Some(SemanticType::UnnamedStruct(TypedUStructType {
                fields,
                is_packed: s.is_packed,
                loc: s.loc.clone(),
            }))
        }
        other => Some(other),
    }
}

pub fn substitute_struct_sig(sig: &StructSig, ctx: Rc<RefCell<GenericMappingCtx>>) -> Option<StructSig> {
    let new_fields = sig
        .fields
        .iter()
        .map(|f| {
            let substituted = substitute_type(f.ty.clone(), ctx.clone())?;
            let mut f2 = f.clone();
            f2.ty = substituted;
            Some(f2)
        })
        .collect::<Option<Vec<_>>>()?;

    Some(StructSig {
        name: sig.name.clone(),
        fields: new_fields,
        impls: sig.impls.clone(),
        methods: sig.methods.clone(),
        generic_params: sig.generic_params.clone(),
        is_packed: sig.is_packed,
        vis: sig.vis.clone(),
        loc: sig.loc.clone(),
    })
}

pub fn substitute_union_sig(sig: &UnionSig, ctx: Rc<RefCell<GenericMappingCtx>>) -> Option<UnionSig> {
    let new_fields = sig
        .fields
        .iter()
        .map(|f| {
            let substituted = substitute_type(f.ty.clone(), ctx.clone())?;
            let mut f2 = f.clone();
            f2.ty = substituted;
            Some(f2)
        })
        .collect::<Option<Vec<_>>>()?;

    Some(UnionSig {
        symbol_id: sig.symbol_id,
        name: sig.name.clone(),
        fields: new_fields,
        methods: sig.methods.clone(),
        generic_params: sig.generic_params.clone(),
        vis: sig.vis.clone(),
        loc: sig.loc.clone(),
    })
}

pub fn substitute_enum_sig(sig: &EnumSig, ctx: Rc<RefCell<GenericMappingCtx>>) -> Option<EnumSig> {
    let new_variants = sig
        .variants
        .iter()
        .map(|v| match v {
            TypedEnumVariant::Identifier(ident) => Some(TypedEnumVariant::Identifier(ident.clone())),
            TypedEnumVariant::Valued(ident, expr) => {
                let substituted = substitute_type(expr.sema_ty.clone().unwrap(), ctx.clone())?;
                let mut expr_v2 = *expr.clone();
                expr_v2.sema_ty = Some(substituted);
                Some(TypedEnumVariant::Valued(ident.clone(), Box::new(expr_v2)))
            }
            TypedEnumVariant::Variant(ident, fields) => {
                let new_fields = fields
                    .iter()
                    .map(|f| {
                        let substituted = substitute_type(f.field_ty.clone(), ctx.clone())?;
                        Some(TypedEnumValuedField {
                            field_ty: substituted,
                            loc: f.loc.clone(),
                        })
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(TypedEnumVariant::Variant(ident.clone(), new_fields))
            }
        })
        .collect::<Option<Vec<_>>>()?;

    Some(EnumSig {
        symbol_id: sig.symbol_id,
        name: sig.name.clone(),
        methods: sig.methods.clone(),
        variants: new_variants,
        generic_params: sig.generic_params.clone(),
        vis: sig.vis.clone(),
        loc: sig.loc.clone(),
    })
}
