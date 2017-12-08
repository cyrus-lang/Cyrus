use crate::diagnostics::GenericsDiagKind;
use ast::source_loc::SourceLoc;
use diagcentral::{Diag, DiagLevel, DiagLoc};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use typed_ast::{
    ScopeID, SymbolID, TypedGenericParam, TypedGenericParamsList, TypedIdentifier, TypedTypeArg, TypedTypeArgs,
    types::ConcreteType,
};

mod diagnostics;

pub type ChildGenericParamSymbolID = SymbolID;

#[derive(Debug, Clone, Default)]
pub struct GenericMappingCtx {
    pub named: HashMap<TypedIdentifier, ConcreteType>,
    pub parent: Option<Box<GenericMappingCtx>>,
    pub linked_gps: Rc<RefCell<HashMap<ChildGenericParamSymbolID, SymbolID>>>,
}

#[macro_export]
macro_rules! generic_mapping_ctx_scope {
    // parent context provided explicitly
    ($self:ident, $generic_params:expr, $type_args:expr, $loc:expr, $parent_ctx:expr, $exposed_ctx:ident, $body:block) => {{
        let mut __ctx = $parent_ctx
            .and_then(|ctx| Some(ctx.new_child()))
            .unwrap_or(GenericMappingCtx::new_root());

        #[allow(unused_mut)]
        let mut __child_ctx = $self.get_generic_mapping_ctx(&mut __ctx, &$generic_params, &$type_args, $loc);

        $self.generic_ctx_stack.push(__child_ctx.clone());
        let __result = {
            let mut $exposed_ctx = __child_ctx;
            $body
        };
        $self.generic_ctx_stack.pop();

        __result
    }};
}

impl GenericMappingCtx {
    pub fn get_with_symbol_id(&self, symbol_id: SymbolID) -> Option<ConcreteType> {
        fn lookup_named(ctx: &GenericMappingCtx, sid: SymbolID) -> Option<ConcreteType> {
            ctx.named
                .iter()
                .find_map(|(key, val)| (key.symbol_id == sid).then(|| val.clone()))
        }

        fn lookup_in_chain(mut ctx_opt: Option<&Box<GenericMappingCtx>>, sid: SymbolID) -> Option<ConcreteType> {
            while let Some(ctx) = ctx_opt {
                if let Some(t) = lookup_named(ctx, sid) {
                    return Some(t);
                }
                ctx_opt = ctx.parent.as_ref();
            }
            None
        }

        fn lookup_via_links(ctx: &GenericMappingCtx, sid: SymbolID) -> Option<ConcreteType> {
            let linked_gps = ctx.linked_gps.borrow();

            for (child, mapped) in &*linked_gps {
                if *child == sid {
                    if let Some(t) =
                        lookup_named(ctx, *mapped).or_else(|| lookup_in_chain(ctx.parent.as_ref(), *mapped))
                    {
                        return Some(t);
                    }
                } else if *mapped == sid {
                    if let Some(t) = lookup_named(ctx, *child).or_else(|| lookup_in_chain(ctx.parent.as_ref(), *child))
                    {
                        return Some(t);
                    }
                }
            }
            None
        }

        lookup_named(self, symbol_id)
            .or_else(|| lookup_via_links(self, symbol_id))
            .or_else(|| self.parent.as_ref()?.get_with_symbol_id(symbol_id))
    }

    pub fn insert_named(&mut self, id: TypedIdentifier, ty: ConcreteType) {
        // only insert if this id is not yet inferred
        if !self.named.contains_key(&id) {
            self.named.insert(id.clone(), ty.clone());
        }
    }

    pub fn insert_linked(&self, child_id: SymbolID, parent_id: SymbolID) {
        self.linked_gps.borrow_mut().insert(child_id, parent_id);
    }

    pub fn new_root() -> Self {
        Self {
            named: HashMap::new(),
            linked_gps: Rc::new(RefCell::new(HashMap::new())),
            parent: None,
        }
    }

    pub fn new_child(&self) -> Self {
        Self {
            named: HashMap::new(),
            linked_gps: self.linked_gps.clone(),
            parent: Some(Box::new(self.clone())),
        }
    }
}

pub(crate) fn get_generic_mapping_ctx(
    mapping_ctx: &mut GenericMappingCtx,
    generic_params_opt: &Option<TypedGenericParamsList>,
    input_type_args_opt: &Option<TypedTypeArgs>,
    loc: SourceLoc,
) -> Result<GenericMappingCtx, Diag<GenericsDiagKind>> {
    if let Some(generic_params) = generic_params_opt {
        if let Some(type_args) = input_type_args_opt.as_ref() {
            for (idx, arg) in type_args.iter().enumerate() {
                match arg {
                    TypedTypeArg::Positional(concrete_type) => {
                        let current_gp = match generic_params.get(idx) {
                            Some(gp) => gp,
                            None => continue, // maybe inferred later
                        };

                        if let Some(generic_param) = concrete_type.as_generic_param() {
                            mapping_ctx.insert_linked(current_gp.param_name.symbol_id, generic_param.symbol_id);
                        } else {
                            mapping_ctx.insert_named(current_gp.param_name.clone(), concrete_type.clone());
                        }
                    }
                    TypedTypeArg::Named { key, value } => {
                        if let Some(generic_param) = generic_params.iter().find(|gp| gp.param_name.name == *key) {
                            mapping_ctx.insert_named(generic_param.param_name.clone(), value.clone());
                        }
                    }
                }
            }

            if type_args.len() > generic_params.len() {
                return Err(Diag {
                    level: DiagLevel::Error,
                    kind: GenericsDiagKind::UnexpectedTypeArgs,
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: Some(format!(
                        "Too many positional type arguments (expected <= {}, found {})",
                        generic_params.len(),
                        type_args.len()
                    )),
                });
            }
        }
    } else if input_type_args_opt.is_some() {
        return Err(Diag {
            level: DiagLevel::Error,
            kind: GenericsDiagKind::UnexpectedTypeArgs,
            location: Some(DiagLoc::new(loc.clone())),
            hint: Some("Remove the type arguments or add generic parameters to the type.".to_string()),
        });
    }

    Ok(mapping_ctx.clone())
}

pub(crate) fn normalize_type_args_and_register(
    mapping_ctx: &GenericMappingCtx,
    scope_id_opt: Option<ScopeID>,
    symbol_id: SymbolID,
    generic_params: &Option<TypedGenericParamsList>,
    expected_type: Option<ConcreteType>,
    loc: SourceLoc,
    allow_inference_without_error: bool,
    register_monomorph: bool,
    infer_fn: &dyn Fn(
        SymbolID,
        &[TypedGenericParam],
        &GenericMappingCtx,
        Option<ConcreteType>,
    ) -> Option<Vec<ConcreteType>>,
    register_monomorph_fn: &dyn Fn(SymbolID, Vec<ConcreteType>),
    symbol_formatter: &dyn Fn(Option<ScopeID>, SymbolID) -> String,
) -> Result<Vec<ConcreteType>, Diag<GenericsDiagKind>> {
    let Some(generic_params) = generic_params else {
        return Ok(Vec::new());
    };

    let mut normalized_type_args: Vec<Option<ConcreteType>> = vec![None; generic_params.len()];
    let mut missing: Vec<String> = Vec::new();

    for (name, ty) in &mapping_ctx.named {
        if let Some(idx) = generic_params
            .iter()
            .position(|param| param.param_name.symbol_id == name.symbol_id)
        {
            normalized_type_args[idx] = Some(ty.clone());
        }
    }

    for (i, param) in generic_params.iter().enumerate() {
        if normalized_type_args[i].is_none() {
            if let Some(default_ty) = &param.default {
                normalized_type_args[i] = Some(default_ty.clone());
            }
        }
    }

    for (i, ty_slot) in normalized_type_args.iter_mut().enumerate() {
        if ty_slot.is_none() {
            if let Some(inferred) = infer_fn(symbol_id, &generic_params[i..=i], mapping_ctx, expected_type.clone()) {
                *ty_slot = Some(inferred[0].clone());
            } else {
                missing.push(generic_params[i].param_name.name.clone());
            }
        }
    }

    if !missing.is_empty() && !allow_inference_without_error {
        let type_name = symbol_formatter(scope_id_opt, symbol_id);
        let hint = format!("Provide explicit type arguments for {}", missing.join(", "));
        return Err(Diag {
            level: DiagLevel::Error,
            kind: GenericsDiagKind::ExplicitTypeArgsRequired { type_name },
            location: Some(DiagLoc::new(loc)),
            hint: Some(hint),
        });
    }

    let final_args: Vec<ConcreteType> = normalized_type_args.into_iter().filter_map(|x| x).collect();

    if register_monomorph {
        register_monomorph_fn(symbol_id, final_args.clone());
    }

    Ok(final_args)
}
