use crate::{
    SymbolID,
    format::format_sema_ty,
    generics::{
        diagnostics::GenericTypesDiagKind,
        mapping_ctx::{GenericMappingCtx, mapping_ctx_eq_refcell},
    },
    stmts::{TypedGenericParamsList, TypedTypeArg, TypedTypeArgs},
};
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc};
use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
    rc::Rc,
};

#[derive(Debug, Clone, Eq)]
pub struct GenericType {
    pub base: SymbolID,
    pub type_args: TypedTypeArgs,
    pub mapping_ctx: Rc<RefCell<GenericMappingCtx>>,
    pub is_const: bool,
    pub loc: SourceLoc,
}

impl GenericType {
    pub fn new_unresolved(
        base: SymbolID,
        type_args: TypedTypeArgs,
        mapping_ctx: Rc<RefCell<GenericMappingCtx>>,
        is_const: bool,
        loc: SourceLoc,
    ) -> Self {
        Self {
            base,
            type_args,
            mapping_ctx,
            is_const,
            loc,
        }
    }

    pub fn init(&mut self, template: TypedGenericParamsList) -> Result<(), Diag> {
        for type_arg in &self.type_args {
            match type_arg {
                TypedTypeArg::Positional { idx, ty, loc } => {
                    let generic_param = template.get_positional(*idx).ok_or(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(GenericTypesDiagKind::UndefinedPositionalGenericParam { idx: *idx }),
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: None,
                    })?;

                    let mut mapping_ctx = self.mapping_ctx.borrow_mut();

                    if let Some(target_generic_param) = ty.as_generic_param() {
                        mapping_ctx.insert_linked(target_generic_param.symbol_id, generic_param.param_name.symbol_id);
                    } else {
                        mapping_ctx.insert_named(generic_param.param_name.clone(), ty.clone());
                    }

                    drop(mapping_ctx);
                }
                TypedTypeArg::Named { key, ty, loc } => {
                    let generic_param = template.get_named(key).ok_or(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(GenericTypesDiagKind::UndefinedGenericParam { name: key.clone() }),
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: None,
                    })?;

                    let mut mapping_ctx = self.mapping_ctx.borrow_mut();
                    mapping_ctx.insert_named(generic_param.param_name.clone(), ty.clone());
                    drop(mapping_ctx);
                }
            }
        }
        Ok(())
    }

    pub fn finalize(
        &self,
        template: TypedGenericParamsList,
        format_symbol: impl Fn(SymbolID) -> String,
    ) -> Result<&Self, Diag> {
        // infer default generic params if not specified explicitly

        let mut mapping_ctx = self.mapping_ctx.borrow_mut();
        for generic_param in &template.list {
            if mapping_ctx
                .get_with_symbol_id(generic_param.param_name.symbol_id)
                .is_none()
                && generic_param.default.is_some()
            {
                mapping_ctx.insert_named(generic_param.param_name.clone(), generic_param.default.clone().unwrap());
            }
        }
        drop(mapping_ctx);
        
        if self.includes_unresolved_generic_param(template) {
            let ty = self.format(format_symbol);

            return Err(Diag {
                level: DiagLevel::Error,
                kind: Box::new(GenericTypesDiagKind::RequiresExplicitTypeArgs { ty }),
                location: Some(DiagLoc::new(self.loc.clone())),
                hint: None,
            });
        }

        Ok(self)
    }

    fn includes_unresolved_generic_param(&self, template: TypedGenericParamsList) -> bool {
        {
            let mapping_ctx = self.mapping_ctx.borrow();
            for generic_param in &template.list {
                if mapping_ctx
                    .get_with_symbol_id(generic_param.param_name.symbol_id)
                    .is_none()
                {
                    return true;
                }
            }
            return false;
        }
    }

    pub fn format(&self, format_symbol: impl Fn(SymbolID) -> String) -> String {
        let base = format_symbol(self.base);
        let is_const = if self.is_const { "const " } else { "" };

        let type_args_str = self
            .type_args
            .iter()
            .map(|type_arg| match type_arg {
                TypedTypeArg::Positional { ty, .. } => format_sema_ty(ty.clone(), &format_symbol),
                TypedTypeArg::Named { key, ty, .. } => {
                    format!("{}: {}", key, format_sema_ty(ty.clone(), &format_symbol))
                }
            })
            .collect::<Vec<String>>()
            .join(", ");

        format!(
            "{}{}{}",
            is_const,
            base,
            if self.type_args.len() > 0 {
                format!("<{}>", type_args_str)
            } else {
                "".to_string()
            }
        )
    }
}

impl PartialEq for GenericType {
    fn eq(&self, other: &Self) -> bool {
        if self.base != other.base {
            return false;
        }

        mapping_ctx_eq_refcell(&self.mapping_ctx, &other.mapping_ctx)
    }
}

impl Hash for GenericType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.base.hash(state);
        self.mapping_ctx.borrow().hash(state);
    }
}
