use crate::{
    SymbolID,
    format::format_concrete_type,
    generics::{diagnostics::GenericTypesDiagKind, mapping_ctx::GenericMappingCtx},
    stmts::{TypedGenericParamsList, TypedTypeArg, TypedTypeArgs},
};
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc};
use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericType {
    pub base: SymbolID,
    pub state: GenericTypeState,
    pub mapping_ctx: Rc<RefCell<GenericMappingCtx>>,
    pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericTypeState {
    Unresolved { type_args: TypedTypeArgs },
    Resolved,
}

impl GenericType {
    pub fn init(&self, template: TypedGenericParamsList) -> Result<&Self, Diag> {
        match self.state.get_unresolved_type_args() {
            Some(type_args) => {
                for type_arg in type_args {
                    match type_arg {
                        TypedTypeArg::Positional { idx, ty, loc } => {
                            let generic_param = template.get_positional(*idx).ok_or(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(GenericTypesDiagKind::UndefinedPositionalGenericParam { idx: *idx }),
                                location: Some(DiagLoc::new(loc.clone())),
                                hint: None,
                            })?;

                            let mut mapping_ctx = self.mapping_ctx.borrow_mut();
                            mapping_ctx.insert_named(generic_param.param_name.clone(), ty.clone());
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

                Ok(self)
            }
            None => Ok(self),
        }
    }

    pub fn format(&self, format_symbol: impl Fn(SymbolID) -> String) -> String {
        let base = format_symbol(self.base);
        let is_const = if self.is_const { "const " } else { "" };

        match &self.state {
            GenericTypeState::Unresolved { type_args } => {
                let type_args_str = type_args
                    .iter()
                    .map(|type_arg| match type_arg {
                        TypedTypeArg::Positional { ty, .. } => format_concrete_type(ty.clone(), &format_symbol),
                        TypedTypeArg::Named { key, ty, .. } => {
                            format!("{}: {}", key, format_concrete_type(ty.clone(), &format_symbol))
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("{}{}<{}>", is_const, base, type_args_str)
            }
            GenericTypeState::Resolved => {
                unreachable!("Cannot format resolved generic type.")
            }
        }
    }
}

impl GenericTypeState {
    pub fn get_unresolved_type_args(&self) -> Option<&Vec<TypedTypeArg>> {
        match self {
            GenericTypeState::Unresolved { type_args } => Some(type_args),
            GenericTypeState::Resolved => None,
        }
    }
}

impl Hash for GenericType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.base.hash(state);
        self.mapping_ctx.borrow().hash(state);
    }
}
