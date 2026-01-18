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
use crate::{
    SymbolID,
    exprs::TypedIdentifier,
    format::format_sema_ty,
    generics::{
        diagnostics::GenericTypesDiagKind,
        mapping_ctx::{GenericMappingCtx, GenericMappingEntry, mapping_ctx_eq_refcell},
        mapping_ctx_arena::GenericMappingCtxArena,
    },
    stmts::{TypedGenericParamsList, TypedTypeArg, TypedTypeArgs},
    types::SemanticType,
};
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc, source_loc::SourceLoc};
use std::{
    cell::RefCell,
    fmt::Debug,
    hash::{Hash, Hasher},
    rc::Rc,
    sync::{Arc, Mutex},
};

#[derive(Clone)]
pub struct GenericType {
    pub base: SymbolID,
    pub type_args: TypedTypeArgs,
    pub mapping_ctx: Rc<RefCell<GenericMappingCtx>>,
    pub mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    pub altered_generic_params: Option<TypedGenericParamsList>,
    pub is_const: bool,
    pub loc: SourceLoc,
}

impl GenericType {
    pub fn new_unresolved(
        base: SymbolID,
        type_args: TypedTypeArgs,
        mapping_ctx: Rc<RefCell<GenericMappingCtx>>,
        mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
        is_const: bool,
        loc: SourceLoc,
    ) -> Self {
        Self {
            base,
            type_args,
            mapping_ctx,
            mapping_ctx_arena,
            altered_generic_params: None,
            is_const,
            loc,
        }
    }

    fn check_for_overriding_parent_generic_param(
        &self,
        mapping_ctx_arena: &dyn GenericMappingCtxArena,
        child_mapping_ctx: &GenericMappingCtx,
        generic_param_name: String,
        type_arg_sema_ty: Option<SemanticType>,
        format_symbol: &impl Fn(SymbolID) -> String,
        loc: SourceLoc,
    ) -> Result<(), Diag> {
        if let Some(parent_id) = child_mapping_ctx.get_parent_id() {
            let parent_mapping_ctx = mapping_ctx_arena.get(parent_id).unwrap();

            if let Some(parent_sema_ty) = parent_mapping_ctx.get_with_name(mapping_ctx_arena, &generic_param_name) {
                // NOTE: complain only and only if overrode type is not the same!
                // situations come that type args may always presented explicitly, so we should not complain about that.
                if type_arg_sema_ty
                    .map(|inferred_ty| inferred_ty != parent_sema_ty)
                    .unwrap_or(false)
                {
                    return Err(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(GenericTypesDiagKind::CannotOverrideParentInferredGenericParam {
                            generic_param: generic_param_name.clone(),
                            already_inferred_as: format_sema_ty(parent_sema_ty, &format_symbol),
                        }),
                        location: Some(DiagLoc::new(loc)),
                        hint: None,
                    });
                }
            }
        }

        Ok(())
    }

    pub fn init(
        &mut self,
        mapping_ctx_arena: &dyn GenericMappingCtxArena,
        template: TypedGenericParamsList,
        format_symbol: &impl Fn(SymbolID) -> String,
    ) -> Result<(), Diag> {
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

                    self.check_for_overriding_parent_generic_param(
                        mapping_ctx_arena,
                        &mapping_ctx,
                        generic_param.param_name.name.clone(),
                        Some(ty.clone()),
                        format_symbol,
                        loc.clone(),
                    )?;

                    if let Some(target_generic_param) = ty.as_generic_param() {
                        mapping_ctx.insert_linked(
                            GenericMappingEntry::from(target_generic_param.param_name.clone()),
                            GenericMappingEntry::from(generic_param.param_name.clone()),
                        );
                    } else {
                        mapping_ctx
                            .insert_named(GenericMappingEntry::from(generic_param.param_name.clone()), ty.clone());
                    }

                    drop(mapping_ctx);
                }
                TypedTypeArg::Named { key, ty, loc } => {
                    let mut mapping_ctx = self.mapping_ctx.borrow_mut();

                    self.check_for_overriding_parent_generic_param(
                        mapping_ctx_arena,
                        &mapping_ctx,
                        key.clone(),
                        Some(ty.clone()),
                        format_symbol,
                        loc.clone(),
                    )?;

                    let typed_identifier = template
                        .get_named(key)
                        .map(|generic_param| GenericMappingEntry::from(generic_param.param_name.clone()))
                        .or(mapping_ctx.get_linked_by_name(mapping_ctx_arena, &key))
                        .ok_or({
                            Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(GenericTypesDiagKind::UndefinedGenericParam { name: key.clone() }),
                                location: Some(DiagLoc::new(loc.clone())),
                                hint: None,
                            }
                        })?;

                    mapping_ctx.insert_named(GenericMappingEntry::from(typed_identifier), ty.clone());
                    drop(mapping_ctx);
                }
            }
        }
        Ok(())
    }

    pub fn finalize(
        &self,
        mapping_ctx_arena: &dyn GenericMappingCtxArena,
        template: TypedGenericParamsList,
        format_symbol: impl Fn(SymbolID) -> String,
    ) -> Result<&Self, Diag> {
        // fill defaults
        {
            let mut mapping_ctx = self.mapping_ctx.borrow_mut();
            for generic_param in &template.list {
                if mapping_ctx
                    .get_with_name(mapping_ctx_arena, &generic_param.param_name.name)
                    .is_none()
                {
                    if let Some(default) = &generic_param.default {
                        mapping_ctx.insert_named(
                            GenericMappingEntry::from(generic_param.param_name.clone()),
                            *default.clone(),
                        );
                    }
                }
            }
        }

        // detect + collect unresolved generic params
        let missing = self.collect_unresolved_generic_params(mapping_ctx_arena, &template);

        if !missing.is_empty() {
            let ty = self.format(&format_symbol);

            let missing_fmt = missing
                .iter()
                .map(|ident| format!("'{}'", ident.name))
                .collect::<Vec<_>>()
                .join(", ");

            return Err(Diag {
                level: DiagLevel::Error,
                kind: Box::new(GenericTypesDiagKind::RequiresExplicitTypeArgs { ty }),
                location: Some(DiagLoc::new(self.loc.clone())),
                hint: Some(format!("Missing generic parameters: {}", missing_fmt)),
            });
        }

        Ok(self)
    }

    fn collect_unresolved_generic_params(
        &self,
        mapping_ctx_arena: &dyn GenericMappingCtxArena,
        template: &TypedGenericParamsList,
    ) -> Vec<TypedIdentifier> {
        let mapping_ctx = self.mapping_ctx.borrow();

        template
            .list
            .iter()
            .filter(|gp| {
                mapping_ctx
                    .get_with_name(mapping_ctx_arena, &gp.param_name.name)
                    .is_none()
            })
            .map(|gp| gp.param_name.clone())
            .collect()
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
                    format!("{} = {}", key, format_sema_ty(ty.clone(), &format_symbol))
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

        mapping_ctx_eq_refcell(self.mapping_ctx_arena.clone(), &self.mapping_ctx, &other.mapping_ctx)
    }
}

impl Hash for GenericType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.base.hash(state);
    }
}

impl Debug for GenericType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GenericType")
            .field("base", &self.base)
            .field("type_args", &self.type_args)
            .field("mapping_ctx", &self.mapping_ctx)
            .field("altered_generic_params", &self.altered_generic_params)
            .field("is_const", &self.is_const)
            .field("loc", &self.loc)
            .finish()
    }
}

impl Eq for GenericType {}

#[cfg(debug_assertions)]
pub fn debug_generic_type<'a>(
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    generic_type: &GenericType,
    format_symbol: &(dyn Fn(SymbolID) -> String + 'a),
) {
    use crate::types::SemanticType;

    println!("-----------------------");
    println!("Base: {}", format_symbol(generic_type.base));

    if let Some(generic_params) = &generic_type.altered_generic_params {
        println!("Altered Generic Params: ");

        for generic_param in &generic_params.list {
            print!("{}", generic_param.param_name.name.clone());
            if let Some(default) = &generic_param.default {
                print!(" default({})", format_sema_ty(*default.clone(), format_symbol));
            }
            if let Some(bounds) = &generic_param.bounds {
                print!(
                    " bounds({})",
                    bounds
                        .iter()
                        .map(|bound| {
                            let type_args = bound
                                .type_args
                                .iter()
                                .map(|type_arg| match type_arg {
                                    TypedTypeArg::Positional { idx, ty, .. } => {
                                        format!("  {}:{}\n", idx, format_sema_ty(ty.clone(), format_symbol))
                                    }
                                    TypedTypeArg::Named { key, ty, .. } => {
                                        format!("  {}:{}\n", key, format_sema_ty(ty.clone(), format_symbol))
                                    }
                                })
                                .collect::<Vec<String>>()
                                .join(", ");

                            format!("{}<{}>", bound.symbol.value.clone(), type_args)
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }
        }
    }

    println!("Type Args: ");
    for type_arg in &generic_type.type_args {
        match type_arg {
            TypedTypeArg::Positional { idx, ty, .. } => {
                println!("  {}:{}", idx, format_sema_ty(ty.clone(), format_symbol));
            }
            TypedTypeArg::Named { key, ty, .. } => {
                println!("  {}:{}", key, format_sema_ty(ty.clone(), format_symbol));
            }
        }
    }

    {
        println!("");
        println!("MappingCtx: ");

        let debug_mapping_ctx = |mapping_ctx: &GenericMappingCtx| {
            for (entry, sema_ty) in mapping_ctx.get_named_mapping() {
                println!(
                    "{} -> {}",
                    entry.name.clone(),
                    format_sema_ty(sema_ty.clone(), format_symbol)
                );
            }
        };

        let mapping_ctx = generic_type.mapping_ctx.borrow();
        debug_mapping_ctx(&mapping_ctx);

        fn recurse_debug_mapping_ctx(
            mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
            debug_mapping_ctx: impl Fn(&GenericMappingCtx),
            parent_id: usize,
        ) {
            let arena_mutex = mapping_ctx_arena.lock().unwrap();
            let parent_mapping_ctx = arena_mutex.get(parent_id).unwrap().clone();
            debug_mapping_ctx(&parent_mapping_ctx);
            drop(arena_mutex);

            if let Some(parent_parent_id) = parent_mapping_ctx.get_parent_id() {
                recurse_debug_mapping_ctx(mapping_ctx_arena, debug_mapping_ctx, parent_parent_id);
            }
        }

        if let Some(parent_id) = mapping_ctx.get_parent_id() {
            println!("ParentMappingCtx({}): ", parent_id);

            recurse_debug_mapping_ctx(mapping_ctx_arena, debug_mapping_ctx, parent_id);
        }

        println!("");
    }

    println!(
        "Inline Format: {}",
        format_sema_ty(SemanticType::GenericType(generic_type.clone()), format_symbol)
    );

    println!("-----------------------");
    println!("");
}
