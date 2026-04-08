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

use std::{
    cell::RefCell,
    rc::Rc,
    sync::{Arc, Mutex},
};

use crate::{
    diagnostics::AnalyzerDiagKind,
    env::{func_env::FuncEnv, generic_env::GenericEnv},
    typecheck::type_cache::TypeCache,
};
use cyrusc_diagcentral::{Diag, DiagLevel, exit_with_single_diag, reporter::DiagReporter};
use cyrusc_internal::{
    flow_state::ControlRegion, monomorph::MonomorphRegistry, symbols::SymbolQuery, vtable::VTableRegistry,
};
use cyrusc_source_loc::{Loc, SourceMap};
use cyrusc_typed_ast::{
    GenericParamID, TypedProgramTree,
    decls::table::DeclTablesRegistry,
    format::{Formatter, format_loc},
    stmts::{TypedFuncTypeVariadicParams, TypedGenericParams, TypedTypeArg, TypedTypeArgs},
    types::SemanticType,
};

pub struct AnalysisContext<'a> {
    pub(crate) config: AnalyzerConfig,
    pub entry_points: Arc<EntryPoints>,
    pub program_tree: Rc<RefCell<TypedProgramTree>>,
    pub(crate) reporter: Arc<DiagReporter>,
    pub vtable_registry: Arc<Mutex<VTableRegistry>>,
    pub monomorph_registry: Arc<Mutex<MonomorphRegistry>>,

    pub(crate) query: &'a dyn SymbolQuery,
    pub(crate) decl_tables: Arc<DeclTablesRegistry>,
    pub(crate) formatter: &'a dyn Formatter,

    pub(crate) func_env: FuncEnv,
    pub(crate) type_cache: TypeCache,

    pub(crate) control_stack: Vec<ControlRegion>,
    generic_env_stack: Vec<GenericEnv>,
}

impl<'a> AnalysisContext<'a> {
    pub fn new(
        config: AnalyzerConfig,
        reporter: Arc<DiagReporter>,
        query: &'a dyn SymbolQuery,
        decl_tables: Arc<DeclTablesRegistry>,
        formatter: &'a dyn Formatter,
        program_tree: Rc<RefCell<TypedProgramTree>>,
        entry_points: Arc<EntryPoints>,
        monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
        vtable_registry: Arc<Mutex<VTableRegistry>>,
    ) -> Self {
        let func_env = FuncEnv::new();
        let generic_env_stack = Vec::new();
        let type_cache = TypeCache::new();
        let control_stack = Vec::new();

        Self {
            generic_env_stack,
            type_cache,
            func_env,
            config,
            reporter,
            control_stack,
            program_tree,
            query,
            decl_tables,
            formatter,
            entry_points,
            vtable_registry,
            monomorph_registry,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AnalyzerConfig {
    pub warnings: WarningConfig,
    pub strict_mode: bool,
}

pub struct EntryPoints {
    locs: Mutex<Vec<Loc>>,
    reporter: Arc<DiagReporter>,
    source_map: Arc<SourceMap>,
}

#[derive(Debug, Clone)]
pub struct WarningConfig {
    pub enabled: bool,
    pub warnings_as_errors: bool,

    pub unused_variables: bool,
    pub unreachable_code: bool,
    pub dead_code: bool,
}

impl Default for WarningConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            warnings_as_errors: false,
            unused_variables: true,
            unreachable_code: true,
            dead_code: true,
        }
    }
}

impl Default for AnalyzerConfig {
    fn default() -> Self {
        Self {
            warnings: WarningConfig::default(),
            strict_mode: false,
        }
    }
}

impl EntryPoints {
    pub fn new(reporter: Arc<DiagReporter>, source_map: Arc<SourceMap>) -> Self {
        Self {
            locs: Mutex::new(Vec::new()),
            reporter,
            source_map,
        }
    }

    pub(crate) fn add(&self, loc: Loc) {
        self.locs.lock().unwrap().push(loc);
    }

    /// Validates that the program defines exactly one entry point and reports an
    /// error if none or multiple entry points are found.
    pub fn validate(&self) {
        let entry_points = self.locs.lock().unwrap();

        if entry_points.len() == 1 {
            // valid
        } else if entry_points.len() == 0 {
            exit_with_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MissingEntryPoint),
                loc: None,
                hint: None,
            });
        } else {
            let hint_loc = entry_points.get(entry_points.len().saturating_sub(2)).copied();

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MultipleEntryPoints),
                loc: entry_points.last().copied(),
                hint: hint_loc.map(|loc| format!("Another declaration is at {}.", format_loc(&self.source_map, loc))),
            });
        }
    }
}

impl<'a> AnalysisContext<'a> {
    #[inline]
    pub(crate) fn push_generic_env(&mut self, env: GenericEnv) {
        self.generic_env_stack.push(env);
    }

    #[inline]
    pub(crate) fn pop_generic_env(&mut self) {
        self.generic_env_stack.pop();
    }

    #[inline]
    pub(crate) fn current_generic_env(&self) -> Option<&GenericEnv> {
        self.generic_env_stack.last()
    }

    #[inline]
    pub(crate) fn current_generic_env_mut(&mut self) -> Option<&mut GenericEnv> {
        self.generic_env_stack.last_mut()
    }

    pub(crate) fn lookup_generic_binding(&self, generic_param_id: GenericParamID) -> Option<&SemanticType> {
        for env in self.generic_env_stack.iter().rev() {
            if let Some(ty) = env.lookup(generic_param_id) {
                return Some(ty);
            }
        }

        None
    }

    pub(crate) fn substitute_type(&self, ty: &SemanticType) -> SemanticType {
        let mut result = ty.clone();

        for env in self.generic_env_stack.iter().rev() {
            result = env.substitute_sema_type(&result);
        }

        result
    }

    pub(crate) fn with_generic_env<F, R>(&mut self, generic_env: GenericEnv, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.push_generic_env(generic_env);
        let result = f(self);
        self.pop_generic_env();
        result
    }

    pub(crate) fn collect_instantiated_type_args(&self, generic_params: TypedGenericParams) -> TypedTypeArgs {
        let mut args = Vec::with_capacity(generic_params.len());

        for generic_param_id in generic_params.iter() {
            let generic_param = self.decl_tables.generic_param(*generic_param_id);

            if let Some(ty) = self.lookup_generic_binding(*generic_param_id) {
                args.push(TypedTypeArg::Type(ty.clone(), generic_param.name.loc));
            } else {
                args.push(TypedTypeArg::Type(
                    SemanticType::GenericParam(*generic_param_id),
                    generic_param.name.loc,
                ));
            }
        }

        TypedTypeArgs(args)
    }

    pub(crate) fn infer_generic_param(&mut self, expected_type: &SemanticType, actual_type: &SemanticType) {
        match expected_type {
            SemanticType::GenericParam(generic_param_id) => {
                if let Some(generic_env) = self.current_generic_env_mut() {
                    match generic_env.lookup(*generic_param_id) {
                        Some(_) => { /* skip */ }
                        None => {
                            generic_env.bind(*generic_param_id, actual_type.clone());
                        }
                    }
                }
            }
            SemanticType::Named(expected_named) => {
                if let SemanticType::Named(actual_named) = actual_type {
                    if expected_named.decl_id == actual_named.decl_id {
                        for (exp_arg, act_arg) in expected_named.type_args.iter().zip(actual_named.type_args.iter()) {
                            if let (TypedTypeArg::Type(exp_ty, _), TypedTypeArg::Type(act_ty, _)) = (exp_arg, act_arg) {
                                self.infer_generic_param(exp_ty, act_ty);
                            }
                        }
                    }
                }
            }
            SemanticType::Tuple(exp_tuple) => {
                if let SemanticType::Tuple(act_tuple) = actual_type {
                    for (e, a) in exp_tuple.elements.iter().zip(act_tuple.elements.iter()) {
                        self.infer_generic_param(e, a);
                    }
                }
            }
            SemanticType::Array(exp_array) => {
                if let SemanticType::Array(act_array) = actual_type {
                    self.infer_generic_param(&exp_array.element_type, &act_array.element_type);
                }
            }
            SemanticType::Pointer(exp_inner) => {
                if let SemanticType::Pointer(act_inner) = actual_type {
                    self.infer_generic_param(exp_inner, act_inner);
                }
            }
            SemanticType::FuncType(exp_func) => {
                if let SemanticType::FuncType(act_func) = actual_type {
                    for (e, a) in exp_func.params.list.iter().zip(act_func.params.list.iter()) {
                        self.infer_generic_param(e, a);
                    }

                    if let (Some(exp_variadic), Some(act_variadic)) =
                        (exp_func.params.variadic.clone(), act_func.params.variadic.clone())
                    {
                        match (*exp_variadic, *act_variadic) {
                            (
                                TypedFuncTypeVariadicParams::Typed(exp_ty),
                                TypedFuncTypeVariadicParams::Typed(act_ty),
                            ) => {
                                self.infer_generic_param(&exp_ty, &act_ty);
                            }

                            _ => { /* skip */ }
                        }
                    }
                    // self.infer_generic_param(e, a);

                    self.infer_generic_param(&exp_func.ret_type, &act_func.ret_type);
                }
            }
            _ => {}
        }
    }
}
