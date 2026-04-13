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
use cyrusc_internal::{flow_state::ControlRegion, monomorph::MonomorphRegistry, vtable::VTableRegistry};
use cyrusc_source_loc::{Loc, SourceMap};
use cyrusc_typed_ast::{
    TypedProgramTree,
    decls::{TypedefDeclID, table::DeclTablesRegistry},
    format::{Formatter, format_loc},
};
use fx_hash::FxHashSet;

pub struct AnalysisContext<'a> {
    pub(crate) config: AnalyzerConfig,
    pub entry_points: Arc<EntryPoints>,
    pub program_tree: Rc<RefCell<TypedProgramTree>>,
    pub(crate) reporter: Arc<DiagReporter>,
    pub(crate) source_map: Arc<SourceMap>,
    pub vtable_registry: Arc<Mutex<VTableRegistry>>,
    pub monomorph_registry: Arc<MonomorphRegistry>,

    pub(crate) decl_tables: Arc<DeclTablesRegistry>,
    pub(crate) formatter: &'a dyn Formatter,

    pub(crate) func_env: FuncEnv,
    pub(crate) type_cache: TypeCache,

    pub(crate) control_stack: Vec<ControlRegion>,
    pub(crate) generic_env_stack: Vec<GenericEnv>,

    pub(crate) typedef_expansion_stack: Vec<TypedefDeclID>,
    pub(crate) reported_typedef_cycles: FxHashSet<Vec<TypedefDeclID>>,
}

impl<'a> AnalysisContext<'a> {
    pub fn new(
        config: AnalyzerConfig,
        reporter: Arc<DiagReporter>,
        source_map: Arc<SourceMap>,
        decl_tables: Arc<DeclTablesRegistry>,
        formatter: &'a dyn Formatter,
        program_tree: Rc<RefCell<TypedProgramTree>>,
        entry_points: Arc<EntryPoints>,
        monomorph_registry: Arc<MonomorphRegistry>,
        vtable_registry: Arc<Mutex<VTableRegistry>>,
    ) -> Self {
        let func_env = FuncEnv::new();
        let generic_env_stack = Vec::new();
        let type_cache = TypeCache::new();
        let control_stack = Vec::new();
        let typedef_expansion_stack = Vec::new();
        let reported_typedef_cycles = FxHashSet::default();

        Self {
            typedef_expansion_stack,
            reported_typedef_cycles,
            generic_env_stack,
            type_cache,
            func_env,
            config,
            reporter,
            source_map,
            control_stack,
            program_tree,
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
