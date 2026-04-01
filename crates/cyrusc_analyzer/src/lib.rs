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

use crate::{diagnostics::AnalyzerDiagKind, normalizer::TypeCache, typecheck::typecheck::FuncEnv};
use cyrusc_diagcentral::{Diag, DiagLevel, exit_with_single_diag, reporter::DiagReporter};
use cyrusc_internal::{flow_state::ControlRegion, symbols::table::SymbolQuery, vtable::VTableRegistry};
use cyrusc_source_loc::{Loc, SourceMap};
use cyrusc_typed_ast::{TypedProgramTree, decls::table::DeclTablesRegistry, format::{Formatter, format_loc}};
use std::{
    cell::RefCell,
    rc::Rc,
    sync::{Arc, Mutex},
};

pub mod analyze;
mod diagnostics;
mod generics;
mod lower;
mod nameconv;
mod normalizer;
mod typecheck;

pub struct AnalysisContext<'a> {
    pub(crate) config: AnalyzerConfig,
    pub entry_points: Arc<EntryPoints>,
    pub program_tree: Rc<RefCell<TypedProgramTree>>,
    pub(crate) reporter: Arc<DiagReporter>,
    pub vtable_registry: Arc<Mutex<VTableRegistry>>,
    pub monomorph_registry: Arc<Mutex<MonomorphRegistry>>,

    pub(crate) query: &'a dyn SymbolQuery,
    pub(crate) formatter: Formatter<'a>,
    pub(crate) decl_tables: Arc<DeclTablesRegistry>,

    pub(crate) fenv: FuncEnv,
    pub(crate) type_cache: TypeCache,

    control_stack: Vec<ControlRegion>,
}

pub struct EntryPoints {
    locs: Mutex<Vec<Loc>>,
    source_map: Arc<SourceMap>,
}

#[derive(Debug, Clone)]
pub struct AnalyzerConfig {
    pub warnings: WarningConfig,
    pub strict_mode: bool,
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
    pub fn new(source_map: Arc<SourceMap>) -> Self {
        Self {
            locs: Mutex::new(Vec::new()),
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

            exit_with_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MultipleEntryPoints),
                loc: entry_points.last().copied(),
                hint: hint_loc.map(|loc| format!("Another declaration is at {}.", format_loc(&self.source_map, loc))),
            });
        }
    }
}
