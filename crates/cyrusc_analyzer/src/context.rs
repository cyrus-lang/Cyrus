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
    TypedProgramTree,
    decls::{TypedefDeclID, table::DeclTablesRegistry},
    format::{Formatter, format_loc},
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
    pub(crate) generic_env_stack: Vec<GenericEnv>,

    pub(crate) typedef_expansion_stack: Vec<TypedefDeclID>,
    pub(crate) typedef_cycle_reported: bool,
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
        let typedef_expansion_stack = Vec::new();

        Self {
            typedef_expansion_stack,
            typedef_cycle_reported: false,
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
    /// Pushes a typedef to the stack.
    /// Returns Err(Vec<TypedefDeclID>) containing the cycle path if a cycle is detected.
    pub(crate) fn push_typedef_expansion(&mut self, id: TypedefDeclID) -> Result<(), Vec<TypedefDeclID>> {
        if let Some(index) = self.typedef_expansion_stack.iter().position(|&x| x == id) {
            // found a cycle!
            // the cycle is from the first occurrence of 'id' to the end, plus 'id' again.
            let mut cycle_path = self.typedef_expansion_stack[index..].to_vec();
            cycle_path.push(id);

            return Err(cycle_path);
        }

        self.typedef_expansion_stack.push(id);
        Ok(())
    }

    #[inline]
    pub(crate) fn pop_typedef_expansion(&mut self) {
        self.typedef_expansion_stack.pop();

        // Reset the reporting flag when we fully clear the stack
        if self.typedef_expansion_stack.is_empty() {
            self.typedef_cycle_reported = false;
        }
    }
}
