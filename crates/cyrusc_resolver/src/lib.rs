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

use crate::{diagnostics::ResolverDiagKind, modules::ImportedModuleEntry};
use cyrusc_ast::abi::Visibility;
use cyrusc_ast::format::module_segments_as_string;
use cyrusc_ast::*;
use cyrusc_diagcentral::source_loc::SourceLoc;
use cyrusc_diagcentral::{reporter::DiagReporter, *};
use cyrusc_modulefsloader::{ModuleAlias, ModuleLoader, ModuleLoaderOptions, make_module_name_from_filepath};
use cyrusc_tast::generics::mapping_ctx_arena::GenericMappingCtxArena;
use cyrusc_tast::generics::monomorph::MonomorphRegistry;
use cyrusc_tast::stmts::*;
use cyrusc_tast::*;
use cyrusc_tokens::loc::{Location, Span};
use cyrusc_tokens::{Token, TokenKind};
use rand::Rng;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

mod diagnostics;
pub mod macros;
pub mod modules;
pub mod traverse;

pub type GlobalSymbolsMutex = Mutex<HashMap<ModuleID, SymbolTable>>;

type ModuleGroupName = String;
type ImportedModules = HashMap<ModuleGroupName, ModuleID>;

pub struct Resolver {
    // symbol table that holds
    pub global_symbols: Arc<GlobalSymbolsMutex>,

    // Holds the analyzed modules which is used to prevent analyzing a module multiple times.
    pub analyzed_modules: Arc<Mutex<HashSet<ModuleID>>>,

    // Holds the program trees of the modules that are analyzed successfully.
    pub program_trees: Arc<Mutex<Vec<Rc<ProgramTreeEntry>>>>,

    // Holds file path related to the module.
    pub file_paths: Arc<Mutex<HashMap<ModuleID, PathBuf>>>,

    // Diagnostic Reporter Instance.
    pub reporter: DiagReporter,

    // TODO: Consider to inject ModuleLoader via an implementation of a trait instead of direct injection.
    pub module_loader: ModuleLoader,

    // TODO: Explain why these two are necessary in resolver layer and what we are doing with them here.
    pub monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
    mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,

    // Holds the file path of entry module.
    master_module_file_path: PathBuf,

    // Holds the imported modules by current module by their alias.
    module_aliases: Arc<Mutex<HashMap<ModuleID, ImportedModules>>>,

    // Holds a list of imported modules by current module.
    imported_modules: HashSet<ImportedModuleEntry>,

    // Holds reference to current module.
    current_module: Option<ModuleID>,

    // Used to resolve self type.
    current_object: Option<SymbolID>,

    // Used to resolve generic params.
    current_object_generic_params: Option<TypedGenericParamsList>,
}

pub(crate) struct ProgramTreeEntry {
    pub module_id: ModuleID,
    pub module_name: String,
    pub module_path: PathBuf,
    pub program: Rc<RefCell<TypedProgramTree>>,
}

impl Resolver {
    pub fn new(
        opts: ModuleLoaderOptions,
        monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
        mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
        master_module_file_path: PathBuf,
    ) -> Self {
        let file_paths = Arc::new(Mutex::new(HashMap::new()));

        Self {
            global_symbols: Arc::new(Mutex::new(HashMap::new())),
            analyzed_modules: Arc::new(Mutex::new(HashSet::new())),
            module_aliases: Arc::new(Mutex::new(HashMap::new())),
            program_trees: Arc::new(Mutex::new(Vec::new())),
            imported_modules: HashSet::new(),
            reporter: DiagReporter::new(),
            module_loader: ModuleLoader::new(opts),
            file_paths: file_paths.clone(),
            current_module: None,
            current_object: None,
            master_module_file_path,
            monomorph_registry,
            mapping_ctx_arena,
            current_object_generic_params: None,
        }
    }

    #[inline]
    pub fn current_file_path(&self) -> String {
        let current_module_id = self.current_module.unwrap();
        let file_paths = self.file_paths.lock().unwrap();
        let file_path = match file_paths.get(&current_module_id) {
            Some(child_module_file_path) => child_module_file_path.clone(),
            None => self.master_module_file_path.clone(),
        };
        drop(file_paths);
        file_path.to_string_lossy().to_string()
    }
}

unsafe impl Send for Resolver {}
unsafe impl Sync for Resolver {}
