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

use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::get_directory_of_file;
use cyrusc_internal::vtable::VTableRegistry;
use cyrusc_parser::SourceParser;
use cyrusc_resolver::{
    Resolver,
    fs_module_loader::{FsModuleLoader, FsModuleLoaderOptions},
    modules::VisitingModule,
};
use cyrusc_analyzer::{
    analyze::{AnalysisContext, EntryPoints},
    config::AnalyzerConfig,
};
use cyrusc_source_loc::SourceMap;
use cyrusc_typed_ast::{
    ModuleID,
    generics::{mapping_ctx_arena::GenericMappingCtxArenaImpl, monomorph::MonomorphRegistry},
};
use std::{
    env,
    path::Path,
    process::exit,
    sync::{Arc, Mutex},
    vec,
};

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();

    let input_dir = get_directory_of_file(&file_path).expect("failed to resolve input directory");

    let source_map = Arc::new(SourceMap::new());
    let file_id = source_map.add_file_by_loading(file_path.clone());
    let entry_source_file = { source_map.get_file(file_id).unwrap().clone() };

    let reporter = Arc::new(DiagReporter::new(source_map.clone()));
    let source_parser = Arc::new(SourceParser::new(reporter.clone()));

    match source_parser.parse_program(&entry_source_file) {
        Ok(program) => {
            let mut current_dir = env::current_dir().unwrap();
            current_dir.push("./stdlib");
            let stdlib_path = current_dir.canonicalize().unwrap().to_str().unwrap().to_string();

            let input_file_dir = get_directory_of_file(file_path.clone()).unwrap();
            let module_loader_opts = FsModuleLoaderOptions {
                base_path: current_dir.to_str().unwrap().to_string(),
                stdlib_path: Some(stdlib_path.clone()),
                source_dirs: vec![input_file_dir],
            };

            println!("Stdlib Path: ");
            println!("  {}", stdlib_path);
            println!("Source Dirs: ");
            module_loader_opts.source_dirs.iter().for_each(|file_path| {
                println!("  {}", file_path);
            });

            let fs_module_loader_opts = FsModuleLoaderOptions {
                base_path: env::current_dir().unwrap().to_string_lossy().to_string(),
                stdlib_path: Some(stdlib_path.clone()),
                source_dirs: vec![input_dir.clone()],
            };

            let fs_module_loader = FsModuleLoader::new(source_map.clone(), source_parser, fs_module_loader_opts);

            let mapping_ctx_arena = Arc::new(Mutex::new(GenericMappingCtxArenaImpl::new()));
            let monomorph_registry = Arc::new(Mutex::new(MonomorphRegistry::new()));

            let mut resolver = Resolver::new(
                Box::new(fs_module_loader),
                reporter.clone(),
                monomorph_registry.clone(),
                mapping_ctx_arena.clone(),
                Path::new(&file_path).to_path_buf(),
            );

            let module_id = ModuleID::master_module_id();

            resolver
                .resolve_module(
                    module_id,
                    &program,
                    &mut VisitingModule::new(),
                    true,
                    Path::new(&file_path).to_path_buf(),
                )
                .unwrap();

            if resolver.reporter.has_errors() {
                resolver.reporter.display();
                exit(1);
            }

            {
                let config = AnalyzerConfig::default();

                let entry_points = Arc::new(EntryPoints::new(source_map.clone()));
                let mapping_ctx_arena = Arc::new(Mutex::new(GenericMappingCtxArenaImpl::new()));

                let resolved_program_trees = resolver.program_trees.lock().unwrap();

                for program_tree_entry in &*resolved_program_trees {
                    let vtable_registry = Arc::new(Mutex::new(VTableRegistry::new()));

                    let mut analyzer = AnalysisContext::new(
                        config.clone(),
                        reporter.clone(),
                        source_map.clone(),
                        &resolver,
                        &resolver,
                        module_id,
                        program_tree_entry.program.clone(),
                        entry_points.clone(),
                        monomorph_registry.clone(),
                        mapping_ctx_arena.clone(),
                        vtable_registry,
                    );

                    analyzer.analyze();

                    reporter.display();
                    if reporter.has_errors() {
                        continue;
                    }
                }

                entry_points.validate();

                drop(resolved_program_trees);
            }
        }
        Err(()) => exit(1),
    }
}
