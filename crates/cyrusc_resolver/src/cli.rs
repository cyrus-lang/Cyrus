// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::get_directory_of_file;
use cyrusc_internal::compiler_options::CompilerOptions;
use cyrusc_parser::SourceParser;
use cyrusc_resolver::{
    Resolver,
    fs_module_loader::{FsModuleLoader, FsModuleLoaderOptions},
    modules::VisitingModule,
};
use cyrusc_source_loc::SourceMap;
use cyrusc_typed_ast::decls::table::DeclTablesRegistry;
use std::{env, path::Path, process::exit, sync::Arc, vec};

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();

    let input_dir = get_directory_of_file(&file_path).expect("failed to resolve input directory");

    let source_map = Arc::new(SourceMap::new());
    let file_id = source_map.add_file_by_loading(file_path.clone());
    let entry_source_file = { source_map.get_file(file_id).unwrap().clone() };

    let reporter = Arc::new(DiagReporter::new(source_map.clone()));
    let source_parser = Arc::new(SourceParser::new(reporter.clone()));

    let opts = CompilerOptions::default();

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

            let decl_tables = Arc::new(DeclTablesRegistry::new());

            let mut resolver = Resolver::new(&opts, Box::new(fs_module_loader), source_map.clone(), reporter, decl_tables);

            let module_symbol_id = resolver.create_entry_module_symbol_id(Path::new(&file_path), file_id);

            let typed_program_tree = resolver
                .resolve_module(module_symbol_id, &program, &mut VisitingModule::new(), file_id, true)
                .unwrap();

            if resolver.reporter.has_errors() {
                resolver.reporter.display();
                exit(1);
            }

            dbg!(typed_program_tree);
        }
        Err(()) => exit(1),
    }
}
