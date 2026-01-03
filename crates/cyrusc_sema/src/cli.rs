// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::{get_directory_of_file, read_file};
use cyrusc_lexer::Lexer;
use cyrusc_modulefsloader::ModuleLoaderOptions;
use cyrusc_parser::Parser;
use cyrusc_resolver::{Resolver, Visiting, generate_module_id};
use cyrusc_sema::analyze::AnalysisContext;
use cyrusc_tast::generics::monomorph::MonomorphRegistry;
use std::{
    env,
    process::exit,
    sync::{Arc, Mutex},
    vec,
};

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();
    let file_content = read_file(file_path.clone()).0;
    let mut lexer = Lexer::new(file_content, file_path.clone());
    let mut parser = Parser::new(lexer.tokenize(), file_path.clone());

    match parser.parse() {
        Ok(program) => {
            let mut current_dir = env::current_dir().unwrap();
            current_dir.push("./stdlib");
            let stdlib_path = current_dir.canonicalize().unwrap().to_str().unwrap().to_string();

            let input_file_dir = get_directory_of_file(file_path.clone()).unwrap();
            let module_loader_opts = ModuleLoaderOptions {
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

            let monomorph_registry = Arc::new(Mutex::new(MonomorphRegistry::new()));

            let mut resolver = Resolver::new(module_loader_opts, monomorph_registry.clone(), file_path.clone());
            let module_id = generate_module_id();

            // resolve entry module
            resolver.resolve_module(module_id, &program, &mut Visiting::new(), true, file_path);

            {
                let entry_points = Arc::new(Mutex::new(Vec::new()));

                let resolved_program_trees = resolver.program_trees.lock().unwrap();
                for program_tree_entry in &*resolved_program_trees {
                    let mut analyzer = AnalysisContext::new(
                        &resolver,
                        module_id,
                        program_tree_entry.program.clone(),
                        entry_points.clone(),
                        monomorph_registry.clone(),
                        true,
                    );
                    analyzer.analyze();
                    DiagReporter::display(&analyzer.reporter);
                    if analyzer.reporter.has_errors() {
                        continue;
                    }

                    dbg!(program_tree_entry.program.clone());
                }

                AnalysisContext::check_entry_points(entry_points);
                drop(resolved_program_trees);
            }
        }
        Err(errors) => {
            parser.display_parser_errors(errors.clone());
            exit(1);
        }
    }
}
