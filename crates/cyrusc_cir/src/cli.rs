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
use cyrusc_abi::mangler::Cyrus_ABI_Impl;
use cyrusc_cir::{monomorph::CIRMonomorphRegistry, walk::walk_program_trees_in_parallel};
use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::{get_directory_of_file, read_file};
use cyrusc_lexer::Lexer;
use cyrusc_modulefsloader::ModuleLoaderOptions;
use cyrusc_parser::Parser;
use cyrusc_resolver::{Resolver, Visiting, generate_module_id};
use cyrusc_sema::analyze::AnalysisContext;
use cyrusc_tast::{
    TypedProgramTree,
    generics::{mapping_ctx_arena::GenericMappingCtxArenaImpl, monomorph::MonomorphRegistry},
};
use cyrusc_vtable_registry::VTableRegistry;
use std::{
    cell::RefCell,
    env,
    process::exit,
    rc::Rc,
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
        Ok(program_tree) => {
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

            let mapping_ctx_arena = Arc::new(Mutex::new(GenericMappingCtxArenaImpl::new()));
            let monomorph_registry = Arc::new(Mutex::new(MonomorphRegistry::new()));

            let mut resolver = Resolver::new(
                module_loader_opts,
                monomorph_registry.clone(),
                mapping_ctx_arena.clone(),
                file_path.clone(),
            );
            let module_id = generate_module_id();
            resolver.resolve_module(module_id, &program_tree, &mut Visiting::new(), true, file_path.clone());
            if resolver.reporter.has_errors() {
                DiagReporter::display(&resolver.reporter);
                exit(1);
            }

            let mut vtable_registries: Vec<Arc<Mutex<VTableRegistry>>> = Vec::new();
            let entry_points = Arc::new(Mutex::new(Vec::new()));

            let resolved_program_trees = resolver.program_trees.lock().unwrap();

            let mut analyzed_program_trees: Vec<Rc<RefCell<TypedProgramTree>>> = Vec::new();
            for program_tree_entry in &*resolved_program_trees {
                // vtable registry of module
                let vtable_registry = Arc::new(Mutex::new(VTableRegistry::new()));
                vtable_registries.push(vtable_registry.clone());

                let mut analyzer = AnalysisContext::new(
                    &resolver,
                    program_tree_entry.module_id,
                    program_tree_entry.program.clone(),
                    entry_points.clone(),
                    monomorph_registry.clone(),
                    mapping_ctx_arena.clone(),
                    vtable_registry,
                    true,
                );

                analyzer.analyze();
                DiagReporter::display(&analyzer.reporter);

                analyzed_program_trees.push(analyzer.program_tree.clone());
            }

            AnalysisContext::check_entry_points(entry_points);
            drop(resolved_program_trees);

            let cloned_program_trees: Vec<Box<TypedProgramTree>> = analyzed_program_trees
                .into_iter()
                .map(|rc_refcell_tree| {
                    match Rc::try_unwrap(rc_refcell_tree) {
                        Ok(refcell_tree) => Box::new(refcell_tree.into_inner()),
                        Err(rc_still_shared) => {
                            // fallback borrow immutably and clone the inner tree
                            let cloned_tree = rc_still_shared.borrow().clone();
                            Box::new(cloned_tree)
                        }
                    }
                })
                .collect();

            let cir_monomorph_registry = Arc::new(Mutex::new(CIRMonomorphRegistry::new()));

            let cir_program_trees = walk_program_trees_in_parallel(
                None,
                cloned_program_trees,
                &resolver,
                cir_monomorph_registry,
                mapping_ctx_arena.clone(),
                &vtable_registries,
            );

            dbg!(cir_program_trees);
        }
        Err(errors) => {
            parser.display_parser_errors(errors.clone());
            exit(1);
        }
    }
}
