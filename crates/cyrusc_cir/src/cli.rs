use cyrusc_cir::{monomorph::CIRMonomorphRegistry, walk::walk_program_trees_in_parallel};
use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::{get_directory_of_file, read_file};
use cyrusc_lexer::Lexer;
use cyrusc_modulefsloader::ModuleLoaderOptions;
use cyrusc_parser::Parser;
use cyrusc_resolver::{Resolver, Visiting, generate_module_id};
use cyrusc_sema::analyze::AnalysisContext;
use cyrusc_tast::{TypedProgramTree, generics::monomorph::MonomorphRegistry};
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
            resolver.resolve_module(module_id, &program_tree, &mut Visiting::new(), true, file_path.clone());
            if resolver.reporter.has_errors() {
                DiagReporter::display(&resolver.reporter);
                exit(1);
            }

            // if resolver.reporter.has_errors() {
            //     resolver.reporter.display();
            //     exit(1);
            // }

            let entry_points = Arc::new(Mutex::new(Vec::new()));

            let mut has_error = false;
            let resolved_program_trees = resolver.program_trees.lock().unwrap();

            let mut analyzed_program_trees: Vec<Rc<RefCell<TypedProgramTree>>> = Vec::new();
            for program_tree_entry in &*resolved_program_trees {
                let mut analyzer = AnalysisContext::new(
                    &resolver,
                    program_tree_entry.module_id,
                    program_tree_entry.program.clone(),
                    entry_points.clone(),
                    monomorph_registry.clone(),
                    true,
                );

                analyzer.analyze();
                DiagReporter::display(&analyzer.reporter);
                if analyzer.reporter.has_errors() {
                    has_error = true;
                }

                analyzed_program_trees.push(analyzer.program_tree.clone());
            }

            AnalysisContext::check_entry_points(entry_points);

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

            let cir_program_trees =
                walk_program_trees_in_parallel(None, cloned_program_trees, &resolver, cir_monomorph_registry);

            dbg!(cir_program_trees);
        }
        Err(errors) => {
            parser.display_parser_errors(errors.clone());
            exit(1);
        }
    }
}
