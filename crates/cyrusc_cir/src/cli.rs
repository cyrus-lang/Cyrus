use cyrusc_cir::walk::walk_program_trees_in_parallel;
use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::{get_directory_of_file, read_file};
use cyrusc_lexer::Lexer;
use cyrusc_modulefsloader::ModuleLoaderOptions;
use cyrusc_parser::Parser;
use cyrusc_resolver::{Resolver, Visiting, generate_module_id};
use cyrusc_sema::{analyze::AnalysisContext, monomorph::MonomorphRegistry};
use cyrusc_tast::TypedProgramTree;
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
        Ok(program) => {
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

            let mut resolver = Resolver::new(module_loader_opts, file_path.clone());
            let module_id = generate_module_id();
            let typed_program_tree =
                match resolver.resolve_module(module_id, &program, &mut Visiting::new(), true, file_path) {
                    Some(program_tree) => program_tree,
                    None => panic!(),
                };
            if resolver.reporter.has_errors() {
                resolver.reporter.display();
                exit(1);
            }

            let mut program_trees: Vec<Rc<RefCell<TypedProgramTree>>> = Vec::new();

            {
                let entry_points = Arc::new(Mutex::new(Vec::new()));
                let monomorph_registry = Arc::new(Mutex::new(MonomorphRegistry::new()));
                let mut analyzer = AnalysisContext::new(
                    &resolver,
                    module_id,
                    typed_program_tree.clone(),
                    entry_points.clone(),
                    monomorph_registry,
                    true,
                );
                analyzer.analyze();
                DiagReporter::display(&analyzer.reporter);
                if analyzer.reporter.has_errors() {
                    return;
                }

                AnalysisContext::check_entry_points(entry_points);
                program_trees.push(analyzer.program_tree.clone());
            }

            let cloned_program_trees: Vec<Box<TypedProgramTree>> = program_trees
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

            let cir_program_trees = walk_program_trees_in_parallel(None, cloned_program_trees, &resolver);
            dbg!(cir_program_trees);
        }
        Err(errors) => {
            parser.display_parser_errors(errors.clone());
            exit(1);
        }
    }
}
