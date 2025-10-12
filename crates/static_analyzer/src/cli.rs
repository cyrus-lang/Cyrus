use diagcentral::reporter::DiagReporter;
use lexer::Lexer;
use parser::Parser;
use resolver::{Resolver, Visiting, generate_module_id, modulefsloader::ModuleLoaderOptions};
use static_analyzer::{context::AnalysisContext, monomorph::MonomorphRegistry};
use std::{
    env,
    process::exit,
    sync::{Arc, Mutex},
    vec,
};
use utils::fs::read_file;

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();
    let file_content = read_file(file_path.clone()).0;
    let mut lexer = Lexer::new(file_content, file_path.clone());
    let mut parser = Parser::new(lexer.tokenize(), file_path.clone());

    match parser.parse() {
        Ok(node) => {
            let mut current_dir = env::current_dir().unwrap();
            current_dir.push("./stdlib");
            let stdlib_path = current_dir.canonicalize().unwrap().to_str().unwrap().to_string();

            let input_file_dir = utils::fs::get_directory_of_file(file_path.clone()).unwrap();
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
                match resolver.resolve_module(module_id, node.as_program(), &mut Visiting::new(), true, file_path) {
                    Some(program_tree) => program_tree,
                    None => panic!(),
                };
            if resolver.reporter.has_errors() {
                resolver.reporter.display();
                exit(1);
            }

            {
                let entry_points = Arc::new(Mutex::new(Vec::new()));
                let monomorph_registry = Arc::new(Mutex::new(MonomorphRegistry::new()));
                let mut analyzer = AnalysisContext::new(
                    &resolver,
                    module_id,
                    typed_program_tree.clone(),
                    entry_points.clone(),
                    monomorph_registry,
                    false,
                );
                analyzer.analyze();
                DiagReporter::display(&analyzer.reporter);
                if analyzer.reporter.has_errors() {
                    return;
                }

                AnalysisContext::check_entry_points(entry_points);
            }

            dbg!(typed_program_tree);
        }
        Err(errors) => {
            parser.display_parser_errors(errors.clone());
            exit(1);
        }
    }
}
