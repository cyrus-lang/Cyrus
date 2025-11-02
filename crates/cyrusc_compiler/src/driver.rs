use crate::{
    context::CodeGenContext,
    linker::Linker,
    options::{BuildDir, CodeGenOptions, LinkerOutputKind},
};
use cyrusc_ast::ProgramTree;
use cyrusc_buildmanifest::BuildManifest;
use cyrusc_cir::{CIRProgramTree, walk::walk_program_trees_in_parallel};
use cyrusc_diagcentral::{display_single_custom_diag, reporter::DiagReporter};
use cyrusc_fs_utils::{ensure_output_dir, get_directory_of_file, read_file};
use cyrusc_lexer::Lexer;
use cyrusc_modulefsloader::ModuleLoaderOptions;
use cyrusc_parser::Parser;
use cyrusc_resolver::{Resolver, Visiting, generate_module_id};
use cyrusc_scaffold_parser::{
    OBJECTS_FILENAME, OUTPUT_FILENAME, PROJECT_FILE_PATH, SOURCES_DIR_PATH, parse_project_toml,
};
use cyrusc_sema::{analyze::AnalysisContext, monomorph::MonomorphRegistry};
use cyrusc_tast::TypedProgramTree;
use cyrusc_tui_utils::tui_error;
use std::{
    cell::RefCell,
    env,
    path::{Path, PathBuf},
    process::exit,
    rc::Rc,
    sync::{Arc, Mutex},
};

pub struct CodeGenContextBundle {
    pub options: CodeGenOptions,
    pub entry_file: String,
    pub build_dir: String,
    pub program_trees: Vec<Box<CIRProgramTree>>,
    pub monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
}

pub fn create_compiler_context(
    opts: CodeGenOptions,
    file_path: Option<String>,
    linker_output_kind: LinkerOutputKind,
) -> CodeGenContext {
    let entry_module_file_path = get_entry_source_code_path(opts.base_path.clone(), file_path);
    let build_dir = get_final_build_directory_path(opts.base_path.clone(), opts.build_dir.clone());
    let build_dir_path = Path::new(&build_dir);
    let base_path = opts
        .base_path
        .clone()
        .and_then(|path| Some(Path::new(&path).to_path_buf()));

    let build_manifest = Arc::new(Mutex::new(BuildManifest::new(base_path, build_dir_path.to_path_buf())));

    let linker = match Linker::new(opts.clone()) {
        Ok(linker) => linker,
        Err(err) => {
            display_single_custom_diag!(err);
        }
    };

    CodeGenContext::new(opts, build_manifest, entry_module_file_path, linker_output_kind, linker)
}

pub fn build_compilation_bundle(opts: &mut CodeGenOptions, file_path: Option<String>) -> CodeGenContextBundle {
    // disable modulefs cache if a specific file is being compiled
    if file_path.is_some() {
        opts.disable_modulefs_cache = true;
    }

    // resolve paths
    let entry_file = get_entry_source_code_path(opts.base_path.clone(), file_path);
    let build_dir = get_final_build_directory_path(opts.base_path.clone(), opts.build_dir.clone());
    ensure_build_dir_subs(opts.base_path.clone(), build_dir.clone());

    // read and lex the source
    let file_content = read_file(entry_file.clone()).0;
    let mut lexer = Lexer::new(file_content, entry_file.clone());
    let mut parser = Parser::new(lexer.tokenize(), entry_file.clone());

    // parse program
    let program_tree = parser.parse().unwrap_or_else(|errors| {
        parser.display_parser_errors(errors);
        exit(1);
    });

    // setup module loader and resolver
    let mut resolver = setup_resolver(entry_file.clone());

    // resolve the typed program tree
    let typed_program_tree = resolve_typed_program_tree(&mut resolver, &program_tree, entry_file.clone());

    // create monomorph registry
    let monomorph_registry = Arc::new(Mutex::new(MonomorphRegistry::new()));

    // run analysis
    let typed_program_trees = analyze_program_tree(&resolver, monomorph_registry.clone(), typed_program_tree);

    // convert Rc<RefCell> trees into boxed trees
    let boxed_trees = box_program_trees(typed_program_trees);

    // walk program trees in parallel
    let cir_program_trees = walk_program_trees_in_parallel(opts.jobs, boxed_trees, &resolver);

    CodeGenContextBundle {
        options: opts.clone(),
        entry_file,
        build_dir,
        program_trees: cir_program_trees,
        monomorph_registry,
    }
}

fn setup_resolver(entry_file: String) -> Resolver {
    let stdlib_path = env::current_dir()
        .unwrap()
        .join("./stdlib")
        .canonicalize()
        .unwrap()
        .to_string_lossy()
        .to_string();

    let input_file_dir = get_directory_of_file(entry_file.clone()).unwrap();
    let module_loader_opts = ModuleLoaderOptions {
        stdlib_path: Some(stdlib_path),
        source_dirs: vec![input_file_dir],
    };

    Resolver::new(module_loader_opts, entry_file.to_string())
}

fn resolve_typed_program_tree(
    resolver: &mut Resolver,
    program: &ProgramTree,
    entry_file: String,
) -> Rc<RefCell<TypedProgramTree>> {
    let module_id = generate_module_id();
    let program_tree = resolver
        .resolve_module(module_id, program, &mut Visiting::new(), true, entry_file)
        .unwrap_or_else(|| panic!("Failed to resolve module."));

    if resolver.reporter.has_errors() {
        resolver.reporter.display();
        exit(1);
    }

    program_tree
}

fn analyze_program_tree(
    resolver: &Resolver,
    monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
    typed_program_tree: Rc<RefCell<TypedProgramTree>>,
) -> Vec<Rc<RefCell<TypedProgramTree>>> {
    let entry_points = Arc::new(Mutex::new(Vec::new()));

    let mut analyzer = AnalysisContext::new(
        resolver,
        generate_module_id(),
        typed_program_tree.clone(),
        entry_points.clone(),
        monomorph_registry,
        true,
    );

    analyzer.analyze();
    DiagReporter::display(&analyzer.reporter);
    if analyzer.reporter.has_errors() {
        exit(1);
    }

    AnalysisContext::check_entry_points(entry_points);
    vec![analyzer.program_tree.clone()]
}

fn box_program_trees(program_trees: Vec<Rc<RefCell<TypedProgramTree>>>) -> Vec<Box<TypedProgramTree>> {
    program_trees
        .into_iter()
        .map(|rc_refcell_tree| match Rc::try_unwrap(rc_refcell_tree) {
            Ok(refcell_tree) => Box::new(refcell_tree.into_inner()),
            Err(rc_still_shared) => Box::new(rc_still_shared.borrow().clone()),
        })
        .collect()
}

fn get_final_build_directory_path(base_path: Option<String>, build_dir: BuildDir) -> String {
    let base = base_path.unwrap_or_default();

    match build_dir {
        BuildDir::Provided(path) => path,
        BuildDir::Default => {
            // resolve project file path
            let project_file = env::current_dir()
                .unwrap_or_else(|_| PathBuf::from("."))
                .join(&base)
                .join(PROJECT_FILE_PATH);

            parse_project_toml(&project_file)
                .map(|cfg| {
                    cfg.compiler
                        .and_then(|c| c.build_dir)
                        .filter(|p| !p.is_empty())
                        .map(|p| {
                            let final_dir = Path::new(&base).join(&p);
                            ensure_output_dir(final_dir.to_string_lossy().to_string());
                            final_dir.to_string_lossy().to_string()
                        })
                        .unwrap_or_else(fallback_temp_dir)
                })
                .unwrap_or_else(|_| fallback_temp_dir())
        }
    }
}

/// Returns a fallback temporary directory path as a String
fn fallback_temp_dir() -> String {
    let temp_dir = env::temp_dir();
    ensure_output_dir(temp_dir.to_string_lossy().to_string());
    temp_dir.to_string_lossy().to_string()
}

fn get_entry_source_code_path(base_path: Option<String>, input_file_path: Option<String>) -> String {
    input_file_path.unwrap_or_else(|| {
        let base = base_path.unwrap_or_default();
        let main_file = env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join(&base)
            .join("src/main.cyrus");

        if !main_file.exists() {
            display_single_custom_diag!("Project.toml not found in the current directory.".to_string());
        }

        main_file.to_string_lossy().to_string()
    })
}

fn ensure_build_dir_subs(base_path: Option<String>, build_dir_path: String) {
    let base = base_path.unwrap_or_default();
    let dirs = [SOURCES_DIR_PATH, OBJECTS_FILENAME, OUTPUT_FILENAME];

    let base_build_dir = Path::new(&base).join(build_dir_path);

    for dir in dirs {
        let build_dir = base_build_dir.join(dir);
        ensure_output_dir(build_dir.to_str().unwrap().to_string());
    }
}

pub fn get_artifact_output_path(build_dir: BuildDir, output_path: Option<String>) -> String {
    if let Some(output) = output_path {
        return output;
    }

    match build_dir {
        BuildDir::Provided(ref path) => {
            let path_buf = Path::new(path);
            if !path_buf.exists() {
                std::fs::create_dir_all(path_buf).unwrap_or_else(|err| {
                    tui_error(format!("Failed to create build dir {}: {}", path, err));
                });
            }
            path.clone()
        }
        BuildDir::Default => {
            // if default build dir is not present, fallback to temp
            let temp_dir = env::temp_dir();
            std::fs::create_dir_all(&temp_dir).unwrap_or_else(|err| {
                tui_error(format!("Failed to create temp dir {}: {}", temp_dir.display(), err));
            });
            temp_dir.to_string_lossy().to_string()
        }
    }
}
