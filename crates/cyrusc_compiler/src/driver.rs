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
use crate::{
    context::CodeGenContext,
    linker::Linker,
    options::{BuildDir, CodeGenOptions, LinkerOutputKind},
};
use cyrusc_buildmanifest::BuildManifest;
use cyrusc_cir::{CIRProgramTree, monomorph::CIRMonomorphRegistry, walk::walk_program_trees_in_parallel};
use cyrusc_diagcentral::{display_single_custom_diag, reporter::DiagReporter};
use cyrusc_fs_utils::{ensure_output_dir, file_name_without_extension, get_directory_of_file, read_file};
use cyrusc_lexer::Lexer;
use cyrusc_modulefsloader::ModuleLoaderOptions;
use cyrusc_parser::Parser;
use cyrusc_resolver::{Resolver, Visiting, generate_module_id};
use cyrusc_scaffold_parser::{
    OBJ_DIR_FILENAME, OUTPUT_DIR_FILENAME, PROJECT_FILE_PATH, SOURCES_DIR_PATH, parse_project_toml,
};
use cyrusc_sema::analyze::AnalysisContext;
use cyrusc_tast::{
    TypedProgramTree,
    generics::{mapping_ctx_arena::GenericMappingCtxArenaImpl, monomorph::MonomorphRegistry},
};
use cyrusc_tui_utils::tui_error;
use cyrusc_vtable_registry::VTableRegistry;
use std::{
    cell::RefCell,
    env,
    path::{Path, PathBuf},
    process::exit,
    rc::Rc,
    sync::{Arc, Mutex},
};

pub struct CodeGenContextBundle {
    pub opts: CodeGenOptions,
    pub entry_file: String,
    pub build_dir: String,
    pub program_trees: Vec<Box<CIRProgramTree>>,
    pub monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
}

pub struct CodeGenSemanticBundle {
    pub analyzed_program_trees: Vec<Rc<RefCell<TypedProgramTree>>>,
    pub vtable_registries: Vec<Arc<Mutex<VTableRegistry>>>,
    pub mapping_ctx_arena: Arc<Mutex<GenericMappingCtxArenaImpl>>,
    pub resolver: Box<Resolver>,
    pub entry_file: String,
    pub build_dir: String,
}

pub fn create_compiler_context(
    opts: CodeGenOptions,
    file_path: Option<String>,
    linker_output_kind: LinkerOutputKind,
) -> CodeGenContext {
    let entry_module_file_path = get_entry_module_file_path(opts.base_path.clone(), file_path);
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

pub fn build_semantic_bundle(opts: &mut CodeGenOptions, file_path: Option<String>) -> Box<CodeGenSemanticBundle> {
    // disable modulefs cache if compiling a single file
    if file_path.is_some() {
        opts.disable_modulefs_cache = true;
    }

    // resolve entry module file path & build directory path
    let entry_file = get_entry_module_file_path(opts.base_path.clone(), file_path);
    let build_dir = get_final_build_directory_path(opts.base_path.clone(), opts.build_dir.clone());
    ensure_build_dir_subs_exist(opts.base_path.clone(), build_dir.clone());

    // lex & parse
    let file_content = read_file(entry_file.clone()).0;
    let mut lexer = Lexer::new(file_content, entry_file.clone());
    let mut parser = Parser::new(lexer.tokenize(), entry_file.clone());
    let program_tree = parser.parse().unwrap_or_else(|errors| {
        parser.display_parser_errors(errors);
        exit(1);
    });

    // discover stdlib and input source directory
    let input_file_dir = get_directory_of_file(entry_file.clone()).unwrap();

    let module_loader_opts = ModuleLoaderOptions {
        base_path: opts.base_path.clone().unwrap(),
        stdlib_path: opts.stdlib_path.clone(),
        source_dirs: vec![input_file_dir],
    };

    let monomorph_registry = Arc::new(Mutex::new(MonomorphRegistry::new()));
    let mapping_ctx_arena = Arc::new(Mutex::new(GenericMappingCtxArenaImpl::new()));

    let mut resolver = Resolver::new(
        module_loader_opts,
        monomorph_registry.clone(),
        mapping_ctx_arena.clone(),
        entry_file.clone(),
    );

    // resolve the entry module
    let module_id = generate_module_id();
    resolver.resolve_module(module_id, &program_tree, &mut Visiting::new(), true, entry_file.clone());
    if resolver.reporter.has_errors() {
        DiagReporter::display(&resolver.reporter);
        exit(1);
    }

    // analyze modules

    let entry_points = Arc::new(Mutex::new(Vec::new()));
    let resolved_program_trees = resolver.program_trees.lock().unwrap();

    let mut has_error = false;
    let mut analyzed_program_trees: Vec<Rc<RefCell<TypedProgramTree>>> = Vec::new();
    let mut vtable_registries: Vec<Arc<Mutex<VTableRegistry>>> = Vec::new();

    for program_tree_entry in &*resolved_program_trees {
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
        if analyzer.reporter.has_errors() {
            has_error = true;
        }

        analyzed_program_trees.push(analyzer.program_tree.clone());
    }

    AnalysisContext::check_entry_points(entry_points);
    drop(resolved_program_trees);

    if has_error {
        exit(1);
    }

    Box::new(CodeGenSemanticBundle {
        resolver: Box::new(resolver),
        analyzed_program_trees,
        vtable_registries,
        mapping_ctx_arena,
        entry_file,
        build_dir,
    })
}

pub fn build_compilation_bundle(opts: &mut CodeGenOptions, file_path: Option<String>) -> CodeGenContextBundle {
    let codegen_semantic_bundle = build_semantic_bundle(opts, file_path);

    // prepare trees for codegen

    let boxed_program_trees: Vec<Box<TypedProgramTree>> = codegen_semantic_bundle
        .analyzed_program_trees
        .into_iter()
        .map(|rc_refcell_tree| match Rc::try_unwrap(rc_refcell_tree) {
            Ok(refcell_tree) => Box::new(refcell_tree.into_inner()),
            Err(rc_still_shared) => Box::new(rc_still_shared.borrow().clone()),
        })
        .collect();

    let cir_monomorph_registry = Arc::new(Mutex::new(CIRMonomorphRegistry::new()));

    let cir_program_trees = walk_program_trees_in_parallel(
        opts.jobs,
        boxed_program_trees,
        &codegen_semantic_bundle.resolver,
        cir_monomorph_registry.clone(),
        codegen_semantic_bundle.mapping_ctx_arena.clone(),
        &codegen_semantic_bundle.vtable_registries,
    );

    CodeGenContextBundle {
        opts: opts.clone(),
        program_trees: cir_program_trees,
        monomorph_registry: cir_monomorph_registry,
        entry_file: codegen_semantic_bundle.entry_file,
        build_dir: codegen_semantic_bundle.build_dir,
    }
}

pub fn get_executable_output_path(
    opts: &CodeGenOptions,
    build_dir: &String,
    entry_file_path: &String,
    output_path_opt: Option<String>,
) -> String {
    if let Some(output_path) = output_path_opt {
        return output_path;
    }

    // use `build_dir/output` directory to store executable artifact.
    let dir_path = Path::new(build_dir).join(OUTPUT_DIR_FILENAME);
    let file_path = dir_path.join({
        opts.project_name
            .clone()
            .unwrap_or(file_name_without_extension(entry_file_path).unwrap_or("unknown".to_string()))
    });

    return file_path.to_str().unwrap().to_string();
}

fn get_final_build_directory_path(base_path: Option<String>, build_dir: BuildDir) -> String {
    fn temp_build_dir() -> String {
        let temp_dir = env::temp_dir();
        ensure_output_dir(temp_dir.to_string_lossy().to_string());
        temp_dir.to_string_lossy().to_string()
    }

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
                        .unwrap_or_else(temp_build_dir)
                })
                .unwrap_or_else(|_| temp_build_dir())
        }
    }
}

// TODO: Support for library-project-type and executable-project-type.
// Lookup for `lib.cyrus` and `main.cyrus` subsequently.
fn get_entry_module_file_path(base_path: Option<String>, input_file_path: Option<String>) -> String {
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

fn ensure_build_dir_subs_exist(base_path: Option<String>, build_dir_path: String) {
    let base = base_path.unwrap_or_default();
    let dirs = [SOURCES_DIR_PATH, OBJ_DIR_FILENAME, OUTPUT_DIR_FILENAME];

    let base_build_dir = Path::new(&base).join(build_dir_path);

    for dir in dirs {
        let build_dir = base_build_dir.join(dir);
        ensure_output_dir(build_dir.to_str().unwrap().to_string());
    }
}
