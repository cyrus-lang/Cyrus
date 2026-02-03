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
    options::{BuildDir, CodeGenOptions, CodeGenOptionsProjectType, LinkerOutputKind},
};
use cyrusc_buildmanifest::BuildManifest;
use cyrusc_cir::{CIRProgramTree, monomorph::CIRMonomorphRegistry, walk::walk_program_trees_in_parallel};
use cyrusc_diagcentral::{display_single_custom_diag, reporter::DiagReporter};
use cyrusc_fs_utils::{ensure_output_dir, file_name_without_extension, read_file};
use cyrusc_lexer::Lexer;
use cyrusc_modulefsloader::ModuleLoaderOptions;
use cyrusc_parser::Parser;
use cyrusc_resolver::{Resolver, Visiting, generate_module_id};
use cyrusc_scaffold_parser::{
    ASSEMBLY_DIR_PATH, BITCODE_DIR_PATH, LLVM_IR_DIR_PATH, OBJECT_CACHE_DIR_FILENAME, OBJECT_DIR_FILENAME,
    OUTPUT_DIR_FILENAME, SHARED_LIB_DIR_PATH, SRC_CACHE_DIR_PATH, STATIC_LIB_DIR_PATH,
};
use cyrusc_sema::analyze::AnalysisContext;
use cyrusc_tast::{
    TypedProgramTree,
    generics::{mapping_ctx_arena::GenericMappingCtxArenaImpl, monomorph::MonomorphRegistry},
};
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
    pub entry_file: PathBuf,
    pub build_dir: PathBuf,
    pub program_trees: Vec<Box<CIRProgramTree>>,
    pub monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
}

pub struct CodeGenSemanticBundle {
    pub analyzed_program_trees: Vec<Rc<RefCell<TypedProgramTree>>>,
    pub vtable_registries: Vec<Arc<Mutex<VTableRegistry>>>,
    pub mapping_ctx_arena: Arc<Mutex<GenericMappingCtxArenaImpl>>,
    pub resolver: Box<Resolver>,
    pub entry_file: PathBuf,
    pub build_dir: PathBuf,
}

pub fn create_compiler_context(
    opts: CodeGenOptions,
    file_path: &Option<PathBuf>,
    linker_output_kind: LinkerOutputKind,
) -> CodeGenContext {
    let base_path = opts.base_path.clone().map(|path| Path::new(&path).to_path_buf());

    let entry_module_file_path = get_entry_module_file_path(&opts, &base_path, &file_path);
    let build_dir = get_final_build_directory_path(&opts.build_dir);

    let build_manifest = Arc::new(Mutex::new(BuildManifest::load_manifest_or_make_new(
        &base_path.unwrap_or_default(),
        &build_dir,
    )));

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

    let base_path = opts.base_path.clone().map(|path| Path::new(&path).to_path_buf());
    let file_path = file_path.map(|path| Path::new(&path).to_path_buf());

    // resolve entry module file path & build directory path
    let entry_file = get_entry_module_file_path(opts, &base_path, &file_path);
    let build_dir = get_final_build_directory_path(&opts.build_dir);
    ensure_build_dir_subs_exist(&base_path, build_dir.clone());

    // lex & parse
    let file_content = read_file(entry_file.clone()).0;
    let mut lexer = Lexer::new(file_content, entry_file.to_string_lossy().to_string());
    let mut parser = Parser::new(lexer.tokenize(), entry_file.to_string_lossy().to_string());
    let program_tree = parser.parse().unwrap_or_else(|errors| {
        parser.display_parser_errors(errors);
        exit(1);
    });

    let module_loader_opts = ModuleLoaderOptions {
        base_path: opts.base_path.clone().unwrap(),
        stdlib_path: opts.stdlib_path.clone(),
        source_dirs: opts.source_dirs.clone(),
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

pub fn get_llvm_dir_output_path(build_dir: &PathBuf, output_path_opt: &Option<String>) -> PathBuf {
    if let Some(output_path) = output_path_opt {
        ensure_output_dir(output_path);
        return Path::new(&output_path).to_path_buf();
    }

    let dir_path = build_dir.join(OUTPUT_DIR_FILENAME).join(LLVM_IR_DIR_PATH);
    ensure_output_dir(&dir_path);
    return dir_path;
}

pub fn get_bitcode_dir_output_path(build_dir: &PathBuf, output_path_opt: &Option<String>) -> PathBuf {
    if let Some(output_path) = output_path_opt {
        ensure_output_dir(output_path);
        return Path::new(&output_path).to_path_buf();
    }

    let dir_path = build_dir.join(OUTPUT_DIR_FILENAME).join(BITCODE_DIR_PATH);
    ensure_output_dir(&dir_path);
    return dir_path;
}

pub fn get_assembly_dir_output_path(build_dir: &PathBuf, output_path_opt: &Option<String>) -> PathBuf {
    if let Some(output_path) = output_path_opt {
        ensure_output_dir(output_path);
        return Path::new(&output_path).to_path_buf();
    }

    let dir_path = build_dir.join(OUTPUT_DIR_FILENAME).join(ASSEMBLY_DIR_PATH);
    ensure_output_dir(&dir_path);
    return dir_path;
}

pub fn get_object_dir_output_path(build_dir: &PathBuf, output_path_opt: &Option<String>) -> PathBuf {
    if let Some(output_path) = output_path_opt {
        ensure_output_dir(output_path);
        return Path::new(&output_path).to_path_buf();
    }

    let dir_path = build_dir.join(OUTPUT_DIR_FILENAME).join(OBJECT_DIR_FILENAME);
    ensure_output_dir(&dir_path);
    return dir_path;
}

pub fn get_shared_lib_dir_output_path(build_dir: &PathBuf, output_path_opt: &Option<String>) -> PathBuf {
    if let Some(output_path) = output_path_opt {
        ensure_output_dir(output_path);
        return Path::new(&output_path).to_path_buf();
    }

    let dir_path = build_dir.join(OUTPUT_DIR_FILENAME).join(SHARED_LIB_DIR_PATH);
    ensure_output_dir(&dir_path);
    return dir_path;
}

pub fn get_static_lib_dir_output_path(build_dir: &PathBuf, output_path_opt: &Option<String>) -> PathBuf {
    if let Some(output_path) = output_path_opt {
        ensure_output_dir(output_path);
        return Path::new(&output_path).to_path_buf();
    }

    let dir_path = build_dir.join(OUTPUT_DIR_FILENAME).join(STATIC_LIB_DIR_PATH);
    ensure_output_dir(&dir_path);
    return dir_path;
}

pub fn get_executable_output_path(
    opts: &CodeGenOptions,
    build_dir: &PathBuf,
    entry_file_path: &PathBuf,
    output_path_opt: Option<PathBuf>,
) -> PathBuf {
    if let Some(output_path) = output_path_opt {
        return output_path;
    }

    let dir_path = Path::new(build_dir).join(OUTPUT_DIR_FILENAME);
    ensure_output_dir(&dir_path);

    let file_path = dir_path.join({
        opts.project_name
            .clone()
            .unwrap_or(file_name_without_extension(entry_file_path.to_path_buf()).unwrap_or("unknown".to_string()))
    });

    return file_path;
}

pub fn get_final_build_directory_path(build_dir: &BuildDir) -> PathBuf {
    fn temp_build_dir() -> PathBuf {
        let temp_dir = env::temp_dir();
        ensure_output_dir(&temp_dir);
        temp_dir
    }

    match build_dir {
        BuildDir::Provided(dir_path_str) => PathBuf::from(dir_path_str),
        BuildDir::Default => temp_build_dir(),
    }
}

fn get_entry_module_file_path(
    opts: &CodeGenOptions,
    base_path: &Option<PathBuf>,
    input_file_path: &Option<PathBuf>,
) -> PathBuf {
    // if an explicit input file is provided, use it
    if let Some(input_path) = input_file_path {
        return input_path.clone();
    }

    let base_path = base_path.clone().unwrap_or_default();
    let project_type = opts.project_type.clone().unwrap_or_default();

    let current_dir = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let project_root = current_dir.join(&base_path).canonicalize().unwrap();

    // use provided source dirs or default to "src"
    let source_dirs: Vec<PathBuf> = if opts.source_dirs.is_empty() {
        vec![PathBuf::from("src")]
    } else {
        opts.source_dirs.iter().map(|s| PathBuf::from(s)).collect()
    };

    // files we're looking for
    let library_file = "lib.cyrus";
    let executable_file = "main.cyrus";

    // determine which file we expect based on project type
    let expected_file = match project_type {
        CodeGenOptionsProjectType::Library => library_file,
        CodeGenOptionsProjectType::Executable => executable_file,
    };

    let unexpected_file = match project_type {
        CodeGenOptionsProjectType::Library => executable_file,
        CodeGenOptionsProjectType::Executable => library_file,
    };

    let project_type_name = match project_type {
        CodeGenOptionsProjectType::Library => "library",
        CodeGenOptionsProjectType::Executable => "executable",
    };

    // search for files in all source directories
    let mut found_expected = None;
    let mut found_unexpected = None;
    let mut searched_paths = Vec::new();

    for source_dir in &source_dirs {
        // build full path: project_root + source_dir + filename
        let expected_path = project_root.join(source_dir).join(expected_file);
        let unexpected_path = project_root.join(source_dir).join(unexpected_file);

        searched_paths.push(expected_path.clone());
        searched_paths.push(unexpected_path.clone());

        if expected_path.exists() {
            found_expected = Some(expected_path);
        }

        if unexpected_path.exists() {
            found_unexpected = Some(unexpected_path);
        }
    }

    // check if we found an unexpected file but not the expected one
    if found_unexpected.is_some() {
        if found_expected.is_none() {
            display_single_custom_diag!(format!(
                "Project type mismatch: Found '{}' but expected '{}' for a {} project.\n\
                 \n\
                 Solutions:\n\
                 1. Change project type in 'Project.toml'\n\
                 2. Rename the file to '{}'\n\
                 3. Specify an input file with --input",
                unexpected_file, expected_file, project_type_name, expected_file
            ));
        }
    }

    // if we found the expected file, return it
    if let Some(expected_path) = found_expected {
        return expected_path;
    }

    // helper function to display paths in a user-friendly way
    fn format_path_for_display(path: &Path, current_dir: &Path) -> String {
        // try to show as relative path if it's under current directory
        if let Ok(relative) = path.strip_prefix(current_dir) {
            if relative.as_os_str().is_empty() {
                // If it's exactly the current directory
                ".".to_string()
            } else {
                format!("./{}", relative.display())
            }
        } else {
            path.display().to_string()
        }
    }

    // format source directories for display (just the directory names)
    let source_dirs_list = source_dirs
        .iter()
        .map(|d| format!("  - {}/{}", d.display(), expected_file))
        .collect::<Vec<_>>()
        .join("\n");

    // format searched paths for display (clean relative paths)
    let searched_paths_list = searched_paths
        .iter()
        .map(|p| format!("  - {}", format_path_for_display(p, &current_dir)))
        .collect::<Vec<_>>()
        .join("\n");

    // format fallback path for display
    let fallback_path = project_root.join(&source_dirs[0]).join(expected_file);
    let fallback_display = format_path_for_display(&fallback_path, &current_dir);

    // format project root for display
    let project_root_display = format_path_for_display(&project_root, &current_dir);

    // neither expected nor unexpected file found
    display_single_custom_diag!(format!(
        "No '{}' entry point found for {} project.\n\
         \n\
         Expected '{}' in one of these directories:\n{}\n\
         \n\
         Searched paths:\n{}\n\
         \n\
         Project root: {}\n\
         \n\
         Solutions:\n\
         1. Create the file at: {}\n\
         2. Check your source directories with --source-dirs\n\
         3. Verify your base path with --base-path",
        expected_file,
        project_type_name,
        expected_file,
        source_dirs_list,
        searched_paths_list,
        project_root_display,
        fallback_display,
    ));
}

fn ensure_build_dir_subs_exist(base_path: &Option<PathBuf>, build_dir_path: PathBuf) {
    let base = base_path.clone().unwrap_or_default();
    let dirs = [SRC_CACHE_DIR_PATH, OBJECT_CACHE_DIR_FILENAME, OUTPUT_DIR_FILENAME];

    let base_build_dir = Path::new(&base).join(build_dir_path);

    for dir in dirs {
        let build_dir = base_build_dir.join(dir);
        ensure_output_dir(&build_dir);
    }
}
