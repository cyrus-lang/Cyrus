// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::CodeGenContext, linker::Linker};
use cyrusc_analyzer::context::{AnalysisContext, AnalyzerConfig, EntryPoints};
use cyrusc_buildmanifest::BuildManifest;
use cyrusc_cir_lower::lower_program_trees_in_parallel;
use cyrusc_diagcentral::{exit_with_msg, reporter::DiagReporter};
use cyrusc_fs_utils::{ensure_output_dir, file_name_without_extension, get_directory_of_file};
use cyrusc_internal::{
    abi::target::{ABITarget, ABITargetArch, ABITargetInfo, ABITargetOS, ABITargetObjectFormat, create_target_abi},
    cir::cir::CIRModule,
    compiler_options::{
        CompilerOption_BuildDir, CompilerOption_LinkerOutputKind, CompilerOption_ProjectType, CompilerOptions,
    },
    monomorph::MonomorphRegistry,
    vtable::VTableRegistry,
};
use cyrusc_parser::SourceParser;
use cyrusc_resolver::{
    Resolver,
    fs_module_loader::{FsModuleLoader, FsModuleLoaderOptions},
    modules::VisitingModule,
};
use cyrusc_scaffold_parser::{
    ASSEMBLY_DIR_PATH, BITCODE_DIR_PATH, CIR_DUMP_DIR_PATH, LLVM_IR_DIR_PATH, OBJECT_CACHE_DIR_FILENAME,
    OBJECT_DIR_FILENAME, OUTPUT_DIR_FILENAME, SHARED_LIB_DIR_PATH, SRC_CACHE_DIR_PATH, STATIC_LIB_DIR_PATH,
};
use cyrusc_source_loc::{FileID, SourceMap};
use cyrusc_tui_utils::tui_error;
use cyrusc_typed_ast::{TypedProgramTree, decls::table::DeclTablesRegistry};
use fx_hash::FxHashMap;
use inkwell::targets::{InitializationConfig, Target as InkwellTarget, TargetTriple};
use std::{
    cell::RefCell,
    env,
    path::{Path, PathBuf},
    process::exit,
    rc::Rc,
    sync::{Arc, Mutex},
};

pub struct CodeGenContextBundle {
    pub opts: CompilerOptions,
    pub entry_file: PathBuf,
    pub build_dir: PathBuf,
    pub program_trees: Vec<Box<CIRModule>>,
    pub target: Arc<ABITarget>,
    pub llvm_target: InkwellTarget,
    pub llvm_target_triple: TargetTriple,
    pub source_map: Arc<SourceMap>,
}

pub struct CodeGenSemanticBundle<'a> {
    pub analyzed_program_trees: Vec<Rc<RefCell<TypedProgramTree>>>,
    pub vtable_registries: FxHashMap<FileID, Arc<VTableRegistry>>,
    pub monomorph_registry: Arc<MonomorphRegistry>,
    pub resolver: Box<Resolver<'a>>,
    pub decl_tables: Arc<DeclTablesRegistry>,
    pub source_map: Arc<SourceMap>,
    pub entry_file: PathBuf,
    pub build_dir: PathBuf,
    pub target: Arc<ABITarget>,
}

fn create_compiler_context_target(target_info: &ABITargetInfo) -> (InkwellTarget, TargetTriple) {
    InkwellTarget::initialize_all(&InitializationConfig::default());

    let target_triple = TargetTriple::create(&target_info.triple());
    match InkwellTarget::from_triple(&target_triple) {
        Ok(target) => (target, target_triple),
        Err(llvm_string) => {
            tui_error(format!("LLVM Target Triple Error: {}", llvm_string.to_string()));
            exit(1);
        }
    }
}

// REVIEW: Consider to refactor this to get reference to CodeGenContextBundle
// it's much cleaner than getting it's fields one by one!
pub fn create_compiler_context(
    opts: CompilerOptions,
    file_path: &Option<PathBuf>,
    linker_output_kind: CompilerOption_LinkerOutputKind,
    target: Arc<ABITarget>,
    llvm_target: InkwellTarget,
    llvm_target_triple: TargetTriple,
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
            exit_with_msg!(err);
        }
    };

    CodeGenContext::new(
        opts,
        target,
        llvm_target,
        llvm_target_triple,
        build_manifest,
        entry_module_file_path,
        linker_output_kind,
        linker,
    )
}

pub fn build_semantic_bundle<'a>(
    opts: &'a CompilerOptions,
    file_path_opt: Option<String>,
) -> Box<CodeGenSemanticBundle<'a>> {
    let base_path = opts.base_path.clone().map(|path| Path::new(&path).to_path_buf());
    let file_path = file_path_opt.map(|path| Path::new(&path).to_path_buf());

    // resolve entry module file path & build directory path

    let entry_file = get_entry_module_file_path(opts, &base_path, &file_path);
    let build_dir = get_final_build_directory_path(&opts.build_dir);
    ensure_build_dir_subs_exist(&base_path, build_dir.clone());

    // create source map

    let source_map = Arc::new(SourceMap::new());
    let file_id = source_map.add_file_by_loading(entry_file.clone());
    let entry_source_file = { source_map.get_file(file_id).unwrap().clone() };

    let reporter = Arc::new(DiagReporter::new(source_map.clone()));
    let source_parser = Arc::new(SourceParser::new(reporter.clone()));

    match source_parser.parse_program(&entry_source_file) {
        Ok(program_tree) => {
            let module_loader_opts = FsModuleLoaderOptions {
                base_path: opts.base_path.clone().unwrap_or_default(),
                stdlib_path: opts.stdlib_path.clone(),
                source_dirs: opts.source_dirs.clone(),
            };

            let fs_module_loader = FsModuleLoader::new(source_map.clone(), source_parser, module_loader_opts);

            let decl_tables = Arc::new(DeclTablesRegistry::new());
            let monomorph_registry = Arc::new(MonomorphRegistry::new());

            let mut resolver = Resolver::new(
                opts,
                Box::new(fs_module_loader),
                source_map.clone(),
                reporter.clone(),
                decl_tables.clone(),
            );

            let module_symbol_id = resolver.create_entry_module_symbol_id(Path::new(&entry_file), file_id);

            let entry_module = resolver.resolve_module(
                module_symbol_id,
                &program_tree,
                &mut VisitingModule::new(),
                file_id,
                true,
            );

            // If entry module is None, it means something went wrong in resolver layer,
            // And we don't run analyzer and exit immediately.
            if entry_module.is_none() {
                DiagReporter::display(&resolver.reporter);
                exit(1);
            }

            if resolver.reporter.has_errors() {
                DiagReporter::display(&resolver.reporter);
                exit(1);
            }

            // target

            let target_info = resolve_target_info_from_opts(&opts);
            let target_abi = match create_target_abi(target_info.clone()) {
                Ok(target_abi) => target_abi,
                Err(err) => {
                    tui_error(err);
                    exit(1)
                }
            };
            let target = Arc::new(ABITarget::new(target_info, target_abi));

            // analyze modules

            let config = AnalyzerConfig::default();

            let entry_points = Arc::new(EntryPoints::new(reporter.clone(), source_map.clone()));
            let resolved_program_trees = resolver.program_trees.lock().unwrap();

            let mut has_error = false;
            let mut analyzed_program_trees: Vec<Rc<RefCell<TypedProgramTree>>> = Vec::new();
            let mut vtable_registries: FxHashMap<FileID, Arc<VTableRegistry>> = FxHashMap::default();

            for program_tree_entry in &*resolved_program_trees {
                let vtable_registry = Arc::new(VTableRegistry::new());

                vtable_registries.insert(program_tree_entry.file_id, vtable_registry.clone());

                let mut analyzer = AnalysisContext::new(
                    config.clone(),
                    reporter.clone(),
                    source_map.clone(),
                    &target,
                    decl_tables.clone(),
                    &resolver,
                    &resolver,
                    program_tree_entry.program_tree.clone(),
                    entry_points.clone(),
                    monomorph_registry.clone(),
                    vtable_registry,
                );

                analyzer.analyze();

                if reporter.has_errors() {
                    has_error = true;
                }

                reporter.display_first();
                analyzed_program_trees.push(analyzer.program_tree.clone());
            }

            entry_points.validate();
            drop(resolved_program_trees);

            if has_error {
                exit(1);
            }

            Box::new(CodeGenSemanticBundle {
                resolver: Box::new(resolver),
                source_map,
                decl_tables,
                analyzed_program_trees,
                vtable_registries,
                monomorph_registry,
                entry_file,
                build_dir,
                target,
            })
        }
        Err(_) => exit(1),
    }
}

pub fn build_compilation_bundle(opts: &mut CompilerOptions, file_path_opt: Option<String>) -> CodeGenContextBundle {
    // disable modulefs cache if compiling a single file

    if let Some(file_path) = &file_path_opt {
        opts.disable_modulefs_cache = true;

        // use the same directory as source directory
        let dir_path = get_directory_of_file(file_path).unwrap();
        opts.source_dirs.push(dir_path);
    }

    let codegen_semantic_bundle = build_semantic_bundle(opts, file_path_opt);

    let target = codegen_semantic_bundle.target;
    let (llvm_target, llvm_target_triple) = create_compiler_context_target(&target.info);

    // prepare trees for codegen

    let boxed_program_trees: Vec<Box<TypedProgramTree>> = codegen_semantic_bundle
        .analyzed_program_trees
        .into_iter()
        .map(|rc_refcell_tree| match Rc::try_unwrap(rc_refcell_tree) {
            Ok(refcell_tree) => Box::new(refcell_tree.into_inner()),
            Err(rc_still_shared) => Box::new(rc_still_shared.borrow().clone()),
        })
        .collect();

    let cir_modules = lower_program_trees_in_parallel(
        opts.jobs,
        boxed_program_trees,
        &*codegen_semantic_bundle.resolver,
        &*codegen_semantic_bundle.resolver,
        codegen_semantic_bundle.source_map.clone(),
        codegen_semantic_bundle.decl_tables.clone(),
        &codegen_semantic_bundle.vtable_registries,
        codegen_semantic_bundle.monomorph_registry.clone(),
        &target,
    );

    CodeGenContextBundle {
        opts: opts.clone(),
        program_trees: cir_modules,
        entry_file: codegen_semantic_bundle.entry_file,
        build_dir: codegen_semantic_bundle.build_dir,
        source_map: codegen_semantic_bundle.source_map,
        llvm_target_triple,
        llvm_target,
        target,
    }
}

pub fn resolve_target_info_from_opts(opts: &CompilerOptions) -> ABITargetInfo {
    let triple_str = if let Some(t) = opts.target.as_ref() {
        if !t.is_empty() { t.clone() } else { "".to_string() }
    } else {
        "".to_string()
    };

    let arch = if !triple_str.is_empty() {
        match triple_str.split('-').next().unwrap_or("") {
            "x86_64" => ABITargetArch::X86_64,
            "aarch64" => ABITargetArch::Aarch64,
            "riscv64" => ABITargetArch::RiscV64,
            "wasm32" => ABITargetArch::Wasm32,

            other => {
                tui_error(format!("Unsupported target architecture: {}", other));
                exit(1);
            }
        }
    } else {
        // fallback to host env
        match env::consts::ARCH {
            "x86_64" => ABITargetArch::X86_64,
            "aarch64" => ABITargetArch::Aarch64,
            "riscv64" => ABITargetArch::RiscV64,
            "wasm32" => ABITargetArch::Wasm32,

            other => {
                tui_error(format!("Unsupported host architecture: {}", other));
                exit(1);
            }
        }
    };

    let os = if !triple_str.is_empty() {
        match triple_str.split('-').nth(1).unwrap_or("") {
            "linux" => ABITargetOS::Linux,
            "windows" => ABITargetOS::Windows,
            "darwin" => ABITargetOS::MacOS,
            "unknown" | "" => {
                // fallback to host OS
                match env::consts::OS {
                    "linux" => ABITargetOS::Linux,
                    "windows" => ABITargetOS::Windows,
                    "macos" => ABITargetOS::MacOS,
                    _ => ABITargetOS::Unknown,
                }
            }
            other => {
                tui_error(format!("Unsupported target OS: {}", other));
                exit(1);
            }
        }
    } else {
        match env::consts::OS {
            "linux" => ABITargetOS::Linux,
            "windows" => ABITargetOS::Windows,
            "macos" => ABITargetOS::MacOS,
            _ => ABITargetOS::Unknown,
        }
    };

    // Step 4: resolve object format (derived, not user-selected)
    let format = match os {
        ABITargetOS::Linux | ABITargetOS::Unknown => ABITargetObjectFormat::Elf,
        ABITargetOS::MacOS => ABITargetObjectFormat::MachO,
        ABITargetOS::Windows => ABITargetObjectFormat::Coff,
    };

    ABITargetInfo { arch, os, format }
}

pub fn get_cir_dump_output_path(build_dir: &PathBuf, output_path_opt: &Option<String>) -> PathBuf {
    if let Some(output_path) = output_path_opt {
        ensure_output_dir(output_path);
        return Path::new(&output_path).to_path_buf();
    }

    let dir_path = build_dir.join(OUTPUT_DIR_FILENAME).join(CIR_DUMP_DIR_PATH);
    ensure_output_dir(&dir_path);
    return dir_path;
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
    opts: &CompilerOptions,
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

pub fn get_final_build_directory_path(build_dir: &CompilerOption_BuildDir) -> PathBuf {
    fn temp_build_dir() -> PathBuf {
        let temp_dir = env::temp_dir();
        ensure_output_dir(&temp_dir);
        temp_dir
    }

    match build_dir {
        CompilerOption_BuildDir::Provided(dir_path_str) => PathBuf::from(dir_path_str),
        CompilerOption_BuildDir::Default => temp_build_dir(),
    }
}

fn get_entry_module_file_path(
    opts: &CompilerOptions,
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
        CompilerOption_ProjectType::Library => library_file,
        CompilerOption_ProjectType::Executable => executable_file,
    };

    let unexpected_file = match project_type {
        CompilerOption_ProjectType::Library => executable_file,
        CompilerOption_ProjectType::Executable => library_file,
    };

    let project_type_name = match project_type {
        CompilerOption_ProjectType::Library => "library",
        CompilerOption_ProjectType::Executable => "executable",
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
            exit_with_msg!(format!(
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
    exit_with_msg!(format!(
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
