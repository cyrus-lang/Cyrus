use codegen::{
    context::context::CodeGenContext,
    options::{CodeGenOptions, OutputKind},
};
use diagcentral::display_single_cusotm_diag;

const PROJECT_FILE_PATH: &str = "Project.toml";

pub fn get_codegen_context(
    file_path: Option<String>,
    opts: &CodeGenOptions,
    output_kind: OutputKind,
) -> CodeGenContext {
    // if let Some(file_path) = file_path {
    //     init_from_single_file(context, file_path, opts, output_kind)
    // } else {
    //     init_from_project_file(context, opts, output_kind)
    // }
    todo!()
}

// fn init_from_single_file(
//     context: &CompilerContext,
//     file_path: String,
//     opts: &CodeGenOptions,
//     output_kind: OutputKind,
// ) -> CodeGenLLVM {
//     // Validate extension
//     if !file_path.ends_with(".cyr") {
//         error_and_exit("Invalid file extension.");
//     }

//     // Determine source dirs
//     let mut opts = opts.clone();
//     opts.source_dirs = if !opts.source_dirs.is_empty() {
//         opts.source_dirs.clone()
//     } else {
//         get_directory_of_file(file_path.clone())
//             .map(|d| vec![d])
//             .unwrap_or_else(|| error_and_exit("Could not get directory path of the input file."))
//     };

//     // Parse
//     let (program, file_name) = parse_program(file_path.clone());

//     // Create CodeGenLLVM
//     CodeGenLLVM::new(context, file_path, file_name, program, opts, true, output_kind)
//         .unwrap_or_else(|err| error_and_exit(&format!("Creating CodeGenLLVM instance failed: {}", err)))
// }

// fn init_from_project_file(context: &CompilerContext, opts: &CodeGenOptions, output_kind: OutputKind) -> CodeGenLLVM {
//     project_file_required();

//     let mut options = match codegen::opts::Options::read_toml(PROJECT_FILE_PATH.to_string()) {
//         Ok(o) => o,
//         Err(err) => error_and_exit(&err.to_string()),
//     };

//     // Ensure main file exists
//     let main_file_path = std::path::Path::new("src/main.cyr");
//     if !main_file_path.exists() {
//         error_and_exit("'src/main.cyr' file not found.");
//     }

//     let main_file_path = main_file_path
//         .canonicalize()
//         .unwrap_or_else(|_| error_and_exit("Failed to get absolute path for 'src/main.cyr'."))
//         .to_str()
//         .unwrap()
//         .to_string();

//     // Set source dirs
//     options.source_dirs = if !opts.source_dirs.is_empty() {
//         opts.source_dirs.clone()
//     } else {
//         get_directory_of_file(main_file_path.clone())
//             .map(|d| vec![d])
//             .unwrap_or_else(|| error_and_exit("Could not get directory path of the input file."))
//     };

//     options.override_options(opts);

//     let (program, file_name) = parse_program(main_file_path.clone());

//     CodeGenLLVM::new(context, main_file_path, file_name, program, options, false, output_kind)
//         .unwrap_or_else(|err| error_and_exit(&format!("Creating CodeGenLLVM instance failed: {}", err)))
// }

pub(crate) fn project_file_required() {
    if !std::path::Path::new(PROJECT_FILE_PATH).exists() {
        display_single_cusotm_diag!(format!("'{}' not found in current directory.", PROJECT_FILE_PATH));
    }
}
