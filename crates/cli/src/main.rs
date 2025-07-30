use ::parser::{Parser as CyrusParser, parse_program};
use ast::token::TokenKind;
use clap::{Parser, ValueEnum};
use codegen::build::OutputKind;
use codegen::context::CodeGenLLVM;
use codegen::diag::*;
use codegen::opts::{BuildDir, CodeModelOptions, RelocModeOptions};
use lexer::Lexer;
use utils::fs::{get_directory_of_file, read_file};

const PROJECT_FILE_PATH: &str = "Project.toml";

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum OptimizeLevel {
    None,
    O1,
    O2,
    O3,
}

impl std::fmt::Display for OptimizeLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptimizeLevel::None => write!(f, "none"),
            OptimizeLevel::O1 => write!(f, "o1"),
            OptimizeLevel::O2 => write!(f, "o2"),
            OptimizeLevel::O3 => write!(f, "o3"),
        }
    }
}

#[derive(Parser, Debug, Clone)]
struct CompilerOptions {
    #[clap(long, default_value_t = String::new(),
        help = "Specify the target triple (e.g., x86_64-pc-linux-gnu, aarch64-apple-darwin). Defaults to host triple if not specified."
    )]
    target_triple: String,

    #[clap(long, default_value_t = String::new(),
        help = "Specify the target CPU name (e.g., skylake, broadwell, generic). Defaults to host CPU if not specified."
    )]
    cpu: String,

    #[clap(long, value_enum, default_value_t = OptimizeLevel::None, help = "Set optimization level.")]
    optimize: OptimizeLevel,

    #[clap(long, value_name = "LIBRARY_PATH", help = "Add a library search path.")]
    library_path: Vec<String>,

    #[clap(long = "library", value_name = "LIB", help = "Link a library.")]
    libraries: Vec<String>,

    #[clap(long = "sources", value_name = "SOURCES", help = "Source files.")]
    source_dirs: Vec<String>,

    #[clap(
        long = "build-dir",
        value_name = "PATH",
        help = "Specifies the directory where build artifacts will be stored."
    )]
    build_dir: Option<String>,

    #[clap(long, short = 'q', help = "Suppress unnecessary output messages.")]
    quiet: bool,

    #[clap(long, help = "Set cyrus standard library path.")]
    stdlib: Option<String>,

    #[clap(long = "target-machine", help = "Display Target Machine information.")]
    display_target_machine: bool,

    #[clap(long, value_enum, default_value_t = RelocModeOptions::Default,
    help = "Set the relocation model for code generation."
    )]
    reloc_mode: RelocModeOptions,

    #[clap(long, value_enum, default_value_t = CodeModelOptions::Default,
    help = "Set the code model for code generation."
    )]
    code_model: CodeModelOptions,
}

impl CompilerOptions {
    pub fn to_compiler_options(&self) -> codegen::opts::Options {
        codegen::opts::Options {
            opt_level: match self.optimize {
                OptimizeLevel::None => None,
                OptimizeLevel::O1 => Some(1),
                OptimizeLevel::O2 => Some(2),
                OptimizeLevel::O3 => Some(3),
            },
            library_path: self.library_path.clone(),
            libraries: self.libraries.clone(),
            source_dirs: self.source_dirs.clone(),
            project_name: None,
            project_version: None,
            cyrus_version: None,
            authors: None,
            project_type: None,
            build_dir: {
                match self.build_dir.clone() {
                    Some(path) => BuildDir::Provided(path),
                    None => BuildDir::Default,
                }
            },
            quiet: self.quiet,
            stdlib_path: self.stdlib.clone(),
            display_target_machine: self.display_target_machine,
            reloc_mode: self.reloc_mode.clone(),
            code_model: self.code_model.clone(),
            target_triple: {
                if self.target_triple.trim() == "" {
                    None
                } else {
                    Some(self.target_triple.clone())
                }
            },
            cpu: {
                if self.cpu.trim() == "" {
                    None
                } else {
                    Some(self.cpu.to_string())
                }
            },
        }
    }
}

#[derive(clap::Parser, Clone)]
#[command()]
struct Args {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(clap::Subcommand, Debug, Clone)]
enum Commands {
    #[clap(about = "Create a new project", display_order = 0)]
    New {
        project_name: String,
        #[clap(long, default_value_t = false)]
        lib: bool,
    },

    #[clap(about = "Execute a compiled program", display_order = 1)]
    Run {
        file_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Fetches a library into vendor directory.", display_order = 2)]
    Fetch { libraries: String },

    #[clap(about = "Compile source code into an executable", display_order = 3)]
    Build {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Generate an object file", display_order = 4)]
    Object {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Generate a dynamic library (shared object)", display_order = 5)]
    Dylib {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Emit LLVM IR as a .ll file per module.", display_order = 6)]
    EmitLLVM {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Emit asm as a .s file per module.", display_order = 7)]
    EmitASM {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Lexical analysis only.", display_order = 8)]
    LexOnly { file_path: String },

    #[clap(about = "Display program tree.", display_order = 9)]
    ParseOnly { file_path: String },

    #[clap(about = "Check program correctness syntactically.", display_order = 10)]
    SyntacticOnly { file_path: String },

    #[clap(about = "Print version information", display_order = 11)]
    Version,
}

fn project_file_required() {
    if !std::path::Path::new(PROJECT_FILE_PATH).exists() {
        display_single_diag(Diag {
            level: DiagLevel::Error,
            kind: DiagKind::Custom(format!("'{}' not found in current directory.", PROJECT_FILE_PATH)),
            location: None,
        });
        std::process::exit(1);
    }
}

macro_rules! init_compiler {
    ($context:expr, $file_path:expr, $opts:expr, $output_kind:expr) => {{
        if let Some(file_path) = $file_path {
            if !file_path.ends_with(".cyr") {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Invalid file extension.".to_string()),
                    location: None,
                });
                std::process::exit(1);
            }

            let mut opts = $opts.clone();

            opts.source_dirs = {
                if $opts.source_dirs.len() > 0 {
                    $opts.source_dirs.clone()
                } else {
                    match get_directory_of_file(file_path.clone()) {
                        Some(source_dir) => [source_dir].to_vec(),
                        None => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom("Could not get directory path of the input file.".to_string()),
                                location: None,
                            });
                            std::process::exit(1);
                        }
                    }
                }
            };

            let (program, file_name) = parse_program(file_path.clone());
            let codegen = match CodeGenLLVM::new(
                $context,
                file_path,
                file_name.clone(),
                program,
                opts,
                true,
                $output_kind,
            ) {
                Ok(instance) => instance,
                Err(err) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!("Creating CodeGenLLVM instance failed:{}", err.to_string())),
                        location: None,
                    });
                    std::process::exit(1);
                }
            };
            codegen
        } else {
            project_file_required();

            match codegen::opts::Options::read_toml(PROJECT_FILE_PATH.to_string()) {
                Ok(mut options) => {
                    if !std::path::Path::new("src/main.cyr").exists() {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom("'src/main.cyr' file not found.".to_string()),
                            location: None,
                        });
                        std::process::exit(1);
                    }

                    let main_file_path = std::path::Path::new("src/main.cyr")
                        .canonicalize()
                        .unwrap_or_else(|_| {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom("Failed to get absolute path for 'src/main.cyr'.".to_string()),
                                location: None,
                            });
                            std::process::exit(1);
                        })
                        .to_str()
                        .unwrap()
                        .to_string();

                    options.source_dirs = {
                        if $opts.source_dirs.len() > 0 {
                            $opts.source_dirs.clone()
                        } else {
                            match get_directory_of_file(main_file_path.clone()) {
                                Some(source_dir) => [source_dir].to_vec(),
                                None => {
                                    display_single_diag(Diag {
                                        level: DiagLevel::Error,
                                        kind: DiagKind::Custom(
                                            "Could not get directory path of the input file.".to_string(),
                                        ),
                                        location: None,
                                    });
                                    std::process::exit(1);
                                }
                            }
                        }
                    };

                    options.override_options($opts);

                    let (program, file_name) = parse_program(main_file_path.clone());
                    let codegen = match CodeGenLLVM::new(
                        $context,
                        main_file_path,
                        file_name.clone(),
                        program,
                        options,
                        false,
                        $output_kind,
                    ) {
                        Ok(instance) => instance,
                        Err(err) => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom(format!(
                                    "Creating CodeGenLLVM instance failed:{}",
                                    err.to_string()
                                )),
                                location: None,
                            });
                            std::process::exit(1);
                        }
                    };

                    codegen
                }
                Err(err) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(err.to_string()),
                        location: None,
                    });
                    std::process::exit(1);
                }
            }
        }
    }};
}

pub fn main() {
    let context = CodeGenLLVM::new_context();
    let version = env!("CARGO_PKG_VERSION");
    let args = Args::parse();

    match args.cmd {
        Commands::Fetch { .. } => {
            todo!();
        }
        Commands::New { project_name, lib } => {
            if lib {
                if let Err(err) = layout::create_library_project(project_name) {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(err),
                        location: None,
                    });
                    std::process::exit(1);
                }
            } else {
                if let Err(err) = layout::create_project(project_name) {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(err),
                        location: None,
                    });
                    std::process::exit(1);
                }
            }
        }
        Commands::Run {
            file_path,
            compiler_options,
        } => {
            if file_path.is_none() {
                project_file_required();
            }

            let mut codegen = init_compiler!(
                &context,
                file_path.clone(),
                compiler_options.to_compiler_options(),
                OutputKind::None
            );
            codegen.compile();
            codegen.compilation_process_finished();
            codegen.execute();
        }
        Commands::EmitLLVM {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let mut codegen = init_compiler!(
                &context,
                file_path.clone(),
                compiler_options.to_compiler_options(),
                OutputKind::LlvmIr(output_path)
            );
            codegen.compile();
            codegen.compilation_process_finished();
        }
        Commands::EmitASM {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let mut codegen = init_compiler!(
                &context,
                file_path.clone(),
                compiler_options.to_compiler_options(),
                OutputKind::Asm(output_path)
            );
            codegen.compile();
            codegen.compilation_process_finished();
        }
        Commands::Build {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let mut codegen = init_compiler!(
                &context,
                file_path.clone(),
                compiler_options.to_compiler_options(),
                OutputKind::None
            );
            codegen.compile();
            codegen.generate_executable_file(output_path);
            codegen.compilation_process_finished();
        }
        Commands::Object {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let mut codegen = init_compiler!(
                &context,
                file_path.clone(),
                compiler_options.to_compiler_options(),
                OutputKind::ObjectFile(output_path)
            );
            codegen.compile();
            codegen.compilation_process_finished();
        }
        Commands::Dylib {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let mut codegen = init_compiler!(
                &context,
                file_path.clone(),
                compiler_options.to_compiler_options(),
                OutputKind::Dylib(output_path)
            );
            codegen.compile();
            codegen.compilation_process_finished();
        }
        Commands::Version => {
            println!("Cyrus {}", version)
        }
        Commands::LexOnly { file_path } => lex_only_command(file_path),
        Commands::ParseOnly { file_path } => parse_only_command(file_path),
        Commands::SyntacticOnly { file_path } => syntactic_only_command(file_path),
    }
}

fn lex_only_command(file_path: String) {
    let (file_content, file_name) = read_file(file_path.clone());
    let mut lexer = Lexer::new(file_content, file_name);
    loop {
        let token = lexer.next_token();
        if token.kind == TokenKind::EOF {
            break;
        }

        println!(
            "{:?} Span({}, {}) Line({}) Column({})",
            token.kind, token.span.start, token.span.end, token.loc.line, token.loc.column
        );
    }
}

fn parse_only_command(file_path: String) {
    let (file_content, file_name) = read_file(file_path.clone());
    let mut lexer = Lexer::new(file_content, file_name);

    match CyrusParser::new(&mut lexer).parse() {
        Ok(result) => println!("{:#?}", result),
        Err(errors) => {
            for err in errors {
                err.print();
            }
        }
    }
}

fn syntactic_only_command(file_path: String) {
    let (file_content, file_name) = read_file(file_path.clone());
    let mut lexer = Lexer::new(file_content, file_name);

    match CyrusParser::new(&mut lexer).parse() {
        Ok(_) => {
            println!("Program is correct grammatically.");
        }
        Err(errors) => {
            for err in errors {
                err.print();
            }
        }
    }
}
