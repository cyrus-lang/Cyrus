use crate::funcs::FuncMetadata;
use crate::modules::ImportedModuleMetadata;
use crate::opts::BuildDir;
use crate::stmts::{LoopBlockRefs, TerminatedBlockMetadata};
use crate::types::TypedefTable;
use crate::variables::GlobalVariablesTable;
use ast::ast::*;
use ast::token::Location;
use build::{BuildManifest, OutputKind};
use diag::*;
use funcs::FuncTable;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple};
use inkwell::values::PointerValue;
use inkwell::{AddressSpace, OptimizationLevel};
use opts::Options;
use scope::{Scope, ScopeRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::process::exit;
use std::rc::Rc;
use structs::StructTable;
use types::InternalType;
use utils::fs::{file_stem, relative_to_absolute};
use utils::tui::{tui_compile_finished, tui_compiled};
use values::InternalValue;

pub mod build;
pub mod diag;
mod enums;
mod exprs;
mod funcs;
mod internals;
mod modules;
pub mod opts;
mod runtime;
mod scope;
mod stmts;
mod strings;
mod structs;
mod types;
mod values;
mod variables;

pub struct CodeGenLLVM<'ctx> {
    #[allow(dead_code)]
    opts: Options,
    context: &'ctx Context,
    module: Rc<RefCell<Module<'ctx>>>,
    module_id: String,
    builder: Builder<'ctx>,
    target_machine: TargetMachine,
    build_manifest: BuildManifest,
    program: ProgramTree,
    file_path: String,
    reporter: DiagReporter,
    entry_point: Option<FuncDef>,
    entry_point_path: String,
    compiler_invoked_single: bool,
    current_func_ref: Option<FuncMetadata<'ctx>>,
    current_block_ref: Option<BasicBlock<'ctx>>,
    terminated_blocks: Vec<TerminatedBlockMetadata<'ctx>>,
    current_loop_ref: Option<LoopBlockRefs<'ctx>>,
    dependent_modules: HashMap<String, Vec<String>>,
    output_kind: OutputKind,
    final_build_dir: String,
    func_table: FuncTable<'ctx>,
    struct_table: StructTable<'ctx>,
    global_variables_table: GlobalVariablesTable<'ctx>,
    typedef_table: TypedefTable<'ctx>,
    imported_modules: Vec<ImportedModuleMetadata<'ctx>>,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn new(
        context: &'ctx Context,
        file_path: String,
        file_name: String,
        program: ProgramTree,
        opts: Options,
        compiler_invoked_single: bool,
        output_kind: OutputKind,
    ) -> Result<Self, LLVMString> {
        let reporter = DiagReporter::new();
        let module_id = file_stem(&file_name).unwrap_or(&file_name).to_string();
        let module = Rc::new(RefCell::new(context.create_module(&module_id.clone())));
        let builder = context.create_builder();
        let target_machine = CodeGenLLVM::setup_target_machine(
            Rc::clone(&module),
            opts.reloc_mode.to_llvm_reloc_mode(),
            opts.code_model.to_llvm_code_model(),
            opts.cpu.clone(),
            opts.target_triple.clone(),
        );

        let final_build_dir = {
            match opts.build_dir.clone() {
                BuildDir::Default => {
                    // specify a tmp directory to be used as build_dir
                    env::temp_dir().to_str().unwrap().to_string()
                }
                BuildDir::Provided(path) => path,
            }
        };

        let base_dir = env::current_dir().unwrap().to_str().unwrap().to_string();
        let file_path = relative_to_absolute(file_path.clone(), base_dir).unwrap();

        let codegen_llvm = CodeGenLLVM {
            final_build_dir,
            opts,
            context,
            builder,
            program,
            file_path: file_path.clone(),
            reporter,
            target_machine,
            entry_point: None,
            entry_point_path: file_path.clone(),
            func_table: FuncTable::new(),
            struct_table: StructTable::new(),
            typedef_table: TypedefTable::new(),
            global_variables_table: GlobalVariablesTable::new(),
            build_manifest: BuildManifest::default(),
            compiler_invoked_single,
            current_func_ref: None,
            current_block_ref: None,
            terminated_blocks: Vec::new(),
            current_loop_ref: None,
            module: module.clone(),
            module_id: module_id.clone(),
            imported_modules: Vec::new(),
            dependent_modules: HashMap::new(),
            output_kind,
        };

        if codegen_llvm.opts.display_target_machine {
            codegen_llvm.display_target_machine_information();
        }
        Ok(codegen_llvm)
    }

    fn display_target_machine_information(&self) {
        if !self.opts.quiet {
            println!("Target Triple: {}", self.target_machine.get_triple().to_string());
            println!("CPU Name: {}", self.target_machine.get_cpu().to_string());
            println!(
                "Data Layout: {}",
                self.target_machine
                    .get_target_data()
                    .get_data_layout()
                    .as_str()
                    .to_str()
                    .unwrap()
            );
            println!("Pointer Size: {}-bit", {
                self.target_machine.get_target_data().get_pointer_byte_size(None) * 8
            });
            println!("Optimization Level: {}", {
                if let Some(opt_level) = self.opts.opt_level {
                    format!("O{}", opt_level)
                } else {
                    "Default".to_string()
                }
            });
            println!("Relocation Mode: {}", self.opts.reloc_mode);
            println!("Code Model: {}", self.opts.code_model);
        }
    }

    pub fn setup_target_machine(
        module: Rc<RefCell<Module>>,
        reloc_mode: RelocMode,
        code_model: CodeModel,
        cpu: Option<String>,
        target_triple_opt: Option<String>,
    ) -> TargetMachine {
        Target::initialize_all(&InitializationConfig::default());

        let module = module.borrow_mut();

        let cpu = if let Some(cpu) = cpu {
            cpu
        } else {
            TargetMachine::get_host_cpu_name().to_string()
        };
        let features = TargetMachine::get_host_cpu_features().to_string();

        let target_triple = if let Some(target_triple_str) = target_triple_opt {
            TargetTriple::create(&target_triple_str)
        } else {
            TargetMachine::get_default_triple()
        };

        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = match target.create_target_machine(
            &target_triple,
            &cpu,
            &features,
            OptimizationLevel::Default,
            reloc_mode,
            code_model,
        ) {
            Some(target_machine) => target_machine,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Failed to create LLVM Target Machine with given target_triple, cpu, features.",
                    )),
                    location: None,
                });
                exit(1);
            }
        };

        module.set_triple(&target_machine.get_triple());
        module.set_data_layout(&target_machine.get_target_data().get_data_layout());
        target_machine
    }

    pub fn new_context() -> Context {
        Context::create()
    }

    pub fn compile(&mut self) {
        self.enable_module_flags();

        let scope: ScopeRef<'ctx> = Rc::new(RefCell::new(Scope::new()));
        self.build_statements(Rc::clone(&scope), self.program.body.clone());

        if self.reporter.has_errors() {
            self.reporter.display_diags();
            exit(1);
        }

        self.ensure_build_directory(self.final_build_dir.clone());
        self.ensure_build_manifest(self.final_build_dir.clone());

        self.build_entry_point();

        match self.output_kind {
            OutputKind::LlvmIr(_) => {}
            _ => {
                self.run_passes();
            }
        }
        if !self.compiler_invoked_single {
            if self.source_code_changed(self.final_build_dir.clone()) || !self.object_file_exists() {
                self.save_object_file(self.final_build_dir.clone());
            }
        } else {
            self.save_object_file(self.final_build_dir.clone());
        }

        self.generate_output();

        if !self.opts.quiet {
            tui_compiled(self.file_path.clone());
        }
    }

    pub fn compilation_process_finished(&self) {
        if !self.opts.quiet {
            tui_compile_finished();
        }
    }

    pub(crate) fn is_current_module_entry_point(&self) -> bool {
        self.file_path == self.entry_point_path
    }
}
