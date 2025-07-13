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
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};
use opts::Options;
use scope::{Scope, ScopeRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::process::exit;
use std::rc::Rc;
use structs::StructTable;
use types::{InternalStringType, InternalType};
use utils::fs::file_stem;
use utils::tui::{tui_compile_finished, tui_compiled};
use values::{InternalValue, StringValue};

pub mod build;
pub mod diag;
mod enums;
mod exprs;
mod funcs;
mod internals;
mod intrinsics;
mod modules;
pub mod opts;
mod runtime;
mod scope;
mod stmts;
mod strings;
mod structs;
mod tests;
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
    current_func_ref: Option<FunctionValue<'ctx>>,
    current_block_ref: Option<BasicBlock<'ctx>>,
    terminated_blocks: Vec<TerminatedBlockMetadata<'ctx>>,
    current_loop_ref: Option<LoopBlockRefs<'ctx>>,
    dependent_modules: HashMap<String, Vec<String>>,
    output_kind: OutputKind,
    final_build_dir: String,
    string_type: InternalStringType<'ctx>,
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
        let target_machine = CodeGenLLVM::target_machine(Rc::clone(&module));

        let final_build_dir = {
            match opts.build_dir.clone() {
                BuildDir::Default => {
                    // specify a tmp directory to be used as build_dir
                    env::temp_dir().to_str().unwrap().to_string()
                }
                BuildDir::Provided(path) => path,
            }
        };

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
            string_type: CodeGenLLVM::build_string_type(context),
            module: module.clone(),
            module_id: module_id.clone(),
            imported_modules: Vec::new(),
            dependent_modules: HashMap::new(),
            output_kind,
        };

        Ok(codegen_llvm)
    }

    pub fn target_machine(module: Rc<RefCell<Module>>) -> TargetMachine {
        let module = module.borrow_mut();

        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let target_triple = TargetMachine::get_default_triple();
        let cpu = TargetMachine::get_host_cpu_name().to_string();
        let features = TargetMachine::get_host_cpu_features().to_string();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                &cpu,
                &features,
                OptimizationLevel::Default,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();
        module.set_triple(&target_triple);
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
            self.rebuild_dependent_modules();
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

    pub(crate) fn build_alloca(
        &self,
        var_type: TypeSpecifier,
        var_name: String,
        loc: Location,
        span_end: usize,
    ) -> (PointerValue<'ctx>, InternalType<'ctx>) {
        let internal_type = self.build_type(var_type.clone(), loc.clone(), span_end);
        let basic_type = match internal_type.to_basic_type(self.context.ptr_type(AddressSpace::default())) {
            Ok(basic_type) => basic_type,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(err.to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        };

        let alloca = self.builder.build_alloca(basic_type, &var_name).unwrap();
        (alloca, internal_type)
    }

    pub(crate) fn build_store(&self, ptr: PointerValue, value: InternalValue<'ctx>) {
        let result = match value {
            InternalValue::BoolValue(bool_value, _) => self.builder.build_store(ptr, bool_value),
            InternalValue::IntValue(int_value, _) => self.builder.build_store(ptr, int_value),
            InternalValue::FloatValue(float_value, _) => self.builder.build_store(ptr, float_value),
            InternalValue::ArrayValue(array_value, _) => self.builder.build_store(ptr, array_value),
            InternalValue::VectorValue(vector_value, _) => self.builder.build_store(ptr, vector_value),
            InternalValue::StructValue(struct_value, _) => self.builder.build_store(ptr, struct_value),
            InternalValue::UnnamedStructValue(struct_value, _) => self.builder.build_store(ptr, struct_value),
            InternalValue::StringValue(string_value) => self.builder.build_store(ptr, string_value.struct_value),
            InternalValue::PointerValue(pointer_value) => self.builder.build_store(ptr, pointer_value.ptr),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(String::from("Cannot store non-basic value in pointer.")),
                    location: None,
                });
                exit(1);
            }
        };

        if let Err(err) = result {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!("Cannot store value in pointer:\n{}", err.to_string())),
                location: None,
            });
            exit(1);
        }
    }

    pub(crate) fn is_current_module_entry_point(&self) -> bool {
        self.file_path == self.entry_point_path
    }
}
