use ast::ast::*;
use ast::token::Location;
use build::{BuildManifest, OutputKind};
use diag::*;
use funcs::FuncTable;
use inkwell::OptimizationLevel;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::values::{FunctionValue, PointerValue};
use modules::ModuleMetadata;
use opts::Options;
use scope::{Scope, ScopeRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::process::exit;
use std::rc::Rc;
use structs::StructTable;
use types::{InternalType, StringType};
use utils::fs::file_stem;
use utils::tui::tui_compiled;
use values::{InternalValue, StringValue};

pub mod build;
pub mod diag;
mod enums;
mod exprs;
mod funcs;
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

pub struct CodeGenLLVM<'ctx> {
    #[allow(dead_code)]
    opts: Options,
    context: &'ctx Context,
    module: Rc<RefCell<Module<'ctx>>>,
    module_name: String,
    builder: Builder<'ctx>,
    target_machine: TargetMachine,
    build_manifest: BuildManifest,
    program: ProgramTree,
    file_path: String,
    reporter: DiagReporter,
    entry_point: Option<FuncDef>,
    is_entry_point: bool,
    entry_point_path: String,
    func_table: FuncTable<'ctx>,
    struct_table: StructTable<'ctx>,
    compiler_invoked_single: bool,
    current_func_ref: Option<FunctionValue<'ctx>>,
    current_block_ref: Option<BasicBlock<'ctx>>,
    terminated_blocks: Vec<BasicBlock<'ctx>>,
    string_type: StringType<'ctx>,
    loaded_modules: Vec<ModuleMetadata<'ctx>>,
    dependent_modules: HashMap<String, Vec<String>>,
    internal_object_modules: Vec<String>,
    output_kind: OutputKind,
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
        let module_name = file_stem(&file_name).unwrap_or(&file_name).to_string();
        let module = Rc::new(RefCell::new(context.create_module(&module_name.clone())));
        let builder = context.create_builder();
        let target_machine = CodeGenLLVM::target_machine(Rc::clone(&module));

        let codegen_llvm = CodeGenLLVM {
            opts,
            context,
            builder,
            program,
            file_path: file_path.clone(),
            reporter,
            target_machine,
            entry_point: None,
            entry_point_path: file_path.clone(),
            is_entry_point: true,
            func_table: FuncTable::new(),
            struct_table: StructTable::new(),
            build_manifest: BuildManifest::default(),
            compiler_invoked_single,
            current_func_ref: None,
            current_block_ref: None,
            terminated_blocks: Vec::new(),
            string_type: CodeGenLLVM::build_string_type(context),
            module: module.clone(),
            module_name: module_name.clone(),
            loaded_modules: Vec::new(),
            dependent_modules: HashMap::new(),
            internal_object_modules: Vec::new(),
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
                RelocMode::Default,
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
        let scope: ScopeRef<'ctx> = Rc::new(RefCell::new(Scope::new()));
        self.build_statements(Rc::clone(&scope), self.program.body.clone());

        if self.reporter.has_errors() {
            self.reporter.display_diags();
            exit(1);
        }

        self.build_entry_point();
        self.optimize();
        if !self.compiler_invoked_single {
            self.ensure_build_directory();
            self.ensure_build_manifest();
            self.rebuild_dependent_modules();
            if self.source_code_changed() || !self.object_file_exists() {
                self.save_object_file();
            }
        }

        self.generate_output();

        tui_compiled(self.file_path.clone());
    }

    pub(crate) fn build_alloca(
        &self,
        var_type: TypeSpecifier,
        var_name: String,
        loc: Location,
        span_end: usize,
    ) -> (PointerValue<'ctx>, InternalType<'ctx>) {
        let internal_type = self.build_type(var_type, loc.clone(), span_end);
        match internal_type {
            InternalType::VectorType(_) => todo!(),
            InternalType::StructType(struct_type) => (
                self.builder.build_alloca(struct_type, &var_name).unwrap(),
                InternalType::StructType(struct_type),
            ),
            InternalType::IntType(int_type) => (
                self.builder.build_alloca(int_type, &var_name).unwrap(),
                InternalType::IntType(int_type),
            ),
            InternalType::FloatType(float_type) => (
                self.builder.build_alloca(float_type, &var_name).unwrap(),
                InternalType::FloatType(float_type),
            ),
            InternalType::PointerType(typed_pointer) => (
                self.builder.build_alloca(typed_pointer.ptr_type, &var_name).unwrap(),
                typed_pointer.pointee_ty,
            ),
            InternalType::ArrayType(array_type) => (
                self.builder.build_alloca(array_type, &var_name).unwrap(),
                InternalType::ArrayType(array_type),
            ),
            InternalType::StringType(string_type) => (
                self.builder.build_alloca(string_type.struct_type, &var_name).unwrap(),
                InternalType::StringType(StringType {
                    struct_type: string_type.struct_type,
                }),
            ),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot allocate memory for non-basic type.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_store(&self, ptr: PointerValue, value: InternalValue<'ctx>) {
        let result = match value {
            InternalValue::IntValue(int_value, _) => self.builder.build_store(ptr, int_value),
            InternalValue::FloatValue(float_value, _) => self.builder.build_store(ptr, float_value),
            InternalValue::ArrayValue(array_value, _) => self.builder.build_store(ptr, array_value),
            InternalValue::VectorValue(vector_value, _) => self.builder.build_store(ptr, vector_value),
            InternalValue::StructValue(struct_value, _) => self.builder.build_store(ptr, struct_value),
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
}
