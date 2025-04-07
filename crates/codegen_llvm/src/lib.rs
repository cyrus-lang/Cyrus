use ast::ast::*;
use ast::token::{Location, TokenKind};
use build::BuildManifest;
use diag::*;
use funcs::FuncTable;
use inkwell::OptimizationLevel;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::llvm_sys::prelude::LLVMValueRef;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use opts::Options;
use scope::{Scope, ScopeRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::process::exit;
use std::rc::Rc;
use structs::StructTable;
use types::{AnyType, StringType};
use values::{AnyValue, StringValue};

mod build;
pub mod diag;
mod enums;
mod exprs;
mod funcs;
mod linkage;
pub mod opts;
mod runtime;
mod scope;
mod stmts;
mod structs;
mod tests;
mod types;
mod values;

pub struct CodeGenLLVM<'ctx> {
    #[allow(dead_code)]
    opts: Options,
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    target_machine: TargetMachine,
    build_manifest: BuildManifest,
    program: ProgramTree,
    file_path: String,
    reporter: DiagReporter,
    entry_point: Option<FuncDef>,
    func_table: FuncTable<'ctx>,
    struct_table: StructTable<'ctx>,
    internal_funcs_table: HashMap<String, LLVMValueRef>,
    compiler_invoked_single: bool,
    current_func_ref: Option<FunctionValue<'ctx>>,
    current_block_ref: Option<BasicBlock<'ctx>>,
    terminated_blocks: Vec<BasicBlock<'ctx>>,
    string_type: StringType<'ctx>,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn new(
        context: &'ctx Context,
        file_path: String,
        file_name: String,
        program: ProgramTree,
        opts: Options,
        compiler_invoked_single: bool,
    ) -> Result<Self, LLVMString> {
        let reporter = DiagReporter::new();
        let module = context.create_module(&file_name);
        let builder = context.create_builder();

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

        let mut codegen_llvm = CodeGenLLVM {
            opts,
            context,
            module,
            builder,
            program,
            file_path,
            reporter,
            target_machine,
            entry_point: None,
            func_table: FuncTable::new(),
            struct_table: StructTable::new(),
            internal_funcs_table: HashMap::new(),
            build_manifest: BuildManifest::default(),
            compiler_invoked_single,
            current_func_ref: None,
            current_block_ref: None,
            terminated_blocks: Vec::new(),
            string_type: CodeGenLLVM::build_string_type(context),
        };

        codegen_llvm.load_runtime();
        Ok(codegen_llvm)
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

        self.optimize();
        self.build_entry_point();

        if !self.compiler_invoked_single {
            self.ensure_build_directory();
            self.ensure_build_manifest();
            if self.source_code_changed() || !self.object_file_exists() {
                self.save_object_file();
            }
        }
    }

    pub(crate) fn build_alloca(
        &self,
        var_type_token: TokenKind,
        var_name: String,
        loc: Location,
        span_end: usize,
    ) -> (PointerValue<'ctx>, AnyType<'ctx>) {
        let any_type = self.build_type(var_type_token, loc.clone(), span_end);
        match any_type {
            AnyType::VectorType(vector_type) => todo!(),
            AnyType::StructType(struct_type) => (
                self.builder.build_alloca(struct_type, &var_name).unwrap(),
                AnyType::StructType(struct_type),
            ),
            AnyType::IntType(int_type) => (
                self.builder.build_alloca(int_type, &var_name).unwrap(),
                AnyType::IntType(int_type),
            ),
            AnyType::FloatType(float_type) => (
                self.builder.build_alloca(float_type, &var_name).unwrap(),
                AnyType::FloatType(float_type),
            ),
            AnyType::PointerType(typed_pointer) => (
                self.builder.build_alloca(typed_pointer.ptr_type, &var_name).unwrap(),
                typed_pointer.pointee_ty,
            ),
            AnyType::ArrayType(array_type) => (
                self.builder.build_alloca(array_type, &var_name).unwrap(),
                AnyType::ArrayType(array_type),
            ),
            AnyType::StringType(string_type) => (
                self.builder.build_alloca(string_type.struct_type, &var_name).unwrap(),
                AnyType::StringType(StringType {
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

    pub(crate) fn build_store(&self, ptr: PointerValue, value: AnyValue<'ctx>) {
        let result = match value {
            AnyValue::IntValue(int_value) => self.builder.build_store(ptr, int_value),
            AnyValue::FloatValue(float_value) => self.builder.build_store(ptr, float_value),
            AnyValue::ArrayValue(array_value) => self.builder.build_store(ptr, array_value),
            AnyValue::PointerValue(pointer_value) => self.builder.build_store(ptr, pointer_value.ptr),
            AnyValue::VectorValue(vector_value) => self.builder.build_store(ptr, vector_value),
            AnyValue::StructValue(struct_value) => self.builder.build_store(ptr, struct_value),
            AnyValue::StringValue(string_value) => self.builder.build_store(ptr, string_value.struct_value),
            AnyValue::OpaquePointer(pointer_value) => self.builder.build_store(ptr, pointer_value),
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

    fn unescape_string(&self, s: &str) -> String {
        s.replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\\"", "\"")
            .replace("\\'", "'")
            .replace("\\\\", "\\")
    }
}
