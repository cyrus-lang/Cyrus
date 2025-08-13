use crate::options::CodeGenOptions;
use diagcentral::display_single_custom_diag;
use inkwell::{
    OptimizationLevel,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
    types::StructType,
    values::{FunctionValue, GlobalValue},
};
use resolver::Resolver;
use std::{cell::RefCell, collections::HashMap, path::Path, rc::Rc};
use typed_ast::{ModuleID, TypedProgramTree};
use utils::fs::ensure_output_dir;

pub struct CodeGenModule<'module> {
    ctx: Rc<Context>,
    opts: &'module CodeGenOptions,
    program_tree: Rc<RefCell<TypedProgramTree>>,
}

pub(crate) struct CodeGenBuilder<'a> {
    pub module_id: ModuleID,
    pub llvmmodule: Rc<RefCell<Module<'a>>>,
    pub llvmbuilder: Builder<'a>,
    pub llvmctx: &'a Context,
    pub llvmtm: TargetMachine,
    pub irreg: LocalIRValueRegistryRef<'a>,
    pub resolver: Rc<Resolver>,
}

impl<'module> CodeGenModule<'module> {
    pub fn new(opts: &'module CodeGenOptions, program_tree: Rc<RefCell<TypedProgramTree>>) -> Self {
        let ctx = Rc::new(Context::create());

        Self {
            ctx,
            opts,
            program_tree,
        }
    }

    pub fn codegen<'a>(
        &'a self,
        resolver_rc: Rc<Resolver>,
        module_id: ModuleID,
        module_name: String,
    ) -> CodeGenModuleOutput<'a> {
        let llvmmodule = self.ctx.create_module(&module_name);
        let builder = self.ctx.create_builder();
        let llvmtm = self.get_target_machine(
            self.opts.reloc_mode.to_llvm_reloc_mode(),
            self.opts.code_model.to_llvm_code_model(),
            self.opts.cpu.clone(),
            self.opts.target_triple.clone(),
        );
        let irreg: LocalIRValueRegistryRef = Rc::new(RefCell::new(HashMap::new()));

        llvmmodule.set_triple(&llvmtm.get_triple());
        llvmmodule.set_data_layout(&llvmtm.get_target_data().get_data_layout());
        let llvmmodule = Rc::new(RefCell::new(llvmmodule));

        let codegen_builder = CodeGenBuilder {
            module_id,
            llvmmodule: llvmmodule.clone(),
            llvmbuilder: builder,
            llvmctx: &self.ctx,
            llvmtm,
            irreg,
            resolver: resolver_rc,
        };

        let program_tree_borrowed = self.program_tree.borrow();
        codegen_builder.build_toplevel_statements(&program_tree_borrowed.body);
        CodeGenModuleOutput::new(module_name, llvmmodule)
    }

    fn get_target_machine(
        &self,
        reloc_mode: RelocMode,
        code_model: CodeModel,
        cpu: Option<String>,
        target_triple_opt: Option<String>,
    ) -> TargetMachine {
        Target::initialize_all(&InitializationConfig::default());

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
                display_single_custom_diag!(
                    "Failed to create LLVM Target Machine with given target_triple, cpu, features.".to_string()
                );
            }
        };

        target_machine
    }
}

pub struct CodeGenModuleOutput<'a> {
    module_name: String,
    llvmmodule: Rc<RefCell<Module<'a>>>,
}

impl<'a> CodeGenModuleOutput<'a> {
    pub fn new(module_name: String, llvmmodule: Rc<RefCell<Module<'a>>>) -> Self {
        Self {
            module_name,
            llvmmodule,
        }
    }

    pub fn emit_bitcode(&self, output_file_path: &Path) {
        let llvmmodule = self.llvmmodule.borrow();
        llvmmodule.write_bitcode_to_path(output_file_path);
        drop(llvmmodule);
    }

    pub fn emit_llvm_ir(&self, output_path: String) {
        let llvmmodule = self.llvmmodule.borrow();
        let llvmir_path = format!("{}/{}.ll", output_path, self.module_name);
        ensure_output_dir(output_path.clone());
        ensure_output_dir(output_path);
        llvmmodule.print_to_file(llvmir_path).unwrap();
        drop(llvmmodule);
    }
}

pub type LocalIRValueID = u32;
pub type LocalIRValueRegistryRef<'a> = Rc<RefCell<LocalIRValueRegistry<'a>>>;
pub type LocalIRValueRegistry<'a> = HashMap<LocalIRValueID, LocalIRValue<'a>>;

#[derive(Debug, Clone)]
pub enum LocalIRValue<'a> {
    Func(FunctionValue<'a>),
    GlobalValue(GlobalValue<'a>),
    Struct(StructType<'a>),
}

impl<'a> LocalIRValue<'a> {
    pub fn as_func(&self) -> Option<&FunctionValue<'a>> {
        match self {
            LocalIRValue::Func(func) => Some(func),
            _ => None,
        }
    }

    pub fn as_global_value(&self) -> Option<&GlobalValue<'a>> {
        match self {
            LocalIRValue::GlobalValue(global_value) => Some(global_value),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<&StructType<'a>> {
        match self {
            LocalIRValue::Struct(struct_type) => Some(struct_type),
            _ => None,
        }
    }
}
