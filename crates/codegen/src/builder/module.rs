use crate::options::CodeGenOptions;
use diagcentral::display_single_custom_diag;
use inkwell::{
    OptimizationLevel,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
    types::StructType,
    values::{FunctionValue, GlobalValue, PointerValue},
};
use resolver::{Resolver, moduleloader::ModuleFilePath};
use std::{cell::RefCell, collections::HashMap, path::Path, rc::Rc};
use typed_ast::{ModuleID, TypedProgramTree, types::ConcreteType};
use utils::fs::ensure_output_dir;

pub struct CodeGenModule<'module> {
    ctx: Rc<Context>,
    opts: &'module CodeGenOptions,
    program_tree: Rc<RefCell<TypedProgramTree>>,
}

pub(crate) struct CodeGenBuilder<'a> {
    pub module_id: ModuleID,
    pub module_file_path: ModuleFilePath,
    pub llvmmodule: Rc<RefCell<Module<'a>>>,
    pub llvmbuilder: Builder<'a>,
    pub llvmctx: &'a Context,
    pub llvmtm: Rc<TargetMachine>,
    pub irreg: LocalIRValueRegistryRef<'a>,
    pub blockreg: BlockRegistry<'a>,
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
        module_file_path: ModuleFilePath,
    ) -> CodeGenModuleOutput<'a> {
        let llvmmodule = self.ctx.create_module(&module_name);
        let builder = self.ctx.create_builder();
        let llvmtm = Rc::new(self.get_target_machine(
            self.opts.reloc_mode.to_llvm_reloc_mode(),
            self.opts.code_model.to_llvm_code_model(),
            self.opts.cpu.clone(),
            self.opts.target_triple.clone(),
        ));
        let irreg: LocalIRValueRegistryRef = Rc::new(RefCell::new(HashMap::new()));

        llvmmodule.set_triple(&llvmtm.get_triple());
        llvmmodule.set_data_layout(&llvmtm.get_target_data().get_data_layout());
        let llvmmodule = Rc::new(RefCell::new(llvmmodule));

        let mut codegen_builder = CodeGenBuilder {
            module_id,
            module_file_path,
            llvmmodule: llvmmodule.clone(),
            llvmbuilder: builder,
            llvmctx: &self.ctx,
            llvmtm: llvmtm.clone(),
            irreg,
            resolver: resolver_rc,
            blockreg: BlockRegistry::new(),
        };

        let program_tree_borrowed = self.program_tree.borrow();
        codegen_builder.build_toplevel_statements(&program_tree_borrowed.body);
        CodeGenModuleOutput::new(module_name, llvmmodule, llvmtm.clone())
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
    llvmtm: Rc<TargetMachine>,
}

impl<'a> CodeGenModuleOutput<'a> {
    pub fn new(module_name: String, llvmmodule: Rc<RefCell<Module<'a>>>, llvmtm: Rc<TargetMachine>) -> Self {
        Self {
            module_name,
            llvmmodule,
            llvmtm,
        }
    }

    pub fn emit_bitcode(&self, output_file_path: &Path) {
        let llvmmodule = self.llvmmodule.borrow();
        llvmmodule.write_bitcode_to_path(output_file_path);
        drop(llvmmodule);
    }

    pub fn emit_llvm_ir(&self, output_file_path: String) {
        let llvmmodule = self.llvmmodule.borrow();
        let llvmir_path = format!("{}/{}.ll", output_file_path, self.module_name);
        llvmmodule.print_to_file(llvmir_path).unwrap();
        drop(llvmmodule);
    }

    pub fn emit_asm(&self, output_file_path: &Path) {
        let llvmmodule = self.llvmmodule.borrow();
        self.llvmtm
            .write_to_file(&llvmmodule, FileType::Assembly, &output_file_path)
            .expect("Failed to write assembly file");
        drop(llvmmodule);
    }

    pub fn emit_obj(&self, output_file_path: &Path) {
        let llvmmodule = self.llvmmodule.borrow();
        self.llvmtm
            .write_to_file(&llvmmodule, FileType::Object, output_file_path)
            .expect("Failed to write object file");
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
    LValue(PointerValue<'a>, ConcreteType),
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

    pub fn as_lvalue(&self) -> Option<(&PointerValue<'a>, &ConcreteType)> {
        match self {
            LocalIRValue::LValue(pointer, concrete_type) => Some((pointer, concrete_type)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockRegistry<'a> {
    pub current_func_ref: Option<FunctionValue<'a>>,
    pub current_block_ref: Option<BasicBlock<'a>>,
    pub terminated_blocks: Vec<TerminatedBlockMetadata<'a>>,
    pub current_loop_ref: Option<LoopBlockRefs<'a>>,
    pub current_switch: Option<SwitchBlockRefs<'a>>,
}

impl<'a> BlockRegistry<'a> {
    pub fn new() -> Self {
        Self {
            current_func_ref: None,
            current_block_ref: None,
            current_loop_ref: None,
            current_switch: None,
            terminated_blocks: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoopBlockRefs<'a> {
    pub cond_block: BasicBlock<'a>,
    pub end_block: BasicBlock<'a>,
}

#[derive(Debug, Clone)]
pub struct SwitchBlockRefs<'a> {
    pub exit_block: BasicBlock<'a>,
}

#[derive(Debug, Clone)]
pub struct TerminatedBlockMetadata<'a> {
    pub basic_block: BasicBlock<'a>,
    pub terminated_with_return: bool,
}
