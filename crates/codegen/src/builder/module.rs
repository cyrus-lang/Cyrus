use crate::{
    builder::dibuilder::new_di_builder,
    context::context::{get_target_machine, make_module_name},
    options::CodeGenOptions,
};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    debug_info::{DICompileUnit, DebugInfoBuilder},
    module::Module,
    targets::{FileType, TargetMachine},
    types::{ArrayType, StructType},
    values::{FunctionValue, GlobalValue, PointerValue},
};
use resolver::Resolver;
use std::{cell::RefCell, collections::HashMap, fs, path::Path, rc::Rc};
use typed_ast::{ModuleID, TypedProgramTree, types::ConcreteType};

pub struct CodeGenModule<'module> {
    ctx: Rc<Context>,
    opts: &'module CodeGenOptions,
    program_tree: Rc<RefCell<TypedProgramTree>>,
}

pub(crate) struct CodeGenDIBuilder<'a> {
    pub dibuilder: DebugInfoBuilder<'a>,
    pub dicompunit: DICompileUnit<'a>,
}

pub(crate) struct CodeGenBuilder<'a> {
    pub module_id: ModuleID,
    pub llvmmodule: Rc<RefCell<Module<'a>>>,
    pub llvmbuilder: Builder<'a>,
    pub llvmctx: &'a Context,
    pub llvmtm: Rc<TargetMachine>,
    pub irreg: LocalIRValueRegistryRef<'a>,
    pub blockreg: BlockRegistry<'a>,
    pub resolver: Rc<Resolver>,
    pub dibuilder: CodeGenDIBuilder<'a>,
}

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn get_module_name(&self, module_id: ModuleID) -> String {
        let module_file_path = self.resolver.get_module_file_path(module_id).unwrap();
        make_module_name(self.resolver.master_module_file_path.clone(), module_file_path)
    }
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
        module_file_path: String,
    ) -> CodeGenModuleOutput<'a> {
        let llvmmodule = self.ctx.create_module(&module_name);
        let builder = self.ctx.create_builder();
        let llvmtm = Rc::new(get_target_machine(
            self.opts.reloc_mode.to_llvm_reloc_mode(),
            self.opts.code_model.to_llvm_code_model(),
            self.opts.cpu.clone(),
            self.opts.target_triple.clone(),
        ));
        let irreg: LocalIRValueRegistryRef = Rc::new(RefCell::new(HashMap::new()));

        llvmmodule.set_triple(&llvmtm.get_triple());
        llvmmodule.set_data_layout(&llvmtm.get_target_data().get_data_layout());

        let (dibuilder, dicompunit) = new_di_builder(&llvmmodule, module_file_path.clone());

        let llvmmodule = Rc::new(RefCell::new(llvmmodule));
        let mut codegen_builder = CodeGenBuilder {
            module_id,
            llvmmodule: llvmmodule.clone(),
            llvmbuilder: builder,
            llvmctx: &self.ctx,
            llvmtm: llvmtm.clone(),
            irreg,
            resolver: resolver_rc,
            blockreg: BlockRegistry::new(),
            dibuilder: CodeGenDIBuilder { dibuilder, dicompunit },
        };

        let program_tree_borrowed = self.program_tree.borrow();
        codegen_builder.build_toplevel_statements(&program_tree_borrowed.body);
        CodeGenModuleOutput::new(module_name, llvmmodule, llvmtm.clone())
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
        let output_path = Path::new(&output_file_path);

        if let Some(parent) = output_path.parent() {
            if let Err(e) = fs::create_dir_all(parent) {
                eprintln!("Error creating directory: {}", e);
                return;
            }
        }

        let llvmir_path = output_path.join(format!("{}.ll", self.module_name));
        llvmmodule.print_to_file(&llvmir_path).unwrap();
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
    Struct(StructType<'a>),
    Enum((StructType<'a>, ArrayType<'a>)),
    GlobalValue(GlobalValue<'a>, ConcreteType),
    LValue(PointerValue<'a>, ConcreteType),
}

impl<'a> LocalIRValue<'a> {
    pub fn as_func(&self) -> Option<&FunctionValue<'a>> {
        match self {
            LocalIRValue::Func(func) => Some(func),
            _ => None,
        }
    }

    pub fn as_global_value(&self) -> Option<(&GlobalValue<'a>, &ConcreteType)> {
        match self {
            LocalIRValue::GlobalValue(global_value, concrete_type) => Some((global_value, concrete_type)),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<&StructType<'a>> {
        match self {
            LocalIRValue::Struct(struct_type) => Some(struct_type),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<(&StructType<'a>, &ArrayType<'a>)> {
        match self {
            LocalIRValue::Enum((struct_type, payload_type)) => Some((struct_type, payload_type)),
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
    pub end_block: BasicBlock<'a>,
    pub inc_block: BasicBlock<'a>,
}

#[derive(Debug, Clone)]
pub struct SwitchBlockRefs<'a> {
    pub exit_block: BasicBlock<'a>,
}

#[derive(Debug, Clone)]
pub struct TerminatedBlockMetadata<'a> {
    pub basic_block: BasicBlock<'a>,
}
