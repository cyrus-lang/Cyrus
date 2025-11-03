use crate::{
    OwnedModule,
    builder::irreg::{LocalIRValue, LocalIRValueRegistry, LocalIRValueRegistryRef},
};
use cyrusc_abi::make_global_var_abi_name;
use cyrusc_cir::{
    CIRBlockStmt, CIRGlobalVarStmt, CIRProgramTree, CIRReturnStmt, CIRStmt, CIRVarStmt, cir_enum_as_enum_ty,
    cir_func_def_as_decl, cir_struct_as_struct_ty, cir_union_as_union_ty,
};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::TargetMachine,
    types::BasicTypeEnum,
    values::{FunctionValue, GlobalValue},
};
use std::{cell::RefCell, rc::Rc};

pub(crate) struct IRBuilderCtx<'ll> {
    pub(crate) llvmctx: &'ll Context,
    pub(crate) llvmbuilder: &'ll Builder<'ll>,
    pub(crate) llvmmodule: Rc<RefCell<Module<'ll>>>,
    pub(crate) llvmtm: &'ll TargetMachine,
    pub(crate) irreg: LocalIRValueRegistryRef<'ll>,
    pub(crate) cur_fn: Option<FunctionValue<'ll>>,
    pub(crate) cur_block: Option<BasicBlock<'ll>>,
}

impl<'ll> IRBuilderCtx<'ll> {
    pub fn new(owned_module: &'ll OwnedModule, llvmbuilder: &'ll Builder<'ll>, llvmtm: &'ll TargetMachine) -> Self {
        let llvmmodule = unsafe {
            std::mem::transmute::<Rc<RefCell<Module<'static>>>, Rc<RefCell<Module<'ll>>>>(owned_module.module.clone())
        };

        let irreg = Rc::new(RefCell::new(LocalIRValueRegistry::new()));

        Self {
            llvmctx: &owned_module.context,
            llvmbuilder,
            llvmmodule,
            llvmtm,
            irreg,
            cur_fn: None,
            cur_block: None,
        }
    }

    pub fn emit_program_tree(&mut self, cir_program_tree: &CIRProgramTree) {
        for cir_stmt in &cir_program_tree.body {
            self.emit_stmt(cir_stmt);
        }
    }

    pub(crate) fn emit_stmt(&mut self, cir_stmt: &CIRStmt) {
        match cir_stmt {
            CIRStmt::Variable(var_stmt) => self.emit_var(var_stmt),
            CIRStmt::GlobalVar(global_var_stmt) => {
                self.emit_global_var(global_var_stmt);
            }
            CIRStmt::FuncDef(func_def_stmt) => {
                let func_decl = cir_func_def_as_decl(func_def_stmt);
                let fn_value = self.emit_func_decl(&func_decl);
                self.cur_fn = Some(fn_value);
                self.emit_func_body(&func_def_stmt.body);
            }
            CIRStmt::FuncDecl(func_decl_stmt) => {
                self.emit_func_decl(func_decl_stmt);
            }
            CIRStmt::Block(block_stmt) => self.emit_body(block_stmt),
            CIRStmt::Struct(struct_stmt) => {
                self.emit_struct_ty(cir_struct_as_struct_ty(struct_stmt));
            }
            CIRStmt::Enum(enum_stmt) => {
                self.emit_enum_ty(cir_enum_as_enum_ty(enum_stmt));
            }
            CIRStmt::Union(union_stmt) => {
                self.emit_union_ty(cir_union_as_union_ty(union_stmt));
            }
            CIRStmt::ExportTuple(export_tuple_stmt) => todo!(),
            CIRStmt::Switch(cirswitch_stmt) => todo!(),
            CIRStmt::Return(return_stmt) => self.emit_ret(return_stmt),
        }
    }

    pub(crate) fn emit_body(&mut self, cir_block: &CIRBlockStmt) {
        cir_block.stmts.iter().for_each(|cir_stmt| self.emit_stmt(cir_stmt));
    }

    pub(crate) fn emit_block(&mut self, name: &str) {
        let cur_fn = self.cur_fn.unwrap();
        let basic_block = self.llvmctx.append_basic_block(cur_fn, name);
        self.llvmbuilder.position_at_end(basic_block);
        self.cur_block = Some(basic_block);
    }

    pub(crate) fn emit_ret(&self, return_stmt: &CIRReturnStmt) {
        if let Some(expr) = &return_stmt.arg {
            let basic_value = self.emit_expr(&expr).as_basic_value();
            self.llvmbuilder.build_return(Some(&basic_value)).unwrap();
        } else {
            self.llvmbuilder.build_return(None).unwrap();
        }
    }

    fn emit_var(&self, cir_var: &CIRVarStmt) {
        let ty: BasicTypeEnum<'ll> = self.emit_ty(cir_var.ty.clone()).try_into().unwrap();
        let ptr = self.llvmbuilder.build_alloca(ty, &cir_var.name).unwrap();

        if let Some(expr) = &cir_var.expr {
            let value = self.emit_expr(expr).as_basic_value();
            self.llvmbuilder.build_store(ptr, value).unwrap();
        } else {
            // zero init
            let value = ty.const_zero();
            self.llvmbuilder.build_store(ptr, value).unwrap();
        }

        let mut irreg = self.irreg.borrow_mut();
        irreg.insert(cir_var.irv_id, LocalIRValue::LValue(ptr));
    }

    fn emit_global_var(&self, cir_global_var: &CIRGlobalVarStmt) -> GlobalValue<'ll> {
        let irreg = self.irreg.borrow();

        if let Some(local_ir_value) = irreg.get(cir_global_var.irv_id) {
            return local_ir_value.as_global().cloned().unwrap();
        }

        let llvmmodule = self.llvmmodule.borrow();
        let llvmmodule_name = llvmmodule.get_name().to_str().unwrap();
        let name = make_global_var_abi_name(llvmmodule_name, &cir_global_var.name, &cir_global_var.vis);

        let ty: BasicTypeEnum<'ll> = self.emit_ty(cir_global_var.ty.clone()).try_into().unwrap();
        let global_value = llvmmodule.add_global(ty, None, &name);

        if let Some(expr) = &cir_global_var.expr {
            let value = self.emit_expr(&expr);
            self.llvmbuilder
                .build_store(global_value.as_pointer_value(), value.as_basic_value())
                .unwrap();
        } else {
            // zero init
            self.llvmbuilder
                .build_store(global_value.as_pointer_value(), ty.const_zero())
                .unwrap();
        }

        let mut irreg = self.irreg.borrow_mut();
        irreg.insert(cir_global_var.irv_id, LocalIRValue::Global(global_value));
        drop(irreg);

        global_value
    }
}
