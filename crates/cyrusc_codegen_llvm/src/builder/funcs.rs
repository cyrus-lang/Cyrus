use crate::builder::{
    builder::IRBuilderCtx,
    irreg::LocalIRValue,
    values::{InternalValue, InternalValueKind},
};
use cyrusc_abi::{
    callconv::CallConv, export::ExportKind, flags::OptionalFlag, inline::Inlining, linkage::Linkage,
    modifiers::FuncModifiers, prologue::Prologue,
};
use cyrusc_cir::{CIRBlockStmt, CIRFuncDeclStmt, CIRFuncParams, CIRLambda, cir_func_decl_as_func_ty, types::CIRTy};
use cyrusc_tast::types::PlainType;
use inkwell::{
    DLLStorageClass,
    attributes::{Attribute, AttributeLoc},
    types::BasicTypeEnum,
    values::{FunctionValue, PointerValue},
};

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_fn_as_ptr(&self, fn_value: FunctionValue<'ll>) -> PointerValue<'ll> {
        fn_value.as_global_value().as_pointer_value()
    }

    pub(crate) fn emit_func_params(&self, func_params: &CIRFuncParams) {
        func_params.list.iter().enumerate().for_each(|(param_idx, param)| {
            let basic_value = self
                .cur_fn
                .unwrap()
                .get_nth_param(param_idx.try_into().unwrap())
                .unwrap();

            let mut irreg = self.irreg.borrow_mut();
            if basic_value.is_pointer_value() {
                irreg.insert(
                    param.irv_id,
                    LocalIRValue::LValue(basic_value.into_pointer_value(), param.ty.clone()),
                );
            } else {
                let ty: BasicTypeEnum<'ll> = self.emit_ty(param.ty.clone()).try_into().unwrap();
                let ptr = self.llvmbuilder.build_alloca(ty, "param").unwrap();
                self.llvmbuilder.build_store(ptr, basic_value).unwrap();

                irreg.insert(param.irv_id, LocalIRValue::LValue(ptr, param.ty.clone()));
            }
            drop(irreg);
        });
    }

    pub(crate) fn emit_lambda(&mut self, lambda: &CIRLambda) -> InternalValue<'ll> {
        let parent_fn = self.cur_fn.clone();
        let parent_block = self.cur_block.clone();

        let mut modifiers = FuncModifiers::default();
        modifiers.inline = Some(Inlining::Inline);
        let func_decl = CIRFuncDeclStmt {
            irv_id: lambda.irv_id,
            name: "lambda".to_string(),
            params: lambda.params.clone(),
            ret: lambda.ret.clone(),
            modifiers,
        };
        let fn_value = self.emit_func_decl(&func_decl);
        self.cur_fn = Some(fn_value);

        self.emit_func_body(&lambda.params, &lambda.body);

        let fn_pointer = fn_value.as_global_value().as_pointer_value().into();

        self.cur_fn = parent_fn;
        self.cur_block = parent_block;
        if let Some(basic_block) = self.cur_block {
            self.llvmbuilder.position_at_end(basic_block);
        }

        InternalValue::new(
            CIRTy::Pointer(Box::new(CIRTy::PlainType(PlainType::Void))),
            InternalValueKind::RValue(fn_pointer),
        )
    }

    pub(crate) fn emit_func_decl(&mut self, func_decl: &CIRFuncDeclStmt) -> FunctionValue<'ll> {
        let fn_type = self.emit_func_ty(cir_func_decl_as_func_ty(func_decl));

        let func_name = &func_decl.name;
        let llvmmodule = self.llvmmodule.borrow();

        let fn_value = llvmmodule
            .get_function(func_name)
            .unwrap_or_else(|| llvmmodule.add_function(func_name, fn_type, None));

        self.set_func_modifiers(&fn_value, &func_decl.modifiers);

        {
            let cir_func_ty = cir_func_decl_as_func_ty(func_decl);
            let mut irreg = self.irreg.borrow_mut();
            irreg.insert(
                func_decl.irv_id,
                LocalIRValue::Func(fn_value, CIRTy::FuncType(cir_func_ty)),
            );
        }

        fn_value
    }

    pub(crate) fn set_func_modifiers(&self, func: &FunctionValue<'ll>, modifiers: &FuncModifiers) {
        if let Some(export) = &modifiers.export {
            let dll_storage_class = match export {
                ExportKind::DllImport => DLLStorageClass::Import,
                ExportKind::DllExport => DLLStorageClass::Export,
            };
            func.as_global_value().set_dll_storage_class(dll_storage_class);
        }

        if let Some(linkage) = &modifiers.linkage {
            let llvm_linkage = match linkage {
                Linkage::Extern => inkwell::module::Linkage::External,
                Linkage::Weak => inkwell::module::Linkage::WeakAny,
                Linkage::LinkOnce => inkwell::module::Linkage::LinkOnceAny,
            };
            func.set_linkage(llvm_linkage);
        }

        if let Some(inline) = &modifiers.inline {
            let attr_name = match inline {
                Inlining::Inline => "inlinehint",
                Inlining::NoInline => "noinline",
                Inlining::AlwaysInline => "alwaysinline",
            };

            let enum_kind_id = Attribute::get_named_enum_kind_id(attr_name);
            let enum_attr = self.llvmctx.create_enum_attribute(enum_kind_id, 0);

            func.add_attribute(inkwell::attributes::AttributeLoc::Function, enum_attr);
        }

        if let Some(prologue) = &modifiers.prologue {
            if *prologue == Prologue::Naked {
                let naked_attr = self.llvmctx.create_string_attribute("naked", "");
                func.add_attribute(AttributeLoc::Function, naked_attr);
            }
        }

        if let Some(cc) = &modifiers.callconv {
            let llvm_cc = match cc {
                CallConv::C => 0,
                CallConv::Naked => 8,
                CallConv::Interrupt => 83,
                CallConv::Fast => 8,
                CallConv::Cold => 9,
            };
            func.set_call_conventions(llvm_cc);
        }

        for section in &modifiers.placement {
            func.set_section(Some(section.0.as_str()));
        }

        if let Some(section) = &modifiers.section {
            func.set_section(Some(section.0.as_str()));
        }

        for flag in &modifiers.optional_flags {
            match flag {
                OptionalFlag::NoReturn => {
                    let id = Attribute::get_named_enum_kind_id("noreturn");
                    let attr = self.llvmctx.create_enum_attribute(id, 0);
                    func.add_attribute(inkwell::attributes::AttributeLoc::Function, attr);
                }
                OptionalFlag::NoUnwind => {
                    let id = Attribute::get_named_enum_kind_id("nounwind");
                    let attr = self.llvmctx.create_enum_attribute(id, 0);
                    func.add_attribute(inkwell::attributes::AttributeLoc::Function, attr);
                }
                OptionalFlag::Cold => {
                    let id = Attribute::get_named_enum_kind_id("cold");
                    let attr = self.llvmctx.create_enum_attribute(id, 0);
                    func.add_attribute(inkwell::attributes::AttributeLoc::Function, attr);
                }
                OptionalFlag::Hot => {
                    let id = Attribute::get_named_enum_kind_id("hot");
                    let attr = self.llvmctx.create_enum_attribute(id, 0);
                    func.add_attribute(inkwell::attributes::AttributeLoc::Function, attr);
                }
                OptionalFlag::OptSize => {
                    let id = Attribute::get_named_enum_kind_id("optsize");
                    let attr = self.llvmctx.create_enum_attribute(id, 0);
                    func.add_attribute(inkwell::attributes::AttributeLoc::Function, attr);
                }
                OptionalFlag::OptNone => {
                    let id = Attribute::get_named_enum_kind_id("optnone");
                    let attr = self.llvmctx.create_enum_attribute(id, 0);
                    func.add_attribute(inkwell::attributes::AttributeLoc::Function, attr);
                }
                OptionalFlag::NoSanitize(_kind) => {
                    // FIXME Didn't worked and I don't know why at the moment.
                    // let attr = self.llvmctx.create_string_attribute("no_sanitize", kind);
                    // func.add_attribute(inkwell::attributes::AttributeLoc::Function, attr);
                }
            }
        }
    }

    pub(crate) fn emit_func_body(&mut self, func_params: &CIRFuncParams, cir_block: &CIRBlockStmt) {
        self.emit_block("entry");
        self.emit_func_params(func_params);
        self.emit_body(cir_block);
        self.ensure_void_fn_terminated();
    }

    pub(crate) fn ensure_void_fn_terminated(&self) {
        let cur_fn = self.cur_fn.unwrap();
        if cur_fn.get_type().get_return_type().is_some() {
            return; // works only for void return type
        }

        let basic_block = cur_fn.get_last_basic_block().unwrap();
        if basic_block.get_terminator().is_some() {
            return;
        }

        self.llvmbuilder.build_return(None).unwrap();
    }
}
