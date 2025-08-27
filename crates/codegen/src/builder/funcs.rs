use super::module::CodeGenBuilder;
use crate::builder::{abi::make_func_abi_name, module::LocalIRValue};
use ast::AccessSpecifier;
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    llvm_sys::{
        core::LLVMFunctionType,
        prelude::{LLVMBool, LLVMTypeRef},
    },
    module::Linkage,
    types::{AsTypeRef, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
    values::FunctionValue,
};
use resolver::scope::LocalScopeRef;
use typed_ast::{
    ModuleID, TypedFuncDef, TypedFuncParamKind, TypedFuncParams, TypedFuncVariadicParams, TypedVariable,
    types::ConcreteType,
};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_func_params(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        params: &TypedFuncParams,
        fn_value: FunctionValue<'a>,
    ) {
        params.list.iter().enumerate().for_each(|(param_idx, param_kind)| {
            let (name, concrete_type, loc) = match param_kind {
                TypedFuncParamKind::FuncParam(typed_func_param) => (
                    typed_func_param.name.clone(),
                    typed_func_param.ty.clone(),
                    typed_func_param.loc.clone(),
                ),
                TypedFuncParamKind::SelfModifier(typed_self_modifier) => (
                    "self".to_string(),
                    typed_self_modifier.ty.clone().unwrap(),
                    typed_self_modifier.loc.clone(),
                ),
            };

            let local_scope_rc = local_scope_opt.clone().unwrap();
            let local_scope = local_scope_rc.borrow();
            let local_param_symbol_id = local_scope.resolve(&name).unwrap().get_symbol_id();
            drop(local_scope);

            let lvalue_pointer = self.build_local_variable(
                local_scope_opt.clone(),
                &TypedVariable {
                    symbol_id: local_param_symbol_id,
                    name: name.clone(),
                    ty: Some(concrete_type.clone()),
                    rhs: None,
                    loc,
                },
            );

            let basic_value = fn_value.get_nth_param(param_idx.try_into().unwrap()).unwrap();
            self.llvmbuilder.build_store(lvalue_pointer, basic_value).unwrap();

            self.insert_forward_decl_to_registry(
                local_param_symbol_id,
                LocalIRValue::LValue(lvalue_pointer, concrete_type),
            );
        })
    }

    pub(crate) fn build_func_def(&mut self, func_def: &TypedFuncDef) {
        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, func_def.body.scope_id);
        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&func_def.symbol_id).unwrap();

        let fn_value = local_ir_value.as_func().unwrap().clone();
        self.blockreg.current_func_ref = Some(fn_value.clone());

        let entry_block = self.llvmctx.append_basic_block(fn_value, "entry");
        self.blockreg.current_block_ref = Some(entry_block);
        self.llvmbuilder.position_at_end(entry_block);
        drop(irreg);

        self.build_func_params(local_scope_opt, &func_def.params, fn_value);

        if let Some(variadic_params) = &func_def.params.variadic {
            if let TypedFuncVariadicParams::Typed(_, _) = variadic_params {
                todo!();
            }
        }

        self.build_block_statement(&func_def.body);

        let current_block = self.blockreg.current_block_ref.unwrap();
        if !self.is_block_terminated(current_block) {
            self.llvmbuilder.build_return(None).unwrap();
        }
    }

    pub(crate) fn build_func_decl(
        &mut self,
        func_name: String,
        use_func_real_name: bool,
        module_id: Option<ModuleID>,
        params: TypedFuncParams,
        return_type: ConcreteType,
        vis: AccessSpecifier,
    ) -> FunctionValue<'a> {
        let linkage = {
            if func_name == "main" {
                Linkage::External
            } else {
                self.build_func_linkage(vis)
            }
        };

        let fn_type = self.build_func_type(params, return_type);

        let func_abi_name = {
            if func_name == "main" || use_func_real_name {
                func_name
            } else {
                let module_name = self.get_module_name(module_id.unwrap());
                make_func_abi_name(module_name, func_name)
            }
        };

        let llvmmodule = self.llvmmodule.borrow();
        let fn_value = llvmmodule.add_function(&func_abi_name, fn_type, Some(linkage));
        self.add_func_attrs(fn_value);
        drop(llvmmodule);
        fn_value
    }

    pub(crate) fn add_func_attrs(&self, func: FunctionValue) {
        let attr_kind_id = |kind: &str| Attribute::get_named_enum_kind_id(kind);

        func.add_attribute(
            AttributeLoc::Function,
            self.llvmctx.create_enum_attribute(attr_kind_id("uwtable"), 2),
        );
        func.add_attribute(
            AttributeLoc::Function,
            self.llvmctx.create_enum_attribute(attr_kind_id("ssp"), 0),
        );
        func.add_attribute(
            AttributeLoc::Function,
            self.llvmctx.create_enum_attribute(attr_kind_id("nounwind"), 0),
        );

        func.add_attribute(
            AttributeLoc::Function,
            self.llvmctx.create_string_attribute("frame-pointer", "all"),
        );
        func.add_attribute(
            AttributeLoc::Function,
            self.llvmctx.create_string_attribute("no-trapping-math", "true"),
        );
        func.add_attribute(
            AttributeLoc::Function,
            self.llvmctx.create_string_attribute("stack-protector-buffer-size", "8"),
        );
    }

    pub(crate) fn build_func_linkage(&self, vis: AccessSpecifier) -> Linkage {
        match vis {
            AccessSpecifier::Extern => Linkage::External,
            AccessSpecifier::Public => Linkage::External,
            AccessSpecifier::Internal => Linkage::Private,
            AccessSpecifier::Inline => Linkage::Internal,
            AccessSpecifier::PublicInline => Linkage::LinkOnceODR,
            AccessSpecifier::PublicExtern => Linkage::External,
        }
    }

    pub(crate) fn build_func_type(&mut self, params: TypedFuncParams, return_type: ConcreteType) -> FunctionType<'a> {
        let param_types: Vec<BasicMetadataTypeEnum<'a>> = params
            .list
            .iter()
            .map(|param| {
                let basic_type_enum: BasicTypeEnum<'a> = match param {
                    TypedFuncParamKind::FuncParam(typed_func_param) => self
                        .build_concrete_type(None, typed_func_param.ty.clone())
                        .try_into()
                        .unwrap(),
                    TypedFuncParamKind::SelfModifier(self_modifier) => {
                        let concrete_type =
                            self.build_concrete_type_from_symbol_id(None, self_modifier.symbol_id.unwrap());
                        concrete_type.try_into().unwrap()
                    }
                };
                BasicMetadataTypeEnum::from(basic_type_enum.clone())
            })
            .collect();

        let return_type = self.build_concrete_type(None, return_type);

        let is_var_args = params.variadic.is_some();
        let fn_type = unsafe {
            let mut param_types = param_types
                .iter()
                .map(|ty| ty.as_type_ref())
                .collect::<Vec<LLVMTypeRef>>();

            LLVMFunctionType(
                return_type.as_type_ref(),
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                is_var_args as LLVMBool,
            )
        };
        unsafe { FunctionType::new(fn_type) }
    }
}
