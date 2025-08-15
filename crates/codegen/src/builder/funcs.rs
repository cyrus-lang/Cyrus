use super::module::CodeGenBuilder;
use ast::{AccessSpecifier, SelfModifierKind};
use inkwell::{
    AddressSpace,
    attributes::{Attribute, AttributeLoc},
    llvm_sys::{
        core::LLVMFunctionType,
        prelude::{LLVMBool, LLVMTypeRef},
    },
    module::Linkage,
    types::{AsTypeRef, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType, StructType},
    values::FunctionValue,
};
use typed_ast::{TypedFuncDef, TypedFuncParamKind, TypedFuncParams, types::ConcreteType};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_func_def(&self, func_def: &TypedFuncDef) {
        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&func_def.symbol_id).unwrap();

        let fn_value = local_ir_value.as_func().unwrap();

        let entry_block = self.llvmctx.append_basic_block(*fn_value, "entry");
        self.llvmbuilder.position_at_end(entry_block);
        drop(irreg);
        
        self.build_block_statement(&func_def.body);
    }

    pub(crate) fn build_func_decl(
        &self,
        func_name: String,
        params: TypedFuncParams,
        return_type: ConcreteType,
        vis: AccessSpecifier,
        method_struct_type: Option<StructType<'a>>,
    ) -> FunctionValue<'a> {
        let linkage = self.build_func_linkage(vis);
        let fn_type = self.build_func_type(params, return_type, method_struct_type);

        let llvmmodule = self.llvmmodule.borrow();
        let fn_value = llvmmodule.add_function(&func_name, fn_type, Some(linkage));
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

    pub(crate) fn build_func_type(
        &self,
        params: TypedFuncParams,
        return_type: ConcreteType,
        method_struct_type: Option<StructType<'a>>,
    ) -> FunctionType<'a> {
        let param_types: Vec<BasicMetadataTypeEnum<'a>> = params
            .list
            .iter()
            .map(|param| {
                let basic_type_enum: BasicTypeEnum<'a> = match param {
                    TypedFuncParamKind::FuncParam(typed_func_param) => self
                        .build_concrete_type(None, typed_func_param.ty.clone())
                        .try_into()
                        .unwrap(),
                    TypedFuncParamKind::SelfModifier(self_modifier) => match self_modifier.kind {
                        SelfModifierKind::Copied => BasicTypeEnum::StructType(method_struct_type.unwrap()),
                        SelfModifierKind::Referenced => {
                            BasicTypeEnum::PointerType(self.llvmctx.ptr_type(AddressSpace::default()))
                        }
                    },
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
