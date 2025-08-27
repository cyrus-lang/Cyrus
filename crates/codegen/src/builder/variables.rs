use crate::builder::{
    abi::make_global_var_abi_name,
    module::{CodeGenBuilder, LocalIRValue},
};
use ast::AccessSpecifier;
use inkwell::{
    module::Linkage,
    types::BasicTypeEnum,
    values::{GlobalValue, PointerValue},
};
use resolver::{declsign::GlobalVarSig, scope::LocalScopeRef};
use typed_ast::{TypedExpression, TypedGlobalVariable, TypedVariable, types::ConcreteType};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn get_or_declare_global_var(&mut self, global_var_sig: GlobalVarSig) -> GlobalValue<'a> {
        let concrete_type: BasicTypeEnum<'a> = self
            .build_concrete_type(None, global_var_sig.ty.unwrap())
            .try_into()
            .unwrap();
        let module_name = self.get_module_name(global_var_sig.module_id);
        let abi_name = make_global_var_abi_name(module_name, global_var_sig.name);

        let llvmmodule = self.llvmmodule.borrow_mut();
        let global_value = llvmmodule.add_global(concrete_type, None, &abi_name);
        global_value.set_linkage(Linkage::External);
        drop(llvmmodule);

        global_value
    }

    pub(crate) fn build_global_var_decl(&mut self, global_var: &TypedGlobalVariable) -> GlobalValue<'a> {
        let linkage = self.build_global_variable_linkage(global_var.vis.clone());

        let mut global_var_type = {
            if let Some(concrete_type) = &global_var.ty {
                Some(self.build_concrete_type(None, concrete_type.clone()))
            } else {
                None
            }
        };

        if global_var_type.is_none() {
            let typed_expr: TypedExpression = global_var.expr.clone().unwrap();
            global_var_type = Some(self.build_concrete_type(None, typed_expr.concrete_type.unwrap()));
        }

        let global_var_type: BasicTypeEnum<'a> = global_var_type.unwrap().try_into().unwrap();
        let module_name = self.get_module_name(global_var.module_id);
        let abi_name = make_global_var_abi_name(module_name, global_var.name.clone());

        let llvmmodule = self.llvmmodule.borrow();
        let global_var_value = llvmmodule.add_global(global_var_type, None, &abi_name);
        global_var_value.set_linkage(linkage);
        global_var_value.set_unnamed_addr(true);
        drop(llvmmodule);

        global_var_value
    }

    pub(crate) fn build_global_var_def(&mut self, global_var: &TypedGlobalVariable) {
        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&global_var.symbol_id).unwrap();
        let global_value = local_ir_value.as_global_value().unwrap().0.clone();
        drop(irreg);

        if global_var.expr.is_some() {
            let lvalue = self.build_expr(None, global_var.expr.as_ref().unwrap());
            let rvalue = self.build_load_lvalue_to_rvalue(None, lvalue);

            let basic_rvalue = rvalue.as_basic_value();
            global_value.set_initializer(&basic_rvalue);
        }

        if let Some(concrete_type) = &global_var.ty {
            global_value.set_constant(concrete_type.is_const());
        }

        self.insert_forward_decl_to_registry(
            global_var.symbol_id,
            LocalIRValue::GlobalValue(global_value, global_var.ty.clone().unwrap()),
        );
    }

    fn build_global_variable_linkage(&self, vis: AccessSpecifier) -> Linkage {
        match vis {
            AccessSpecifier::PublicExtern => Linkage::Common,
            AccessSpecifier::Extern => Linkage::Common,
            AccessSpecifier::Public => Linkage::External,
            AccessSpecifier::Internal => Linkage::Private,
            AccessSpecifier::Inline => unreachable!(),
            AccessSpecifier::PublicInline => unreachable!(),
        }
    }

    pub(crate) fn build_local_variable(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        variable: &TypedVariable,
    ) -> PointerValue<'a> {
        let (basic_type, concrete_type): (BasicTypeEnum<'a>, ConcreteType) = {
            if let Some(concrete_type) = &variable.ty {
                (
                    self.build_concrete_type(local_scope_opt.clone(), concrete_type.clone())
                        .try_into()
                        .unwrap(),
                    concrete_type.clone(),
                )
            } else {
                match variable.rhs.clone().unwrap().concrete_type {
                    Some(concrete_type) => (
                        self.build_concrete_type(local_scope_opt.clone(), concrete_type.clone())
                            .try_into()
                            .unwrap(),
                        concrete_type,
                    ),
                    None => {
                        let concrete_type = variable.rhs.clone().unwrap().concrete_type.unwrap();
                        (
                            self.build_concrete_type(local_scope_opt.clone(), concrete_type.clone())
                                .try_into()
                                .unwrap(),
                            concrete_type,
                        )
                    }
                }
            }
        };

        let alloca = self.llvmbuilder.build_alloca(basic_type, &variable.name).unwrap();

        self.insert_forward_decl_to_registry(variable.symbol_id, LocalIRValue::LValue(alloca, concrete_type));

        if let Some(typed_expr) = &variable.rhs {
            let lvalue = self.build_expr(local_scope_opt.clone(), typed_expr);
            let mut rvalue = self.build_load_lvalue_to_rvalue(None, lvalue);

            // implicit cast
            if let Some(concrete_type) = &variable.ty {
                rvalue = self.build_implicit_cast(local_scope_opt, concrete_type.clone(), rvalue);
            }

            let basic_rvalue = rvalue.as_basic_value();
            self.llvmbuilder.build_store(alloca, basic_rvalue).unwrap();
        }

        alloca
    }
}
