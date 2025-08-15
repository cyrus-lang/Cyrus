use crate::builder::module::{CodeGenBuilder, LocalIRValue};
use ast::AccessSpecifier;
use inkwell::{
    module::Linkage,
    types::{AnyType, BasicTypeEnum},
    values::{BasicValueEnum, GlobalValue},
};
use resolver::scope::LocalScopeRef;
use typed_ast::{TypedExpression, TypedGlobalVariable, TypedVariable, types::ConcreteType};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_global_var_decl(&self, global_var: &TypedGlobalVariable) -> GlobalValue<'a> {
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

        let llvmmodule = self.llvmmodule.borrow();
        let global_var_value = llvmmodule.add_global(global_var_type, None, &global_var.name);
        global_var_value.set_linkage(linkage);
        drop(llvmmodule);
        global_var_value
    }

    pub(crate) fn build_global_var_def(&self, global_var: &TypedGlobalVariable) {
        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&global_var.symbol_id).unwrap();
        let global_value = local_ir_value.as_global_value().cloned().unwrap();
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

        let mut irreg = self.irreg.borrow_mut();
        irreg.insert(global_var.symbol_id, LocalIRValue::GlobalValue(global_value));
        drop(irreg);
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

    pub(crate) fn build_local_variable(&self, local_scope_opt: Option<LocalScopeRef>, variable: &TypedVariable) {
        let (basic_type, concrete_type): (BasicTypeEnum<'a>, ConcreteType) = {
            if let Some(concrete_type) = &variable.ty {
                (
                    self.build_concrete_type(local_scope_opt.clone(), concrete_type.clone())
                        .try_into()
                        .unwrap(),
                    concrete_type.clone(),
                )
            } else {
                let concrete_type = variable.rhs.clone().unwrap().concrete_type.unwrap();
                (
                    self.build_concrete_type(local_scope_opt.clone(), concrete_type.clone())
                        .try_into()
                        .unwrap(),
                    concrete_type,
                )
            }
        };

        let alloca = self.llvmbuilder.build_alloca(basic_type, &variable.name).unwrap();

        let mut irreg = self.irreg.borrow_mut();
        irreg.insert(variable.symbol_id, LocalIRValue::LValue(alloca, concrete_type));
        drop(irreg);

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
    }
}
