use crate::builder::module::CodeGenBuilder;
use ast::AccessSpecifier;
use inkwell::{module::Linkage, types::BasicTypeEnum, values::{BasicValueEnum, GlobalValue}};
use resolver::scope::LocalScopeRef;
use typed_ast::{TypedExpression, TypedGlobalVariable, TypedVariable};

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
        let var_type: BasicTypeEnum<'a> = {
            if let Some(concrete_type) = &variable.ty {
                self.build_concrete_type(local_scope_opt.clone(), concrete_type.clone())
                    .try_into()
                    .unwrap()
            } else {
                self.build_concrete_type(
                    local_scope_opt.clone(),
                    variable.rhs.clone().unwrap().concrete_type.unwrap(),
                )
                .try_into()
                .unwrap()
            }
        };

        let alloca = self.llvmbuilder.build_alloca(var_type, &variable.name).unwrap();
        if let Some(typed_expr) = &variable.rhs {
            let rvalue: BasicValueEnum<'a> = self.build_expr(local_scope_opt, typed_expr).try_into().unwrap();
            // TODO
            // let rvalue = self.implicit_cast();
            self.llvmbuilder.build_store(alloca, rvalue).unwrap();
        }
    }
}
