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
use resolver::{scope::LocalScopeRef, sigs::GlobalVarSig};
use typed_ast::{TypedGlobalVarStmt, TypedVarStmt};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn get_or_declare_global_var(&mut self, global_var_sig: GlobalVarSig) -> GlobalValue<'a> {
        let sema_ty: BasicTypeEnum<'a> = self
            .build_concrete_type(None, global_var_sig.ty.unwrap())
            .try_into()
            .unwrap();
        let abi_name = make_global_var_abi_name(
            &self.get_module_name(global_var_sig.module_id),
            &global_var_sig.name,
            &global_var_sig.vis,
        );

        let llvmmodule = self.llvmmodule.borrow_mut();
        let global_value = match llvmmodule.get_global(&abi_name) {
            Some(global_value) => global_value,
            None => llvmmodule.add_global(sema_ty, None, &abi_name),
        };
        global_value.set_linkage(Linkage::External);
        drop(llvmmodule);

        global_value
    }

    pub(crate) fn build_global_var_decl(&mut self, global_var: &TypedGlobalVarStmt) -> GlobalValue<'a> {
        let linkage = self.build_global_variable_linkage(global_var.vis.clone());

        let mut global_var_type = {
            if let Some(sema_ty) = &global_var.ty {
                Some(self.build_concrete_type(None, sema_ty.clone()))
            } else {
                None
            }
        };

        if global_var_type.is_none() {
            dbg!(global_var.clone());
            let typed_expr = global_var.expr.clone().unwrap();
            global_var_type = Some(self.build_concrete_type(None, typed_expr.sema_ty.unwrap()));
        }

        let global_var_type: BasicTypeEnum<'a> = global_var_type.unwrap().try_into().unwrap();
        let abi_name = make_global_var_abi_name(
            &self.get_module_name(global_var.module_id),
            &global_var.name,
            &global_var.vis,
        );

        let llvmmodule = self.llvmmodule.borrow();
        let global_var_value = llvmmodule.add_global(global_var_type, None, &abi_name);
        global_var_value.set_linkage(linkage);
        global_var_value.set_unnamed_addr(true);
        drop(llvmmodule);

        global_var_value
    }

    pub(crate) fn build_global_var_def(&mut self, global_var: &TypedGlobalVarStmt) {
        let local_ir_value = self.get_ir_value(global_var.symbol_id).unwrap();
        let global_value = local_ir_value.as_global_value().unwrap().0.clone();

        if let Some(sema_ty) = &global_var.ty {
            global_value.set_constant(sema_ty.is_const());
        }

        if global_var.expr.is_some() {
            let lvalue = self.build_expr(None, global_var.expr.as_ref().unwrap());
            let rvalue = self.build_load_lvalue_to_rvalue(None, lvalue);

            let basic_rvalue = rvalue.as_basic_value();
            global_value.set_initializer(&basic_rvalue);
        } else {
            let basic_type: BasicTypeEnum<'a> = self
                .build_concrete_type(None, global_var.ty.clone().unwrap())
                .try_into()
                .unwrap();

            let zero_init_value = self.build_zero_init_value(basic_type);
            global_value.set_initializer(&zero_init_value);
        }

        self.insert_ir_value(
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
        variable: &TypedVarStmt,
        zero_init_by_default: bool,
    ) -> PointerValue<'a> {
        let rhs_value_opt = variable.rhs.as_ref().map(|rhs_expr| {
            let lvalue_r = self.build_expr(local_scope_opt.clone(), rhs_expr);
            self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue_r)
        });

        let llvm_alloca_type: BasicTypeEnum<'a> = if let Some(ref rvalue) = rhs_value_opt {
            rvalue.as_basic_value().get_type()
        } else {
            let var_type = variable
                .ty
                .clone()
                .expect("Cannot infer type without RHS or explicit type");

            self.build_concrete_type(local_scope_opt.clone(), var_type)
                .try_into()
                .unwrap()
        };

        let alloca = self.llvmbuilder.build_alloca(llvm_alloca_type, &variable.name).unwrap();

        let initial_concrete_type = variable
            .ty
            .clone()
            .or_else(|| variable.rhs.as_ref().unwrap().sema_ty.clone())
            .unwrap();

        self.insert_ir_value(
            variable.symbol_id,
            LocalIRValue::LValue(alloca, initial_concrete_type.clone()),
        );

        if zero_init_by_default && rhs_value_opt.is_none() {
            let zero_init_value = self.build_zero_init_value(llvm_alloca_type);
            self.llvmbuilder.build_store(alloca, zero_init_value).unwrap();
        }

        if let Some(mut rvalue) = rhs_value_opt {
            if let Some(expected_ty) = &variable.ty {
                rvalue = self.build_implicit_cast(local_scope_opt.clone(), expected_ty.clone(), rvalue);
            }

            let final_basic = rvalue.as_basic_value();
            self.llvmbuilder.build_store(alloca, final_basic).unwrap();
        }

        alloca
    }
}
