use super::module::{CodeGenBuilder, LocalIRValue};
use ast::AccessSpecifier;
use inkwell::{module::Linkage, types::BasicTypeEnum, values::GlobalValue};
use typed_ast::{SymbolID, TypedFuncDecl, TypedGlobalVariable, TypedStatement};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_toplevel_statements(&self, stmts: &Vec<TypedStatement>) {
        self.build_forward_decls(stmts);

        // for stmt in stmts {
        //     match stmt {
        //         TypedStatement::GlobalVariable(typed_global_var) => todo!(),
        //         TypedStatement::FuncDef(typed_func_def) => todo!(),
        //         TypedStatement::FuncDecl(typed_func_decl) => {
        //             self.build_func_decl_stmt(typed_func_decl);
        //         }
        //         TypedStatement::Struct(typed_struct) => todo!(),
        //         TypedStatement::Enum(typed_enum) => todo!(),
        //         TypedStatement::Interface(typed_interface) => todo!(),
        //         _ => continue,
        //     }
        // }
    }

    fn build_forward_decls(&self, stmts: &Vec<TypedStatement>) {
        let insert_forward_decl_to_registry = |symbol_id: SymbolID, local_value: LocalIRValue<'a>| {
            let mut irreg = self.irreg.borrow_mut();
            irreg.insert(symbol_id, local_value);
            drop(irreg);
        };

        for stmt in stmts {
            match stmt {
                TypedStatement::GlobalVariable(typed_global_var) => {
                    let global_value = self.build_global_var_decl(typed_global_var);
                    insert_forward_decl_to_registry(
                        typed_global_var.symbol_id,
                        LocalIRValue::GlobalValue(global_value),
                    );
                }
                TypedStatement::FuncDef(typed_func_def) => {
                    let fn_value = self.build_func_decl(
                        typed_func_def.name.clone(),
                        typed_func_def.params.clone(),
                        typed_func_def.return_type.clone(),
                        typed_func_def.vis.clone(),
                        None,
                    );

                    insert_forward_decl_to_registry(typed_func_def.symbol_id, LocalIRValue::Func(fn_value));
                }
                TypedStatement::FuncDecl(typed_func_decl) => {
                    let fn_value = self.build_func_decl(
                        typed_func_decl.name.clone(),
                        typed_func_decl.params.clone(),
                        typed_func_decl.return_type.clone(),
                        typed_func_decl.vis.clone(),
                        None,
                    );

                    insert_forward_decl_to_registry(typed_func_decl.symbol_id, LocalIRValue::Func(fn_value));
                }
                TypedStatement::Struct(typed_struct) => todo!(),
                TypedStatement::Enum(typed_enum) => todo!(),
                TypedStatement::Interface(typed_interface) => todo!(),
                _ => continue,
            }
        }
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

    fn build_global_var_decl(&self, global_var: &TypedGlobalVariable) -> GlobalValue<'a> {
        let linkage = self.build_global_variable_linkage(global_var.vis.clone());

        let mut global_var_type = {
            if let Some(concrete_type) = &global_var.ty {
                Some(self.build_conrete_type(concrete_type.clone()))
            } else {
                None
            }
        };

        if global_var_type.is_none() {
            // global_var.expr.unwrap()
            // global_var_type = Some(self.build_concrete_type());
            todo!();
        }

        let global_var_type: BasicTypeEnum<'a> = global_var_type.unwrap().try_into().unwrap();

        let llvmmodule = self.llvmmodule.borrow();
        let global_var_value = llvmmodule.add_global(global_var_type, None, &global_var.name);
        global_var_value.set_linkage(linkage);
        drop(llvmmodule);
        global_var_value
    }

    fn build_func_def_stmt(&self, func_decl: &TypedFuncDecl) {
        todo!()
    }
}
