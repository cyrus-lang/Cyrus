use super::module::{CodeGenBuilder, LocalIRValue};
use ast::AccessSpecifier;
use inkwell::{
    module::Linkage,
    types::{BasicTypeEnum, StructType},
    values::GlobalValue,
};
use typed_ast::{SymbolID, TypedExpression, TypedFuncDef, TypedGlobalVariable, TypedStatement, TypedStruct};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_toplevel_statements(&self, stmts: &Vec<TypedStatement>) {
        self.build_forward_decls(stmts);

        for stmt in stmts {
            match stmt {
                TypedStatement::FuncDef(typed_func_def) => self.build_func_def(typed_func_def),
                TypedStatement::Struct(typed_struct) => self.build_struct_def(typed_struct),
                TypedStatement::Enum(typed_enum) => todo!(),
                TypedStatement::Interface(typed_interface) => todo!(),
                // already handled in build_forward_decls
                TypedStatement::GlobalVariable(_) => continue,
                TypedStatement::FuncDecl(_) => continue,
                _ => continue,
            }
        }
    }

    fn build_forward_decls(&self, stmts: &Vec<TypedStatement>) {
        let insert_forward_decl_to_registry = |symbol_id: SymbolID, local_value: LocalIRValue<'a>| {
            let mut irreg = self.irreg.borrow_mut();
            irreg.insert(symbol_id, local_value);
            drop(irreg);
        };

        for stmt in stmts {
            match stmt {
                TypedStatement::Struct(typed_struct) => {
                    let struct_type = self.build_struct_decl(&typed_struct.name);
                    insert_forward_decl_to_registry(typed_struct.symbol_id, LocalIRValue::Struct(struct_type));
                }
                TypedStatement::Enum(typed_enum) => {
                    let struct_type = self.build_enum_decl(&typed_enum.name);
                    insert_forward_decl_to_registry(typed_enum.symbol_id, LocalIRValue::Struct(struct_type));
                }
                TypedStatement::Interface(typed_interface) => todo!(),
                _ => continue,
            }
        }

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
                _ => continue,
            }
        }
    }

    fn build_struct_decl(&self, name: &String) -> StructType<'a> {
        self.llvmctx.opaque_struct_type(name)
    }

    fn build_struct_def(&self, typed_struct: &TypedStruct) {
        let field_types: Vec<BasicTypeEnum<'a>> = typed_struct
            .fields
            .iter()
            .map(|field| self.build_concrete_type(None, field.ty.clone()).try_into().unwrap())
            .collect();

        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&typed_struct.symbol_id).unwrap();

        let struct_type = local_ir_value.as_struct().unwrap();
        struct_type.set_body(&field_types, typed_struct.packed);
        drop(irreg);
    }

    fn build_enum_decl(&self, name: &String) -> StructType<'a> {
        self.llvmctx.opaque_struct_type(name)
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

    fn build_func_def(&self, func_def: &TypedFuncDef) {
        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&func_def.symbol_id).unwrap();

        let fn_value = local_ir_value.as_func().unwrap();

        let entry_block = self.llvmctx.append_basic_block(*fn_value, "entry");
        self.llvmbuilder.position_at_end(entry_block);

        // TODO build body block

        drop(irreg);
    }
}
