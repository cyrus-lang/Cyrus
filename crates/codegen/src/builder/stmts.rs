use super::module::{CodeGenBuilder, LocalIRValue};
use inkwell::types::{BasicTypeEnum, StructType};
use typed_ast::{SymbolID, TypedBlockStatement, TypedStatement, TypedStruct};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_toplevel_statements(&self, stmts: &Vec<TypedStatement>) {
        self.build_forward_decls(stmts);

        for stmt in stmts {
            match stmt {
                TypedStatement::FuncDef(typed_func_def) => self.build_func_def(typed_func_def),
                TypedStatement::Struct(typed_struct) => self.build_struct_def(typed_struct),
                TypedStatement::Enum(typed_enum) => self.build_enum_def(typed_enum),
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

    pub(crate) fn build_block_statement(&self, block_stmt: &TypedBlockStatement) {
        let local_scope_opt = Some(
            self.resolver
                .get_scope_ref(self.module_id, block_stmt.scope_id)
                .unwrap(),
        );

        for stmt in &block_stmt.exprs {
            match stmt {
                TypedStatement::Variable(typed_variable) => {
                    self.build_local_variable(local_scope_opt.clone(), typed_variable)
                }
                TypedStatement::If(typed_if) => todo!(),
                TypedStatement::Return(typed_return) => todo!(),
                TypedStatement::Break(typed_break) => todo!(),
                TypedStatement::Continue(typed_continue) => todo!(),
                TypedStatement::For(typed_for) => todo!(),
                TypedStatement::Foreach(typed_foreach) => todo!(),
                TypedStatement::Switch(typed_switch) => todo!(),
                TypedStatement::Struct(typed_struct) => todo!(),
                TypedStatement::Enum(typed_enum) => todo!(),
                TypedStatement::Expression(typed_expr) => {
                    self.build_expr(local_scope_opt.clone(), typed_expr);
                }
                TypedStatement::BlockStatement(typed_block_statement) => {
                    self.build_block_statement(typed_block_statement);
                }
                TypedStatement::Interface(typed_interface) => todo!(),
                // Skipped statements
                TypedStatement::Typedef(_) => continue,
                // Invalid statements
                TypedStatement::FuncDef(_) => unreachable!(),
                TypedStatement::FuncDecl(_) => unreachable!(),
                TypedStatement::Import(_) => unreachable!(),
                TypedStatement::GlobalVariable(_) => unreachable!(),
            }
        }
    }
}
