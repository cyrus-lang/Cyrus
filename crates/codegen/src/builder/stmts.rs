use super::module::{CodeGenBuilder, LocalIRValue};
use crate::{
    builder::values::{InternalValue, InternalValueKind},
    llvm_set_current_location,
};
use inkwell::debug_info::AsDIScope;
use inkwell::{types::BasicTypeEnum, values::BasicValueEnum};
use resolver::{scope::LocalScopeRef, typed_struct_as_struct_sig};
use typed_ast::{TypedBlockStatement, TypedExportTupleValues, TypedStatement, TypedStruct};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_toplevel_statements(&mut self, stmts: &Vec<TypedStatement>) {
        self.build_forward_decls(stmts);

        for stmt in stmts {
            llvm_set_current_location!(&self, stmt.get_loc());

            match stmt {
                TypedStatement::GlobalVariable(typed_global_var) => self.build_global_var_def(typed_global_var),
                TypedStatement::FuncDef(typed_func_def) => self.build_func_def(typed_func_def),
                TypedStatement::Enum(typed_enum) => self.build_enum_def(typed_enum),
                TypedStatement::Union(typed_union) => self.build_union_def(typed_union),
                TypedStatement::Struct(typed_struct) => self.build_struct_def(typed_struct),
                TypedStatement::Interface(..) | TypedStatement::FuncDecl(..) => {}
                _ => continue,
            }
        }
    }

    pub(crate) fn build_block_statement(&mut self, block_stmt: &TypedBlockStatement) {
        let local_scope_opt = Some(
            self.resolver
                .get_scope_ref(self.module_id, block_stmt.scope_id)
                .unwrap(),
        );

        for stmt in &block_stmt.exprs {
            llvm_set_current_location!(&self, stmt.get_loc());
            self.build_statement(local_scope_opt.clone(), stmt);
        }

        for defer in block_stmt.defers.iter().rev() {
            self.build_statement(local_scope_opt.clone(), &defer.operand);
        }
    }

    pub(crate) fn build_statement(&mut self, local_scope_opt: Option<LocalScopeRef>, stmt: &TypedStatement) {
        match stmt {
            TypedStatement::Defer(..) => unreachable!(),
            TypedStatement::Variable(typed_variable) => {
                self.build_local_variable(local_scope_opt.clone(), typed_variable, true);
            }
            TypedStatement::If(typed_if) => self.build_if(local_scope_opt.clone(), typed_if),
            TypedStatement::For(typed_for) => self.build_for(local_scope_opt.clone(), typed_for),
            TypedStatement::While(typed_while) => self.build_while(local_scope_opt.clone(), typed_while),
            TypedStatement::Return(typed_return) => self.build_return(local_scope_opt.clone(), typed_return),
            TypedStatement::Break(typed_break) => self.build_break(typed_break),
            TypedStatement::Continue(typed_continue) => self.build_continue(typed_continue),
            TypedStatement::Switch(typed_switch) => self.build_switch(local_scope_opt.clone(), typed_switch),
            TypedStatement::Struct(typed_struct) => {
                self.get_or_declare_struct(typed_struct.symbol_id, &typed_struct_as_struct_sig(typed_struct));
            }
            TypedStatement::Enum(typed_enum) => self.build_enum_def(typed_enum),
            TypedStatement::Union(typed_union) => self.build_union_def(typed_union),
            TypedStatement::Expression(typed_expr) => {
                self.build_expr(local_scope_opt.clone(), typed_expr);
            }
            TypedStatement::BlockStatement(typed_block_statement) => {
                self.build_block_statement(typed_block_statement);
            }
            TypedStatement::ExportTupleValues(export_tuple_values) => {
                self.build_export_tuple_values(local_scope_opt, export_tuple_values);
            }
            // Skipped statements
            TypedStatement::Interface(..) => {}
            TypedStatement::Typedef(_) => {}
            // Invalid statements
            TypedStatement::FuncDef(_) | TypedStatement::FuncDecl(_) | TypedStatement::GlobalVariable(_) => {
                unreachable!()
            }
        }
    }

    fn build_struct_def(&mut self, typed_struct: &TypedStruct) {
        if typed_struct.generic_params.is_none() {
            self.get_or_declare_struct(typed_struct.symbol_id, &typed_struct_as_struct_sig(typed_struct));
            self.build_methods(typed_struct.module_id, &typed_struct.methods);
        } else {
            // generic struct is generated at use time
        }
    }

    fn build_export_tuple_values(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        export_tuple_values: &TypedExportTupleValues,
    ) {
        let tuple_concrete_type = export_tuple_values
            .ty
            .clone()
            .or_else(|| {
                export_tuple_values
                    .rhs
                    .clone()
                    .and_then(|typed_expr| typed_expr.concrete_type)
            })
            .expect("Type must either be explicitly provided or inferred from RHS");

        let tuple_type = tuple_concrete_type.as_tuple_type().unwrap();

        for (idx, symbol_id) in export_tuple_values.exports.iter().enumerate() {
            let element_type = tuple_type.type_list.get(idx).unwrap();
            let element_basic_type: BasicTypeEnum<'a> = self
                .build_concrete_type(local_scope_opt.clone(), element_type.clone())
                .try_into()
                .unwrap();

            let element_pointer = self
                .llvmbuilder
                .build_alloca(element_basic_type, "tuple_extracted_value")
                .unwrap();

            let element_basic_value: BasicValueEnum<'a>;

            if let Some(typed_expr) = &export_tuple_values.rhs {
                let lvalue = self.build_expr(local_scope_opt.clone(), typed_expr);
                let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);

                let tuple_struct_value = rvalue.as_basic_value().into_struct_value();

                let basic_value = self
                    .llvmbuilder
                    .build_extract_value(tuple_struct_value, idx.try_into().unwrap(), "extractvalue")
                    .unwrap();

                element_basic_value = self
                    .build_implicit_cast(
                        local_scope_opt.clone(),
                        element_type.clone(),
                        InternalValue::new(element_type.clone(), InternalValueKind::RValue(basic_value)),
                    )
                    .as_basic_value();
            } else {
                element_basic_value = self.build_zero_init_value(element_basic_type);
            }

            self.llvmbuilder
                .build_store(element_pointer, element_basic_value)
                .unwrap();

            self.insert_ir_value(*symbol_id, LocalIRValue::LValue(element_pointer, element_type.clone()));
        }
    }
}
