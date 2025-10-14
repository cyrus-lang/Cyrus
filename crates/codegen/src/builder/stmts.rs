use super::module::{CodeGenBuilder, LocalIRValue};
use crate::{
    builder::{
        abi::generate_union_abi_name,
        values::{InternalValue, InternalValueKind},
    },
    llvm_set_current_location,
};

use inkwell::debug_info::AsDIScope;
use inkwell::{
    types::{AnyType, BasicTypeEnum, StructType},
    values::BasicValueEnum,
};
use resolver::{scope::LocalScopeRef, signatures::UnionSig, typed_func_type_from_func_sig, typed_struct_as_struct_sig};
use std::collections::HashMap;
use typed_ast::{
    ModuleID, SymbolID, TypedBlockStatement, TypedExportTupleValues, TypedFuncVariadicParams, TypedStatement, TypedStruct, TypedUnion, types::ConcreteType
};

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
                TypedStatement::Interface(..) => continue,
                TypedStatement::FuncDecl(..) => continue,
                _ => continue,
            }
        }
    }

    pub(crate) fn build_methods(&mut self, module_id: ModuleID, methods: &HashMap<String, SymbolID>) {
        for method_symbol_id in methods.values() {
            let symbol_entry = self
                .resolver
                .lookup_symbol_entry_with_id(module_id, *method_symbol_id)
                .unwrap();

            let resolved_method = symbol_entry.as_method().unwrap().clone();

            let fn_value = self.get_or_declare_func(*method_symbol_id, resolved_method.func_sig.clone());

            self.insert_ir_value(
                *method_symbol_id,
                LocalIRValue::Func(
                    fn_value,
                    ConcreteType::FuncType(typed_func_type_from_func_sig(&resolved_method.func_sig)),
                ),
            );

            self.blockreg.current_func_ref = Some(fn_value.clone());

            let entry_block = self.llvmctx.append_basic_block(fn_value, "entry");
            self.blockreg.current_block_ref = Some(entry_block);
            self.llvmbuilder.position_at_end(entry_block);

            let local_scope_opt = self
                .resolver
                .get_scope_ref(module_id, resolved_method.func_body.clone().unwrap().scope_id);

            self.build_func_params(local_scope_opt, &resolved_method.func_sig.params, fn_value);

            if let Some(variadic_params) = &resolved_method.func_sig.params.variadic {
                if let TypedFuncVariadicParams::Typed(_, _) = variadic_params {
                    todo!();
                }
            }

            self.build_block_statement(&resolved_method.func_body.clone().unwrap());

            let current_block = self.blockreg.current_block_ref.unwrap();
            if !self.is_block_terminated(current_block) {
                self.llvmbuilder.build_return(None).unwrap();
            }
        }
    }

    pub(crate) fn build_union_struct_type(&mut self, union_sig: &UnionSig) -> StructType<'a> {
        let llvm_struct_name = generate_union_abi_name(&self.get_module_name(self.module_id), &union_sig.name.clone());
        let union_opaque_struct = self.llvmctx.opaque_struct_type(&llvm_struct_name);

        let field_types: Vec<BasicTypeEnum<'a>> = union_sig
            .fields
            .iter()
            .map(|field| self.build_concrete_type(None, field.ty.clone()).try_into().unwrap())
            .collect();

        let mut largest_field_type: BasicTypeEnum = BasicTypeEnum::IntType(self.llvmctx.bool_type());

        field_types.iter().for_each(|basic_type| {
            let largest_store_size = self
                .llvmtm
                .get_target_data()
                .get_store_size(&largest_field_type.as_any_type_enum());

            let field_store_size = self
                .llvmtm
                .get_target_data()
                .get_store_size(&basic_type.as_any_type_enum());

            if field_store_size > largest_store_size {
                largest_field_type = basic_type.clone();
            }
        });

        union_opaque_struct.set_body(&[largest_field_type], true);
        union_opaque_struct
    }

    pub(crate) fn build_union_def(&mut self, typed_union: &TypedUnion) {
        let union_struct_type = self.build_union_struct_type(&typed_union_as_union_sig(typed_union));

        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&typed_union.symbol_id).unwrap();
        let struct_type = local_ir_value.as_struct().unwrap().clone();
        drop(irreg);
        struct_type.set_body(&union_struct_type.get_field_types(), true);
        self.build_methods(typed_union.module_id, &typed_union.methods);

        self.build_methods(typed_union.module_id, &typed_union.methods);
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
            TypedStatement::Enum(typed_enum) => self.build_local_enum_def(typed_enum),
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

fn typed_union_as_union_sig(typed_union: &TypedUnion) -> UnionSig {
    UnionSig {
        symbol_id: typed_union.symbol_id,
        name: typed_union.name.clone(),
        fields: typed_union.fields.clone(),
        methods: typed_union.methods.clone(),
        vis: typed_union.vis.clone(),
        loc: typed_union.loc.clone(),
    }
}
