use crate::builder::module::{CodeGenBuilder, LocalIRValue};
use resolver::{
    typed_enum_as_enum_sig, typed_func_decl_as_func_sig, typed_func_def_as_func_sig, typed_func_type_from_func_sig,
    typed_struct_as_struct_sig, typed_union_as_union_sig,
};
use tast::{TypedEnumStmt, TypedStmt, TypedStructStmt, TypedUnionStmt, types::SemanticType};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_forward_decls(&mut self, stmts: &Vec<TypedStmt>) {
        for stmt in stmts {
            match stmt {
                TypedStmt::Struct(typed_struct) => {
                    self.build_struct_forward_decl(&typed_struct);
                }
                TypedStmt::Enum(typed_enum) => {
                    self.build_enum_forward_decl(&typed_enum);
                }
                TypedStmt::Union(typed_union) => {
                    self.build_union_forward_decl(&typed_union);
                }
                TypedStmt::Interface(..) => continue,
                _ => continue,
            }
        }

        for stmt in stmts {
            match stmt {
                TypedStmt::GlobalVariable(typed_global_var) => {
                    let global_value = self.build_global_var_decl(typed_global_var);
                    self.insert_ir_value(
                        typed_global_var.symbol_id,
                        LocalIRValue::GlobalValue(global_value, typed_global_var.ty.clone().unwrap()),
                    );
                }
                TypedStmt::FuncDef(typed_func_def) => {
                    let fn_value = self.build_func_decl(
                        typed_func_def.name.clone(),
                        false,
                        Some(typed_func_def.module_id),
                        typed_func_def.params.clone(),
                        typed_func_def.return_type.clone(),
                        typed_func_def.vis.clone(),
                    );
                    self.insert_ir_value(
                        typed_func_def.symbol_id,
                        LocalIRValue::Func(
                            fn_value,
                            SemanticType::FuncType(typed_func_type_from_func_sig(&typed_func_def_as_func_sig(
                                typed_func_def,
                            ))),
                        ),
                    );
                }
                TypedStmt::FuncDecl(typed_func_decl) => {
                    let fn_value = self.build_func_decl(
                        typed_func_decl.name.clone(),
                        true,
                        None,
                        typed_func_decl.params.clone(),
                        typed_func_decl.return_type.clone(),
                        typed_func_decl.vis.clone(),
                    );
                    self.insert_ir_value(
                        typed_func_decl.symbol_id,
                        LocalIRValue::Func(
                            fn_value,
                            SemanticType::FuncType(typed_func_type_from_func_sig(&typed_func_decl_as_func_sig(
                                typed_func_decl,
                            ))),
                        ),
                    );
                }
                _ => continue,
            }
        }
    }

    fn build_enum_forward_decl(&mut self, typed_enum: &TypedEnumStmt) {
        if typed_enum.generic_params.is_none() {
            self.get_or_declare_enum(typed_enum.symbol_id, &typed_enum_as_enum_sig(typed_enum));
        } else {
            // generic enum are generated at use site
        }
    }

    fn build_union_forward_decl(&mut self, typed_union: &TypedUnionStmt) {
        if typed_union.generic_params.is_none() {
            self.get_or_declare_union(typed_union.symbol_id, &typed_union_as_union_sig(typed_union));
        } else {
            // generic union are generated at use site
        }
    }

    fn build_struct_forward_decl(&mut self, typed_struct: &TypedStructStmt) {
        if typed_struct.generic_params.is_none() {
            self.get_or_declare_struct(typed_struct.symbol_id, &typed_struct_as_struct_sig(typed_struct));
        } else {
            // generic structs are generated at use site
        }
    }
}
