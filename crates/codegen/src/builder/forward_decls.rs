use crate::builder::{
    abi::{generate_enum_abi_name, generate_struct_abi_name, generate_union_abi_name},
    module::{CodeGenBuilder, LocalIRValue},
};
use inkwell::{module::Linkage, types::StructType, values::FunctionValue};
use resolver::{
    signatures::{EnumSig, FuncSig, StructSig, UnionSig},
    typed_func_decl_as_func_sig, typed_func_def_as_func_sig, typed_func_type_from_func_sig,
};
use typed_ast::{SymbolID, TypedStatement, types::ConcreteType};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_forward_decls(&mut self, stmts: &Vec<TypedStatement>) {
        for stmt in stmts {
            match stmt {
                TypedStatement::Struct(typed_struct) => {
                    if typed_struct.generic_params.is_none() {
                        let struct_type = self.build_struct_decl(&typed_struct.name);
                        self.insert_ir_value(typed_struct.symbol_id, LocalIRValue::Struct(struct_type));
                    } else {
                        // generics are generated when instantiated
                    }
                }
                TypedStatement::Enum(typed_enum) => {
                    let struct_type = self.build_enum_decl(&typed_enum.name);
                    let payload_type = self.llvmctx.i32_type().array_type(0);
                    self.insert_ir_value(typed_enum.symbol_id, LocalIRValue::Enum((struct_type, payload_type)));
                }
                TypedStatement::Union(typed_union) => {
                    let struct_type = self.build_union_decl(&typed_union.name);
                    self.insert_ir_value(typed_union.symbol_id, LocalIRValue::Struct(struct_type));
                }
                TypedStatement::Interface(..) => continue,
                _ => continue,
            }
        }

        for stmt in stmts {
            match stmt {
                TypedStatement::GlobalVariable(typed_global_var) => {
                    let global_value = self.build_global_var_decl(typed_global_var);
                    self.insert_ir_value(
                        typed_global_var.symbol_id,
                        LocalIRValue::GlobalValue(global_value, typed_global_var.ty.clone().unwrap()),
                    );
                }
                TypedStatement::FuncDef(typed_func_def) => {
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
                            ConcreteType::FuncType(typed_func_type_from_func_sig(&typed_func_def_as_func_sig(
                                typed_func_def,
                            ))),
                        ),
                    );
                }
                TypedStatement::FuncDecl(typed_func_decl) => {
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
                            ConcreteType::FuncType(typed_func_type_from_func_sig(&typed_func_decl_as_func_sig(
                                typed_func_decl,
                            ))),
                        ),
                    );
                }
                _ => continue,
            }
        }
    }

    pub(crate) fn insert_ir_value(&mut self, symbol_id: SymbolID, local_value: LocalIRValue<'a>) {
        let mut irreg = self.irreg.borrow_mut();
        irreg.insert(symbol_id, local_value);
        drop(irreg);
    }

    pub(crate) fn get_ir_value(&mut self, symbol_id: SymbolID) -> Option<LocalIRValue<'a>> {
        let irreg = self.irreg.borrow();
        let opt = irreg.get(&symbol_id).cloned();
        drop(irreg);
        opt
    }

    pub(crate) fn get_or_declare_func(&mut self, symbol_id: SymbolID, func_sig: FuncSig) -> FunctionValue<'a> {
        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&symbol_id).cloned();
        drop(irreg);

        let fn_value = match local_ir_value {
            Some(local_ir_value) => match local_ir_value.as_func() {
                Some((fn_value, _)) => fn_value.clone(),
                None => unreachable!(),
            },
            None => {
                let fn_value = self.build_func_decl(
                    func_sig.name.clone(),
                    func_sig.is_func_decl,
                    Some(func_sig.module_id),
                    func_sig.params.clone(),
                    func_sig.return_type.clone(),
                    func_sig.vis.clone(),
                );
                fn_value.set_linkage(Linkage::External);

                self.insert_ir_value(
                    symbol_id,
                    LocalIRValue::Func(
                        fn_value,
                        ConcreteType::FuncType(typed_func_type_from_func_sig(&func_sig)),
                    ),
                );
                fn_value
            }
        };

        fn_value
    }

    pub(crate) fn get_or_declare_struct(&mut self, symbol_id: SymbolID, struct_sig: &StructSig) -> StructType<'a> {
        if struct_sig.generic_params.is_none() {
            let irreg = self.irreg.borrow();
            let local_ir_value_opt = irreg.get(&symbol_id).cloned();
            drop(irreg);

            match local_ir_value_opt {
                Some(local_ir_value) => *local_ir_value.as_struct().unwrap(),
                None => {
                    let struct_type = self.build_struct_only_type(struct_sig);
                    self.insert_ir_value(symbol_id, LocalIRValue::Struct(struct_type));
                    struct_type
                }
            }
        } else {
            unreachable!("Generic struct used without type args!");
        }
    }

    pub(crate) fn get_or_declare_union(&mut self, symbol_id: SymbolID, union_sig: &UnionSig) -> StructType<'a> {
        let irreg = self.irreg.borrow();
        let local_ir_value_opt = irreg.get(&symbol_id).cloned();
        drop(irreg);

        match local_ir_value_opt {
            Some(local_ir_value) => *local_ir_value.as_struct().unwrap(),
            None => {
                let struct_type = self.build_union_struct_type(union_sig);
                self.insert_ir_value(symbol_id, LocalIRValue::Struct(struct_type));
                struct_type
            }
        }
    }

    pub(crate) fn get_or_declare_enum(&mut self, symbol_id: SymbolID, enum_sig: &EnumSig) -> StructType<'a> {
        let irreg = self.irreg.borrow();
        let local_ir_value_opt = irreg.get(&symbol_id).cloned();
        drop(irreg);

        match local_ir_value_opt {
            Some(local_ir_value) => *local_ir_value.as_struct().unwrap(),
            None => {
                let (struct_type, payload_type) = self.build_enum_struct_type(enum_sig);
                self.insert_ir_value(symbol_id, LocalIRValue::Enum((struct_type, payload_type)));
                struct_type
            }
        }
    }

    fn build_enum_decl(&self, name: &String) -> StructType<'a> {
        let module_name = self.get_module_name(self.module_id);
        self.llvmctx
            .opaque_struct_type(&generate_enum_abi_name(module_name, name.to_string()))
    }

    fn build_union_decl(&self, name: &String) -> StructType<'a> {
        let module_name = self.get_module_name(self.module_id);
        self.llvmctx
            .opaque_struct_type(&generate_union_abi_name(module_name, name.to_string()))
    }

    fn build_struct_decl(&self, name: &String) -> StructType<'a> {
        let module_name = self.get_module_name(self.module_id);
        self.llvmctx
            .opaque_struct_type(&generate_struct_abi_name(module_name, name.to_string()))
    }
}
