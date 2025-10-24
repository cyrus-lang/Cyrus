use crate::builder::module::{CodeGenBuilder, LocalIRValue};
use inkwell::{
    module::Linkage,
    types::{ArrayType, StructType},
    values::FunctionValue,
};
use resolver::{
    sigs::{EnumSig, FuncSig, StructSig, UnionSig},
    typed_func_type_from_func_sig,
};
use tast::{SymbolID, types::SemanticType};

impl<'a> CodeGenBuilder<'a> {
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
        let local_ir_value = self.get_ir_value(symbol_id);

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
                        SemanticType::FuncType(typed_func_type_from_func_sig(&func_sig)),
                    ),
                );
                fn_value
            }
        };

        fn_value
    }

    pub(crate) fn get_or_declare_struct(&mut self, symbol_id: SymbolID, struct_sig: &StructSig) -> StructType<'a> {
        if struct_sig.generic_params.is_none() {
            let local_ir_value = self.get_ir_value(symbol_id);

            match local_ir_value {
                Some(local_ir_value) => *local_ir_value.as_struct().unwrap(),
                None => {
                    let struct_type = self.build_struct_type(struct_sig, None);
                    self.insert_ir_value(symbol_id, LocalIRValue::Struct(struct_type));
                    struct_type
                }
            }
        } else {
            unreachable!("Generic struct used without type args!");
        }
    }

    pub(crate) fn get_or_declare_union(&mut self, symbol_id: SymbolID, union_sig: &UnionSig) -> StructType<'a> {
        if union_sig.generic_params.is_none() {
            let local_ir_value = self.get_ir_value(symbol_id);

            match local_ir_value {
                Some(local_ir_value) => *local_ir_value.as_struct().unwrap(),
                None => {
                    let struct_type = self.build_union_type(union_sig, None);
                    self.insert_ir_value(symbol_id, LocalIRValue::Struct(struct_type));
                    struct_type
                }
            }
        } else {
            unreachable!("Generic union used without type args!");
        }
    }

    pub(crate) fn get_or_declare_enum(
        &mut self,
        symbol_id: SymbolID,
        enum_sig: &EnumSig,
    ) -> (StructType<'a>, ArrayType<'a>) {
        if enum_sig.generic_params.is_none() {
            let local_ir_value = self.get_ir_value(symbol_id);

            match local_ir_value {
                Some(local_ir_value) => {
                    let (struct_type, payload_type) = local_ir_value.as_enum().unwrap();
                    (struct_type.clone(), payload_type.clone())
                }
                None => {
                    let (struct_type, payload_type) = self.build_enum_type(enum_sig, None);
                    self.insert_ir_value(symbol_id, LocalIRValue::Enum((struct_type, payload_type)));
                    (struct_type, payload_type)
                }
            }
        } else {
            unreachable!("Generic enum used without type args!");
        }
    }
}
