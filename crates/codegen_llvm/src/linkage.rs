use ast::ast::{StorageClass, StorageClass};
use inkwell::module::Linkage;

use crate::CodeGenLLVM;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_linkage(&self, storage_class: StorageClass) -> Linkage {
        match storage_class {
            StorageClass::Extern => Linkage::External,
            StorageClass::Public => Linkage::External,
            StorageClass::Internal => Linkage::Private,
            StorageClass::Inline => Linkage::LinkOnceODRAutoHide,
            StorageClass::PublicInline => todo!(),
            StorageClass::PublicExtern => todo!(),
        }
    }
}
