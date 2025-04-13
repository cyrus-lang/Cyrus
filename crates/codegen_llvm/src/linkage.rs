use ast::ast::VisType;
use inkwell::module::Linkage;

use crate::CodeGenLLVM;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_linkage(&self, vis_type: VisType) -> Linkage {
        match vis_type {
            VisType::Extern => Linkage::External,
            VisType::Pub => Linkage::External,
            VisType::Internal => Linkage::Private,
            VisType::Inline => Linkage::LinkOnceODRAutoHide,
        }
    }
}
