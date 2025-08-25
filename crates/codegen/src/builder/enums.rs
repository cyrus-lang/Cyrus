use typed_ast::TypedEnum;

use crate::builder::module::CodeGenBuilder;

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_enum_def(&self, _typed_enum: &TypedEnum) {
        todo!()
    }

    pub(crate) fn build_local_enum_def(&self, _typed_enum: &TypedEnum) {
        todo!();
    }
}
