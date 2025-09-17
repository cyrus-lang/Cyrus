use crate::{builder::module::CodeGenBuilder, diagnostics::CodeGenDiagKind};
use ast::source_loc::SourceLoc;
use diagcentral::{Diag, DiagLevel, DiagLoc, reporter::DiagReporter};
use inkwell::{
    module::Linkage,
    types::ArrayType,
    values::{BasicValue, BasicValueEnum, GlobalValue},
};
use utils::escaping::unescape_string;

impl<'ctx> CodeGenBuilder<'ctx> {
    fn unescape_or_exit(&self, value: &str, loc: &SourceLoc) -> String {
        match unescape_string(&unescape_string(value).unwrap_or_else(|e| {
            self.report_unescape_error(&e.to_string(), loc);
            unreachable!()
        })) {
            Ok(v) => v,
            Err(err) => {
                self.report_unescape_error(&err.to_string(), loc);
                unreachable!()
            }
        }
    }

    fn report_unescape_error(&self, err: &str, loc: &SourceLoc) {
        DiagReporter::display_single(Diag {
            level: DiagLevel::Error,
            kind: CodeGenDiagKind::UnescapeError { err: err.to_string() },
            location: Some(DiagLoc {
                file: loc.file_path.clone(),
                line: loc.line,
                column: loc.column,
            }),
            hint: None,
        });
    }

    pub(crate) fn build_byte_string(&self, value: String, loc: SourceLoc) -> BasicValueEnum<'ctx> {
        let unescaped = self.unescape_or_exit(&value, &loc);
        let const_str = self.llvmctx.const_string(unescaped.as_bytes(), true);

        const_str.as_basic_value_enum()
    }

    pub(crate) fn build_c_style_string(&self, value: String, loc: SourceLoc) -> BasicValueEnum<'ctx> {
        self.build_string_literal(value, loc)
    }

    pub(crate) fn build_global_str(&self, value: String, loc: SourceLoc) -> (GlobalValue<'ctx>, ArrayType<'ctx>) {
        let unescaped = self.unescape_or_exit(&value, &loc);
        let const_str = self.llvmctx.const_string(unescaped.as_bytes(), true);

        let llvmmodule = self.llvmmodule.borrow_mut();
        let global_str = llvmmodule.add_global(const_str.get_type(), None, ".str");
        global_str.set_initializer(&const_str);
        global_str.set_constant(true);
        global_str.set_unnamed_addr(true);
        global_str.set_linkage(Linkage::Private);
        global_str.set_alignment(1);
        drop(llvmmodule);

        (global_str, const_str.get_type())
    }

    pub(crate) fn build_string_literal(&self, value: String, loc: SourceLoc) -> BasicValueEnum<'ctx> {
        let (global_str, _) = self.build_global_str(value, loc);

        global_str.as_pointer_value().as_basic_value_enum()
    }
}
