use inkwell::debug_info::{DICompileUnit, DWARFEmissionKind, DWARFSourceLanguage, DebugInfoBuilder};
use inkwell::module::Module;
use utils::fs::split_paths;

pub fn new_di_builder<'a>(
    llvmmodule: &Module<'a>,
    module_file_path: String,
) -> (DebugInfoBuilder<'a>, DICompileUnit<'a>) {
    let (dirname, filename) = split_paths(&module_file_path);

    llvmmodule.create_debug_info_builder(
        true,
        DWARFSourceLanguage::C,
        &filename,
        &dirname,
        "cyrus",
        false,
        "",
        0,
        "",
        DWARFEmissionKind::Full,
        0,
        false,
        false,
        "",
        "",
    )
}

#[macro_export]
macro_rules! llvm_set_current_location {
    ($self:expr, $loc:expr) => {{
        let fn_scope = $self.dibuilder.dicompunit.as_debug_info_scope();
        let dilocation = $self.dibuilder.dibuilder.create_debug_location(
            $self.llvmctx,
            $loc.line.try_into().unwrap(),
            $loc.column.try_into().unwrap(),
            fn_scope,
            None,
        );
        $self.llvmbuilder.set_current_debug_location(dilocation);
    }};
}
