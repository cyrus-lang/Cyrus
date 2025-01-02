use utils::compiler_error;
use crate::Compiler;

impl<'a> Compiler<'a> {
    pub fn execute(&self) {
        let result = self.context.compile();
        let main = result.get_function("main");
        if main.is_null() {
            compiler_error!("A 'main' function required as the entry point.");
        }

        unsafe {
            let main_fn: extern "C" fn() = std::mem::transmute(main);
            main_fn();
        }
    }

    pub fn make_executable_file(&self, file_path: String) {
        self.context.compile_to_file(gccjit::OutputKind::Executable, file_path);
    }

    pub fn make_object_file(&self, file_path: String) {
        self.context.compile_to_file(gccjit::OutputKind::ObjectFile, file_path);
    }

    pub fn set_debug_info(&self, is_debug_mode: bool) {
        self.context.set_debug_info(is_debug_mode);
    }

    pub fn make_dump_file(&self, file_path: String) {
        self.context.dump_to_file(file_path, true);
    }
}
