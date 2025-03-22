#[derive(Debug, Clone)]
pub struct CodeGenLLVMOptions {
    pub optimization_level: i32,
    pub library_path: Vec<String>,
    pub libraries: Vec<String>,
    pub build_dir: String
}

impl CodeGenLLVMOptions {
    pub fn default() -> Self {
        Self {
            optimization_level: 0,
            library_path: Vec::new(),
            libraries: Vec::new(),
            build_dir: String::new(),
        }
    }
}
