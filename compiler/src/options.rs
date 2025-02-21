#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub optimization_level: i32,
    pub library_path: Vec<String>,
    pub libraries: Vec<String>,
    pub build_dir: String
}

impl CompilerOptions {
    pub fn default() -> Self {
        CompilerOptions {
            optimization_level: 0,
            library_path: Vec::new(),
            libraries: Vec::new(),
            build_dir: String::new(),
        }
    }
}
