pub struct ObjectFileInfo {
    pub file_path: String,
}

impl ObjectFileInfo {
    pub fn new(file_path: String) -> Self {
        Self { file_path }
    }
}
