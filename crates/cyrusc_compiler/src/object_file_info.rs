#[derive(Debug, Clone)]
pub struct ObjectFileInfo {
    pub path: String,
    pub size: usize
}

impl ObjectFileInfo {
    pub fn new(path: String, size: usize) -> Self {
        Self { path, size }
    }
}