#[derive(Debug, Clone)]
pub struct ObjectFileInfo {
    pub path: String,
    pub sz: usize
}

impl ObjectFileInfo {
    pub fn new(path: String, sz: usize) -> Self {
        Self { path, sz }
    }
}