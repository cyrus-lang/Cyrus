#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportKind {
    DllImport,
    DllExport,
}