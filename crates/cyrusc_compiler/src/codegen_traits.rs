pub trait CodeGen {
    // Takes the fully analyzed module from the context
    fn process_module(&self, module_name: &str);

    // Links all processed modules into a final artifact
    fn link(&self, artifact_kind: ArtifactKind);
}

pub enum ArtifactKind {
    Executable,
    ObjectFile,
    SharedLibrary,
    Assembly, 
    BackendIR,
    Bytecode,
}