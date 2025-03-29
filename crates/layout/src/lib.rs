use codegen_llvm::diag::{Diag, DiagKind, DiagLevel, display_single_diag};
use std::{
    fs::{self, File},
    io::Write,
};

fn create_common_files(output: String) -> Result<(), String> {
    if fs::exists(output.clone()).map_err(|err| err.to_string())? {
        display_single_diag(Diag {
            level: DiagLevel::Error,
            kind: DiagKind::Custom(format!("A project named '{}' already exists in this location.", output)),
            location: None,
        });
        std::process::exit(1);
    }

    fs::create_dir(output.clone()).map_err(|_| format!("Failed to create '{}' directory.", output))?;

    let mut gitignore = File::create(format!("{}/.gitignore", output))
        .map_err(|_| format!("Failed to create '{}/.gitignore' file.", output))?;

    gitignore.write("build\n".as_bytes()).map_err(|err| err.to_string())?;
    gitignore.write("tmp\n".as_bytes()).map_err(|err| err.to_string())?;
    gitignore.write(".env\n".as_bytes()).map_err(|err| err.to_string())?;

    Ok(())
}

pub fn create_project(project_name: String) -> Result<(), String> {
    create_common_files(project_name.clone())?;

    File::create(format!("{}/Project.toml", project_name))
        .map_err(|_| "Failed to create 'Project.toml' file.".to_string())?;

    fs::create_dir(format!("{}/src", project_name))
        .map_err(|_| format!("Failed to create '{}/src' directory.", project_name))?;

    let mut main_file = File::create(format!("{}/src/main.cyr", project_name))
        .map_err(|_| format!("Failed to create '{}/src/main.cyr' file.", project_name))?;

    main_file
        .write("fn main() {\n\tprintf(\"Hello World\");\n}".as_bytes())
        .map_err(|err| err.to_string())?;

    Ok(())
}

pub fn create_library_project(project_name: String) -> Result<(), String> {
    create_common_files(project_name.clone())?;

    File::create(format!("{}/Project.toml", project_name))
        .map_err(|_| "Failed to create 'Project.toml' file.".to_string())?;

    fs::create_dir(format!("{}/src", project_name))
        .map_err(|_| format!("Failed to create '{}/src' directory.", project_name))?;

    let mut main_file = File::create(format!("{}/src/lib.cyr", project_name))
        .map_err(|_| format!("Failed to create '{}/src/lib.cyr' file.", project_name))?;

    main_file
        .write(
            "/*
Welcome to your new library project! (lib.cyr)

You can start binding some functions in this library and do whatever you want.

Some suggestions to get started:
1. Define your public API functions first
2. Add documentation comments for each exported function
3. Organize related functionality into modules
4. Consider error handling strategies

Remember to:
- Keep your interfaces clean and simple
- Maintain consistent naming conventions
- Document any platform-specific behavior
- Version your library appropriately

Happy coding!
*/\n\n"
            .as_bytes(),
        )
        .map_err(|err| err.to_string())?;

    main_file
        .write("// extern fn some_library(): *void;\n".as_bytes())
        .map_err(|err| err.to_string())?;

    Ok(())
}
