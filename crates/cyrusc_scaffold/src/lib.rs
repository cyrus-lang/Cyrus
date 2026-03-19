/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

use crate::{diagnostics::ProjectLayoutDiagKind, version::CYRUS_COMPILER_VERSION};
use cyrusc_diagcentral::{Diag, DiagLevel, display_and_exit_with_single_diag};
use std::{
    fs::{self, File},
    io::Write,
    path::Path,
};

mod diagnostics;
pub mod version;

fn create_common_files(output: String) -> Result<(), String> {
    if fs::exists(output.clone()).map_err(|err| err.to_string())? {
        display_and_exit_with_single_diag!(Diag {
            level: DiagLevel::Error,
            kind: Box::new(ProjectLayoutDiagKind::DuplicateProjectName { name: output.clone() }),
            loc: None,
            hint: None
        });
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

    let mut project_file = File::create(format!("{}/Project.toml", project_name))
        .map_err(|_| "Failed to create 'Project.toml' file.".to_string())?;

    let pure_project_name = Path::new(&project_name.clone())
        .file_name()
        .ok_or("Failed to retrieve project directory name.")?
        .to_str()
        .unwrap()
        .to_string();

    project_file
        .write(
            format!(
                "[project]
name = \"{}\"
version = \"0.0.1\"
type = \"executable\"
authors = [\"<John Doe> johnDoe@mail.com\"]

[dependencies]
libraries = []
library_path = []

[compiler]
cpu = \"generic\"
optimize = \"o1\"
sources = [\"src/*\"]
version = \"{}\"
build_dir = \"./build\"
",
                pure_project_name, CYRUS_COMPILER_VERSION
            )
            .as_bytes(),
        )
        .map_err(|err| err.to_string())?;

    fs::create_dir(format!("{}/src", project_name))
        .map_err(|_| format!("Failed to create '{}/src' directory.", project_name))?;

    let mut main_file = File::create(format!("{}/src/main.cyrus", project_name))
        .map_err(|_| format!("Failed to create '{}/src/main.cyrus' file.", project_name))?;

    main_file
        .write("import std::libc{printf};\n\npub fn main() {\n\tprintf(\"Hello, Cyrus!\");\n}".as_bytes())
        .map_err(|err| err.to_string())?;

    Ok(())
}

pub fn create_library_project(project_name: String) -> Result<(), String> {
    create_common_files(project_name.clone())?;

    let mut project_file = File::create(format!("{}/Project.toml", project_name))
        .map_err(|_| "Failed to create 'Project.toml' file.".to_string())?;

    let pure_project_name = Path::new(&project_name.clone())
        .file_name()
        .ok_or("Failed to retrieve project directory name.")?
        .to_str()
        .unwrap()
        .to_string();

    project_file
        .write(
            format!(
                "[project]
name = \"{}\"
version = \"0.0.1\"
type = \"lib\"
authors = [ \"<John Doe> john_doe@mail.com\"]

[dependencies]
libraries = []
library_path = []

[compiler]
cpu = \"generic\"
optimize = \"o1\"
sources = [\"src/*\"]
version = \"{}\"
",
                pure_project_name, CYRUS_COMPILER_VERSION
            )
            .as_bytes(),
        )
        .map_err(|err| err.to_string())?;

    fs::create_dir(format!("{}/src", project_name))
        .map_err(|_| format!("Failed to create '{}/src' directory.", project_name))?;

    let mut main_file = File::create(format!("{}/src/lib.cyrus", project_name))
        .map_err(|_| format!("Failed to create '{}/src/lib.cyrus' file.", project_name))?;

    main_file
        .write(
            "/*
Welcome to your new library project! (lib.cyrus)

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
        .write("// extern \"C\" fn foo() void;\n".as_bytes())
        .map_err(|err| err.to_string())?;

    Ok(())
}
