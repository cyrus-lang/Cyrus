use serde::Deserialize;

#[derive(Deserialize, Debug, Clone)]
pub struct Options {
    pub project_name: Option<String>, 
    pub project_version: Option<String>,
    pub cyrus_version: Option<String>,
    pub authors: Option<Vec<String>>,
    pub cpu: String,
    pub opt_level: i32,
    pub library_path: Vec<String>,
    pub libraries: Vec<String>,
    pub sources_dir: Vec<String>,
    pub build_dir: String,
}

impl Options {
    pub fn default() -> Self {
        Self {
            project_name: None,
            authors: None,
            opt_level: 0,
            cpu: String::new(),
            library_path: Vec::new(),
            libraries: Vec::new(),
            build_dir: String::new(),
            sources_dir: Vec::new(),
            cyrus_version: None,
            project_version: None,
        }
    }

    pub fn read_toml(file_path: String) -> Result<Options, String> {
        let mut options = Options::default();

        let file_content =
            std::fs::read_to_string(file_path).map_err(|_| "Failed to read file 'Project.toml' content.")?;

        let file_toml: toml::Value =
            toml::from_str(&file_content).map_err(|_| "Failed to parse 'Project.toml' content.")?;

        if let Some(value) = file_toml.get("compiler") {
            let table = value
                .as_table()
                .ok_or("Failed to parse 'compiler' options from 'Project.toml'.")?;

            options.opt_level = table
                .get("optimize")
                .and_then(|v| v.as_integer())
                .ok_or("Invalid value given for 'optimize' key in 'Project.toml'.")?
                .try_into()
                .unwrap();
        }

        if let Some(value) = file_toml.get("dependencies") {
            let table = value
                .as_table()
                .ok_or("Failed to parse 'dependencies' from 'Project.toml'.")?;

            options.library_path = table
                .get("library_path")
                .and_then(|v| v.as_array())
                .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
                .unwrap_or_else(Vec::new);

            options.libraries = table
                .get("libraries")
                .and_then(|v| v.as_array())
                .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
                .unwrap_or_else(Vec::new);
        }

        Ok(options)
    }
}

