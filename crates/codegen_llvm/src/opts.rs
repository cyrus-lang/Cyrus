use serde::Deserialize;

#[derive(Deserialize, Debug, Clone)]
pub struct Options {
    pub project_type: Option<String>,
    pub project_name: Option<String>,
    pub project_version: Option<String>,
    pub cyrus_version: Option<String>,
    pub authors: Option<Vec<String>>,
    pub cpu: Option<String>,
    pub opt_level: Option<i32>,
    pub library_path: Vec<String>,
    pub libraries: Vec<String>,
    pub sources_dir: Vec<String>,
    pub build_dir: BuildDir,
    pub quiet: bool,
    pub stdlib_path: Option<String>,
}

#[derive(Deserialize, Debug, Clone)]
pub enum BuildDir {
    Default,
    Provided(String),
}

impl Options {
    pub fn default() -> Self {
        Self {
            project_type: None,
            project_name: None,
            authors: None,
            opt_level: None,
            cpu: None,
            library_path: Vec::new(),
            libraries: Vec::new(),
            build_dir: BuildDir::Default,
            cyrus_version: None,
            project_version: None,
            sources_dir: vec!["./".to_string()],
            quiet: false,
            stdlib_path: None,
        }
    }

    pub fn override_options(&mut self, instance: Self) {
        *self = Self {
            project_type: instance.project_type.or(self.project_type.clone()),
            project_name: instance.project_name.or(self.project_name.clone()),
            project_version: instance.project_version.or(self.project_version.clone()),
            cyrus_version: instance.cyrus_version.or(self.cyrus_version.clone()),
            authors: instance.authors.or(self.authors.clone()),
            opt_level: instance.opt_level,
            cpu: instance.cpu.clone(),
            library_path: {
                let mut library_paths = self.library_path.clone();
                library_paths.extend(instance.library_path);
                library_paths
            },
            libraries: {
                let mut libraries = self.libraries.clone();
                libraries.extend(instance.libraries);
                libraries
            },
            build_dir: match instance.build_dir.clone() {
                BuildDir::Provided(provided) => BuildDir::Provided(provided),
                BuildDir::Default => self.build_dir.clone(),
            },
            sources_dir: {
                let mut sources = self.sources_dir.clone();
                sources.extend(instance.sources_dir);
                sources
            },
            quiet: instance.quiet || self.quiet,
            stdlib_path: instance.stdlib_path.or(self.stdlib_path.clone()),
        };
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

            let optimize: String = table
                .get("optimize")
                .and_then(|v| v.as_str())
                .ok_or("'optimize' key must be string in 'Project.toml'.")?
                .try_into()
                .unwrap();

            options.opt_level = match optimize.as_str() {
                "none" => Some(0),
                "o1" => Some(1),
                "o2" => Some(2),
                "o3" => Some(3),
                _ => {
                    return Err("'optimize' key in 'Project.toml' must be one of o1, o2, o3 or none.".to_string());
                }
            };

            let build_dir = match table.get("build_dir").and_then(|v| v.as_str()) {
                Some(build_dir) => BuildDir::Provided(build_dir.to_string()),
                None => BuildDir::Default,
            };

            options.build_dir = build_dir;

            // TODO Get `Target Platform` with `cpu` keyword.
            // TODO Get `Compiler Version` with `version` keyword.
            // TODO Get `Input Sources` with `sources` keyword.
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
                .unwrap_or_default();
        }

        if let Some(value) = file_toml.get("project") {
            let table = value
                .as_table()
                .ok_or("Failed to parse 'project' from 'Project.toml'.")?;

            options.project_name = Some(
                table
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or("Failed to parse 'project name' in 'Project.toml'.")?
                    .to_string(),
            );

            options.sources_dir = table
                .get("sources")
                .and_then(|v| v.as_array())
                .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
                .unwrap_or_default();
        }

        Ok(options)
    }
}
