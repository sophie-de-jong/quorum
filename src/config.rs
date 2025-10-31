use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::{fs, io};

use egg::{Rewrite, Symbol};
use serde::Deserialize;

use crate::math::{ConstantFolding, Math};
use crate::parse::parse_pattern;

static CONFIG_FILE: OnceLock<PathBuf> = OnceLock::new();

pub fn init_config_file(path: &Path) -> bool {
    CONFIG_FILE.set(path.to_path_buf()).is_ok()
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case", default, deny_unknown_fields)]
pub struct Config {
    pub timeout: f64,
    pub iter_limit: usize,
    pub node_limit: usize,
    pub rule_ban_length: usize,
    pub rule_match_limit: usize,
    pub num_threads: usize,
    pub logging: String,
    pub folding: bool,
    #[serde(rename = "rule")]
    pub rules: Vec<Rule>,
    #[serde(rename = "strategy")]
    pub strategies: Vec<Strategy>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            iter_limit: 30,
            node_limit: 10_000,
            timeout: 5.0,
            rule_ban_length: 5,
            rule_match_limit: 1000,
            num_threads: 1,
            logging: "off".to_string(),
            folding: true,
            rules: vec![],
            strategies: vec![Strategy {
                name: String::from("simple"),
                optimize: Optimize::Size,
                operator_weight: 1.0,
                number_weight: 1.0,
                variable_weight: 1.0,
            }],
        }
    }
}

impl Config {
    pub fn load() -> io::Result<Config> {
        let path = CONFIG_FILE.get_or_init(|| PathBuf::from("config.toml"));
        Self::load_from_path(path)
    }

    pub fn load_from_path(path: &Path) -> io::Result<Config> {
        let buf = fs::read_to_string(path)?;
        match toml::from_str(&buf) {
            Ok(config) => Ok(config),
            Err(error) => Err(io::Error::new(io::ErrorKind::InvalidInput, error)),
        }
    }

    pub fn rules(&self) -> Vec<Rewrite<Math, ConstantFolding>> {
        self.rules
            .iter()
            .map(|rule| {
                let lhs = parse_pattern(&rule.lhs).unwrap();
                let rhs = parse_pattern(&rule.rhs).unwrap();
                Rewrite::new(Symbol::new(&rule.name), lhs, rhs).unwrap()
            })
            .collect()
    }
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct Rule {
    pub name: String,
    pub lhs: String,
    pub rhs: String,
    pub condition: Option<String>,
    #[serde(default)]
    pub bidirectional: bool,
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct Strategy {
    pub name: String,
    pub optimize: Optimize,
    pub operator_weight: f32,
    pub number_weight: f32,
    pub variable_weight: f32,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Optimize {
    #[default]
    Size,
    Depth,
    Both,
}
