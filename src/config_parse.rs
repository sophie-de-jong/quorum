use std::path::Path;

use serde::Deserialize;

#[derive(Debug, Default, Deserialize, PartialEq)]
pub struct Config {
    #[serde(default)]
    pub runner: Runner,
    #[serde(default)]
    pub scheduler: Scheduler,
    #[serde(default)]
    pub cost: Cost,
    #[serde(default)]
    pub logging: Logging,
    #[serde(default, rename = "rule")]
    pub rules: Vec<Rule>,
}

#[derive(Debug, Default, Deserialize, PartialEq)]
pub struct Rule {
    pub name: String,
    pub lhs: String,
    pub rhs: String,
    #[serde(default)]
    pub bidirectional: bool,
    #[serde(default)]
    pub protected: bool,
    #[serde(default)]
    pub disabled: bool,
    #[serde(default)]
    pub conditions: Vec<String>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Runner {
    pub iteration_limit: u32,
    pub node_limit: u32,
    pub timeout_ms: u32,
}

impl Default for Runner {
    fn default() -> Self {
        Runner {
            iteration_limit: 30,
            node_limit: 10_000,
            timeout_ms: 5000,
        }
    }
}

#[derive(Debug, Default, Deserialize, PartialEq)]
pub struct Scheduler {
    pub disabled: bool,
    pub ban_length: u32,
    pub match_limit: u32,
}

#[derive(Debug, Default, Deserialize, PartialEq)]
pub struct Cost {
    pub operator_weight: u32,
    pub number_weight: f32,
    pub variable_weight: f32,
}

#[derive(Debug, Default, Deserialize, PartialEq)]
pub struct Logging {
    pub disabled: bool,
    pub trace_rules: Vec<String>,
}

pub fn load_config(path: &Path) -> Config {
    std::fs::read_to_string(path)
        .and_then(|source| toml::from_str(&source)
            .map_err(|err| std::io::Error::other(err)))
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_empty_config() {
        let config: Config = toml::from_str("").unwrap();
        assert_eq!(
            config,
            Config {
                runner: Runner {
                    iteration_limit: 0,
                    node_limit: 0,
                    timeout_ms: 0,
                },
                scheduler: Scheduler {
                    disabled: false,
                    ban_length: 0,
                    match_limit: 0,
                },
                cost: Cost {
                    operator_weight: 0,
                    number_weight: 0.0,
                    variable_weight: 0.0,
                },
                logging: Logging {
                    disabled: false,
                    trace_rules: Vec::new(),
                },
                rules: Vec::new(),
            }
        )
    }
}
