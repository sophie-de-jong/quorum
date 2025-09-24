use std::borrow::Cow;

use serde::Deserialize;

#[derive(Debug, Default, Deserialize, PartialEq)]
pub struct Config<'s> {
    #[serde(default)]
    pub runner: Runner,
    #[serde(default)]
    pub scheduler: Scheduler,
    #[serde(default)]
    pub cost: Cost,
    #[serde(default)]
    pub logging: Logging,
    #[serde(borrow, default, rename = "rule")]
    pub rules: Vec<Rule<'s>>,
}

#[derive(Debug, Default, Deserialize, PartialEq)]
pub struct Rule<'s> {
    pub name: Cow<'s, str>,
    pub lhs: Cow<'s, str>,
    pub rhs: Cow<'s, str>,
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

#[derive(Debug, Deserialize, PartialEq)]
pub struct Scheduler {
    pub disabled: bool,
    pub ban_length: u32,
    pub match_limit: u32,
}

impl Default for Scheduler {
    fn default() -> Self {
        Scheduler {
            disabled: false,
            ban_length: 5,
            match_limit: 1000,
        }
    }
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Cost {
    pub operator_weight: u32,
    pub number_weight: f32,
    pub variable_weight: f32,
}

impl Default for Cost {
    fn default() -> Self {
        Cost {
            operator_weight: 1,
            number_weight: 1.0,
            variable_weight: 1.0,
        }
    }
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Logging {
    pub trace_rules: Vec<String>,
}

impl Default for Logging {
    fn default() -> Self {
        Logging {
            trace_rules: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_empty_config() {
        let config: Config = toml::from_str("").unwrap();
        assert_eq!(config, Config::default());
    }

    #[test]
    fn test_rule_parsing() {
        let config: Config = toml::from_str(
            r#"
            [[rule]]
            name = "test_rule_1"
            lhs = "x + y"
            rhs = "y + x"

            [[rule]]
            name = "test_rule_2"
            lhs = "x + 0"
            rhs = "x"
        "#,
        )
        .unwrap();
        assert_eq!(
            config,
            Config {
                rules: vec![
                    Rule {
                        name: "test_rule_1".into(),
                        lhs: "x + y".into(),
                        rhs: "y + x".into(),
                        ..Default::default()
                    },
                    Rule {
                        name: "test_rule_2".into(),
                        lhs: "x + 0".into(),
                        rhs: "x".into(),
                        ..Default::default()
                    }
                ],
                ..Default::default()
            }
        )
    }
}
