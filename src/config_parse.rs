use serde::Deserialize;
use toml::from_str;


#[derive(Deserialize)]
pub struct Config {
    pub runner: Runner,
    pub scheduler: Scheduler,
    pub cost: Cost,
    pub logging: Logging,
    #[serde(rename = "rule")]
    pub rules: Vec<Rule>,

}

#[derive(Default, Deserialize)]
pub struct Rule{
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

#[derive(Deserialize)]
pub struct Runner{
    pub iteration_limit: u32,
    pub node_limit: u32,
    pub timeout_ms: u32,
}

#[derive(Deserialize)]
pub struct Scheduler{
    pub disabled: bool,
    pub ban_length: u32,
    pub match_limit: u32,
}

#[derive(Deserialize)]
pub struct Cost{
    pub operator_weight: u32,
    pub number_weight: f32,
    pub variable_weight: f32,
}

#[derive(Deserialize)]
pub struct Logging{
    pub disabled: bool,
    pub trace_rules: Vec<String>,
}

pub fn load_config(){
    let source = std::fs::read_to_string("config.toml").unwrap();
    let config: Config = toml::from_str(&source).unwrap();
}