use std::collections::HashSet;
use std::io;

use egg::{AstSize, Extractor, Runner, Symbol};
use quorum::config::{Config, Strategy};
use quorum::math::{ConstantFolding, Math};
use quorum::parse::{Expr, Rewrite};
use quorum::simplify::SimplifyError;

pub struct App {
    config: Config,
    queue: Vec<(Expr, Expr)>,
    runner: Runner<Math, ConstantFolding>,
    disabled_rules: HashSet<Symbol>,
}

impl App {
    pub fn new() -> io::Result<App> {
        Ok(App {
            config: Config::load()?,
            queue: Vec::new(),
            runner: Runner::new(ConstantFolding::default()),
            disabled_rules: HashSet::new(),
        })
    }

    pub fn enable_rule(&mut self, rule: impl Into<Symbol>) -> bool {
        self.disabled_rules.remove(&rule.into())
    }

    pub fn disable_rule(&mut self, rule: impl Into<Symbol>) -> bool {
        self.disabled_rules.insert(rule.into())
    }

    pub fn queue(&self) -> &[(Expr, Expr)] {
        &self.queue
    }

    pub fn config(&self) -> &Config {
        &self.config
    }

    pub fn config_mut(&mut self) -> &mut Config {
        &mut self.config
    }

    pub fn run_expr(&mut self, expr: Expr) -> Result<&Expr, SimplifyError> {
        todo!()
    }
}
