use egg::{AstSize, Extractor, Rewrite, RewriteScheduler, Runner, StopReason};
use thiserror::Error;

use crate::math::{ConstantFolding, Math};
use crate::parse::Expr;

#[derive(Debug, Clone, PartialEq, Error)]
pub enum SimplifyError {
    #[error("hit iteration limit while simplifying: {0}")]
    IterationLimit(usize),
    #[error("hit node limit while simplifying: {0}")]
    NodeLimit(usize),
    #[error("hit time limit while simplifying: {0}")]
    TimeLimit(f64),
    #[error("arithmetic error: {0}")]
    ArithmeticError(String),
}

pub fn simplify<'a, S, R>(expr: Expr, rules: R, scheduler: S) -> Result<Expr, SimplifyError>
where
    S: RewriteScheduler<Math, ConstantFolding> + 'static,
    R: IntoIterator<Item = &'a Rewrite<Math, ConstantFolding>>,
{
    let runner: Runner<Math, ConstantFolding> = Runner::new(ConstantFolding::default())
        .with_scheduler(scheduler)
        .with_expr(&expr)
        .with_hook(|runner| runner.egraph.analysis.check_err())
        .run(rules);

    match runner.stop_reason {
        Some(StopReason::Saturated) => {
            let root = runner.roots[0];
            let extractor = Extractor::new(&runner.egraph, AstSize);
            Ok(extractor.find_best(root).1)
        }
        Some(StopReason::IterationLimit(limit)) => Err(SimplifyError::IterationLimit(limit)),
        Some(StopReason::NodeLimit(limit)) => Err(SimplifyError::NodeLimit(limit)),
        Some(StopReason::TimeLimit(limit)) => Err(SimplifyError::TimeLimit(limit)),
        Some(StopReason::Other(s)) => Err(SimplifyError::ArithmeticError(s)),
        None => unreachable!("we just ran the runner"),
    }
}
