//mod config_parse;
pub mod fmt;
mod math;
mod parallel_scheduler;
pub mod parser;

use egg::*;
use thiserror::Error;

use crate::math::{ConstantFolding, Math};
use crate::parallel_scheduler::ParallelBackoffScheduler;
use crate::parser::Expr;

macro_rules! rw {
    ($name:expr; $lhs:expr => $rhs:expr) => {
        Rewrite::new($name, pat!($lhs), pat!($rhs))
            .expect("error while creating rewrite") 
    };
}

fn rules() -> Vec<Rewrite<Math, ConstantFolding>> {
    vec![
        // rw!("comm-add"; A + B => B + A),
        // rw!("comm-mul"; A * B => B * A),
        // rw!("assoc-add"; A + (B + C) => (A + B) + C),
        // rw!("assoc-mul"; A * (B * C) => (A * B) * C),

        // rw!("sub-canon"; A - B => A + (-1 * B)),
        rw!("div-canon"; A / B => A * (B ^ -1)),

        // rw!("zero-add"; A + 0 => A),
        // rw!("zero-mul"; A * 0 => 0),
        // rw!("one-mul";  A * 1 => A),

        // rw!("add-zero"; A => A + 0),
        // rw!("mul-one";  A => A * 1),

        // rw!("cancel-sub"; A - A => 0),
        // rw!("cancel-div"; A / A => 1),

        // rw!("distribute"; A * (B + C) => (A * B) + (A * C)),
        // rw!("factor";     (A * B) + (A * C) => A * (B + C)),

        // rw!("pow-mul";       A ^ B * A ^ C => A ^ (B + C)),
        // rw!("pow-zero";      X ^ 0 => 1),
        // rw!("pow-one";       X ^ 1 => X),
        // rw!("pow-two";       X ^ 2 => X * X),
        // rw!("pow-recip";     X ^ -1 => 1 / X),
        // rw!("recip-mul-div"; X * (1 / X) => 1),
    ]
}

#[derive(Debug, Clone, PartialEq, Error)]
pub enum SimplifyError {
    #[error("hit iteration limit while simplifying: {0}")]
    IterationLimit(usize),
    #[error("hit node limit while simplifying: {0}")]
    NodeLimit(usize),
    #[error("hit time limit while simplifying: {0}")]
    TimeLimit(f64),
    #[error("arithmetic error: {0}")]
    ArithmeticError(String)
}

pub fn simplify(expr: Expr) -> Result<RecExpr<Math>, SimplifyError> {
    let runner: Runner<Math, ConstantFolding, ()> = Runner::new(ConstantFolding::default())
        //.with_scheduler(BackoffScheduler::default())
        .with_expr(&expr)
        .with_hook(|runner| {
            match runner.egraph.analysis.error {
                None => Ok(()),
                Some(err) => Err(err.to_string()),
            }
        })
        .run(&rules());
    
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
        None => unreachable!("we just ran the runner")
    }
}
