//mod config_parse;
pub mod fmt;
mod math;
mod parallel_scheduler;
pub mod parser;
mod arith;

use egg::*;

use crate::math::{ConstantFolding, Math};
use crate::parallel_scheduler::ParallelBackoffScheduler;
use crate::parser::Expr;

fn rewrite(name: &str, lhs: &str, rhs: &str) -> Rewrite<Math, ConstantFolding> {
    let lhs = parser::parse_pattern(lhs).unwrap();
    let rhs = parser::parse_pattern(rhs).unwrap();
    Rewrite::new(name, lhs, rhs).unwrap()
}

fn rules() -> Vec<Rewrite<Math, ConstantFolding>> {
    vec![
        rewrite("commute-add", "A + B", "B + A"),
        rewrite("commute-mul", "A * B", "B * A"),
        rewrite("add-0", "A + 0", "A"),
        rewrite("sub-0", "A - A", "0"),
        rewrite("mul-0", "A * 0", "0"),
        rewrite("mul-1", "A * 1", "A"),
        rewrite("distr", "A * (B + C)", "A * B + A * C"),
        rewrite("assoc-add", "A + (B + C)", "(A + B) + C"),
        rewrite("sub-def", "A - B", "A + (-B)"),
        rewrite("sub-def-rev", "A + (-B)", "A - B"),
        rewrite("neg-def", "-A", "(-1)*A"),
        rewrite("neg-def-rev", "(-1)*A", "-A"),
    ]
}

pub fn simplify(expr: Expr) -> Option<Expr> {
    let runner: Runner<Math, ConstantFolding, ()> = Runner::new(ConstantFolding)
        .with_scheduler(ParallelBackoffScheduler::new())
        .with_expr(&expr)
        .with_hook(|runner| {
            for class in runner.egraph.classes() {
                if let Some(Err(err)) = class.data {
                    return Err(err.to_string())
                }
            }
            Ok(())
        })
        .run(&rules());
    
    match runner.stop_reason {
        Some(StopReason::Saturated) => {
            let root = runner.roots[0];
            let extractor = Extractor::new(&runner.egraph, AstSize);
            Some(extractor.find_best(root).1)
        }
        Some(StopReason::IterationLimit(_)) => {
            println!("hit iteration limit");
            None
        }
        Some(StopReason::NodeLimit(_)) => {
            println!("hit node limit");
            None
        }
        Some(StopReason::TimeLimit(_)) => {
            println!("hit time limit");
            None
        }
        Some(StopReason::Other(s)) => {
            println!("{s}");
            None
        }
        None => unreachable!("we just ran the runner")
    }
}
