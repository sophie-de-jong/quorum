mod config_parse;
pub mod fmt;
mod math;
mod parallel_scheduler;
pub mod parser;

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
        rewrite("mul-0", "A * 0", "0"),
        rewrite("mul-1", "A * 1", "A"),
        rewrite("distr", "A * (B + C)", "A * B + A * C"),
        rewrite("assoc", "A + (B + C)", "(A + B) + C"),
    ]
}

pub fn simplify(expr: Expr) -> Expr {
    let runner: Runner<Math, ConstantFolding, ()> = Runner::new(ConstantFolding)
        .with_scheduler(ParallelBackoffScheduler::new())
        .with_expr(&expr)
        .run(&rules());

    let root = runner.roots[0];

    let extractor = Extractor::new(&runner.egraph, AstSize);
    extractor.find_best(root).1
}
