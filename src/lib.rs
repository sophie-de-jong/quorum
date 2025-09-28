mod config_parse;
mod math;
mod parser;


use egg::*;

use crate::math::Node;
use crate::parser::Expr;

fn rewrite(name: &str, lhs: &str, rhs: &str) -> Rewrite<Node, ()> {
    let lhs = parser::parse_pattern(lhs).unwrap();
    let rhs = parser::parse_pattern(rhs).unwrap();
    Rewrite::new(name, lhs, rhs).unwrap()
}

fn make_rules() -> Vec<Rewrite<Node, ()>> {
    vec![
        rewrite("commute-add", "A + B", "B + A"),
        rewrite("commute-mul", "A * B", "B * A"),
        rewrite("add-0", "A + 0", "A"),
        rewrite("mul-0", "A * 0", "0"),
        rewrite("mul-1", "A * 1", "A"),
    ]
}

/// parse an expression, simplify it using egg, and pretty print it back out
fn simplify(s: &str) -> String {
    // parse the expression, the type annotation tells it which Language to use
    let expr: Expr = parser::parse_expr(s).unwrap();

    // simplify the expression using a Runner, which creates an e-graph with
    // the given expression and runs the given rules over it
    let runner = Runner::default().with_expr(&expr).run(&make_rules());

    // the Runner knows which e-class the expression given with `with_expr` is in
    let root = runner.roots[0];

    // use an Extractor to pick the best element of the root eclass
    let extractor = Extractor::new(&runner.egraph, AstSize);
    let (best_cost, best) = extractor.find_best(root);
    println!("Simplified {} to {} with cost {}", expr, best, best_cost);
    best.to_string()
}

#[test]
fn simple_tests() {
    assert_eq!(simplify("42 * 0"), "0");
    assert_eq!(simplify("foo * 1 + 0"), "foo");
}
