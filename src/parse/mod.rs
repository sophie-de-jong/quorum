mod op;
mod pratt;
mod token;

use crate::condition::Condition;
use crate::math::{ConstantFolding, Math};
use egg::{ConditionalApplier, Symbol};
use pratt::{ParseError, parse_pratt};
use token::tokenize;

pub type Expr = egg::RecExpr<Math>;
pub type Pattern = egg::Pattern<Math>;
pub type Rewrite = egg::Rewrite<Math, ConstantFolding>;

/// Parse a string into a `Pattern`.
///
/// The resulting `Pattern` can be used as either the left-hand or
/// right-hand side of a rewrite rule.
///
/// # Errors
///
/// Returns a `ParseError` if the input contains unexpected tokens,
/// unbalanced parentheses, or other syntax errors.
pub fn parse_pattern(input: &str) -> Result<Pattern, ParseError> {
    let mut tokens = tokenize(input)?.into_iter().peekable();
    parse_pratt(&mut tokens).map(Pattern::new)
}

#[macro_export]
macro_rules! pat {
    ($e:expr) => {
        $crate::parser::parse_pattern(stringify!($e)).unwrap()
    };
}

pub fn parse_condition(input: &str) -> Result<Condition, ParseError> {
    let mut tokens = tokenize(input)?.into_iter().peekable();
    parse_pratt(&mut tokens).map(Condition::new)
}

/// Parse a string into an `Expr`.
///
/// An expression is similar to a pattern, except it cannot contain any
/// pattern variables. This produces a concrete expression tree that can
/// be added to an e-graph.
///
/// # Errors
///
/// Returns a `ParseError` if the input is malformed or if a pattern
/// variable is encountered.
pub fn parse_expr(input: &str) -> Result<Expr, ParseError> {
    let mut tokens = tokenize(input)?.into_iter().peekable();
    parse_pratt(&mut tokens)
}

#[macro_export]
macro_rules! expr {
    ($e:expr) => {
        $crate::parser::parse_expr(stringify!($e)).unwrap()
    };
}

/// Parses a `Rewrite`.
///
/// # Errors
///
/// Returns a `ParseError` if the input contains unexpected tokens,
/// unbalanced parentheses, unbound variables or other syntax errors.
pub fn parse_rewrite(name: &str, lhs: &str, rhs: &str) -> Result<Rewrite, ParseError> {
    let name = Symbol::new(name);
    let lhs = parse_pattern(lhs)?;
    let rhs = parse_pattern(rhs)?;
    Rewrite::new(name, lhs, rhs).map_err(|_| ParseError::UnboundVariable(name))
}

pub fn parse_cond_rewrite(
    name: &str,
    lhs: &str,
    rhs: &str,
    cond: &str,
) -> Result<Rewrite, ParseError> {
    let name = Symbol::new(name);
    let lhs = parse_pattern(lhs)?;
    let applier = parse_pattern(rhs)?;
    let condition = parse_condition(cond)?;
    let rhs = ConditionalApplier { condition, applier };
    Rewrite::new(name, lhs, rhs).map_err(|_| ParseError::UnboundVariable(name))
}

#[macro_export]
macro_rules! rw {
    ($name:expr; $lhs:expr => $rhs:expr) => {
        $crate::parser::parse_rewrite($name, stringify!($lhs), stringify!($rhs)).unwrap()
    };
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_single_number() {
        let s = parse_expr("1").unwrap();
        assert_eq!(s.to_string(), "1");
    }

    #[test]
    fn parse_addition_and_multiplication_precedence() {
        let s = parse_expr("1 + 2 * 3").unwrap();
        assert_eq!(s.to_string(), "(+ 1 (* 2 3))");
    }

    #[test]
    fn parse_complex_expression_with_left_associativity() {
        let s = parse_expr("a + b * c * d + e").unwrap();
        assert_eq!(s.to_string(), "(+ (+ a (* (* b c) d)) e)");
    }

    #[test]
    fn parse_right_associative_operator() {
        let s = parse_expr("f ^ g ^ h").unwrap();
        assert_eq!(s.to_string(), "(^ f (^ g h))");
    }

    #[test]
    fn parse_complex_expression_with_left_and_right_associativity() {
        let s = parse_expr(" 1 + 2 + f ^ g ^ h * 3 * 4").unwrap();
        assert_eq!(s.to_string(), "(+ (+ 1 2) (* (* (^ f (^ g h)) 3) 4))");
    }

    #[test]
    fn parse_double_negation_with_multiplication() {
        let s = parse_expr("--1 * 2").unwrap();
        assert_eq!(s.to_string(), "(* (- (- 1)) 2)");
    }

    #[test]
    fn parse_double_negation_with_right_associativity() {
        let s = parse_expr("--f ^ g").unwrap();
        assert_eq!(s.to_string(), "(- (- (^ f g)))");
    }

    #[test]
    fn parse_redundant_parentheses() {
        let s = parse_expr("(((0)))").unwrap();
        assert_eq!(s.to_string(), "0");
    }

    #[test]
    fn parse_parenthesized() {
        let s = parse_expr("(1 + 2) * 3").unwrap();
        assert_eq!(s.to_string(), "(* (+ 1 2) 3)");

        let s = parse_expr("1 + (2 * 3)").unwrap();
        assert_eq!(s.to_string(), "(+ 1 (* 2 3))");
    }

    #[test]
    fn parse_complex_pattern() {
        let p = parse_pattern("A + B * c * D + E").unwrap();
        assert_eq!(p.to_string(), "(+ (+ A (* (* B c) D)) E)");
    }

    #[test]
    fn parse_function() {
        let s = parse_expr("sin(x) + max(x, y)").unwrap();
        assert_eq!(s.to_string(), "(+ (sin x) (max x y))")
    }
}
