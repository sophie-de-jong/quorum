use std::cell::Cell;
use std::cmp::Ordering;
use std::fmt::{self, Write};

use egg::Id;

use crate::math::{Assoc, Math, Op, Precedence};
use crate::parser::Expr;

pub trait DisplayExpr {
    fn display<'a>(&'a self) -> Display<'a>;

    fn display_subexpr<'a>(&'a self, id: Id) -> Display<'a>;
}

impl DisplayExpr for Expr {
    fn display<'a>(&'a self) -> Display<'a> {
        self.display_subexpr(self.root())
    }

    fn display_subexpr<'a>(&'a self, id: Id) -> Display<'a> {
        Display {
            expr: self,
            prec: Cell::new(Precedence::Add),
            root: id,
        }
    }
}

pub struct Display<'a> {
    expr: &'a Expr,
    prec: Cell<Precedence>,
    root: Id,
}

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_subexpr(f, self.root, 0)
    }
}

impl<'a> Display<'a> {
    fn fmt_subexpr(&self, f: &mut fmt::Formatter, id: Id, max_bp: u8) -> fmt::Result {
        let (lhs, rhs, op) = match &self.expr[id] {
            Math::Add([a, b]) => (Some(*a), *b, Op::Add),
            Math::Sub([a, b]) => (Some(*a), *b, Op::Sub),
            Math::Mul([a, b]) => (Some(*a), *b, Op::Mul),
            Math::Div([a, b]) => (Some(*a), *b, Op::Div),
            Math::Rem([a, b]) => (Some(*a), *b, Op::Rem),
            Math::Pow([a, b]) => (Some(*a), *b, Op::Pow),
            Math::Neg(a) => (None, *a, Op::Sub),
            Math::Num(num) => return write!(f, "{num}"),
            Math::Sym(sym) => return write!(f, "{sym}"),
            Math::Fn(sym, args) => {
                write!(f, "{sym}(")?;
                let mut iter = args.iter();
                if let Some(arg) = iter.next() {
                    self.fmt_subexpr(f, *arg, 0)?
                }
                for arg in iter {
                    f.write_str(", ")?;
                    self.fmt_subexpr(f, *arg, 0)?
                }
                return f.write_char(')');
            }
        };

        // Binary ops use infix precedence; unary ops use prefix precedence
        let prec = match lhs {
            Some(_) => op.precedence(),
            None => op.prefix_precedence().unwrap()
        };
        let lbp = prec.left_bp();

        // We add parentheses when this expression binds weaker than its parent,
        // or when precedence is equal but associativity doesn’t allow omission.
        let needs_parens = match max_bp.cmp(&lbp) {
            Ordering::Equal => op.assoc() != Assoc::Both,
            Ordering::Greater => true,
            Ordering::Less => false,
        };

        if needs_parens {
            f.write_char('(')?
        }

        // Print the left-hand side if binary and the operator.
        if let Some(lhs) = lhs {
            self.fmt_subexpr(f, lhs, lbp)?
        }
        f.write_char(op.as_char())?;

        // If the RHS is a unary negation, raise its binding power
        // to force parentheses (e.g. `1-(-1)` instead of `1--1`)
        let rbp = match self.expr[rhs] {
            Math::Neg(_) => u8::MAX,
            _ => prec.right_bp()
        };
        self.fmt_subexpr(f, rhs, rbp)?;

        if needs_parens {
            f.write_char(')')?
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse_expr;

    fn expr(input: &str) -> Expr {
        parse_expr(input).unwrap()
    }

    #[test]
    fn test_simple_binary_ops() {
        assert_eq!(expr("a + b").display().to_string(), "a+b");
        assert_eq!(expr("a - b").display().to_string(), "a-b");
        assert_eq!(expr("a * b").display().to_string(), "a*b");
        assert_eq!(expr("a / b").display().to_string(), "a/b");
        assert_eq!(expr("a ^ b").display().to_string(), "a^b");
    }

    #[test]
    fn test_precedence_rules() {
        let e = expr("1 + 2 * 3");
        assert_eq!(e.display().to_string(), "1+2*3");

        let e = expr("(1 + 2) * 3");
        assert_eq!(e.display().to_string(), "(1+2)*3");

        let e = expr("1 - (2 + 3)");
        assert_eq!(e.display().to_string(), "1-(2+3)");

        let e = expr("1 / (2 * 3)");
        assert_eq!(e.display().to_string(), "1/(2*3)");
    }

    #[test]
    fn test_commutitative_op_collapses_parens() {
        let e = expr("1 + 2 + 3");
        assert_eq!(e.display().to_string(), "1+2+3");

        let e = expr("1 - 2 - 3");
        assert_eq!(e.display().to_string(), "(1-2)-3");
    }

    #[test]
    fn test_unary_op() {
        let e = expr("--x");
        assert_eq!(e.display().to_string(), "-(-x)");

        let e = expr("-(x^2)");
        assert_eq!(e.display().to_string(), "-x^2");

        let e = expr("(-x)^2");
        assert_eq!(e.display().to_string(), "(-x)^2");

        let e = expr("1 - -2");
        assert_eq!(e.display().to_string(), "1-(-2)");

        let e = expr("1 * -1");
        assert_eq!(e.display().to_string(), "1*(-1)")
    }
}
