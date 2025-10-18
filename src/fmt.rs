use std::cmp::Ordering;
use std::fmt::{self, Write};

use egg::Id;

use crate::math::{Assoc, Math, Op};
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
            root: id,
        }
    }
}

pub struct Display<'a> {
    expr: &'a Expr,
    root: Id,
}

impl fmt::Debug for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.debug_subexpr(f, self.root)
    }
}

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format_subexpr(f, self.root, 0)
    }
}

impl<'a> Display<'a> {
    fn debug_subexpr(&self, f: &mut fmt::Formatter, id: Id) -> fmt::Result {
        let (lhs, rhs, mut d) = match &self.expr[id] {
            Math::Add([a, b]) => (*a, *b, f.debug_tuple("Add")),
            Math::Sub([a, b]) => (*a, *b, f.debug_tuple("Sub")),
            Math::Rem([a, b]) => (*a, *b, f.debug_tuple("Rem")),
            Math::Mul([a, b]) => (*a, *b, f.debug_tuple("Mul")),
            Math::Div([a, b]) => (*a, *b, f.debug_tuple("Div")),
            Math::Pow([a, b]) => (*a, *b, f.debug_tuple("Pow")),
            Math::Neg(a) => return f.debug_tuple("Neg")
                .field(&self.expr.display_subexpr(*a))
                .finish(),
            Math::Num(num) => return f.debug_tuple("Num").field(num).finish(),
            Math::Sym(sym) => return f.debug_tuple("Sym").field(sym).finish(),
            Math::Fn(sym, args) => {
                let mut d = f.debug_tuple("Fn");
                d.field(sym);
                for arg in args {
                    d.field(&self.expr.display_subexpr(*arg));
                }
                return d.finish()
            },
        };

        d.field(&self.expr.display_subexpr(lhs));
        d.field(&self.expr.display_subexpr(rhs));
        d.finish()
    }

    fn format_subexpr(&self, f: &mut fmt::Formatter, id: Id, max_bp: u8) -> fmt::Result {
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
                    self.format_subexpr(f, *arg, 0)?
                }
                for arg in iter {
                    f.write_str(", ")?;
                    self.format_subexpr(f, *arg, 0)?
                }
                return f.write_char(')');
            }
        };

        // Binary ops use infix precedence; unary ops use prefix precedence
        let prec = match lhs {
            Some(_) => op.precedence(),
            None => op.prefix_precedence().unwrap(),
        };
        let lbp = prec.left_bp();
        let rbp = match self.expr[rhs] {
            // If the RHS is a unary negation, raise its binding power
            // to force parentheses (e.g. `1-(-1)` instead of `1--1`)
            Math::Neg(_) => u8::MAX,
            _ => prec.right_bp(),
        };
        let needs_parens = match max_bp.cmp(&lbp) {
            // Expression binds stronger than parent -> parentheses
            Ordering::Less => false,
            // Expression binds weaker than parent -> no parentheses
            Ordering::Greater => true,
            // Expression binds equally but is associative -> no parentheses
            Ordering::Equal => op.assoc() != Assoc::Both,
        };

        if needs_parens {
            f.write_char('(')?
        }

        if let Some(lhs) = lhs {
            self.format_subexpr(f, lhs, lbp)?;
            f.write_str(op.as_str())?;
        } else {
            f.write_str(op.as_str_prefix())?;
        }
        self.format_subexpr(f, rhs, rbp)?;

        if needs_parens {
            f.write_char(')')?
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    macro_rules! expr {
        ($($t:tt)*) => {{
            use crate::fmt::DisplayExpr;
            crate::parser::parse_expr(stringify!($($t)*))
                .unwrap()
                .display()
                .to_string()
        }};
    }

    #[test]
    fn test_simple_binary_ops() {
        assert_eq!(expr!(a + b), "a + b");
        assert_eq!(expr!(a - b), "a - b");
        assert_eq!(expr!(a % b), "a % b");
        assert_eq!(expr!(a * b), "a*b");
        assert_eq!(expr!(a / b), "a/b");
        assert_eq!(expr!(a ^ b), "a^b");
    }

    #[test]
    fn test_precedence_rules() {
        assert_eq!(expr!(1 + 2 * 3), "1 + 2*3");
        assert_eq!(expr!((1 + 2) * 3), "(1 + 2)*3");
        assert_eq!(expr!(1 - (2 + 3)), "1 - (2 + 3)");
        assert_eq!(expr!(1 / (2 * 3)), "1/(2*3)");
    }

    #[test]
    fn test_commutitative_op_collapses_parens() {
        assert_eq!(expr!(1 + 2 + 3), "1 + 2 + 3");
        assert_eq!(expr!(1 - 2 - 3), "(1 - 2) - 3");
    }

    #[test]
    fn test_unary_op() {
        assert_eq!(expr!(--x), "-(-x)");
        assert_eq!(expr!(-(x^2)), "-x^2");
        assert_eq!(expr!((-x)^2), "(-x)^2");
        assert_eq!(expr!(1 - -2), "1 - (-2)");
        assert_eq!(expr!(1 * -1), "1*(-1)")
    }
}
