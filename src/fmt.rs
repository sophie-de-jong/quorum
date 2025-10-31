use std::cmp::Ordering;
use std::fmt::{self, Write};

use egg::Id;

use crate::math::{Assoc, BinaryOp, Math, UnaryOp};
use crate::parse::Expr;

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
        self.display_subexpr(f, self.root, 0)
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
            Math::Neg(a) => {
                return f
                    .debug_tuple("Neg")
                    .field(&self.expr.display_subexpr(*a))
                    .finish();
            }
            Math::Num(num) => return f.debug_tuple("Num").field(num).finish(),
            Math::Sym(sym) => return f.debug_tuple("Sym").field(sym).finish(),
            Math::Fn(sym, args) => {
                let mut d = f.debug_tuple("Fn");
                d.field(sym);
                for arg in args {
                    d.field(&self.expr.display_subexpr(*arg));
                }
                return d.finish();
            }
        };

        d.field(&self.expr.display_subexpr(lhs));
        d.field(&self.expr.display_subexpr(rhs));
        d.finish()
    }

    fn display_subexpr(&self, f: &mut fmt::Formatter, id: Id, max_bp: u8) -> fmt::Result {
        match &self.expr[id] {
            Math::Add([a, b]) => self.display_binary_expr(f, BinaryOp::Add, *a, *b, max_bp),
            Math::Sub([a, b]) => self.display_binary_expr(f, BinaryOp::Sub, *a, *b, max_bp),
            Math::Mul([a, b]) => self.display_binary_expr(f, BinaryOp::Mul, *a, *b, max_bp),
            Math::Div([a, b]) => self.display_binary_expr(f, BinaryOp::Div, *a, *b, max_bp),
            Math::Rem([a, b]) => self.display_binary_expr(f, BinaryOp::Rem, *a, *b, max_bp),
            Math::Pow([a, b]) => self.display_binary_expr(f, BinaryOp::Pow, *a, *b, max_bp),
            Math::Neg(a) => self.display_unary_expr(f, UnaryOp::Neg, *a, max_bp),
            Math::Num(num) => write!(f, "{num}"),
            Math::Sym(sym) => write!(f, "{sym}"),
            Math::Fn(sym, args) => {
                write!(f, "{sym}(")?;
                let mut iter = args.iter();
                if let Some(arg) = iter.next() {
                    self.display_subexpr(f, *arg, 0)?
                }
                for arg in iter {
                    f.write_str(", ")?;
                    self.display_subexpr(f, *arg, 0)?
                }
                f.write_char(')')
            }
        }
    }

    fn display_unary_expr(
        &self,
        f: &mut fmt::Formatter,
        op: UnaryOp,
        rhs: Id,
        max_bp: u8,
    ) -> fmt::Result {
        let prec = op.precedence();
        let rbp = match self.expr[rhs] {
            // If the RHS is a unary negation, raise its binding power
            // to force parentheses (e.g. `1-(-1)` instead of `1--1`)
            Math::Neg(_) => u8::MAX,
            _ => prec.right_bp(),
        };
        let needs_parens = max_bp > rbp;

        if needs_parens {
            f.write_char('(')?
        }

        write!(f, "{op}")?;
        self.display_subexpr(f, rhs, rbp)?;

        if needs_parens {
            f.write_char(')')?
        };
        Ok(())
    }

    fn display_binary_expr(
        &self,
        f: &mut fmt::Formatter,
        op: BinaryOp,
        lhs: Id,
        rhs: Id,
        max_bp: u8,
    ) -> fmt::Result {
        let prec = op.precedence();
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

        self.display_subexpr(f, lhs, lbp)?;
        write!(f, "{op}")?;
        self.display_subexpr(f, rhs, rbp)?;

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
            crate::parse::parse_expr(stringify!($($t)*))
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
        assert_eq!(expr!(-(x ^ 2)), "-x^2");
        assert_eq!(expr!((-x) ^ 2), "(-x)^2");
        assert_eq!(expr!(1 - -2), "1 - (-2)");
        assert_eq!(expr!(1 * -1), "1*(-1)")
    }
}
