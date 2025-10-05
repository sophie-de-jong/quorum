use std::cell::Cell;
use std::fmt::{self, Write};

use egg::Id;

use crate::math::{Math, Op, Precedence};
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
            prec: Cell::new(Precedence::ROOT), 
            root: id
        }
    }
}

pub struct Display<'a> {
    expr: &'a Expr,
    prec: Cell<Precedence>,
    root: Id
}

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_subexpr(f, self.root)
    }
}

impl<'a> Display<'a> {
    fn fmt_subexpr(&self, f: &mut fmt::Formatter, id: Id) -> fmt::Result {
        match &self.expr[id] {
            Math::Num(num) => write!(f, "{num}"),
            Math::Sym(sym) => write!(f, "{sym}"),
            Math::Add([a, b]) => self.fmt_binary_op(f, *a, *b, Op::Add),
            Math::Sub([a, b]) => self.fmt_binary_op(f, *a, *b, Op::Sub),
            Math::Mul([a, b]) => self.fmt_binary_op(f, *a, *b, Op::Mul),
            Math::Div([a, b]) => self.fmt_binary_op(f, *a, *b, Op::Div),
            Math::Rem([a, b]) => self.fmt_binary_op(f, *a, *b, Op::Rem),
            Math::Pow([a, b]) => self.fmt_binary_op(f, *a, *b, Op::Pow),
            Math::Neg(a) => {
                f.write_char('-')?;
                self.fmt_subexpr(f, *a)
            }
            Math::Fn(sym, args) => {
                write!(f, "{sym}")?;
                self.fmt_args(f, args)
            }
        }
    }

    fn fmt_binary_op(&self, f: &mut fmt::Formatter, a: Id, b: Id, op: Op) -> fmt::Result {
        self.fmt_subexpr(f, a)?;
        f.write_char(op.as_char())?;
        self.fmt_subexpr(f, b)
    }

    fn fmt_args(&self, f: &mut fmt::Formatter, args: &[Id]) -> fmt::Result {
        f.write_char('(')?;
        let mut args = args.iter();
        
        if let Some(arg) = args.next() {
            self.fmt_subexpr(f, *arg)?
        }
        for arg in args {
            f.write_str(", ")?;
            self.fmt_subexpr(f, *arg)?
        }
        f.write_char(')')
    }
}
