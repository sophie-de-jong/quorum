use std::fmt::{self, Debug, Write};
use std::hash::Hash;

use egg::{Analysis, EGraph, Id, Symbol, define_language};
//use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Assoc {
    Left,
    Right,
    Both,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Add,
    Mul,
    Neg,
    Pow,
}

impl Precedence {
    // Binding power.
    // See https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    const ADD_BP: u8 = 10;
    const MUL_BP: u8 = 20;
    const NEG_BP: u8 = 30;
    const POW_BP: u8 = 40;

    pub fn left_bp(self) -> u8 {
        match self {
            Precedence::Add => Self::ADD_BP,
            Precedence::Mul => Self::MUL_BP,
            Precedence::Neg => Self::NEG_BP,
            Precedence::Pow => Self::POW_BP,
        }
    }

    pub fn right_bp(self) -> u8 {
        match self {
            Precedence::Add => Self::ADD_BP + 1, // left-associative
            Precedence::Mul => Self::MUL_BP + 1, // left-associative
            Precedence::Neg => Self::NEG_BP,
            Precedence::Pow => Self::POW_BP - 1, // right-associative
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

impl Op {
    pub fn as_char(self) -> char {
        match self {
            Op::Add => '+',
            Op::Sub => '-',
            Op::Mul => '*',
            Op::Div => '/',
            Op::Rem => '%',
            Op::Pow => '^',
        }
    }

    pub fn assoc(self) -> Assoc {
        match self {
            Op::Add | Op::Mul | Op::Rem => Assoc::Both,
            Op::Sub | Op::Div => Assoc::Left,
            Op::Pow => Assoc::Right,
        }
    }

    pub fn precedence(self) -> Precedence {
        match self {
            Op::Add | Op::Sub => Precedence::Add,
            Op::Mul | Op::Div | Op::Rem => Precedence::Mul,
            Op::Pow => Precedence::Pow,
        }
    }

    pub fn prefix_precedence(self) -> Option<Precedence> {
        match self {
            Op::Sub => Some(Precedence::Neg),
            _ => None,
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(self.as_char())
    }
}

define_language! {
    pub enum Math {
        // Binary operators
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "%" = Rem([Id; 2]),
        "^" = Pow([Id; 2]),

        // Unary operators
        "-" = Neg(Id),

        // Leaves
        Num(i64),
        Sym(Symbol),

        // Other
        Fn(Symbol, Vec<Id>),
    }
}

impl Math {
    pub fn from_binary_op(op: Op, lhs: Id, rhs: Id) -> Option<Math> {
        match op {
            Op::Add => Some(Math::Add([lhs, rhs])),
            Op::Sub => Some(Math::Sub([lhs, rhs])),
            Op::Mul => Some(Math::Mul([lhs, rhs])),
            Op::Div => Some(Math::Div([lhs, rhs])),
            Op::Rem => Some(Math::Rem([lhs, rhs])),
            Op::Pow => Some(Math::Pow([lhs, rhs])),
        }
    }

    pub fn from_unary_op(op: Op, arg: Id) -> Option<Math> {
        match op {
            Op::Sub => Some(Math::Neg(arg)),
            _ => None,
        }
    }
}

// #[derive(Debug, Error)]
// pub enum ArithmeticError {
//     #[error("division by zero")]
//     DivisionByZero,
//     #[error("overflow")]
//     Overflow,
// }

pub struct ConstantFolding;
impl Analysis<Math> for ConstantFolding {
    type Data = Option<i64>;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> egg::DidMerge {
        egg::merge_max(to, from)
    }

    fn make(egraph: &mut EGraph<Math, Self>, enode: &Math, _: Id) -> Self::Data {
        let x = |i: &Id| egraph[*i].data;
        match enode {
            Math::Add([a, b]) => Some(x(a)? + x(b)?),
            Math::Sub([a, b]) => Some(x(a)? - x(b)?),
            Math::Mul([a, b]) => Some(x(a)? * x(b)?),
            Math::Div([a, b]) => Some(x(a)? / x(b)?),
            Math::Rem([a, b]) => Some(x(a)? % x(b)?),
            Math::Pow([a, b]) => match u32::try_from(x(b)?) {
                Ok(b) => Some(x(a)?.pow(b)),
                Err(_) => None,
            },
            Math::Neg(a) => Some(-x(a)?),
            Math::Num(n) => Some(*n),
            Math::Sym(..) | Math::Fn(..) => None,
        }
    }

    fn modify(egraph: &mut EGraph<Math, Self>, id: Id) {
        if let Some(n) = egraph[id].data {
            let added = egraph.add(Math::Num(n));
            egraph.union(id, added);
        }
    }
}
