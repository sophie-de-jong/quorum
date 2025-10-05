use std::backtrace;
use std::fmt::{self, Debug, Write};
use std::hash::Hash;

use egg::{define_language, Analysis, EGraph, Id, Language, Symbol};
use thiserror::Error;

const fn checked_exact_div(lhs: i64, rhs: i64) -> Option<i64> {
    match lhs.checked_rem(rhs) {
        Some(n) if n != 0 => Some(lhs / rhs),
        _ => None
    }
} 

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Precedence {
    level: u8,
    assoc: Assoc,
}

impl Ord for Precedence {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.level.cmp(&other.level)
    }
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Precedence {
    pub const ROOT: Precedence = Precedence { level: 0, assoc: Assoc::None };
    pub const ADD_SUB: Precedence = Precedence { level: 10, assoc: Assoc::Left };
    pub const MUL_DIV: Precedence = Precedence { level: 20, assoc: Assoc::Left };
    pub const PREFIX: Precedence = Precedence { level: 30, assoc: Assoc::None };
    pub const POW: Precedence = Precedence { level: 40, assoc: Assoc::Right };

    pub fn assoc(self) -> Assoc {
        self.assoc
    }

    pub fn left_bp(self) -> u8 {
        self.level
    }

    pub fn right_bp(self) -> u8 {
        self.level.wrapping_add_signed(self.assoc as i8)
    }
}


#[repr(i8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Assoc {
    Left = 1,
    None = 0,
    Right = -1,
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

    pub fn prefix_prec(self) -> Option<Precedence> {
        match self {
            Op::Sub => Some(Precedence::PREFIX),
            _ => None,
        }
    }

    pub fn infix_prec(self) -> Option<Precedence> {
        match self {
            Op::Add | Op::Sub => Some(Precedence::ADD_SUB),
            Op::Mul | Op::Div | Op::Rem => Some(Precedence::MUL_DIV),
            Op::Pow => Some(Precedence::POW),
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
            Op::Add  => Some(Math::Add([lhs, rhs])),
            Op::Sub => Some(Math::Sub([lhs, rhs])),
            Op::Mul  => Some(Math::Mul([lhs, rhs])),
            Op::Div => Some(Math::Div([lhs, rhs])),
            Op::Rem => Some(Math::Rem([lhs, rhs])),
            Op::Pow => Some(Math::Pow([lhs, rhs])),
        }
    }

    pub fn from_unary_op(op: Op, arg: Id) -> Option<Math> {
        match op {
            Op::Sub => Some(Math::Neg(arg)),
            _ => None
        }
    }
}

#[derive(Debug, Error)]
pub enum ArithmeticError {
    #[error("division by zero")]
    DivisionByZero,
    #[error("overflow")]
    Overflow
}

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
                Err(_) => None
            }
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
