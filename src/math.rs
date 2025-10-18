use std::fmt::{self, Debug};
use std::hash::Hash;

use egg::{Analysis, EGraph, Id, Symbol, define_language};
use crate::arith::{self, ArithmeticError};

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
    Rem,
    Mul,
    Div,
    Pow,
}

impl Op {
    pub fn as_str(self) -> &'static str {
        match self {
            Op::Add => " + ",
            Op::Sub => " - ",
            Op::Rem => " % ",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Pow => "^",
        }
    }

    pub fn as_str_prefix(&self) -> &'static str {
        match self {
            Op::Sub => "-",
            _ => panic!("tried to get a prefix str from a non prefix operator"),
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
        match self {
            Op::Add => write!(f, " + "),
            Op::Sub => write!(f, " - "),
            Op::Rem => write!(f, " % "),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Pow => write!(f, "^"),
        }
    }
}

define_language! {
    pub enum Math {
        // Binary operators
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "%" = Rem([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "^" = Pow([Id; 2]),

        // Unary operators
        "-" = Neg(Id),

        // Leaves
        Num(u64),
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

fn combine_binary<F: FnOnce(i64, i64) -> Result<i64, ArithmeticError>>(
    lhs: Result<i64, ArithmeticError>,
    rhs: Result<i64, ArithmeticError>,
    f: F
) -> Result<i64, ArithmeticError> {
    match (lhs, rhs) {
        (Err(err), _) => Err(err),
        (_, Err(err)) => Err(err),
        (Ok(lhs), Ok(rhs)) => f(lhs, rhs)
    }
}

fn combine_unary<F: FnOnce(i64) -> Result<i64, ArithmeticError>>(
    rhs: Result<i64, ArithmeticError>,
    f: F
) -> Result<i64, ArithmeticError> {
    match rhs {
        Err(err) => Err(err),
        Ok(rhs) => f(rhs)
    }
}

pub struct ConstantFolding {
    pub error: Option<ArithmeticError>
}

impl Analysis<Math> for ConstantFolding {
    type Data = Option<i64>;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> egg::DidMerge {
        let changed = if to.is_none() && from.is_some() {
            *to = from;
            true
        } else {
            false
        };
        egg::DidMerge(changed, false)
    }

    fn make(egraph: &mut EGraph<Math, Self>, enode: &Math, _: Id) -> Self::Data {
        let data = |i: &Id| egraph[*i].data;
        let result = match enode {
            Math::Add([a, b]) => arith::add(data(a)?, data(b)?),
            Math::Sub([a, b]) => arith::sub(data(a)?, data(b)?),
            Math::Mul([a, b]) => arith::mul(data(a)?, data(b)?),
            Math::Div([a, b]) => arith::div(data(a)?, data(b)?),
            Math::Rem([a, b]) => arith::rem(data(a)?, data(b)?),
            Math::Pow([a, b]) => arith::pow(data(a)?, data(b)?),
            Math::Neg(a) => arith::neg(data(a)?),
            Math::Num(n) => i64::try_from(*n).map_err(|_| ArithmeticError::Overflow),
            Math::Sym(..) | Math::Fn(..) => return None,
        };

        match result {
            Ok(n) => Some(n),
            Err(err) => {
                self
            }
        }
    }

    fn modify(egraph: &mut EGraph<Math, Self>, id: Id) {
        if let Some(Ok(n)) = egraph[id].data {
            let mut added = egraph.add(Math::Num(n.unsigned_abs()));

            if n.is_negative() {
                added = egraph.add(Math::Neg(added))
            }
            egraph.union(id, added);
        }
    }
}
