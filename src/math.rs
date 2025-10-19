use std::fmt::{self, Debug};
use std::hash::Hash;

use egg::{define_language, Analysis, EGraph, Id, Language, Symbol};
use thiserror::Error;

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

#[derive(Debug, Error, Clone, Copy)]
pub enum ArithmeticError {
    #[error("division by zero")]
    DivisionByZero,
    #[error("overflow")]
    Overflow,
}

#[derive(Default)]
pub struct ConstantFolding {
    pub error: Option<ArithmeticError>
}

impl ConstantFolding {
    pub fn report_err(&mut self, err: ArithmeticError) {
        if self.error.is_none() {
            self.error = Some(err)
        }
    }

    pub fn report_err_if(&mut self, opt: Option<i64>, err: ArithmeticError) -> Option<i64> {
        if let Some(n) = opt {
            Some(n)
        } else {
            self.report_err(err);
            None
        }
    }

    pub fn num(&mut self, num: u64) -> Option<i64> {
        self.report_err_if(i64::try_from(num).ok(), ArithmeticError::Overflow)
    }

    pub fn add(&mut self, lhs: i64, rhs: i64) -> Option<i64> {
        self.report_err_if(lhs.checked_add(rhs), ArithmeticError::Overflow)
    }

    pub fn sub(&mut self, lhs: i64, rhs: i64) -> Option<i64> {
        self.report_err_if(lhs.checked_sub(rhs), ArithmeticError::Overflow)
    }

    pub fn mul(&mut self, lhs: i64, rhs: i64) -> Option<i64> {
        self.report_err_if(lhs.checked_mul(rhs), ArithmeticError::Overflow)
    }

    pub fn div(&mut self, lhs: i64, rhs: i64) -> Option<i64> {
        if rhs != 0 {
            self.report_err_if(lhs.checked_div(rhs), ArithmeticError::Overflow)
        } else {
            self.report_err(ArithmeticError::DivisionByZero);
            None
        }
    }

    pub fn rem(&mut self, lhs: i64, rhs: i64) -> Option<i64> {
        if rhs != 0 {
            self.report_err_if(lhs.checked_rem(rhs), ArithmeticError::Overflow)
        } else {
            self.report_err(ArithmeticError::DivisionByZero);
            None
        }
    }

    pub fn pow(&mut self, lhs: i64, rhs: i64) -> Option<i64> {
        let result = u32::try_from(rhs).ok().and_then(|exp| lhs.checked_pow(exp));
        self.report_err_if(result, ArithmeticError::Overflow)
    }

    pub fn neg(&mut self, rhs: i64) -> Option<i64> {
        self.report_err_if(rhs.checked_neg(), ArithmeticError::Overflow)
    }
}

impl Analysis<Math> for ConstantFolding {
    type Data = Option<i64>;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> egg::DidMerge {
        egg::merge_option(to, from, |a, b| {
            assert_eq!(*a, b, "merged non-equal constants");
            egg::DidMerge(false, false)
        })
    }

    fn make(egraph: &mut EGraph<Math, Self>, enode: &Math, _: Id) -> Self::Data {
        match enode {
            Math::Add([a, b]) => {
                let (a, b) = (egraph[*a].data?, egraph[*b].data?);
                egraph.analysis.add(a, b)
            },
            Math::Sub([a, b]) => {
                let (a, b) = (egraph[*a].data?, egraph[*b].data?);
                egraph.analysis.sub(a, b)
            },
            Math::Mul([a, b]) => {
                let (a, b) = (egraph[*a].data?, egraph[*b].data?);
                egraph.analysis.mul(a, b)
            },
            Math::Div([a, b]) => {
                let (a, b) = (egraph[*a].data?, egraph[*b].data?);
                egraph.analysis.div(a, b)
            },
            Math::Rem([a, b]) => {
                let (a, b) = (egraph[*a].data?, egraph[*b].data?);
                egraph.analysis.rem(a, b)
            },
            Math::Pow([a, b]) => {
                let (a, b) = (egraph[*a].data?, egraph[*b].data?);
                egraph.analysis.pow(a, b)
            },
            Math::Neg(a) => egraph.analysis.neg(egraph[*a].data?),
            Math::Num(n) => egraph.analysis.num(*n),
            Math::Sym(..) | Math::Fn(..) => None,
        }
    }

    fn modify(egraph: &mut EGraph<Math, Self>, id: Id) {
        if let Some(n) = egraph[id].data {
            let mut added = egraph.add(Math::Num(n.unsigned_abs()));

            if n.is_negative() {
                added = egraph.add(Math::Neg(added))
            }
            egraph.union(id, added);

            // Prune non-leaf nodes.          
            egraph[id].nodes.retain(|n| {
                dbg!(n, n.is_leaf() || matches!(n, Math::Neg(_)));
                n.is_leaf() || matches!(n, Math::Neg(_))
            });

            #[cfg(debug_assertions)]
            egraph[id].assert_unique_leaves();
        }
    }
}
