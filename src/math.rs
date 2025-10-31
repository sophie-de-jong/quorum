use std::fmt::{self, Debug};
use std::hash::Hash;
use std::str::FromStr;

use egg::{Analysis, EGraph, Id, Language, Symbol, define_language};
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Assoc {
    Left,
    Right,
    Both,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    And = 10,
    Or = 20,
    Eq = 30,
    Cmp = 40,
    Add = 50,
    Mul = 60,
    Neg = 70,
    Pow = 80,
    Not = 90,
}

impl Precedence {
    pub fn left_bp(self) -> u8 {
        self as u8
    }

    pub fn right_bp(self) -> u8 {
        match self {
            Precedence::Add | Precedence::Mul => self.left_bp() + 1,
            Precedence::Pow => self.left_bp() - 1,
            _ => self.left_bp(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Rem,
    Mul,
    Div,
    Pow,
    And,
    Or,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
}

impl BinaryOp {
    pub fn assoc(self) -> Assoc {
        use BinaryOp as Op;
        match self {
            Op::Add | Op::Mul | Op::Rem => Assoc::Both,
            Op::Sub | Op::Div => Assoc::Left,
            Op::Pow => Assoc::Right,
            _ => Assoc::None,
        }
    }

    pub fn precedence(self) -> Precedence {
        use BinaryOp as Op;
        match self {
            Op::And => Precedence::And,
            Op::Or => Precedence::Or,
            Op::Eq | Op::Ne => Precedence::Eq,
            Op::Gt | Op::Ge | Op::Lt | Op::Le => Precedence::Cmp,
            Op::Add | Op::Sub => Precedence::Add,
            Op::Mul | Op::Div | Op::Rem => Precedence::Mul,
            Op::Pow => Precedence::Pow,
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinaryOp as Op;
        match self {
            Op::And => write!(f, " && "),
            Op::Or => write!(f, " || "),
            Op::Eq => write!(f, " == "),
            Op::Ne => write!(f, " != "),
            Op::Gt => write!(f, " > "),
            Op::Ge => write!(f, " >= "),
            Op::Lt => write!(f, " < "),
            Op::Le => write!(f, " < "),
            Op::Add => write!(f, " + "),
            Op::Sub => write!(f, " - "),
            Op::Rem => write!(f, " % "),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Pow => write!(f, "^"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    pub fn precedence(self) -> Precedence {
        use UnaryOp as Op;
        match self {
            Op::Neg => Precedence::Neg,
            Op::Not => Precedence::Not,
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UnaryOp as Op;
        match self {
            Op::Neg => write!(f, "-"),
            Op::Not => write!(f, "~"),
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

// Dummy implementation of `FromStr` required by the `egg::define_language` macro.
impl FromStr for Math {
    type Err = std::convert::Infallible;

    fn from_str(_: &str) -> Result<Self, Self::Err> {
        unreachable!("`Math::from_str` should never be called")
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
    pub error: Option<ArithmeticError>,
}

impl ConstantFolding {
    fn report_err(&mut self, err: ArithmeticError) {
        if self.error.is_none() {
            self.error = Some(err)
        }
    }

    fn report_err_if(&mut self, opt: Option<i64>, err: ArithmeticError) -> Option<i64> {
        if let Some(n) = opt {
            Some(n)
        } else {
            self.report_err(err);
            None
        }
    }

    pub fn check_err(&self) -> Result<(), String> {
        match self.error {
            Some(err) => Err(err.to_string()),
            None => Ok(()),
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
            }
            Math::Sub([a, b]) => {
                let (a, b) = (egraph[*a].data?, egraph[*b].data?);
                egraph.analysis.sub(a, b)
            }
            Math::Mul([a, b]) => {
                let (a, b) = (egraph[*a].data?, egraph[*b].data?);
                egraph.analysis.mul(a, b)
            }
            Math::Div([a, b]) => {
                let (a, b) = (egraph[*a].data?, egraph[*b].data?);
                egraph.analysis.div(a, b)
            }
            Math::Rem([a, b]) => {
                let (a, b) = (egraph[*a].data?, egraph[*b].data?);
                egraph.analysis.rem(a, b)
            }
            Math::Pow([a, b]) => {
                let (a, b) = (egraph[*a].data?, egraph[*b].data?);
                egraph.analysis.pow(a, b)
            }
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
            egraph[id]
                .nodes
                .retain(|n| n.is_leaf() || matches!(n, Math::Neg(_)));

            #[cfg(debug_assertions)]
            egraph[id].assert_unique_leaves();
        }
    }
}
