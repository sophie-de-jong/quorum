use egg::{EGraph, Id, Language, PatternAst, Subst, define_language};

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::str::FromStr;

use crate::math::{ConstantFolding, Math};

define_language! {
    pub enum Logic<L> {
        // An enode from the underlying `Language`
        ENode(L),

        // Logical operators
        "!" = Not(Id),
        "&&" = And([Id; 2]),
        "||" = Or([Id; 2]),

        // Comparison operators
        ">" = Gt([Id; 2]),
        "<" = Lt([Id; 2]),
        ">=" = Ge([Id; 2]),
        "<=" = Le([Id; 2]),
        "==" = Eq([Id; 2]),
        "!=" = Ne([Id; 2]),
    }
    where
        L: Language + FromStr + Display,
        <L as FromStr>::Err: Debug
}

// Dummy implementation of `FromStr` required by the `egg::define_language` macro.
impl<L> FromStr for Logic<L> {
    type Err = std::convert::Infallible;

    fn from_str(_: &str) -> Result<Self, Self::Err> {
        unreachable!("`Logic::<L>::from_str` should never be called")
    }
}

#[non_exhaustive]
pub struct Condition {
    pub ast: ConditionAst,
}

pub type ConditionAst = PatternAst<Logic<Math>>;

impl Condition {
    pub fn new(ast: ConditionAst) -> Self {
        Condition { ast }
    }
}

impl egg::Condition<Math, ConstantFolding> for Condition {
    fn check(&self, egraph: &mut EGraph<Math, ConstantFolding>, eclass: Id, subst: &Subst) -> bool {
        todo!()
    }
}
