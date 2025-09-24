use core::num;
use std::fmt::{self, Write};

use egg::{Id, Language, Symbol};
use smallvec::SmallVec;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Percent,
}

impl Op {
    pub fn as_char(self) -> char {
        match self {
            Op::Plus => '+',
            Op::Minus => '-',
            Op::Star => '*',
            Op::Slash => '/',
            Op::Caret => '^',
            Op::Percent => '%',
        }
    }

    pub fn prefix_binding_power(self) -> Option<u8> {
        match self {
            Op::Minus => Some(5),
            _ => None,
        }
    }

    pub fn infix_binding_power(self) -> Option<(u8, u8)> {
        match self {
            Op::Plus | Op::Minus => Some((1, 2)),
            Op::Star | Op::Slash => Some((3, 4)),
            Op::Caret => Some((8, 7)),
            _ => None
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(self.as_char())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeKind {
    Number(i64),
    Symbol(Symbol),
    Operator(Op),
    Function { name: Symbol },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MathNode {
    kind: NodeKind,
    children: SmallVec<[Id; 2]>,
}

impl MathNode {
    pub fn leaf(kind: NodeKind) -> MathNode {
        MathNode {
            kind,
            children: SmallVec::new(),
        }
    }

    pub fn new(kind: NodeKind, children: SmallVec<[Id; 2]>) -> MathNode {
        MathNode {
            kind,
            children,
        }
    }

    pub fn kind(&self) -> NodeKind {
        self.kind
    }
}

impl Language for MathNode {
    type Discriminant = NodeKind;

    fn discriminant(&self) -> Self::Discriminant {
        self.kind
    }

    fn matches(&self, other: &Self) -> bool {
        self.kind == other.kind && self.children.len() == other.children.len()
    }

    fn children(&self) -> &[Id] {
        &self.children
    }

    fn children_mut(&mut self) -> &mut [Id] {
        &mut self.children
    }
}

impl fmt::Display for MathNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            NodeKind::Number(num) => write!(f, "{num}"),
            NodeKind::Symbol(sym) => write!(f, "{sym}"),
            NodeKind::Operator(op) => write!(f, "{op}"),
            NodeKind::Function { name } => write!(f, "{name}"),
        }
    }
}
