use std::fmt::{self, Debug, Display, Write};
use std::hash::Hash;

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
            _ => None,
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

impl fmt::Display for NodeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeKind::Number(num) => write!(f, "{num}"),
            NodeKind::Symbol(sym) => write!(f, "{sym}"),
            NodeKind::Operator(op) => write!(f, "{op}"),
            NodeKind::Function { name } => write!(f, "{name}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Node {
    kind: NodeKind,
    children: SmallVec<[Id; 2]>,
}

impl Node {
    pub fn new(kind: NodeKind, children: Vec<Id>) -> Node {
        Node {
            kind,
            children: SmallVec::from_vec(children),
        }
    }

    pub fn leaf(kind: NodeKind) -> Node {
        Node {
            kind,
            children: SmallVec::new(),
        }
    }

    pub fn unary(kind: NodeKind, arg: Id) -> Node {
        Node {
            kind,
            children: SmallVec::from_buf_and_len([arg, Id::default()], 1),
        }
    }

    pub fn binary(kind: NodeKind, l_arg: Id, r_arg: Id) -> Node {
        Node {
            kind,
            children: SmallVec::from_buf([l_arg, r_arg]),
        }
    }

    pub fn kind(&self) -> NodeKind {
        self.kind
    }
}

impl Language for Node {
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

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.kind, f)
    }
}
