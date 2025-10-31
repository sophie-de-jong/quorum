use std::fmt;

use super::pratt::ParseError;
use crate::math::{BinaryOp, UnaryOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpSymbol {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    AndAnd,
    OrOr,
    Bang,
    EqualEqual,
    BangEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
}

impl fmt::Display for OpSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OpSymbol::Plus => write!(f, "+"),
            OpSymbol::Minus => write!(f, "-"),
            OpSymbol::Star => write!(f, "*"),
            OpSymbol::Slash => write!(f, "/"),
            OpSymbol::Percent => write!(f, "%"),
            OpSymbol::Caret => write!(f, "^"),
            OpSymbol::AndAnd => write!(f, "&&"),
            OpSymbol::OrOr => write!(f, "||"),
            OpSymbol::Bang => write!(f, "!"),
            OpSymbol::EqualEqual => write!(f, "=="),
            OpSymbol::BangEqual => write!(f, "!="),
            OpSymbol::Greater => write!(f, ">"),
            OpSymbol::Less => write!(f, "<"),
            OpSymbol::GreaterEqual => write!(f, ">="),
            OpSymbol::LessEqual => write!(f, "<="),
        }
    }
}

impl OpSymbol {
    pub fn to_infix_op(self) -> Result<BinaryOp, ParseError> {
        match self {
            OpSymbol::Plus => Ok(BinaryOp::Add),
            OpSymbol::Minus => Ok(BinaryOp::Sub),
            OpSymbol::Star => Ok(BinaryOp::Mul),
            OpSymbol::Slash => Ok(BinaryOp::Div),
            OpSymbol::Percent => Ok(BinaryOp::Rem),
            OpSymbol::Caret => Ok(BinaryOp::Pow),
            OpSymbol::AndAnd => Ok(BinaryOp::And),
            OpSymbol::OrOr => Ok(BinaryOp::Or),
            OpSymbol::EqualEqual => Ok(BinaryOp::Eq),
            OpSymbol::BangEqual => Ok(BinaryOp::Ne),
            OpSymbol::Greater => Ok(BinaryOp::Gt),
            OpSymbol::Less => Ok(BinaryOp::Lt),
            OpSymbol::GreaterEqual => Ok(BinaryOp::Ge),
            OpSymbol::LessEqual => Ok(BinaryOp::Le),
            _ => Err(ParseError::InvalidInfixOp(self)),
        }
    }

    pub fn to_prefix_op(self) -> Result<UnaryOp, ParseError> {
        match self {
            OpSymbol::Bang => Ok(UnaryOp::Neg),
            OpSymbol::Minus => Ok(UnaryOp::Neg),
            _ => Err(ParseError::InvalidPrefixOp(self)),
        }
    }
}

impl From<BinaryOp> for OpSymbol {
    fn from(value: BinaryOp) -> Self {
        match value {
            BinaryOp::Add => OpSymbol::Plus,
            BinaryOp::Sub => OpSymbol::Minus,
            BinaryOp::Rem => OpSymbol::Percent,
            BinaryOp::Mul => OpSymbol::Star,
            BinaryOp::Div => OpSymbol::Slash,
            BinaryOp::Pow => OpSymbol::Caret,
            BinaryOp::And => OpSymbol::AndAnd,
            BinaryOp::Or => OpSymbol::OrOr,
            BinaryOp::Eq => OpSymbol::EqualEqual,
            BinaryOp::Ne => OpSymbol::BangEqual,
            BinaryOp::Gt => OpSymbol::Greater,
            BinaryOp::Lt => OpSymbol::Less,
            BinaryOp::Ge => OpSymbol::GreaterEqual,
            BinaryOp::Le => OpSymbol::LessEqual,
        }
    }
}

impl From<UnaryOp> for OpSymbol {
    fn from(value: UnaryOp) -> Self {
        match value {
            UnaryOp::Neg => OpSymbol::Minus,
            UnaryOp::Not => OpSymbol::Bang,
        }
    }
}
