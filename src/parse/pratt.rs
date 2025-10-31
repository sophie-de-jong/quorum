use std::fmt::{Debug, Display};
use std::str::FromStr;

use egg::{ENodeOrVar, Id, Language, RecExpr, Symbol, Var};
use thiserror::Error;

use crate::condition::Logic;
use crate::math::{BinaryOp, Math, UnaryOp};

use super::op::OpSymbol;
use super::token::{Token, TokenIter};

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("unexpected end of file")]
    UnexpectedEof,
    #[error("invalid character: {0:?}")]
    InvalidChar(char),
    #[error("unexpected token: {0:?}")]
    UnexpectedToken(Token),
    #[error("variable in expression: {0:?}")]
    VariableInExpr(Var),
    #[error("invalid prefix operator: {0:?}")]
    InvalidPrefixOp(OpSymbol),
    #[error("invalid infix operator: {0:?}")]
    InvalidInfixOp(OpSymbol),
    #[error("integer overflow (too large)")]
    IntegerOverflow,
    #[error("unbound variable in rewrite '{0}'")]
    UnboundVariable(Symbol),
    #[error("logic operator in expression '{0}'")]
    LogicOpInExpr(OpSymbol),
}

/// A language that supports Pratt parsing.
///
/// Implementors define how to construct AST nodes for various token types.
/// This trait is designed to be used with the generic `pratt_parser` function,
/// enabling modular expression grammars (e.g., math, logic, or patterns)
pub trait PrattLanguage: Language + Sized {
    /// Constructs a numeric literal node.
    fn make_num(num: u64) -> Result<Self, ParseError>;

    /// Constructs a variable node (for use in pattern expressions).
    fn make_var(var: Var) -> Result<Self, ParseError>;

    /// Constructs a symbolic constant or identifier node.
    fn make_sym(sym: Symbol) -> Result<Self, ParseError>;

    /// Constructs a function application node.
    fn make_fn(name: Symbol, args: Vec<Id>) -> Result<Self, ParseError>;

    /// Constructs a unary operation node.
    fn make_unary_op(op: UnaryOp, id: Id) -> Result<Self, ParseError>;

    /// Constructs a binary operation node.
    fn make_binary_op(op: BinaryOp, lhs: Id, rhs: Id) -> Result<Self, ParseError>;
}

/// Pratt language implementation for mathematical expressions.
impl PrattLanguage for Math {
    fn make_num(num: u64) -> Result<Self, ParseError> {
        Ok(Math::Num(num))
    }

    fn make_var(var: Var) -> Result<Self, ParseError> {
        Err(ParseError::VariableInExpr(var))
    }

    fn make_sym(sym: Symbol) -> Result<Self, ParseError> {
        Ok(Math::Sym(sym))
    }

    fn make_fn(name: Symbol, args: Vec<Id>) -> Result<Self, ParseError> {
        Ok(Math::Fn(name, args))
    }

    fn make_unary_op(op: UnaryOp, id: Id) -> Result<Self, ParseError> {
        match op {
            UnaryOp::Neg => Ok(Math::Neg(id)),
            _ => Err(ParseError::LogicOpInExpr(op.into())),
        }
    }

    fn make_binary_op(op: BinaryOp, lhs: Id, rhs: Id) -> Result<Self, ParseError> {
        match op {
            BinaryOp::Add => Ok(Math::Add([lhs, rhs])),
            BinaryOp::Sub => Ok(Math::Sub([lhs, rhs])),
            BinaryOp::Rem => Ok(Math::Rem([lhs, rhs])),
            BinaryOp::Mul => Ok(Math::Mul([lhs, rhs])),
            BinaryOp::Div => Ok(Math::Div([lhs, rhs])),
            BinaryOp::Pow => Ok(Math::Pow([lhs, rhs])),
            _ => Err(ParseError::LogicOpInExpr(op.into())),
        }
    }
}

/// Pratt language implementation for logical expressions that embed another language `L`.
impl<L> PrattLanguage for Logic<L>
where
    L: PrattLanguage + Language + Display + FromStr,
    <L as FromStr>::Err: Debug,
{
    fn make_num(num: u64) -> Result<Self, ParseError> {
        Ok(Logic::ENode(L::make_num(num)?))
    }

    fn make_var(var: Var) -> Result<Self, ParseError> {
        Err(ParseError::VariableInExpr(var))
    }

    fn make_sym(sym: Symbol) -> Result<Self, ParseError> {
        Ok(Logic::ENode(L::make_sym(sym)?))
    }

    fn make_fn(name: Symbol, args: Vec<Id>) -> Result<Self, ParseError> {
        Ok(Logic::ENode(L::make_fn(name, args)?))
    }

    fn make_unary_op(op: UnaryOp, id: Id) -> Result<Self, ParseError> {
        match op {
            UnaryOp::Neg => Ok(Logic::ENode(L::make_unary_op(op, id)?)),
            UnaryOp::Not => Ok(Logic::Not(id)),
        }
    }

    fn make_binary_op(op: BinaryOp, lhs: Id, rhs: Id) -> Result<Self, ParseError> {
        match op {
            BinaryOp::And => Ok(Logic::And([lhs, rhs])),
            BinaryOp::Or => Ok(Logic::Or([lhs, rhs])),
            BinaryOp::Eq => Ok(Logic::Eq([lhs, rhs])),
            BinaryOp::Ne => Ok(Logic::Ne([lhs, rhs])),
            BinaryOp::Gt => Ok(Logic::Gt([lhs, rhs])),
            BinaryOp::Lt => Ok(Logic::Lt([lhs, rhs])),
            BinaryOp::Ge => Ok(Logic::Ge([lhs, rhs])),
            BinaryOp::Le => Ok(Logic::Le([lhs, rhs])),
            _ => Ok(Logic::ENode(L::make_binary_op(op, lhs, rhs)?)),
        }
    }
}

/// Pratt language implementation for patterns that embed another language `L`.
impl<L> PrattLanguage for ENodeOrVar<L>
where
    L: PrattLanguage + Language + Display + FromStr,
    <L as FromStr>::Err: Debug,
{
    fn make_num(num: u64) -> Result<Self, ParseError> {
        Ok(ENodeOrVar::ENode(L::make_num(num)?))
    }

    fn make_var(var: Var) -> Result<Self, ParseError> {
        Ok(ENodeOrVar::Var(var))
    }

    fn make_sym(sym: Symbol) -> Result<Self, ParseError> {
        Ok(ENodeOrVar::ENode(L::make_sym(sym)?))
    }

    fn make_fn(name: Symbol, args: Vec<Id>) -> Result<Self, ParseError> {
        Ok(ENodeOrVar::ENode(L::make_fn(name, args)?))
    }

    fn make_unary_op(op: UnaryOp, id: Id) -> Result<Self, ParseError> {
        Ok(ENodeOrVar::ENode(L::make_unary_op(op, id)?))
    }

    fn make_binary_op(op: BinaryOp, lhs: Id, rhs: Id) -> Result<Self, ParseError> {
        Ok(ENodeOrVar::ENode(L::make_binary_op(op, lhs, rhs)?))
    }
}

/// Parses a token stream into an expression tree using Pratt parsing.
pub fn parse_pratt<L: PrattLanguage>(tokens: &mut TokenIter) -> Result<RecExpr<L>, ParseError> {
    let mut ast = RecExpr::default();
    parse_pratt_rec(tokens, &mut ast, 0)?;
    Ok(ast)
}

fn parse_pratt_rec<L: PrattLanguage>(
    tokens: &mut TokenIter,
    ast: &mut RecExpr<L>,
    min_bp: u8,
) -> Result<Id, ParseError> {
    use Token as T;

    let mut lhs = match tokens.next() {
        // Number literal.
        Some(T::Num(num)) => {
            let node = L::make_num(num)?;
            ast.add(node)
        }
        // Identifier or function call.
        Some(T::Ident(sym)) => {
            let node = if let Some(&T::LParen) = tokens.peek() {
                tokens.next(); // Skip '('

                let mut args = Vec::new();
                loop {
                    let arg = parse_pratt_rec(tokens, ast, 0)?;
                    args.push(arg);
                    match tokens.next() {
                        Some(T::Comma) => (),
                        Some(T::RParen) => break,
                        Some(t) => return Err(ParseError::UnexpectedToken(t)),
                        None => return Err(ParseError::UnexpectedEof),
                    }
                }
                L::make_fn(sym, args)?
            } else {
                L::make_sym(sym)?
            };

            ast.add(node)
        }
        // Variable (part of a pattern).
        Some(T::Var(var)) => {
            let node = L::make_var(var)?;
            ast.add(node)
        }
        // Parenthesized sub-expression: recurse, then expect ')'.
        Some(T::LParen) => {
            let lhs = parse_pratt_rec(tokens, ast, 0)?;
            match tokens.next() {
                Some(T::RParen) => lhs,
                Some(t) => return Err(ParseError::UnexpectedToken(t)),
                None => return Err(ParseError::UnexpectedEof),
            }
        }
        // Prefix operator: parse its operand recursively.
        Some(T::Op(op)) => {
            let op = op.to_prefix_op()?;
            let prec = op.precedence();
            let arg = parse_pratt_rec(tokens, ast, prec.right_bp())?;
            let node = L::make_unary_op(op, arg)?;
            ast.add(node)
        }
        // Error cases.
        Some(t) => return Err(ParseError::UnexpectedToken(t)),
        None => return Err(ParseError::UnexpectedEof),
    };

    loop {
        // Only infix operators keep the loop going, everything else is
        // either an exit case or a fail case.
        let op = match tokens.peek() {
            Some(T::Op(op)) => *op,
            Some(T::RParen) | Some(T::Comma) | None => break,
            Some(_) => {
                let t = tokens.next().unwrap();
                return Err(ParseError::UnexpectedToken(t));
            }
        };

        let op = op.to_infix_op()?;
        let prec = op.precedence();
        // If the precedence is too low, return control to caller.
        if prec.left_bp() < min_bp {
            break;
        }
        tokens.next();

        // Parse the right-hand side with the right binding power.
        let rhs = parse_pratt_rec(tokens, ast, prec.right_bp())?;
        let node = L::make_binary_op(op, lhs, rhs)?;

        // Update lhs to the new combined binary expression.
        lhs = ast.add(node)
    }

    Ok(lhs)
}
