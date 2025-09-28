use std::iter::Peekable;
use std::str::Chars;

use crate::math::{Node, NodeKind, Op};
use egg::{ENodeOrVar, Id, PatternAst, Symbol, Var};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("unexpected end of file")]
    UnexpectedEof,
    #[error("unexpected char: {0:?}")]
    UnexpectedChar(char),
    #[error("unexpected token: {0:?}")]
    UnexpectedToken(Token),
    #[error("variable in expression: {0:?}")]
    VariableInExpr(Var),
    #[error("invalid prefix operator: {0:?}")]
    InvalidPrefixOp(Op),
    #[error("invalid infix operator: {0:?}")]
    InvalidInfixOp(Op),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Num(i64),
    Ident(String),
    Var(Var),
    Op(Op),
    LParen,
    RParen,
}

pub fn scan_token<'i>(chars: &mut Peekable<Chars<'i>>) -> Result<Option<Token>, ParseError> {
    while chars.next_if(|c| c.is_whitespace()).is_some() {}

    let token = match chars.next() {
        None => None,
        Some(first) => match first {
            '+' => Some(Token::Op(Op::Plus)),
            '-' => Some(Token::Op(Op::Minus)),
            '*' => Some(Token::Op(Op::Star)),
            '/' => Some(Token::Op(Op::Slash)),
            '^' => Some(Token::Op(Op::Caret)),
            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            a if a.is_alphabetic() && a.is_lowercase() => {
                let mut ident = String::new();
                ident.push(a);
                while let Some(a) =
                    chars.next_if(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || *c == '_')
                {
                    ident.push(a);
                }
                Some(Token::Ident(ident))
            }
            a if a.is_alphabetic() && a.is_uppercase() => {
                let mut var = String::new();
                var.push(a);
                while let Some(a) =
                    chars.next_if(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || *c == '_')
                {
                    var.push(a);
                }
                Some(Token::Var(Var::from(Symbol::from(var))))
            }
            d if d.is_ascii_digit() => {
                let mut num = String::new();
                num.push(d);
                while let Some(d) = chars.next_if(|c| c.is_ascii_digit()) {
                    num.push(d);
                }
                Some(Token::Num(num.parse().unwrap()))
            }
            bad_char => return Err(ParseError::UnexpectedChar(bad_char)),
        },
    };
    Ok(token)
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, ParseError> {
    let mut chars = input.chars().peekable();
    let mut tokens = Vec::new();

    while let Some(token) = scan_token(&mut chars)? {
        tokens.push(token);
    }
    Ok(tokens)
}

pub type Expr = egg::RecExpr<Node>;
pub type Pattern = egg::Pattern<Node>;
type TokenIter = Peekable<std::vec::IntoIter<Token>>;

pub fn parse_pattern(input: &str) -> Result<Pattern, ParseError> {
    let mut tokens = tokenize(input)?.into_iter().peekable();
    let mut ast = PatternAst::default();
    parse_inner(&mut tokens, &mut ast, 0)?;
    Ok(Pattern::new(ast))
}

pub fn parse_expr(input: &str) -> Result<Expr, ParseError> {
    let mut tokens = tokenize(input)?.into_iter().peekable();
    let mut ast = PatternAst::default();
    parse_inner(&mut tokens, &mut ast, 0)?;
    ast.try_into().map_err(ParseError::VariableInExpr)
}

fn parse_inner(
    tokens: &mut TokenIter,
    ast: &mut PatternAst<Node>,
    min_bp: u8,
) -> Result<Id, ParseError> {
    use Token as T;

    let mut lhs = match tokens.next() {
        Some(T::Num(num)) => {
            let node = Node::leaf(NodeKind::Number(num));
            ast.add(ENodeOrVar::ENode(node))
        }
        Some(T::Ident(sym)) => {
            let node = Node::leaf(NodeKind::Symbol(sym.into()));
            ast.add(ENodeOrVar::ENode(node))
        }
        Some(T::Var(var)) => ast.add(ENodeOrVar::Var(var)),
        Some(T::LParen) => {
            let lhs = parse_inner(tokens, ast, 0)?;
            match tokens.next() {
                Some(T::RParen) => lhs,
                Some(t) => return Err(ParseError::UnexpectedToken(t)),
                None => return Err(ParseError::UnexpectedEof),
            }
        }
        Some(T::Op(op)) => {
            let r_bp = op
                .prefix_binding_power()
                .ok_or(ParseError::InvalidPrefixOp(op))?;
            let arg = parse_inner(tokens, ast, r_bp)?;
            let node = Node::unary(NodeKind::Operator(op), arg);
            ast.add(ENodeOrVar::ENode(node))
        }
        Some(t) => return Err(ParseError::UnexpectedToken(t)),
        None => return Err(ParseError::UnexpectedEof),
    };

    loop {
        let op = match tokens.peek() {
            Some(T::Op(op)) => *op,
            Some(T::RParen) => break,
            Some(_) => return Err(ParseError::UnexpectedToken(tokens.next().unwrap())),
            None => break,
        };

        let (l_bp, r_bp) = op
            .infix_binding_power()
            .ok_or(ParseError::InvalidInfixOp(op))?;
        if l_bp < min_bp {
            break;
        }
        tokens.next();

        let rhs = parse_inner(tokens, ast, r_bp)?;
        let node = Node::binary(NodeKind::Operator(op), lhs, rhs);
        lhs = ast.add(ENodeOrVar::ENode(node))
    }

    Ok(lhs)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_single_number() {
        let s = parse_expr("1").unwrap();
        assert_eq!(s.to_string(), "1");
    }

    #[test]
    fn parse_addition_and_multiplication_precedence() {
        let s = parse_expr("1 + 2 * 3").unwrap();
        assert_eq!(s.to_string(), "(+ 1 (* 2 3))");
    }

    #[test]
    fn parse_complex_expression_with_left_associativity() {
        let s = parse_expr("a + b * c * d + e").unwrap();
        assert_eq!(s.to_string(), "(+ (+ a (* (* b c) d)) e)");
    }

    #[test]
    fn parse_right_associative_operator() {
        let s = parse_expr("f ^ g ^ h").unwrap();
        assert_eq!(s.to_string(), "(^ f (^ g h))");
    }

    #[test]
    fn parse_complex_expression_with_left_and_right_associativity() {
        let s = parse_expr(" 1 + 2 + f ^ g ^ h * 3 * 4").unwrap();
        assert_eq!(s.to_string(), "(+ (+ 1 2) (* (* (^ f (^ g h)) 3) 4))");
    }

    #[test]
    fn parse_double_negation_with_multiplication() {
        let s = parse_expr("--1 * 2").unwrap();
        assert_eq!(s.to_string(), "(* (- (- 1)) 2)");
    }

    #[test]
    fn parse_double_negation_with_right_associativity() {
        let s = parse_expr("--f ^ g").unwrap();
        assert_eq!(s.to_string(), "(- (- (^ f g)))");
    }

    #[test]
    fn parse_redundant_parentheses() {
        let s = parse_expr("(((0)))").unwrap();
        assert_eq!(s.to_string(), "0");
    }

    #[test]
    fn parse_parenthesized() {
        let s = parse_expr("(1 + 2) * 3").unwrap();
        assert_eq!(s.to_string(), "(* (+ 1 2) 3)");

        let s = parse_expr("1 + (2 * 3)").unwrap();
        assert_eq!(s.to_string(), "(+ 1 (* 2 3))");
    }

    #[test]
    fn parse_complex_pattern() {
        let p = parse_pattern("A + B * c * D + E").unwrap();
        assert_eq!(p.to_string(), "(+ (+ A (* (* B c) D)) E)");
    }
}
