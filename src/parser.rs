use std::iter::Peekable;
use std::str::Chars;

use crate::math::{Math, Op};
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
    Comma,
}

pub fn scan_token<'i>(chars: &mut Peekable<Chars<'i>>) -> Result<Option<Token>, ParseError> {
    while chars.next_if(|c| c.is_whitespace()).is_some() {}

    let token = match chars.next() {
        None => None,
        Some(first) => match first {
            '+' => Some(Token::Op(Op::Add)),
            '-' => Some(Token::Op(Op::Sub)),
            '*' => Some(Token::Op(Op::Mul)),
            '/' => Some(Token::Op(Op::Div)),
            '^' => Some(Token::Op(Op::Pow)),
            ',' => Some(Token::Comma),
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



pub fn tokenize_iter(input: &str) -> impl Iterator<Item = Result<Token, ParseError>> {
    let mut chars = input.chars().peekable();

    std::iter::from_fn(move || {
        scan_token(&mut chars).transpose()
    })
}

pub type Expr = egg::RecExpr<Math>;
pub type Pattern = egg::Pattern<Math>;
type TokenIter = Peekable<std::vec::IntoIter<Token>>;

/// Parse a string into a `Pattern`.
///
/// The resulting `Pattern` can be used as either the left-hand or
/// right-hand side of a rewrite rule.
///
/// # Errors
///
/// Returns a `ParseError` if the input contains unexpected tokens,
/// unbalanced parentheses, or other syntax errors.
pub fn parse_pattern(input: &str) -> Result<Pattern, ParseError> {
    let mut tokens = tokenize(input)?.into_iter().peekable();
    let mut ast = PatternAst::default();
    parse_pratt(&mut tokens, &mut ast, 0)?;
    Ok(Pattern::new(ast))
}

/// Parse a string into an `Expr`.
///
/// An expression is similar to a pattern, except it cannot contain any
/// pattern variables. This produces a concrete expression tree that can
/// be added to an e-graph.
///
/// # Errors
///
/// Returns a `ParseError` if the input is malformed or if a pattern
/// variable is encountered.
pub fn parse_expr(input: &str) -> Result<Expr, ParseError> {
    let mut tokens = tokenize(input)?.into_iter().peekable();
    let mut ast = PatternAst::default();
    parse_pratt(&mut tokens, &mut ast, 0)?;
    ast.try_into().map_err(ParseError::VariableInExpr)
}

fn parse_pratt(
    tokens: &mut TokenIter,
    ast: &mut PatternAst<Math>,
    min_bp: u8,
) -> Result<Id, ParseError> {
    use Token as T;

    let mut lhs = match tokens.next() {
        // Number literal.
        Some(T::Num(num)) => {
            let node = Math::Num(num);
            ast.add(ENodeOrVar::ENode(node))
        }
        // Identifier or function call.
        Some(T::Ident(sym)) => {
            let node = if let Some(&T::LParen) = tokens.peek() {
                tokens.next(); // Skip '('

                let mut args = Vec::new();
                loop {
                    let arg = parse_pratt(tokens, ast, 0)?;
                    args.push(arg);
                    match tokens.next() {
                        Some(T::Comma) => (),
                        Some(T::RParen) => break,
                        Some(t) => return Err(ParseError::UnexpectedToken(t)),
                        None => return Err(ParseError::UnexpectedEof),
                    }
                }
                Math::Fn(sym.into(), args)
            } else {
                Math::Sym(sym.into())
            };

            ast.add(ENodeOrVar::ENode(node))
        }
        // Variable (part of a pattern).
        Some(T::Var(var)) => ast.add(ENodeOrVar::Var(var)),
        // Parenthesized sub-expression: recurse, then expect ')'.
        Some(T::LParen) => {
            let lhs = parse_pratt(tokens, ast, 0)?;
            match tokens.next() {
                Some(T::RParen) => lhs,
                Some(t) => return Err(ParseError::UnexpectedToken(t)),
                None => return Err(ParseError::UnexpectedEof),
            }
        }
        // Prefix operator: parse its operand recursively.
        Some(T::Op(op)) => {
            let prec = op
                .prefix_prec()
                .ok_or(ParseError::InvalidPrefixOp(op))?;
            let arg = parse_pratt(tokens, ast, prec.left_bp())?;
            let node = Math::from_unary_op(op, arg).unwrap();
            ast.add(ENodeOrVar::ENode(node))
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
            Some(T::RParen) | None => break,
            Some(_) => {
                let t = tokens.next().unwrap();
                return Err(ParseError::UnexpectedToken(t))
            },
        };

        let prec = op
            .infix_prec()
            .ok_or(ParseError::InvalidInfixOp(op))?;
        // If the precedence is too low, return control to caller.
        if prec.left_bp() < min_bp {
            break;
        }
        tokens.next();

        // Parse the right-hand side with the right binding power.
        let rhs = parse_pratt(tokens, ast, prec.right_bp())?;
        let node = Math::from_binary_op(op, lhs, rhs).unwrap();

        // Update lhs to the new combined binary expression.
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

    #[test]
    fn parse_function() {
        let s = parse_expr("sin(x) + max(x, y)").unwrap();
        assert_eq!(s.to_string(), "(+ (sin x) (max x y))")
    }
}
