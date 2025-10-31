use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

use egg::{Symbol, Var};
use unicode_xid::UnicodeXID;

use super::op::OpSymbol;
use super::pratt::ParseError;

pub(super) type TokenIter = Peekable<std::vec::IntoIter<Token>>;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Num(u64),
    Ident(Symbol),
    Var(Var),
    Op(OpSymbol),
    LParen,
    RParen,
    Comma,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Num(num) => write!(f, "a number: `{num}`"),
            Token::Ident(ident) => write!(f, "an identifier: `{ident}`"),
            Token::Var(var) => write!(f, "a variable: `{var}`"),
            Token::Op(op) => write!(f, "an operator: `{op}`"),
            Token::LParen => write!(f, "`(`"),
            Token::RParen => write!(f, "`)`"),
            Token::Comma => write!(f, "`,`"),
        }
    }
}

fn is_id_start(c: char) -> bool {
    c.is_xid_start() && c.is_lowercase()
}

fn is_id_continue(c: char) -> bool {
    (c.is_xid_continue() && c.is_lowercase()) || c == '_'
}

fn is_var_start(c: char) -> bool {
    c.is_xid_start() && c.is_uppercase()
}

fn is_var_continue(c: char) -> bool {
    (c.is_xid_continue() && c.is_uppercase()) || c == '_'
}

fn fill_buf_while<'a, F: FnMut(char) -> bool>(
    chars: &mut Peekable<Chars<'a>>,
    buf: &mut String,
    mut f: F,
) {
    while let Some(c) = chars.next_if(|&c| f(c)) {
        buf.push(c);
    }
}

fn scan_token<'a>(
    chars: &mut Peekable<Chars<'a>>,
    buf: &mut String,
) -> Result<Option<Token>, ParseError> {
    while chars.next_if(|c| c.is_whitespace()).is_some() {}

    let token = match chars.next() {
        None => None,
        Some(first) => match first {
            ',' => Some(Token::Comma),
            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            '+' => Some(Token::Op(OpSymbol::Plus)),
            '-' => Some(Token::Op(OpSymbol::Minus)),
            '%' => Some(Token::Op(OpSymbol::Percent)),
            '*' => Some(Token::Op(OpSymbol::Star)),
            '/' => Some(Token::Op(OpSymbol::Slash)),
            '^' => Some(Token::Op(OpSymbol::Caret)),
            '>' => {
                if let Some('=') = chars.peek() {
                    Some(Token::Op(OpSymbol::Greater))
                } else {
                    Some(Token::Op(OpSymbol::GreaterEqual))
                }
            }
            '<' => {
                if let Some('=') = chars.peek() {
                    Some(Token::Op(OpSymbol::Less))
                } else {
                    Some(Token::Op(OpSymbol::LessEqual))
                }
            }
            '!' => {
                if let Some('=') = chars.peek() {
                    Some(Token::Op(OpSymbol::BangEqual))
                } else {
                    Some(Token::Op(OpSymbol::Bang))
                }
            }
            '=' => {
                if let Some('=') = chars.peek() {
                    Some(Token::Op(OpSymbol::EqualEqual))
                } else {
                    return Err(ParseError::InvalidChar('='));
                }
            }
            '&' => {
                if let Some('&') = chars.peek() {
                    Some(Token::Op(OpSymbol::EqualEqual))
                } else {
                    return Err(ParseError::InvalidChar('&'));
                }
            }
            '|' => {
                if let Some('|') = chars.peek() {
                    Some(Token::Op(OpSymbol::EqualEqual))
                } else {
                    return Err(ParseError::InvalidChar('|'));
                }
            }
            a if is_id_start(a) => {
                buf.push(a);
                fill_buf_while(chars, buf, is_id_continue);
                Some(Token::Ident(Symbol::new(buf)))
            }
            a if is_var_start(a) => {
                buf.push(a);
                fill_buf_while(chars, buf, is_var_continue);
                Some(Token::Var(Var::from(Symbol::new(buf))))
            }
            d if d.is_ascii_digit() => {
                buf.push(d);
                fill_buf_while(chars, buf, |c| c.is_ascii_digit());
                match buf.parse::<i64>() {
                    // We parse as an `i64` and cast to a `u64` because computation is done using
                    // `i64` and we don't want to have a panic overflow.
                    Ok(num) => Some(Token::Num(num as u64)),
                    Err(_) => return Err(ParseError::IntegerOverflow),
                }
            }
            bad_char => return Err(ParseError::InvalidChar(bad_char)),
        },
    };
    Ok(token)
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, ParseError> {
    let mut chars = input.chars().peekable();
    let mut buf = String::new();
    let mut tokens = Vec::new();

    while let Some(token) = scan_token(&mut chars, &mut buf)? {
        buf.clear();
        tokens.push(token);
    }
    Ok(tokens)
}
