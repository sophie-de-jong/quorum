use thiserror::Error;

#[derive(Debug, Error, Clone, Copy)]
pub enum ArithmeticError {
    #[error("division by zero")]
    DivisionByZero,
    #[error("overflow")]
    Overflow,
}

pub fn add(lhs: i64, rhs: i64) -> Result<i64, ArithmeticError> {
    match lhs.checked_add(rhs) {
        Some(n) => Ok(n),
        None => Err(ArithmeticError::Overflow)
    }
}

pub fn sub(lhs: i64, rhs: i64) -> Result<i64, ArithmeticError> {
    match lhs.checked_sub(rhs) {
        Some(n) => Ok(n),
        None => Err(ArithmeticError::Overflow)
    }
}

pub fn mul(lhs: i64, rhs: i64) -> Result<i64, ArithmeticError> {
    match lhs.checked_mul(rhs) {
        Some(n) => Ok(n),
        None => Err(ArithmeticError::Overflow)
    }
}

pub fn div(lhs: i64, rhs: i64) -> Result<i64, ArithmeticError> {
    if rhs == 0 {
        return Err(ArithmeticError::DivisionByZero)
    }
    match lhs.checked_div(rhs) {
        Some(n) => Ok(n),
        None => Err(ArithmeticError::Overflow)
    }
}

pub fn rem(lhs: i64, rhs: i64) -> Result<i64, ArithmeticError> {
    if rhs == 0 {
        return Err(ArithmeticError::DivisionByZero)
    }
    match lhs.checked_rem(rhs) {
        Some(n) => Ok(n),
        None => Err(ArithmeticError::Overflow)
    }
}

pub fn pow(lhs: i64, rhs: i64) -> Result<i64, ArithmeticError> {
    let exp = u32::try_from(rhs).map_err(|_| ArithmeticError::Overflow)?;
    match lhs.checked_pow(exp) {
        Some(n) => Ok(n),
        None => Err(ArithmeticError::Overflow)
    }
}

pub fn neg(rhs: i64) -> Result<i64, ArithmeticError> {
    match rhs.checked_neg() {
        Some(n) => Ok(n),
        None => Err(ArithmeticError::Overflow)
    }
}
