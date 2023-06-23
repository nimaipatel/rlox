use std::{rc::Rc, error::Error, fmt::Display};

use crate::{token::Token, lox_type::LoxType};

#[derive(Debug)]
pub enum RunTimeError<'a> {
    WrongNumArgs {
        paren: &'a Token<'a>,
        expected: usize,
        actual: usize,
    },
    NotCallable {
        paren: &'a Token<'a>,
    },
    OperandShouldBeNumber {
        operator: &'a Token<'a>,
        operand: Rc<LoxType>,
    },
    OperandsShouldBeNumber {
        op: &'a Token<'a>,
        left: Rc<LoxType>,
        right: Rc<LoxType>,
    },
    UndefinedVariable(&'a Token<'a>),
}

impl<'a> Error for RunTimeError<'a> {}

impl<'a> Display for RunTimeError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunTimeError::OperandShouldBeNumber { operator, operand } => write!(
                f,
                "Operand for the unary operator {:?} on line {} must be number but found {:?}",
                operator.token_type, operator.line, operand
            ),
            RunTimeError::OperandsShouldBeNumber {
                op: operator,
                left,
                right,
            } => write!(
                f,
                "Operands for the binary operator {:?} on line {} must be numbers but found {:?} and {:?}",
                operator, operator.line, left, right
            ),
            RunTimeError::UndefinedVariable(name) =>
                write!(
                    f,
                    "Found undefined variable {} on line {}",
                    name.lexeme, name.line
            ),
            RunTimeError::NotCallable { paren } =>
                write!(
                    f,
                    "Can't call {} on line {} as it is not callable",
                    paren.lexeme, paren.line
            ),
            RunTimeError::WrongNumArgs { paren, expected, actual } =>
                write!(
                    f,
                    "Expected {} arguments but got {} for {} on line {}", 
                    expected, actual, paren.lexeme, paren.line
            )

        }
    }
}
