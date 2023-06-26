use core::fmt;
use std::{error::Error, fmt::Display};

use crate::{token::Token, token_type::TokenType};

#[derive(Debug, PartialEq)]
pub enum ParseError<'a> {
    UnexpectedEndOfInput {
        expected: &'static str,
    },
    InvalidToken {
        token: &'a Token<'a>,
    },
    ExpectedSomething {
        actual: &'a Token<'a>,
        expected: &'a TokenType<'a>,
    },
    InvalidAssignment {
        equals: &'a Token<'a>,
    },
}

impl<'a> Error for ParseError<'a> {}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedEndOfInput { expected } => {
                write! {f, "Unexpected end of input, expected {} token", expected}
            }
            ParseError::InvalidAssignment { equals } => {
                write!(f, "Invalid assignment target on line {}", equals.line)
            }
            ParseError::InvalidToken { token } => {
                write!(
                    f,
                    "Found invalid token {} on line {}",
                    token.lexeme, token.line
                )
            }
            ParseError::ExpectedSomething { actual, expected } => {
                write!(
                    f,
                    "Found token {} but expected {:?} on line {}",
                    actual.lexeme, expected, actual.line
                )
            }
        }
    }
}
