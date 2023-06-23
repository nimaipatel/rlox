use core::fmt;
use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum ScanError {
    UnexpectedChar(char, usize),
}

impl Error for ScanError {}

impl Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScanError::UnexpectedChar(c, line) => {
                write!(
                    f,
                    "Found unexpected character {} while scanning on line {}",
                    c, line
                )
            }
        }
    }
}
