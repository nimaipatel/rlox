use crate::token_type::TokenType;
#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    token_type: TokenType<'a>,
    lexeme: &'a [u8],
    line: usize,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType<'a>, lexeme: &'a [u8], line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}
