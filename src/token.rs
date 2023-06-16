use crate::token_type::TokenType;
#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    pub token_type: TokenType<'a>,
    lexeme: &'a str,
    line: usize,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType<'a>, lexeme: &'a str, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}
