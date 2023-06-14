use crate::token_type::TokenType;
#[derive(Debug)]
pub struct Token<'a> {
    token_type: TokenType<'a>,
    lexeme: &'a str,
    line: usize,
}

impl<'a> Token<'a> {
    fn new(token_type: TokenType<'a>, lexeme: &'a str, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}
