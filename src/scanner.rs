use crate::{error, token::Token, token_type::TokenType};

pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let tokens = Vec::new();
        Self {
            source,
            tokens,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::EOF, b"", self.line));
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> Option<u8> {
        let result = self.source.as_bytes().get(self.current);
        self.current += 1;
        result.copied()
    }

    fn add_token(&mut self, token_type: TokenType<'a>) {
        let lexeme = &self.source.as_bytes()[self.start..self.current];
        let token = Token::new(token_type, lexeme, self.line);
        self.tokens.push(token);
    }

    fn scan_token(&mut self) {
        if let Some(c) = self.advance() {
            match c {
                b'(' => self.add_token(TokenType::LeftParen),
                b')' => self.add_token(TokenType::RightParen),
                b'{' => self.add_token(TokenType::LeftBrace),
                b'}' => self.add_token(TokenType::RightBrace),
                b',' => self.add_token(TokenType::COMMA),
                b'.' => self.add_token(TokenType::DOT),
                b'-' => self.add_token(TokenType::MINUS),
                b'+' => self.add_token(TokenType::PLUS),
                b';' => self.add_token(TokenType::SEMICOLON),
                b'*' => self.add_token(TokenType::STAR),
                // b'!' => self.add_token({
                //     let cond = self.token_matches(b'=');
                //     if cond {
                //         TokenType::BangEqual
                //     } else {
                //         TokenType::BANG
                //     }
                // }),
                _ => error(self.line, "Unexpected character."),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_left_right_paren_no_spaces() {
        let source = "()";
        let mut scanner = Scanner::new(source);
        scanner.scan_tokens();
        let exp_out = vec![
            Token::new(TokenType::LeftParen, b"(", 1),
            Token::new(TokenType::RightParen, b")", 1),
            Token::new(TokenType::EOF, b"", 1),
        ];
        assert_eq!(scanner.tokens, exp_out);
    }
}
