mod scan_error;
mod tests;

use crate::{
    token::Token,
    token_type::{string_to_keyword, TokenType},
};

use self::scan_error::ScanError;

pub fn scan<'a>(src: &'a str) -> Result<Vec<Token<'a>>, ScanError> {
    let mut line = 1;
    let mut tokens = Vec::new();
    let mut chars = src.char_indices().peekable();
    while let Some((idx, c)) = chars.next() {
        match c {
            '{' => {
                tokens.push(Token::new(TokenType::LeftBrace, &src[idx..=idx], line));
            }
            '}' => {
                tokens.push(Token::new(TokenType::RightBrace, &src[idx..=idx], line));
            }
            '(' => {
                tokens.push(Token::new(TokenType::LeftParen, &src[idx..=idx], line));
            }
            ')' => {
                tokens.push(Token::new(TokenType::RightParen, &src[idx..=idx], line));
            }
            '.' => {
                tokens.push(Token::new(TokenType::Dot, &src[idx..=idx], line));
            }
            '-' => {
                tokens.push(Token::new(TokenType::Minus, &src[idx..=idx], line));
            }
            '+' => {
                tokens.push(Token::new(TokenType::Plus, &src[idx..=idx], line));
            }
            ',' => {
                tokens.push(Token::new(TokenType::Comma, &src[idx..=idx], line));
            }
            ';' => {
                tokens.push(Token::new(TokenType::Semicolon, &src[idx..=idx], line));
            }
            '*' => {
                tokens.push(Token::new(TokenType::Star, &src[idx..=idx], line));
            }
            '=' => {
                if let Some((end_idx, '=')) = chars.peek() {
                    tokens.push(Token::new(
                        TokenType::EqualEqual,
                        &src[idx..=*end_idx],
                        line,
                    ));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::Equal, &src[idx..=idx], line));
                }
            }
            '!' => {
                if let Some((end_idx, '=')) = chars.peek() {
                    tokens.push(Token::new(TokenType::BangEqual, &src[idx..=*end_idx], line));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::Bang, &src[idx..=idx], line));
                }
            }
            '>' => {
                if let Some((end_idx, '=')) = chars.peek() {
                    tokens.push(Token::new(
                        TokenType::GreaterEqual,
                        &src[idx..=*end_idx],
                        line,
                    ));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::Greater, &src[idx..=idx], line));
                }
            }
            '<' => {
                if let Some((end_idx, '=')) = chars.peek() {
                    tokens.push(Token::new(TokenType::LessEqual, &src[idx..=*end_idx], line));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::Less, &src[idx..=idx], line));
                }
            }
            '/' => {
                if let Some((_, '/')) = chars.peek() {
                    while let Some((_, maybe_newline)) = chars.peek() {
                        if *maybe_newline == '\n' {
                            break;
                        } else {
                            chars.next();
                        }
                    }
                } else {
                    tokens.push(Token::new(TokenType::Slash, &src[idx..=idx], line));
                }
            }
            '"' => {
                while let Some((end_idx, maybe_end_quote)) = chars.next() {
                    match maybe_end_quote {
                        '"' => {
                            let lexeme = &src[idx..=end_idx];
                            tokens.push(Token::new(
                                TokenType::String(&lexeme[1..lexeme.len() - 1]),
                                lexeme,
                                line,
                            ));
                            break;
                        }
                        '\n' => line += 1,
                        _ => (),
                    }
                }
            }
            some_digit if some_digit.is_digit(10) => {
                let mut end_idx = idx;
                let mut peek = chars.clone();
                let mut peekpeek = chars.clone().skip(1);
                'num_loop: while let (Some((_, peek)), peekpeek) = (peek.next(), peekpeek.next()) {
                    match (peek, peekpeek) {
                        (digit, _) if digit.is_digit(10) => {
                            end_idx += 1;
                            chars.next();
                        }
                        ('.', Some((_, digit))) if digit.is_digit(10) => {
                            end_idx += 1; // consume the '.'
                            chars.next();

                            loop {
                                let peek = chars.peek();
                                match peek {
                                    Some((_, digit)) if digit.is_digit(10) => {
                                        end_idx += 1;
                                        chars.next();
                                    }
                                    _ => break 'num_loop,
                                }
                            }
                        }
                        _ => break 'num_loop,
                    }
                }
                let lexeme = &src[idx..=end_idx];
                tokens.push(Token::new(
                    TokenType::Number(lexeme.parse().unwrap()),
                    lexeme,
                    line,
                ));
            }
            some_alpha if some_alpha.is_alphabetic() => {
                let mut end_idx = idx;
                while let Some((_, maybe_alnum)) = chars.peek() {
                    if !maybe_alnum.is_alphanumeric() {
                        break;
                    }
                    end_idx += 1;
                    chars.next();
                }
                let lexeme = &src[idx..=end_idx];
                match string_to_keyword(lexeme) {
                    Some(keyword) => tokens.push(Token::new(keyword, lexeme, line)),
                    None => tokens.push(Token::new(TokenType::Identifier, lexeme, line)),
                }
            }
            '\n' => line += 1,
            ' ' => (),
            '\r' => (),
            '\t' => (),

            c => return Err(ScanError::UnexpectedChar(c, line)),
        }
    }
    tokens.push(Token::new(TokenType::Eof, "", line));
    Ok(tokens)
}
