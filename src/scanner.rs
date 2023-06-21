use core::fmt;
use std::error::Error;

use crate::{
    // error,
    token::Token,
    token_type::{string_to_keyword, TokenType},
};

#[derive(Debug)]
pub enum ScanError {
    UnexpectedChar(char, usize),
}

impl Error for ScanError {}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScanError::UnexpectedChar(c, line) => {
                write!(f, "Found unexpected character {} while scanning on line {}", c, line)
            }
        }
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn leftparen_rightparen_bang() {
        let source = "()!";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::LeftParen, "(", 1),
            Token::new(TokenType::RightParen, ")", 1),
            Token::new(TokenType::Bang, "!", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn leftparen_rightparen_bangequal() {
        let source = "()!=";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::LeftParen, "(", 1),
            Token::new(TokenType::RightParen, ")", 1),
            Token::new(TokenType::BangEqual, "!=", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn number_operator_number() {
        let source = "456!=123";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::Number(456.), "456", 1),
            Token::new(TokenType::BangEqual, "!=", 1),
            Token::new(TokenType::Number(123.), "123", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn something_then_comment_then_something() {
        let source = "456!=123// this is a comment\n789!=789";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::Number(456.), "456", 1),
            Token::new(TokenType::BangEqual, "!=", 1),
            Token::new(TokenType::Number(123.), "123", 1),
            Token::new(TokenType::Number(789.), "789", 2),
            Token::new(TokenType::BangEqual, "!=", 2),
            Token::new(TokenType::Number(789.), "789", 2),
            Token::new(TokenType::Eof, "", 2),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn string_literal_nl_string_literal() {
        let source = "\"nice\"\n\"lol\"";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::String("nice"), "\"nice\"", 1),
            Token::new(TokenType::String("lol"), "\"lol\"", 2),
            Token::new(TokenType::Eof, "", 2),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn identifier() {
        let source = "nice != 69";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::Identifier, "nice", 1),
            Token::new(TokenType::BangEqual, "!=", 1),
            Token::new(TokenType::Number(69.), "69", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn test() {
        let source = "if nice == 69 {}";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::If, "if", 1),
            Token::new(TokenType::Identifier, "nice", 1),
            Token::new(TokenType::EqualEqual, "==", 1),
            Token::new(TokenType::Number(69.), "69", 1),
            Token::new(TokenType::LeftBrace, "{", 1),
            Token::new(TokenType::RightBrace, "}", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn test_decimal_token() {
        let source = "1.23 >= 3.45";
        let actual = scan(source).unwrap();
        let expected = vec![
            Token::new(TokenType::Number(1.23), "1.23", 1),
            Token::new(TokenType::GreaterEqual, ">=", 1),
            Token::new(TokenType::Number(3.45), "3.45", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(actual, expected);
    }
}
