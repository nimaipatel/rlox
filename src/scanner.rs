use std::error::Error;

use crate::{
    token::{self, Token},
    token_type::{string_to_keyword, TokenType},
};

pub fn scan<'a>(source: &'a str) -> Result<Vec<Token<'a>>, Box<dyn Error>> {
    let mut line = 1;
    let mut tokens = Vec::new();
    let mut chars = source.char_indices().peekable();
    while let Some((cur_idx, c)) = chars.next() {
        let mut end_idx = cur_idx + 1;
        match c {
            '{' => {
                tokens.push(Token::new(
                    TokenType::LeftBrace,
                    &source[cur_idx..end_idx],
                    line,
                ));
            }
            '}' => {
                tokens.push(Token::new(
                    TokenType::RightBrace,
                    &source[cur_idx..end_idx],
                    line,
                ));
            }
            '(' => {
                tokens.push(Token::new(
                    TokenType::LeftParen,
                    &source[cur_idx..end_idx],
                    line,
                ));
            }
            ')' => {
                tokens.push(Token::new(
                    TokenType::RightParen,
                    &source[cur_idx..end_idx],
                    line,
                ));
            }
            '=' => {
                Token::new(TokenType::Bang, &source[cur_idx..end_idx], line);
                if let Some((_, '=')) = chars.clone().next() {
                    end_idx += 1;
                    tokens.push(Token::new(
                        TokenType::EqualEqual,
                        &source[cur_idx..end_idx],
                        line,
                    ));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::Equal, &source[cur_idx..end_idx], line));
                }
            }
            '!' => {
                Token::new(TokenType::Bang, &source[cur_idx..end_idx], line);
                if let Some((_, '=')) = chars.clone().next() {
                    end_idx += 1;
                    tokens.push(Token::new(
                        TokenType::BangEqual,
                        &source[cur_idx..end_idx],
                        line,
                    ));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::Bang, &source[cur_idx..end_idx], line));
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
                    tokens.push(Token::new(
                        TokenType::Slash,
                        &source[cur_idx..end_idx],
                        line,
                    ));
                }
            }
            '"' => {
                while let Some((_, maybe_end_quote)) = chars.next() {
                    end_idx += 1;
                    if maybe_end_quote == '"' {
                        let lexeme = &source[cur_idx..end_idx];
                        tokens.push(Token::new(
                            TokenType::String(&lexeme[1..lexeme.len() - 1]),
                            lexeme,
                            line,
                        ));
                        break;
                    } else if maybe_end_quote == '\n' {
                        line += 1;
                    }
                }
            }
            some_digit if some_digit.is_digit(10) => {
                while let Some((_, maybe_digit)) = chars.peek() {
                    if maybe_digit.is_digit(10) {
                        end_idx += 1;
                        chars.next();
                    } else {
                        break;
                    }
                }
                let lexeme = &source[cur_idx..end_idx];
                tokens.push(Token::new(
                    TokenType::Number(lexeme.parse().unwrap()),
                    lexeme,
                    line,
                ))
            }
            some_alpha if some_alpha.is_alphabetic() => {
                while let Some((_, maybe_alnum)) = chars.peek() {
                    if maybe_alnum.is_alphanumeric() {
                        end_idx += 1;
                        chars.next();
                    } else {
                        break;
                    }
                }
                let lexeme = &source[cur_idx..end_idx];
                match string_to_keyword(lexeme) {
                    Some(keyword) => tokens.push(Token::new(keyword, lexeme, line)),
                    None => tokens.push(Token::new(TokenType::Identifier, lexeme, line)),
                }
            }
            '\n' => line += 1,
            ' ' => (),
            '\r' => (),
            '\t' => (),

            not_impl => todo!("haven't implemented for {}", not_impl),
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
            Token::new(TokenType::Number(456), "456", 1),
            Token::new(TokenType::BangEqual, "!=", 1),
            Token::new(TokenType::Number(123), "123", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn something_then_comment_then_something() {
        let source = "456!=123// this is a comment\n789!=789";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::Number(456), "456", 1),
            Token::new(TokenType::BangEqual, "!=", 1),
            Token::new(TokenType::Number(123), "123", 1),
            Token::new(TokenType::Number(789), "789", 2),
            Token::new(TokenType::BangEqual, "!=", 2),
            Token::new(TokenType::Number(789), "789", 2),
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
            Token::new(TokenType::Number(69), "69", 1),
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
            Token::new(TokenType::Number(69), "69", 1),
            Token::new(TokenType::LeftBrace, "{", 1),
            Token::new(TokenType::RightBrace, "}", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }
}
