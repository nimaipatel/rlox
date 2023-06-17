use crate::expr::Expr;
use crate::token::Token;
use crate::token_type::TokenType;
use crate::{scanner, token_type};

// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;

pub fn parse<'a>(tokens: &'a Vec<Token<'a>>) -> Result<Expr<'a>, String> {
    let parsed = parse_expression(&tokens, 0);
    match parsed {
        Err(e) => Err(e),
        Ok((parsed, _)) => Ok(parsed),
    }
}

fn parse_expression<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Expr<'a>, usize), String> {
    parse_equality(tokens, pos)
}

fn parse_equality<'a>(tokens: &'a Vec<Token<'a>>, pos: usize) -> Result<(Expr<'a>, usize), String> {
    let (mut expr, mut pos) = parse_comp(tokens, pos)?;
    loop {
        let comp_token = tokens
            .get(pos)
            .ok_or("unexpected end of input, expected equality token")?;
        if comp_token.token_type != TokenType::EqualEqual
            && comp_token.token_type != TokenType::BangEqual
        {
            break;
        }
        let (right, new_pos) = parse_comp(tokens, pos + 1)?;
        pos = new_pos;
        expr = Expr::Binary {
            left: Box::new(expr),
            op: comp_token,
            right: Box::new(right),
        };
    }
    Ok((expr, pos))
}

fn parse_comp<'a>(tokens: &'a Vec<Token<'a>>, pos: usize) -> Result<(Expr<'a>, usize), String> {
    let (mut expr, mut pos) = parse_term(tokens, pos)?;
    loop {
        let comp_token = tokens
            .get(pos)
            .ok_or("unexpected end of input, expected comparision token")?;
        if comp_token.token_type != TokenType::Greater
            && comp_token.token_type != TokenType::GreaterEqual
            && comp_token.token_type != TokenType::Less
            && comp_token.token_type != TokenType::LessEqual
        {
            break;
        }
        let (right, new_pos) = parse_term(tokens, pos + 1)?;
        pos = new_pos;
        expr = Expr::Binary {
            left: Box::new(expr),
            op: comp_token,
            right: Box::new(right),
        };
    }
    Ok((expr, pos))
}

fn parse_term<'a>(tokens: &'a Vec<Token<'a>>, pos: usize) -> Result<(Expr<'a>, usize), String> {
    let (mut expr, mut pos) = parse_factor(tokens, pos)?;
    loop {
        let op_token = tokens
            .get(pos)
            .ok_or("unexpected end of input, expected term token")?;
        if op_token.token_type != TokenType::Plus && op_token.token_type != TokenType::Minus {
            break;
        }
        let (right, new_pos) = parse_factor(tokens, pos + 1)?;
        pos = new_pos;
        expr = Expr::Binary {
            left: Box::new(expr),
            op: op_token,
            right: Box::new(right),
        }
    }
    Ok((expr, pos))
}

fn parse_factor<'a>(tokens: &'a Vec<Token<'a>>, pos: usize) -> Result<(Expr<'a>, usize), String> {
    let (mut expr, mut pos) = parse_unary(tokens, pos)?;
    loop {
        let op_token = tokens
            .get(pos)
            .ok_or("unexpected end of input, expected factor token")?;
        if op_token.token_type != TokenType::Star && op_token.token_type != TokenType::Slash {
            break;
        }
        let (right, new_pos) = parse_unary(tokens, pos + 1)?;
        pos = new_pos;
        expr = Expr::Binary {
            left: Box::new(expr),
            op: op_token,
            right: Box::new(right),
        }
    }
    Ok((expr, pos))
}

fn parse_unary<'a>(tokens: &'a Vec<Token<'a>>, pos: usize) -> Result<(Expr<'a>, usize), String> {
    let operator_token = tokens
        .get(pos)
        .ok_or("unexpected end of input, expected unary operator")?;
    match &operator_token.token_type {
        TokenType::Bang | TokenType::Minus => {
            let (right, pos) = parse_unary(tokens, pos + 1)?;
            Ok((
                Expr::Unary {
                    op: operator_token,
                    expr: Box::new(right),
                },
                pos,
            ))
        }
        _ => parse_primary(tokens, pos),
    }
}

fn parse_primary<'a>(tokens: &'a Vec<Token<'a>>, pos: usize) -> Result<(Expr<'a>, usize), String> {
    let token = tokens
        .get(pos)
        .ok_or("unexpected end of input, expected a literal")?;
    match &token.token_type {
        TokenType::False => Ok((Expr::BoolLiteral(false), pos + 1)),
        TokenType::True => Ok((Expr::BoolLiteral(true), pos + 1)),
        TokenType::Nil => Ok((Expr::NilLiteral, pos + 1)),

        TokenType::Number(n) => Ok((Expr::NumericLiteral(*n), pos + 1)),
        TokenType::String(s) => Ok((Expr::StringLiteral(s), pos + 1)),

        TokenType::LeftParen => {
            let (expr, pos) = parse_expression(tokens, pos + 1)?;
            match tokens.get(pos) {
                Some(Token {
                    token_type: TokenType::RightParen,
                    ..
                }) => Ok((Expr::Grouping(Box::new(expr)), pos + 1)),
                _ => {
                    return Err("unexpected end of input, expected right paren".to_string());
                }
            }
        }

        something_else => Err(format!(
            "invalid token {:?} while trying to parse `primary` expression ",
            something_else
        )),
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_parse_literal() {
        let source = "123\n\n";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_primary(&tokens, 0).unwrap();
        let expected = Expr::NumericLiteral(123);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_unary() {
        let source = "\n!123\n";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_unary(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Unary {
                op: Token {
                    token_type: TokenType::Bang,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_parse_factor() {
        let source = "\n123 * 123\n";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_factor(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Binary {
                op: Token {
                    token_type: TokenType::Star,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_parse_factor2() {
        let source = "\n123 / 123\n";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_factor(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Binary {
                op: Token {
                    token_type: TokenType::Slash,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_parse_term() {
        let source = "1 * 2 + 2";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_term(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Binary {
                op: Token {
                    token_type: TokenType::Plus,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_parse_term2() {
        let source = "1 - 2 * 2";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_term(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Binary {
                op: Token {
                    token_type: TokenType::Minus,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_comp() {
        let source = "1 * 2 >=  1 + 1";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_comp(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Binary {
                op: Token {
                    token_type: TokenType::GreaterEqual,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_grouping() {
        let source = "(1 + 3)";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_primary(&tokens, 0).unwrap();
        assert!(matches!(actual, Expr::Grouping(..)))
    }

    #[test]
    fn test_grouping_fail() {
        let source = "123 > (1 + 3";
        let tokens = scanner::scan(source).unwrap();
        let actual = parse(&tokens);
        assert_eq!(actual, Err("unexpected end of input, expected right paren".to_string()));
    }
}
