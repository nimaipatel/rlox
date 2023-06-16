use crate::expr::Expr;
use crate::scanner;
use crate::token::Token;
use crate::token_type::TokenType;

// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;

// fn parse<'a>(source: &str) -> Result<(Expr<'a>, usize), String> {
//     let tokens = scanner::scan(source).unwrap();
//     parse_unary(&tokens, 0)
// }

// fn parse_expression<'a>(tokens: &'a Vec<Token<'a>>, pos: usize) -> Result<(Expr<'a>, usize), String> {
//     return equality(tokens, pos);
// }

// fn equality<'a>(tokens: &'a Vec<Token<'a>>, pos: usize) -> Result<(Expr<'a>, usize), String> {
//     todo!()
// }

fn parse_factor<'a>(tokens: &'a Vec<Token<'a>>, pos: usize) -> Result<(Expr<'a>, usize), String> {
    let (mut expr, mut pos) = parse_unary(tokens, pos)?;
    loop {
        let op_token = tokens.get(pos).unwrap();
        println!("op_token = {:?}", op_token);
        if op_token.token_type != TokenType::Star && op_token.token_type != TokenType::Slash {
            break;
        }
        let (right, new_pos) = parse_unary(tokens, pos + 1).unwrap();
        pos = new_pos;
        expr = Expr::Binary {
            left: Box::new(expr),
            op: op_token,
            right: Box::new(right),
        }
    }
    Ok((expr, pos + 1))
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
                pos + 1,
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
        TokenType::False => Ok((Expr::FalseLiteral, pos + 1)),
        TokenType::True => Ok((Expr::TrueLiteral, pos + 1)),
        TokenType::Nil => Ok((Expr::NilLiteral, pos + 1)),
        TokenType::String(s) => Ok((Expr::StringLiteral(s), pos + 1)),
        TokenType::Number(n) => Ok((Expr::NumericLiteral(*n), pos + 1)),
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
        println!("actual = {:?}", actual);
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
        println!("actual = {:?}", actual);
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
}
