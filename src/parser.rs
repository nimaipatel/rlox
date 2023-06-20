use core::fmt;
use std::error::Error;
use std::fmt::{format, write};

use crate::expr::Expr;
use crate::stmt::{self, Stmt};
use crate::token::Token;
use crate::token_type::TokenType;
use crate::{scanner, token_type};

// program        → declaration* EOF ;

// declaration    → varDecl
//                | statement ;

// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

// exprStmt       → expression ";" ;
// printStmt      → "print" expression ";" ;

// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → "true" | "false" | "nil"
//                | NUMBER | STRING
//                | "(" expression ")"
//                | IDENTIFIER ;

// TODO: make ParseError accept the token to get line information
#[derive(Debug, PartialEq)]
pub enum ParseError<'a> {
    UnexpectedEndOfInput {
        expected: &'static str,
    },
    InvalidToken {
        token: &'a Token<'a>,
    },
    ExpectedSomething {
        actual: &'a Token<'a>,
        expected: &'a TokenType<'a>,
    },
    InvalidAssignment {
        equals: &'a Token<'a>,
    },
}

impl<'a> Error for ParseError<'a> {}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedEndOfInput { expected } => {
                write! {f, "Unexpected end of input, expected {} token", expected}
            }
            ParseError::InvalidAssignment { equals } => {
                write!(f, "Invalid assignment target on line {}", equals.line)
            }
            ParseError::InvalidToken { token } => {
                write!(
                    f,
                    "Found invalid token {} on line {}",
                    token.lexeme, token.line
                )
            }
            ParseError::ExpectedSomething { actual, expected } => {
                write!(
                    f,
                    "Found token {} but expected {:?} on line {}",
                    actual.lexeme, expected, actual.line
                )
            }
        }
    }
}

pub fn parse<'a>(tokens: &'a Vec<Token<'a>>) -> Result<Vec<Stmt<'a>>, ParseError> {
    let mut statements = Vec::new();
    let mut cur: usize = 0;
    loop {
        if tokens[cur].token_type == TokenType::Eof {
            break;
        } else {
            let (statement, new_cur) = parse_declaration(&tokens, cur)?;
            cur = new_cur;
            statements.push(statement);
        }
    }
    Ok(statements)
}

// add synchronization here
fn parse_declaration<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Stmt<'a>, usize), ParseError> {
    match tokens[pos].token_type {
        TokenType::Var => parse_var_declaration(tokens, pos + 1),
        _ => parse_statement(tokens, pos),
    }
}

fn parse_var_declaration<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Stmt<'a>, usize), ParseError> {
    let (name, pos) = consume(tokens, pos, &TokenType::Identifier)?;

    match consume(tokens, pos, &TokenType::Equal) {
        Ok((_, pos)) => {
            let (initializer, pos) = parse_expression(tokens, pos)?;
            let (_, pos) = consume(tokens, pos, &TokenType::Semicolon)?;
            Ok((Stmt::Var(name, Some(initializer)), pos))
        }
        Err(_) => {
            let (_, pos) = consume(tokens, pos, &TokenType::Semicolon)?;
            Ok((Stmt::Var(name, None), pos))
        }
    }
}

fn parse_statement<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Stmt<'a>, usize), ParseError> {
    match tokens[pos].token_type {
        TokenType::Print => parse_print_statement(tokens, pos + 1),
        _ => parse_expression_statement(tokens, pos),
    }
}

fn parse_expression_statement<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Stmt<'a>, usize), ParseError> {
    let (expr, pos) = parse_expression(tokens, pos)?;
    let (_, pos) = consume(tokens, pos, &TokenType::Semicolon)?;
    return Ok((Stmt::Expression(expr), pos));
}

fn parse_print_statement<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Stmt<'a>, usize), ParseError> {
    let (value, pos) = parse_expression(tokens, pos)?;
    let (_, pos) = consume(tokens, pos, &TokenType::Semicolon)?;
    return Ok((Stmt::Print(value), pos));
}

// TODO: replace all instances of the consuming pattern with this function
fn consume<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
    expected: &'a TokenType<'a>,
) -> Result<(&'a Token<'a>, usize), ParseError<'a>> {
    if tokens[pos].token_type == *expected {
        Ok((&tokens[pos], pos + 1))
    } else {
        Err(ParseError::ExpectedSomething {
            actual: &tokens[pos],
            expected: &expected,
        })
    }
}

fn parse_expression<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Expr<'a>, usize), ParseError> {
    parse_assignment(tokens, pos)
}

fn parse_assignment<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Expr<'a>, usize), ParseError> {
    let (expr, pos) = parse_equality(tokens, pos)?;
    match consume(tokens, pos, &TokenType::Equal) {
        Ok((equals, pos)) => {
            let (value, pos) = parse_assignment(tokens, pos)?;
            match expr {
                Expr::Variable(name) => Ok((
                    Expr::Assign {
                        name,
                        value: Box::new(value),
                    },
                    pos,
                )),
                _ => Err(ParseError::InvalidAssignment { equals }),
            }
        }
        Err(_) => Ok((expr, pos)),
    }
}

fn parse_equality<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Expr<'a>, usize), ParseError> {
    let (mut expr, mut pos) = parse_comp(tokens, pos)?;
    loop {
        let comp_token = tokens.get(pos).ok_or(ParseError::UnexpectedEndOfInput {
            expected: "equality",
        })?;
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

fn parse_comp<'a>(tokens: &'a Vec<Token<'a>>, pos: usize) -> Result<(Expr<'a>, usize), ParseError> {
    let (mut expr, mut pos) = parse_term(tokens, pos)?;
    loop {
        let comp_token = tokens.get(pos).ok_or(ParseError::UnexpectedEndOfInput {
            expected: "comparision",
        })?;
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

fn parse_term<'a>(tokens: &'a Vec<Token<'a>>, pos: usize) -> Result<(Expr<'a>, usize), ParseError> {
    let (mut expr, mut pos) = parse_factor(tokens, pos)?;
    loop {
        let op_token = tokens.get(pos).ok_or(ParseError::UnexpectedEndOfInput {
            expected: "term operator",
        })?;
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

fn parse_factor<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Expr<'a>, usize), ParseError> {
    let (mut expr, mut pos) = parse_unary(tokens, pos)?;
    loop {
        let op_token = tokens
            .get(pos)
            .ok_or(ParseError::UnexpectedEndOfInput { expected: "factor" })?;
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

fn parse_unary<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Expr<'a>, usize), ParseError> {
    let operator_token = tokens.get(pos).ok_or(ParseError::UnexpectedEndOfInput {
        expected: "unary operator",
    })?;
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

fn parse_primary<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Expr<'a>, usize), ParseError> {
    let token = tokens.get(pos).ok_or(ParseError::UnexpectedEndOfInput {
        expected: "literal",
    })?;
    match &token.token_type {
        TokenType::False => Ok((Expr::BoolLiteral(false), pos + 1)),
        TokenType::True => Ok((Expr::BoolLiteral(true), pos + 1)),
        TokenType::Nil => Ok((Expr::NilLiteral, pos + 1)),

        TokenType::Number(n) => Ok((Expr::NumericLiteral(*n), pos + 1)),
        TokenType::String(s) => Ok((Expr::StringLiteral(s), pos + 1)),

        TokenType::LeftParen => {
            let (expr, pos) = parse_expression(tokens, pos + 1)?;
            let (_, pos) = consume(tokens, pos, &TokenType::RightParen)?;
            Ok(((Expr::Grouping(Box::new(expr))), pos))
        }

        TokenType::Identifier => Ok((Expr::Variable(token), pos + 1)),

        invalid_token => Err(ParseError::InvalidToken { token }),
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_parse_literal() {
        let source = "123\n\n";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_primary(&tokens, 0).unwrap();
        let expected = Expr::NumericLiteral(123.);
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
}
