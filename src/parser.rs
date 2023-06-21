use core::fmt;
use std::error::Error;

use crate::expr::Expr;
use crate::stmt::Stmt;
use crate::token::Token;
use crate::token_type::TokenType;

// program        → declaration* EOF ;

// statement      → exprStmt
//                | ifStmt
//                | printStmt
//                | block ;
//

// ifStmt         → "if" "(" expression ")" statement
//                ( "else" statement )? ;

// block          → "{" declaration* "}" ;

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

pub fn parse<'a>(tokens: &'a Vec<Token<'a>>) -> (Vec<Stmt<'a>>, Vec<ParseError>) {
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    let mut pos: usize = 0;
    loop {
        if tokens[pos].token_type == TokenType::Eof {
            break;
        } else {
            match parse_declaration(tokens, pos) {
                Ok((statement, new_pos)) => {
                    pos = new_pos;
                    statements.push(statement);
                }
                Err(e) => {
                    errors.push(e);
                    pos = synchronize(tokens, pos);
                }
            }
        }
    }
    (statements, errors)
}

fn synchronize(tokens: &[Token<'_>], pos: usize) -> usize {
    let mut pos = pos + 1;
    loop {
        match tokens[pos].token_type {
            TokenType::Eof => break,
            TokenType::Class => break,
            TokenType::Fun => break,
            TokenType::Var => break,
            TokenType::For => break,
            TokenType::While => break,
            TokenType::Print => break,
            TokenType::Return => break,
            TokenType::Semicolon => {
                pos += 1;
                break;
            }
            _ => pos += 1,
        }
    }
    pos
}

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

    match matchh(tokens, pos, vec![TokenType::Equal]) {
        Some((_, pos)) => {
            let (initializer, pos) = parse_expression(tokens, pos)?;
            let (_, pos) = consume(tokens, pos, &TokenType::Semicolon)?;
            Ok((Stmt::Var(name, Some(initializer)), pos))
        }
        None => {
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
        TokenType::If => parse_if_statment(tokens, pos + 1),
        TokenType::Print => parse_print_statement(tokens, pos + 1),
        TokenType::LeftBrace => parse_block(tokens, pos + 1),
        _ => parse_expression_statement(tokens, pos),
    }
}

fn parse_if_statment<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Stmt<'a>, usize), ParseError<'a>> {
    let (_, pos) = consume(tokens, pos, &TokenType::LeftParen)?;
    let (condition, pos) = parse_expression(tokens, pos)?;
    let (_, pos) = consume(tokens, pos, &TokenType::RightParen)?;
    let (then_branch, pos) = parse_statement(tokens, pos)?;
    match matchh(tokens, pos, vec![TokenType::Else]) {
        Some((_, pos)) => {
            let (else_branch, pos) = parse_statement(tokens, pos)?;
            Ok((
                Stmt::If {
                    condition,
                    then_branch: Box::new(then_branch),
                    else_branch: Some(Box::new(else_branch)),
                },
                pos,
            ))
        }
        None => Ok((
            Stmt::If {
                condition,
                then_branch: Box::new(then_branch),
                else_branch: None,
            },
            pos,
        )),
    }
}

fn parse_block<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
) -> Result<(Stmt<'a>, usize), ParseError> {
    let mut pos = pos;
    let mut statements = Vec::new();

    loop {
        match tokens[pos].token_type {
            TokenType::Eof | TokenType::RightBrace => {
                let (_, pos) = consume(tokens, pos, &TokenType::RightBrace)?;
                return Ok((Stmt::Block(statements), pos));
            }
            _ => {
                let (statement, new_pos) = parse_declaration(tokens, pos)?;
                pos = new_pos;
                statements.push(statement);
            }
        }
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

fn matchh<'a>(
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
    expected: Vec<TokenType<'a>>,
) -> Option<(&'a Token<'a>, usize)> {
    for expected in expected.iter() {
        if tokens[pos].token_type == *expected {
            return Some((&tokens[pos], pos + 1));
        }
    }
    None
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
    match matchh(tokens, pos, vec![TokenType::Equal]) {
        Some((equals, pos)) => {
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
        None => Ok((expr, pos)),
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

        _ => Err(ParseError::InvalidToken { token }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner;

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
