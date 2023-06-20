use std::{error::Error, fmt::Display};

use crate::{expr::Expr, stmt::Stmt, token::Token, token_type::TokenType};

#[derive(Debug, PartialEq)]
pub enum LoxType {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl LoxType {
    fn stringify(&self) -> String {
        match self {
            LoxType::Nil => "Nil".into(),
            LoxType::Boolean(b) => match b {
                true => "true".into(),
                false => "false".into(),
            },
            LoxType::Number(n) => format!("{}", n),
            LoxType::String(s) => s.clone(),
        }
    }
}

#[derive(Debug)]
pub enum RunTimeError<'a> {
    OperandShouldBeNumber {
        operator: &'a Token<'a>,
        operand: LoxType,
    },
    OperandsShouldBeNumber {
        op: &'a Token<'a>,
        left: LoxType,
        right: LoxType,
    },
}

impl<'a> Error for RunTimeError<'a> {}

impl<'a> Display for RunTimeError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunTimeError::OperandShouldBeNumber { operator, operand } => write!(
                f,
                "Operand for the unary operator {:?} on line {} must be number but found {:?}",
                operator.token_type, operator.line, operand
            ),
            RunTimeError::OperandsShouldBeNumber {
                op: operator,
                left,
                right,
            } => write!(
                f,
                "Operands for the binary operator {:?} on line {} must be numbers but found {:?} and {:?}",
                operator, operator.line, left, right
            ),
        }
    }
}

pub fn interpret<'a>(stmts: &'a Vec<Stmt<'a>>) -> Result<(), RunTimeError<'a>> {
    for stmt in stmts.iter() {
        evaluate_stmt(stmt)?;
    }
    Ok(())
}

fn is_truthy<'a>(lox_type: &'a LoxType) -> bool {
    match lox_type {
        LoxType::Nil => false,
        LoxType::Boolean(b) => *b,
        _ => true,
    }
}

pub fn evaluate_stmt<'a>(stmt: &'a Stmt) -> Result<(), RunTimeError<'a>> {
    match stmt {
        Stmt::Print(expr) => {
            let value = evaluate_expr(expr)?;
            println!("{}", value.stringify());
            Ok(())
        }
        Stmt::Expression(expr) => {
            evaluate_expr(expr)?;
            Ok(())
        }
    }
}

pub fn evaluate_expr<'a>(expr: &'a Expr) -> Result<LoxType, RunTimeError<'a>> {
    match expr {
        Expr::StringLiteral(s) => Ok(LoxType::String(s.to_string())),
        Expr::NumericLiteral(n) => Ok(LoxType::Number(*n as f64)),
        Expr::BoolLiteral(b) => Ok(LoxType::Boolean(*b)),
        Expr::NilLiteral => Ok(LoxType::Nil),
        Expr::Grouping(expr) => evaluate_expr(expr),
        Expr::Unary { op, expr } => {
            let right = evaluate_expr(expr)?;
            match (&op.token_type, &right) {
                (TokenType::Bang, _) => Ok(LoxType::Boolean(!is_truthy(&right))),
                (TokenType::Minus, LoxType::Number(n)) => Ok(LoxType::Number(*n)),
                (TokenType::Minus, _) => Err(RunTimeError::OperandShouldBeNumber {
                    operator: *op,
                    operand: right,
                }),
                _ => unreachable!(),
            }
        }
        Expr::Binary { left, op, right } => {
            let left = evaluate_expr(left)?;
            let right = evaluate_expr(right)?;
            match (left, &op.token_type, right) {
                (LoxType::Number(n1), TokenType::Minus, LoxType::Number(n2)) => {
                    Ok(LoxType::Number(n1 - n2))
                }
                (left, TokenType::Minus, right) => Err(RunTimeError::OperandsShouldBeNumber {
                    op: *op,
                    left,
                    right,
                }),
                (LoxType::Number(n1), TokenType::Slash, LoxType::Number(n2)) => {
                    Ok(LoxType::Number(n1 / n2))
                }
                (left, TokenType::Slash, right) => Err(RunTimeError::OperandsShouldBeNumber {
                    op: *op,
                    left,
                    right,
                }),
                (LoxType::Number(n1), TokenType::Star, LoxType::Number(n2)) => {
                    Ok(LoxType::Number(n1 * n2))
                }
                (left, TokenType::Star, right) => Err(RunTimeError::OperandsShouldBeNumber {
                    op: *op,
                    left,
                    right,
                }),
                (LoxType::Number(n1), TokenType::Plus, LoxType::Number(n2)) => {
                    Ok(LoxType::Number(n1 + n2))
                }
                (LoxType::String(s1), TokenType::Plus, LoxType::String(s2)) => {
                    Ok(LoxType::String(format!("{}{}", s1, s2)))
                }
                (left, TokenType::Plus, right) => Err(RunTimeError::OperandsShouldBeNumber {
                    op: *op,
                    left,
                    right,
                }),
                (LoxType::Number(n1), TokenType::Greater, LoxType::Number(n2)) => {
                    Ok(LoxType::Boolean(n1 > n2))
                }
                (left, TokenType::Greater, right) => Err(RunTimeError::OperandsShouldBeNumber {
                    op: *op,
                    left,
                    right,
                }),
                (LoxType::Number(n1), TokenType::GreaterEqual, LoxType::Number(n2)) => {
                    Ok(LoxType::Boolean(n1 >= n2))
                }
                (left, TokenType::GreaterEqual, right) => {
                    Err(RunTimeError::OperandsShouldBeNumber {
                        op: *op,
                        left,
                        right,
                    })
                }
                (LoxType::Number(n1), TokenType::Less, LoxType::Number(n2)) => {
                    Ok(LoxType::Boolean(n1 > n2))
                }
                (left, TokenType::Less, right) => Err(RunTimeError::OperandsShouldBeNumber {
                    op: *op,
                    left,
                    right,
                }),
                (LoxType::Number(n1), TokenType::LessEqual, LoxType::Number(n2)) => {
                    Ok(LoxType::Boolean(n1 >= n2))
                }
                (left, TokenType::LessEqual, right) => Err(RunTimeError::OperandsShouldBeNumber {
                    op: *op,
                    left,
                    right,
                }),
                (LoxType::Number(n1), TokenType::BangEqual, LoxType::Number(n2)) => {
                    Ok(LoxType::Boolean(n1 != n2))
                }
                (left, TokenType::BangEqual, right) => Err(RunTimeError::OperandsShouldBeNumber {
                    op: *op,
                    left,
                    right,
                }),
                (LoxType::Number(n1), TokenType::EqualEqual, LoxType::Number(n2)) => {
                    Ok(LoxType::Boolean(n1 == n2))
                }
                (left, TokenType::EqualEqual, right) => Err(RunTimeError::OperandsShouldBeNumber {
                    op: *op,
                    left,
                    right,
                }),
                _ => unreachable!(),
            }
        }
    }
}
