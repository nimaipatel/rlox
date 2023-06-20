use std::fmt::format;
use std::rc::Rc;
use std::{error::Error, fmt::Display};

use crate::{
    environment::Environment, expr::Expr, stmt::Stmt, token::Token, token_type::TokenType,
};

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
        operand: Rc<LoxType>,
    },
    OperandsShouldBeNumber {
        op: &'a Token<'a>,
        left: Rc<LoxType>,
        right: Rc<LoxType>,
    },
    UndefinedVariable(&'a Token<'a>),
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
            RunTimeError::UndefinedVariable(name) => write!(f, "Found undefined variable {} on line {}", name.lexeme, name.line)
        }
    }
}

pub fn interpret<'a>(
    env: &mut Environment,
    stmts: &'a Vec<Stmt<'a>>,
) -> Result<(), RunTimeError<'a>> {
    for stmt in stmts.iter() {
        evaluate_stmt(env, stmt)?;
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

pub fn evaluate_stmt<'a>(env: &mut Environment, stmt: &'a Stmt) -> Result<(), RunTimeError<'a>> {
    match stmt {
        Stmt::Print(expr) => {
            let value = evaluate_expr(env, expr)?;
            println!("{}", value.stringify());
            Ok(())
        }
        Stmt::Expression(expr) => {
            evaluate_expr(env, expr)?;
            Ok(())
        }
        Stmt::Var(name, Some(initializer)) => {
            let value = evaluate_expr(env, initializer)?;
            env.define(name.lexeme.into(), value);
            Ok(())
        }
        Stmt::Var(name, None) => {
            env.define(name.lexeme.into(), Rc::new(LoxType::Nil));
            Ok(())
        }
        Stmt::Block(stmts) => {
            todo!();
        }
    }
}

pub fn evaluate_expr<'a>(
    env: &mut Environment,
    expr: &'a Expr,
) -> Result<Rc<LoxType>, RunTimeError<'a>> {
    match expr {
        Expr::StringLiteral(s) => Ok(Rc::new(LoxType::String(s.to_string()))),
        Expr::NumericLiteral(n) => Ok(Rc::new(LoxType::Number(*n as f64))),
        Expr::BoolLiteral(b) => Ok(Rc::new(LoxType::Boolean(*b))),
        Expr::NilLiteral => Ok(Rc::new(LoxType::Nil)),
        Expr::Grouping(expr) => evaluate_expr(env, expr),
        Expr::Unary { op, expr } => {
            let right = evaluate_expr(env, expr)?;
            match &op.token_type {
                TokenType::Bang => Ok(Rc::new(LoxType::Boolean(!is_truthy(&right)))),
                TokenType::Minus => match right.as_ref() {
                    LoxType::Number(n) => Ok(Rc::new(LoxType::Number(n * -1.))),
                    _ => Err(RunTimeError::OperandShouldBeNumber {
                        operator: *op,
                        operand: Rc::clone(&right),
                    }),
                },
                _ => unreachable!(),
            }
        }

        Expr::Binary { left, op, right } => {
            let left = evaluate_expr(env, left)?;
            let right = evaluate_expr(env, right)?;
            match &op.token_type {
                TokenType::Minus => match (left.as_ref(), right.as_ref()) {
                    (LoxType::Number(n1), LoxType::Number(n2)) => {
                        Ok(Rc::new(LoxType::Number(n1 - n2)))
                    }
                    _ => Err(RunTimeError::OperandsShouldBeNumber {
                        op: *op,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    }),
                },
                TokenType::Slash => match (left.as_ref(), right.as_ref()) {
                    (LoxType::Number(n1), LoxType::Number(n2)) => {
                        Ok(Rc::new(LoxType::Number(n1 / n2)))
                    }
                    _ => Err(RunTimeError::OperandsShouldBeNumber {
                        op: *op,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    }),
                },
                TokenType::Star => match (left.as_ref(), right.as_ref()) {
                    (LoxType::Number(n1), LoxType::Number(n2)) => {
                        Ok(Rc::new(LoxType::Number(n1 * n2)))
                    }
                    _ => Err(RunTimeError::OperandsShouldBeNumber {
                        op: *op,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    }),
                },
                TokenType::Plus => match (left.as_ref(), right.as_ref()) {
                    (LoxType::Number(n1), LoxType::Number(n2)) => {
                        Ok(Rc::new(LoxType::Number(n1 + n2)))
                    }
                    (LoxType::String(s1), LoxType::String(s2)) => {
                        Ok(Rc::new(LoxType::String(format!("{}{}", s1, s2))))
                    }
                    _ => Err(RunTimeError::OperandsShouldBeNumber {
                        op: *op,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    }),
                },
                TokenType::Greater => match (left.as_ref(), right.as_ref()) {
                    (LoxType::Number(n1), LoxType::Number(n2)) => {
                        Ok(Rc::new(LoxType::Boolean(n1 > n2)))
                    }
                    _ => Err(RunTimeError::OperandsShouldBeNumber {
                        op: *op,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    }),
                },
                TokenType::GreaterEqual => match (left.as_ref(), right.as_ref()) {
                    (LoxType::Number(n1), LoxType::Number(n2)) => {
                        Ok(Rc::new(LoxType::Boolean(n1 >= n2)))
                    }
                    _ => Err(RunTimeError::OperandsShouldBeNumber {
                        op: *op,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    }),
                },
                TokenType::Less => match (left.as_ref(), right.as_ref()) {
                    (LoxType::Number(n1), LoxType::Number(n2)) => {
                        Ok(Rc::new(LoxType::Boolean(n1 < n2)))
                    }
                    _ => Err(RunTimeError::OperandsShouldBeNumber {
                        op: *op,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    }),
                },
                TokenType::LessEqual => match (left.as_ref(), right.as_ref()) {
                    (LoxType::Number(n1), LoxType::Number(n2)) => {
                        Ok(Rc::new(LoxType::Boolean(n1 <= n2)))
                    }
                    _ => Err(RunTimeError::OperandsShouldBeNumber {
                        op: *op,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    }),
                },
                TokenType::BangEqual => {
                    Ok(Rc::new(LoxType::Boolean(left.as_ref() != right.as_ref())))
                }
                TokenType::EqualEqual => {
                    Ok(Rc::new(LoxType::Boolean(left.as_ref() == right.as_ref())))
                }
                _ => unreachable!(),
            }
        }
        Expr::Variable(name) => {
            let value = env.get(name)?;
            Ok(value)
        }
        Expr::Assign { name, value } => {
            let value = evaluate_expr(env, value)?;
            env.assign(name, value)
        }
    }
}
