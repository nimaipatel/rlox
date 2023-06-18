use crate::{expr::Expr, token_type::TokenType};

#[derive(Debug, PartialEq)]
pub enum LoxType {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

fn is_truthy(lox_type: LoxType) -> bool {
    match lox_type {
        LoxType::Nil => false,
        LoxType::Boolean(b) => b,
        _ => true,
    }
}

impl TryInto<f64> for LoxType {
    type Error = String;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            LoxType::Number(n) => Ok(n),
            _ => Err("failed cast".to_string()),
        }
    }
}

pub fn evaluate(expr: &Expr) -> LoxType {
    match expr {
        Expr::StringLiteral(s) => LoxType::String(s.to_string()),
        Expr::NumericLiteral(n) => LoxType::Number(*n as f64),
        Expr::BoolLiteral(b) => LoxType::Boolean(*b),
        Expr::NilLiteral => LoxType::Nil,
        Expr::Grouping(expr) => evaluate(expr),
        Expr::Unary { op, expr } => {
            let right = evaluate(expr);
            match &op.token_type {
                TokenType::Minus => LoxType::Number(right.try_into().unwrap()),
                TokenType::Bang => LoxType::Boolean(!is_truthy(right)),
                _ => unreachable!(),
            }
        }
        Expr::Binary { left, op, right } => {
            let left = evaluate(left);
            let right = evaluate(right);
            match &op.token_type {
                TokenType::Minus => {
                    let op1: f64 = left.try_into().unwrap();
                    let op2: f64 = right.try_into().unwrap();
                    LoxType::Number(op1 - op2)
                }
                TokenType::Slash => {
                    let op1: f64 = left.try_into().unwrap();
                    let op2: f64 = right.try_into().unwrap();
                    LoxType::Number(op1 / op2)
                }
                TokenType::Star => {
                    let op1: f64 = left.try_into().unwrap();
                    let op2: f64 = right.try_into().unwrap();
                    LoxType::Number(op1 * op2)
                }
                TokenType::Plus => match (left, right) {
                    (LoxType::Number(n1), LoxType::Number(n2)) => LoxType::Number(n1 + n2),
                    (LoxType::String(s1), LoxType::String(s2)) => {
                        LoxType::String(format!("{}{}", s1, s2))
                    }
                    _ => todo!("book says this will be done later"),
                },
                TokenType::Greater => {
                    let op1: f64 = left.try_into().unwrap();
                    let op2: f64 = right.try_into().unwrap();
                    LoxType::Boolean(op1 > op2)
                }
                TokenType::GreaterEqual => {
                    let op1: f64 = left.try_into().unwrap();
                    let op2: f64 = right.try_into().unwrap();
                    LoxType::Boolean(op1 >= op2)
                }
                TokenType::Less => {
                    let op1: f64 = left.try_into().unwrap();
                    let op2: f64 = right.try_into().unwrap();
                    LoxType::Boolean(op1 < op2)
                }
                TokenType::LessEqual => {
                    let op1: f64 = left.try_into().unwrap();
                    let op2: f64 = right.try_into().unwrap();
                    LoxType::Boolean(op1 <= op2)
                }
                TokenType::BangEqual => LoxType::Boolean(left != right),
                TokenType::EqualEqual => LoxType::Boolean(left == right),
                _ => unreachable!(),
            }
        }
    }
}
