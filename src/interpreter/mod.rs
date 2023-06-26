use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::Environment;
use crate::lox_type::{FunctionType, LoxType};
use crate::runtime_error::RunTimeError;
use crate::{expr::Expr, stmt::Stmt, token_type::TokenType};

pub fn interpret<'a>(
    // env: Rc<RefCell<Environment<'a>>>,
    stmts: &'a Vec<Stmt<'a>>,
) -> Result<(), RunTimeError<'a>> {
    let env = Environment::fresh();
    for stmt in stmts.iter() {
        match evaluate_stmt(Rc::clone(&env), stmt) {
            Ok(_) => (),
            Err(e) => match e {
                RTErrorOrReturn::RunTimeError(e) => return Err(e),
                RTErrorOrReturn::Return(_) => return Err(RunTimeError::ReturnNotInAFunc),
            },
        }
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

enum RTErrorOrReturn<'a> {
    RunTimeError(RunTimeError<'a>),
    Return(Rc<LoxType<'a>>),
}

impl<'a> From<RunTimeError<'a>> for RTErrorOrReturn<'a> {
    fn from(value: RunTimeError<'a>) -> Self {
        RTErrorOrReturn::RunTimeError(value)
    }
}

fn evaluate_stmt<'a>(
    env: Rc<RefCell<Environment<'a>>>,
    stmt: &'a Stmt,
) -> Result<(), RTErrorOrReturn<'a>> {
    match stmt {
        Stmt::Print(expr) => {
            let value = evaluate_expr(Rc::clone(&env), expr)?;
            println!("{}", value);
            Ok(())
        }
        Stmt::Expression(expr) => {
            evaluate_expr(Rc::clone(&env), expr)?;
            Ok(())
        }
        Stmt::Var(name, Some(initializer)) => {
            let value = evaluate_expr(Rc::clone(&env), initializer)?;
            env.borrow_mut().define(name.lexeme.into(), value);
            Ok(())
        }
        Stmt::Var(name, None) => {
            env.borrow_mut()
                .define(name.lexeme.into(), Rc::new(LoxType::Nil));
            Ok(())
        }
        Stmt::Block(stmts) => {
            let new_env = Environment::new(Some(Rc::clone(&env)));
            let new_env = Rc::new(RefCell::new(new_env));
            execute_block(Rc::clone(&new_env), stmts)?;
            Ok(())
        }
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition = evaluate_expr(Rc::clone(&env), condition)?;
            match is_truthy(&condition) {
                true => evaluate_stmt(env, &then_branch)?,
                false => match else_branch {
                    Some(else_branch) => evaluate_stmt(env, &else_branch)?,
                    None => (),
                },
            }
            Ok(())
        }
        Stmt::While { condition, body } => {
            while is_truthy(evaluate_expr(Rc::clone(&env), condition)?.borrow()) {
                evaluate_stmt(Rc::clone(&env), &body)?;
            }
            Ok(())
        }
        Stmt::Function { name, params, body } => {
            let arity = params.len();
            let params: Vec<_> = params.iter().map(|e| e.lexeme.to_string()).collect();
            let lox_function = LoxType::Function {
                closure: Some(Rc::clone(&env)),
                function_type: FunctionType::UserDefined(name.lexeme.to_string().into()),
                call: Box::new(move |env, args| {
                    let env = Rc::new(RefCell::new(Environment::new(Some(env))));
                    for (param, arg) in params.iter().zip(args.iter()) {
                        env.borrow_mut().define(param.into(), Rc::clone(arg));
                    }
                    match evaluate_stmt(Rc::clone(&env), &body) {
                        Ok(()) => Ok(Rc::new(LoxType::Nil)),
                        Err(e) => match e {
                            RTErrorOrReturn::RunTimeError(e) => Err(e),
                            RTErrorOrReturn::Return(val) => Ok(val),
                        },
                    }
                }),
                arity,
            };
            env.borrow_mut()
                .define(name.lexeme.to_string(), Rc::new(lox_function));
            Ok(())
        }
        Stmt::Return { keyword: _, value } => {
            let value = {
                match value {
                    Some(value) => evaluate_expr(env, value)?,
                    None => Rc::new(LoxType::Nil),
                }
            };
            Err(RTErrorOrReturn::Return(value))
        }
    }
}

fn execute_block<'a>(
    env: Rc<RefCell<Environment<'a>>>,
    stmts: &'a [Stmt<'a>],
) -> Result<(), RTErrorOrReturn<'a>> {
    for stmt in stmts.iter() {
        evaluate_stmt(Rc::clone(&env), stmt)?;
    }
    Ok(())
}

fn evaluate_expr<'a>(
    env: Rc<RefCell<Environment<'a>>>,
    expr: &'a Expr,
) -> Result<Rc<LoxType<'a>>, RunTimeError<'a>> {
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
            let left = evaluate_expr(Rc::clone(&env), left)?;
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
            let value = env.borrow_mut().get(name)?;
            Ok(value)
        }
        Expr::Assign { name, value } => {
            let value = evaluate_expr(Rc::clone(&env), value)?;
            env.borrow_mut().assign(name, value)
        }
        Expr::Logical { left, op, right } => {
            let left = evaluate_expr(Rc::clone(&env), left)?;
            match (is_truthy(&left), &op.token_type) {
                (true, TokenType::Or) | (false, TokenType::And) => Ok(left),
                (false, TokenType::Or) | (true, TokenType::And) => evaluate_expr(env, &right),
                _ => unreachable!(),
            }
        }
        Expr::Call {
            callee,
            paren,
            arguments,
        } => {
            let callee = evaluate_expr(Rc::clone(&env), callee)?;
            let arguments: Result<Vec<_>, RunTimeError> = arguments
                .into_iter()
                .map(|a| evaluate_expr(Rc::clone(&env), a))
                .collect();
            let arguments = arguments?;
            match callee.as_ref() {
                LoxType::Function {
                    function_type: _,
                    call,
                    arity,
                    closure,
                } => {
                    let actual = arguments.len();
                    if *arity == actual {
                        match closure {
                            Some(closure) => (call)(Rc::clone(closure), arguments),
                            None => (call)(Rc::clone(&env), arguments),
                        }
                    } else {
                        Err(RunTimeError::WrongNumArgs {
                            paren,
                            expected: *arity,
                            actual,
                        })
                    }
                }
                _ => Err(RunTimeError::NotCallable { paren }),
            }
        }
    }
}
