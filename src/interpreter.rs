use std::borrow::Borrow;
use std::cell::RefCell;

use std::fmt::Debug;

use std::rc::Rc;
use std::{error::Error, fmt::Display};

use crate::{
    environment::Environment, expr::Expr, stmt::Stmt, token::Token, token_type::TokenType,
};

struct Callable<'a> {
    call: fn(env: Rc<RefCell<Environment>>, arguments: Vec<Rc<LoxType>>) -> Rc<LoxType>,
    arity: fn() -> usize,
    name: String,
    paren: Token<'a>,
}

impl<'a> Display for Callable<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq)]
pub enum FunctionType {
    UserDefined(Option<String>),
    NativeFunction(String),
}

pub enum LoxType {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Function {
        function_type: FunctionType,
        // call: fn(env: Rc<RefCell<Environment>>, arguments: Vec<Rc<LoxType>>) -> Rc<LoxType>,
        call: Box<dyn Fn(Rc<RefCell<Environment>>, Vec<Rc<LoxType>>) -> Rc<LoxType>>,
        arity: usize,
    },
}

impl Debug for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Boolean(arg0) => f.debug_tuple("Boolean").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Function {
                function_type,
                call,
                arity,
            } => f
                .debug_struct("Function")
                .field("function_type", function_type)
                // .field("call", call)
                .field("arity", arity)
                .finish(),
        }
    }
}

impl PartialEq for LoxType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            // (
            //     Self::Function {
            //         function_type: l_function_type,
            //         call: l_call,
            //         arity: l_arity,
            //     },
            //     Self::Function {
            //         function_type: r_function_type,
            //         call: r_call,
            //         arity: r_arity,
            //     },
            // ) => l_function_type == r_function_type && l_call == r_call && l_arity == r_arity,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

// impl<'a> TryInto<Callable<'a>> for LoxType {
//     type Error = RunTimeError<'a>;

//     fn try_into(self) -> Result<Callable<'a>, Self::Error> {
//         match self {
//             LoxType::Function { function_type, call, arity } => todo!(),
//             _ => Err(RunTimeError::NotCallable { paren: self. })
//         }
//     }
// }

impl Display for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxType::Nil => write!(f, "nil"),
            LoxType::Boolean(b) => match b {
                true => write!(f, "true"),
                false => write!(f, "false"),
            },
            LoxType::Number(n) => write!(f, "{}", n),
            LoxType::String(s) => write!(f, "{}", s),
            LoxType::Function {
                function_type,
                call,
                arity,
            } => match function_type {
                FunctionType::UserDefined(Some(name)) => {
                    write!(f, "<User-defined Function `{}`>", name)
                }
                FunctionType::UserDefined(None) => write!(f, "<User-defined Function>"),
                FunctionType::NativeFunction(name) => write!(f, "<Native function `{}`>", name),
            },
        }
    }
}

#[derive(Debug)]
pub enum RunTimeError<'a> {
    WrongNumArgs {
        paren: &'a Token<'a>,
        expected: usize,
        actual: usize,
    },
    NotCallable {
        paren: &'a Token<'a>,
    },
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
            RunTimeError::UndefinedVariable(name) =>
                write!(
                    f,
                    "Found undefined variable {} on line {}",
                    name.lexeme, name.line
            ),
            RunTimeError::NotCallable { paren } =>
                write!(
                    f,
                    "Can't call {} on line {} as it is not callable",
                    paren.lexeme, paren.line
            ),
            RunTimeError::WrongNumArgs { paren, expected, actual } =>
                write!(
                    f,
                    "Expected {} arguments but got {} for {} on line {}", 
                    expected, actual, paren.lexeme, paren.line
            )

        }
    }
}

pub fn interpret<'a>(
    env: Rc<RefCell<Environment>>,
    stmts: &'a Vec<Stmt<'a>>,
) -> Result<(), RunTimeError<'a>> {
    for stmt in stmts.iter() {
        evaluate_stmt(Rc::clone(&env), stmt)?;
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

pub fn evaluate_stmt<'a>(
    env: Rc<RefCell<Environment>>,
    stmt: &'a Stmt,
) -> Result<(), RunTimeError<'a>> {
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
                function_type: FunctionType::UserDefined(name.lexeme.to_string().into()),
                call: Box::new(move |env, args| {
                    let env = Rc::new(RefCell::new(Environment::new(Some(env))));
                    for (param, arg) in params.iter().zip(args.iter()) {
                        env.borrow_mut().define(param.into(), Rc::clone(arg));
                    }
                    evaluate_stmt(Rc::clone(&env), todo!());
                    Rc::new(LoxType::Nil)
                }),
                arity,
            };
            env.borrow_mut()
                .define(name.lexeme.to_string(), Rc::new(lox_function));
            Ok(())
        }
    }
}

fn execute_block<'a>(
    env: Rc<RefCell<Environment>>,
    stmts: &'a [Stmt<'a>],
) -> Result<(), RunTimeError<'a>> {
    for stmt in stmts.iter() {
        evaluate_stmt(Rc::clone(&env), stmt)?;
    }
    Ok(())
}

pub fn evaluate_expr<'a>(
    env: Rc<RefCell<Environment>>,
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
                } => {
                    let actual = arguments.len();
                    if *arity == actual {
                        Ok((call)(env, arguments))
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
