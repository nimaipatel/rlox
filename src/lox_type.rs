use std::fmt::{Debug, Display};
use std::{rc::Rc, cell::RefCell};

use crate::environment::Environment;

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
                call: _,
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
                call: _,
                arity: _,
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
