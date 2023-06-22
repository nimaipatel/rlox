use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    interpreter::LoxType,
    interpreter::{Callable, RunTimeError},
    token::Token,
    token_type::TokenType,
};

pub struct Environment {
    pub map: HashMap<String, Rc<LoxType>>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        if let Some(enclosing) = enclosing {
            Self {
                map: HashMap::new(),
                enclosing: Some(enclosing),
            }
        } else {
            let mut globals = Self {
                map: HashMap::new(),
                enclosing: None,
            };
            let clock = LoxType::Function(Callable {
                call: |env, _| {
                    let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
                    LoxType::Number(time as f64).into()
                },
                arity: || 0,
            });
            globals.define("clock".into(), clock.into());
            globals
        }
    }

    pub fn define(&mut self, name: String, value: Rc<LoxType>) {
        self.map.insert(name, value);
    }

    pub fn get<'a>(&self, name: &'a Token) -> Result<Rc<LoxType>, RunTimeError<'a>> {
        match &name.token_type {
            TokenType::Identifier => match self.map.get(name.lexeme) {
                Some(lox_val) => Ok(Rc::clone(lox_val)),
                None => match &self.enclosing {
                    Some(enclosing) => enclosing.borrow_mut().get(name),
                    None => Err(RunTimeError::UndefinedVariable(name)),
                },
            },
            _ => unreachable!(),
        }
    }

    pub fn assign<'a>(
        &mut self,
        name: &'a Token<'a>,
        value: Rc<LoxType>,
    ) -> Result<Rc<LoxType>, RunTimeError<'a>> {
        if let Some(old_val) = self.map.get_mut(name.lexeme) {
            *old_val = value;
            Ok(LoxType::Nil.into())
        } else {
            match &mut self.enclosing {
                Some(enclosing) => enclosing.borrow_mut().assign(name, value),
                None => Err(RunTimeError::UndefinedVariable(name)),
            }
        }
    }
}
