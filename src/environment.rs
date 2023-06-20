use std::collections::HashMap;
use std::rc::Rc;

use crate::{interpreter::LoxType, interpreter::RunTimeError, token::Token, token_type::TokenType};

pub struct Environment {
    map: HashMap<String, Rc<LoxType>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Rc<LoxType>) {
        self.map.insert(name, value);
    }

    pub fn get<'a>(&self, name: &'a Token) -> Result<Rc<LoxType>, RunTimeError<'a>> {
        match &name.token_type {
            TokenType::Identifier => match self.map.get(name.lexeme) {
                Some(lox_val) => Ok(Rc::clone(lox_val)),
                None => Err(RunTimeError::UndefinedVariable(name)),
            },
            _ => unreachable!(),
        }
    }
}
