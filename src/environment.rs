use std::env;
use std::rc::Rc;
use std::{collections::HashMap, hash::Hash};

use crate::{interpreter::LoxType, interpreter::RunTimeError, token::Token, token_type::TokenType};

pub struct Environment {
    pub map: HashMap<String, Rc<LoxType>>,
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

    pub fn assign<'a>(
        &mut self,
        name: &'a Token<'a>,
        value: Rc<LoxType>,
    ) -> Result<Rc<LoxType>, RunTimeError<'a>> {
        if let Some(old_val) = self.map.get_mut(name.lexeme) {
            *old_val = value;
            Ok(LoxType::Nil.into())
        } else {
            Err(RunTimeError::UndefinedVariable(name))
        }
    }
}
