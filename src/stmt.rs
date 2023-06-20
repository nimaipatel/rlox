use crate::{expr::Expr, token::Token};

#[derive(Debug)]
pub enum Stmt<'a> {
    Print(Expr<'a>),
    Expression(Expr<'a>),
    Var(&'a Token<'a>, Option<Expr<'a>>),
}
