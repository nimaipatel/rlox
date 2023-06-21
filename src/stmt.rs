use crate::{expr::Expr, token::Token};

#[derive(Debug)]
pub enum Stmt<'a> {
    Print(Expr<'a>),
    Expression(Expr<'a>),
    Var(&'a Token<'a>, Option<Expr<'a>>),
    Block(Vec<Stmt<'a>>),
    If {
        condition: Expr<'a>,
        then_branch: Box<Stmt<'a>>,
        else_branch: Option<Box<Stmt<'a>>>,
    },
    While {
        condition: Expr<'a>,
        body: Box<Stmt<'a>>,
    },
}
