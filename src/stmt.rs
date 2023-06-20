use crate::expr::Expr;

pub enum Stmt<'a> {
    Print(Expr<'a>),
    Expression(Expr<'a>),
}
