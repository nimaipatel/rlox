use crate::token::Token;

#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'a> {
    StringLiteral(&'a str),
    NumericLiteral(u32),
    TrueLiteral,
    FalseLiteral,
    NilLiteral,
    Unary {
        op: &'a Token<'a>,
        expr: Box<Expr<'a>>,
    },
    Binary {
        left: Box<Expr<'a>>,
        op: &'a Token<'a>,
        right: Box<Expr<'a>>,
    },
    Grouping(Box<Expr<'a>>),
}
