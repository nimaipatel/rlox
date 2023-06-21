use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    StringLiteral(&'a str),
    NumericLiteral(f64),
    BoolLiteral(bool),
    NilLiteral,
    Logical {
        left: Box<Expr<'a>>,
        op: &'a Token<'a>,
        right: Box<Expr<'a>>
    },
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
    Variable(&'a Token<'a>),
    Assign {
        name: &'a Token<'a>,
        value: Box<Expr<'a>>,
    },
}
