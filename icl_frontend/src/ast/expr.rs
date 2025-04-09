//! Expressions

use crate::lex::span::Pos;

use super::{
    atom::Atom,
    ops::{BinOp, UnOp},
    single::{Dot, ParenClose, ParenOpen},
};

/// Maybe wrapped in parenthesis.
/// ```text
/// ( T ) // Wrapped
/// T // Normal
/// ```
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum MaybeWrapped<T> {
    Normal(T),
    Wrapped {
        par_open: ParenOpen,
        par_close: ParenClose,
        value: T,
    },
}

/// Expression.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expression<'s> {
    Atom(Atom<'s>),
    Call(CallExpr<'s>),
    Prefix(PrefixExpr<'s>),
    Infix(InfixExpr<'s>),
    Member(MemberExpr<'s>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CallExpr<'s> {
    pub callee: Box<Expression<'s>>,
    pub par_open: ParenOpen,
    pub args: Vec<Expression<'s>>,
    pub par_close: ParenClose,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PrefixExpr<'s> {
    pub pos: Pos,
    pub op: UnOp,
    pub rhs: Box<Expression<'s>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct InfixExpr<'s> {
    pub op_pos: Pos,
    pub lhs: Box<Expression<'s>>,
    pub op: BinOp,
    pub rhs: Box<Expression<'s>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MemberExpr<'s> {
    pub lhs: Box<Expression<'s>>,
    pub dot: Dot,
    pub rhs: Atom<'s>,
}
