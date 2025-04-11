//! Expressions

use crate::lex::span::{Pos, Span, Spanned};

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

impl Spanned for Expression<'_> {
    fn span(&self) -> Span {
        match self {
            Self::Atom(s) => s.span(),
            Self::Call(s) => s.span(),
            Self::Infix(s) => s.span(),
            Self::Prefix(s) => s.span(),
            Self::Member(s) => s.span(),
        }
    }
}

impl Spanned for CallExpr<'_> {
    fn span(&self) -> Span {
        Span::new(
            self.callee.span().start,
            self.par_close.pos.clone().skip(")"),
        )
    }
}

impl Spanned for PrefixExpr<'_> {
    fn span(&self) -> Span {
        Span::new(self.pos.clone(), self.rhs.span().end)
    }
}

impl Spanned for InfixExpr<'_> {
    fn span(&self) -> Span {
        Span::new(self.lhs.span().start, self.lhs.span().end)
    }
}

impl Spanned for MemberExpr<'_> {
    fn span(&self) -> Span {
        Span::new(self.lhs.span().start, self.lhs.span().end)
    }
}

impl<S: Spanned> Spanned for MaybeWrapped<S> {
    fn span(&self) -> Span {
        match self {
            Self::Normal(s) => s.span(),
            Self::Wrapped {
                par_open,
                par_close,
                ..
            } => Span::new(par_open.pos.clone(), par_close.pos.clone().skip(")")),
        }
    }
}
