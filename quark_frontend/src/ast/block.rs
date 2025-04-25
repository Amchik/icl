//! Code and structure blocks

use crate::lex::span::{Span, Spanned};

use super::{
    atom::Ident,
    single::{BraceClose, BraceOpen, Colon, Ellipsis},
    stmt::Statement,
    ty::Type,
};

/// Either normal block `B` or [`BuiltinBlock`]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Block<B> {
    Normal(B),
    Builtin(BuiltinBlock),
}

/// Builtin block, contains only three dots inside.
/// ```text
/// { ... }
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltinBlock {
    pub start: BraceOpen,
    pub dots: Ellipsis,
    pub end: BraceClose,
}

/// Block for structure or class definition.
/// ```text
/// { foo: Type, bar: Type }
/// { foo: Type, bar: Type, } // <-- optional comma
/// {  } // empty
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructBlock<'s> {
    pub start: BraceOpen,
    pub props: Vec<StructProperty<'s>>,
    pub end: BraceClose,
}

/// Structure (or class) property.
/// ```text
/// foo: Type
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructProperty<'s> {
    pub ident: Ident<'s>,
    pub colon: Colon,
    pub ty: Type<'s>,
}

/// Code block, list of statements. Note that each statement can have or haven't [`super::single::Semicolon`] at
/// end.
/// ```text
/// { <stmt1> <stmt2> <...> }
/// {  } // empty
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodeBlock<'s> {
    pub start: BraceOpen,
    pub stmts: Vec<Statement<'s>>,
    pub end: BraceClose,
}

impl<B: Spanned> Spanned for Block<B> {
    #[inline]
    fn span(&self) -> Span {
        match self {
            Self::Normal(b) => b.span(),
            Self::Builtin(b) => b.span(),
        }
    }
}

impl Spanned for BuiltinBlock {
    fn span(&self) -> Span {
        Span::new(self.start.pos.clone(), self.end.pos.clone().skip(";"))
    }
}
impl Spanned for StructBlock<'_> {
    fn span(&self) -> Span {
        Span::new(self.start.pos.clone(), self.end.pos.clone().skip(";"))
    }
}
impl Spanned for CodeBlock<'_> {
    fn span(&self) -> Span {
        Span::new(self.start.pos.clone(), self.end.pos.clone().skip(";"))
    }
}
