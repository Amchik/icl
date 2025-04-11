//! Atom, Identifier and Literals

use std::borrow::Cow;

use crate::lex::span::{Pos, Span, Spanned};

/// Atom, either [`Ident`] or [`Literal`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Atom<'s> {
    Ident(Ident<'s>),
    Literal(LiteralData<'s>),
}

/// Identifier
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident<'s> {
    pub text: &'s str,
    pub pos: Pos,
}

/// Literal, like string, integer, float or boolean
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LiteralData<'s> {
    pub text: &'s str,
    pub pos: Pos,
    pub ty: Literal<'s>,
}

/// Parsed literal
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal<'s> {
    String(Cow<'s, str>),
    Integer(usize),
    Float((usize, usize)),
    Bool(bool),
}

impl Spanned for Atom<'_> {
    fn span(&self) -> Span {
        match self {
            Self::Ident(s) => s.span(),
            Self::Literal(s) => s.span(),
        }
    }
}
impl Spanned for Ident<'_> {
    fn span(&self) -> Span {
        Span::new(self.pos.clone(), self.pos.clone().skip(self.text))
    }
}
impl Spanned for LiteralData<'_> {
    fn span(&self) -> Span {
        Span::new(self.pos.clone(), self.pos.clone().skip(self.text))
    }
}

impl Ident<'_> {
    /// Returns `true` if ident is known "intentional" ident.
    ///
    /// # Example
    /// ```
    /// # use icl_frontend::{ast::atom::Ident, lex::span::Pos};
    /// # let pos = Pos { line: 1, column: 1, index: 0, };
    ///
    /// let a = Ident { text: "foobar", pos: pos.clone() };
    /// let b = Ident { text: "_foobar", pos: pos.clone() };
    /// let nothing = Ident { text: "_", pos: pos.clone() };
    ///
    /// assert!(!a.is_intentional());
    /// assert!(b.is_intentional());
    /// assert!(nothing.is_intentional());
    /// ```
    pub const fn is_intentional(&self) -> bool {
        matches!(self.text.as_bytes(), &[b'_', ..])
    }

    /// Returns `true` if ident is known "ugly" ident.
    ///
    /// # Example
    /// ```
    /// # use icl_frontend::{ast::atom::Ident, lex::span::Pos};
    /// # let pos = Pos { line: 1, column: 1, index: 0, };
    ///
    /// let ugly = Ident {
    ///     text: "__internal_do_not_use_please_private__only_in_gnu99__U32_MAX",
    ///     pos,
    /// };
    ///
    /// assert!(ugly.is_ugly());
    /// ```
    pub const fn is_ugly(&self) -> bool {
        matches!(self.text.as_bytes(), &[b'_', b'_', ..])
    }
}
