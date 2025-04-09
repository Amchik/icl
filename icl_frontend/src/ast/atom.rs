//! Atom, Identifier and Literals

use crate::lex::span::Pos;

/// Atom, either [`Ident`] or [`Literal`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Atom<'s> {
    Ident(Ident<'s>),
    Literal(Literal<'s>),
}

/// Identifier
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident<'s> {
    pub text: &'s str,
    pub pos: Pos,
}

/// Literal, like string, integer, float or boolean
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Literal<'s> {
    pub text: &'s str,
    pub pos: Pos,
    pub ty: LiteralType,
}

/// Type of [`Literal`]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LiteralType {
    String,
    Integer,
    Float,
    Bool,
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
