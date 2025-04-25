//! Type

use crate::lex::span::Spanned;

use super::atom::Ident;

/// Type. Syntax for types aren't implemented yet, so temporally it just [`Ident`].
/// To obtain type use [`crate::parser::Parse`] trait, do not obtain it from just [`Ident`].
///
/// Please note, type isn't just [`Ident`] and current implementation is subject to change.
///
/// ```text
/// TypeIdent
/// ^~~~~~~~~ ident
/// ```
/// # Future syntax
/// Example of possible syntax for [`Type`] in future:
/// ```text
/// some_lib.Structure(Generic, 42).TraitType
/// ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// | isn't just a identifier
/// | use Parse::parse to obtain Type
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type<'s> {
    // FIXME: type isn't just a identifier. At least it may have generic arguments.
    // But I'm too lazy to implement it before writing working hello-world compiler so temporally it
    // is just identifier.
    #[deprecated(
        since = "TBD",
        note = "`Type` isn't just an `Ident`, this will be changed in future. See `Type` documentation for more"
    )]
    pub ident: Ident<'s>,
}

impl Spanned for Type<'_> {
    #[expect(deprecated_in_future)]
    fn span(&self) -> crate::lex::span::Span {
        self.ident.span()
    }
}
