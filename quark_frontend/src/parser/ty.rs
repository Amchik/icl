//! Types parsing

use crate::{
    ast::{atom::Ident, ty::Type},
    lex::{self, TokenData},
};

use super::Parse;

impl<'s, T> Parse<'s, T> for Type<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    // FIXME: see [`Type`] documentation
    #[expect(deprecated_in_future)]
    fn parse(token_stream: &mut super::TokenStream<'s, T>) -> Result<Self, super::Error<'s>> {
        let id = Ident::parse(token_stream)?;

        Ok(Self { ident: id })
    }
}
