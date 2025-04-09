//! Atoms, Identifiers and Literals

use crate::{
    ast::atom::{Atom, Ident, Literal, LiteralType},
    lex::{self, TokenData, token::Token},
};

use super::Parse;

impl<'s, T> Parse<'s, T> for Atom<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut super::TokenStream<'s, T>) -> Result<Self, super::Error<'s>> {
        let token = token_stream.current()?.token;

        let res = match token {
            Token::Ident => Self::Ident(Ident::parse(token_stream)?),
            Token::LitFalse
            | Token::LitTrue
            | Token::LitInteger
            | Token::LitString
            | Token::LitFloat => Self::Literal(Literal::parse(token_stream)?),
            _ => {
                return Err(token_stream
                    .unexpected_token()
                    .with_expected(
                        [
                            Token::Ident,
                            Token::LitString,
                            Token::LitInteger,
                            Token::LitFloat,
                            Token::LitTrue,
                            Token::LitFalse,
                        ][..]
                            .into(),
                    )
                    .into());
            }
        };

        Ok(res)
    }
}

impl<'s, T> Parse<'s, T> for Ident<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut super::TokenStream<'s, T>) -> Result<Self, super::Error<'s>> {
        let token = token_stream.consume(Token::Ident)?;

        Ok(Self {
            text: token.text,
            pos: token.pos,
        })
    }
}

impl<'s, T> Parse<'s, T> for Literal<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut super::TokenStream<'s, T>) -> Result<Self, super::Error<'s>> {
        let token = token_stream.current()?;
        let ty = match token.token {
            Token::LitTrue | Token::LitFalse => LiteralType::Bool,
            Token::LitString => LiteralType::String,
            Token::LitInteger => LiteralType::Integer,
            Token::LitFloat => LiteralType::Float,
            _ => {
                return Err(token_stream
                    .unexpected_token()
                    .with_expected(
                        [
                            Token::LitString,
                            Token::LitInteger,
                            Token::LitFloat,
                            Token::LitTrue,
                            Token::LitFalse,
                        ][..]
                            .into(),
                    )
                    .into());
            }
        };

        token_stream.next_token()?;

        Ok(Self {
            text: token.text,
            pos: token.pos,
            ty,
        })
    }
}

impl<'s> From<TokenData<'s>> for Ident<'s> {
    fn from(value: TokenData<'s>) -> Self {
        Self {
            text: value.text,
            pos: value.pos,
        }
    }
}
