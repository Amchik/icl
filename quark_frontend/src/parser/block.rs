//! Code and struct blocks

use crate::{
    ast::{
        atom::Ident,
        block::{Block, BuiltinBlock, CodeBlock, StructBlock, StructProperty},
        single::{BraceClose, BraceOpen, Colon, Ellipsis},
        stmt::Statement,
        ty::Type,
    },
    lex::{self, TokenData, token::Token},
};

use super::{Error, Parse, TokenStream};

impl<'s, T> Parse<'s, T> for Block<CodeBlock<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    #[inline]
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        let start = BraceOpen::parse(token_stream)?;

        let res = if token_stream.check(Token::Ellipsis) {
            let dots = Ellipsis::parse(token_stream)?;
            let end = BraceClose::parse(token_stream)?;

            Self::Builtin(BuiltinBlock { start, end, dots })
        } else {
            let stmts = parse_stmts(token_stream)?;
            let end = BraceClose::parse(token_stream)?;

            Self::Normal(CodeBlock { start, end, stmts })
        };

        Ok(res)
    }
}

impl<'s, T> Parse<'s, T> for Block<StructBlock<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    #[inline]
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        let start = BraceOpen::parse(token_stream)?;

        let res = if token_stream.check(Token::Ellipsis) {
            let dots = Ellipsis::parse(token_stream)?;
            let end = BraceClose::parse(token_stream)?;

            Self::Builtin(BuiltinBlock { start, end, dots })
        } else {
            let props = parse_struct_props(token_stream)?;
            let end = BraceClose::parse(token_stream)?;

            Self::Normal(StructBlock { start, end, props })
        };

        Ok(res)
    }
}

impl<'s, T> Parse<'s, T> for CodeBlock<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    #[inline]
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        let start = BraceOpen::parse(token_stream)?;
        let stmts = parse_stmts(token_stream)?;
        let end = BraceClose::parse(token_stream)?;

        Ok(Self { start, end, stmts })
    }
}

impl<'s, T> Parse<'s, T> for StructBlock<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    #[inline]
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        let start = BraceOpen::parse(token_stream)?;
        let props = parse_struct_props(token_stream)?;
        let end = BraceClose::parse(token_stream)?;

        Ok(Self { start, end, props })
    }
}

fn parse_stmts<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
) -> Result<Vec<Statement<'s>>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    let mut stmts = Vec::new();

    while !token_stream.check(Token::BraceClose) {
        stmts.push(Statement::parse(token_stream)?);
    }

    Ok(stmts)
}

fn parse_struct_props<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
) -> Result<Vec<StructProperty<'s>>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    let mut props = Vec::new();

    while !token_stream.check(Token::BraceClose) {
        let ident = Ident::parse(token_stream)?;
        let colon = Colon::parse(token_stream)?;
        let ty = Type::parse(token_stream)?;
        props.push(StructProperty { ident, colon, ty });

        match token_stream.current()?.token {
            Token::Comma => _ = token_stream.next_token()?,
            Token::ParenClose => break,
            _ => {
                return Err(token_stream
                    .unexpected_token()
                    .with_expected([Token::Comma, Token::ParenClose][..].into())
                    .into());
            }
        };
    }

    Ok(props)
}
