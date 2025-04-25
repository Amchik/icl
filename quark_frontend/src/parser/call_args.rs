//! # Arguments parser
//!
//! Module provides 3 functions for parsing arguments:
//! - [`parse_call_args`] - parses arguments for [`Expression::Call`].
//! - [`parse_define_args`] - parses arguments for [`crate::ast::doc::DefineStmt`].
//! - [`parse_unknown_args`] - parses arguments for code blocks when both expression and define
//!   statements possible.
//!
//! All functions should start from first token after [`Token::ParenOpen`] and all function finish
//! at [`Token::ParenClose`].
//!
//! ```text
//! "foobar ( arg1, arg2, arg3 )"
//!           ^                ^
//!           |                | token_stream will points here after parsing
//!           | token_stream should points here before parsing
//! ```

use crate::{
    ast::{
        atom::{Atom, Ident},
        block::StructProperty,
        doc::DefineArg,
        expr::Expression,
        single::{Colon, SingleTokenTy},
        ty::Type,
    },
    lex::{self, TokenData, token::Token},
    parser::{ExpectedTokens, Parse, UnexpectedTokenError},
};

use super::{Error, TokenStream};

const EXPECTED_PAREN_CLOSE: Error = Error::UnexpectedToken(
    UnexpectedTokenError::eof().with_expected(ExpectedTokens::One(Token::ParenClose)),
);

/// A result of parsing arguments of unknown type.
pub enum CallArgs<'s> {
    /// Result for only-expressions arguments ([`parse_call_args`]).
    Expression(Vec<Expression<'s>>),
    /// Result for only-define arguments ([`parse_define_args`]).
    Define(Vec<DefineArg<'s>>),
    /// Result that can be either expression or define arguments.
    Unknown(Vec<Ident<'s>>),
}

/// Parse unknown type of arguments. Will return [`CallArgs`] of specific argument type, if found.
/// This function can throw parse error when argument does not match found argument type, e.g.
/// first argument was only-define and second only-expression.
///
/// See module level documentation for more info.
pub fn parse_unknown_args<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
) -> Result<CallArgs<'s>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    let mut idents = Vec::new();

    while token_stream
        .current()
        .map_err(|_| EXPECTED_PAREN_CLOSE)?
        .token
        != Token::ParenClose
    {
        let peek = dbg!(token_stream.peek()?.ok_or(EXPECTED_PAREN_CLOSE)?);
        let current = dbg!(token_stream.current()?);

        match (current.token, peek.token) {
            (Token::Ident, Token::ParenClose) => {
                idents.push(Ident::from(current));
                _ = token_stream.next_token()?;
                break;
            }
            (Token::Ident, Token::Comma) => {
                idents.push(Ident::from(current));
                _ = token_stream.next_token()?;
                _ = token_stream.next_token()?;
            }
            (_, Token::Colon) => {
                let mut args: Vec<_> = idents.into_iter().map(DefineArg::Ident).collect();
                let tail = parse_define_args(token_stream)?;

                args.extend(tail);

                return Ok(CallArgs::Define(args));
            }
            _ => {
                let mut args: Vec<_> = idents
                    .into_iter()
                    .map(|v| Expression::Atom(Atom::Ident(v)))
                    .collect();
                let tail = parse_call_args(token_stream)?;

                args.extend(tail);

                return Ok(CallArgs::Expression(args));
            }
        }
    }

    Ok(CallArgs::Unknown(idents))
}

/// Parse call arguments for expression. See module-level documentation for more.
pub fn parse_call_args<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
) -> Result<Vec<Expression<'s>>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    let mut res = Vec::new();

    while token_stream
        .current()
        .map_err(|_| EXPECTED_PAREN_CLOSE)?
        .token
        != Token::ParenClose
    {
        res.push(Expression::parse(token_stream)?);
    }

    Ok(res)
}

/// Parses arguments for define statement. See module-level documentation for more.
pub fn parse_define_args<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
) -> Result<Vec<DefineArg<'s>>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    let mut res = Vec::new();

    while token_stream
        .current()
        .map_err(|_| EXPECTED_PAREN_CLOSE)?
        .token
        != Token::ParenClose
    {
        let id = match token_stream.next_token()? {
            td @ TokenData {
                token: Token::Ident,
                ..
            } => Ident::from(td),
            _ => {
                return Err(token_stream
                    .unexpected_token()
                    .with_expected([Token::Ident, Token::ParenClose][..].into())
                    .into());
            }
        };

        let arg = match token_stream.current()? {
            TokenData {
                token: Token::Comma | Token::ParenClose,
                ..
            } => DefineArg::Ident(id),
            TokenData {
                token: Token::Colon,
                pos,
                ..
            } => {
                token_stream.next_token()?;

                let prop = StructProperty {
                    ident: id,
                    colon: Colon::from_pos(pos),
                    ty: Type::parse(token_stream)?,
                };

                DefineArg::Typed(prop)
            }
            _ => {
                return Err(token_stream
                    .unexpected_token()
                    .with_expected([Token::Colon, Token::Comma, Token::ParenClose][..].into())
                    .into());
            }
        };

        res.push(arg);

        match token_stream.current()?.token {
            Token::Comma => _ = token_stream.next_token()?,
            Token::ParenClose => break,
            _ => {
                return Err(token_stream
                    .unexpected_token()
                    .with_expected([Token::Comma, Token::ParenClose][..].into())
                    .into());
            }
        }
    }

    Ok(res)
}
