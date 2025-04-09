//! Expressions parsing.

// main idea of implementation pasted from:
// https://github.com/DemetryF/smpl/blob/06e92c2a/compiler/frontend/smplc_parse/src/parse/expr.rs
use crate::{
    ast::{
        atom::Atom,
        expr::{CallExpr, Expression, InfixExpr, MaybeWrapped, MemberExpr, PrefixExpr},
        ops::{BinOp, UnOp},
        single::{Dot, ParenClose, ParenOpen, SingleTokenTy},
    },
    lex::{self, TokenData, token::Token},
};

use super::{Error, Parse, TokenStream, call_args};

impl<'s, T, P> Parse<'s, T> for MaybeWrapped<P>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
    P: Parse<'s, T>,
{
    #[inline]
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        if token_stream.check(Token::ParenOpen) {
            let par_open = ParenOpen::parse(token_stream)?;
            let value = P::parse(token_stream)?;
            let par_close = ParenClose::parse(token_stream)?;

            Ok(Self::Wrapped {
                par_open,
                par_close,
                value,
            })
        } else {
            Ok(Self::Normal(P::parse(token_stream)?))
        }
    }
}

const EXPECTED_IN_ONE: super::UnexpectedTokenError = super::UnexpectedTokenError::eof()
    .with_expected(super::ExpectedTokens::Static(&[
        Token::Ident,
        Token::LitTrue,
        Token::LitFalse,
        Token::LitString,
        Token::LitInteger,
        Token::LitFloat,
        Token::ParenOpen,
        Token::Plus,
        Token::Minus,
        Token::Asterisk,
        Token::SingleSlash,
        Token::Equal,
        // TODO: (lex) Token::NotEqual,
        Token::Gt,
        Token::Lt,
        Token::GtEq,
        Token::LtEq,
    ]));

impl<'s, T> Parse<'s, T> for Expression<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    #[inline(always)]
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        parse_expr_with_power(token_stream, 0, true)
    }
}

#[inline(always)]
pub fn parse_expr_no_equal<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
    lhs: Option<Expression<'s>>,
) -> Result<Expression<'s>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    if let Some(lhs) = lhs {
        parse_expr_with_power_from(token_stream, lhs, 0, false)
    } else {
        parse_expr_with_power(token_stream, 0, false)
    }
}

#[inline(always)]
fn parse_expr_with_power<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
    min_pow: u8,
    allow_equal: bool,
) -> Result<Expression<'s>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    let lhs = parse_one(token_stream, allow_equal)?;

    parse_expr_with_power_from(token_stream, lhs, min_pow, allow_equal)
}

fn parse_expr_with_power_from<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
    mut lhs: Expression<'s>,
    min_pow: u8,
    allow_equal: bool,
) -> Result<Expression<'s>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    loop {
        match token_stream.current()? {
            data @ TokenData {
                token: Token::Dot, ..
            } => {
                token_stream.next_token()?;
                let rhs = Atom::parse(token_stream)?;

                lhs = Expression::Member(MemberExpr {
                    dot: Dot::from_pos(data.pos),
                    lhs: Box::new(lhs),
                    rhs,
                });
            }

            data @ TokenData {
                token: Token::ParenOpen,
                ..
            } => {
                let par_open = ParenOpen::from_pos(data.pos);
                token_stream.next_token()?;
                let args = call_args::parse_call_args(token_stream)?;
                let par_close = ParenClose::from_pos(token_stream.consume(Token::ParenClose)?.pos);

                lhs = Expression::Call(CallExpr {
                    par_open,
                    par_close,
                    args,
                    callee: Box::new(lhs),
                });
            }

            TokenData {
                token: Token::Equal,
                ..
            } if !allow_equal => break,

            data @ TokenData { token, .. } => {
                if let Ok(op) = BinOp::try_from(token) {
                    let (lp, rp) = op.power();

                    if lp < min_pow {
                        break;
                    }
                    token_stream.next_token()?;

                    let rhs = parse_expr_with_power(token_stream, rp, allow_equal)?;

                    lhs = Expression::Infix(InfixExpr {
                        op,
                        op_pos: data.pos,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    });
                } else {
                    break;
                }
            }
        }
    }

    Ok(lhs)
}

fn parse_one<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
    allow_equal: bool,
) -> Result<Expression<'s>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    let res = match token_stream.current()? {
        TokenData {
            token:
                Token::Ident
                | Token::LitTrue
                | Token::LitFalse
                | Token::LitString
                | Token::LitInteger
                | Token::LitFloat,
            ..
        } => Expression::Atom(Atom::parse(token_stream)?),
        TokenData {
            token: Token::ParenOpen,
            ..
        } => {
            token_stream.next_token()?;

            let expr = Expression::parse(token_stream)?;

            token_stream.consume(Token::ParenClose)?;

            expr
        }
        td @ TokenData { token, .. } => {
            if let Ok(op) = UnOp::try_from(token) {
                let pow = op.power();

                let rhs = parse_expr_with_power(token_stream, pow, allow_equal)?;

                let pos = td.pos;
                Expression::Prefix(PrefixExpr {
                    op,
                    pos,
                    rhs: Box::new(rhs),
                })
            } else {
                return Err(EXPECTED_IN_ONE.with_token(td).into());
            }
        }
    };

    Ok(res)
}
