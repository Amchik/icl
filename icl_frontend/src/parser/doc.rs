//! Entire document and define statement parsing

use crate::{
    ast::{
        atom::{Ident, Literal},
        doc::{
            DefineArgs, DefineHint, DefineMeta, DefineMetaArgs, DefineOutput, DefineStmt,
            DefineStmtBody, DefineType, Document, DocumentStmt,
        },
        expr::Expression,
        single::{Colon, Define, ParenClose, ParenOpen, RightArrow, Semicolon, SingleTokenTy},
        stmt::Terminated,
        ty::Type,
    },
    lex::{self, TokenData, token::Token},
    parser::call_args,
};

use super::{Error, Parse, TokenStream};

impl<'s, T> Parse<'s, T> for Document<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        let mut stmts = Vec::new();

        while !token_stream.is_end() {
            stmts.push(DocumentStmt::Define(DefineStmt::parse(token_stream)?));
        }

        Ok(Self { stmts })
    }
}

impl<'s, T> Parse<'s, T> for DefineStmt<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        let name = Ident::parse(token_stream)?;

        let hint = match token_stream.current()?.token {
            Token::ParenOpen => {
                let par_open = ParenOpen::parse(token_stream)?;
                let args = call_args::parse_define_args(token_stream)?;
                let par_close = ParenClose::parse(token_stream)?;

                DefineHint::Arguments(DefineArgs {
                    par_open,
                    par_close,
                    args,
                })
            }
            Token::Colon => {
                let colon = Colon::parse(token_stream)?;
                let ty = Type::parse(token_stream)?;

                DefineHint::Type(DefineType { colon, ty })
            }
            // drop error for this token in next check
            Token::Equal | Token::Define => DefineHint::None,
            _ => {
                return Err(token_stream
                    .unexpected_token()
                    .with_expected([Token::ParenOpen, Token::Colon, Token::Define][..].into())
                    .with_notes(&["expected argument definition, type hint or `:=`"])
                    .into());
            }
        };

        let define = match token_stream.current()? {
            data @ TokenData {
                token: Token::Define,
                ..
            } => {
                _ = token_stream.next_token()?;
                Define::from_pos(data.pos)
            }
            TokenData {
                token: Token::Equal,
                ..
            } => {
                return Err(token_stream
                    .unexpected_token()
                    .with_expected(Token::Define.into())
                    .with_help(&["you may change `=` (assign) to `:=` (define)"])
                    .into());
            }
            TokenData {
                token: Token::Colon,
                ..
            } if matches!(hint, DefineHint::Arguments(_)) => {
                return Err(token_stream
                    .unexpected_token()
                    .with_expected(Token::Define.into())
                    .with_notes(&["function definition cannot have a type"])
                    .with_help(&[
                        "if you need to specify return type for function, use arrow syntax:",
                        "`name() := fn -> ReturnType { ... }`",
                    ])
                    .into());
            }
            _ => {
                return Err(token_stream
                    .unexpected_token()
                    .with_expected(Token::Define.into())
                    .into());
            }
        };

        match token_stream.current()?.token {
            Token::KwFn => {
                _ = token_stream.next_token()?;
                let meta = Parse::parse(token_stream)?;
                let output = Parse::parse(token_stream)?;
                let body = DefineStmtBody::Fn(Parse::parse(token_stream)?);

                Ok(Self {
                    name,
                    hint,
                    define,
                    meta,
                    output,
                    body,
                })
            }
            Token::KwStruct => {
                _ = token_stream.next_token()?;
                let meta = Parse::parse(token_stream)?;
                let output = Parse::parse(token_stream)?;
                let body = DefineStmtBody::Struct(Parse::parse(token_stream)?);

                Ok(Self {
                    name,
                    hint,
                    define,
                    meta,
                    output,
                    body,
                })
            }
            Token::KwClass => Err(token_stream
                .unexpected_token()
                .with_expected(Token::KwStruct.into())
                .with_notes(&["classes not implemented yet"])
                .with_help(&["try use `struct` instead"])
                .into()),
            _ => {
                let expr = Expression::parse(token_stream)?;
                let semicolon = Semicolon::parse(token_stream)?;

                Ok(Self {
                    name,
                    hint,
                    define,
                    meta: None,
                    output: None,
                    body: DefineStmtBody::PlainExpr(Terminated(expr, semicolon)),
                })
            }
        }
    }
}

impl<'s, T> Parse<'s, T> for Option<DefineMeta<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        if !token_stream.check(Token::ParenOpen) {
            return Ok(None);
        }

        let par_open = token_stream.consume(Token::ParenOpen)?.pos;

        let mut args = Vec::new();

        while !token_stream.check(Token::ParenClose) {
            let ident = Ident::parse(token_stream)?;
            let arg = if token_stream.check(Token::Equal) {
                token_stream.consume(Token::Equal)?;
                let literal = Literal::parse(token_stream)?;
                DefineMetaArgs::Pair(ident, literal)
            } else {
                DefineMetaArgs::Single(ident)
            };

            args.push(arg);
        }

        let par_close = token_stream.consume(Token::ParenClose)?.pos;

        Ok(Some(DefineMeta {
            args,
            par_open: ParenOpen::from_pos(par_open),
            par_close: ParenClose::from_pos(par_close),
        }))
    }
}

impl<'s, T> Parse<'s, T> for Option<DefineOutput<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        if !token_stream.check(Token::RightArrow) {
            return Ok(None);
        }

        let pos = token_stream.consume(Token::RightArrow)?.pos;
        let ty = Type::parse(token_stream)?;

        Ok(Some(DefineOutput {
            arrow: RightArrow::from_pos(pos),
            ty,
        }))
    }
}
