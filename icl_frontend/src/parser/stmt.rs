//! Statements parsing

use crate::{
    ast::{
        atom::{Atom, Ident},
        block::CodeBlock,
        doc::{
            DefineArg, DefineArgs, DefineHint, DefineMeta, DefineOutput, DefineStmt,
            DefineStmtBody, DefineType,
        },
        expr::{CallExpr, Expression},
        single::{
            Assign, Colon, Define, IfKeyword, ParenClose, ParenOpen, ReturnKeyword, Semicolon,
            SingleTokenTy, WhileKeyword,
        },
        stmt::{AssignStmt, IfElseStmt, IfStmt, ReturnStmt, Statement, Terminated, WhileStmt},
        ty::Type,
    },
    lex::{self, TokenData, token::Token},
    parser::call_args,
};

use super::{Error, Parse, TokenStream, call_args::CallArgs, expr::parse_expr_no_equal};

impl<'s, T, V> Parse<'s, T> for Terminated<V>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
    V: Parse<'s, T>,
{
    #[inline]
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        let value = V::parse(token_stream)?;
        let semicolon = Semicolon::parse(token_stream)?;
        Ok(Self(value, semicolon))
    }
}

impl<'s, T> Parse<'s, T> for Statement<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut super::TokenStream<'s, T>) -> Result<Self, super::Error<'s>> {
        match token_stream.current()?.token {
            Token::Ident => parse_expr_like_stmts(token_stream),

            Token::KwIf => Ok(Self::If(IfStmt::parse(token_stream)?)),
            Token::KwWhile => Ok(Self::While(WhileStmt::parse(token_stream)?)),

            Token::KwReturn => Ok(Self::Return(Terminated::parse(token_stream)?)),
            Token::KwContinue => Ok(Self::Continue(Terminated::parse(token_stream)?)),
            Token::KwBreak => Ok(Self::Break(Terminated::parse(token_stream)?)),

            _ => parse_assign_or_expr(token_stream, None),
        }
    }
}

fn parse_expr_like_stmts<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
) -> Result<Statement<'s>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    let ident = Ident::parse(token_stream)?;

    if token_stream.check(Token::Colon) {
        let pos = token_stream.next_token()?.pos;
        let ty = Type::parse(token_stream)?;

        parse_define_stmt(
            token_stream,
            ident,
            DefineHint::Type(DefineType {
                ty,
                colon: Colon::from_pos(pos),
            }),
        )
    } else if token_stream.check(Token::ParenOpen) {
        let par_open = ParenOpen::from_pos(token_stream.next_token()?.pos);
        let args = call_args::parse_unknown_args(token_stream)?;
        let par_close = ParenClose::from_pos(token_stream.consume(Token::ParenClose)?.pos);

        if token_stream.check(Token::Define) {
            let args = match args {
                CallArgs::Define(args) => args,
                CallArgs::Unknown(args) => args.into_iter().map(DefineArg::Ident).collect(),
                CallArgs::Expression(_) => return Err(token_stream.unexpected_token().with_notes(&[
                        "this syntax is invalid for definition"
                ]).with_help(&[
                        "you may use `=` operator instead of `:=` to assign value",
                        "if you need to mutate reference you may use `*method_call() = value;` syntax",
                ]).into())
            };
            parse_define_stmt(
                token_stream,
                ident,
                DefineHint::Arguments(DefineArgs {
                    par_open,
                    par_close,
                    args,
                }),
            )
        } else if token_stream.check(Token::Colon) {
            return Err(token_stream
                .unexpected_token()
                .with_notes(&["this syntax is invalid for definition or expression"])
                .with_help(&[
                    "if you need to specify return type of function use this syntax:",
                    "`name() := fn -> ReturnType { ... }`",
                ])
                .into());
        } else {
            let args = match args {
                CallArgs::Expression(args) => args,
                CallArgs::Unknown(args) => args
                    .into_iter()
                    .map(|v| Expression::Atom(Atom::Ident(v)))
                    .collect(),
                CallArgs::Define(_) => {
                    return Err(Error::UnexpectedToken(
                        super::UnexpectedTokenError::new(TokenData {
                            text: ":",
                            pos: par_open.pos,
                            token: Token::Colon,
                        })
                        .with_notes(&[
                            "arguments in this function call are invalid",
                            "this syntax is used only for definitions",
                        ]),
                    ));
                }
            };

            let lhs = Expression::Call(CallExpr {
                args,
                par_open,
                par_close,
                callee: Box::new(Expression::Atom(Atom::Ident(ident))),
            });

            parse_assign_or_expr(token_stream, Some(lhs))
        }
    } else if token_stream.check(Token::Define) {
        parse_define_stmt(token_stream, ident, DefineHint::None)
    } else {
        parse_assign_or_expr(token_stream, Some(Expression::Atom(Atom::Ident(ident))))
    }
}

fn parse_assign_or_expr<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
    lhs: Option<Expression<'s>>,
) -> Result<Statement<'s>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    let lhs = parse_expr_no_equal(token_stream, lhs)?;

    let stmt = if token_stream.check(Token::Equal) {
        let pos = token_stream.next_token()?.pos;
        let rhs = Parse::parse(token_stream)?;
        let semicolon = Semicolon::from_pos(token_stream.consume(Token::Semicolon)?.pos);
        Statement::Assign(Terminated(
            AssignStmt {
                lhs,
                rhs,
                assign: Assign::from_pos(pos),
            },
            semicolon,
        ))
    } else {
        let semicolon = Semicolon::from_pos(token_stream.consume(Token::Semicolon)?.pos);

        Statement::Expression(Terminated(lhs, semicolon))
    };

    Ok(stmt)
}

fn parse_define_stmt<'s, T>(
    token_stream: &mut TokenStream<'s, T>,
    ident: Ident<'s>,
    hint: DefineHint<'s>,
) -> Result<Statement<'s>, Error<'s>>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    let define = token_stream.consume(Token::Define)?.pos;
    let define = Define::from_pos(define);

    match token_stream.current()?.token {
        Token::KwFn => {
            token_stream.next_token()?;
            let meta = Option::<DefineMeta>::parse(token_stream)?;
            let output = Option::<DefineOutput>::parse(token_stream)?;

            let block = Parse::parse(token_stream)?;

            Ok(Statement::Define(DefineStmt {
                name: ident,
                hint,
                define,
                meta,
                output,
                body: DefineStmtBody::Fn(block),
            }))
        }
        Token::KwClass => Err(token_stream
            .unexpected_token()
            .with_notes(&["classes isn't support yet, use structures"])
            .with_help(&["replace `class` keyword with `struct`"])
            .into()),
        Token::KwStruct => {
            token_stream.next_token()?;
            let meta = Option::<DefineMeta>::parse(token_stream)?;
            let output = Option::<DefineOutput>::parse(token_stream)?;

            let block = Parse::parse(token_stream)?;

            Ok(Statement::Define(DefineStmt {
                name: ident,
                hint,
                define,
                meta,
                output,
                body: DefineStmtBody::Struct(block),
            }))
        }
        _ => {
            let expr = Expression::parse(token_stream)?;
            let semicolon = token_stream.consume(Token::Semicolon)?.pos;
            let semicolon = Semicolon::from_pos(semicolon);

            Ok(Statement::Define(DefineStmt {
                name: ident,
                hint,
                define,
                meta: None,
                output: None,
                body: DefineStmtBody::PlainExpr(Terminated(expr, semicolon)),
            }))
        }
    }
}

impl<'s, T> Parse<'s, T> for IfStmt<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut super::TokenStream<'s, T>) -> Result<Self, super::Error<'s>> {
        let start = IfKeyword::parse(token_stream)?;
        let cond = Expression::parse(token_stream)?;
        let body = CodeBlock::parse(token_stream)?;

        let else_branch = if token_stream.check(Token::KwElse) {
            token_stream.next_token()?;
            match token_stream.current()?.token {
                Token::KwIf => IfElseStmt::If(Box::new(IfStmt::parse(token_stream)?)),
                Token::BraceOpen => IfElseStmt::Else(CodeBlock::parse(token_stream)?),
                _ => {
                    return Err(token_stream
                        .unexpected_token()
                        .with_expected([Token::KwIf, Token::BraceOpen][..].into())
                        .into());
                }
            }
        } else {
            IfElseStmt::None
        };

        Ok(Self {
            start,
            cond,
            body,
            else_branch,
        })
    }
}

impl<'s, T> Parse<'s, T> for WhileStmt<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut super::TokenStream<'s, T>) -> Result<Self, super::Error<'s>> {
        let start = WhileKeyword::parse(token_stream)?;
        let cond = Expression::parse(token_stream)?;
        let body = CodeBlock::parse(token_stream)?;

        Ok(Self { start, cond, body })
    }
}

impl<'s, T> Parse<'s, T> for ReturnStmt<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut super::TokenStream<'s, T>) -> Result<Self, super::Error<'s>> {
        let ret = ReturnKeyword::parse(token_stream)?;

        let expr = if token_stream.check(Token::Semicolon) {
            None
        } else {
            Some(Expression::parse(token_stream)?)
        };

        Ok(Self { ret, value: expr })
    }
}
