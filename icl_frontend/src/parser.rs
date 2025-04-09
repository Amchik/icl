//! # Abstract Syntax Tree parser
//!
//! Describes and implements [`Parse`] trait. See [`TokenStream`] to create it and parse
//! [`crate::ast::doc::Document`].

// TODO: move errors to parser/error.rs and token stream to parser/token_stream.rs
pub mod atom;
pub mod block;
pub mod call_args;
pub mod doc;
pub mod expr;
pub mod ops;
pub mod stmt;
pub mod ty;

use std::iter::Peekable;

use crate::{
    ast::single::SingleTokenTy,
    lex::{TokenData, error::Error as LexError, token::Token},
};

/// Token stream.
pub struct TokenStream<'s, T: Iterator> {
    tokens: Peekable<T>,
    current: TokenData<'s>,

    is_end: bool,
}

/// Error of parsing AST.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error<'s> {
    Lex(LexError),
    UnexpectedToken(UnexpectedTokenError<'s>),
}

impl From<LexError> for Error<'_> {
    fn from(value: LexError) -> Self {
        Self::Lex(value)
    }
}
impl<'s> From<UnexpectedTokenError<'s>> for Error<'s> {
    fn from(value: UnexpectedTokenError<'s>) -> Self {
        Self::UnexpectedToken(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnexpectedTokenError<'s> {
    pub token: Option<TokenData<'s>>,
    pub expected: ExpectedTokens,
    pub notes: &'static [&'static str],
    pub help: &'static [&'static str],
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpectedTokens {
    None,
    One(Token),
    Static(&'static [Token]),
}

impl From<Token> for ExpectedTokens {
    fn from(value: Token) -> Self {
        Self::One(value)
    }
}
impl From<&'static [Token]> for ExpectedTokens {
    fn from(value: &'static [Token]) -> Self {
        if let [token] = value {
            Self::One(*token)
        } else if value.is_empty() {
            Self::None
        } else {
            Self::Static(value)
        }
    }
}

impl<'s> UnexpectedTokenError<'s> {
    pub const fn new(token: TokenData<'s>) -> Self {
        Self {
            token: Some(token),
            expected: ExpectedTokens::None,
            notes: &[],
            help: &[],
        }
    }
    pub const fn eof() -> Self {
        Self {
            token: None,
            expected: ExpectedTokens::None,
            notes: &[],
            help: &[],
        }
    }

    #[inline(always)]
    pub const fn with_token(mut self, token: TokenData<'s>) -> Self {
        self.token = Some(token);
        self
    }

    #[inline(always)]
    pub const fn with_expected(mut self, expected: ExpectedTokens) -> Self {
        self.expected = expected;
        self
    }
    #[inline(always)]
    pub const fn with_notes(mut self, notes: &'static [&'static str]) -> Self {
        self.notes = notes;
        self
    }
    #[inline(always)]
    pub const fn with_help(mut self, help: &'static [&'static str]) -> Self {
        self.help = help;
        self
    }
}

impl<'s, T> TokenStream<'s, T>
where
    T: Iterator<Item = Result<TokenData<'s>, LexError>>,
{
    /// Initialize [`TokenStream`]. Throws error if lexer throw an error while obtaining first
    /// token or `iter` is empty.
    pub fn init(iter: T) -> Result<Self, Error<'s>> {
        let mut iter = iter.peekable();

        let current = match iter.next() {
            Some(Ok(c)) => c,
            Some(Err(e)) => return Err(Error::Lex(e)),
            None => {
                return Err(Error::UnexpectedToken(
                    UnexpectedTokenError::eof()
                        .with_notes(&["token stream can't be constructed without any tokens"]),
                ));
            }
        };

        Ok(Self {
            tokens: iter,
            current,
            is_end: false,
        })
    }

    /// Gets the current token. Throws error when there no current token (end of file).
    pub fn current(&self) -> Result<TokenData<'s>, Error<'s>> {
        if self.is_end {
            Err(Error::UnexpectedToken(UnexpectedTokenError::eof()))
        } else {
            Ok(self.current.clone())
        }
    }

    /// Gets the next token without changing stream position. Throws error if stream is empty (no
    /// current token) or if lexer returned own error. Returns [`None`] if current token is last
    /// token.
    pub fn peek(&mut self) -> Result<Option<TokenData<'s>>, Error<'s>> {
        if self.is_end {
            Err(Error::UnexpectedToken(UnexpectedTokenError::eof()))
        } else if let Some(token) = self.tokens.peek() {
            Ok(Some(token.clone()?))
        } else {
            Ok(None)
        }
    }

    /// Checks token on current position.
    pub fn check(&self, token: Token) -> bool {
        !self.is_end && self.current.token == token
    }

    /// Fetchs next token (if any) and return previous token. Throws error if there no current
    /// token (already on end of file).
    /// Can also return error from lexer.
    pub fn next_token(&mut self) -> Result<TokenData<'s>, Error<'s>> {
        let current = self.current()?;

        match self.tokens.next() {
            Some(next) => self.current = next?,
            None => self.is_end = true,
        };

        Ok(current)
    }

    /// Checks for current token and fetchs next token, otherwise throws unexpected token error.
    /// Can also return error from lexer.
    pub fn consume(&mut self, token: Token) -> Result<TokenData<'s>, Error<'s>> {
        if self.check(token) {
            Ok(self.next_token()?)
        } else {
            Err(self.unexpected_token().with_expected(token.into()).into())
        }
    }

    /// Returns `true` if no tokens left
    pub fn is_end(&self) -> bool {
        self.is_end
    }

    /// Returns [`UnexpectedTokenError`] for current token (or nothing if there no current token).
    pub fn unexpected_token(&self) -> UnexpectedTokenError<'s> {
        if let Ok(current) = self.current() {
            UnexpectedTokenError::new(current)
        } else {
            UnexpectedTokenError::eof()
        }
    }
}

/// A trait for types that can be constructed by parsing a stream of tokens.
/// Implementations define how to recognize and convert a sequence of tokens into an AST node or intermediate structure.
pub trait Parse<'s, T: Iterator>: Sized {
    /// Attempts to parse `Self` from the given token stream, returning either a successful result or an error describing why parsing failed.
    /// The implementation consumes tokens from the stream as needed to produce a complete value.
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>>;
}

impl<'s, T, S> Parse<'s, T> for S
where
    T: Iterator<Item = Result<TokenData<'s>, LexError>>,
    S: SingleTokenTy,
{
    fn parse(token_stream: &mut TokenStream<'s, T>) -> Result<Self, Error<'s>> {
        token_stream
            .consume(Self::TOKEN)
            .map(|v| Self::from_pos(v.pos))
    }
}
