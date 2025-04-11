//! Atoms, Identifiers and Literals

use std::borrow::Cow;

use crate::{
    ast::atom::{Atom, Ident, Literal, LiteralData},
    lex::{self, token::Token, TokenData},
};

use super::{LiteralParsingError, LiteralParsingErrorKind, Parse};

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
            | Token::LitFloat => Self::Literal(LiteralData::parse(token_stream)?),
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

impl<'s, T> Parse<'s, T> for LiteralData<'s>
where
    T: Iterator<Item = Result<TokenData<'s>, lex::error::Error>>,
{
    fn parse(token_stream: &mut super::TokenStream<'s, T>) -> Result<Self, super::Error<'s>> {
        let token = token_stream.current()?;
        let ty = match token.token {
            Token::LitTrue => Literal::Bool(true),
            Token::LitFalse => Literal::Bool(false),
            Token::LitString => Literal::String(
                parse_string(token.text).map_err(|k| LiteralParsingError::new(token.clone(), k))?,
            ),
            Token::LitInteger => Literal::Integer(
                parse_int_prefix(token.text)
                    .map_err(|k| LiteralParsingError::new(token.clone(), k))?,
            ),
            Token::LitFloat => Literal::Float(
                parse_float(token.text).map_err(|k| LiteralParsingError::new(token.clone(), k))?,
            ),
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

fn parse_string(text: &str) -> Result<Cow<'_, str>, LiteralParsingErrorKind> {
    // remove "
    let text = &text[1..text.len().wrapping_sub(1)];

    let mut parts = String::new();
    let mut remainder = text;

    let mut chars = text.char_indices();

    while let Some((idx, c)) = chars.next() {
        match c {
            '\\' => {
                let (_, next) = chars.next().expect("lexer should return valid string");
                let code = match next {
                    '\\' => '\\',
                    '"' => '"',
                    'n' => '\n',
                    't' => '\t',
                    _ => return Err(LiteralParsingErrorKind::InvalidStringEscape(next)),
                };
                let (prev_str, next_str) = remainder.split_at(idx);
                remainder = &next_str[next.len_utf8()..];
                parts.push_str(prev_str);
                parts.push(code);
                chars = remainder.char_indices();
            }
            _ => continue,
        }
    }

    if parts.is_empty() {
        Ok(Cow::Borrowed(text))
    } else {
        parts.push_str(remainder);
        Ok(Cow::Owned(parts))
    }
}

fn parse_int_prefix(text: &str) -> Result<usize, LiteralParsingErrorKind> {
    let (radix, text) = match text.as_bytes() {
        [b'0', b'x', ..] => (16, &text[2..]),
        [b'0', b'o', ..] => (8, &text[2..]),
        [b'0', b'b', ..] => (2, &text[2..]),
        _ => (10, text),
    };

    parse_int(text, radix)
}

fn parse_float(text: &str) -> Result<(usize, usize), LiteralParsingErrorKind> {
    let (int, frac) = text
        .split_once('.')
        .expect("lexer should return correct float literal");

    let int = parse_int(int, 10);
    let frac = parse_int(frac, 10);

    match (int, frac) {
        (Ok(int), Ok(frac)) => Ok((int, frac)),
        (Err(LiteralParsingErrorKind::InvalidIntegerDigit), _)
        | (_, Err(LiteralParsingErrorKind::InvalidIntegerDigit)) => {
            Err(LiteralParsingErrorKind::InvalidFloatDigit)
        }
        (Err(e), _) | (_, Err(e)) => Err(e),
    }
}

fn parse_int(text: &str, radix: u8) -> Result<usize, LiteralParsingErrorKind> {
    let mut res = 0_usize;

    for c in text.bytes() {
        let digit = match c {
            b'_' => continue,

            b'0' | b'1' => c.wrapping_sub(b'0'),
            b'2'..=b'7' if radix > 2 => c.wrapping_sub(b'0'),
            b'8' | b'9' if radix > 8 => c.wrapping_sub(b'0'),

            b'a'..=b'f' if radix == 16 => c.wrapping_sub(const { b'a' + 10 }),
            b'A'..=b'F' if radix == 16 => c.wrapping_sub(const { b'A' + 10 }),

            _ => return Err(LiteralParsingErrorKind::InvalidIntegerDigit),
        };
        if let Some(r) = res
            .checked_mul(radix.into())
            .and_then(|r| r.checked_add(digit.into()))
        {
            res = r;
        } else {
            return Err(LiteralParsingErrorKind::IntegerTooBig);
        }
    }

    Ok(res)
}

impl<'s> From<TokenData<'s>> for Ident<'s> {
    fn from(value: TokenData<'s>) -> Self {
        Self {
            text: value.text,
            pos: value.pos,
        }
    }
}
