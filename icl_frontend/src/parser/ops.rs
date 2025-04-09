//! Unary and binary operators parsing

use crate::{
    ast::ops::{BinOp, UnOp},
    lex::token::Token,
};

impl TryFrom<Token> for UnOp {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        let res = match value {
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,

            // TODO: (lex) Token::Ampersand => Self::Ref,
            Token::Asterisk => Self::Deref,

            _ => return Err(()),
        };

        Ok(res)
    }
}

impl TryFrom<Token> for BinOp {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        let res = match value {
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::Asterisk => Self::Mult,
            Token::SingleSlash => Self::Div,

            Token::Equal => Self::Equal,
            // TODO: (lex) Token::NotEqual => Self::NotEqual,
            Token::Gt => Self::Great,
            Token::Lt => Self::Less,
            Token::GtEq => Self::GreatEqual,
            Token::LtEq => Self::LessEqual,

            _ => return Err(()),
        };

        Ok(res)
    }
}
