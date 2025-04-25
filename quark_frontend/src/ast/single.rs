//! Single token structures

use crate::lex::{span::Pos, token::Token};

/// Marker trait for structures in AST that represents just single *simple* token, e.g. [`Semicolon`].
pub trait SingleTokenTy {
    /// Token used to construct this structure.
    const TOKEN: Token;

    /// Obtain structure from it's position.
    fn from_pos(pos: Pos) -> Self;
}

macro_rules! single_token_define {
    ($( Token::$tok:ident -> $name:ident),* $(,)?) => {
        $(
            #[derive(Clone, PartialEq, Eq, Debug)]
            pub struct $name {
                pub pos: Pos,
            }

            impl SingleTokenTy for $name {
                const TOKEN: Token = Token::$tok;

                fn from_pos(pos: Pos) -> Self {
                    Self { pos }
                }
            }
        )+
    };
}

single_token_define!(
    Token::Colon -> Colon,
    Token::Define -> Define,
    Token::Semicolon -> Semicolon,
    Token::RightArrow -> RightArrow,
    Token::Dot -> Dot,
    Token::Ellipsis -> Ellipsis,
    Token::Equal -> Assign,

    Token::ParenOpen -> ParenOpen,
    Token::ParenClose -> ParenClose,

    Token::BraceOpen -> BraceOpen,
    Token::BraceClose -> BraceClose,

    Token::KwIf -> IfKeyword,
    Token::KwWhile -> WhileKeyword,
    Token::KwContinue -> ContinueKeyword,
    Token::KwBreak -> BreakKeyword,
    Token::KwReturn -> ReturnKeyword,
);
