//! Errors

use std::fmt::Display;

use super::span::Pos;

/// Error from lexer
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// Unexpected [`char`]
    UnexpectedChar { pos: Pos, char: char },
    /// Unexpected end of file
    UnexpectedEOF,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEOF => write!(f, "unexpected end of file"),
            Self::UnexpectedChar { pos, char } => write!(
                f,
                "unexpected char '{}' at {}:{}",
                char, pos.line, pos.column
            ),
        }
    }
}

impl std::error::Error for Error {}
