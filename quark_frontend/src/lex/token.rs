//! Tokens

use std::fmt::Display;

/// Token kind for [`super::TokenData`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    /// Identifier of something.
    Ident,
    /// String literal.
    LitString,
    /// Integer literal.
    LitInteger,
    /// Float literal.
    LitFloat,
    /// Boolean `true` literal.
    LitTrue,
    /// Boolean `false` literal.
    LitFalse,

    /// C-style comment (`/* comment */`)
    CommentBlock,
    /// C++-style comment (`// comment`)
    CommentLine,
    /// Doc-comment (`/// Documentation`)
    CommentDoc,

    /// `:=`
    Define,
    /// `=`
    Equal,
    /// `->`
    RightArrow,
    /// `:`
    Colon,
    /// `;`
    Semicolon,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `..`
    DotDot,
    /// `...`
    Ellipsis,

    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Asterisk,
    /// `/`
    SingleSlash,

    /// `>`
    Gt,
    /// `<`
    Lt,
    /// `>=`
    GtEq,
    /// `<=`
    LtEq,

    /// `<<`
    Shl,
    /// `>>`
    Shr,

    /// `(`
    ParenOpen,
    /// `)`
    ParenClose,

    /// `{`
    BraceOpen,
    /// `}`
    BraceClose,

    /// `struct` keyword.
    KwStruct,
    /// `class` keyword.
    KwClass,
    /// `fn` keyword.
    KwFn,
    /// `mut` keyword.
    KwMut,
    /// `if` keyword.
    KwIf,
    /// `else` keyword.
    KwElse,
    /// `while` keyword.
    KwWhile,
    /// `continue` keyword.
    KwContinue,
    /// `break` keyword.
    KwBreak,

    /// `return` keyword.
    KwReturn,
}

impl Token {
    /// Get token kind from word.
    ///
    /// # Example
    /// ```
    /// use quark_frontend::lex::token::Token;
    ///
    /// assert_eq!(Token::from_word("struct"), Token::KwStruct);
    /// assert_eq!(Token::from_word("not_keyword"), Token::Ident);
    /// ```
    pub fn from_word(ident: &str) -> Self {
        match ident {
            "true" => Token::LitTrue,
            "false" => Token::LitFalse,

            "struct" => Token::KwStruct,
            "class" => Token::KwClass,
            "fn" => Token::KwFn,
            "mut" => Token::KwMut,

            "if" => Token::KwIf,
            "else" => Token::KwElse,
            "while" => Token::KwWhile,
            "continue" => Token::KwContinue,
            "break" => Token::KwBreak,

            "return" => Token::KwReturn,

            _ => Token::Ident,
        }
    }

    /// Return `true` if token is useless comment
    pub fn is_non_comment(self) -> bool {
        !matches!(
            self,
            Token::CommentDoc | Token::CommentLine | Token::CommentBlock
        )
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident => write!(f, "identifier"),

            Token::LitFloat => write!(f, "float literal"),
            Token::LitInteger => write!(f, "integer literal"),
            Token::LitString => write!(f, "string literal"),
            Token::LitTrue => write!(f, "'true'"),
            Token::LitFalse => write!(f, "'false'"),

            Token::CommentBlock => write!(f, "'/* … */'"),
            Token::CommentLine => write!(f, "'// …'"),
            Token::CommentDoc => write!(f, "'/// …'"),

            Token::Define => write!(f, "':='"),
            Token::Equal => write!(f, "'='"),
            Token::RightArrow => write!(f, "'->'"),
            Token::Colon => write!(f, "':'"),
            Token::Semicolon => write!(f, "';'"),
            Token::Comma => write!(f, "','"),
            Token::Dot => write!(f, "'.'"),
            Token::DotDot => write!(f, "'..'"),
            Token::Ellipsis => write!(f, "'...'"),

            Token::Plus => write!(f, "'+'"),
            Token::Minus => write!(f, "'-'"),
            Token::Asterisk => write!(f, "'*'"),
            Token::SingleSlash => write!(f, "'/'"),

            Token::Gt => write!(f, "'>'"),
            Token::Lt => write!(f, "'<'"),
            Token::GtEq => write!(f, "'>='"),
            Token::LtEq => write!(f, "'<='"),

            Token::Shl => write!(f, "'<<'"),
            Token::Shr => write!(f, "'>>'"),

            Token::ParenOpen => write!(f, "'('"),
            Token::ParenClose => write!(f, "')'"),

            Token::BraceOpen => write!(f, "'{{'"),
            Token::BraceClose => write!(f, "'}}'"),

            Token::KwStruct => write!(f, "'struct'"),
            Token::KwFn => write!(f, "'fn'"),
            Token::KwClass => write!(f, "'class'"),
            Token::KwMut => write!(f, "'mut'"),
            Token::KwIf => write!(f, "'if'"),
            Token::KwElse => write!(f, "'else'"),
            Token::KwWhile => write!(f, "'while'"),
            Token::KwContinue => write!(f, "'continue'"),
            Token::KwBreak => write!(f, "'break'"),
            Token::KwReturn => write!(f, "'return'"),
        }
    }
}

impl Token {
    /// Returns `true` if byte NOT allowed in middle or end of ident.
    pub const fn is_disallowed_ident_byte(b: u8) -> bool {
        // #[allow(clippy::ugly)]
        matches!(
            b,
            b'!' | b'@'
                | b'#'
                | b'$'
                | b'%'
                | b'^'
                | b'&'
                | b'*'
                | b'('
                | b')'
                | b'-'
                | b'+'
                | b'='
                | b'{'
                | b'}'
                | b'['
                | b']'
                | b'|'
                | b'\\'
                | b':'
                | b';'
                | b'\''
                | b'"'
                | b'<'
                | b'>'
                | b','
                | b'.'
                | b'?'
                | b'/'
                | b'~'
                | b'`'
                | b'\n'
                | b'\r'
                | b' '
                | b'\t'
        )
    }

    // See [`Self::is_disallowed_ident_byte`]
    pub fn is_disallowed_ident_char(c: char) -> bool {
        if let Ok(b) = u8::try_from(c) {
            Self::is_disallowed_ident_byte(b)
        } else {
            // All other unicode chars are allowed
            false
        }
    }
}
