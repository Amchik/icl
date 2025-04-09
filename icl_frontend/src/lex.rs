//! # Lexer
//!
//! See [`Lex`] for lexer and [`Token`] for tokens information.

use std::{iter::Peekable, str::CharIndices};

use error::Error as LexError;
use span::Pos;
use token::Token;

pub mod error;
pub mod span;
pub mod token;

#[cfg(test)]
mod test;

/// Information about parsed token
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenData<'s> {
    /// Text of token
    pub text: &'s str,
    /// Position
    pub pos: Pos,
    /// Token kind
    pub token: Token,
}

/// Lexer and iterator. Can be created with [`Lex::new`].
pub struct Lex<'s> {
    pub text: &'s str,
    iter: Peekable<CharIndices<'s>>,
    line: u32,
    column: usize,
}

// NOTE: There no small integer types (less than int32), so overflow usually isn't possible here
#[allow(clippy::arithmetic_side_effects)]
impl<'s> Lex<'s> {
    pub fn new(text: &'s str) -> Self {
        Self {
            text,
            iter: text.char_indices().peekable(),
            line: 1,
            column: 0,
        }
    }

    fn skip_whitespaces(&mut self) {
        let mut skip_lines = 0;
        let mut column = self.column;

        while let Some((_, c)) = self.iter.peek() {
            match c {
                ' ' | '\t' => {
                    column += 1;
                }
                '\n' => {
                    column = 0;
                    skip_lines += 1;
                }
                '\r' => (),
                _ => break,
            }
            self.iter.next();
        }

        self.column = column;
        self.line += skip_lines;
    }

    const fn token_by_single_char(c: char) -> Token {
        match c {
            '(' => Token::ParenOpen,
            ')' => Token::ParenClose,
            '{' => Token::BraceOpen,
            '}' => Token::BraceClose,
            '=' => Token::Equal,
            '+' => Token::Plus,
            '*' => Token::Asterisk,
            ';' => Token::Semicolon,
            ',' => Token::Comma,

            // see Self::next_token
            _ => unreachable!(),
        }
    }

    pub fn next_token(&mut self) -> Option<Result<TokenData<'s>, LexError>> {
        self.skip_whitespaces();

        let (start, c) = self.iter.next()?;
        self.column += 1;

        let start_pos = Pos {
            line: self.line,
            column: self.column,
            index: start,
        };

        match c {
            '(' | ')' | '{' | '}' | '=' | '+' | '*' | ';' | ',' => Some(Ok(TokenData {
                text: &self.text[start..start + 1],
                pos: start_pos,
                token: Self::token_by_single_char(c),
            })),

            ':' => {
                let (end, token) = if let Some((_, '=')) = self.iter.peek() {
                    self.iter.next();
                    self.column += 1;
                    (start + 2, Token::Define)
                } else {
                    (start + 1, Token::Colon)
                };

                Some(Ok(TokenData {
                    text: &self.text[start..end],
                    pos: start_pos,
                    token,
                }))
            }

            '-' => {
                let (end, token) = if let Some((_, '>')) = self.iter.peek() {
                    self.iter.next();
                    self.column += 1;
                    (start + 2, Token::RightArrow)
                } else {
                    (start + 1, Token::Minus)
                };

                Some(Ok(TokenData {
                    text: &self.text[start..end],
                    pos: start_pos,
                    token,
                }))
            }

            '.' => {
                let (end, token) = if let Some((_, '.')) = self.iter.peek() {
                    self.iter.next();
                    self.column += 1;
                    if let Some((_, '.')) = self.iter.peek() {
                        self.iter.next();
                        self.column += 1;
                        (start + 3, Token::Ellipsis)
                    } else {
                        (start + 2, Token::DotDot)
                    }
                } else {
                    (start + 1, Token::Dot)
                };

                Some(Ok(TokenData {
                    text: &self.text[start..end],
                    pos: start_pos,
                    token,
                }))
            }

            '>' => {
                let (end, token) = if let Some((_, c @ ('>' | '='))) = self.iter.peek() {
                    (start + 2, if *c == '=' { Token::GtEq } else { Token::Shr })
                } else {
                    (start + 1, Token::Gt)
                };

                Some(Ok(TokenData {
                    text: &self.text[start..end],
                    pos: start_pos,
                    token,
                }))
            }

            '<' => {
                let (end, token) = if let Some((_, c @ ('<' | '='))) = self.iter.peek() {
                    (start + 2, if *c == '=' { Token::LtEq } else { Token::Shl })
                } else {
                    (start + 1, Token::Lt)
                };

                Some(Ok(TokenData {
                    text: &self.text[start..end],
                    pos: start_pos,
                    token,
                }))
            }

            '/' => {
                let Some((_, c)) = self.iter.next() else {
                    // return Some(Err(LexError::UnexpectedEOF));
                    return Some(Ok(TokenData {
                        text: &self.text[start..start + 1],
                        pos: start_pos,
                        token: Token::SingleSlash,
                    }));
                };
                self.column += 1;

                if c == '*' {
                    loop {
                        match (self.iter.next(), self.iter.peek()) {
                            (Some((_, '*')), Some((end, '/'))) => {
                                let end = *end;

                                _ = self.iter.next();
                                self.column += 2;

                                return Some(Ok(TokenData {
                                    text: &self.text[start..end + 1],
                                    pos: start_pos,
                                    token: Token::CommentBlock,
                                }));
                            }
                            (_, None) | (None, _) => {
                                return Some(Err(LexError::UnexpectedEOF));
                            }
                            (Some((_, '\n')), _) => {
                                self.column = 0;
                                self.line += 1;
                            }
                            _ => self.column += 1,
                        }
                    }
                } else if c != '/' {
                    return Some(Ok(TokenData {
                        text: &self.text[start..start + 1],
                        pos: start_pos,
                        token: Token::SingleSlash,
                    }));
                };

                let token = if let Some((_, '/')) = self.iter.peek() {
                    _ = self.iter.next();
                    self.column += 1;
                    Token::CommentDoc
                } else {
                    Token::CommentLine
                };

                let end = loop {
                    match self.iter.next() {
                        Some((end, '\n')) => break end,
                        None => break self.text.len(),
                        _ => (),
                    }
                };
                self.column = 0;
                self.line += 1;

                Some(Ok(TokenData {
                    text: &self.text[start..end],
                    pos: start_pos,
                    token,
                }))
            }

            '0'..='9' => {
                let mut idx = 1;

                let token = loop {
                    match self.iter.peek() {
                        Some((_, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_')) => {
                            self.iter.next();
                            self.column += 1;
                            idx += 1;
                        }
                        Some((_, '.')) => {
                            self.iter.next();
                            self.column += 1;
                            idx += 1;
                            break Token::LitFloat;
                        }
                        _ => break Token::LitInteger,
                    }
                };

                if token == Token::LitFloat {
                    while let Some((_, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_')) = self.iter.peek()
                    {
                        self.iter.next();
                        self.column += 1;
                        idx += 1;
                    }
                }

                Some(Ok(TokenData {
                    text: &self.text[start..start + idx],
                    pos: start_pos,
                    token,
                }))
            }

            '"' => {
                let end = loop {
                    self.column += 1;
                    match (self.iter.next(), self.iter.peek()) {
                        (None, _) | (_, None) => {
                            return Some(Err(LexError::UnexpectedEOF));
                        }
                        (Some((_, '\n')), _) => {
                            self.column = 0;
                            self.line += 1;
                        }
                        (Some((_, '\\')), _) => {
                            self.iter.next();
                            self.column += 1;
                        }
                        (Some((end, '"')), _) => {
                            break end + 1;
                        }
                        _ => {}
                    }
                };

                Some(Ok(TokenData {
                    text: &self.text[start..end],
                    pos: start_pos,
                    token: Token::LitString,
                }))
            }

            _ if !Token::is_disallowed_ident_char(c) => {
                let end = loop {
                    match self.iter.peek() {
                        Some((end, c)) if Token::is_disallowed_ident_char(*c) => {
                            break *end;
                        }
                        None => break self.text.len(),
                        _ => {
                            self.iter.next();
                            self.column += 1; // newlines are disallowed
                        }
                    }
                };

                let text = &self.text[start..end];
                let token = Token::from_word(text);

                Some(Ok(TokenData {
                    text,
                    pos: start_pos,
                    token,
                }))
            }

            char => Some(Err(LexError::UnexpectedChar {
                pos: start_pos,
                char,
            })),
        }
    }
}

impl<'s> Iterator for Lex<'s> {
    type Item = Result<TokenData<'s>, LexError>;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
