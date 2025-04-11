//! Token position and span

/// Inclusive region.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub const fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pos {
    /// Line indexed every `\n` char
    pub line: u32,
    /// Column in chars count from line start
    pub column: usize,
    /// Index of position in raw offset (not chars count)
    pub index: usize,
}

impl Pos {
    // NOTE: all types are big enough
    #[allow(clippy::arithmetic_side_effects)]
    pub fn skip(mut self, v: &str) -> Self {
        for c in v.chars() {
            if c == '\n' {
                self.column = 0;
                self.line += 1;
            } else if c != '\r'  {
                self.column += 1;
            }
        }
        self.index += v.len();
        self
    }
}

