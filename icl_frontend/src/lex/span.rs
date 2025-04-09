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

    pub const fn wrap<T>(self, value: T) -> Spanned<T> {
        Spanned { span: self, value }
    }
}

/// Value `T` wrapped in [`Span`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spanned<T: ?Sized> {
    pub span: Span,
    pub value: T,
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
