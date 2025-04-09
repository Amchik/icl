//! Unary and binary operators

/// Unary operator
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp {
    Plus,
    Minus,

    Ref,
    Deref,
}

/// Binary operator
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Minus,
    Mult,
    Div,

    Equal,
    NotEqual,

    Great,
    Less,
    GreatEqual,
    LessEqual,
}

// NOTE: power() consts is subject to change

impl UnOp {
    /// Power of operator
    pub const fn power(self) -> u8 {
        match self {
            Self::Plus | Self::Minus => 50,
            Self::Ref | Self::Deref => 50,
        }
    }
}

impl BinOp {
    /// Power of operator, `(left, right)`.
    pub const fn power(self) -> (u8, u8) {
        match self {
            Self::Equal | Self::NotEqual => (10, 11),
            Self::Great | Self::Less | Self::GreatEqual | Self::LessEqual => (12, 13),

            Self::Plus | Self::Minus => (20, 21),
            Self::Mult | Self::Div => (22, 23),
        }
    }
}
