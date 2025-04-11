//! Statements in code blocks

use crate::lex::span::{Span, Spanned};

use super::{
    block::CodeBlock,
    doc::DefineStmt,
    expr::{Expression, MaybeWrapped},
    single::{
        Assign, BreakKeyword, ContinueKeyword, IfKeyword, ReturnKeyword, Semicolon, WhileKeyword,
    },
};

/// Structure, terminated by [`Semicolon`].
/// ```text
/// T;
///  ^ always required
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Terminated<T>(pub T, pub Semicolon);

/// Statement of [`CodeBlock`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement<'s> {
    /// Define. Can define new variables and functions/structs/etc.
    /// See [`DefineStmt`] for examples.
    Define(DefineStmt<'s>),
    /// Assign. Mutates defined variables.
    /// ```text
    /// foo = bar;
    /// ^~~ ^ ^~~ MaybeWrapped Expression
    /// |   | Assign
    /// | Expression
    /// ```
    Assign(Terminated<AssignStmt<'s>>),
    /// Expression.
    Expression(Terminated<Expression<'s>>),
    /// If statement.
    /// ```text
    /// if expression { ... }
    /// ^~ ^~~~~~~~~~ ^~~~~~~ CodeBlock
    /// |  | cond
    /// | IfKeyword
    ///
    /// else if expression { ... } // optional
    /// ^~~~ ^~~~~~~~~~~~~~~~~~~~~
    /// |    | IfElseStmt::If(...)
    /// | ElseKeyword
    ///
    /// else { ... }               // optional, or...
    ///      ^~~~~~~ IfElseStmt::Else(CodeBlock)
    ///
    /// /* neither `else if` or else` */
    /// ^~~~~~ IfElseStmt::None
    /// ```
    If(IfStmt<'s>),
    /// While loop.
    /// ```text
    /// while expression { ... }
    /// ^~~~~ ^~~~~~~~~~ ^~~~~~~ CodeBlock
    /// |     | cond
    /// | WhileKeyword
    /// ```
    While(WhileStmt<'s>),
    /// Return statement.
    /// ```text
    /// return;
    /// // or
    /// return expression;
    ///        ^~~~~~~~~~ optional
    /// ```
    Return(Terminated<ReturnStmt<'s>>),
    /// Break statement.
    /// ```text
    /// break;
    /// ```
    Break(Terminated<BreakKeyword>),
    /// Continue statement.
    /// ```text
    /// continue;
    /// ```
    Continue(Terminated<ContinueKeyword>),
}

/// See [`Expression`] for examples.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AssignStmt<'s> {
    pub lhs: Expression<'s>,
    pub assign: Assign,
    pub rhs: MaybeWrapped<Expression<'s>>,
}

/// See [`Expression`] for examples.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfStmt<'s> {
    pub start: IfKeyword,
    pub cond: Expression<'s>,
    pub body: CodeBlock<'s>,
    pub else_branch: IfElseStmt<'s>,
}

/// See [`Expression`] for examples.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IfElseStmt<'s> {
    If(Box<IfStmt<'s>>),
    Else(CodeBlock<'s>),
    None,
}

/// See [`Expression`] for examples.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WhileStmt<'s> {
    pub start: WhileKeyword,
    pub cond: Expression<'s>,
    pub body: CodeBlock<'s>,
}

/// See [`Expression`] for examples.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReturnStmt<'s> {
    pub ret: ReturnKeyword,
    pub value: Option<Expression<'s>>,
}

impl<S: Spanned> Spanned for Terminated<S> {
    fn span(&self) -> Span {
        Span::new(self.0.span().start, self.1.pos.clone().skip(";"))
    }
}
