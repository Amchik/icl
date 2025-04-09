//! Entire document, and define statement

use super::{
    atom::{Ident, Literal},
    block::{Block, CodeBlock, StructBlock, StructProperty},
    expr::Expression,
    single::{Colon, Define, ParenClose, ParenOpen, RightArrow},
    stmt::Terminated,
    ty::Type,
};

/// Entire document.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Document<'s> {
    pub stmts: Vec<DocumentStmt<'s>>,
}

/// Statements allowed only on top level of document.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DocumentStmt<'s> {
    Define(DefineStmt<'s>),
}

/// Define statement.
/// ```text
/// const_define: Int := 42;
/// struct_define := struct { a: Int }
/// fn_define(args) := fn -> Int { return 42; }
///
/// // also can appears in CodeBlock:
/// variable_define := 42;
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DefineStmt<'s> {
    /// Name of defined thing.
    pub name: Ident<'s>,
    /// Hint of defined thing.
    /// ```text
    /// name(arg1, arg2, arg3: String) := fn { ... }
    ///     ^~~~~~~~~~~~~~~~~~~~~~~~~~ arguments hint
    ///
    /// name: Int := 42;
    ///     ^~~~~ type hint
    ///
    /// name := struct { foo: Bar }
    ///     ^ no hint
    /// ```
    pub hint: DefineHint<'s>,

    /// `:=` token.
    pub define: Define,

    /// Meta of defined thing, if available. Meta does not exists if `body` contains PlainExpr.
    /// ```text
    /// name := struct(lang_item = "name" builtin) { ... }
    ///               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~ meta
    ///
    /// name := struct { ... }
    ///               ^ no meta
    /// ```
    pub meta: Option<DefineMeta<'s>>,
    /// Defined thing output. Usually function return type.
    /// Output does not exists if `body` contains PlainExpr.
    /// ```text
    /// name := fn -> ReturnTy { ... }
    ///            ^~~~~~~~~~~ output
    ///
    /// name := fn { ... }
    ///           ^ no output
    /// ```
    pub output: Option<DefineOutput<'s>>,
    /// Body, type of define and it's block (or expression).
    /// ```test
    /// name := fn(foo bar) -> Baz { ... }
    ///         ^~                 ^~~~~~~ Block
    ///         | type of define
    pub body: DefineStmtBody<'s>,
    //pub semicolon: Semicolon,
}

/// See [`DefineStmt`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DefineHint<'s> {
    Arguments(DefineArgs<'s>),
    Type(DefineType<'s>),
    None,
}

/// See [`DefineStmt`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DefineArgs<'s> {
    pub par_open: ParenOpen,
    pub par_close: ParenClose,

    pub args: Vec<DefineArg<'s>>,
}

/// Argument of define hint.
/// ```text
/// (arg1, arg2: Type)
///  ^~~~  ^~~~~~~~~~ Typed
///  | Ident
/// ```
///
/// See [`DefineStmt`] for more.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DefineArg<'s> {
    Ident(Ident<'s>),
    Typed(StructProperty<'s>),
}

/// Type of define hint.
///
/// See [`DefineStmt`] for more.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DefineType<'s> {
    pub colon: Colon,
    pub ty: Type<'s>,
}

/// Meta of define.
/// ```text
/// (ident ident2 pair_arg = "any literal")
///  ^~~~~        ^~~~~~~~~~~~~~~~~~~~~~~~ Pair
///  | Single argument
/// ```
///
/// See [`DefineStmt`] for more.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DefineMeta<'s> {
    pub par_open: ParenOpen,
    pub par_close: ParenClose,

    pub args: Vec<DefineMetaArgs<'s>>,
}

/// See [`DefineMeta`]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DefineMetaArgs<'s> {
    Single(Ident<'s>),
    Pair(Ident<'s>, Literal<'s>),
}

/// See [`DefineStmt`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DefineOutput<'s> {
    pub arrow: RightArrow,
    pub ty: Type<'s>,
}

/// Define type and body.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DefineStmtBody<'s> {
    /// Function
    Fn(Block<CodeBlock<'s>>),
    /// Struct
    Struct(Block<StructBlock<'s>>),

    /// Expression
    PlainExpr(Terminated<Expression<'s>>),
}
