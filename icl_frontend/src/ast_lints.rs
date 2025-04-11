pub mod formatting;
mod macros;

use std::borrow::Cow;

use crate::{ast::doc::Document, lex::span::Span};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum LintLevel {
    /// Always report warnings from lint as errors.
    ForceDeny,
    /// Report warnings from lint as errors.
    Deny,
    /// Report warnings from lint.
    Warn,
    /// Ignore lint.
    #[default]
    Allow,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LintWarning {
    pub span: Span,
    pub title: Cow<'static, str>,
    pub notes: Cow<'static, [Cow<'static, str>]>,
    pub help: Cow<'static, [Cow<'static, str>]>,
}

#[derive(Clone)]
pub struct Lint {
    pub category: &'static str,
    pub name: &'static str,
    pub level: LintLevel,
    pub worker: &'static dyn Fn(&Document) -> Vec<LintWarning>,
}

pub const LINTS: &[Lint] =
    macros::define_lints![formatting::parenthesized_equal_on_assign(LintLevel::Warn),];
