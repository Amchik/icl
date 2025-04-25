//! # Quark Programming Language Frontend
//!
//! Example of usage:
//! ```
//! use quark_frontend::{
//!     ast::{
//!         atom::{Atom, Ident, Literal, LiteralType},
//!         doc::{DefineHint, DefineStmt, DefineStmtBody, Document, DocumentStmt},
//!         expr::{Expression, MaybeWrapped}, stmt::Terminated,
//!     },
//!     lex::Lex,
//!     parser::{Parse, TokenStream},
//! };
//!
//! let code = "foo := 42;";
//! let tokens =
//!     Lex::new(code)
//!         .filter(|v| {
//!             v
//!                 .as_ref()
//!                 .map(|d| d.token.is_non_comment())
//!                 .unwrap_or(true)
//!         });
//! let mut token_stream = TokenStream::init(tokens).unwrap();
//!
//! let document = Document::parse(&mut token_stream).unwrap();
//!
//! assert!(matches!(
//!     &document.stmts[..],
//!     [DocumentStmt::Define(DefineStmt {
//!         name: Ident { text: "foo", .. },
//!         hint: DefineHint::None,
//!         body: DefineStmtBody::PlainExpr(Terminated(
//!             MaybeWrapped::Normal(Expression::Atom(Atom::Literal(Literal {
//!                 text: "42",
//!                 ty: LiteralType::Integer,
//!                 ..
//!             }))),
//!             _
//!         )),
//!         ..
//!     })]
//! ));
//! ```

pub mod ast;
pub mod ast_lints;
pub mod lex;
pub mod parser;
