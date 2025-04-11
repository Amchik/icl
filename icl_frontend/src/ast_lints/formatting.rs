use std::borrow::Cow;

use crate::{
    ast::{
        block::{Block, CodeBlock},
        doc::{DefineStmtBody, Document, DocumentStmt},
        expr::{Expression, InfixExpr, MaybeWrapped},
        ops::BinOp,
        single::Semicolon,
        stmt::{AssignStmt, Statement, Terminated},
    },
    lex::span::{Span, Spanned},
};

use super::LintWarning;

pub fn parenthesized_equal_on_assign(ast: &Document) -> Vec<LintWarning> {
    fn add_define_warn(warns: &mut Vec<LintWarning>, span: Span) {
        warns.push(LintWarning {
            span,
            title: "equals operator used in definition without parentheses".into(),
            help: (&[Cow::Borrowed(
                "to suppress this warning wrap value in (...)",
            )])
                .into(),
            notes: (&[]).into(),
        });
    }

    fn check_block(warns: &mut Vec<LintWarning>, code_block: &CodeBlock) {
        let stmts = &code_block.stmts;

        for stmt in stmts {
            if let Statement::Assign(Terminated(
                AssignStmt {
                    rhs:
                        rhs @ MaybeWrapped::Normal(Expression::Infix(InfixExpr {
                            op: BinOp::Equal, ..
                        })),
                    ..
                },
                Semicolon { pos: end },
            )) = stmt
            {
                let start = rhs.span().start;
                let end = end.clone();

                warns.push(LintWarning {
                    span: Span::new(start, end),
                    title: "equals operator used in assign without parentheses".into(),
                    help: (&[Cow::Borrowed(
                        "this code performs only one assign, second `=` interpreted as equal operator",
                    ), Cow::Borrowed(
                        "to suppress this warning wrap value in (...)",
                    )])
                        .into(),
                    notes: (&[]).into(),
                });
            } else if let Statement::Define(stmt) = stmt {
                if let DefineStmtBody::PlainExpr(Terminated(
                    MaybeWrapped::Normal(Expression::Infix(InfixExpr {
                        lhs,
                        op: BinOp::Equal,
                        ..
                    })),
                    Semicolon { pos: end },
                )) = &stmt.body
                {
                    let start = lhs.span().start;
                    let end = end.clone();

                    add_define_warn(warns, Span::new(start, end));
                }
            }
        }
    }

    let mut warns = Vec::new();

    let stmts = ast.stmts.iter().map(|v| match v {
        DocumentStmt::Define(s) => s,
    });

    for stmt in stmts {
        if let DefineStmtBody::PlainExpr(Terminated(
            MaybeWrapped::Normal(Expression::Infix(InfixExpr {
                lhs,
                op: BinOp::Equal,
                ..
            })),
            Semicolon { pos: end },
        )) = &stmt.body
        {
            let start = lhs.span().start;
            let end = end.clone();

            add_define_warn(&mut warns, Span::new(start, end));
        } else if let DefineStmtBody::Fn(Block::Normal(code_block)) = &stmt.body {
            check_block(&mut warns, code_block);
        }
    }

    warns
}
