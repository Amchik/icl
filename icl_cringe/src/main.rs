use std::{env, fs, process::ExitCode};

use icl_frontend::{
    ast::doc::Document, ast_lints, lex::Lex, parser::{Parse, TokenStream}
};

fn main() -> ExitCode {
    let args = env::args().nth(1).expect("слыш даун 1 аргумент предъяви");
    let code = fs::read_to_string(args).expect("ну ёпта, это не файл");

    let lex =
        Lex::new(&code).filter(|v| v.as_ref().map(|d| d.token.is_non_comment()).unwrap_or(true));
    let parser = TokenStream::init(lex);

    let mut parser = match parser {
        Ok(r) => r,
        Err(e) => {
            eprintln!("ЕГГОГ:\n{e:#?}");
            return ExitCode::FAILURE;
        }
    };

    let doc = Document::parse(&mut parser);

    match doc {
        Ok(r) => {
            for lint in ast_lints::LINTS {
                let warns = (lint.worker)(&r);
                if warns.is_empty() {
                    continue;
                }
                println!("==== LINT {}::{} FOUND {} warning(s)", lint.category, lint.name, warns.len());
                for warn in warns {
                    println!("warning: {}", warn.title);
                    let len = warn.span.end.column - warn.span.start.column;
                    let start = warn.span.start.column;

                    let line = code.lines().nth(warn.span.start.line as usize - 1).expect("...");

                    println!(" {: >3} | {line}", warn.span.start.line);
                    println!("     | {}^{}", " ".repeat(start - 1), "~".repeat(len - 1));
                    for note in &warn.notes[..] {
                        println!("     = note: {note}");
                    }
                    for help in &warn.help[..] {
                        println!("     = help: {help}");
                    }
                }
            }
        },
        Err(e) => {
            eprintln!("ЕГГОГ:\n{e:#?}");
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}
