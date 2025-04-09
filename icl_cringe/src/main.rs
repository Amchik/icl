use std::{env, fs, process::ExitCode};

use icl_frontend::{
    ast::doc::Document,
    lex::Lex,
    parser::{Parse, TokenStream},
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
        Ok(r) => println!("{r:#?}"),
        Err(e) => {
            eprintln!("ЕГГОГ:\n{e:#?}");
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}
