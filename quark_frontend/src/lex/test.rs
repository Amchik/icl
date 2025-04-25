//! Unit tests for lexer

use super::{Lex, TokenData, span::Pos, token::Token};

fn assert_tokendata_eq(expected: &[TokenData], actual: &[TokenData]) {
    if expected.len() != actual.len() {
        eprintln!("=> Test failed: expected and actual lengths are different");
        eprintln!("Expected: {expected:#?}");
        eprintln!("Actual: {actual:#?}");
        assert_eq!(expected, actual);
    }

    for (idx, (exp, act)) in expected.iter().zip(actual).enumerate() {
        if exp != act {
            eprintln!("=> Test failed for token #{idx}");
        } else {
            continue;
        }
        if exp.text != act.text {
            eprintln!("==> Text not equal");
            eprintln!("Expected: {:?}\nActual: {:?}", exp.text, act.text);
        } else {
            eprintln!("==> Text: {:?}", exp.text);
        }
        if exp.pos != act.pos {
            eprintln!("==> Positions not equal");
            eprintln!("Expected: {:?}\nActual: {:?}", exp.pos, act.pos);
        } else {
            eprintln!("==> Position: {:?}", exp.pos);
        }
        if exp.token != act.token {
            eprintln!("==> Token kind not equal");
            eprintln!("Expected: {:?}\nActual: {:?}", exp.token, act.token);
        } else {
            eprintln!("==> Token: {:?}", exp.token);
        }
        assert_eq!(exp, act);
    }
}

macro_rules! tokendata_arr {
    (@int $text:literal $line:literal : $col:literal @ $index:literal Token::$tok:ident) => {
        TokenData {
            text: $text,
            pos: Pos {
                line: $line,
                column: $col,
                index: $index,
            },
            token: Token::$tok,
        }
    };
    ($( $text:literal $line:literal : $col:literal @ $index:literal Token::$tok:ident ),+ $(,)?) => {
        &[
            $( tokendata_arr!( @int $text $line : $col @ $index Token::$tok ) ),+
        ][..]
    };
}

#[test]
fn test_lexer_infty_loop() {
    let text = "four\nsecond_line";

    let mut lex = Lex::new(text);
    while lex.next_token().is_some() {}

    // this point should be reachable
}

#[test]
fn test_lexer_position() {
    let text = "four\nsecond_line columns_test";
    let expected = tokendata_arr! [
        // text line:column @ index token,
        "four"         1:1  @ 0  Token::Ident,
        "second_line"  2:1  @ 5  Token::Ident,
        "columns_test" 2:13 @ 17 Token::Ident,
    ];

    let lex = Lex::new(text);
    let actual: Result<Vec<_>, _> = lex.collect();
    let actual = actual.unwrap();

    assert_tokendata_eq(expected, &actual);
}

#[test]
fn test_lexer() {
    let text = include_str!("tests/test_lexer.txt");
    let expected = tokendata_arr![
        "// idk syntax btw"           1:1  @ 0   Token::CommentLine,
        "import"                      2:1  @ 18  Token::Ident,
        "ceheki"                      2:8  @ 25  Token::Ident,
        "."                           2:14 @ 31  Token::Dot,
        "org"                         2:15 @ 32  Token::Ident,
        ";"                           2:18 @ 35  Token::Semicolon,

        "/// The program entry point" 4:1  @ 38  Token::CommentDoc,
        "main"                        5:1  @ 66  Token::Ident,
        "("                           5:5  @ 70  Token::ParenOpen,
        "args"                        5:6  @ 71  Token::Ident,
        ")"                           5:10 @ 75  Token::ParenClose,
        ":="                          5:12 @ 77  Token::Define,
        "fn"                          5:15 @ 80  Token::KwFn,
        "{"                           5:18 @ 83  Token::BraceOpen,
        "..."                         5:20 @ 85  Token::Ellipsis,
        "}"                           5:24 @ 89  Token::BraceClose,
        ";"                           5:25 @ 90  Token::Semicolon,

        "Empty"                       7:1  @ 93  Token::Ident,
        ":="                          7:7  @ 99  Token::Define,
        "struct"                      7:10 @ 102 Token::KwStruct,
        "{"                           7:17 @ 109 Token::BraceOpen,
        "/* noop */"                  7:19 @ 111 Token::CommentBlock,
        "}"                           7:30 @ 122 Token::BraceClose,
        ";"                           7:31 @ 123 Token::Semicolon,
    ];

    let lex = Lex::new(text);
    let actual: Result<Vec<_>, _> = lex.collect();
    let actual = actual.unwrap();

    assert_tokendata_eq(expected, &actual);
}

#[test]
fn test_lex_comments() {
    let text = "//\n\
                      ///\n\
                      /**/ z\n\
                      /* */ z";
    let expected = tokendata_arr! [
        "//"    1:1 @ 0  Token::CommentLine,
        "///"   2:1 @ 3  Token::CommentDoc,
        "/**/"  3:1 @ 7  Token::CommentBlock,
        "z"     3:6 @ 12 Token::Ident,
        "/* */" 4:1 @ 14 Token::CommentBlock,
        "z"     4:7 @ 20 Token::Ident,
    ];

    let lex = Lex::new(text);
    let actual: Result<Vec<_>, _> = lex.collect();
    let actual = actual.unwrap();

    assert_tokendata_eq(expected, &actual);
}
