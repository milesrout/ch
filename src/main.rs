extern crate regex;
extern crate itertools;

mod lexer;
mod parser;

use lexer::Token;
use parser::Parser;

use std::path::Path;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let keywords = vec![
        "fn",
        "let",
        "return",
        "or",
        "and",
        "not"
    ].into_iter()
     .map(|s| (s, s))
     .collect::<Vec<_>>();

    let literal_tokens = vec![
        ("lbrack", r"\("),
        ("rbrack", r"\)"),
        ("lcurly", r"\{"),
        ("rcurly", r"\}"),
        ("lsqbrack", r"\["),
        ("rsqbrack", r"\]"),
        ("colon", r":"),
        ("arrow", r"->"),
        ("plus", r"\+"),
        ("minus", r"-"),
        ("modulus", r"%"),
        ("times", r"×"),
        ("divide", r"÷"),
        ("semi", r";"),
        ("equal", r"="),
        ("comma", r","),
        ("lt", r"<"),
        ("gt", r">"),
        ("le", r"<="),
        ("ge", r">="),
        ("eq", r"=="),
        ("ne", r"!="),
        ("lshift", r"<<"),
        ("rshift", r">>"),
        ("bitor", r"\|"),
        ("bitand", r"&"),
        ("bitxor", r"\^"),
        ("exp", r"\*\*"),
    ];

    let regex_tokens = vec![
        ("string", r#""(\\"|[^"])*""#),
        ("ident", "[a-zA-Z_][a-zA-Z0-9_]*"),
        ("wspace", "[ \\n\\r\\t]*"),
        ("ERROR", ".*?"),
    ];

    let regex = lexer::create_lexer_regex(keywords.iter()
        .chain(literal_tokens.iter())
        .chain(regex_tokens.iter())).unwrap();

    let path = Path::new("example.ch");
        
    let mut f = File::open(&path).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();

    let tokens = lexer::tokenise(&regex, &s);

    for &Token { name: ref n, string: ref s } in &tokens {
        if n != "wspace" {
            println!("{:} '{:}'", n, s);
        }
    }

    let mut parser = Parser::new(tokens.into_iter()
        .filter(|&Token { name: ref n, .. }| n != "wspace"));

    println!("{:?}", parser.parse());
}
