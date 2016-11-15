extern crate backtrace;
extern crate itertools;
extern crate regex;

mod lexer;
mod parser;

use lexer::{Lexer, Token, TokenType};
use parser::Parser;

use std::path::Path;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let path = Path::new("example.ch");
    let mut f = File::open(&path)
        .expect(format!("Could not open file {:}", path.to_string_lossy()).as_ref());

    let mut s = String::new();
    f.read_to_string(&mut s)
        .expect(format!("Could not read from file {:}", path.to_string_lossy()).as_ref());

    let lexer = Lexer::new().expect("Creating lexer failed");
    let tokens = lexer.tokenise(&s).expect("Tokenisation failed");

    for &Token { name: n, string: ref s } in &tokens {
        if n != TokenType::WSpace {
            println!("{:?} {:?}", n, s);
        }
    }

    let tokens = tokens.into_iter()
        .filter(|&Token { name: n, .. }| n != TokenType::WSpace);
    let mut parser = Parser::new(tokens).expect("Creating parser failed");

    println!("{:?}", parser.parse().expect("Parsing failed"));
}