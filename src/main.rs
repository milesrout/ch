extern crate regex;
extern crate itertools;

mod lexer;
mod parser;

use lexer::{Lexer, Token, TokenType};
use parser::Parser;

use std::path::Path;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let path = Path::new("example.ch");
    let mut f = File::open(&path).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();

    let lexer = Lexer::new();
    let tokens = lexer.tokenise(&s);

    for &Token { name: n, string: ref s } in &tokens {
        if n != TokenType::WSpace {
            println!("{:?} {:?}", n, s);
        }
    }

    let mut parser = Parser::new(tokens.into_iter()
        .filter(|&Token { name: n, .. }| n != TokenType::WSpace));

    println!("{:?}", parser.parse());
}
