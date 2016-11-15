use itertools::Itertools;
use regex::{Regex, Captures, Error as RegexError};
use std::error::Error;
use std::fmt;

pub fn create_lexer_regex<'a: 'b, 'b, I>(tokens: I) -> Result<Regex, LexerError>
    where I: Iterator<Item = &'b (&'a str, &'a str)>
{
	let s = tokens
		.map(|&(k, v)| format!("(?P<{}>{})", k, v))
		.join("|");
    Regex::new(&s).map_err(LexerError::from_regex_error)
}

#[derive(Debug, Clone)]
pub struct Token {
    pub name: String,
    pub string: String,
}

impl Token {
    pub fn new(name: String, string: String) -> Token {
        Token {
            name: name,
            string: string,
        }
    }

    pub fn from_strs(name: &str, string: &str) -> Token {
        Token::new(name.to_string(), string.to_string())
    }

    pub fn from_tuple((name, string): (&str, &str)) -> Token {
        Token::from_strs(name, string)
    }
}

#[derive(Debug)]
pub enum LexerError {
    Regex(RegexError),
    Internal
}

impl LexerError {
    fn from_regex_error(err: RegexError) -> LexerError {
        LexerError::Regex(err)
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LexerError")
    }
}

impl Error for LexerError {
    fn description(&self) -> &str {
        match self {
            &LexerError::Regex(ref err) => err.description(),
            &LexerError::Internal => "LexerError",
        }
    }
}

fn first_token(caps: &Captures) -> Result<Token, LexerError> {
    caps.iter_named()
    	.filter_map(|(n, s)| s.map(|t| (n, t)))
        .last()
        .map(Token::from_tuple)
        .ok_or(LexerError::Internal)
}

pub fn tokenise<'t>(regex: &Regex, string: &'t str) -> Vec<Token> {
    let mut pos = 0;
    let mut tokens = vec![];

    while pos != string.len() {
        let c = regex.captures(&string[pos..]).unwrap();
        let token = first_token(&c).unwrap();
        let len = token.string.len();
        tokens.push(token);
        pos += len;
    }

    tokens
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert!(true);
    }
}