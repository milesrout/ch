use itertools::Itertools;
use regex::{Regex, Captures, Error as RegexError};
use std::error::Error;
use std::fmt;

pub static TOKENS: [(&'static str, &'static str); 38] = [
    ("fn", "fn"),
    ("let", "let"),
    ("return", "return"),
    ("or", "or"),
    ("and", "and"),
    ("not", "not"),
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
    ("string", r#""(\\"|[^"])*""#),
    ("ident", "[a-zA-Z_][a-zA-Z0-9_]*"),
    ("wspace", "[ \\n\\r\\t]*"),
    ("ERROR", ".*?"),
];

pub struct Lexer(Regex);

impl Lexer {
    pub fn new() -> Lexer {
        Lexer::from_tokens(TOKENS.as_ref())
    }

    pub fn from_tokens<'a: 'b, 'b, I>(tokens: I) -> Lexer
        where I: IntoIterator<Item = &'b (&'a str, &'a str)>
    {
        let s = tokens.into_iter()
            .map(|&(k, v)| format!("(?P<{}>{})", k, v))
            .join("|");
        Lexer(Regex::new(&s).map_err(LexerError::from_regex_error).unwrap())
    }

    pub fn tokenise<'t>(&self, string: &'t str) -> Vec<Token> {
        let mut pos = 0;
        let mut tokens = vec![];

        while pos != string.len() {
            let c = self.0.captures(&string[pos..]).unwrap();
            let token = Lexer::first_token(&c).unwrap();
            let len = token.string.len();
            tokens.push(token);
            pos += len;
        }

        tokens
    }

    fn first_token(caps: &Captures) -> Result<Token, LexerError> {
        caps.iter_named()
            .filter_map(|(n, s)| s.map(|t| (n, t)))
            .last()
            .map(Token::from_tuple)
            .ok_or(LexerError::Internal)
    }
}



#[derive(Debug, Clone)]
pub struct Token {
    pub name: String,
    pub string: String,
}

impl Token {
    pub fn new(name: String, string: String) -> Token {
        Token { name: name, string: string, }
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

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert!(true);
    }
}