use itertools::Itertools;
use regex::{Regex, Captures, Error as RegexError};
use std::error;
use std::fmt;

#[derive(Debug,Copy,Clone,PartialEq,Eq)]
pub enum TokenType {
    Fn,
    Let,
    Return,
    Or,
    And,
    Not,
    If,
    Else,
    LBrack,
    RBrack,
    LCurly,
    RCurly,
    LSqBrack,
    RSqBrack,
    Colon,
    Arrow,
    Tilde,
    Plus,
    Minus,
    Modulus,
    Times,
    Divide,
    Semi,
    Assign,
    Comma,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    EqualTo,
    NotEqualTo,
    LShift,
    RShift,
    BitOr,
    BitAnd,
    BitXor,
    Exp,
    String,
    Ident,
    WSpace,
    Error,
    Eof,
}

impl TokenType {
    fn name(self) -> &'static str {
        match self {
            TokenType::Fn => "fn",
            TokenType::Let => "let",
            TokenType::Return => "return",
            TokenType::Or => "or",
            TokenType::And => "and",
            TokenType::Not => "not",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::LBrack => "lbrack",
            TokenType::RBrack => "rbrack",
            TokenType::LCurly => "lcurly",
            TokenType::RCurly => "rcurly",
            TokenType::LSqBrack => "lsqbrack",
            TokenType::RSqBrack => "rsqbrack",
            TokenType::Colon => "colon",
            TokenType::Arrow => "arrow",
            TokenType::Tilde => "tilde",
            TokenType::Plus => "plus",
            TokenType::Minus => "minus",
            TokenType::Modulus => "modulus",
            TokenType::Times => "times",
            TokenType::Divide => "divide",
            TokenType::Semi => "semi",
            TokenType::Assign => "assign",
            TokenType::Comma => "comma",
            TokenType::LessThan => "lessthan",
            TokenType::GreaterThan => "greaterthan",
            TokenType::LessThanOrEqual => "lessthanorequal",
            TokenType::GreaterThanOrEqual => "greaterthanorequal",
            TokenType::EqualTo => "equalto",
            TokenType::NotEqualTo => "notequalto",
            TokenType::LShift => "lshift",
            TokenType::RShift => "rshift",
            TokenType::BitOr => "bitor",
            TokenType::BitAnd => "bitand",
            TokenType::BitXor => "bitxor",
            TokenType::Exp => "exp",
            TokenType::String => "string",
            TokenType::Ident => "ident",
            TokenType::WSpace => "wspace",
            TokenType::Error => "ERROR",
            TokenType::Eof => "EOF",
        }
    }

    fn from_name(name: &str) -> Option<Self> {
        match name {
            "fn" => Some(TokenType::Fn),
            "let" => Some(TokenType::Let),
            "return" => Some(TokenType::Return),
            "or" => Some(TokenType::Or),
            "and" => Some(TokenType::And),
            "not" => Some(TokenType::Not),
            "if" => Some(TokenType::If),
            "else" => Some(TokenType::Else),
            "lbrack" => Some(TokenType::LBrack),
            "rbrack" => Some(TokenType::RBrack),
            "lcurly" => Some(TokenType::LCurly),
            "rcurly" => Some(TokenType::RCurly),
            "lsqbrack" => Some(TokenType::LSqBrack),
            "rsqbrack" => Some(TokenType::RSqBrack),
            "colon" => Some(TokenType::Colon),
            "arrow" => Some(TokenType::Arrow),
            "tilde" => Some(TokenType::Tilde),
            "plus" => Some(TokenType::Plus),
            "minus" => Some(TokenType::Minus),
            "modulus" => Some(TokenType::Modulus),
            "times" => Some(TokenType::Times),
            "divide" => Some(TokenType::Divide),
            "semi" => Some(TokenType::Semi),
            "assign" => Some(TokenType::Assign),
            "comma" => Some(TokenType::Comma),
            "lessthan" => Some(TokenType::LessThan),
            "greaterthan" => Some(TokenType::GreaterThan),
            "lessthanorequal" => Some(TokenType::LessThanOrEqual),
            "greaterthanorequal" => Some(TokenType::GreaterThanOrEqual),
            "equalto" => Some(TokenType::EqualTo),
            "notequalto" => Some(TokenType::NotEqualTo),
            "lshift" => Some(TokenType::LShift),
            "rshift" => Some(TokenType::RShift),
            "bitor" => Some(TokenType::BitOr),
            "bitand" => Some(TokenType::BitAnd),
            "bitxor" => Some(TokenType::BitXor),
            "exp" => Some(TokenType::Exp),
            "string" => Some(TokenType::String),
            "ident" => Some(TokenType::Ident),
            "wspace" => Some(TokenType::WSpace),
            "ERROR" => Some(TokenType::Error),
            "EOF" => Some(TokenType::Eof),
            _ => None,
        }
    }
    fn pattern(self) -> &'static str {
        match self {
            TokenType::Fn => "fn",
            TokenType::Let => "let",
            TokenType::Return => "return",
            TokenType::Or => "or",
            TokenType::And => "and",
            TokenType::Not => "not",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::LBrack => r"\(",
            TokenType::RBrack => r"\)",
            TokenType::LCurly => r"\{",
            TokenType::RCurly => r"\}",
            TokenType::LSqBrack => r"\[",
            TokenType::RSqBrack => r"\]",
            TokenType::Colon => r":",
            TokenType::Arrow => r"->",
            TokenType::Tilde => r"~",
            TokenType::Plus => r"\+",
            TokenType::Minus => r"-",
            TokenType::Modulus => r"%",
            TokenType::Times => r"×",
            TokenType::Divide => r"÷",
            TokenType::Semi => r";",
            TokenType::Assign => r"=",
            TokenType::Comma => r",",
            TokenType::LessThan => r"<",
            TokenType::GreaterThan => r">",
            TokenType::LessThanOrEqual => r"<=",
            TokenType::GreaterThanOrEqual => r">=",
            TokenType::EqualTo => r"==",
            TokenType::NotEqualTo => r"!=",
            TokenType::LShift => r"<<",
            TokenType::RShift => r">>",
            TokenType::BitOr => r"\|",
            TokenType::BitAnd => r"&",
            TokenType::BitXor => r"\^",
            TokenType::Exp => r"\*\*",
            TokenType::String => r#""(\\"|[^"])*""#,
            TokenType::Ident => "[a-zA-Z_][a-zA-Z0-9_]*",
            TokenType::WSpace => "[ \\n\\r\\t]+",
            TokenType::Error => ".*?",
            TokenType::Eof => "",
        }
    }
}

pub static TOKENS: [TokenType; 41] = [
    TokenType::Fn,
    TokenType::Let,
    TokenType::Return,
    TokenType::Or,
    TokenType::And,
    TokenType::Not,
    TokenType::If,
    TokenType::Else,
    TokenType::LBrack,
    TokenType::RBrack,
    TokenType::LCurly,
    TokenType::RCurly,
    TokenType::LSqBrack,
    TokenType::RSqBrack,
    TokenType::Colon,
    TokenType::Arrow,
    TokenType::Tilde,
    TokenType::Plus,
    TokenType::Minus,
    TokenType::Modulus,
    TokenType::Times,
    TokenType::Divide,
    TokenType::Semi,
    TokenType::Assign,
    TokenType::Comma,
    TokenType::LessThan,
    TokenType::GreaterThan,
    TokenType::LessThanOrEqual,
    TokenType::GreaterThanOrEqual,
    TokenType::EqualTo,
    TokenType::NotEqualTo,
    TokenType::LShift,
    TokenType::RShift,
    TokenType::BitOr,
    TokenType::BitAnd,
    TokenType::BitXor,
    TokenType::Exp,
    TokenType::String,
    TokenType::Ident,
    TokenType::WSpace,
    TokenType::Error,
];

#[derive(Debug, Clone)]
pub struct Token {
    pub name: TokenType,
    pub string: String,
}

impl Token {
    pub fn new(name: TokenType, string: String) -> Token {
        Token { name: name, string: string, }
    }

    pub fn from_tuple((name, string): (&str, &str)) -> Option<Token> {
        TokenType::from_name(name)
            .map(|n| Token::new(n, string.to_string()))
    }
}

pub struct Lexer(Regex);

impl Lexer {
    pub fn new() -> Result<Lexer, Error> {
        Lexer::from_tokens(TOKENS.iter())
            .map_err(Error::from_regex_error)
    }

    fn from_tokens<'a, I>(tokens: I) -> Result<Lexer, RegexError>
        where I: IntoIterator<Item=&'a TokenType>
    {
        let pattern = tokens.into_iter()
            .map(|tok| format!("(?P<{}>{})", tok.name(), tok.pattern()))
            .join("|");
        Regex::new(&pattern)
            .map(|r| Lexer(r))
    }

    pub fn tokenise<'t>(&self, string: &'t str) -> Result<Vec<Token>, Error> {
        let mut pos = 0;
        let mut tokens = vec![];

        while pos != string.len() {
            let c = self.0.captures(&string[pos..])
                .ok_or(Error::NoTokens)?;
            let token = Lexer::first_token(&c)?;
            if token.name == TokenType::Error {
                return Err(Error::BadToken((&string[pos..]).to_string()));
            }

            let len = token.string.len();
            tokens.push(token);
            pos += len;
        }

        Ok(tokens)
    }

    fn first_token(caps: &Captures) -> Result<Token, Error> {
        caps.iter_named()
            .filter_map(|(n, s)| s.map(|t| (n, t)))
            .last()
            .map(Token::from_tuple)
            .map(Option::unwrap) // guaranteed to come from TOKENS
            .ok_or(Error::NoTokens)
    }
}

#[derive(Debug)]
pub enum Error {
    Regex(RegexError),
    NoTokens,
    BadToken(String),
}

impl Error {
    fn from_regex_error(err: RegexError) -> Error {
        Error::Regex(err)
    }

    fn description(&self) -> &str {
        match self {
            &Error::Regex(ref err) => (err as &error::Error).description(),
            &Error::NoTokens => "there are no tokens in the given string",
            &Error::BadToken(_) => "a bad token was encountered",
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:}", self.description())
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        (self as &self::Error).description()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert!(true);
    }
}