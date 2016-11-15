use itertools::Itertools;
use regex::{Regex, Captures, Error as RegexError};
use std::error::Error;
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
    Equal,
    Comma,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
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
            TokenType::Equal => "equal",
            TokenType::Comma => "comma",
            TokenType::Lt => "lt",
            TokenType::Gt => "gt",
            TokenType::Le => "le",
            TokenType::Ge => "ge",
            TokenType::Eq => "eq",
            TokenType::Ne => "ne",
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
            "equal" => Some(TokenType::Equal),
            "comma" => Some(TokenType::Comma),
            "lt" => Some(TokenType::Lt),
            "gt" => Some(TokenType::Gt),
            "le" => Some(TokenType::Le),
            "ge" => Some(TokenType::Ge),
            "eq" => Some(TokenType::Eq),
            "ne" => Some(TokenType::Ne),
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
            TokenType::Equal => r"=",
            TokenType::Comma => r",",
            TokenType::Lt => r"<",
            TokenType::Gt => r">",
            TokenType::Le => r"<=",
            TokenType::Ge => r">=",
            TokenType::Eq => r"==",
            TokenType::Ne => r"!=",
            TokenType::LShift => r"<<",
            TokenType::RShift => r">>",
            TokenType::BitOr => r"\|",
            TokenType::BitAnd => r"&",
            TokenType::BitXor => r"\^",
            TokenType::Exp => r"\*\*",
            TokenType::String => r#""(\\"|[^"])*""#,
            TokenType::Ident => "[a-zA-Z_][a-zA-Z0-9_]*",
            TokenType::WSpace => "[ \\n\\r\\t]*",
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
    TokenType::Equal,
    TokenType::Comma,
    TokenType::Lt,
    TokenType::Gt,
    TokenType::Le,
    TokenType::Ge,
    TokenType::Eq,
    TokenType::Ne,
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
    pub fn new() -> Lexer {
        Lexer::from_tokens(TOKENS.iter())
    }

    fn from_tokens<'a, I>(tokens: I) -> Lexer
        where I: IntoIterator<Item=&'a TokenType>
    {
        let s = tokens.into_iter()
            .map(|tok| format!("(?P<{}>{})", tok.name(), tok.pattern()))
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
            .map(Option::unwrap) // guaranteed to come from TOKENS
            .ok_or(LexerError::Internal)
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