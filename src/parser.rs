use std;
use backtrace::Backtrace;
use lexer::{TokenType, Token};

#[derive(Debug, Clone)]
pub enum BuiltinType {
    Int,
}

use self::BuiltinType::*;

#[derive(Debug, Clone)]
pub enum Type {
    Builtin(BuiltinType),
    Named(String),
}

impl Type {
    fn from_string(string: String) -> Self {
        match &string as &str {
            "int" => Type::Builtin(Int),
            _ => Type::Named(string),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(String),
}

impl Pattern {
    fn from_string(string: String) -> Self {
        Pattern::Ident(string)
    }
}


#[derive(Debug, Clone)]
pub struct Param(Pattern, Type);

#[derive(Debug, Clone)]
pub enum Item {
    Fn(String, Vec<Param>, Type, Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Let(Pattern, Type, Expr),
    Return(Expr),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Call(Box<Expr>, Vec<Expr>),
    Idx(Box<Expr>, Vec<Expr>),
    String(String),
    Ident(String),
    IfElse { expr: Box<Expr>, cond: Box<Expr>, alt: Box<Expr> },
    UnaryPlus(Box<Expr>),
    UnaryMinus(Box<Expr>),
    UnaryTilde(Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Divide(Box<Expr>, Box<Expr>), // should really 'divided by' or 'over'
    Modulus(Box<Expr>, Box<Expr>),
    Power(Box<Expr>, Box<Expr>),
    LogicalAnd(Vec<Expr>),
    LogicalOr(Vec<Expr>),
    LogicalNot(Box<Expr>),
    BitOr(Vec<Expr>),
    BitXor(Vec<Expr>),
    BitAnd(Vec<Expr>),
    LShift(Box<Expr>, Box<Expr>),
    RShift(Box<Expr>, Box<Expr>),
}

pub struct Parser<I: Iterator<Item=Token>> {
    tokens: I,
    current_token: Token,
}

static COMP_OPS: [TokenType; 6] = [
    TokenType::EqualTo,
    TokenType::NotEqualTo,
    TokenType::LessThan,
    TokenType::GreaterThan,
    TokenType::LessThanOrEqual,
    TokenType::GreaterThanOrEqual,
];

#[derive(Debug)]
pub struct ParserError(ParserErrorKind, Backtrace);

impl ParserError {
    pub fn new(kind: ParserErrorKind) -> ParserError {
        ParserError(kind, Backtrace::new())
    }
}

#[derive(Debug)]
pub enum ParserErrorKind {
    WrongToken { expected: TokenType, actual: Token },
    TooFewTokens
}

use self::ParserErrorKind::*;

pub type Result<T> = std::result::Result<T, ParserError>;

impl<I: Iterator<Item=Token> + std::fmt::Debug> Parser<I> {
    pub fn new(mut tokens: I) -> Result<Parser<std::iter::Fuse<I>>> {
        let first = tokens.next();
        first.map(|tok| Parser {
            current_token: tok, 
            tokens: tokens.fuse(),
        }).ok_or(ParserError::new(TooFewTokens))
    }

    pub fn parse(&mut self) -> Result<Vec<Item>> {
        self.toplevel()
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt> {
        self.statement()
    }

    pub fn parse_expr(&mut self) -> Result<Expr> {
        self.expression()
    }

    fn next_token(&mut self) {
        self.current_token = match self.tokens.next() {
            Some(t) => t,
            None => Token::new(TokenType::Eof, String::new()),
        }
    }

    fn lookahead(&mut self) -> Token {
        self.current_token.clone()
    }

    fn get_token(&mut self) -> Token {
        let x = self.lookahead();
        self.next_token();
        x
    }

    fn accept(&mut self, tok: TokenType) -> bool {
        self.lookahead().name == tok
    }

    fn expect(&mut self, tok: TokenType) -> Result<()> {
        if self.accept(tok) {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError::new(WrongToken {
                actual: self.lookahead(),
                expected: tok,
            }))
        }
    }

    fn expect_get(&mut self, tok: TokenType) -> Result<String> {
        if self.lookahead().name == tok {
            Ok(self.get_token().string)
        } else {
            Err(ParserError::new(WrongToken {
                actual: self.lookahead(),
                expected: tok,
            }))
        }
    }

    fn expect_ident(&mut self) -> Result<String> {
        self.expect_get(TokenType::Ident)
    }

    fn accept_mut(&mut self, tok: TokenType) -> bool {
        if self.lookahead().name == tok {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn toplevel(&mut self) -> Result<Vec<Item>> {
        let mut items = vec![];
        while !self.accept(TokenType::Eof) {
            items.push(self.item()?);
        }
        Ok(items)
    }

    fn item(&mut self) -> Result<Item> {
        match self.lookahead().name {
            TokenType::Fn => self.fn_item(),
            // TokenType::Static => self.static_item(),
            // TokenType::Const => self.const_item(),
            // TokenType::Import => self.import_item(),
            _ => panic!(),
        }
    }

    fn block(&mut self) -> Result<Stmt> {
        self.expect(TokenType::LCurly)?;
        let mut stmts = vec![];
        while !self.accept_mut(TokenType::RCurly) {
            stmts.push(self.statement()?);
        }
        Ok(Stmt::Block(stmts))
    }

    fn statement(&mut self) -> Result<Stmt> {
        match self.lookahead().name {
            TokenType::Let => self.let_statement(),
            TokenType::Return => self.return_statement(),
            TokenType::LCurly => self.block(),
            _ => self.expr_statement(),
        }
    }

    fn fn_item(&mut self) -> Result<Item> {
        self.expect(TokenType::Fn)?;
        let name = self.expect_ident()?;
        self.expect(TokenType::LBrack)?;
        let params = self.param_list()?;
        self.expect(TokenType::RBrack)?;
        self.expect(TokenType::Arrow)?;
        let ret_typ = self.typ()?;
        self.expect(TokenType::LCurly)?;
        let mut body = vec![];
        while !self.accept_mut(TokenType::RCurly) {
            body.push(self.statement()?);
        }
        Ok(Item::Fn(name, params, ret_typ, body))
    }

    fn param_list(&mut self) -> Result<Vec<Param>> {
        let mut params = vec![];
        while !self.accept(TokenType::RBrack) {
            params.push(self.parameter()?);
            self.accept_mut(TokenType::Comma);
        }
        Ok(params)
    }

    fn parameter(&mut self) -> Result<Param> {
        let pat = self.pattern()?;
        self.expect(TokenType::Colon)?;
        let typ = self.typ()?;
        Ok(Param(pat, typ))
    }

    fn let_statement(&mut self) -> Result<Stmt> {
        self.expect(TokenType::Let)?;
        let pat = self.pattern()?;
        self.expect(TokenType::Colon)?;
        let typ = self.typ()?;
        self.expect(TokenType::Assign)?;
        let expr = self.expression()?;
        self.expect(TokenType::Semi)?;
        Ok(Stmt::Let(pat, typ, expr))
    }

    fn pattern(&mut self) -> Result<Pattern> {
        Ok(Pattern::from_string(self.expect_ident()?))
    }

    fn typ(&mut self) -> Result<Type> {
        Ok(Type::from_string(self.expect_ident()?))
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        self.expect(TokenType::Return)?;
        let stmt = Stmt::Return(self.expression()?);
        self.expect(TokenType::Semi)?;
        Ok(stmt)
    }

    fn expr_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.expect(TokenType::Semi)?;
        Ok(Stmt::Expr(expr))
    }

    fn expression(&mut self) -> Result<Expr> {
        let expr = self.or_expr()?;
        Ok(if self.accept_mut(TokenType::If) {
            let cond = self.or_expr()?;
            self.expect(TokenType::Else)?;
            let alt = self.or_expr()?;
            Expr::IfElse { 
                expr: Box::new(expr),
                cond: Box::new(cond),
                alt: Box::new(alt),
            }
        } else {
            expr
        })
    }

    fn or_expr(&mut self) -> Result<Expr> {
        self.op_expr(Self::and_expr, TokenType::Or, Expr::LogicalOr)
    }

    fn op_expr<F>(&mut self, next: fn (&mut Self) -> Result<Expr>, token: TokenType, make_ast_node: F) -> Result<Expr>
        where F: Fn(Vec<Expr>) -> Expr
    {
        let expr = next(self)?;
        if self.accept(token) {
            let mut exprs = vec![];
            while self.accept_mut(token) {
                exprs.push(next(self)?);
            }
            Ok(make_ast_node(exprs))
        } else {
            Ok(expr)
        }
    }

    fn and_expr(&mut self) -> Result<Expr> {
        self.op_expr(Self::not_expr, TokenType::And, Expr::LogicalAnd)
    }

    fn not_expr(&mut self) -> Result<Expr> {
        if self.accept_mut(TokenType::Not) {
            Ok(Expr::LogicalNot(Box::new(self.not_expr()?)))
        } else {
            self.comparison()
        }
    }

    fn comparison(&mut self) -> Result<Expr> {
        let expr = self.bitor_expr();
        while COMP_OPS.contains(&self.lookahead().name) {
            unimplemented!();
        }
        expr
    }

    fn bitor_expr(&mut self) -> Result<Expr> {
        self.op_expr(Self::bitxor_expr, TokenType::BitOr, Expr::BitOr)
    }

    fn bitxor_expr(&mut self) -> Result<Expr> {
        self.op_expr(Self::bitand_expr, TokenType::BitXor, Expr::BitXor)
    }

    fn bitand_expr(&mut self) -> Result<Expr> {
        self.op_expr(Self::bitshf_expr, TokenType::BitAnd, Expr::BitAnd)
    }

    fn bitshf_expr(&mut self) -> Result<Expr> {
        let expr = self.arith_expr()?;
        if self.accept_mut(TokenType::LShift) {
            Ok(Expr::LShift(Box::new(expr), Box::new(self.bitshf_expr()?)))
        } else if self.accept_mut(TokenType::RShift) {
            Ok(Expr::RShift(Box::new(expr), Box::new(self.bitshf_expr()?)))
        } else {
            Ok(expr)
        }
    }

    fn arith_expr(&mut self) -> Result<Expr> {
        let expr = self.term()?;
        if self.accept_mut(TokenType::Plus) {
            Ok(Expr::Plus(Box::new(expr), Box::new(self.arith_expr()?)))
        } else if self.accept_mut(TokenType::Minus) {
            Ok(Expr::Minus(Box::new(expr), Box::new(self.arith_expr()?)))
        } else {
            Ok(expr)
        }
    }

    fn term(&mut self) -> Result<Expr> {
        let expr = self.factor()?;
        if self.accept_mut(TokenType::Times) {
            Ok(Expr::Times(Box::new(expr), Box::new(self.term()?)))
        } else if self.accept_mut(TokenType::Divide) {
            Ok(Expr::Divide(Box::new(expr), Box::new(self.term()?)))
        } else if self.accept_mut(TokenType::Modulus) {
            Ok(Expr::Modulus(Box::new(expr), Box::new(self.term()?)))
        } else {
            Ok(expr)
        }
    }

    fn factor(&mut self) -> Result<Expr> {
        if self.accept_mut(TokenType::Plus) {
            Ok(Expr::UnaryPlus(Box::new(self.factor()?)))
        } else if self.accept_mut(TokenType::Minus) {
            Ok(Expr::UnaryMinus(Box::new(self.factor()?)))
        } else if self.accept_mut(TokenType::Tilde) {
            Ok(Expr::UnaryTilde(Box::new(self.factor()?)))
        } else {
            self.power()
        }
    }

    fn power(&mut self) -> Result<Expr> {
        let expr = self.atom_expr()?;
        if self.accept_mut(TokenType::Exp) {
            Ok(Expr::Power(Box::new(expr), Box::new(self.factor()?)))
        } else {
            Ok(expr)
        }
    }

    fn atom_expr(&mut self) -> Result<Expr> {
        let expr = self.atom()?;
        if self.accept(TokenType::LBrack) {
            self.call_or_idx_expr(expr)
        } else if self.accept(TokenType::LSqBrack) {
            self.call_or_idx_expr(expr)
        } else {
            Ok(expr)
        }
    }

    fn call_or_idx_expr(&mut self, expr: Expr) -> Result<Expr> {
        if self.accept(TokenType::LBrack) {
            let args = self.call_trailer()?;
            self.call_or_idx_expr(Expr::Call(Box::new(expr), args))
        } else if self.accept(TokenType::LSqBrack) {
            let subscrs = self.idx_trailer()?;
            self.call_or_idx_expr(Expr::Idx(Box::new(expr), subscrs))
        } else {
            Ok(expr)
        }
    }

    fn call_trailer(&mut self) -> Result<Vec<Expr>> {
        self.expect(TokenType::LBrack)?;
        let mut args = vec![];
        while !self.accept(TokenType::RBrack) {
            args.push(self.expression()?);
            self.accept_mut(TokenType::Comma);
        }
        self.expect(TokenType::RBrack)?;
        Ok(args)
    }

    fn idx_trailer(&mut self) -> Result<Vec<Expr>> {
        self.expect(TokenType::LSqBrack)?;
        let mut subscrs = vec![];
        while !self.accept(TokenType::RSqBrack) {
            subscrs.push(self.subscript()?);
        }
        self.expect(TokenType::RSqBrack)?;
        Ok(subscrs)
    }

    fn subscript(&mut self) -> Result<Expr> {
        self.expression()
    }

    fn atom(&mut self) -> Result<Expr> {
        if self.accept(TokenType::LBrack) {
            self.paren_expr()
        } else if self.accept(TokenType::String) {
            self.string()
        } else {
            self.ident()
        }
    }

    fn paren_expr(&mut self) -> Result<Expr> {
        self.expect(TokenType::LBrack)?;
        let expr = self.expression()?;
        self.expect(TokenType::RBrack)?;
        Ok(expr)
    }

    fn ident(&mut self) -> Result<Expr> {
        self.expect_ident()
            .map(Expr::Ident)
    }

    fn string(&mut self) -> Result<Expr> {
        self.expect_get(TokenType::String)
            .map(Expr::String)
    }
}