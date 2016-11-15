use lexer::{TokenType, Token};

use std;

#[derive(Debug, Clone)]
pub struct Type;

impl Type {
    fn from_string(_: String) -> Self {
        Type
    }
}

#[derive(Debug, Clone)]
pub struct Param { name: String, typ: Type }

#[derive(Debug, Clone)]
pub enum Stmt {
    Block { stmts: Vec<Stmt> },
    Let { pat: String, typ: Type, expr: Expr },
    Fn { name: String, params: Vec<Param>, returntype: Type, body: Vec<Stmt> },
    Return { expr: Expr },
    Expr { expr: Expr }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Call { expr: Box<Expr>, args: Vec<Expr> },
    Idx { expr: Box<Expr>, subscrs: Vec<Expr> },
    String { string: String },
    Ident { name: String },
    IfElse { expr: Box<Expr>, cond: Box<Expr>, alt: Box<Expr> },
    UnaryPlus { expr: Box<Expr> },
    UnaryMinus { expr: Box<Expr> },
    UnaryTilde { expr: Box<Expr> },
    Plus { lhs: Box<Expr>, rhs: Box<Expr> },
    Minus { lhs: Box<Expr>, rhs: Box<Expr> },
    Times { lhs: Box<Expr>, rhs: Box<Expr> },
    Divide { lhs: Box<Expr>, rhs: Box<Expr> }, // should really 'divided by' or 'over'
    Mod { lhs: Box<Expr>, rhs: Box<Expr> },
    Power { lhs: Box<Expr>, rhs: Box<Expr> },
    LogicalAnd { exprs: Vec<Expr> },
    LogicalOr { exprs: Vec<Expr> },
    LogicalNot { expr: Box<Expr> },
    BitOr { exprs: Vec<Expr> },
    BitXor { exprs: Vec<Expr> },
    BitAnd { exprs: Vec<Expr> },
    LShift { lhs: Box<Expr>, rhs: Box<Expr> },
    RShift { lhs: Box<Expr>, rhs: Box<Expr> },
}

pub struct Parser<I: Iterator<Item=Token>> {
    tokens: I,
    current_token: Token,
}

static COMP_OPS: [TokenType; 6] = [
    TokenType::Eq,
    TokenType::Ne,
    TokenType::Lt,
    TokenType::Gt,
    TokenType::Le,
    TokenType::Ge,
];

impl<I: Iterator<Item=Token>> Parser<I> {
    pub fn new(mut tokens: I) -> Parser<std::iter::Fuse<I>> {
        Parser { current_token: tokens.next().unwrap(), tokens: tokens.fuse() }
    }

    pub fn parse(&mut self) -> Stmt {
        self.statements()
    }

    pub fn parse_expr(&mut self) -> Expr {
        self.expression()
    }

    fn next_token(&mut self) {
        self.current_token = match self.tokens.next() {
            Some(t) => t,
            None => Token::new(TokenType::Eof, String::new()),
        }
    }

    fn lookahead(&self) -> Token {
        self.current_token.clone()
    }

    fn get_token(&mut self) -> Token {
        let x = self.lookahead();
        self.next_token();
        x
    }

    fn statements(&mut self) -> Stmt {
        let mut v = vec![];
        while self.lookahead().name != TokenType::Eof {
            v.push(self.statement());
        }
        if v.len() == 1 {
            v[0].clone()
        } else {
            Stmt::Block { stmts: v }
        }
    }

    fn statement(&mut self) -> Stmt {
        match self.lookahead().name {
            TokenType::Fn => self.fn_statement(),
            TokenType::Let => self.let_statement(),
            TokenType::Return => self.return_statement(),
            _ => self.expr_statement(),
        }
    }

    fn fn_statement(&mut self) -> Stmt {
        assert_eq!(self.get_token().name, TokenType::Fn);
        assert_eq!(self.lookahead().name, TokenType::Ident);
        let name = self.lookahead().string;
        self.next_token();
        assert_eq!(self.get_token().name, TokenType::LBrack);
        let params = self.param_list();
        assert_eq!(self.get_token().name, TokenType::RBrack);
        assert_eq!(self.get_token().name, TokenType::Arrow);
        assert_eq!(self.lookahead().name, TokenType::Ident);
        let returns = self.lookahead().string;
        self.next_token();
        assert_eq!(self.get_token().name, TokenType::LCurly);
        let mut body = vec![];
        while self.lookahead().name != TokenType::RCurly {
            body.push(self.statement());
        }
        assert_eq!(self.get_token().name, TokenType::RCurly);
        Stmt::Fn {
            name: name,
            params: params,
            returntype: Type::from_string(returns),
            body: body,
        }
    }

    fn param_list(&mut self) -> Vec<Param> {
        let mut params = vec![];
        while self.lookahead().name != TokenType::RBrack {
            params.push(self.parameter());
        }
        params
    }

    fn parameter(&mut self) -> Param {
        assert_eq!(self.lookahead().name, TokenType::Ident);
        let name = self.get_token().string;
        assert_eq!(self.get_token().name, TokenType::Colon);
        assert_eq!(self.lookahead().name, TokenType::Ident);
        let typ = self.get_token().string;
        Param { name: name, typ: Type::from_string(typ) }
    }

    fn let_statement(&mut self) -> Stmt {
        assert_eq!(self.get_token().name, TokenType::Let);
        let pat = self.pattern();
        assert_eq!(self.get_token().name, TokenType::Colon);
        assert_eq!(self.lookahead().name, TokenType::Ident);
        let typ = self.get_token().string;
        assert_eq!(self.get_token().name, TokenType::Equal);
        let expr = self.expression();
        assert_eq!(self.get_token().name, TokenType::Semi);
        Stmt::Let { 
            pat: pat, 
            typ: Type::from_string(typ), 
            expr: expr,
        }
    }

    fn pattern(&mut self) -> String {
        assert_eq!(self.lookahead().name, TokenType::Ident);
        self.get_token().string
    }

    fn return_statement(&mut self) -> Stmt {
        assert_eq!(self.get_token().name, TokenType::Return);
        let stmt = Stmt::Return { expr: self.expression() };
        assert_eq!(self.get_token().name, TokenType::Semi);
        stmt
    }

    fn expr_statement(&mut self) -> Stmt {
        let expr = self.expression();
        assert_eq!(self.get_token().name, TokenType::Semi);
        Stmt::Expr { expr: expr }
    }

    fn expression(&mut self) -> Expr {
        let expr = self.or_expr();
        if self.lookahead().name == TokenType::If {
            self.next_token();
            let cond = self.or_expr();
            assert_eq!(self.get_token().name, TokenType::Else);
            let alt = self.or_expr();
            Expr::IfElse { 
                expr: Box::new(expr),
                cond: Box::new(cond),
                alt: Box::new(alt),
            }
        } else {
            expr
        }
    }

    fn or_expr(&mut self) -> Expr {
        let expr = self.and_expr();
        if self.lookahead().name == TokenType::Or {
            let mut exprs = vec![];
            while self.lookahead().name == TokenType::Or {
                self.next_token();
                exprs.push(self.and_expr());
            }
            Expr::LogicalOr { exprs: exprs }
        } else {
            expr
        }
    }

    fn op_expr<F>(&mut self, next: fn (&mut Self) -> Expr, token: TokenType, make_ast_node: F) -> Expr
        where F: Fn(Vec<Expr>) -> Expr
    {
        let expr = next(self);
        if self.lookahead().name == token {
            let mut exprs = vec![];
                while self.lookahead().name == token {
                    self.next_token();
                    exprs.push(self.and_expr());
                }
                make_ast_node(exprs)
        } else {
            expr
        }
    }

    fn and_expr(&mut self) -> Expr {
        self.op_expr(Self::not_expr, TokenType::And, |exprs| Expr::LogicalAnd { exprs: exprs })
    }

    fn not_expr(&mut self) -> Expr {
        if self.lookahead().name == TokenType::Not {
            self.next_token();
            Expr::LogicalNot { expr: Box::new(self.not_expr()) }
        } else {
            self.comparison()
        }
    }

    fn comparison(&mut self) -> Expr {
        let expr = self.bitor_expr();
        while COMP_OPS.contains(&self.lookahead().name) {
            unimplemented!();
        }
        expr
    }

    fn bitor_expr(&mut self) -> Expr {
        self.op_expr(Self::bitxor_expr, TokenType::BitOr, |exprs| Expr::BitOr { exprs: exprs })
    }

    fn bitxor_expr(&mut self) -> Expr {
        self.op_expr(Self::bitand_expr, TokenType::BitXor, |exprs| Expr::BitXor { exprs: exprs })
    }

    fn bitand_expr(&mut self) -> Expr {
        self.op_expr(Self::bitshf_expr, TokenType::BitAnd, |exprs| Expr::BitAnd { exprs: exprs })
    }

    fn bitshf_expr(&mut self) -> Expr {
        let expr = self.arith_expr();
        if self.lookahead().name == TokenType::LShift {
            self.next_token();
            Expr::LShift { lhs: Box::new(expr), rhs: Box::new(self.bitshf_expr()) }
        } else if self.lookahead().name == TokenType::RShift {
            self.next_token();
            Expr::RShift { lhs: Box::new(expr), rhs: Box::new(self.bitshf_expr()) }
        } else {
            expr
        }
    }

    fn arith_expr(&mut self) -> Expr {
        let expr = self.term();
        if self.lookahead().name == TokenType::Plus {
            self.next_token();
            Expr::Plus { 
                lhs: Box::new(expr), 
                rhs: Box::new(self.arith_expr())
            }
        } else if self.lookahead().name == TokenType::Minus {
            self.next_token();
            Expr::Minus { 
                lhs: Box::new(expr), 
                rhs: Box::new(self.arith_expr())
            }
        } else {
            expr
        }
    }

    fn term(&mut self) -> Expr {
        let expr = self.factor();
        if self.lookahead().name == TokenType::Times {
            self.next_token();
            Expr::Times { 
                lhs: Box::new(expr), 
                rhs: Box::new(self.arith_expr())
            }
        } else if self.lookahead().name == TokenType::Divide {
            self.next_token();
            Expr::Divide { 
                lhs: Box::new(expr), 
                rhs: Box::new(self.arith_expr())
            }
        } else if self.lookahead().name == TokenType::Modulus {
            self.next_token();
            Expr::Mod { 
                lhs: Box::new(expr), 
                rhs: Box::new(self.arith_expr())
            }
        } else {
            expr
        }
    }

    fn factor(&mut self) -> Expr {
        if self.lookahead().name == TokenType::Plus {
            self.next_token();
            Expr::UnaryPlus { expr: Box::new(self.factor()) }
        } else if self.lookahead().name == TokenType::Minus {
            self.next_token();
            Expr::UnaryMinus { expr: Box::new(self.factor()) }
        } else if self.lookahead().name == TokenType::Tilde {
            self.next_token();
            Expr::UnaryTilde { expr: Box::new(self.factor()) }
        } else {
            self.power()
        }
    }

    fn power(&mut self) -> Expr {
        let expr = self.atom_expr();
        if self.lookahead().name == TokenType::Exp {
            Expr::Power { lhs: Box::new(expr), rhs: Box::new(self.factor()) }
        } else {
            expr
        }
    }

    fn atom_expr(&mut self) -> Expr {
        let expr = self.atom();
        if self.lookahead().name == TokenType::LBrack {
            self.call_or_idx_expr(expr)
        } else if self.lookahead().name == TokenType::LSqBrack {
            self.call_or_idx_expr(expr)
        } else {
            expr
        }
    }

    fn call_or_idx_expr(&mut self, expr: Expr) -> Expr {
        if self.lookahead().name == TokenType::LBrack {
            let args = self.call_trailer();
            self.call_or_idx_expr(Expr::Call { expr: Box::new(expr), args: args })
        } else if self.lookahead().name == TokenType::LSqBrack {
            let subscrs = self.idx_trailer();
            self.call_or_idx_expr(Expr::Idx { expr: Box::new(expr), subscrs: subscrs })
        } else {
            expr
        }
    }

    fn call_trailer(&mut self) -> Vec<Expr> {
        assert_eq!(self.get_token().name, TokenType::LBrack);
        let mut args = vec![];
        while self.lookahead().name != TokenType::RBrack {
            args.push(self.expression());
            if self.lookahead().name == TokenType::Comma {
                self.next_token();
            }
        }
        assert_eq!(self.get_token().name, TokenType::RBrack);
        args
    }

    fn idx_trailer(&mut self) -> Vec<Expr> {
        assert_eq!(self.get_token().name, TokenType::LSqBrack);
        let mut subscrs = vec![];
        while self.lookahead().name != TokenType::RSqBrack {
            subscrs.push(self.subscript());
        }
        assert_eq!(self.get_token().name, TokenType::RSqBrack);
        subscrs
    }

    fn subscript(&mut self) -> Expr {
        self.expression()
    }

    fn atom(&mut self) -> Expr {
        if self.lookahead().name == TokenType::LBrack {
            self.paren_expr()
        } else if self.lookahead().name == TokenType::String {
            Expr::String { string: self.get_token().string }
        } else {
            self.ident()
        }
    }

    fn paren_expr(&mut self) -> Expr {
        assert_eq!(self.get_token().name, TokenType::LBrack);
        let expr = self.expression();
        assert_eq!(self.get_token().name, TokenType::RBrack);
        expr
    }

    fn ident(&mut self) -> Expr {
        assert_eq!(self.lookahead().name, TokenType::Ident);
        Expr::Ident { name: self.get_token().string }
    }
}