use lexer::Token;

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

static COMP_OPS: &'static [&'static str] = &[
    "eq", "ne", "le", "ge", "lt", "gt",
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
            None => Token::new("EOF".to_string(), "".to_string()),
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
        while self.lookahead().name != "EOF" {
            v.push(self.statement());
        }
        if v.len() == 1 {
            v[0].clone()
        } else {
            Stmt::Block { stmts: v }
        }
    }

    fn statement(&mut self) -> Stmt {
        match self.lookahead().name.as_str() {
            "fn" => self.fn_statement(),
            "let" => self.let_statement(),
            "return" => self.return_statement(),
            _ => self.expr_statement(),
        }
    }

    fn fn_statement(&mut self) -> Stmt {
        assert_eq!(self.get_token().name, "fn");
        assert_eq!(self.lookahead().name, "ident");
        let name = self.lookahead().string;
        self.next_token();
        assert_eq!(self.get_token().name, "lbrack");
        let params = self.param_list();
        assert_eq!(self.get_token().name, "rbrack");
        assert_eq!(self.get_token().name, "arrow");
        assert_eq!(self.lookahead().name, "ident");
        let returns = self.lookahead().string;
        self.next_token();
        assert_eq!(self.get_token().name, "lcurly");
        let mut body = vec![];
        while self.lookahead().name != "rcurly" {
            body.push(self.statement());
        }
        assert_eq!(self.get_token().name, "rcurly");
        Stmt::Fn {
            name: name,
            params: params,
            returntype: Type::from_string(returns),
            body: body,
        }
    }

    fn param_list(&mut self) -> Vec<Param> {
        let mut params = vec![];
        while self.lookahead().name != "rbrack" {
            params.push(self.parameter());
        }
        params
    }

    fn parameter(&mut self) -> Param {
        assert_eq!(self.lookahead().name, "ident");
        let name = self.get_token().string;
        assert_eq!(self.get_token().name, "colon");
        assert_eq!(self.lookahead().name, "ident");
        let typ = self.get_token().string;
        Param { name: name, typ: Type::from_string(typ) }
    }

    fn let_statement(&mut self) -> Stmt {
        assert_eq!(self.get_token().name, "let");
        let pat = self.pattern();
        assert_eq!(self.get_token().name, "colon");
        assert_eq!(self.lookahead().name, "ident");
        let typ = self.get_token().string;
        assert_eq!(self.get_token().name, "equal");
        let expr = self.expression();
        assert_eq!(self.get_token().name, "semi");
        Stmt::Let { 
            pat: pat, 
            typ: Type::from_string(typ), 
            expr: expr,
        }
    }

    fn pattern(&mut self) -> String {
        assert_eq!(self.lookahead().name, "ident");
        self.get_token().string
    }

    fn return_statement(&mut self) -> Stmt {
        assert_eq!(self.get_token().name, "return");
        let stmt = Stmt::Return { expr: self.expression() };
        assert_eq!(self.get_token().name, "semi");
        stmt
    }

    fn expr_statement(&mut self) -> Stmt {
        let expr = self.expression();
        assert_eq!(self.get_token().name, "semi");
        Stmt::Expr { expr: expr }
    }

    fn expression(&mut self) -> Expr {
        let expr = self.or_expr();
        if self.lookahead().name == "if" {
            self.next_token();
            let cond = self.or_expr();
            assert_eq!(self.get_token().name, "else");
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
        if self.lookahead().name == "or" {
            let mut exprs = vec![];
            while self.lookahead().name == "or" {
                self.next_token();
                exprs.push(self.and_expr());
            }
            Expr::LogicalOr { exprs: exprs }
        } else {
            expr
        }
    }

    fn op_expr<F>(&mut self, next: fn (&mut Self) -> Expr, token: &str, make_ast_node: F) -> Expr
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
        self.op_expr(Self::not_expr, "and", |exprs| Expr::LogicalAnd { exprs: exprs })
    }

    fn not_expr(&mut self) -> Expr {
        if self.lookahead().name == "not" {
            self.next_token();
            Expr::LogicalNot { expr: Box::new(self.not_expr()) }
        } else {
            self.comparison()
        }
    }

    fn comparison(&mut self) -> Expr {
        let expr = self.bitor_expr();
        while COMP_OPS.contains(&&&*self.lookahead().name) {
            unimplemented!();
        }
        expr
    }

    fn bitor_expr(&mut self) -> Expr {
        self.op_expr(Self::bitxor_expr, "bitor", |exprs| Expr::BitOr { exprs: exprs })
    }

    fn bitxor_expr(&mut self) -> Expr {
        self.op_expr(Self::bitand_expr, "bitxor", |exprs| Expr::BitXor { exprs: exprs })
    }

    fn bitand_expr(&mut self) -> Expr {
        self.op_expr(Self::bitshf_expr, "bitand", |exprs| Expr::BitAnd { exprs: exprs })
    }

    fn bitshf_expr(&mut self) -> Expr {
        let expr = self.arith_expr();
        if self.lookahead().name == "lshift" {
            self.next_token();
            Expr::LShift { lhs: Box::new(expr), rhs: Box::new(self.bitshf_expr()) }
        } else if self.lookahead().name == "rshift" {
            self.next_token();
            Expr::RShift { lhs: Box::new(expr), rhs: Box::new(self.bitshf_expr()) }
        } else {
            expr
        }
    }

    fn arith_expr(&mut self) -> Expr {
        let expr = self.term();
        if self.lookahead().name == "plus" {
            self.next_token();
            Expr::Plus { 
                lhs: Box::new(expr), 
                rhs: Box::new(self.arith_expr())
            }
        } else if self.lookahead().name == "minus" {
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
        if self.lookahead().name == "times" {
            self.next_token();
            Expr::Times { 
                lhs: Box::new(expr), 
                rhs: Box::new(self.arith_expr())
            }
        } else if self.lookahead().name == "divide" {
            self.next_token();
            Expr::Divide { 
                lhs: Box::new(expr), 
                rhs: Box::new(self.arith_expr())
            }
        } else if self.lookahead().name == "modulus" {
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
        if self.lookahead().name == "plus" {
            self.next_token();
            Expr::UnaryPlus { expr: Box::new(self.factor()) }
        } else if self.lookahead().name == "minus" {
            self.next_token();
            Expr::UnaryMinus { expr: Box::new(self.factor()) }
        } else if self.lookahead().name == "tilde" {
            self.next_token();
            Expr::UnaryTilde { expr: Box::new(self.factor()) }
        } else {
            self.power()
        }
    }

    fn power(&mut self) -> Expr {
        let expr = self.atom_expr();
        if self.lookahead().name == "exp" {
            Expr::Power { lhs: Box::new(expr), rhs: Box::new(self.factor()) }
        } else {
            expr
        }
    }

    fn atom_expr(&mut self) -> Expr {
        let expr = self.atom();
        if self.lookahead().name == "lbrack" {
            self.call_or_idx_expr(expr)
        } else if self.lookahead().name == "lsqbrack" {
            self.call_or_idx_expr(expr)
        } else {
            expr
        }
    }

    fn call_or_idx_expr(&mut self, expr: Expr) -> Expr {
        if self.lookahead().name == "lbrack" {
            let args = self.call_trailer();
            self.call_or_idx_expr(Expr::Call { expr: Box::new(expr), args: args })
        } else if self.lookahead().name == "lsqbrack" {
            let subscrs = self.idx_trailer();
            self.call_or_idx_expr(Expr::Idx { expr: Box::new(expr), subscrs: subscrs })
        } else {
            expr
        }
    }

    fn call_trailer(&mut self) -> Vec<Expr> {
        assert_eq!(self.get_token().name, "lbrack");
        let mut args = vec![];
        while self.lookahead().name != "rbrack" {
            args.push(self.expression());
            if self.lookahead().name == "comma" {
                self.next_token();
            }
        }
        assert_eq!(self.get_token().name, "rbrack");
        args
    }

    fn idx_trailer(&mut self) -> Vec<Expr> {
        assert_eq!(self.get_token().name, "lsqbrack");
        let mut subscrs = vec![];
        while self.lookahead().name != "rsqbrack" {
            subscrs.push(self.subscript());
        }
        assert_eq!(self.get_token().name, "rsqbrack");
        subscrs
    }

    fn subscript(&mut self) -> Expr {
        self.expression()
    }

    fn atom(&mut self) -> Expr {
        if self.lookahead().name == "lbrack" {
            self.paren_expr()
        } else if self.lookahead().name == "string" {
            Expr::String { string: self.get_token().string }
        } else {
            self.ident()
        }
    }

    fn paren_expr(&mut self) -> Expr {
        assert_eq!(self.get_token().name, "lbrack");
        let expr = self.expression();
        assert_eq!(self.get_token().name, "rbrack");
        expr
    }

    fn ident(&mut self) -> Expr {
        assert_eq!(self.lookahead().name, "ident");
        Expr::Ident { name: self.get_token().string }
    }
}