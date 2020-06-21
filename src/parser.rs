use crate::tokenizer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    stack: Vec<State>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl From<Token> for BinOp {
    fn from(token: Token) -> BinOp {
        match token {
            Token::Add => BinOp::Add,
            Token::Sub => BinOp::Sub,
            Token::Mul => BinOp::Mul,
            Token::Div => BinOp::Div,
            Token::Pow => BinOp::Pow,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum State {
    PareOpen,
    BinOp(BinOp),
    Expr(Expr),
}

impl State {
    fn into_expr(self) -> Expr {
        match self {
            State::Expr(e) => e,
            _ => panic!(),
        }
    }

    fn make_expr(self, head_expr: Expr, tail_expr: Expr) -> Expr {
        match self {
            State::BinOp(BinOp::Add) => Expr::Add(Box::new(head_expr), Box::new(tail_expr)),
            State::BinOp(BinOp::Sub) => Expr::Sub(Box::new(head_expr), Box::new(tail_expr)),
            State::BinOp(BinOp::Mul) => Expr::Mul(Box::new(head_expr), Box::new(tail_expr)),
            State::BinOp(BinOp::Div) => Expr::Div(Box::new(head_expr), Box::new(tail_expr)),
            // State::BinOp(BinOp::Pow) => Expr::Pow(Box::new(head_expr), Box::new(tail_expr)),
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxError {
    UnexpectedToken(Token),
    TooManyCloseParen,
    UnexpectedEndOfLine,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AcceptStatus {
    Accept,
    Yet,
}

impl Parser {
    pub fn new() -> Parser {
        Parser { stack: Vec::new() }
    }

    pub fn accept_token(&mut self, token: Token) -> Result<AcceptStatus, SyntaxError> {
        match self.stack.last() {
            None | Some(State::PareOpen) => match token {
                Token::Space => Ok(AcceptStatus::Yet),
                Token::Number(n) => {
                    self.stack.push(State::Expr(Expr::Number(n)));
                    Ok(AcceptStatus::Yet)
                }
                Token::ParenOpen => {
                    self.stack.push(State::PareOpen);
                    Ok(AcceptStatus::Yet)
                }
                t => Err(SyntaxError::UnexpectedToken(t)),
            },
            Some(State::Expr(_)) => match token {
                Token::Space => Ok(AcceptStatus::Yet),
                Token::Add | Token::Sub => {
                    self.resolve_tail_connected();
                    self.resolve_head_connected([BinOp::Mul, BinOp::Div]);
                    self.resolve_head_connected([BinOp::Add, BinOp::Sub]);
                    self.stack.push(State::BinOp(token.into()));
                    Ok(AcceptStatus::Yet)
                }
                Token::Mul | Token::Div => {
                    self.resolve_tail_connected();
                    self.resolve_head_connected([BinOp::Mul, BinOp::Div]);
                    self.stack.push(State::BinOp(token.into()));
                    Ok(AcceptStatus::Yet)
                }
                Token::Pow => {
                    self.stack.push(State::BinOp(BinOp::Pow));
                    Ok(AcceptStatus::Yet)
                }
                Token::ParenClose => {
                    self.resolve_tail_connected();
                    self.resolve_head_connected([BinOp::Mul, BinOp::Div]);
                    self.resolve_head_connected([BinOp::Add, BinOp::Sub]);
                    if matches!(&self.stack[..], [.., State::PareOpen, State::Expr(_)]) {
                        self.stack.swap_remove(self.stack.len() - 2);
                        Ok(AcceptStatus::Yet)
                    } else {
                        Err(SyntaxError::TooManyCloseParen)
                    }
                }
                Token::EOL => {
                    self.resolve_tail_connected();
                    self.resolve_head_connected([BinOp::Mul, BinOp::Div]);
                    self.resolve_head_connected([BinOp::Add, BinOp::Sub]);
                    if matches!(&self.stack[..], [State::Expr(_)]) {
                        Ok(AcceptStatus::Accept)
                    } else {
                        Err(SyntaxError::UnexpectedEndOfLine)
                    }
                }
                Token::ParenOpen | Token::Number(_) => Err(SyntaxError::UnexpectedToken(token)),
            },
            Some(State::BinOp(_)) => match token {
                Token::Space => Ok(AcceptStatus::Yet),
                Token::Number(n) => {
                    self.stack.push(State::Expr(Expr::Number(n)));
                    Ok(AcceptStatus::Yet)
                }
                t => Err(SyntaxError::UnexpectedToken(t)),
            },
        }
    }

    fn resolve_tail_connected(&mut self) {
        while let [.., State::Expr(_), State::BinOp(BinOp::Pow), State::Expr(_)] = &self.stack[..] {
            let tail_expr = self.stack.pop().unwrap().into_expr();
            self.stack.pop();
            let head_expr = self.stack.pop().unwrap().into_expr();
            self.stack.push(State::Expr(Expr::Pow(
                Box::new(head_expr),
                Box::new(tail_expr),
            )));
        }
    }

    fn resolve_head_connected(&mut self, ops: [BinOp; 2]) {
        let mut stack = Vec::new();
        while matches!(
            &self.stack[..],
            [.., State::Expr(_), State::BinOp(op), State::Expr(_)]
            if *op == ops[0] || *op == ops[1]
        ) {
            stack.push(self.stack.pop().unwrap());
            stack.push(self.stack.pop().unwrap());
        }
        if stack.is_empty() {
            return;
        }
        let mut head_expr = self.stack.pop().unwrap().into_expr();
        let mut iter = stack.into_iter();
        while let Some(op) = iter.next() {
            let tail_expr = iter.next().unwrap().into_expr();
            head_expr = op.make_expr(head_expr, tail_expr);
        }
        self.stack.push(State::Expr(head_expr));
    }
}
