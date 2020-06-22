use crate::expr::Expr;
use crate::tokenizer::Token;

/// Parser
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

    pub fn build(self) -> Expr {
        self.stack.into_iter().next().unwrap().into_expr()
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
        while let Some(op) = stack.pop() {
            let tail_expr = stack.pop().unwrap().into_expr();
            head_expr = op.make_expr(head_expr, tail_expr);
        }
        self.stack.push(State::Expr(head_expr));
    }
}

impl Default for Parser {
    fn default() -> Parser {
        Parser::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_number() {
        let mut p = Parser::new();
        assert_eq!(p.accept_token(Token::Number(1.0)), Ok(AcceptStatus::Yet));
        assert_eq!(p.accept_token(Token::EOL), Ok(AcceptStatus::Accept));
        assert_eq!(p.build(), Expr::Number(1.0));
    }

    #[test]
    fn single_add() {
        let mut p = Parser::new();
        assert_eq!(p.accept_token(Token::Number(1.0)), Ok(AcceptStatus::Yet));
        assert_eq!(p.accept_token(Token::Add), Ok(AcceptStatus::Yet));
        assert_eq!(p.accept_token(Token::Number(2.0)), Ok(AcceptStatus::Yet));
        assert_eq!(p.accept_token(Token::EOL), Ok(AcceptStatus::Accept));
        assert_eq!(
            p.build(),
            Expr::Add(Box::new(Expr::Number(1.0)), Box::new(Expr::Number(2.0)))
        );
    }

    /// `1 + 2 * 3 * 4 - 5 / 6 ** 7 ** 8 * 9 + 10` is
    /// `((1 + ((2 * 3) * 4)) - ((5 / (6 ** (7 ** 8))) * 9)) + 10`
    #[test]
    fn connectivity() {
        let tokens = vec![
            Token::Number(1.0),
            Token::Add,
            Token::Number(2.0),
            Token::Mul,
            Token::Number(3.0),
            Token::Mul,
            Token::Number(4.0),
            Token::Sub,
            Token::Number(5.0),
            Token::Div,
            Token::Number(6.0),
            Token::Pow,
            Token::Number(7.0),
            Token::Pow,
            Token::Number(8.0),
            Token::Mul,
            Token::Number(9.0),
            Token::Add,
            Token::Number(10.0),
        ];
        let mut p = Parser::new();
        for t in tokens {
            assert_eq!(p.accept_token(t), Ok(AcceptStatus::Yet));
            assert_eq!(p.accept_token(Token::Space), Ok(AcceptStatus::Yet));
        }
        assert_eq!(p.accept_token(Token::EOL), Ok(AcceptStatus::Accept));
        assert_eq!(
            p.build(),
            Expr::Add(
                Box::new(Expr::Sub(
                    Box::new(Expr::Add(
                        Box::new(Expr::Number(1.0)),
                        Box::new(Expr::Mul(
                            Box::new(Expr::Mul(
                                Box::new(Expr::Number(2.0)),
                                Box::new(Expr::Number(3.0)),
                            )),
                            Box::new(Expr::Number(4.0)),
                        ))
                    )),
                    Box::new(Expr::Mul(
                        Box::new(Expr::Div(
                            Box::new(Expr::Number(5.0)),
                            Box::new(Expr::Pow(
                                Box::new(Expr::Number(6.0)),
                                Box::new(Expr::Pow(
                                    Box::new(Expr::Number(7.0)),
                                    Box::new(Expr::Number(8.0)),
                                )),
                            )),
                        )),
                        Box::new(Expr::Number(9.0)),
                    )),
                )),
                Box::new(Expr::Number(10.0)),
            )
        );
    }

    #[test]
    fn paren() {
        let n = 100;
        let mut p = Parser::new();
        for _ in 0..n * 2 {
            assert_eq!(p.accept_token(Token::ParenOpen), Ok(AcceptStatus::Yet));
            assert_eq!(p.accept_token(Token::Space), Ok(AcceptStatus::Yet));
        }
        assert_eq!(p.accept_token(Token::Number(1.0)), Ok(AcceptStatus::Yet));
        assert_eq!(p.accept_token(Token::Add), Ok(AcceptStatus::Yet));
        assert_eq!(p.accept_token(Token::Number(0.0)), Ok(AcceptStatus::Yet));
        for _ in 0..n {
            assert_eq!(p.accept_token(Token::ParenClose), Ok(AcceptStatus::Yet));
            assert_eq!(p.accept_token(Token::Space), Ok(AcceptStatus::Yet));
        }
        assert_eq!(p.accept_token(Token::Mul), Ok(AcceptStatus::Yet));
        assert_eq!(p.accept_token(Token::Number(2.0)), Ok(AcceptStatus::Yet));
        for _ in 0..n {
            assert_eq!(p.accept_token(Token::ParenClose), Ok(AcceptStatus::Yet));
            assert_eq!(p.accept_token(Token::Space), Ok(AcceptStatus::Yet));
        }
        assert_eq!(p.accept_token(Token::EOL), Ok(AcceptStatus::Accept));
        assert_eq!(
            p.build(),
            Expr::Mul(
                Box::new(Expr::Add(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(0.0)),
                )),
                Box::new(Expr::Number(2.0)),
            )
        );
    }
}
