//! Tokenizer

use core::iter::Peekable;

/// Token
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token {
    /// like 1.0
    Number(f64),
    /// `(`
    ParenOpen,
    /// `)`
    ParenClose,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `**`
    Pow,
    /// Space
    Space,
    /// end-of-line like `\n`, `\r\n`, or EOF
    EOL,
}

/// Tokenizer
#[derive(Debug, Clone, PartialEq)]
pub struct Tokenizer {
    /// vertical position, 0-index
    pub col: usize,
    /// horizontal position, 0-index
    pub row: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenizeErrorKind {
    UnexpectedChar(char),
    InvalidNumberFormat,
    IsolatedCarriageReturn,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenizeError {
    /// error kind
    pub kind: TokenizeErrorKind,
    /// vertical position, 0-index
    pub col: usize,
    /// horizontal position, 0-index
    pub row: usize,
}

impl Tokenizer {
    /// create new `Tokenizer` instance
    ///
    /// # Example
    ///
    /// ```rust
    /// # use recursive_syntax_parser_practice::tokenizer::Tokenizer;
    /// let _t = Tokenizer::new();
    /// ```
    pub fn new() -> Tokenizer {
        Tokenizer { col: 0, row: 0 }
    }

    /// calculate next token
    ///
    /// # Example
    ///
    /// ```rust
    /// # use recursive_syntax_parser_practice::tokenizer::{Tokenizer, Token};
    /// let mut s = "(1 +.2)**2.5".chars().peekable();
    /// let mut t = Tokenizer::new();
    /// assert_eq!(t.next_token(&mut s).unwrap(), Token::ParenOpen);
    /// assert_eq!(t.next_token(&mut s).unwrap(), Token::Number(1.0));
    /// assert_eq!(t.next_token(&mut s).unwrap(), Token::Space);
    /// assert_eq!(t.next_token(&mut s).unwrap(), Token::Add);
    /// assert_eq!(t.next_token(&mut s).unwrap(), Token::Number(0.2));
    /// assert_eq!(t.next_token(&mut s).unwrap(), Token::ParenClose);
    /// assert_eq!(t.next_token(&mut s).unwrap(), Token::Pow);
    /// assert_eq!(t.next_token(&mut s).unwrap(), Token::Number(2.5));
    /// assert_eq!(t.next_token(&mut s).unwrap(), Token::EOL);
    /// ```
    pub fn next_token<I>(&mut self, chars: &mut Peekable<I>) -> Result<Token, TokenizeError>
    where
        I: Iterator<Item = char>,
    {
        match chars.peek() {
            Some(&ch) => match ch {
                '(' => self.consume_and_return(chars, Token::ParenOpen),
                ')' => self.consume_and_return(chars, Token::ParenClose),
                '+' => self.consume_and_return(chars, Token::Add),
                '-' => self.consume_and_return(chars, Token::Sub),
                '/' => self.consume_and_return(chars, Token::Div),
                '*' => {
                    self.consume(chars);
                    match chars.peek() {
                        Some('*') => self.consume_and_return(chars, Token::Pow),
                        _ => Ok(Token::Mul),
                    }
                }
                ' ' => self.consume_and_return(chars, Token::Space),
                '\r' => {
                    self.consume(chars);
                    match chars.peek() {
                        Some('\n') => self.consume_and_newline(chars),
                        _ => self.error(TokenizeErrorKind::IsolatedCarriageReturn),
                    }
                }
                '\n' => self.consume_and_newline(chars),
                '0'..='9' | '.' => {
                    let mut s = String::with_capacity(1);
                    s.push(ch);
                    self.consume(chars);
                    while let Some(&ch) = chars.peek() {
                        match ch {
                            '0'..='9' | '.' => {
                                self.consume(chars);
                                s.push(ch);
                            }
                            _ => break,
                        }
                    }
                    match s.parse() {
                        Ok(n) => Ok(Token::Number(n)),
                        Err(_) => self.error(TokenizeErrorKind::InvalidNumberFormat),
                    }
                }
                ch => self.error(TokenizeErrorKind::UnexpectedChar(ch)),
            },
            None => Ok(Token::EOL),
        }
    }

    fn consume_and_newline<I: Iterator>(&mut self, it: &mut I) -> Result<Token, TokenizeError> {
        self.col = 0;
        self.row += 1;
        it.next();
        Ok(Token::EOL)
    }

    fn consume<I: Iterator>(&mut self, it: &mut I) {
        self.col += 1;
        it.next();
    }

    fn consume_and_return<I: Iterator>(
        &mut self,
        it: &mut I,
        token: Token,
    ) -> Result<Token, TokenizeError> {
        self.col += 1;
        it.next();
        Ok(token)
    }

    fn error(&self, kind: TokenizeErrorKind) -> Result<Token, TokenizeError> {
        Err(TokenizeError {
            kind,
            row: self.row,
            col: self.col,
        })
    }
}

impl Default for Tokenizer {
    fn default() -> Tokenizer {
        Tokenizer::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::str::Chars;

    fn chars(s: &str) -> Peekable<Chars> {
        s.chars().peekable()
    }

    #[test]
    fn tokenize_operation() {
        let s = "+-/** *()\n(";
        let mut chars = chars(s);
        let mut t = Tokenizer::new();
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Add);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Sub);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Div);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Pow);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Space);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Mul);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::ParenOpen);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::ParenClose);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::EOL);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::ParenOpen);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::EOL);
    }

    #[test]
    fn tokenize_number() {
        let s = "1 2.5 .25";
        let mut chars = chars(s);
        let mut t = Tokenizer::new();
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Number(1.0));
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Space);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Number(2.5));
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Space);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Number(0.25));
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::EOL);
    }

    #[test]
    fn unexpected_token() {
        let s = "1 **3 @ ";
        let mut chars = chars(s);
        let mut t = Tokenizer::new();
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Number(1.0));
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Space);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Pow);
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Number(3.0));
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::Space);
        let e = t.next_token(&mut chars).unwrap_err();
        assert_eq!(e.kind, TokenizeErrorKind::UnexpectedChar('@'));
        assert_eq!(e.row, 0);
        assert_eq!(e.col, 6);
    }

    #[test]
    fn invalid_number() {
        let s = "\r\n1.1.1";
        let mut chars = chars(s);
        let mut t = Tokenizer::new();
        assert_eq!(t.next_token(&mut chars).unwrap(), Token::EOL);
        let e = t.next_token(&mut chars).unwrap_err();
        assert_eq!(e.kind, TokenizeErrorKind::InvalidNumberFormat);
        assert_eq!(e.row, 1);
        assert_eq!(e.col, 5);
    }
}
