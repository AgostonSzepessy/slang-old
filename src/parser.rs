use token::Token;
use tokenizer::TokenIterator;

use failure::Error;

use std::mem;
use std::fmt;
use std::iter::Peekable;

#[derive(Debug)]
pub struct TokenVec(pub Vec<Token>);

impl fmt::Display for TokenVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "vector")
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    /// Expressions that have 2 operands
    Binary(Box<Expr>, Token, Box<Expr>),
    /// Expressions that have 1 operand
    Unary(Token, Box<Expr>),
    /// Expressions that only contain a number or string
    Primary(Token),
    /// Expressions that are within parentheses
    Grouping(Box<Expr>),
}

#[derive(Debug, Fail)]
pub enum ParseError {
    #[fail(display = "Error: expected {}, found end of file", _0)]
    UnexpectedEndofFile(Token),
    #[fail(display = "Error: expected {}, found {}", _0, _1)]
    UnexpectedToken(Token, Token),
    #[fail(display = "Error: expected one of: {}", _0)]
    UnmatchedTokens(TokenVec),
    #[fail(display = "Error: expected an expression")]
    NoExpression,
}

pub struct Parser<'a> {
    tokens: Peekable<TokenIterator<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Peekable<TokenIterator<'a>>) -> Self {
        Parser {
            tokens: tokens,
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }
    
    fn equality(&mut self) -> Result<Expr, ParseError> {
        let expr = self.comparison()?;

        if self.match_token(&[mem::discriminant(&Token::BangEq), mem::discriminant(&Token::EqEq)]) {
            let operator = self.tokens.next().unwrap();
            let right = self.comparison()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let expr = self.addition()?;

        if self.match_token(&[mem::discriminant(&Token::Greater), mem::discriminant(&Token::GreaterEq), 
                            mem::discriminant(&Token::Less), mem::discriminant(&Token::LessEq)]) {
            let operator = self.tokens.next().unwrap();
            let right = self.addition()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        let expr = self.multiplication()?;

        if self.match_token(&[mem::discriminant(&Token::Plus), mem::discriminant(&Token::Minus)]) {
            let operator = self.tokens.next().unwrap();
            let right = self.multiplication()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        let expr = self.unary()?;

        if self.match_token(&[mem::discriminant(&Token::Star), mem::discriminant(&Token::Slash), mem::discriminant(&Token::Percent)]) {
            let operator = self.tokens.next().unwrap();
            let right = self.unary()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(&[mem::discriminant(&Token::Minus), mem::discriminant(&Token::Bang)]) {
            let operator = self.tokens.next().unwrap();
            let expr = self.primary()?;
            return Ok(Expr::Unary(operator, Box::new(expr)));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        // Found a primary token
        if self.match_token(&[mem::discriminant(&Token::Int(0)), mem::discriminant(&Token::Float(0f64)),
            mem::discriminant(&Token::False), mem::discriminant(&Token::True), mem::discriminant(&Token::None)]) {
            let token = self.tokens.next().unwrap();
            return Ok(Expr::Primary(token));
        }

        // Found the start of a grouping
        if self.match_token(&[mem::discriminant(&Token::LeftParen)]) {
            self.consume(Token::LeftParen)?;
            let expr = self.expression()?;
            self.consume(Token::RightParen)?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(ParseError::NoExpression)
    }

    /// Checks if the current token matches one of the specified tokens
    /// Parameters: `tokens`: slice of tokens to check
    fn match_token(&mut self, tokens: &[mem::Discriminant<Token>]) -> bool {
        for token in tokens.iter() {
            // Make sure we haven't reached the end of the tokens
            if let Some(ref t) = self.tokens.peek() {
                if token == &mem::discriminant(*t) {
                    return true;
                }
            } else {
                return false;
            }
        }

        return false;
    }

    /// Checks whether the current token is equal to the specified token. If it is, the iterator
    /// advances to the next token. Otherwise, it returns an error.
    /// Parameters: `token`: `Token` to check for.
    fn consume(&mut self, token: Token) -> Result<(), ParseError> {
        if self.match_token(&[mem::discriminant(&token)]) {
            self.tokens.next();
            return Ok(());
        }

        Err(ParseError::UnexpectedToken(token, self.tokens.peek().unwrap().clone()))
    }
}
